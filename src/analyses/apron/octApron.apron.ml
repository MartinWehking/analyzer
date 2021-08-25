(** Analysis using Apron for integer variables. *)

open Prelude.Ana
open Analyses
open OctApronDomain

module M = Messages

module SpecFunctor (Priv: OctApronPriv.S) : Analyses.MCPSpec =
struct
  include Analyses.DefaultSpec

  let name () = "octApron"

  module D = OctApronComponents (Priv.D)
  module G = Priv.G
  module C = D

  module AD = OctApronDomain.D2

  let should_join = Priv.should_join

  let context x = if GobConfig.get_bool "ana.octapron.no-context" then D.bot () else x


  let exitstate  _ = { oct = AD.bot (); priv = Priv.startstate () }
  let startstate _ = { oct = AD.bot (); priv = Priv.startstate () }

  (* Functions for manipulating globals as temporary locals. *)

  let read_global ask getg st g x =
    if ThreadFlag.is_multi ask then
      Priv.read_global ask getg st g x
    else (
      let oct = st.oct in
      let g_var = V.global g in
      let x_var = V.local x in
      let oct' = AD.add_vars oct [g_var] in
      let oct' = AD.assign_var oct' x_var g_var in
      oct'
    )

  module VH = BatHashtbl.Make (Basetype.Variables)

  let read_globals_to_locals ask getg st e =
    let v_ins = VH.create 10 in
    let visitor = object
      inherit nopCilVisitor
      method! vvrbl (v: varinfo) =
        if v.vglob then (
          let v_in =
            if VH.mem v_ins v then
              VH.find v_ins v
            else
              let v_in = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#in") v.vtype in (* temporary local g#in for global g *)
              VH.replace v_ins v v_in;
              v_in
          in
          ChangeTo v_in
        )
        else
          SkipChildren
    end
    in
    let e' = visitCilExpr visitor e in
    let oct = AD.add_vars st.oct (List.map V.local (VH.values v_ins |> List.of_enum)) in (* add temporary g#in-s *)
    let oct' = VH.fold (fun v v_in oct ->
        if M.tracing then M.trace "apron" "read_global %a %a\n" d_varinfo v d_varinfo v_in;
        read_global ask getg {st with oct = oct} v v_in (* g#in = g; *)
      ) v_ins oct
    in
    (oct', e', v_ins)

  let read_from_globals_wrapper ask getg st e f =
    let (oct', e', _) = read_globals_to_locals ask getg st e in
    f oct' e' (* no need to remove g#in-s *)

  let assign_from_globals_wrapper ask getg st e f =
    let (oct', e', v_ins) = read_globals_to_locals ask getg st e in
    if M.tracing then M.trace "apron" "assign_from_globals_wrapper %a\n" d_exp e';
    let oct' = f oct' e' in (* x = e; *)
    let oct'' = AD.remove_vars oct' (List.map V.local (VH.values v_ins |> List.of_enum)) in (* remove temporary g#in-s *)
    oct''

  let write_global ask getg sideg st g x =
    if ThreadFlag.is_multi ask then
      Priv.write_global ask getg sideg st g x
    else (
      let oct = st.oct in
      let g_var = V.global g in
      let x_var = V.local x in
      let oct' = AD.add_vars oct [g_var] in
      let oct' = AD.assign_var oct' g_var x_var in
      {st with oct = oct'}
    )

  let assign_to_global_wrapper ask getg sideg st lv f =
    match lv with
    (* Lvals which are numbers, have no offset and their address wasn't taken *)
    | (Var v, NoOffset) when AD.varinfo_tracked v ->
      if not v.vglob then
        {st with oct = f st v}
      else (
        let v_out = Goblintutil.create_var @@ makeVarinfo false (v.vname ^ "#out") v.vtype in (* temporary local g#out for global g *)
        let st = {st with oct = AD.add_vars st.oct [V.local v_out]} in (* add temporary g#out *)
        let st' = {st with oct = f st v_out} in (* g#out = e; *)
        if M.tracing then M.trace "apron" "write_global %a %a\n" d_varinfo v d_varinfo v_out;
        let st' = write_global ask getg sideg st' v v_out in (* g = g#out; *)
        let oct'' = AD.remove_vars st'.oct [V.local v_out] in (* remove temporary g#out *)
        {st' with oct = oct''}
      )
    (* Ignoring all other assigns *)
    | _ ->
      st

  let assert_type_bounds oct x =
    assert (AD.varinfo_tracked x);
    let ik = Cilfacade.get_ikind x.vtype in
    if not (IntDomain.should_ignore_overflow ik) then ( (* don't add type bounds for signed when assume_none *)
      let (type_min, type_max) = IntDomain.Size.range_big_int ik in
      (* TODO: don't go through CIL exp? *)
      let oct = AD.assert_inv oct (BinOp (Le, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_max)), intType)) false in
      let oct = AD.assert_inv oct (BinOp (Ge, Lval (Cil.var x), (Cil.kintegerCilint ik (Cilint.cilint_of_big_int type_min)), intType)) false in
      oct
    )
    else
      oct


  (* Basic transfer functions. *)

  let assign ctx (lv:lval) e =
    let st = ctx.local in
    if !GU.global_initialization && e = MyCFG.unknown_exp then
      st (* ignore extern inits because there's no body before assign, so octagon env is empty... *)
    else (
      if M.tracing then M.traceli "apron" "assign %a = %a\n" d_lval lv d_exp e;
      let ask = Analyses.ask_of_ctx ctx in
      let r = assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
          assign_from_globals_wrapper ask ctx.global st e (fun oct' e' ->
              AD.assign_exp oct' (V.local v) e'
            )
        )
      in
      if M.tracing then M.traceu "apron" "-> %a\n" D.pretty r;
      r
    )

  let branch ctx e b =
    let st = ctx.local in
    let res = assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global st e (fun oct' e' ->
        (* not an assign, but must remove g#in-s still *)
        AD.assert_inv oct' e' (not b)
      )
    in
    if AD.is_bot_env res then raise Deadcode;
    {st with oct = res}


  (* Function call transfer functions. *)

  let enter ctx r f args =
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron enter f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron enter formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron enter local: %a\n" D.pretty ctx.local;
    let arg_assigns =
      Goblintutil.zip f.sformals args
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 V.arg)
    in
    let arg_vars = List.map fst arg_assigns in
    let new_oct = AD.add_vars st.oct arg_vars in
    (* AD.assign_exp_parallel_with new_oct arg_assigns; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_oct = List.fold_left (fun new_oct (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with oct = new_oct} e (fun oct' e' ->
            AD.assign_exp oct' var e'
          )
      ) new_oct arg_assigns
    in
    AD.remove_filter_with new_oct (fun var ->
        match V.find_metadata var with
        | Some Local -> true (* remove caller locals *)
        | Some Arg when not (List.mem_cmp Var.compare var arg_vars) -> true (* remove caller args, but keep just added args *)
        | _ -> false (* keep everything else (just added args, globals, global privs) *)
      );
    if M.tracing then M.tracel "combine" "apron enter newd: %a\n" AD.pretty new_oct;
    [st, {st with oct = new_oct}]

  let body ctx f =
    let st = ctx.local in
    let formals = List.filter AD.varinfo_tracked f.sformals in
    let locals = List.filter AD.varinfo_tracked f.slocals in
    let new_oct = AD.add_vars st.oct (List.map V.local (formals @ locals)) in
    (* TODO: do this after local_assigns? *)
    let new_oct = List.fold_left (fun new_oct x ->
        assert_type_bounds new_oct x
      ) new_oct (formals @ locals)
    in
    let local_assigns = List.map (fun x -> (V.local x, V.arg x)) formals in
    AD.assign_var_parallel_with new_oct local_assigns; (* doesn't need to be parallel since arg vars aren't local vars *)
    {st with oct = new_oct}

  let return ctx e f =
    let st = ctx.local in
    let new_oct =
      if AD.type_tracked (Cilfacade.fundec_return_type f) then (
        let oct' = AD.add_vars st.oct [V.return] in
        match e with
        | Some e ->
          assign_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global {st with oct = oct'} e (fun oct' e' ->
              AD.assign_exp oct' V.return e'
            )
        | None ->
          oct' (* leaves V.return unconstrained *)
      )
      else
        AD.copy st.oct
    in
    let local_vars =
      f.sformals @ f.slocals
      |> List.filter AD.varinfo_tracked
      |> List.map V.local
    in
    AD.remove_vars_with new_oct local_vars;
    {st with oct = new_oct}

  let combine ctx r fe f args fc fun_st =
    let st = ctx.local in
    if M.tracing then M.tracel "combine" "apron f: %a\n" d_varinfo f.svar;
    if M.tracing then M.tracel "combine" "apron formals: %a\n" (d_list "," d_varinfo) f.sformals;
    if M.tracing then M.tracel "combine" "apron args: %a\n" (d_list "," d_exp) args;
    let new_fun_oct = AD.add_vars fun_st.oct (AD.vars st.oct) in
    let arg_substitutes =
      Goblintutil.zip f.sformals args
      |> List.filter (fun (x, _) -> AD.varinfo_tracked x)
      |> List.map (Tuple2.map1 V.arg)
    in
    (* AD.substitute_exp_parallel_with new_fun_oct arg_substitutes; (* doesn't need to be parallel since exps aren't arg vars directly *) *)
    (* TODO: parallel version of assign_from_globals_wrapper? *)
    let ask = Analyses.ask_of_ctx ctx in
    let new_fun_oct = List.fold_left (fun new_fun_oct (var, e) ->
        assign_from_globals_wrapper ask ctx.global {st with oct = new_fun_oct} e (fun oct' e' ->
            (* not an assign, but still works? *)
            AD.substitute_exp oct' var e'
          )
      ) new_fun_oct arg_substitutes
    in
    let arg_vars = List.map fst arg_substitutes in
    if M.tracing then M.tracel "combine" "apron remove vars: %a\n" (docList (fun v -> Pretty.text (Var.to_string v))) arg_vars;
    AD.remove_vars_with new_fun_oct arg_vars; (* fine to remove arg vars that also exist in caller because unify from new_oct adds them back with proper constraints *)
    let new_oct = AD.keep_filter st.oct (fun var ->
        match V.find_metadata var with
        | Some Local -> true (* keep caller locals *)
        | Some Arg -> true (* keep caller args *)
        | _ -> false (* remove everything else (globals, global privs) *)
      )
    in
    let unify_oct = A.unify Man.mgr new_oct new_fun_oct in (* TODO: unify_with *)
    if M.tracing then M.tracel "combine" "apron unifying %a %a = %a\n" AD.pretty new_oct AD.pretty new_fun_oct AD.pretty unify_oct;
    let unify_st = {fun_st with oct = unify_oct} in
    if AD.type_tracked (Cilfacade.fundec_return_type f) then (
      let unify_st' = match r with
        | Some lv ->
          assign_to_global_wrapper (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg unify_st lv (fun st v ->
              AD.assign_var st.oct (V.local v) V.return
            )
        | None ->
          unify_st
      in
      AD.remove_vars_with unify_st'.oct [V.return]; (* mutates! *)
      unify_st'
    )
    else
      unify_st

  let special ctx r f args =
    let st = ctx.local in
    match LibraryFunctions.classify f.vname args with
    (* TODO: assert handling from https://github.com/goblint/analyzer/pull/278 *)
    | `Assert expression -> st
    | `Unknown "__goblint_check" -> st
    | `Unknown "__goblint_commit" -> st
    | `Unknown "__goblint_assert" -> st
    | _ ->
      let ask = Analyses.ask_of_ctx ctx in
      let invalidate_one st lv =
        assign_to_global_wrapper ask ctx.global ctx.sideg st lv (fun st v ->
            let oct' = AD.forget_vars st.oct [V.local v] in
            assert_type_bounds oct' v (* re-establish type bounds after forget *)
          )
      in
      let st' = match LibraryFunctions.get_invalidate_action f.vname with
        | Some fnc -> st (* nothing to do because only AddrOf arguments may be invalidated *)
        | None ->
          if GobConfig.get_bool "sem.unknown_function.invalidate.globals" then (
            let globals = foldGlobals !Cilfacade.current_file (fun acc global ->
                match global with
                | GVar (vi, _, _) when not (BaseUtil.is_static vi) ->
                  (Var vi, NoOffset) :: acc
                (* TODO: what about GVarDecl? *)
                | _ -> acc
              ) []
            in
            List.fold_left invalidate_one st globals
          )
          else
            st
      in
      (* invalidate lval if present *)
      match r with
      | Some lv -> invalidate_one st' lv
      | None -> st'


  let query ctx (type a) (q: a Queries.t): a Queries.result =
    let open Queries in
    let st = ctx.local in
    match q with
    | EvalInt e ->
      if M.tracing then M.traceli "evalint" "octapron query %a\n" d_exp e;
      let r = read_from_globals_wrapper (Analyses.ask_of_ctx ctx) ctx.global st e (fun oct' e' ->
          AD.eval_int oct' e'
        )
      in
      if M.tracing then M.traceu "evalint" "octapron query %a -> %a\n" d_exp e ID.pretty r;
      r
    | _ -> Result.top q


  (* Thread transfer functions. *)

  let threadenter ctx lval f args =
    let st = ctx.local in
    match Cilfacade.find_varinfo_fundec f with
    | fd ->
      (* TODO: HACK: Simulate enter_multithreaded for first entering thread to publish global inits before analyzing thread.
        Otherwise thread is analyzed with no global inits, reading globals gives bot, which turns into top, which might get published...
        sync `Thread doesn't help us here, it's not specific to entering multithreaded mode.
        EnterMultithreaded events only execute after threadenter and threadspawn. *)
      if not (ThreadFlag.is_multi (Analyses.ask_of_ctx ctx)) then
        ignore (Priv.enter_multithreaded (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg st);
      let st' = Priv.threadenter (Analyses.ask_of_ctx ctx) ctx.global st in
      let arg_vars =
        fd.sformals
        |> List.filter AD.varinfo_tracked
        |> List.map V.arg
      in
      let new_oct = AD.add_vars st'.oct arg_vars in
      [{st' with oct = new_oct}]
    | exception Not_found ->
      (* Unknown functions *)
      (* TODO: do something like base? *)
      failwith "octApron.threadenter: unknown function"

  let threadspawn ctx lval f args fctx =
    ctx.local

  let event ctx e octx =
    let st = ctx.local in
    match e with
    | Events.Lock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.lock (Analyses.ask_of_ctx octx) octx.global st addr
    | Events.Unlock addr when ThreadFlag.is_multi (Analyses.ask_of_ctx ctx) -> (* TODO: is this condition sound? *)
      Priv.unlock (Analyses.ask_of_ctx octx) octx.global octx.sideg st addr
    (* No need to handle escape because escaped variables are always referenced but this analysis only considers unreferenced variables. *)
    | Events.EnterMultiThreaded ->
      Priv.enter_multithreaded (Analyses.ask_of_ctx octx) octx.global octx.sideg st
    | _ ->
      st

  let sync ctx reason =
    Priv.sync (Analyses.ask_of_ctx ctx) ctx.global ctx.sideg ctx.local (reason :> [`Normal | `Join | `Return | `Init | `Thread])

  let init () =
    Priv.init ()

  let finalize () =
    Priv.finalize ()
end


let spec_module: (module MCPSpec) Lazy.t =
  lazy (
    let module Priv = (val OctApronPriv.get_priv ()) in
    let module Spec = SpecFunctor (Priv) in
    (module Spec)
  )

let get_spec (): (module MCPSpec) =
  Lazy.force spec_module

let after_config () =
  let module Spec = (val get_spec ()) in
  MCP.register_analysis (module Spec : MCPSpec);
  GobConfig.set_string "ana.path_sens[+]"  (Spec.name ())

let _ =
  AfterConfig.register after_config


let () =
  Printexc.register_printer
    (function
      | Apron.Manager.Error e ->
        let () = Apron.Manager.print_exclog Format.str_formatter e in
        Some(Printf.sprintf "Apron.Manager.Error\n %s" (Format.flush_str_formatter ()))
      | _ -> None (* for other exceptions *)
    )
