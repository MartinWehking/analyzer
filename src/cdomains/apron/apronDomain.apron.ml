open Prelude
open Cil
open Pretty
(* A binding to a selection of Apron-Domains *)
open Apron
open RelationDomain

module BI = IntOps.BigIntOps

module M = Messages

(** Resources for working with Apron:
    - OCaml API docs: https://antoinemine.github.io/Apron/doc/api/ocaml/index.html
    - C API docs (better function descriptions): https://antoinemine.github.io/Apron/doc/api/c/index.html
    - CEA 2007 slides (overview, mathematical descriptions): https://antoinemine.github.io/Apron/doc/papers/expose_CEA_2007.pdf
    - C API docs PDF (alternative mathematical descriptions): https://antoinemine.github.io/Apron/doc/api/c/apron.pdf
    - heterogeneous environments: https://link.springer.com/chapter/10.1007%2F978-3-030-17184-1_26 (Section 4.1) *)

let widening_thresholds_apron = ResettableLazy.from_fun (fun () ->
  let t = WideningThresholds.thresholds_incl_mul2 () in
  let r = List.map (fun x -> Apron.Scalar.of_mpqf @@ Mpqf.of_mpz @@ Z_mlgmpidl.mpz_of_z x) t in
  Array.of_list r
)

let reset_lazy () =
  ResettableLazy.reset widening_thresholds_apron

module Var =
struct
  include Var

  let equal x y = Var.compare x y = 0
end

module type Manager =
sig
  type mt
  type t = mt Apron.Manager.t
  val mgr : mt Apron.Manager.t
  val name : unit -> string
end

(** Manager for the Oct domain, i.e. an octagon domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Oct.html *)
module OctagonManager =
struct
  type mt = Oct.t

  (* Type of the manager *)
  type t = mt Manager.t

  (* Create the manager *)
  let mgr =  Oct.manager_alloc ()
  let name () = "Octagon"
end

(** Manager for the Polka domain, i.e. a polyhedra domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Polka.html *)
module PolyhedraManager =
struct
  (** We chose a the loose polyhedra here, i.e. with polyhedra with no strict inequalities *)
  type mt = Polka.loose Polka.t
  type t = mt Manager.t
  (* Create manager that fits to loose polyhedra *)
  let mgr = Polka.manager_alloc_loose ()
  let name () = "Polyhedra"
end

(** Another manager for the Polka domain but specifically for affine equalities.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Polka.html *)
module AffEqManager =
struct
  (** Affine equalities in apron used for comparison with our own implementation *)
  type mt = Polka.equalities Polka.t
  type t = mt Manager.t
  let mgr = Polka.manager_alloc_equalities ()
  let name () = "ApronAffEq"
end

(** Manager for the Box domain, i.e. an interval domain.
    For Documentation for the domain see: https://antoinemine.github.io/Apron/doc/api/ocaml/Box.html*)
module IntervalManager =
struct
  type mt = Box.t
  type t = mt Manager.t
  let mgr = Box.manager_alloc ()
  let name () = "Interval"
end

let manager =
  lazy (
    let options =
      ["octagon", (module OctagonManager: Manager);
       "interval", (module IntervalManager: Manager);
       "polyhedra", (module PolyhedraManager: Manager);
       "affeq", (module AffEqManager: Manager)]
    in
    let domain = (GobConfig.get_string "ana.apron.domain") in
    match List.assoc_opt domain options with
    | Some man -> man
    | None -> failwith @@ "Apron domain " ^ domain ^ " is not supported. Please check the ana.apron.domain setting."
  )

let get_manager (): (module Manager) =
  Lazy.force manager

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1

let int_of_scalar ?round (scalar: Scalar.t) =
  if Scalar.is_infty scalar <> 0 then (* infinity means unbounded *)
    None
  else
    match scalar with
    | Float f -> (* octD, boxD *)
      (* bound_texpr on bottom also gives Float even with MPQ *)
      let f_opt = match round with
        | Some `Floor -> Some (Float.floor f)
        | Some `Ceil -> Some (Float.ceil f)
        | None -> None
      in
      Option.map (fun f -> BI.of_bigint (Z.of_float f)) f_opt
    | Mpqf scalar -> (* octMPQ, boxMPQ, polkaMPQ *)
      let n = Mpqf.get_num scalar in
      let d = Mpqf.get_den scalar in
      let z_opt =
        if Mpzf.cmp_int d 1 = 0 then (* exact integer (denominator 1) *)
          Some n
        else
          begin match round with
            | Some `Floor -> Some (Mpzf.fdiv_q n d) (* floor division *)
            | Some `Ceil -> Some (Mpzf.cdiv_q n d) (* ceiling division *)
            | None -> None
          end
      in
      Option.map Z_mlgmpidl.z_of_mpzf z_opt
    | _ ->
      failwith ("int_of_scalar: unsupported: " ^ Scalar.to_string scalar)

module Bounds (Man: Manager) =
struct
  type t = Man.mt A.t

  let bound_texpr d texpr1 =
    let bounds = A.bound_texpr Man.mgr d texpr1 in
    let min = int_of_scalar ~round:`Ceil bounds.inf in
    let max = int_of_scalar ~round:`Floor bounds.sup in
    (min, max)
end

(** Convenience operations on A. *)
module AOps (Tracked: SharedFunctions.Tracked) (Man: Manager) =
struct
  module Convert = SharedFunctions.Convert (Bounds(Man))
  include SharedFunctions.EnvOps

  type t = Man.mt A.t

  let env t = A.env t

  let copy = A.copy Man.mgr

  let vars d = vars (A.env d)

  let vars_as_array d =
    let ivs, fvs = Environment.vars (A.env d) in
    assert (Array.length fvs = 0); (* shouldn't ever contain floats *)
    ivs

  (* marshal type: Abstract0.t and an array of var names *)
  type marshal = Man.mt Abstract0.t * string array

  let unmarshal ((abstract0, vs): marshal): t =
    let vars = Array.map Var.of_string vs in
    (* We do not have real-valued vars, so we pass an empty array in their place. *)
    let env = Environment.make vars [||] in
    {abstract0; env}

  let marshal (x: t): marshal =
    let vars = Array.map Var.to_string (vars_as_array x) in
    x.abstract0, vars

  let mem_var d v = Environment.mem_var (A.env d) v

  let add_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var env v))
      |> Array.of_enum
    in
    let env' = Environment.add env vs' [||] in
    A.change_environment_with Man.mgr nd env' false

  let add_vars d vs =
    let nd = copy d in
    add_vars_with nd vs;
    nd

  let remove_vars_with nd vs =
    let env = A.env nd in
    let vs' =
      vs
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var env v)
      |> Array.of_enum
    in
    let env' = Environment.remove env vs' in
    A.change_environment_with Man.mgr nd env' false

  let remove_vars d vs =
    let nd = copy d in
    remove_vars_with nd vs;
    nd

  let remove_filter_with nd f =
    let env = remove_filter (A.env nd) f in
    A.change_environment_with Man.mgr nd env false

  let remove_filter d f =
    let nd = copy d in
    remove_filter_with nd f;
    nd


  let keep_vars_with nd vs =
    let env = keep_vars (A.env nd) vs in
    A.change_environment_with Man.mgr nd env false

  let keep_vars d vs =
    let nd = copy d in
    keep_vars_with nd vs;
    nd

  let keep_filter_with nd f =
    let env = keep_filter (A.env nd) f in
    A.change_environment_with Man.mgr nd env false

  let keep_filter d f =
    let nd = copy d in
    keep_filter_with nd f;
    nd

  let forget_vars_with nd vs =
    (* Unlike keep_vars_with, this doesn't check mem_var, but assumes valid vars, like assigns *)
    let vs' = Array.of_list vs in
    A.forget_array_with Man.mgr nd vs' false

  let forget_vars d vs =
    let nd = copy d in
    forget_vars_with nd vs;
    nd

  let assign_exp_with nd v e ov =
    let no_ov = IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) in
    match Convert.texpr1_of_cil_exp nd (A.env nd) e no_ov with
    | texpr1 ->
      A.assign_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let assign_exp d v e ov =
    let nd = copy d in
    assign_exp_with nd v e ov;
    nd

  let assign_exp_parallel_with nd ves ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition assigns with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env ov e with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp -> None
        ))
      |> Enum.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel assign supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Enum.map (Tuple2.map2 Option.get)
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.assign_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Enum.map fst
      |> Array.of_enum
    in
    A.forget_array_with Man.mgr nd unsupported_vs false

  let assign_var_with nd v v' =
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.assign_texpr_with Man.mgr nd v texpr1 None

  let assign_var d v v' =
    let nd = copy d in
    assign_var_with nd v v';
    nd

  let assign_var_parallel_with nd vv's =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    let (vs, texpr1s) =
      vv's
      |> List.enum
      |> Enum.map (Tuple2.map2 (Texpr1.var env))
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.assign_texpr_array_with Man.mgr nd vs texpr1s None

  let assign_var_parallel d vv's =
    (* TODO: non-_with version? *)
    let nd = copy d in
    assign_var_parallel_with nd vv's;
    nd

  let assign_var_parallel' d vs v's = (* unpaired parallel assigns *)
    (* TODO: _with version? *)
    let env = A.env d in
    let vs = Array.of_list vs in
    let texpr1s =
      v's
      |> List.enum
      |> Enum.map (Texpr1.var env)
      |> Array.of_enum
    in
    A.assign_texpr_array Man.mgr d vs texpr1s None

  let substitute_exp_with nd v e ov =
    let no_ov = IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) in
    match Convert.texpr1_of_cil_exp nd (A.env nd) e no_ov with
    | texpr1 ->
      A.substitute_texpr_with Man.mgr nd v texpr1 None
    | exception Convert.Unsupported_CilExp ->
      forget_vars_with nd [v]

  let substitute_exp d v e ov =
    let nd = copy d in
    substitute_exp_with nd v e ov;
    nd

  let substitute_exp_parallel_with nd ves ov =
    (* TODO: non-_with version? *)
    let env = A.env nd in
    (* partition substitutes with supported and unsupported exps *)
    let (supported, unsupported) =
      ves
      |> List.enum
      |> Enum.map (Tuple2.map2 (fun e ->
          match Convert.texpr1_of_cil_exp nd env ov e with
          | texpr1 -> Some texpr1
          | exception Convert.Unsupported_CilExp -> None
        ))
      |> Enum.partition (fun (_, e_opt) -> Option.is_some e_opt)
    in
    (* parallel substitute supported *)
    let (supported_vs, texpr1s) =
      supported
      |> Enum.map (Tuple2.map2 Option.get)
      |> Enum.uncombine
      |> Tuple2.map Array.of_enum Array.of_enum
    in
    A.substitute_texpr_array_with Man.mgr nd supported_vs texpr1s None;
    (* forget unsupported *)
    let unsupported_vs =
      unsupported
      |> Enum.map fst
      |> Array.of_enum
    in
    A.forget_array_with Man.mgr nd unsupported_vs false

  let substitute_var_with nd v v' =
    (* TODO: non-_with version? *)
    let texpr1 = Texpr1.of_expr (A.env nd) (Var v') in
    A.substitute_texpr_with Man.mgr nd v texpr1 None

  let meet_with_tcons d tcons1 e =
    let earray = Tcons1.array_make (A.env d) 1 in
    Tcons1.array_set earray 0 tcons1;
    A.meet_tcons_array Man.mgr d earray

  let to_lincons_array d =
    A.to_lincons_array Man.mgr d

  let of_lincons_array (a: Apron.Lincons1.earray) =
    A.of_lincons_array Man.mgr a.array_env a
  let unify (a:t) (b:t) = A.unify Man.mgr a b
end


module type SPrintable =
sig
  include Printable.S
  (* Functions for bot and top for particular environment. *)
  val top_env: Environment.t -> t
  val bot_env: Environment.t -> t
  val is_top_env: t -> bool
  val is_bot_env: t -> bool
end

module DBase (Man: Manager): SPrintable with type t = Man.mt A.t =
struct
  type t = Man.mt A.t

  let name () = "apron"

  (* Functions for bot and top for particular environment. *)
  let top_env = A.top    Man.mgr
  let bot_env = A.bottom Man.mgr
  let is_top_env = A.is_top Man.mgr
  let is_bot_env = A.is_bottom Man.mgr
  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x

  let show (x:t) =
    Format.asprintf "%a (env: %a)" A.print x (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)
  let pretty () (x:t) = text (show x)

  let equal x y =
    Environment.equal (A.env x) (A.env y) && A.is_eq Man.mgr x y

  let hash (x:t) =
    A.hash Man.mgr x

  let compare (x:t) y: int =
    (* there is no A.compare, but polymorphic compare should delegate to Abstract0 and Environment compare's implemented in Apron's C *)
    Stdlib.compare x y
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nconstraints\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%a" A.print x)) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)))
end


module type SLattice =
sig
  include SPrintable
  include Lattice.S with type t := t
end

module DWithOps (Man: Manager) (D: SLattice with type t = Man.mt A.t) =
struct
  include D

  module Tracked = SharedFunctions.Tracked

  include AOps (Tracked) (Man)

  include Tracked
  module Bounds = Bounds(Man)

  (** Assert a constraint expression. *)
  let rec assert_cons d e negate (ov: bool Lazy.t) =
    let no_ov = IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp e) in
    match e with
    (* Apron doesn't properly meet with DISEQ constraints: https://github.com/antoinemine/apron/issues/37.
       Join Gt and Lt versions instead. *)
    | BinOp (Ne, lhs, rhs, intType) when not negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) negate ov in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) negate ov in
      join assert_gt assert_lt
    | BinOp (Eq, lhs, rhs, intType) when negate ->
      let assert_gt = assert_cons d (BinOp (Gt, lhs, rhs, intType)) (not negate) ov in
      let assert_lt = assert_cons d (BinOp (Lt, lhs, rhs, intType)) (not negate) ov in
      join assert_gt assert_lt
    | UnOp (LNot,e,_) -> assert_cons d e (not negate) ov
    | _ ->
      begin match Convert.tcons1_of_cil_exp d (A.env d) e negate no_ov with
        | tcons1 ->
          meet_with_tcons d tcons1 e
        | exception Convert.Unsupported_CilExp ->
          d
      end
end


module DLift (Man: Manager): SLattice with type t = Man.mt A.t =
struct
  include DBase (Man)

  let lift_var = Var.of_string "##LIFT##"

  (** Environment (containing a unique variable [lift_var]) only used for lifted bot and top. *)
  let lift_env = Environment.make [|lift_var|] [||]

  (* Functions for lifted bot and top to implement [Lattice.S]. *)
  let top () = top_env lift_env
  let bot () = bot_env lift_env
  let is_top x = Environment.equal (A.env x) lift_env && is_top_env x
  let is_bot x = Environment.equal (A.env x) lift_env && is_bot_env x

  (* Apron can not join two abstract values have different environments.
     That hapens when we do a join with dead code and for that reason we need
     to handle joining with bottom manually.
     A similar if-based structure with is_top and is_bottom is also there for:
     meet, widen, narrow, equal, leq.*)

  let join x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.tracel "apron" "join %a %a\n" pretty x pretty y;
      A.join (Man.mgr) x y
      (* TODO: return lifted top if different environments? and warn? *)
    )

  let meet x y =
    if is_top x then y else
    if is_top y then x else
      A.meet Man.mgr x y
  (* TODO: return lifted bot if different environments? and warn? *)

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x (* TODO: is this right? *)
    else
      A.widening (Man.mgr) x y
  (* TODO: return lifted top if different environments? and warn? *)

  let narrow = meet

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else (
      if M.tracing then M.tracel "apron" "leq %a %a\n" pretty x pretty y;
      Environment.equal (A.env x) (A.env y) && A.is_leq (Man.mgr) x y
      (* TODO: warn if different environments? *)
    )

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module D (Man: Manager) = DWithOps (Man) (DLift (Man))


(** With heterogeneous environments. *)
module DHetero (Man: Manager): SLattice with type t = Man.mt A.t =
struct
  include DBase (Man)



  let gce (x: Environment.t) (y: Environment.t): Environment.t =
    let (xi, xf) = Environment.vars x in
    (* TODO: check type compatibility *)
    let i = Array.filter (Environment.mem_var y) xi in
    let f = Array.filter (Environment.mem_var y) xf in
    Environment.make i f

  let join x y =
    let x_env = A.env x in
    let y_env = A.env y in
    let c_env = gce x_env y_env in
    let x_c = A.change_environment Man.mgr x c_env false in
    let y_c = A.change_environment Man.mgr y c_env false in
    let join_c = A.join Man.mgr x_c y_c in
    let j_env = Environment.lce x_env y_env in
    A.change_environment Man.mgr join_c j_env false

  (* TODO: move to AOps *)
  let meet_lincons d lincons1 =
    let earray = Lincons1.array_make (A.env d) 1 in
    Lincons1.array_set earray 0 lincons1;
    A.meet_lincons_array Man.mgr d earray

  let strengthening j x y =
    (* TODO: optimize strengthening *)
    if M.tracing then M.traceli "apron" "strengthening %a\n" pretty j;
    let x_env = A.env x in
    let y_env = A.env y in
    let j_env = A.env j in
    let x_j = A.change_environment Man.mgr x j_env false in
    let y_j = A.change_environment Man.mgr y j_env false in
    let x_cons = A.to_lincons_array Man.mgr x_j in
    let y_cons = A.to_lincons_array Man.mgr y_j in
    let try_add_con j con1 =
      if M.tracing then M.tracei "apron" "try_add_con %s\n" (Format.asprintf "%a" (Lincons1.print: Format.formatter -> Lincons1.t -> unit) con1);
      let t = meet_lincons j con1 in
      let t_x = A.change_environment Man.mgr t x_env false in
      let t_y = A.change_environment Man.mgr t y_env false in
      let leq_x = A.is_leq Man.mgr x t_x in
      let leq_y = A.is_leq Man.mgr y t_y in
      if M.tracing then M.trace "apron" "t: %a\n" pretty t;
      if M.tracing then M.trace "apron" "t_x (leq x %B): %a\n" leq_x pretty t_x;
      if M.tracing then M.trace "apron" "t_y (leq y %B): %a\n" leq_y pretty t_y;
      if leq_x && leq_y then (
        if M.tracing then M.traceu "apron" "added\n";
        t
      )
      else (
        if M.tracing then M.traceu "apron" "not added\n";
        j
      )
    in
    let lincons1_array_of_earray (earray: Lincons1.earray) =
      Array.init (Lincons1.array_length earray) (Lincons1.array_get earray)
    in
    let x_cons1 = lincons1_array_of_earray x_cons in
    let y_cons1 = lincons1_array_of_earray y_cons in
    let cons1 =
      (* Whether [con1] contains a var in [env]. *)
      let env_exists_mem_con1 env con1 =
        try
          Lincons1.iter (fun _ var ->
              if Environment.mem_var env var then
                raise Not_found
            ) con1;
          false
        with Not_found ->
          true
      in
      (* Heuristically reorder constraints to pass 36/12 with singlethreaded->multithreaded mode switching. *)
      (* Put those constraints which strictly are in one argument's env first, to (hopefully) ensure they remain. *)
      let (x_cons1_some_y, x_cons1_only_x) = Array.partition (env_exists_mem_con1 y_env) x_cons1 in
      let (y_cons1_some_x, y_cons1_only_y) = Array.partition (env_exists_mem_con1 x_env) y_cons1 in
      Array.concat [x_cons1_only_x; y_cons1_only_y; x_cons1_some_y; y_cons1_some_x]
    in
    let j = Array.fold_left try_add_con j cons1 in
    if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
    j

  let empty_env = Environment.make [||] [||]

  let bot () =
    top_env empty_env

  let top () =
    failwith "D2.top"

  let is_bot = equal (bot ())
  let is_top _ = false

  let strengthening_enabled = GobConfig.get_bool "ana.apron.strengthening"

  let join x y =
    (* just to optimize joining folds, which start with bot *)
    if is_bot x then
      y
    else if is_bot y then
      x
    else (
      if M.tracing then M.traceli "apron" "join %a %a\n" pretty x pretty y;
      let j = join x y in
      if M.tracing then M.trace "apron" "j = %a\n" pretty j;
      let j =
        if strengthening_enabled then
          strengthening j x y
        else
          j
      in
      if M.tracing then M.traceu "apron" "-> %a\n" pretty j;
      j
    )

  let meet x y =
    A.unify Man.mgr x y

  let leq x y =
    (* TODO: float *)
    let x_env = A.env x in
    let y_env = A.env y in
    let (x_vars, _) = Environment.vars x_env in
    if Array.for_all (Environment.mem_var y_env) x_vars then (
      let y' = A.change_environment Man.mgr y x_env false in
      A.is_leq Man.mgr x y'
    )
    else
      false

  let widen x y =
    let x_env = A.env x in
    let y_env = A.env y in
    if Environment.equal x_env y_env  then
      if GobConfig.get_bool "ana.apron.threshold_widening" && Oct.manager_is_oct Man.mgr then
        let octmgr = Oct.manager_to_oct Man.mgr in
        let ts = ResettableLazy.force widening_thresholds_apron in
        let x_oct = Oct.Abstract1.to_oct x in
        let y_oct = Oct.Abstract1.to_oct y in
        let r = Oct.widening_thresholds octmgr (Abstract1.abstract0 x_oct) (Abstract1.abstract0 y_oct) ts in
        Oct.Abstract1.of_oct {x_oct with abstract0 = r}
      else
        A.widening Man.mgr x y
    else
      y (* env increased, just use joined value in y, assuming env doesn't increase infinitely *)

  (* TODO: better narrow *)
  let narrow x y = x

  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y
end

module type S2 =
sig
  module Man: Manager
  module Tracked : SharedFunctions.Tracked
  include module type of AOps (Tracked) (Man)
  include SharedFunctions.Tracked
  include SLattice with type t = Man.mt A.t

  val exp_is_cons : exp -> bool
  val assert_cons : t -> exp -> bool Lazy.t -> t
  val assert_inv : t -> exp -> bool -> t
  val check_assert : t -> exp -> [> `False | `Top | `True ]
  val eval_interval_expr : t -> exp -> Z.t option * Z.t option
  val eval_int : t -> exp -> Queries.ID.t
end

module D2Complete (Man: Manager) =
struct
  type var = SharedFunctions.Var.t
  type lconsarray = Lincons1.earray
  include DWithOps (Man) (DHetero (Man))
  module Man = Man
end

module AD2Complete (Man: Manager) = (*ToDo Improve module structure...*)
struct
  module D2 = D2Complete (Man)
  include D2
  include SharedFunctions.AssertionModule (D2)
end

module D2 (Man: Manager): (RelD2 with type var = Var.t) =
struct
  include AD2Complete(Man)
end
