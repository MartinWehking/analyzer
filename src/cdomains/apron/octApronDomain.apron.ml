open Prelude
open Cil
open Pretty
(* For Apron implementation of octagons *)
open Apron

module M = Messages


module Man =
struct
  (* Manager type, parameter for the command below *)
  type mt = Oct.t
  (* type mt = Polka.equalities Polka.t *)
  (* type mt = Polka.loose Polka.t *)
  (* A type of manager allocated by the underlying octagon domain *)
  type t = mt Manager.t

  (* Allocate a new manager to manipulate octagons *)
  let mgr = Oct.manager_alloc ()
  (* let mgr = Polka.manager_alloc_equalities () *)
  (* let mgr = Polka.manager_alloc_loose () *)
  (* Making an environment from a set of integer and real variables.
  Raise Failure in case of name conflict.
  In this case the environment is empty to begin with. *)
  let eenv = Environment.make [||] [||]
end

(** Conversion from CIL expressions to Apron. *)
module Convert =
struct
  open Texpr1
  open Tcons1

  exception Unsupported_CilExp

  let rec texpr1_expr_of_cil_exp = function
    | Lval (Var v, NoOffset) when isIntegralType v.vtype && not v.vglob ->
      Var (Var.of_string v.vname)
    | Const (CInt64 (i, _, _)) ->
      Cst (Coeff.s_of_int (Int64.to_int i))
    | UnOp (Neg, e, _) ->
      Unop (Neg, texpr1_expr_of_cil_exp e, Int, Near)
    | BinOp (PlusA, e1, e2, _) ->
      Binop (Add, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
    | BinOp (MinusA, e1, e2, _) ->
      Binop (Sub, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
    | BinOp (Mult, e1, e2, _) ->
      Binop (Mul, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
    | BinOp (Div, e1, e2, _) ->
      Binop (Div, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Zero)
    | BinOp (Mod, e1, e2, _) ->
      Binop (Mod, texpr1_expr_of_cil_exp e1, texpr1_expr_of_cil_exp e2, Int, Near)
    | CastE (TInt _, e) ->
      Unop(Cast, texpr1_expr_of_cil_exp e, Int, Zero)
    | _ ->
      raise Unsupported_CilExp

  let rec env_mem_texpr1_expr env = function
    | Cst _ -> true
    | Var var -> Environment.mem_var env var
    | Unop (_, e, _, _) -> env_mem_texpr1_expr env e
    | Binop (_, e1, e2, _, _) -> env_mem_texpr1_expr env e1 && env_mem_texpr1_expr env e2

  let texpr1_of_texpr1_expr env texpr1_expr =
    (* TODO: move this check right into texpr1_expr_of_cil_exp? *)
    if not (env_mem_texpr1_expr env texpr1_expr) then
      raise Unsupported_CilExp;
    Texpr1.of_expr env texpr1_expr

  let texpr1_of_cil_exp env e =
    texpr1_of_texpr1_expr env (texpr1_expr_of_cil_exp e)

  let tcons1_of_cil_exp env e negate =
    let (texpr1_plus, texpr1_minus, typ) =
      match e with
      | BinOp (r, e1, e2, _) ->
        let texpr1_1 = texpr1_expr_of_cil_exp e1 in
        let texpr1_2 = texpr1_expr_of_cil_exp e2 in
        begin match r with
          | Lt -> (texpr1_2, texpr1_1, SUP)
          | Gt -> (texpr1_1, texpr1_2, SUP)
          | Le -> (texpr1_2, texpr1_1, SUPEQ)
          | Ge -> (texpr1_1, texpr1_2, SUPEQ)
          | Eq -> (texpr1_1, texpr1_2, EQ)
          | Ne -> (texpr1_1, texpr1_2, DISEQ)
          | _ -> raise Unsupported_CilExp
        end
      | _ -> raise Unsupported_CilExp
    in
    let inverse_typ = function
      | EQ -> DISEQ
      | DISEQ -> EQ
      | SUPEQ -> SUP
      | SUP -> SUPEQ
      | EQMOD x -> EQMOD x
    in
    let (texpr1_plus, texpr1_minus, typ) =
      if negate then
        (texpr1_minus, texpr1_plus, inverse_typ typ)
      else
        (texpr1_plus, texpr1_minus, typ)
    in
    let texpr1' = Binop (Sub, texpr1_plus, texpr1_minus, Int, Near) in
    Tcons1.make (texpr1_of_texpr1_expr env texpr1') typ
end

(* Generic operations on abstract values at level 1 of interface, there is also Abstract0 *)
module A = Abstract1

(** Convenience operations on A. *)
module AOps =
struct
  type t = Man.mt A.t

  let is_chosen (v:string) =
    let oct_vars =  List.map Json.jsonString (GobConfig.get_list "ana.octapron.vars") in
    if List.length oct_vars == 0 then
      true
    else
      (* let () = print_endline (String.concat ", " oct_vars) in *)
      List.mem ("\""^v^"\"") oct_vars

  let get_vars d =
    let xs, ys = Environment.vars (A.env d) in
    assert (Array.length ys = 0); (* shouldn't ever contain floats *)
    List.of_enum (Array.enum xs)

  let var_in_env (v:string) d =
    if (is_chosen v) then
      let existing_vars_int = get_vars d in
      let existing_var_names_int = List.map (fun v -> Var.to_string v) existing_vars_int in
      List.mem v existing_var_names_int
    else
      false

  let assign_var_eq_with d v v' =
    if var_in_env v d then
      A.assign_texpr_with Man.mgr d (Var.of_string v)
        (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

  let substitute_var_eq_with d v v' =
    if var_in_env v d then
      A.substitute_texpr_with Man.mgr d (Var.of_string v)
        (Texpr1.of_expr (A.env d) (Var (Var.of_string v'))) None

  let assign_var_with d v e =
    (* ignore (Pretty.printf "assign_var_with %a %s %a\n" pretty d v d_plainexp e); *)
    if var_in_env v d then
      begin try
          let exp = Cil.constFold false e in
          let env = A.env d in
          A.assign_texpr_with Man.mgr d (Var.of_string v)
            (Convert.texpr1_of_cil_exp env exp) None
        with Convert.Unsupported_CilExp ->
          A.forget_array_with Man.mgr d [|Var.of_string v|] false
          (* | Manager.Error q -> *)
          (* ignore (Pretty.printf "Manager.Error: %s\n" q.msg); *)
          (* ignore (Pretty.printf "Manager.Error: assign_var_with _ %s %a\n" v d_plainexp e); *)
          (* raise (Manager.Error q) *)
      end

  let assign_var d v e =
    if is_chosen v then
      let newd = A.copy Man.mgr d in
      assign_var_with newd v e;
      newd
    else
      d

  let forget_all_with d xs =
    let xs = List.filter (fun elem -> var_in_env elem d) xs in
    A.forget_array_with Man.mgr d (Array.of_enum (List.enum (List.map Var.of_string xs))) false

  let forget_all d xs =
    let newd = A.copy Man.mgr d in
    forget_all_with newd xs;
    newd

  let substitute_var_with d v e =
    (* ignore (Pretty.printf "substitute_var_with %a %s %a\n" pretty d v d_plainexp e); *)
    begin try
        let exp = Cil.constFold false e in
        let env = A.env d in
        A.substitute_texpr_with Man.mgr d (Var.of_string v)
          (Convert.texpr1_of_cil_exp env exp) None
      with Convert.Unsupported_CilExp ->
        A.forget_array_with Man.mgr d [|Var.of_string v|] false
        (* | Manager.Error q ->
           ignore (Pretty.printf "Manager.Error: %s\n" q.msg);
           ignore (Pretty.printf "Manager.Error: assign_var_with _ %s %a\n" v d_plainexp e);
           raise (Manager.Error q) *)
    end

  let add_vars_with newd newis =
    (* TODO: why is this necessary? *)
    let rec remove_duplicates list =
      match list with
      | [] -> []
      | head::tail -> head::(remove_duplicates (List.filter (fun x -> x <> head) tail)) in
    let oldis = get_vars newd in
    let environment = (A.env newd) in
    let newis = remove_duplicates newis in
    (* why is this not done by remove_duplicates already? *)
    let cis = List.filter (fun x -> not (List.mem x oldis) && (not (Environment.mem_var environment x))) (List.map Var.of_string newis) in (* TODO: why is the mem_var check necessary? *)
    let cis = Array.of_enum (List.enum cis) in
    let newenv = Environment.add environment cis [||] in
    A.change_environment_with Man.mgr newd newenv false

  let add_vars d vars =
    let newd = A.copy Man.mgr d in
    add_vars_with newd vars;
    newd

  let remove_all_but_with d xs =
    let is' = get_vars d in
    let vs = List.filter (fun x -> not (List.mem (Var.to_string x) xs)) is' in
    let env = Environment.remove (A.env d) (Array.of_enum (List.enum vs)) in
    A.change_environment_with Man.mgr d env false

  let remove_all_with d xs =
    if not (List.is_empty xs) then
      (* let vars = List.filter (fun v -> isIntegralType v.vtype) xs in *)
      let vars = Array.of_enum (List.enum (List.map (fun v -> Var.of_string v) xs)) in
      let existing_vars_int = get_vars d in
      let vars_filtered = List.filter (fun elem -> List.mem elem existing_vars_int) (Array.to_list vars) in
      let env = Environment.remove (A.env d) (Array.of_list vars_filtered) in
      A.change_environment_with Man.mgr d env false

  let remove_all d vars =
    let newd = A.copy Man.mgr d in
    remove_all_with newd vars;
    newd

  let copy = A.copy Man.mgr
end

module D =
struct
  include AOps

  let name () = "OctApron"

  let topE = A.top    Man.mgr
  let botE = A.bottom Man.mgr

  let top () = topE Man.eenv
  let bot () = botE Man.eenv
  let is_top = A.is_top    Man.mgr
  let is_bot = A.is_bottom Man.mgr

  let to_yojson x = failwith "TODO implement to_yojson"
  let invariant _ _ = Invariant.none
  let tag _ = failwith "Std: no tag"
  let arbitrary () = failwith "no arbitrary"
  let relift x = x

  let show (x:t) =
    Format.asprintf "%a (env: %a)" A.print x (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)

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
    else
      A.join (Man.mgr) x y

  let meet x y =
    if is_top x then y else
    if is_top y then x else
      A.meet Man.mgr x y

  let widen x y =
    if is_bot x then
      y
    else if is_bot y then
      x
    else
      A.widening (Man.mgr) x y

  let narrow = meet

  let equal x y =
    if is_bot x then is_bot y
    else if is_bot y then false
    else if is_top x then is_top y
    else if is_top y then false
    else A.is_eq Man.mgr x y

  let leq x y =
    if is_bot x || is_top y then true else
    if is_bot y || is_top x then false else
      A.is_leq (Man.mgr) x y

  let hash (x:t) = Hashtbl.hash x
  let compare (x:t) y = Stdlib.compare x y
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nconstraints\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%a" A.print x)) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (A.env x)))
  let pretty () (x:t) = text (show x)
  let pretty_diff () (x,y) = text "pretty_diff"

  let typesort =
    let f is v =
      if isIntegralType v.vtype then
        if GobConfig.get_bool "ana.octapron.no_uints" then
          if Cil.isSigned (Cilfacade.get_ikind v.vtype) then
            v.vname::is
          else
            is
        else
          v.vname::is
      else
        is
    in
    List.fold_left f []

  (* Assert an invariant *)
  (* Gives the result of the meet operation of the given octagon
  with the linear constraints coming from the given expression *)
  let assert_inv d x b =
    try
      let x = match x with
        | BinOp ((Lt | Gt | Le | Ge | Eq | Ne), _, _, _) -> x
        (* For expressions x that aren't a BinOp with a comparison operator,
         assert(x) will be converted it to assert(x != 0) *)
        | _ -> BinOp (Ne, x, (Const (CInt64(Int64.of_int 0, IInt, None))), intType)
        in
      match x with
      (* TODO: why was this ever necessary? it is unsound for 36/18 *)
      (* | BinOp (Ne, lhd, rhs, intType) ->
        let assert_gt = assert_inv d (BinOp (Gt, lhd, rhs, intType)) b in
        let assert_lt = assert_inv d (BinOp (Lt, lhd, rhs, intType)) b in
        if not (is_bot assert_gt) then
          assert_gt
        else
          assert_lt *)
      | _ ->
        (* Linear constraints from an expression x in an environment of octagon d *)
        let tcons1 = Convert.tcons1_of_cil_exp (A.env d) x b in
        (* Get the underlying linear constraint of level 0.
           Modifying the constraint of level 0 (not advisable)
           modifies correspondingly the linear constraint and conversely,
           except for changes of environments *)
        let ea = { Tcons1.tcons0_array = [|Tcons1.get_tcons0 tcons1 |]
                 ; array_env = A.env d
                 }
        in
        (* We perform a meet of the current octagon with the linear constraints
           that come from the expression we wish to assert. *)
        A.meet_tcons_array Man.mgr d ea
    with Convert.Unsupported_CilExp -> d

  (* Creates the opposite invariant and assters it *)
  (* TODO: why is this necessary if assert_inv has boolean argument? *)
  let assert_op_inv d x b =
    (* if assert(x) then convert it to assert(x != 0) *)
    let x = match x with
    | Lval (Var v,NoOffset) when isIntegralType v.vtype ->
      BinOp (Ne, x, (Const (CInt64(Int64.of_int 0, IInt, None))), intType)
    | _ -> x in
    try
      match x with
        | BinOp (Ne, lhd, rhs, intType) ->
          assert_inv d (BinOp (Eq, lhd, rhs, intType)) b

        | BinOp (Eq, lhd, rhs, intType) ->
          (* FIXME: this is probably wrong here too? *)
          let assert_gt = assert_inv d (BinOp (Gt, lhd, rhs, intType)) b in
          let assert_lt = assert_inv d (BinOp (Lt, lhd, rhs, intType)) b in
          if not (is_bot assert_gt) then
            assert_gt
          else
            assert_lt

        | BinOp (Lt, lhd, rhs, intType) ->
          assert_inv d (BinOp (Ge, lhd, rhs, intType)) b

        | BinOp (Gt, lhd, rhs, intType) ->
          assert_inv d (BinOp (Le, lhd, rhs, intType)) b

        | BinOp (Le, lhd, rhs, intType) ->
          assert_inv d (BinOp (Gt, lhd, rhs, intType)) b

        | BinOp (Ge, lhd, rhs, intType) ->
          assert_inv d (BinOp (Lt, lhd, rhs, intType)) b

        | UnOp(LNot, e, t) ->
          assert_inv d e b

        | _ ->  assert_inv d x b
    with Convert.Unsupported_CilExp -> d

  let check_assert (e:exp) state =
    match e with
    | Const (CInt64(i, kind, str)) -> `Top (* Octagon doesn't handle constant integers as assertions *)
    | CastE(t, e) -> `Top (* Octagon doesn't handle casts as assertions *)
    | Const(CChr c) -> `Top (*  Octagon doesn't handle character constants as assertions *)
    | _ ->
      let result_state = (assert_inv state e false) in
      let result_state_op = (assert_op_inv state e false) in (* TODO: why not use assert_inv with true? *)
      if is_bot result_state then
        `False
      else if is_bot result_state_op then
        `True
      else
        `Top

  let get_int_interval_for_cil_exp d cil_exp =
    let get_int_for_apron_scalar (scalar: Scalar.t) =
      match scalar with
      | Float scalar -> Some (Stdlib.int_of_float scalar)
      | Mpqf scalar ->
        begin
          match Mpqf.to_string scalar with
          (* apron has an internal representation of -1/0 as -infinity and 1/0 as infinity.*)
          | "-1/0" | "1/0" -> None
          | _ -> Some (Stdlib.int_of_float (Mpqf.to_float scalar))
        end
      | Mpfrf scalar -> Some (Stdlib.int_of_float (Mpfrf.to_float scalar)) in
    try
      let texpr1 = Convert.texpr1_of_cil_exp (A.env d) (Cil.constFold false cil_exp) in
      let interval_of_variable = A.bound_texpr Man.mgr d texpr1 in
      let infimum = get_int_for_apron_scalar interval_of_variable.inf in
      let supremum = get_int_for_apron_scalar interval_of_variable.sup in
      match infimum, supremum with
      | Some infimum, Some supremum -> Some (Int64.of_int (infimum)),  Some (Int64.of_int (supremum))
      | Some infimum, None -> Some (Int64.of_int (-infimum)), None
      | None, Some supremum ->  None, Some (Int64.of_int (-supremum))
      | _, _ -> None, None
    with Convert.Unsupported_CilExp -> None, None

  let get_int_val_for_cil_exp d cil_exp =
    match get_int_interval_for_cil_exp d cil_exp with
    | Some infimum, Some supremum ->
      begin
        if (supremum = infimum) then
          (Some infimum)
        else None
      end
    | _ -> None

  let cil_exp_equals d exp1 exp2 =
    if (is_bot d) then false
    else
      begin
        let compare_expression = BinOp (Eq, exp1, exp2, TInt (IInt, [])) in
        (* We compare the octagon with the octagon we get by performing meet of it with the linear constraints coming from the expression *)
        let resulting_oct = (assert_inv d compare_expression false) in
        let comp_result = equal d resulting_oct in
        comp_result
      end


  let assign_var_handling_underflow_overflow oct v e =
    match v.vtype with
    | TInt (ikind, _)->
      let signed = Cil.isSigned ikind in
      let new_oct = assign_var oct v.vname e in
      let lower_limit, upper_limit = IntDomain.Size.range_big_int ikind in
      let check_max =
        check_assert (BinOp (Le, Lval (Cil.var @@ v), (Cil.kintegerCilint ikind (Cilint.cilint_of_big_int upper_limit)), intType)) new_oct in
      let check_min =
        check_assert (BinOp (Ge, Lval (Cil.var @@ v), (Cil.kintegerCilint ikind (Cilint.cilint_of_big_int lower_limit)), intType)) new_oct in
      if signed then
        if check_max <> `True || check_min <> `True then
          if GobConfig.get_bool "ana.octapron.no_signed_overflow" then
            new_oct
          else
            (* Signed overflows are undefined behavior, so octagon goes to top if it might have happened. *)
            topE (A.env oct)
        else
          new_oct
      else if check_max <> `True || check_min <> `True then
        (* Unsigned overflows are defined, but for now
           the variable in question goes to top if there is a possibility of overflow. *)
        let () = forget_all_with oct [v.vname] in
        oct
      else
        new_oct
    | _ -> oct
end

(** With heterogeneous environments. *)
module D2 =
struct
  include D

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

  let strengthening j x y =
    if M.tracing then M.traceli "apron" "strengthening %a\n" pretty j;
    let x_env = A.env x in
    let y_env = A.env y in
    let j_env = A.env j in
    let x_j = A.change_environment Man.mgr x j_env false in
    let y_j = A.change_environment Man.mgr y j_env false in
    let x_cons = A.to_lincons_array Man.mgr x_j in
    let y_cons = A.to_lincons_array Man.mgr y_j in
    let try_add_con j con1 =
      let con0: Lincons0.t = con1.Lincons1.lincons0 in
      let cons1: Lincons1.earray = {lincons0_array = [|con0|]; array_env = j_env} in
      if M.tracing then M.tracei "apron" "try_add_con %s\n" (Format.asprintf "%a" (Lincons1.array_print: Format.formatter -> Lincons1.earray -> unit) cons1);
      let t = A.meet_lincons_array Man.mgr j cons1 in
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
    let x_cons1 = Array.map (fun con0 ->
        {Lincons1.lincons0 = con0; env = x_cons.array_env}
      ) x_cons.lincons0_array
    in
    let y_cons1 = Array.map (fun con0 ->
        {Lincons1.lincons0 = con0; env = y_cons.array_env}
      ) y_cons.lincons0_array
    in
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

  let bot () =
    top ()

  let top () =
    failwith "D2.top"

  let equal x y =
    Environment.equal (A.env x) (A.env y) && A.is_eq Man.mgr x y

  let is_bot = equal (bot ())
  let is_top _ = false

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
      let j = strengthening j x y in
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
    if Environment.equal x_env y_env then
      A.widening Man.mgr x y (* widen if env didn't increase *)
    else
      y (* env increased, just use joined value in y, assuming env doesn't increase infinitely *)

  (* TODO: better narrow *)
  let narrow x y = x


  (* Extra helper functions, some just to bypass chosen vars. *)
  let mem_var d v = Environment.mem_var (A.env d) v

  let assign_var' d v v' =
    A.assign_texpr Man.mgr d v (Texpr1.var (A.env d) v') None

  let add_vars_int d vs =
    (* TODO: add_vars which takes Var arguments instead *)
    add_vars d (List.map Var.to_string vs)

  let remove_vars d vs =
    (* TODO: remove_all which takes Var arguments instead *)
    remove_all d (List.map Var.to_string vs)

  let keep_vars d vs =
    let d' = A.copy Man.mgr d in
    (* TODO: remove_all_but_with which takes Var arguments instead *)
    remove_all_but_with d' (List.map Var.to_string vs);
    d'

  let parallel_assign_vars d vs v's =
    let env = A.env d in
    let vs = Array.of_list vs in
    let v's =
      v's
      |> List.enum
      |> Enum.map (Texpr1.var env)
      |> Array.of_enum
    in
    A.assign_texpr_array Man.mgr d vs v's None

  let forget_vars d vs =
    (* TODO: forget_all which takes Var arguments instead *)
    forget_all d (List.map Var.to_string vs)
end


(* Copy-paste from BaseDomain... *)
type 'a octaproncomponents_t = {
  oct: D2.t;
  priv: 'a;
} [@@deriving eq, ord, to_yojson]

module OctApronComponents (PrivD: Lattice.S):
sig
  include Lattice.S with type t = PrivD.t octaproncomponents_t
  val op_scheme: (D2.t -> D2.t -> D2.t) -> (PrivD.t -> PrivD.t -> PrivD.t) -> t -> t -> t
end =
struct
  type t = PrivD.t octaproncomponents_t [@@deriving eq, ord, to_yojson]

  include Printable.Std
  open Pretty
  let hash r  = D2.hash r.oct + PrivD.hash r.priv * 33


  let show r =
    let first  = D2.show r.oct in
    let third  = PrivD.show r.priv in
    "(" ^ first ^ ", " ^ third  ^ ")"

  let pretty () r =
    text "(" ++
    D2.pretty () r.oct
    ++ text ", " ++
    PrivD.pretty () r.priv
    ++ text ")"

  let printXml f r =
    BatPrintf.fprintf f "<value>\n<map>\n<key>\n%s\n</key>\n%a<key>\n%s\n</key>\n%a</map>\n</value>\n" (Goblintutil.escape (D2.name ())) D2.printXml r.oct (Goblintutil.escape (PrivD.name ())) PrivD.printXml r.priv

  let name () = D2.name () ^ " * " ^ PrivD.name ()

  let invariant c {oct; priv} =
    Invariant.(D2.invariant c oct && PrivD.invariant c priv)

  let of_tuple(oct, priv):t = {oct; priv}
  let to_tuple r = (r.oct, r.priv)

  let arbitrary () =
    let tr = QCheck.pair (D2.arbitrary ()) (PrivD.arbitrary ()) in
    QCheck.map ~rev:to_tuple of_tuple tr

  let bot () = { oct = D2.bot (); priv = PrivD.bot ()}
  let is_bot {oct; priv} = D2.is_bot oct && PrivD.is_bot priv
  let top () = {oct = D2.top (); priv = PrivD.bot ()}
  let is_top {oct; priv} = D2.is_top oct && PrivD.is_top priv

  let leq {oct=x1; priv=x3 } {oct=y1; priv=y3} =
    D2.leq x1 y1 && PrivD.leq x3 y3

  let pretty_diff () (({oct=x1; priv=x3}:t),({oct=y1; priv=y3}:t)): Pretty.doc =
    if not (D2.leq x1 y1) then
      D2.pretty_diff () (x1,y1)
    else
      PrivD.pretty_diff () (x3,y3)

  let op_scheme op1 op3 {oct=x1; priv=x3} {oct=y1; priv=y3}: t =
    {oct = op1 x1 y1; priv = op3 x3 y3 }
  let join = op_scheme D2.join PrivD.join
  let meet = op_scheme D2.meet PrivD.meet
  let widen = op_scheme D2.widen PrivD.widen
  let narrow = op_scheme D2.narrow PrivD.narrow
end


module Var =
struct
  include Var

  let equal x y = Var.compare x y = 0
end

module type VarMetadata =
sig
  type t
  val var_name: t -> string
end

module VarMetadataTbl (VM: VarMetadata) =
struct
  module VH = Hashtbl.Make (Var)

  let vh = VH.create 113

  let make_var metadata =
    let var = Var.of_string (VM.var_name metadata) in
    VH.replace vh var metadata;
    var

  let find_metadata var =
    VH.find_option vh var
end

module GVM =
struct
  include CilType.Varinfo
  let var_name g = g.vname
end

module GV =
struct
  include VarMetadataTbl (GVM)

  let make g = make_var g
end
