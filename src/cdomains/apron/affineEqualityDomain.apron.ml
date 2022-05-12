open Prelude
open Pretty
module M = Messages
open Apron
open VectorMatrix

module VarManagement (Vec: AbstractVector) (Mx: AbstractMatrix)=
struct
  include SharedFunctions.EnvOps
  module Vector = Vec (Mpqf)
  module Matrix = Mx(Mpqf) (Vec)

  type t = {
    d :  Matrix.t;
    env : Environment.t
  }

  let bot () =
    {d = Matrix.empty (); env = Environment.make [||] [||]}
  let is_bot t = Environment.size t.env = 0
  let is_bot_env t = Environment.size t.env = 0
  let top () = failwith "D2.top"

  let is_top t = not @@ is_bot_env t && Matrix.is_empty t.d

  let copy_pt t = t

  let dim_add (ch: Apron.Dim.change) m =
    Array.iteri (fun i x -> ch.dim.(i) <- x + i) ch.dim;
    Matrix.add_empty_columns m ch.dim

  let dim_remove (ch: Apron.Dim.change) m del =
    if Array.length ch.dim = 0 || Matrix.is_empty m then m else (
      Array.iteri (fun i x-> ch.dim.(i) <- x + i) ch.dim;
      let m' = if not del then let m = Matrix.copy_pt m in Array.fold_left (fun y x -> Matrix.reduce_col_pt_with y x) m ch.dim else m in
      Matrix.remove_zero_rows @@ Matrix.del_cols m' ch.dim)

  let change_d t new_env add del =
    let dim_change = if add then Environment.dimchange t.env new_env
      else Environment.dimchange new_env t.env
    in let d' = if is_bot t then Matrix.empty () else if add then dim_add dim_change t.d else dim_remove dim_change t.d del
    in {d = d'; env = new_env}

  let add_vars t vars =
    let vs' =
      vars
      |> List.enum
      |> Enum.filter (fun v -> not (Environment.mem_var t.env v))
      |> Array.of_enum in
    let env' = Environment.add t.env vs' [||] in
    change_d t env' true false

  let drop_vars t vars del =
    let vs' =
      vars
      |> List.enum
      |> Enum.filter (fun v -> Environment.mem_var t.env v)
      |> Array.of_enum
    in
    let env' = Environment.remove t.env vs' in
    change_d t env' false del

  let remove_vars t vars = drop_vars t vars false

  let remove_vars_pt_with t vars =
    remove_vars t vars

  let remove_filter t f =
    let env' = remove_filter t.env f in
    change_d t env' false false

  let remove_filter_pt_with t f =
    remove_filter t f

  let keep_filter t f =
    let env' = keep_filter t.env f in
    change_d t env' false false

  let keep_vars t vs =
    let env' = keep_vars t.env vs in
    change_d t env' false false

  let vars t = vars t.env

  include ConvenienceOps(Mpqf)

  let get_c v = match Vector.findi (fun x -> x <> (of_int 0)) v with
    | exception Not_found -> Some (of_int 0)
    | i when Vector.compare_length_with v (i + 1) = 0 -> Some (Vector.nth v i)
    | _ -> None

  (*Parses a Texpr to obtain a coefficient + const (last entry) vector to repr. an affine relation.
    Returns None if the expression is not affine linear*)
  let get_coeff_vec (t: t) texp =
    let open Apron.Texpr1 in
    let exception NotLinear in
    let zero_vec = Vector.zero_vec @@ Environment.size t.env + 1 in
    let neg = Vector.map_pt_with (fun x -> (of_int (-1)) *: x) in
    let is_const_vec v = Vector.compare_length_with (Vector.filteri (fun i x -> (*Inefficient*)
        Vector.compare_length_with v (i + 1) > 0 && x <> of_int 0) v) 1 = 0
    (* let is_const_vec v = match Vector.findi (fun x -> x <> of_int 0) v with
                         | exception Not_found -> false
                         | i -> Vector.compare_length_with v (i + 1) = 0 *)
    in
    let rec convert_texpr texp =
      begin match texp with
        | Cst x -> let of_union union =
                     let open Coeff in
                     match union with
                     | Interval _ -> failwith "Not a constant"
                     | Scalar x -> (match x with
                         | Float x -> Mpqf.of_float x
                         | Mpqf x -> x
                         | Mpfrf x -> Mpfr.to_mpq x) in Vector.set_val zero_vec ((Vector.length zero_vec) - 1) (of_union x)
        | Var x ->
          (*If x is a constant, replace it with its const. val. immediately*)
          let zero_vec_cp = Vector.copy_pt_with zero_vec in
          let entry_only v = Vector.set_val_pt_with v(Environment.dim_of_var t.env x) (of_int 1) in
          begin match is_bot t with
            | false -> let row = Matrix.find_opt (fun r -> Vector.nth r (Environment.dim_of_var t.env x) = of_int 1) t.d in
              begin match row with
                | Some v when is_const_vec v ->
                  Vector.set_val_pt_with zero_vec_cp ((Vector.length zero_vec) - 1) (Vector.nth v (Vector.length v - 1))
                | _ -> entry_only zero_vec_cp end
            | true ->
              (* entry_only zero_vec_cp  *)
            failwith "Can not assign to bottom"
            end
        | Unop (u, e, _, _) ->
          begin match u with
            | Neg -> neg @@ convert_texpr e
            | Cast -> convert_texpr e (*Ignore*)
            | Sqrt -> raise NotLinear end
        | Binop (b, e1, e2, _, _) ->
          begin match b with
            | Add ->  Vector.map2_pt_with (+:)(convert_texpr e1) (convert_texpr e2)
            | Sub -> Vector.map2_pt_with (+:) (convert_texpr  e1) (neg @@ convert_texpr e2)
            | Mul ->
              let x1, x2 = convert_texpr e1, convert_texpr e2 in
              begin match get_c x1, get_c x2 with
                | _, Some c -> Vector.apply_with_c_pt_with ( *:) c x1
                | Some c, _ -> Vector.apply_with_c_pt_with ( *:) c x2
                | _, _ -> raise NotLinear end
            | _ -> raise NotLinear end
      end
    in match convert_texpr texp with
    | exception NotLinear -> None
    | x -> Some(x)
end

module ExpressionBounds (V: AbstractVector) (Mx: AbstractMatrix): (SharedFunctions.ConvBounds with type t = VarManagement(V) (Mx).t) =
struct
  include VarManagement (V) (Mx)

  let bound_texpr t texpr =
    let texpr = Texpr1.to_expr texpr in
    match get_coeff_vec t texpr  with
    | Some v -> begin match get_c v with
        | Some c when Mpqf.get_den c = IntOps.BigIntOps.of_int 1 ->
          let int_val = Mpqf.get_num c
          in Some int_val, Some int_val
        | _ -> None, None end
    | _ -> None, None


  let bound_texpr d texpr1 =
    let res = bound_texpr d texpr1 in
    match res with
    | Some min, Some max ->  if M.tracing then M.tracel "bounds" "min: %s max: %s" (IntOps.BigIntOps.to_string min) (IntOps.BigIntOps.to_string max); res
    | _ -> res
end

module D2(V: AbstractVector) (Mx: AbstractMatrix): SharedFunctions.AssertionRelD2 with type var = SharedFunctions.Var.t =
struct
  include ConvenienceOps (Mpqf)
  include VarManagement (V) (Mx)

  module Bounds = ExpressionBounds (V) (Mx)
  module Convert = SharedFunctions.Convert (Bounds)

  type var = SharedFunctions.Var.t

  let tag t = failwith "No tag"
  let show t =
    match is_bot t with
    | true -> Format.asprintf "⟂ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | false when is_top t -> Format.asprintf "⊤ (env: %a)" (Environment.print:Format.formatter -> Environment.t -> unit) t.env
    | _ -> Format.asprintf "%s env %a" (Matrix.show t.d) (Environment.print:Format.formatter -> Environment.t -> unit) t.env

  let pretty () (x:t) = text (show x)
  let printXml f x = BatPrintf.fprintf f "<value>\n<map>\n<key>\nmatrix\n</key>\n<value>\n%s</value>\n<key>\nenv\n</key>\n<value>\n%s</value>\n</map>\n</value>\n" (XmlUtil.escape (Format.asprintf "%s" (show x) )) (XmlUtil.escape (Format.asprintf "%a" (Environment.print: Format.formatter -> Environment.t -> unit) (x.env)))
  let equal t1 t2 =
    match is_bot t1, is_bot t2 with
    | true, true -> true
    | false, false -> Matrix.equal t1.d t2.d
    | _ -> false

  let hash t =
    Hashtbl.hash t

  let compare t1 t2 = Stdlib.compare t1 t2

  let name () = "affeq"

  let to_yojson t = failwith "ToDo Implement in future"

  let invariant a b = Invariant.none

  let arbitrary () = failwith "no arbitrary"

  let meet t1 t2 =
    let sup_env = Environment.lce t1.env t2.env in
    match is_bot t1, is_bot t2 with
    | false, false when Matrix.is_empty t1.d -> {d = dim_add (Environment.dimchange t2.env sup_env) t2.d; env = sup_env}
    | false, false when Matrix.is_empty t2.d -> {d = dim_add (Environment.dimchange t1.env sup_env) t1.d; env = sup_env}
    | false, false -> let mod_x = dim_add (Environment.dimchange t1.env sup_env) t1.d in
      let mod_y = dim_add (Environment.dimchange t2.env sup_env) t2.d in
      let rref_matr = Matrix.normalize (Matrix.append_matrices mod_x mod_y) in
      if Option.is_none rref_matr then bot () else
      {d = Option.get @@ rref_matr; env = sup_env}
    | _, _ -> bot ()

  let meet t1 t2 =
    let res = meet t1 t2 in
    if M.tracing then M.tracel "meet" "meet \n a: %s \n b: %s -> %s \n" (show t1) (show t2) (show res) ;
    res

  let leq t1 t2 =
    let env_comp = Environment.compare t1.env t2.env in
    if env_comp = -2 || env_comp > 0 then false else
    if is_bot t1 || is_top t2 then true else
    if is_bot t2 || is_top t1 then false else (
      let m1, m2 = t1.d, t2.d in
      let m1' = if env_comp = 0 then m1 else dim_add (Environment.dimchange t1.env t2.env) m1 in
      Matrix.is_covered_by m2 m1')

  let leq t1 t2 =
    let res = leq t1 t2 in
    if M.tracing then M.tracel "leq" "leq a: %s b: %s -> %b \n" (show t1) (show t2) res ;
    res

  let join a b =
    let rec lin_disjunc r s a b =
      if s >= Matrix.num_cols a then a else
        let case_two a r col_b =
          let a_r = Matrix.get_row a r in
          let mapping = Matrix.map2i_pt_with (fun i x y -> if i < r then
                                                 Vector.map2_pt_with (fun u j -> u +: y *: j) x a_r else x) a col_b
          in Matrix.remove_row mapping r
        in
        let case_three a b col_a col_b max =
          let col_a, col_b = Vector.copy_pt_with col_a, Vector.copy_pt_with col_b in
          let col_a, col_b = Vector.keep_vals col_a max, Vector.keep_vals col_b max in
          if Vector.equal col_a col_b then (a, b, max) else
            let a_rev, b_rev = Vector.rev_pt_with col_a,  Vector.rev_pt_with col_b in
            let i = Vector.find2i (fun x y -> x <> y) a_rev b_rev in
            let (x, y) = Vector.nth a_rev i, Vector.nth b_rev i in
            let r, diff = Vector.length a_rev - (i + 1), x -: y  in
            let a_r, b_r = Matrix.get_row a r, Matrix.get_row b r in
            let sub_col = Vector.rev_pt_with @@ Vector.map2_pt_with (fun x y -> x -: y) a_rev b_rev in
            let multiply_by_t m t =
              Matrix.map2i_pt_with (fun i' x c -> if i' <= max then let beta = c /: diff in
                                       Vector.map2_pt_with (fun u j -> u -: (beta *: j)) x t else x) m sub_col
            in
            Matrix.remove_row (multiply_by_t a a_r) r, Matrix.remove_row (multiply_by_t b b_r) r, (max - 1)
        in
        let col_a, col_b = Matrix.get_col a s, Matrix.get_col b s in
        let nth_zero v i =  match Vector.nth v i with
          | exception Invalid_argument _ -> of_int 0
          | x -> x
        in
        let a_rs, b_rs = nth_zero col_a r, nth_zero col_b r in
        if Mpqf.get_den a_rs <> (IntOps.BigIntOps.of_int 1) || Mpqf.get_den b_rs <> (IntOps.BigIntOps.of_int 1) then failwith "Matrix not normalized" else
          begin match Int.of_float @@ Mpqf.to_float @@ a_rs, Int.of_float @@ Mpqf.to_float @@ b_rs with
            | 1, 1 -> lin_disjunc (r + 1) (s + 1) a b
            | 1, 0 -> lin_disjunc r (s + 1) (case_two a r col_b) b
            | 0, 1 -> lin_disjunc r (s + 1) a (case_two b r col_a)
            | 0, 0 ->  let new_a, new_b, new_r = case_three a b col_a col_b r in
              lin_disjunc new_r (s + 1) new_a new_b
            | _      -> failwith "Matrix not normalized" end
    in
    match is_bot a, is_bot b with
    | true, _  -> b
    | _, true -> a
    | false, false when is_top a || is_top b -> {d = Matrix.empty (); env = Environment.lce a.env b.env}
    | false, false when (Environment.compare a.env b.env <> 0) ->
      let sup_env = Environment.lce a.env b.env in
      let mod_x = dim_add (Environment.dimchange a.env sup_env) a.d in
      let mod_y = dim_add (Environment.dimchange b.env sup_env) b.d in
      {d = lin_disjunc 0 0 (Matrix.copy_pt mod_x) (Matrix.copy_pt mod_y); env = sup_env}
    | false, false when Matrix.equal a.d b.d -> {d = a.d; env = a.env}
    | false, false -> {d = lin_disjunc 0 0 (Matrix.copy_pt a.d) (Matrix.copy_pt b.d); env = a.env}

  let join a b =
    let res = join a b in
    if M.tracing then M.tracel "join" "join a: %s b: %s -> %s \n" (show a) (show b) (show res) ;
    res
  let widen a b = join a b
  let narrow a b = meet a b
  let pretty_diff () (x, y) =
    dprintf "%s: %a not leq %a" (name ()) pretty x pretty y

  let remove_rels_with_var x var env imp =
    let j0 = Environment.dim_of_var env var in
    if imp then Matrix.reduce_col_pt_with x j0 else Matrix.reduce_col x j0

  let forget_vars t vars =
    match is_bot t with
    | true -> bot ()
    | false  ->
      if List.is_empty vars then t else
        let rec rem_vars m vars' =
          begin match vars' with
            |            [] -> m
            | x :: xs -> rem_vars (remove_rels_with_var m x t.env true) xs end
        in {d = Matrix.remove_zero_rows @@ rem_vars (Matrix.copy_pt t.d) vars; env = t.env}

  let assign_texpr (t: VarManagement(V)(Mx).t) var texp =
    let assign_invertible_rels x var b env =
      let j0 = Environment.dim_of_var env var in
      let a_j0 = Matrix.get_col x j0  in (*Corresponds to Axj0*)
      let b0 = Vector.nth b j0 in
      let reduced_a = Vector.apply_with_c_pt_with (/:) b0 a_j0 in  (*Corresponds to Axj0/Bj0*)
      let recalc_entries m rd_a = Matrix.map2_pt_with (fun x y -> Vector.map2i_pt_with (fun j z d ->
          if j = j0 then y
          else if Vector.compare_length_with b (j + 1) > 0 then z -: y *: d
          else z +: y *: d) x b) m rd_a
      in {d = recalc_entries x reduced_a; env = env}
    in
    let assign_uninvertible_rel x var b env =
      let neg_vec = Vector.mapi_pt_with (fun i z -> if Vector.compare_length_with b (i + 1) > 0 then of_int (-1) *: z else z) b
      in let var_vec = Vector.set_val_pt_with neg_vec (Environment.dim_of_var env var) (of_int 1)
    in let m = Matrix.normalize_pt_with @@ Matrix.append_row x var_vec
    in if Option.is_none m then bot () else {d = Option.get m ; env = env}
    in
    let is_invertible v = Vector.nth v @@ Environment.dim_of_var t.env var <> of_int 0
    in let affineEq_vec = get_coeff_vec t texp
    in match is_bot t, affineEq_vec with
    | true, _ -> failwith "Can not assign to bottom state!"
    | false, Some v when is_top t -> if is_invertible v then t else assign_uninvertible_rel (Matrix.empty ()) var v t.env
    | false, Some v -> if is_invertible v then let t' = assign_invertible_rels (Matrix.copy_pt t.d) var v t.env
    in let norm_m = Matrix.normalize_pt_with @@ t'.d in if Option.is_none norm_m then bot () else  {d = Option.get norm_m ; env = t'.env}
      else let new_x = Matrix.remove_zero_rows @@ remove_rels_with_var t.d var t.env false
        in assign_uninvertible_rel new_x var v t.env
    | false, None -> {d = Matrix.remove_zero_rows @@ remove_rels_with_var t.d var t.env false; env = t.env}

  let assign_exp (t: VarManagement(V)(Mx).t) var exp (no_ov: bool Lazy.t) =
    match Convert.texpr1_expr_of_cil_exp t t.env (Lazy.force no_ov) exp with
    | exp -> assign_texpr t var exp
    | exception Convert.Unsupported_CilExp -> match is_bot t with
      | false -> t
      | true -> forget_vars t [var]

  let assign_exp t var exp no_ov =
    let res = assign_exp t var exp no_ov in
    if M.tracing then M.tracel "assign" "assign_exp t:\n %s \n var: %s \n exp: %s\n no_ov: %b -> \n %s\n"
        (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) (Lazy.force no_ov) (show res) ;
    res
  let assign_var (t: VarManagement(V)(Mx).t) v v' =
    let texpr1 = Texpr1.of_expr (t.env) (Var v') in
    assign_texpr t v (Apron.Texpr1.to_expr texpr1)

  let assign_var t v v' =
    let res = assign_var t v v' in
    if M.tracing then M.tracel "var" "assign_var t:\n %s \n v: %s \n v': %s\n -> %s\n" (show t) (Var.to_string v) (Var.to_string v') (show res) ;
    res

  let assign_var_parallel t vv's =
    let assigned_vars = List.map (function (v, _) -> v) vv's in
    let primed_vars = List.init (List.length assigned_vars) (fun i -> Var.of_string (Int.to_string i  ^"'")) in
    let t_primed = add_vars t primed_vars in
    let multi_t = List.fold_left2 (fun t' v_prime (_,v') -> assign_var t' v_prime v') t_primed primed_vars vv's in
    match is_bot t with
    | false when not @@ is_top t -> let replace_col m x y = let dim_x, dim_y = Environment.dim_of_var multi_t.env x, Environment.dim_of_var multi_t.env y in
                                                let col_x = Matrix.get_col m dim_x in
                                                Matrix.set_col_with m col_x dim_y in
      let m_cp = Matrix.copy_pt t.d in
      let switched_m = List.fold_left2 (fun m' x y -> replace_col m' x y) m_cp primed_vars assigned_vars in
      let res = drop_vars {d = switched_m; env = multi_t.env} primed_vars true in
      let norm_m = Matrix.normalize_pt_with (res.d) in
      if Option.is_none norm_m then bot () else
      {d = Option.get norm_m; env = res.env}
    | _ -> t

    let assign_var_parallel t vv's =
    let res = assign_var_parallel t vv's in
    if M.tracing then M.tracel "ops" "assign_var parallel: %s -> %s \n" (show t) (show res);
    res

  let assign_var_parallel_pt_with t vv's =
    assign_var_parallel t vv's

  let assign_var_parallel' t vs1 vs2 =
    let vv's = List.combine vs1 vs2 in
    assign_var_parallel t vv's

  let substitute_exp t var exp no_ov =
    let t = if not @@ Environment.mem_var t.env var then add_vars t [var] else t in
    let res = assign_exp t var exp no_ov in
    forget_vars res [var]

  let substitute_exp t var exp ov =
    let res = substitute_exp t var exp ov
    in if M.tracing then M.tracel "sub" "Substitute_expr t: \n %s \n var: %s \n exp: %s \n -> \n %s\n" (show t) (Var.to_string var) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () exp)) (show res);
    res

  (** Assert a constraint expression. *)
  let meet_with_tcons t tcons expr =
    let check_const cmp c = if cmp c (of_int 0) then bot () else t
    in
    let exception NotRefinable in
    let meet_with_vec e =
      (*Flip the sign of the const. val in coeff vec*)
      let flip_e = Vector.mapi_pt_with (fun i x -> if Vector.compare_length_with e (i + 1) = 0 then (of_int (-1)) *: x else x) e in
      let norm_m = Matrix.normalize_pt_with @@ Matrix.init_with_vec flip_e in
      if Option.is_none norm_m then bot () else
      let res = meet t {d = Option.get norm_m; env = t.env} in
      let overflow_res res = if IntDomain.should_ignore_overflow (Cilfacade.get_ikind_exp expr) then res else raise NotRefinable in
      match Convert.determine_bounds_one_var expr with
      | None -> overflow_res res
      | Some (ev, min, max) ->
        begin match Bounds.bound_texpr res (Convert.texpr1_of_cil_exp res res.env ev true) with
          | Some b_min, Some b_max ->  let module BI = IntOps.BigIntOps in
            if min = BI.of_int 0 && b_min = b_max then raise NotRefinable
            else if (b_min < min && b_max < min) || (b_max > max && b_min > max) then
              (if GobConfig.get_string "sem.int.signed_overflow" = "assume_none" then bot () else raise NotRefinable)
            else res
          | _, _ -> overflow_res res end
    in
    (* let refine_by x y = if BI.equal (BI.rem v (BI.of_int 2)) BI.zero then *)
    match get_coeff_vec t (Texpr1.to_expr @@ Tcons1.get_texpr1 tcons) with
    | Some v -> begin match get_c v, Tcons1.get_typ tcons with
        | Some c, DISEQ -> check_const (=:) c
        | Some c, SUP -> check_const (<=:) c
        | Some c, EQ -> check_const (<>) c
        | Some c, SUPEQ -> check_const (<:) c
        | None, DISEQ | None, SUP ->
          begin match meet_with_vec v with
            | exception NotRefinable -> t
            | res -> if equal res t then bot () else t end
        | None, EQ -> begin match meet_with_vec v with
            | exception NotRefinable -> t
            | res -> res end
        | _, _ -> t end
    | None -> t

  let unify a b =
    meet a b

  let unify a b =
    let res = unify a b
    in if M.tracing then M.tracel "unify" "unify: a: %s \n b: %s \n -> %s " (show a) (show b) (show res);
    res


  let assert_cons d e negate no_ov =
    let no_ov = Lazy.force no_ov in
    begin match Convert.tcons1_of_cil_exp d d.env e negate no_ov with
      | tcons1 -> meet_with_tcons d tcons1 e
      | exception Convert.Unsupported_CilExp ->
        d
    end

  let assert_cons d e negate no_ov =
    let res = assert_cons d e negate no_ov in
    if M.tracing then M.tracel "assert_cons" "assert_cons d: %s  %s -> %s \n" (show d) (Pretty.sprint ~width:1 (Cil.printExp Cil.defaultCilPrinter () e)) (show res); res

  let relift t = t

  let env (t: Bounds.t) = t.env

  type marshal = Bounds.t

  let marshal t = t

  let unmarshal t = t

end
