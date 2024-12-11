open Utils
include My_parser

let rec fvs (in_ty : ty) : ident list =
  match in_ty with
  | TUnit        -> []
  | TInt         -> []
  | TFloat       -> []
  | TBool        -> []
  | TVar v       -> [v]
  | TList a      -> fvs (a)
  | TOption a    -> fvs (a)
  | TPair (a, b) -> (fvs (a))@(fvs (b))
  | TFun (a, b)  -> (fvs (a))@(fvs (b))

let type_subst (x : string) (to_ty : ty) (in_ty : ty) : ty =
  let rec type_subst_impl (in_ty : ty) : ty =
    match in_ty with
    | TUnit        -> TUnit
    | TInt         -> TInt
    | TFloat       -> TFloat
    | TBool        -> TBool
    | TVar v       -> if (v = x) then to_ty else in_ty
    | TList a      -> TList (type_subst_impl (a))
    | TOption a    -> TOption (type_subst_impl (a))
    | TPair (a, b) -> TPair ((type_subst_impl (a)), (type_subst_impl (b)))
    | TFun (a, b)  -> TFun ((type_subst_impl (a)), (type_subst_impl (b)))
  in
  type_subst_impl (in_ty)

let unify (in_ty : ty) (cs : constr list) : ty_scheme option =
  let rec constr_subst (x : ident) (to_ty : ty) (cs : constr list) : constr list =
    match cs with
    | []          -> []
    | (t1, t2)::t -> ((type_subst (x) (to_ty) (t1)), (type_subst (x) (to_ty) (t2)))::(constr_subst (x) (to_ty) (t))
  in
  let rec unify_impl (cs : constr list) (curr_s : (ident * ty) list) : (ident * ty) list option =
    (* let _ = print_constrs (cs)
    in *)
    match cs with
    | []                                     -> Some curr_s
    | (t1, t2)::ct when t1 = t2              -> unify_impl (ct) (curr_s)
    | (TList t1, TList t2)::cs               -> unify_impl ((t1, t2)::cs) (curr_s)
    | (TOption t1, TOption t2)::cs           -> unify_impl ((t1, t2)::cs) (curr_s)
    | (TPair (t1, t2), TPair (t1', t2'))::cs -> unify_impl ((t1, t1')::(t2, t2')::cs) (curr_s)
    | (TFun (t1, t2), TFun (t1', t2'))::cs   -> unify_impl ((t1, t1')::(t2, t2')::cs) (curr_s)
    | (TVar a, t)::cs                        -> if (List.mem (a) (fvs (t))) then None
      else unify_impl (constr_subst (a) (t) (cs)) ((a, t)::curr_s)
    | (t, TVar a)::cs                        -> unify_impl ((TVar (a), t)::cs) (curr_s)
    | _                                      -> None
  in
  let rec apply_s (s : (ident * ty) list) (in_ty : ty) : ty =
    match s with
    | []         -> in_ty
    | (x, t)::st -> apply_s (st) (type_subst (x) (t) (in_ty))
  in
  let rec quantify (in_ty : ty) : ident list =
    match in_ty with
    | TUnit        -> []
    | TInt         -> []
    | TFloat       -> []
    | TBool        -> []
    | TVar v       -> [v]
    | TList a      -> quantify (a)
    | TOption a    -> quantify (a)
    | TPair (a, b) -> (quantify (a))@(quantify (b))
    | TFun (a, b)  -> (quantify (a))@(quantify (b))
  in
  match (unify_impl (cs) ([])) with
  | None   -> None
  | Some s -> let final_ty = apply_s (s) (in_ty)
    in
    Some (Forall ((quantify (final_ty)), final_ty))

let type_of (ctx : stc_env) (e : expr) : ty_scheme option =
  let var_type_replace (in_ty : ty_scheme) : ty =
    let rec var_type_replace_impl (fvs : ident list) (in_ty : ty) : ty =
      match fvs with
      | []    -> in_ty
      | x::xs -> (
        let fresh = TVar (gensym())
        in
        var_type_replace_impl (xs) (type_subst (x) (fresh) (in_ty))
      )
    in
    match in_ty with
    | Forall (fvs, t) -> var_type_replace_impl (fvs) (t)
  in
  let rec type_of_impl (ctx : stc_env) (e : expr) : (ty * constr list) =
    let (t, c) = match e with
    | Unit                                 -> (TUnit, [])
    | True                                 -> (TBool, [])
    | False                                -> (TBool, [])
    (* Empty list *)
    | Nil                                  -> (TList (TVar (gensym())), [])
    | ENone                                -> (TOption (TVar (gensym())), [])
    | Int _n                               -> (TInt, [])
    | Float _n                             -> (TFloat, [])
    | Var v                                -> (
      let t_s = Env.find (v) (ctx)
      in
      (var_type_replace (t_s), [])
    )
    | Assert e                             -> (
      if (e = False) then (TVar (gensym()), [])
      else let (t, c) = type_of_impl (ctx) (e)
        in
        (TUnit, (t, TBool)::c)
    )
    | ESome e                              -> (
      let (t, c) = type_of_impl (ctx) (e)
      in
      (TOption (t), c)
    )
    | Bop (op, e1, e2)                     -> (
      let (t1, c1) = type_of_impl (ctx) (e1)
      in
      let (t2, c2) = type_of_impl (ctx) (e2)
      in
      match op with
      | Add | Sub | Mul | Div | Mod      -> 
        (TInt, (t1, TInt)::(t2, TInt)::c1@c2)
      | AddF | SubF | MulF | DivF | PowF ->
        (TFloat, (t1, TFloat)::(t2, TFloat)::c1@c2)
      | Lt | Lte | Gt | Gte | Eq | Neq   ->
        (TBool, (t1, t2)::c1@c2)
      | And | Or                         ->
        (TBool, (t1, TBool)::(t2, TBool)::c1@c2)
      | Concat                           -> (
        let a = TVar (gensym())
        in
        (TList (a), (t1, (TList (a)))::(t2, (TList (a)))::c1@c2)
      )
      | Cons                             ->
        (TList t1, (t2, (TList (t1)))::c1@c2)
      | Comma                            ->
        (TPair (t1, t2), c1@c2)
    )
    | If (ec, et, ef)                      -> (
      let (t1, c1) = type_of_impl (ctx) (ec)
      in
      let (t2, c2) = type_of_impl (ctx) (et)
      in
      let (t3, c3) = type_of_impl (ctx) (ef)
      in
      (t3, (t1, TBool)::(t2, t3)::c1@c2@c3)
    )
    | ListMatch lm                         -> (
      let (t, c) = type_of_impl (ctx) (lm.matched)
      in
      let a = gensym()
      in
      let (t1, c1) = type_of_impl (Env.add (lm.hd_name) (Forall ([a], (TVar (a)))) (Env.add (lm.tl_name) (Forall ([a], (TList (TVar (a))))) (ctx))) (lm.cons_case)
      in
      let (t2, c2) = type_of_impl (ctx) (lm.nil_case)
      in
      (t2, (t, (TList (TVar (a))))::(t1, t2)::c@c1@c2)
    )
    | OptMatch om                          -> (
      let (t, c) = type_of_impl (ctx) (om.matched)
      in
      let a = gensym()
      in
      let (t1, c1) = type_of_impl (Env.add (om.some_name) (Forall ([a], (TVar (a)))) (ctx)) (om.some_case)
      in
      let (t2, c2) = type_of_impl (ctx) (om.none_case)
      in
      (t2, (t, (TOption (TVar (a))))::(t1, t2)::c@c1@c2)
    )
    | PairMatch pm                         -> (
      let (t, c) = type_of_impl (ctx) (pm.matched)
      in
      let a = gensym()
      in
      let b = gensym()
      in
      let (t1, c1) = type_of_impl (Env.add (pm.fst_name) (Forall ([a], (TVar (a)))) (Env.add (pm.snd_name) (Forall ([b], (TVar (b)))) (ctx))) (pm.case)
      in
      (t1, (t, (TPair ((TVar (a)), (TVar (b)))))::c@c1)
    )
    | Fun (x, t, eb)                       -> (
      let a = gensym()
      in
      let tx0 = match t with
        | Some t -> t
        | _      -> TVar (a)
      in
      let tx0_scheme = match t with
        | Some t -> Forall ([], t)
        | _      -> Forall ([a], (TVar (a)))
      in
      let (tx1, c) = type_of_impl (Env.add (x) (tx0_scheme) (ctx)) (eb)
      in
      ((TFun (tx0, tx1)), c)
    )
    | App (e1, e2)                         -> (
      let (t1, c1) = type_of_impl (ctx) (e1)
      in
      let (t2, c2) = type_of_impl (ctx) (e2)
      in
      let a = TVar (gensym())
      in
      (a, (t1, TFun (t2, a))::c1@c2)
    )
    | Annot (e1, t)                        -> (
      let (t1, c1) = type_of_impl (ctx) (e1)
      in
      (t, (t, t1)::c1)
    )
    | Let l                                -> (
      if l.is_rec then let a = gensym()
        in
        let b = gensym()
        in
        let (t1, c1) = type_of_impl (Env.add (l.name) (Forall ([a; b], (TFun ((TVar (a)), (TVar (b)))))) (ctx)) (l.value)
        in
        let (t2, c2) = type_of_impl (Env.add (l.name) (Forall ([], t1)) (ctx)) (l.body)
        in
        (t2, (t1, (TFun ((TVar (a)), (TVar (b)))))::c1@c2)
      else let (t1, c1) = type_of_impl (ctx) (l.value)
        in
        let (t2, c2) = type_of_impl (Env.add (l.name) (Forall ([], t1)) (ctx)) (l.body)
        in
        (t2, c1@c2)
    )
    in
    (t, c)
  in
  let (t, c) = type_of_impl (ctx) (e)
  in
  unify (t) (c)

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals
exception TypeExn

let eval_expr (ev : dyn_env) (e : expr) : value =
  let rec eval_impl (e : expr) (ev : value env) : value =
    (* let _ = print_expr (e)
    in *)
    let v = match e with
    | Unit                                 -> VUnit
    | True                                 -> VBool (true)
    | False                                -> VBool (false)
    | Nil                                  -> VList ([])
    | ENone                                -> VNone
    | ESome s                              -> VSome (eval_impl (s) (ev))
    | Int i                                -> VInt (i)
    | Float f                              -> VFloat (f)
    | Var v                                -> Env.find (v) (ev)
    | If  (ec, et, ef)                     -> (
      match (eval_impl (ec) (ev)) with
      | VBool cond -> if (cond) then (eval_impl (et) (ev)) else (eval_impl (ef) (ev))
      | _          -> raise TypeExn
    )
    | Bop (op, e1, e2)                     -> (
      match (eval_impl (e1) (ev)) with
      (* Arithmetic & Comparison Operations *)
      (* Integer *)
      | VInt n1  -> (
        match (eval_impl (e2) (ev)) with
        | VInt n2  -> (
          match op with
          | Add   -> VInt (n1 + n2)
          | Sub   -> VInt (n1 - n2)
          | Mul   -> VInt (n1 * n2)
          | Div   -> if (n2 = 0) then (raise DivByZero) else VInt (n1 / n2)
          | Mod   -> VInt (n1 mod n2)
          | Lt    -> VBool (n1 < n2)
          | Lte   -> VBool (n1 <= n2)
          | Gt    -> VBool (n1 > n2)
          | Gte   -> VBool (n1 >= n2)
          | Eq    -> VBool (n1 = n2)
          | Neq   -> VBool (n1 <> n2)
          | Comma -> VPair ((VInt (n1)), (VInt (n2)))
          | _     -> raise TypeExn
        )
        | VList l  -> (
          match op with
          | Cons -> VList ((VInt (n1))::l)
          | _    -> raise TypeExn
        )
        | VClos _c -> raise CompareFunVals
        | _        -> raise TypeExn
      )
      (* Float *)
      | VFloat n1  -> (
        match (eval_impl (e2) (ev)) with
        | VFloat n2  -> (
          match op with
          | AddF  -> VFloat (n1 +. n2)
          | SubF  -> VFloat (n1 -. n2)
          | MulF  -> VFloat (n1 *. n2)
          | DivF  -> VFloat (n1 /. n2)
          | PowF  -> VFloat (n1 ** n2)
          | Lt    -> VBool (n1 < n2)
          | Lte   -> VBool (n1 <= n2)
          | Gt    -> VBool (n1 > n2)
          | Gte   -> VBool (n1 >= n2)
          | Eq    -> VBool (n1 = n2)
          | Neq   -> VBool (n1 <> n2)
          | Comma -> VPair ((VFloat (n1)), (VFloat (n2)))
          | _     -> raise TypeExn
        )
        | VList l  -> (
          match op with
          | Cons -> VList ((VFloat (n1))::l)
          | _    -> raise TypeExn
        )
        | VClos _c -> raise CompareFunVals
        | _        -> raise TypeExn
      )
      (* Boolean Operations *)
      | VBool b1 -> (
        match op with
        | And   -> if b1 then (eval_impl (e2) (ev)) else VBool(false)
        | Or    -> if b1 then VBool(true) else (eval_impl (e2) (ev))
        | Lt    -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 < b2)
          | _        -> raise TypeExn
        )
        | Lte   -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 <= b2)
          | _        -> raise TypeExn
        )
        | Gt    -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 > b2)
          | _        -> raise TypeExn
        )
        | Gte   -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 >= b2)
          | _        -> raise TypeExn
        )
        | Eq    -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 = b2)
          | _        -> raise TypeExn
        )
        | Neq   -> (
          match (eval_impl (e2) (ev)) with
          | VBool b2 -> VBool (b1 <> b2)
          | _        -> raise TypeExn
        )
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VBool (b1))::l)
            | _       -> raise TypeExn
          )
        | Comma -> VPair (VBool (b1), (eval_impl (e2) (ev)))
        | _   -> raise TypeExn
      )
      (* List Operations *)
      | VList l1 -> (
        match op with
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VList (l1))::l)
            | _       -> raise TypeExn
          )
        | Concat -> (
          match (eval_impl (e2) (ev)) with
          | VList l2 -> VList (l1@l2)
          | _        -> raise TypeExn
        )
        | Eq     -> (
          match (eval_impl (e2) (ev)) with
          | VList l2 -> VBool (l1 = l2)
          | _        -> raise TypeExn
        )
        | _      -> raise TypeExn
      )
      | VPair (a, b) -> (
        match op with
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VPair (a, b))::l)
            | _       -> raise TypeExn
          )
        | Eq     -> (
          match (eval_impl (e2) (ev)) with
          | VPair (c, d) -> VBool ((a = c) && (b = d))
          | _            -> raise TypeExn
        )
        | _      -> raise TypeExn
      )
      | VClos c  -> (
        match op with
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VClos (c))::l)
            | _       -> raise TypeExn
          )
        | Comma -> VPair (VClos (c), (eval_impl (e2) (ev)))
        | _     -> raise CompareFunVals
      )
      | VNone    -> (
        match op with
        | Eq    -> (
            match (eval_impl (e2) (ev)) with
            | VNone -> VBool (true)
            | _     -> VBool (false)
          )
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VNone)::l)
            | _       -> raise TypeExn
          )
        | Comma -> VPair (VNone, (eval_impl (e2) (ev)))
        | _     -> raise TypeExn
      )
      | VSome s0 -> (
        match op with
        | Eq    -> (
            match (eval_impl (e2) (ev)) with
            | VNone    -> VBool (false)
            | VSome s1 -> VBool (s0 = s1)
            | _        -> raise TypeExn
          )
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VSome (s0))::l)
            | _       -> raise TypeExn
          )
        | Comma -> VPair ((VSome (s0)), (eval_impl (e2) (ev)))
        | _     -> raise TypeExn
      )
      | VUnit    -> (
        match op with
        | Eq    -> (
            match (eval_impl (e2) (ev)) with
            | VUnit -> VBool (true)
            | _     -> VBool (false)
          )
        | Cons  -> (
            match (eval_impl (e2) (ev)) with
            | VList l -> VList ((VUnit)::l)
            | _       -> raise TypeExn
          )
        | Comma -> VPair (VUnit, (eval_impl (e2) (ev)))
        | _     -> raise TypeExn
      )
    )
    | ListMatch lm                         -> (
      match (eval_impl (lm.matched) (ev)) with
      | VList lv -> (
        match lv with
        | []     -> eval_impl (lm.nil_case) (ev)
        | hv::tv -> eval_impl (lm.cons_case) (Env.add (lm.hd_name) (hv) (Env.add (lm.tl_name) (VList (tv)) (ev)))
      )
      | _        -> raise TypeExn
    )
    | OptMatch om                          -> (
      match (eval_impl (om.matched) (ev)) with
      | VNone   -> eval_impl (om.none_case) (ev)
      | VSome s -> eval_impl (om.some_case) (Env.add (om.some_name) (s) (ev))
      | _       -> raise TypeExn
    )
    | PairMatch pm                         -> (
      match (eval_impl (pm.matched) (ev)) with
      | VPair (xv, yv) -> eval_impl (pm.case) (Env.add (pm.fst_name) (xv) (Env.add (pm.snd_name) (yv) (ev)))
      | _              -> raise TypeExn
    )
    | Fun (arg, _, bd)                     -> VClos{ name = None; arg = arg; body = bd; env = ev }
    | App (e1, e2)                         -> (
      match (eval_impl (e1) (ev)) with
      | VClos clos -> (
          match clos.name with
          (* Rec *)
          | Some s -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (Env.add (s) (VClos{ name = clos.name; arg = clos.arg; body = clos.body; env = clos.env }) (clos.env))))
          (* Non-rec *)
          | _      -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (clos.env)))
      )
      | _          -> raise TypeExn
    )
    | Let l                                -> (
      let v1 = eval_impl (l.value) (ev)
      in
      let v1 = if l.is_rec then match v1 with
          | VClos clos -> (
            match clos.name with
            | Some _n -> raise RecWithoutArg
            | _       -> VClos{ clos with name = Some l.name }
          )
          | _          -> raise TypeExn
        else v1
      in
      eval_impl (l.body) (Env.add (l.name) (v1) (ev))
    )
    | Annot (e, _t)                        -> eval_impl (e) (ev)
    | Assert e                             -> (
        match (eval_impl (e) (ev)) with
        | VBool cond -> if cond then VUnit else (raise AssertFail)
        | _          -> raise TypeExn
    )
    (* in
    let _ = print_expr (e) *)
    in
    v
  in
  eval_impl (e) (ev)

let type_check =
  let rec go ctxt = function
  | []                            -> Some (Forall ([], TUnit))
  | { is_rec; name; value } :: ls -> (
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  )
  in
  go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in
  eval_expr Env.empty (nest p)

let interp (input : string) : (value * ty_scheme, error) result =
  match parse input with
  | Some prog -> (
    (* let _ = print_prog (prog)
    in *)
    match type_check prog with
    | Some ty -> Ok ((eval (prog)), ty)
    | None    -> Error (TypeError)
  )
  | None -> Error (ParseError)
