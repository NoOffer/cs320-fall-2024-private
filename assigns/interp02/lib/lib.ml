open Utils
open My_parser

let parse (s : string) : prog option =
    parse (s)

let desugar (p : prog) : expr =
    let rec desugar_toplet_list (tll : toplet list) : sfexpr =
        match tll with
        | []   -> SUnit
        | h::t -> SLet{ is_rec = h.is_rec; name = h.name; args = h.args; ty = h.ty; value = h.value; body = desugar_toplet_list (t) } (* SLet *)
    in
    let rec desugar_sfexpr (se : sfexpr) : expr =
        match se with
        | SUnit             -> Unit
        | STrue             -> True
        | SFalse            -> False
        | SNum n            -> Num(n)
        | SVar v            -> Var(v)        
        | SFun f            -> (
            let (arg_name, arg_ty) = f.arg
            in
            match f.args with
            | []   -> Fun(arg_name, arg_ty, (desugar_sfexpr (f.body)))
            | h::t -> Fun(arg_name, arg_ty, (desugar_sfexpr (SFun{ arg = h; args = t; body = f.body }))) (* desugar_sfexpr SFun *)
        )
        | SApp (e1, e2)     -> App((desugar_sfexpr (e1)), (desugar_sfexpr (e2)))
        | SLet l            -> (
            match l.args with
            | []   -> Let{ is_rec = l.is_rec; name = l.name; ty = l.ty; value = desugar_sfexpr(l.value); body = desugar_sfexpr(l.body) } (* Let *)
            | h::t -> let (arg_name, arg_type) = h
                in
                (* desugar_sfexpr SLet -> Let *)
                match (desugar_sfexpr (SLet{ is_rec = l.is_rec; name = l.name; args = t; ty = l.ty; value = l.value; body = l.body })) with 
                | Let dl -> Let{ dl with ty = FunTy(arg_type, dl.ty); value = Fun(arg_name, arg_type, dl.value) } (* Let *)
                | _      -> Let{ is_rec = l.is_rec; name = l.name; ty = l.ty; value = Unit; body = Unit }
        )
        | SIf  (ec, et, ef) -> If((desugar_sfexpr (ec)), (desugar_sfexpr (et)), (desugar_sfexpr (ef)))
        | SBop (op, e1, e2) -> Bop(op, (desugar_sfexpr (e1)), (desugar_sfexpr (e2)))
        | SAssert e         -> Assert(desugar_sfexpr (e))
    in
    desugar_sfexpr (desugar_toplet_list (p))

let type_of (e : expr) : (ty, error) result =
    let rec find_in_ctx (var_name : string) (ctx : (string * ty) list) : ty option =
        match ctx with
        | []   -> None
        | h::t -> let (name, var_ty) = h
            in
            if (name = var_name) then Some var_ty
            else find_in_ctx (var_name) (t)
    in
    let rec type_of_impl (e : expr) (ctx : (string * ty) list) : (ty, error) result =
        match e with
        | Unit             -> Ok (UnitTy)
        | True             -> Ok (BoolTy)
        | False            -> Ok (BoolTy)
        | Num _n           -> Ok (IntTy)
        | Var v            -> (
            match (find_in_ctx (v) (ctx)) with
            | Some var_ty -> Ok (var_ty)
            | _           -> Error (UnknownVar(v))
        )
        | If  (ec, et, ef) -> (
            match (type_of_impl (ec) (ctx)) with
            | Ok tc    -> (
                if (tc = BoolTy) then match (type_of_impl (et) (ctx)) with
                    | Ok tt    -> (
                        match (type_of_impl (ef) (ctx)) with
                        | Ok tf    -> (
                            if (tt = tf) then Ok (tt)
                            else Error (IfTyErr(tt, tf))
                        )
                        | Error er -> Error(er)
                    )
                    | Error er -> Error(er)
                else Error (IfCondTyErr(tc))
            )
            | Error er -> Error(er)
        ) 
        | Bop (op, e1, e2) -> (
            match op with
            (* Arithmetic Operator *)
            | Add | Sub | Mul | Div | Mod    -> (
                match (type_of_impl (e1) (ctx)) with
                | Ok t1    -> (
                    if (t1 = IntTy) then match (type_of_impl (e2) (ctx)) with
                        | Ok t2    -> (
                            if (t2 = IntTy) then Ok(IntTy)
                            else Error (OpTyErrR(op, IntTy, t2))
                        )
                        | Error er -> Error(er)
                    else Error (OpTyErrL(op, IntTy, t1))
                )
                | Error er -> Error(er)
            )
            (* Comparison *)
            | Lt | Lte | Gt | Gte | Eq | Neq -> (
                match (type_of_impl (e1) (ctx)) with
                | Ok t1    -> (
                    if (t1 = IntTy) then match (type_of_impl (e2) (ctx)) with
                        | Ok t2    -> (
                            if (t2 = IntTy) then Ok(BoolTy)
                            else Error (OpTyErrR(op, IntTy, t2))
                        )
                        | Error er -> Error(er)
                    else Error (OpTyErrL(op, IntTy, t1))
                )
                | Error er -> Error(er)
            )
            (* Logical Operator (And/Or) *)
            | _                              -> (
                match (type_of_impl (e1) (ctx)) with
                | Ok t1    -> (
                    if (t1 = BoolTy) then match (type_of_impl (e2) (ctx)) with
                        | Ok t2    -> (
                            if (t2 = BoolTy) then Ok(BoolTy)
                            else Error (OpTyErrR(op, BoolTy, t2))
                        )
                        | Error er -> Error(er)
                    else Error (OpTyErrL(op, BoolTy, t1))
                )
                | Error er -> Error(er)
            )
        )
        | Fun (x, ty, bd)  -> (
            match (type_of_impl (bd) ((x, ty)::ctx)) with
            | Ok bd_ty -> Ok(FunTy(ty, bd_ty))
            | Error er -> Error(er)
        )
        | App (e1, e2)     -> (
            match (type_of_impl (e1) (ctx)) with
            | Ok t1    -> (
                match t1 with
                | FunTy (t_in, t_out) -> (
                    match (type_of_impl (e2) (ctx)) with
                    | Ok t2    -> (
                        if (t2 = t_in) then Ok (t_out)
                        else Error (FunArgTyErr(t_in, t2))
                    )
                    | Error er -> Error(er)
                )
                | _              -> Error (FunAppTyErr(t1))
            )
            | Error er -> Error(er)
        )
        | Let l            -> (
            if (l.is_rec) then match (type_of_impl (l.value) ((l.name, l.ty)::ctx)) with
                | Ok t1    -> (
                    if (t1 = l.ty) then match (type_of_impl (l.body) ((l.name, t1)::ctx)) with
                        | Ok t2    -> Ok (t2)
                        | Error er -> Error(er)
                    else Error (LetTyErr(l.ty, t1))
                )
                | Error er -> Error(er)
            else match (type_of_impl (l.value) (ctx)) with
                | Ok t1    -> (
                    if (t1 = l.ty) then match (type_of_impl (l.body) ((l.name, l.ty)::ctx)) with
                        | Ok t2    -> Ok (t2)
                        | Error er -> Error(er)
                    else Error (LetTyErr(l.ty, t1))
                )
                | Error er -> Error(er)
        )
        | Assert e         -> (
            match (type_of_impl (e) (ctx)) with
            | Ok t     -> if (t = BoolTy) then Ok(UnitTy) else Error(AssertTyErr(t))
            | Error er -> Error(er)
        )
    in
    type_of_impl (e) ([])

exception DivByZero
exception AssertFail
exception TypeError

let eval (e : expr) : value =
    let rec eval_impl (e : expr) (ev : value env) : value =
        match e with
        | Unit             -> VUnit
        | True             -> VBool(true)
        | False            -> VBool(false)
        | Num n            -> VNum(n)
        | Var v            -> Env.find (v) (ev)
        | If  (ec, et, ef) -> (
            match (eval_impl (ec) (ev)) with
            | VBool cond -> if (cond) then (eval_impl (et) (ev)) else (eval_impl (ef) (ev))
            | _          -> raise TypeError
        )
        | Bop (op, e1, e2) -> (
            match (eval_impl (e1) (ev)) with
            (* Arithmetic & Comparison Operations *)
            | VNum n1  -> (
                match (eval_impl (e2) (ev)) with
                | VNum n2  -> (
                    match op with
                    | Add -> VNum(n1 + n2)
                    | Sub -> VNum(n1 - n2)
                    | Mul -> VNum(n1 * n2)
                    | Div -> if (n2 = 0) then (raise DivByZero) else VNum(n1 / n2)
                    | Mod -> VNum(n1 mod n2)
                    | Lt  -> VBool(n1 < n2)
                    | Lte -> VBool(n1 <= n2)
                    | Gt  -> VBool(n1 > n2)
                    | Gte -> VBool(n1 >= n2)
                    | Eq  -> VBool(n1 = n2)
                    | Neq -> VBool(n1 <> n2)
                    | _   -> raise TypeError
                )
                | _        -> raise TypeError
            )
            (* Boolean Operations *)
            | VBool b1 -> (
                match op with
                | And -> if b1 then (eval_impl (e2) (ev)) else VBool(false)
                | Or  -> if b1 then VBool(true) else (eval_impl (e2) (ev))
                | _   -> raise TypeError
            )
            | _        -> raise TypeError
        )
        | Fun (arg, _, bd) -> VClos{ name = None; arg = arg; body = bd; env = ev }
        | App (e1, e2)     -> (
            match (eval_impl (e1) (ev)) with
            | VClos clos -> (
                match clos.name with
                | Some s -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (Env.add (s) (VClos{ name = clos.name; arg = clos.arg; body = clos.body; env = clos.env }) (clos.env))))
                | _      -> (eval_impl (clos.body) (Env.add (clos.arg) (eval_impl (e2) (ev)) (clos.env)))
            )
            | _          -> raise TypeError
        )
        | Let l            -> (
            let v1 = eval_impl (l.value) (ev)
            in
            let v1 = if l.is_rec then match v1 with
                    | VClos clos -> VClos{ clos with name = Some l.name }
                    | _          -> raise TypeError
                else v1
            in
            eval_impl (l.body) (Env.add (l.name) (v1) (ev))
        )
        | Assert e         -> (
            match (eval_impl (e) (ev)) with
            | VBool cond -> if cond then VUnit else (raise AssertFail)
            | _          -> raise TypeError
        )
    in
    eval_impl (e) (Env.empty)

let interp (s : string) : (value, error) result =
    match (parse (s)) with
    | Some p -> (
        let e = desugar (p)
        (*
        in
        let _ = print_expr (e)
        *)
        in
        match (type_of (e)) with
        | Ok _ty   -> Ok(eval (e))
        | Error er -> Error(er)
    )
    | _      -> Error(ParseErr)