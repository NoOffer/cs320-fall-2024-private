open Utils
open My_parser

let subst (v : value) (x : string) (e : expr) : expr =
    let rec is_fv_in (x : string) (e : expr) : bool =
        match e with
        | Var s             -> (x = s)
        | App (e1, e2)      -> (is_fv_in (x) (e1)) || (is_fv_in (x) (e2))
        | Bop (_op, e1, e2) -> (is_fv_in (x) (e1)) || (is_fv_in (x) (e2))
        | If  (c, et, ef)   -> (is_fv_in (x) (c)) || (is_fv_in (x) (et)) || (is_fv_in (x) (ef))
        | Let (_x, e1, e2)  -> (is_fv_in (x) (e1)) || (is_fv_in (x) (e2))
        | Fun (par, body)   -> (
            if (par = x) then false
            else              is_fv_in (x) (body)
        )
        | _                 -> false
    in
    let rec subst_expr (ve : expr) (x : string) (e : expr) : expr =
        match e with
        | Var var_name     -> (
            if (var_name = x) then ve
            else                   e
        )
        | App (e1, e2)     -> App ((subst_expr (ve) (x) (e1)), (subst_expr (ve) (x) (e2)))
        | Bop (op, e1, e2) -> Bop (op, (subst_expr (ve) (x) (e1)), (subst_expr (ve) (x) (e2)))
        | If  (c, et, ef)  -> If ((subst_expr (ve) (x) (c)), (subst_expr (ve) (x) (et)), (subst_expr (ve) (x) (ef)))
        | Let (nx, e1, e2) -> (
            if (nx = x) then e
            else             Let (nx, (subst_expr (ve) (x) (e1)), (subst_expr (ve) (x) (e2)))
        )
        | Fun (par, body)  -> (
            if (par = x) then e
            else if (is_fv_in (par) (ve)) then (
                let new_par = gensym ()
                in
                subst_expr (ve) (x) (Fun (new_par, (subst_expr (Var (new_par)) (par) (body))))
            )
            else Fun (par, (subst_expr (ve) (x) (body)))
        )
        | _                -> e
    in
    let ve = match v with
        | VNum  n           -> Num (n)
        | VBool b           -> (
            if b then True
            else      False
        )
        | VUnit             -> Unit
        | VFun  (par, body) -> Fun (par, body)
    in
    subst_expr (ve) (x) (e)

let rec eval (e : expr) : (value, error) result =
    match e with
    | Num   n            -> Ok (VNum (n))
    | Var   s            -> Error (UnknownVar (s))
    | Unit               -> Ok (VUnit)
    | True               -> Ok (VBool (true))
    | False              -> Ok (VBool (false))
    | App   (e1, e2)     -> (
        match (eval (e1)) with
        | Ok v1    -> (
            match v1 with
            | VFun (par, body) -> (
                match (eval (e2)) with
                | Ok v2    -> eval (subst (v2) (par) (body))
                | Error er -> Error (er)
            )
            | _               -> Error (InvalidApp)
        )
        | Error er -> Error (er)
    )
    | Bop   (op, e1, e2) -> (
        match (eval (e1)) with
        | Ok v1    -> (
            match v1 with
            (* Arithmetic & Comparison Operations *)
            | VNum n1  -> (
                match (eval (e2)) with
                | Ok v2    -> (
                    match v2 with
                    | VNum n2 -> (
                        match op with
                        | Add -> Ok (VNum (n1 + n2))
                        | Sub -> Ok (VNum (n1 - n2))
                        | Mul -> Ok (VNum (n1 * n2))
                        | Div -> (
                            if (n2 = 0) then Error (DivByZero)
                            else             Ok (VNum (n1 / n2))
                        )
                        | Mod -> Ok (VNum (n1 mod n2))
                        | Lt  -> Ok (VBool (n1 <  n2))
                        | Lte -> Ok (VBool (n1 <= n2))
                        | Gt  -> Ok (VBool (n1 >  n2))
                        | Gte -> Ok (VBool (n1 >= n2))
                        | Eq  -> Ok (VBool (n1 =  n2))
                        | Neq -> Ok (VBool (n1 <> n2))
                        | _   -> Error (InvalidArgs (op))
                    )
                    | _       -> Error (InvalidArgs (op))
                )
                | Error er -> Error (er)
            )
            (* Boolean Operations *)
            | VBool b1 -> (
                match op with
                | And -> 
                    if (b1) then (
                        match (eval (e2)) with
                        | Ok v2    -> (
                            match v2 with
                            | VBool b2 -> Ok (VBool (b2))
                            | _        -> Error (InvalidArgs (And))
                        )
                        | Error er -> Error (er)
                    )
                    else Ok (VBool (false))
                | Or  -> (
                    if (b1) then Ok (VBool (true))
                    else (
                        match (eval (e2)) with
                        | Ok v2    -> (
                            match v2 with
                            | VBool b2 -> Ok (VBool (b2))
                            | _        -> Error (InvalidArgs (Or))
                        )
                        | Error er -> Error (er)
                    )
                )
                | _   -> Error (InvalidArgs (op))
            )
            | _        -> Error (InvalidArgs (Add))
        )
        | Error er -> Error (er)
    )
    | If    (c, et, ef)  -> (
        match (eval (c)) with
        | Ok v     -> (
            match v with
            | VBool b -> (
                if (b) then eval (et)
                else        eval (ef)
            )
            | _       -> Error (InvalidIfCond)
        )
        | Error er -> Error (er)
    )
    | Let   (x, e1, e2)  -> (
        match (eval (e1)) with
        | Ok v     -> eval (subst (v) (x) (e2))
        | Error er -> Error (er)
    )
    | Fun   (par, body)  -> Ok (VFun (par, body))

let interp (input : string) : (value, error) result =
    match (parse (input)) with
    | Some e -> (* let _ = print_ast (e) in *) eval (e)
    | _      -> Error (ParseFail)
