type ident = string

type expr' = 
| True
| False
| Num of int
| Var of ident
| Let of ident * expr' * expr'
| Add of expr' * expr'
| Or of expr' * expr'
| IfThenElse of expr' * expr' * expr'

type ty' = 
| Int
| Bool

type context = (ident * ty') list

let rec find_type_in_ctx (i : ident) (c : context) : ty' option =
    match c with
    | []   -> None
    | h::t -> let (id, ty) = h
        in
        if id = i then Some ty
        else find_type_in_ctx (i) (t)

let rec type_of' (c : context) (e : expr') : ty' option =
    match e with
    | True                    -> Some Bool
    | False                   -> Some Bool
    | Num _n                  -> Some Int
    | Var id                  -> find_type_in_ctx (id) (c)
    | Let (id, e1, e2)        ->
        (match type_of' (c) (e1) with
            | None    -> None
            | Some ty -> type_of' ((id, ty)::c) (e2))
    | Add (n1, n2)            ->
        let ty_n1 : ty' option = type_of' (c) (n1)
        in
        if ty_n1 = Some Int && ty_n1 = type_of' (c) (n2) then Some Int
        else None
    | Or (b1, b2)             ->
        let ty_b1 : ty' option = type_of' (c) (b1)
        in
        if ty_b1 = Some Bool && ty_b1 = type_of' (c) (b2) then Some Bool
        else None
    | IfThenElse (e1, e2, e3) ->
        if type_of' (c) (e1) = Some Bool then
            let ty_e2 : ty' option = type_of' (c) (e2)
            in
            if ty_e2 = type_of' (c) (e3) then ty_e2
            else None
        else None
    