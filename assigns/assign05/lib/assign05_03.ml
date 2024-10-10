type ident = string

type ty = 
| Unit
| Arr of ty * ty

type expr = 
| Var of ident
| Fun of ident * ty * expr
| App of expr * expr

type ctxt = (ident * ty) list

let rec type_of (ctx : ctxt) (e : expr) : ty option =
    let rec find_ty_in_ctx (ctx : ctxt) (id : ident) : ty option =
        match ctx with
        | []   -> None
        | h::t -> let (i, i_ty) = h
            in
            if (i = id) then Some i_ty
            else find_ty_in_ctx (t) (id)
    in
    match e with
    | Var id          -> find_ty_in_ctx (ctx) (id)
    | Fun (id, t, e1) -> (match (type_of ((id, t)::ctx) (e1)) with
      | None      -> None
      | Some t_e1 -> Some (Arr (t, t_e1)))
    | App (e1, e2)    -> match (type_of (ctx) (e1)) with
        | None              -> None
        | Some Unit         -> None
        | Some Arr (t1, t2) -> match type_of (ctx) (e2) with
          | None      -> None
          | Some t_e2 -> if (t1 = t_e2) then Some t2
            else None