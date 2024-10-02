type expr = 
| True
| False
| Num of int
| Add of expr * expr
| Or of expr * expr
| IfThenElse of expr * expr * expr

type ty = 
| Int
| Bool

let rec type_of (e : expr) : ty option =
    match e with
    | True                    -> Some Bool
    | False                   -> Some Bool
    | Num _n                  -> Some Int
    | Add (n1, n2)            ->
        let ty_n1 : ty option = type_of (n1)
        in
        if ty_n1 = Some Int && ty_n1 = type_of (n2) then Some Int
        else None
    | Or (b1, b2)             ->
        let ty_b1 : ty option = type_of (b1)
        in
        if ty_b1 = Some Bool && ty_b1 = type_of (b2) then Some Bool
        else None
    | IfThenElse (e1, e2, e3) ->
        if type_of (e1) = Some Bool then
            let ty_e2 : ty option = type_of (e2)
            in
            if ty_e2 = type_of (e3) then ty_e2
            else None
        else None
    