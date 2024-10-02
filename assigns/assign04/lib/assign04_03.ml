open Assign04_02

type value = 
| VNum of int
| VBool of bool

let rec eval (e : expr) : value =
    match e with
    | True                    -> VBool true
    | False                   -> VBool false
    | Num n                   -> VNum n
    | Add (n1, n2)            ->
        (match eval (n1) with
            | VNum n1' ->
                (match eval (n2) with
                    | VNum n2' -> VNum (n1' + n2')
                    | _        -> VNum (0))
            | _        -> VNum (0))
    | Or (b1, b2)             ->
        (match eval (b1) with
            | VBool b1' ->
                (match eval (b2) with
                    | VBool b2' -> VBool (b1' || b2')
                    | _        -> VBool (false))
            | _        -> VBool (false))
    | IfThenElse (e1, e2, e3) ->
        match eval (e1) with
        | VBool b ->
            if b then eval (e2)
            else eval (e3)
        | _       -> VBool (false)
    