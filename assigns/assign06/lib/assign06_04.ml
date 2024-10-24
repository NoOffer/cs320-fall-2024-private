open Utils

let rec eval (e : expr) : value =
    match e with
    | Num n           -> VNum (n)
    | Add (n1, n2)    -> (
        match (eval (n1)) with
        | VNum v1 -> (
            match (eval (n2)) with
            | VNum v2 -> VNum (v1 + v2)
            | VBool _ -> VNum (-1)
        )
        | VBool _ -> VNum (-1)
    )
    | Lt (e1, e2)     -> (
        match (eval (e1)) with
        | VNum v1 -> (
            match (eval (e2)) with
            | VNum v2 -> VBool (v1 < v2)
            | VBool _ -> VBool (true)
        )
        | VBool _ -> VBool (true)
    )
    | Ite (c, et, ef) -> (
        match (eval (c)) with
        | VNum _   -> VBool (false)
        | VBool vc -> (
            if vc then eval (et)
            else eval (ef)
        )
    )