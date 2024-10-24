open Utils

let rec type_of (e : expr) : ty option =
    match e with
    | Num _n          -> Some TInt
    | Add (n1, n2)    ->
        if (type_of (n1) = Some TInt) && (type_of (n2) = Some TInt) then Some TInt
        else None
    | Lt (e1, e2)     ->
        if (type_of (e1) = type_of (e2)) then Some TBool
        else None
    | Ite (c, et, ef) ->
        if (type_of (c) = Some TBool) then
            let t = type_of (et)
            in
            if (type_of (ef) = t) then t
            else None
        else None