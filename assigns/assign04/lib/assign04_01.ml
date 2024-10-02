let rec lifespan (f : ('a -> 'a)) (s : 'a) (pred : ('a -> bool)) : int =
    let s : 'a = f (s)
    in
    if pred (s) then 1
    else 1 + lifespan (f) (s) (pred)

let last_function_standing (funcs : ('a -> 'a) list) (start : 'a) (pred : ('a -> bool)) : ('a -> 'a) option =
    let rec last_function_standing_impl (funcs : ('a -> 'a) list) : (('a -> 'a) * 'a) list =
        match funcs with
        | []   -> []
        | h::t -> (h, start)::last_function_standing_impl (t) 
    in
    let rec check_survive (fs : (('a -> 'a) * 'a) list) (k : int) : ('a -> 'a) option=
        if k < 1 then let k = List.length (fs)
            in
            if k < 2 then match fs with
                | []    -> None
                | h::_t -> let (f, _s) = h
                    in
                    Some f
            else match fs with
                | []   -> None
                | h::t -> let (f, s) = h
                    in
                    let s' : 'a = f (s)
                    in
                    if (pred (s')) then check_survive (t) (k - 1)
                    else check_survive (t @ [(f, s')]) (k - 1)
        else match fs with
            | []   -> None
            | h::t -> let (f, s) = h
                in
                let s' : 'a = f (s)
                in
                if (pred (s')) then check_survive (t) (k - 1)
                else check_survive (t @ [(f, s')]) (k - 1)
    in
    if List.length (funcs) < 2 then match funcs with
        | []    -> None
        | h::_t -> Some h
    else let funcs' : (('a -> 'a) * 'a) list = last_function_standing_impl (funcs)
        in
        check_survive (funcs') (List.length (funcs'))