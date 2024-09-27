let mk_unique_keys (alst : (string * int) list) : (string * int) list =
    let rec inc_key_value (key : string) (value : int) (l : (string * int) list) : (string * int) list =
        match l with
        | []   -> [(key, value)]
        | h::t -> let (k, v) = h
            in
            if k = key then (k, v + value)::t
            else h::inc_key_value (key) (value) (t)
    in
    let rec mk_unique_keys_impl (alst : (string * int) list) (k : int) : (string * int) list =
        if k < 1 then alst
        else match alst with
            | []   -> []
            | h::t -> let (key, value) = h
                in
                mk_unique_keys_impl (inc_key_value (key) (value) (t)) (k - 1)
    in
    mk_unique_keys_impl (alst) (List.length (alst))