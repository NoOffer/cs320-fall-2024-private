type matrix = {
    entries : float list list;
    rows : int;
    cols : int;
}

let rec append (l) (e) =
    match l with
    | []   -> [e]
    | h::t -> h::append (t) (e)

let mk_matrix (es : float list) (dim : int * int) : matrix =
    let (r, c) = dim
    in
    if r = 0 || c = 0 then {entries = []; rows = r; cols = c}
    else
        let rec mk_matrix_impl (e : float list) (currm : matrix) (currr : float list) : matrix =
            if List.length (currr) >= c then mk_matrix_impl (e) ({currm with entries = append (currm.entries) (currr)}) ([])
            else match e with
                | h::t -> mk_matrix_impl (t) (currm) (append (currr) (h))
                | []   -> currm
        in
        mk_matrix_impl (es) ({entries = []; rows = r; cols = c}) ([])
    
