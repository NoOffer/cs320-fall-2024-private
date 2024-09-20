type matrix = {
    entries : float list list;
    rows : int;
    cols : int;
}

let rec append (l) (e) =
    match l with
    | []   -> [e]
    | h::t -> h::append (t) (e)

let mk_matrix (entries : float list) (dim : int * int) : matrix =
    let (r, c) = dim
    in
    let rec mk_matrix_impl (entries : float list) (currm : matrix) (currr : float list) : matrix =
        if List.length currr = c then mk_matrix_impl (entries) ({currm with entries = append (currm.entries) (currr)}) ([])
        else match entries with
            | h::t -> mk_matrix_impl (t) (currm) (h::currr)
            | _    -> mk_matrix_impl (entries) (currm) (currr)
    in
    mk_matrix_impl (entries) ({entries = []; rows = r; cols = c}) ([])        
