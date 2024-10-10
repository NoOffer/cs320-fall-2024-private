type 'a test = 
| TestCase of 'a
| TestList of 'a test list

let rec fold_left (op : ('a -> 'b -> 'a)) (base : 'a) (t : 'b test) : 'a =
    match t with
    | TestCase (c) -> (op (base) (c))
    | TestList (l) -> match l with
        | []   -> base
        | h::t -> fold_left (op) (fold_left (op) (base) (h)) (TestList t)