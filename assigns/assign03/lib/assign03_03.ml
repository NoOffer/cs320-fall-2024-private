type tree = 
| Leaf of int
| Node of tree list

let collapse (i : int) (t : tree) : tree =
    let rec find_terminal (node : tree list) : tree list =
        match node with
        | []   -> []
        | h::l -> match h with
            | Leaf _     -> h::find_terminal (l)
            | Node child -> find_terminal (child) @ find_terminal (l)
    in
    let rec collapse_impl (depth : int) (nl : tree list) : tree list =
        match nl with
        | []   -> []
        | h::l -> match h with
            | Leaf _     -> h::collapse_impl (depth) (l)
            | Node child ->
                if depth < i - 1 then
                    collapse_impl (depth + 1) (child) @ collapse_impl (depth) (l)
                else
                    find_terminal (child) @ collapse_impl (depth) (l)
    in
    match t with
    | Leaf _-> t
    | Node n -> Node (collapse_impl (0) (n))
