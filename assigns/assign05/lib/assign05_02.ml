type int_tree =
  | Leaf
  | Node of int * int_tree * int_tree

let sum_tr (t : int_tree) : int =
    let rec sum_tr_impl (t : int_tree) (cont : (int -> int)) : int =
        match t with
        | Leaf                 -> cont (0)
        | Node (a, nd_l, nd_r) ->
            sum_tr_impl (nd_l) (fun (s_l : int) -> (sum_tr_impl (nd_r) (fun (s_r : int) -> cont (s_l + a + s_r))))
    in
    sum_tr_impl (t) (fun (x : int) -> x)