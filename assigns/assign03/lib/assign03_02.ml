let gen_fib (lst : int list) (k : int) : int =
    let len_lst = List.length (lst)
    in
    let rec gen_fib_impl (l : int list) (i : int) (curr : int) : int =
        match l with
        | []   -> curr
        | h::t ->
            if i < k then
                if i < len_lst then
                    gen_fib_impl (t @ [h]) (i + 1) (curr + h)
                else
                    gen_fib_impl (t @ [curr]) (i + 1) (curr * 2 - h)
            else
                if i < len_lst then
                    h
                else
                    curr
    in
    gen_fib_impl (lst) (0) (0)