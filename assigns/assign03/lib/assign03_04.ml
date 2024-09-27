let group (ilst : int list) : int list list option =
    let rec group_impl (l : int list) (res : int list list) (curr : int list) (p : int) : int list list option =
        match l with
        | []   -> Some res
        | h::t ->
            if h = 0 then
                match curr with
                | [] -> None
                | _  -> group_impl (t) (res @ [curr]) ([]) (-1 * p)
            else
                if h * p < 0 then
                    None
                else
                    group_impl (t) (res) (curr @ [h]) (if h > 0 then 1 else -1)
    in
    group_impl (ilst) ([]) ([]) (0)
