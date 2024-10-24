open Utils

let parse (tl : tok list) : expr option =
    let rec parse_impl (tl : tok list) (stk : expr list) : expr option =
        match tl with
        | [] -> (
            match stk with
            | []   -> None
            | h::t ->
                match t with
                | [] -> Some h
                | _  -> None
        )
        | tk::t -> 
            match tk with
            | TNum n -> parse_impl (t) ((Num (n))::stk)
            | TAdd   -> (
                match stk with
                | e1::e2::stk_t -> parse_impl (t) ((Add (e2, e1))::stk_t)
                | _             -> None
            )
            | TLt    ->(
                match stk with
                | e1::e2::stk_t -> parse_impl (t) ((Lt (e2, e1))::stk_t)
                | _             -> None
            )
            | TIte   ->(
                match stk with
                | e1::e2::e3::stk_t -> parse_impl (t) ((Ite (e3, e2, e1))::stk_t)
                | _                 -> None
            )
    in
    parse_impl (tl) ([])