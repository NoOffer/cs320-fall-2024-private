open Utils

let lex (s : string) : tok list option =
    let rec lex_impl (sl : string list) : tok list option =
        match sl with
        | []   -> Some []
        | h::t -> (
            match (tok_of_string_opt (h)) with
                | None    -> None
                | Some tk -> (
                    match (lex_impl (t)) with
                    | None   -> None
                    | Some l -> Some (tk::l)
                )
        )
    in
    lex_impl (split (s))