type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type expr =
  | Num of int
  | Var of string
  | Unit | True | False
  | App of expr * expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr
  | Fun of string * expr

type prog = expr

type value =
  | VNum of int
  | VBool of bool
  | VUnit
  | VFun of string * expr

type error =
  | DivByZero
  | InvalidIfCond
  | InvalidArgs of bop
  | InvalidApp
  | UnknownVar of string
  | ParseFail

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VUnit -> "()"
  | VFun (_,_) -> "<fun>"

let string_of_bop = function
  | Add -> "(+)"
  | Sub -> "(-)"
  | Mul -> "(*)"
  | Div -> "(/)"
  | Mod -> "(mod)"
  | Lt -> "(<)"
  | Lte -> "(<=)"
  | Gt -> "(>)"
  | Gte -> "(>=)"
  | Eq -> "(=)"
  | Neq -> "(<>)"
  | And -> "(&&)"
  | Or -> "(||)"

let err_msg = function
  | DivByZero -> "division by zero"
  | InvalidIfCond -> "non-Boolean value given as condition"
  | InvalidArgs op -> "invalid arguments given to " ^ string_of_bop op
  | InvalidApp -> "non-function value used in function application"
  | UnknownVar x -> "unknown variable \'" ^ x ^ "\'"
  | ParseFail -> "syntax error"

let print_ast (e : expr) : unit =
  let rec print_ast_impl (e : expr) (prefix_inhe : string) (prefix_curr : string) : unit =
    let prefix_inc = if (prefix_curr = " ├─") then " │ " else "   "
    in
    match e with
      | Num   n            -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_string ("NUM ") in
        print_endline (string_of_int (n))
      )
      | Var   s            -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_string ("VAR ") in
        print_endline (s)
      )
      | Unit               -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        print_endline ("()")
      )
      | True               -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        print_endline ("True")
      )
      | False              -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        print_endline ("False")
      )
      | App   (e1, e2)     -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_endline ("APP") in
        let _ = print_ast_impl (e1) (prefix_inhe ^ prefix_inc) (" ├─") in
        print_ast_impl (e2) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Bop   (op, e1, e2) -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_endline ("BOP" ^ string_of_bop (op)) in
        let _ = print_ast_impl (e1) (prefix_inhe ^ prefix_inc) (" ├─") in
        print_ast_impl (e2) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | If    (c, et, ef)  -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_endline ("IF") in
        let _ = print_ast_impl (c) (prefix_inhe ^ prefix_inc) (" ├─") in
        let _ = print_ast_impl (et) (prefix_inhe ^ prefix_inc) (" ├─") in
        print_ast_impl (ef) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Let   (x, e1, e2)  -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_endline ("LET ") in

        let _ = print_string (prefix_inhe ^ "   ") in
        let _ = print_endline (" ├─VAR " ^ x) in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "EQUAL") in

        let _ = print_ast_impl (e1) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITHIN") in

        print_ast_impl (e2) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Fun   (par, body)  -> (
        let _ = print_string (prefix_inhe ^ prefix_curr) in
        let _ = print_endline ("FUNC OF " ^ par) in
        print_ast_impl (body) (prefix_inhe ^ prefix_inc) (" └─")
      )
  in
  print_ast_impl (e) ("") ("   ")
