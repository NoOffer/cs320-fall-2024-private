
type ty =
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy of ty * ty

let string_of_ty =
  let rec go = function
    | IntTy -> "int"
    | BoolTy -> "bool"
    | UnitTy -> "unit"
    | FunTy (t1, t2) ->
      go' t1 ^ " -> " ^ go t2
  and go' = function
    | IntTy -> "int"
    | BoolTy -> "bool"
    | UnitTy -> "unit"
    | FunTy (t1, t2)-> "(" ^ go (FunTy (t1, t2)) ^ ")"
  in go

type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

let string_of_bop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "mod"
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Eq -> "="
  | Neq -> "<>"
  | And -> "&&"
  | Or -> "||"

type sfexpr =
  | SUnit
  | STrue
  | SFalse
  | SNum of int
  | SVar of string
  | SFun of
    { arg : string * ty
    ; args : (string * ty) list
    ; body : sfexpr
    }
  | SApp of sfexpr * sfexpr
  | SLet of
    { is_rec : bool
    ; name : string
    ; args : (string * ty) list
    ; ty : ty
    ; value : sfexpr
    ; body : sfexpr
    }
  | SIf of sfexpr * sfexpr * sfexpr
  | SBop of bop * sfexpr * sfexpr
  | SAssert of sfexpr

type toplet =
  { is_rec : bool
  ; name : string
  ; args : (string * ty) list
  ; ty : ty
  ; value : sfexpr
  }

type prog = toplet list

type expr =
  | Unit
  | True
  | False
  | Num of int
  | Var of string
  | If of expr * expr * expr
  | Bop of bop * expr * expr
  | Fun of string * ty * expr
  | App of expr * expr
  | Let of
    { is_rec: bool
    ; name: string
    ; ty : ty
    ; value : expr
    ; body : expr
    }
  | Assert of expr

type value =
  | VUnit
  | VBool of bool
  | VNum of int
  | VClos of
    { name : string option
    ; arg : string
    ; body : expr
    ; env : value env
    }

type error =
  | ParseErr
  | UnknownVar of string
  | IfTyErr of ty * ty
  | IfCondTyErr of ty
  | OpTyErrL of bop * ty * ty
  | OpTyErrR of bop * ty * ty
  | FunArgTyErr of ty * ty
  | FunAppTyErr of ty
  | LetTyErr of ty * ty
  | AssertTyErr of ty

let err_msg = function
  | ParseErr -> "parse error"
  | UnknownVar x -> "Unbound value " ^ x
  | IfTyErr (then_ty, else_ty) ->
    "else-case of if-expression has type "
    ^ string_of_ty else_ty
    ^ " but an expression was expected of type "
    ^ string_of_ty then_ty
  | IfCondTyErr ty ->
    "condition of if-expression has type "
    ^ string_of_ty ty
    ^ " but an expression was expected of type bool"
  | OpTyErrL (op, t1, t2) ->
    "left argument of operator ("
    ^ string_of_bop op
    ^ ") has type "
    ^ string_of_ty t2
    ^ " but an expression was expected of type "
    ^ string_of_ty t1
  | OpTyErrR (op, t1, t2) ->
    "right argument of operator ("
    ^ string_of_bop op
    ^ ") has type "
    ^ string_of_ty t2
    ^ " but an expression was expected of type "
    ^ string_of_ty t1
  | FunArgTyErr (t1, t2) ->
    "argument of function has type "
    ^ string_of_ty t2
    ^ " but an expression was expected of type "
    ^ string_of_ty t1
  | FunAppTyErr ty ->
    "an expression of type "
    ^ string_of_ty ty
    ^ " is not a function; it cannot be applied"
  | LetTyErr (expected, actual) ->
    "let-defined value has type "
    ^ string_of_ty actual
    ^ " but an expression was expected of type "
    ^ string_of_ty expected
  | AssertTyErr ty ->
    "argument of assert has type "
    ^ string_of_ty ty
    ^ " but an expression was expected of type bool"

let print_expr (e : expr) : unit =
  let rec print_expr_impl (e : expr) (prefix_inhe : string) (prefix_curr : string) : unit =
    let prefix_inc = if (prefix_curr = " ├─") then " │ " else "   "
    in
    let _ = print_string (prefix_inhe ^ prefix_curr)
    in
    match e with
      | Num    n               -> (
        let _ = print_string ("NUM ") in
        print_endline (string_of_int (n))
      )
      | Var    s               -> (
        let _ = print_string ("VAR ") in
        print_endline (s)
      )
      | Unit                  -> (
        print_endline ("()")
      )
      | True                  -> (
        print_endline ("True")
      )
      | False                 -> (
        print_endline ("False")
      )
      | App    (e1, e2)        -> (
        let _ = print_endline ("APP") in
        let _ = print_expr_impl (e1) (prefix_inhe ^ prefix_inc) (" ├─") in
        print_expr_impl (e2) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Bop    (op, e1, e2)    -> (
        let _ = print_endline ("BOP" ^ string_of_bop (op)) in
        let _ = print_expr_impl (e1) (prefix_inhe ^ prefix_inc) (" ├─") in
        print_expr_impl (e2) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | If     (c, et, ef)     -> (
        let _ = print_endline ("IF") in

        let _ = print_expr_impl (c) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "THEN") in

        let _ = print_expr_impl (et) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "ELSE") in

        print_expr_impl (ef) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Let    l               -> (
        let _ = print_endline ("LET ") in

        let _ = print_string (prefix_inhe ^ "   ") in
        let _ = print_endline (" ├─VAR " ^ l.name ^ " : " ^ (string_of_ty (l.ty))) in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "EQUAL") in

        let _ = print_expr_impl (l.value) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITHIN") in

        print_expr_impl (l.body) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Fun    (par, ty, body) -> (
        let _ = print_endline ("FUNC OF " ^ par ^ " : " ^ (string_of_ty (ty))) in
        print_expr_impl (body) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Assert cond            -> (
        let _ = print_endline ("ASSERT") in
        print_expr_impl (cond) (prefix_inhe ^ prefix_inc) (" └─")
      )
  in
  print_expr_impl (e) ("") ("   ")

