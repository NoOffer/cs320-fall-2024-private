type ident = string

type ty =
  | TUnit
  | TInt
  | TFloat
  | TBool
  | TVar of ident
  | TList of ty
  | TOption of ty
  | TPair of ty * ty
  | TFun of ty * ty

type ty_scheme =
  | Forall of ident list * ty

type bop =
  | Add | Sub | Mul | Div | Mod
  | AddF | SubF | MulF | DivF | PowF
  | Cons | Concat
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or
  | Comma

type expr =
  | Unit | True | False | Nil | ENone
  | Int of int | Float of float
  | Var of ident
  | Assert of expr
  | ESome of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | ListMatch of
    { matched : expr
    ; hd_name : ident
    ; tl_name : ident
    ; cons_case : expr
    ; nil_case : expr
    }
  | OptMatch of
    { matched : expr
    ; some_name : ident
    ; some_case : expr
    ; none_case : expr
    }
  | PairMatch of
    { matched : expr
    ; fst_name : ident
    ; snd_name : ident
    ; case : expr
    }
  | Fun of ident * ty option * expr
  | App of expr * expr
  | Annot of expr * ty
  | Let of
    { is_rec : bool
    ; name : ident
    ; value : expr
    ; body : expr
    }

type toplet =
  { is_rec : bool
  ; name : ident
  ; value : expr
  }

type prog = toplet list

type constr = ty * ty

type stc_env = ty_scheme env
type var_set = unit env

module VarSet = struct
  let mem = Env.mem
  let empty = Env.empty
  let union = Env.union (fun _ () () -> Some ())
  let of_list l = Env.of_list (List.map (fun x -> x, ()) l)
  let to_list s = List.map (fun (x, _) -> x) (Env.to_list s)
end

type dyn_env = value env

and value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VFloat of float
  | VList of value list
  | VPair of value * value
  | VNone
  | VSome of value
  | VClos of
    { name : string option
    ; arg : string
    ; body : expr
    ; env : dyn_env
    }

type error =
  | ParseError
  | TypeError

let err_msg = function
  | ParseError -> "parse error"
  | TypeError -> "type error"

let string_of_bop = function
  | Add    -> "+"
  | Sub    -> "-"
  | Mul    -> "*"
  | Div    -> "/"
  | Mod    -> "mod"
  | AddF   -> "+."
  | SubF   -> "-."
  | MulF   -> "*."
  | DivF   -> "/."
  | PowF   -> "**"
  | Lt     -> "<"
  | Lte    -> "<="
  | Gt     -> ">"
  | Gte    -> ">="
  | Eq     -> "="
  | Neq    -> "<>"
  | And    -> "&&"
  | Or     -> "||"
  | Cons   -> "::"
  | Concat -> "@"
  | Comma  -> ","

let string_of_ty =
  let rec go = function
    | TInt          -> "int"
    | TFloat        -> "float"
    | TBool         -> "bool"
    | TUnit         -> "unit"
    | TFun (t1, t2) -> "(" ^ (go (t1)) ^ " -> " ^ (go (t2)) ^ ")"
    | TVar a        -> a
    | TList t       -> (go (t)) ^ " list"
    | TOption t     -> (go (t)) ^ " option"
    | TPair (a, b)  -> "(" ^ (go (a)) ^ " * " ^ (go (b)) ^ ")"
  in go

let print_constrs (cs : constr list) : unit =
  let rec print_constrs_impl (cs : constr list) : unit =
    match cs with
    | (t1, t2)::t -> let _ = print_string ((string_of_ty (t1)) ^ " = " ^ (string_of_ty (t2)) ^ ", ") in print_constrs_impl (t)
    | _           -> ()
  in
  let _ = print_string ("[")
  in
  let _ = print_constrs_impl (cs)
  in
  print_endline ("]")

let print_expr (e : expr) : unit =
  let rec print_expr_impl (e : expr) (prefix_inhe : string) (prefix_curr : string) : unit =
    let prefix_inc = if (prefix_curr = " ├─") then " │ " else "   "
    in
    let _ = print_string (prefix_inhe ^ prefix_curr)
    in
    match e with
      | Int    n               -> (
        let _ = print_string ("INT ") in
        print_endline (string_of_int (n))
      )
      | Float  n               -> (
        let _ = print_string ("FLOAT ") in
        print_endline ((string_of_int (int_of_float (n))) ^ ". ...")
      )
      | Var    s               -> (
        let _ = print_string ("VAR ") in
        print_endline (s)
      )
      | Unit                  -> (
        print_endline ("()")
      )
      | Nil                   -> (
        print_endline ("Nil")
      )
      | ENone                 -> (
        print_endline ("NONE")
      )
      | ESome e               -> (
        let _ = print_endline ("SOME ") in
        print_expr_impl (e) (prefix_inhe ^ prefix_inc) (" └─")
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
        let _ = print_endline ("BOP " ^ string_of_bop (op)) in
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
        let _ = print_endline ("LET") in

        let _ = print_string (prefix_inhe ^ "   ") in
        let _ = print_endline (" ├─VAR " ^ l.name) in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "EQUAL") in

        let _ = print_expr_impl (l.value) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITHIN") in

        print_expr_impl (l.body) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Fun    (par, ty, body) -> (
        let _ = match ty with
          | Some ty -> print_endline ("FUNC OF " ^ par ^ " : " ^ (string_of_ty (ty)))
          | _       -> print_endline ("FUNC OF " ^ par)
        in
        print_expr_impl (body) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Assert cond            -> (
        let _ = print_endline ("ASSERT") in
        print_expr_impl (cond) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | ListMatch lm -> (
        let _ = print_endline ("LIST MATCH") in

        let _ = print_expr_impl (lm.matched) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITH []") in

        let _ = print_expr_impl (lm.nil_case) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITH " ^ lm.hd_name ^ "::" ^ lm.tl_name) in

        print_expr_impl (lm.cons_case) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | OptMatch  om -> (
        let _ = print_endline ("OPTION MATCH") in

        let _ = print_expr_impl (om.matched) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITH NONE") in

        let _ = print_expr_impl (om.none_case) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITH SOME" ^ om.some_name) in

        print_expr_impl (om.some_case) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | PairMatch pm -> (
        let _ = print_endline ("PAIR MATCH") in

        let _ = print_expr_impl (pm.matched) (prefix_inhe ^ prefix_inc) (" ├─") in

        let _ = print_endline (prefix_inhe ^ prefix_inc ^ "WITH (" ^ pm.fst_name ^ ", " ^ pm.snd_name ^ ")") in

        print_expr_impl (pm.case) (prefix_inhe ^ prefix_inc) (" └─")
      )
      | Annot  (e, t)          -> (
        let _ = print_endline ("ANNOT EXPR") in

        let _ = print_expr_impl (e) (prefix_inhe ^ prefix_inc) (" ├─") in

        print_endline (prefix_inhe ^ prefix_inc ^ "WITH TYPE " ^ (string_of_ty (t)))
      )
  in
  print_expr_impl (e) ("") ("   ")

let rec print_prog (p : toplet list) : unit =
  match p with
  | []   -> ()
  | h::t -> let _ = print_expr (h.value)
    in
    print_prog (t)
