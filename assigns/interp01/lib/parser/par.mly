%{
open Utils

let rec apply (e : expr) (es : expr list) : expr =
  match es with
  | []       -> e
  | eh :: et -> apply (App (e, eh)) et
%}

%token <int> NUM
%token <string> VAR

%token UNIT

%token TRUE
%token FALSE

%token ADD
%token SUB
%token MUL
%token DIV

%token MOD

%token LES
%token LEQ
%token GTR
%token GEQ
%token EQL
%token NEQ

%token AND
%token OR

%token IF
%token THEN
%token ELSE

%token LET
%token IN

%token FUN
%token TOR

%token LPAREN
%token RPAREN
%token EOF

%right OR
%right AND
%left LES LEQ GTR GEQ EQL NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF {e}

expr:
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr { If(c, e1, e2) }
  | LET; v = VAR; EQL; e1 = expr; IN; e2 = expr    { Let(v, e1, e2) }
  | FUN; v = VAR; TOR; e = expr                    { Fun(v, e) }
  | e = expr2                                      { e }

expr2:
  | e1 = expr3; e2 = expr3*     { apply (e1) (e2) }
  | e1 = expr2; ADD; e2 = expr2 { Bop(Add, e1, e2) }
  | e1 = expr2; SUB; e2 = expr2 { Bop(Sub, e1, e2) }
  | e1 = expr2; MUL; e2 = expr2 { Bop(Mul, e1, e2) }
  | e1 = expr2; DIV; e2 = expr2 { Bop(Div, e1, e2) }
  | e1 = expr2; MOD; e2 = expr2 { Bop(Mod, e1, e2) }
  | e1 = expr2; LES; e2 = expr2 { Bop(Lt, e1, e2) }
  | e1 = expr2; LEQ; e2 = expr2 { Bop(Lte, e1, e2) }
  | e1 = expr2; GTR; e2 = expr2 { Bop(Gt, e1, e2) }
  | e1 = expr2; GEQ; e2 = expr2 { Bop(Gte, e1, e2) }
  | e1 = expr2; EQL; e2 = expr2 { Bop(Eq, e1, e2) }
  | e1 = expr2; NEQ; e2 = expr2 { Bop(Neq, e1, e2) }
  | e1 = expr2; AND; e2 = expr2 { Bop(And, e1, e2) }
  | e1 = expr2; OR; e2 = expr2  { Bop(Or, e1, e2) }

expr3:
  | UNIT                     { Unit }
  | TRUE                     { True }
  | FALSE                    { False }
  | i = NUM                  { Num(i) }
  | v = VAR                  { Var(v) }
  | LPAREN; e = expr; RPAREN { e }
