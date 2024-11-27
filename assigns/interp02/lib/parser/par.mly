%{
open Utils

let rec apply (e : sfexpr) (es : sfexpr list) : sfexpr =
  match es with
  | []       -> e
  | eh :: et -> apply (SApp (e, eh)) et
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
%token REC
%token IN

%token FUN
%token TOR
%token COL

%token LPAREN
%token RPAREN

%token INT
%token BOOL
%token TP_UNIT

%token ASSERT

%token EOF

%right OR
%right AND
%left LES LEQ GTR GEQ EQL NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | e = toplet*; EOF {e}

toplet:
  | LET; v = VAR; al = arg*; COL; t = ty; EQL; e = expr                { { is_rec = false; name = v; args = al; ty = t; value = e } }
  | LET; REC; v = VAR; da = arg; oa = arg*; COL; t = ty; EQL; e = expr { { is_rec = true; name = v; args = da::oa; ty = t; value = e } }

arg:
  | LPAREN; v = VAR; COL; t = ty; RPAREN { (v, t) }

ty:
  | INT                    { IntTy }
  | BOOL                   { BoolTy }
  | TP_UNIT                { UnitTy }
  | ta = ty; TOR; tb = ty  { FunTy(ta, tb) }
  | LPAREN; t = ty; RPAREN { t }

expr:
  | LET; v = VAR; al = arg*; COL; t = ty; EQL; e1 = expr; IN; e2 = expr                { SLet{ is_rec = false; name = v; args = al; ty = t; value = e1; body = e2 } }
  | LET; REC; v = VAR; da = arg; oa = arg*; COL; t = ty; EQL; e1 = expr; IN; e2 = expr { SLet{ is_rec = true; name = v; args = da::oa; ty = t; value = e1; body = e2 } }
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr                                     { SIf(c, e1, e2) }
  | FUN; da = arg; oa = arg*; TOR; e = expr                                            { SFun{ arg = da; args = oa; body = e } }
  | e = expr2                                                                          { e }

expr2:
  | e1 = expr2; ADD; e2 = expr2 { SBop(Add, e1, e2) }
  | e1 = expr2; SUB; e2 = expr2 { SBop(Sub, e1, e2) }
  | e1 = expr2; MUL; e2 = expr2 { SBop(Mul, e1, e2) }
  | e1 = expr2; DIV; e2 = expr2 { SBop(Div, e1, e2) }
  | e1 = expr2; MOD; e2 = expr2 { SBop(Mod, e1, e2) }
  | e1 = expr2; LES; e2 = expr2 { SBop(Lt, e1, e2) }
  | e1 = expr2; LEQ; e2 = expr2 { SBop(Lte, e1, e2) }
  | e1 = expr2; GTR; e2 = expr2 { SBop(Gt, e1, e2) }
  | e1 = expr2; GEQ; e2 = expr2 { SBop(Gte, e1, e2) }
  | e1 = expr2; EQL; e2 = expr2 { SBop(Eq, e1, e2) }
  | e1 = expr2; NEQ; e2 = expr2 { SBop(Neq, e1, e2) }
  | e1 = expr2; AND; e2 = expr2 { SBop(And, e1, e2) }
  | e1 = expr2; OR; e2 = expr2  { SBop(Or, e1, e2) }
  | ASSERT; e = expr3           { SAssert(e) }
  | e1 = expr3; e2 = expr3*     { apply (e1) (e2) }

expr3:
  | UNIT                     { SUnit }
  | TRUE                     { STrue }
  | FALSE                    { SFalse }
  | i = NUM                  { SNum(i) }
  | v = VAR                  { SVar(v) }
  | LPAREN; e = expr; RPAREN { e }
