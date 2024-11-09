{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | "()"       { UNIT }

  | "true"     { TRUE }
  | "false"    { FALSE }
  
  | whitespace { read lexbuf }

  | "+"        { ADD }
  | "-"        { SUB }
  | "*"        { MUL }
  | "/"        { DIV }

  | "mod"      { MOD }
  
  | "="        { EQL }
  | "<>"       { NEQ }
  | "<="       { LEQ }
  | "<"        { LES }
  | ">="       { GEQ }
  | ">"        { GTR }

  | "&&"       { AND }
  | "||"       { OR }

  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }

  | "let"      { LET }
  | "rec"      { REC }
  | "in"       { IN }
  | "fun"      { FUN }
  | "->"       { TOR }

  | "("        { LPAREN }
  | ")"        { RPAREN }

  | num        { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var        { VAR (Lexing.lexeme lexbuf) }

  | eof        { EOF }
