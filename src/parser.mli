type token =
  | COH
  | OBJ
  | PIPE
  | MOR
  | PATH
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | COL
  | IDENT of (string)
  | STRING of (string)
  | CHECK
  | EVAL
  | HYP
  | ENV
  | EQUAL
  | LET
  | IN
  | TEST
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Command.prog
