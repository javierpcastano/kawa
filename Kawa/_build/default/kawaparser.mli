
(* The type of tokens. *)

type token = 
  | WHILE
  | VOID
  | VAR
  | T_INT
  | TRUE
  | THIS
  | SUB
  | SEMI
  | RPAR
  | RETURN
  | REM
  | PRINT
  | OR
  | NOT
  | NEW
  | NEQS
  | NEQ
  | MUL
  | METHOD
  | MAIN
  | LT
  | LPAR
  | LE
  | INT of (int)
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FALSE
  | EXTENDS
  | EQS
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DIV
  | COMMA
  | CLASS
  | BOOL
  | BEGIN
  | ATTRIBUTE
  | AS
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Kawa.program)
