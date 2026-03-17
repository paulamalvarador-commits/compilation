
(* The type of tokens. *)

type token = 
  | SWAP
  | SUB
  | REM
  | PUSH
  | POP
  | MUL
  | INT of (int)
  | EOF
  | DIV
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
