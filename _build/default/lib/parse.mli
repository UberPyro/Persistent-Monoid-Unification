
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | UNIFY
  | EOF
  | CONST of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val problem: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Glue.M.term list * Glue.M.term list)

val expr_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Glue.M.term list)
