{
  open! Batteries
  open! Lexing

  open Parse
}

let whitespace = ' '+ | ['\r' '\n'] | '\r' '\n' | '\t'

let id_char = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let id_tail = ('-'? id_char)*
let id = ['a'-'z'] id_tail
let cap_id = ['A'-'Z'] id_tail

rule token = parse
  | "eof" {EOF}
  | "$" {EOF}
  | whitespace {token lexbuf}
  | id as c {CONST c}
  | cap_id as c {VAR c}
  | "=?" {UNIFY}
  | _ as s {failwith (Printf.sprintf "Unexpected character %c" s)}
