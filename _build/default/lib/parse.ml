
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 10 "lib/parse.mly"
      (string)
# 15 "lib/parse.ml"
  )
    | UNIFY
    | EOF
    | CONST of (
# 10 "lib/parse.mly"
      (string)
# 22 "lib/parse.ml"
  )
  
end

include MenhirBasics

# 1 "lib/parse.mly"
  
  open! Batteries
  open! Uref
  open! U
  open! Glue
  open! M

# 37 "lib/parse.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_expr_file) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: expr_file. *)

  | MenhirState03 : (('s, 'r) _menhir_cell1_term, 'r) _menhir_state
    (** State 03.
        Stack shape : term.
        Start symbol: <undetermined>. *)

  | MenhirState09 : ('s, _menhir_box_problem) _menhir_state
    (** State 09.
        Stack shape : .
        Start symbol: problem. *)

  | MenhirState12 : (('s, _menhir_box_problem) _menhir_cell1_expr, _menhir_box_problem) _menhir_state
    (** State 12.
        Stack shape : expr.
        Start symbol: problem. *)


and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Glue.M.term list)

and ('s, 'r) _menhir_cell1_term = 
  | MenhirCell1_term of 's * ('s, 'r) _menhir_state * (Glue.M.term)

and _menhir_box_problem = 
  | MenhirBox_problem of (Glue.M.term list * Glue.M.term list) [@@unboxed]

and _menhir_box_expr_file = 
  | MenhirBox_expr_file of (Glue.M.term list) [@@unboxed]

let _menhir_action_2 =
  fun _1 ->
    (
# 17 "lib/parse.mly"
               (_1)
# 78 "lib/parse.ml"
     : (Glue.M.term list))

let _menhir_action_3 =
  fun _1 ->
    (
# 23 "lib/parse.mly"
                    (_1)
# 86 "lib/parse.ml"
     : (Glue.M.term list))

let _menhir_action_4 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 94 "lib/parse.ml"
     : (Glue.M.term list))

let _menhir_action_5 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 102 "lib/parse.ml"
     : (Glue.M.term list))

let _menhir_action_6 =
  fun _1 _3 ->
    (
# 24 "lib/parse.mly"
                             (_1, _3)
# 110 "lib/parse.ml"
     : (Glue.M.term list * Glue.M.term list))

let _menhir_action_7 =
  fun _1 ->
    (
# 20 "lib/parse.mly"
        (Monoid (memo _1))
# 118 "lib/parse.ml"
     : (Glue.M.term))

let _menhir_action_8 =
  fun _1 ->
    (
# 21 "lib/parse.mly"
          (Letter _1)
# 126 "lib/parse.ml"
     : (Glue.M.term))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CONST _ ->
        "CONST"
    | EOF ->
        "EOF"
    | UNIFY ->
        "UNIFY"
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_problem) _menhir_cell1_expr -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let MenhirCell1_expr (_menhir_stack, _, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_6 _1 _3 in
          MenhirBox_problem _v
      | _ ->
          _eRR ()
  
  let _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_file =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_3 _1 in
          MenhirBox_expr_file _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_7 _1 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_term : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_term (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VAR _v_0 ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState03
      | CONST _v_1 ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState03
      | EOF | UNIFY ->
          let _v_2 = _menhir_action_4 () in
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 _tok
  
  and _menhir_run_02 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_8 _1 in
      _menhir_goto_term _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_04 : type  ttv_stack ttv_result. (ttv_stack, ttv_result) _menhir_cell1_term -> _ -> _ -> _ -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_term (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_5 x xs in
      _menhir_goto_list_term_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_term_ : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState09 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_05 : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_2 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack ttv_result. ttv_stack -> _ -> _ -> _ -> (ttv_stack, ttv_result) _menhir_state -> _ -> ttv_result =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _v _tok
      | MenhirState09 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState00 ->
          _menhir_run_07 _menhir_stack _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_problem) _menhir_state -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | UNIFY ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v_0 ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState12
          | CONST _v_1 ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState12
          | EOF ->
              let _v_2 = _menhir_action_4 () in
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState12 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_expr_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00
      | CONST _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00
      | EOF ->
          let _v = _menhir_action_4 () in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState00 _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_problem =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09
      | CONST _v ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09
      | UNIFY ->
          let _v = _menhir_action_4 () in
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState09 _tok
      | _ ->
          _eRR ()
  
end

let problem =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_problem v = _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

let expr_file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_expr_file v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
