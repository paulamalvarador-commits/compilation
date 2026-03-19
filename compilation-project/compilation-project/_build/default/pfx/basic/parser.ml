
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SWAP
    | SUB
    | REM
    | PUSH
    | POP
    | MUL
    | INT of 
# 15 "pfx/basic/parser.mly"
       (int)
# 21 "pfx/basic/parser.ml"
  
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

# 1 "pfx/basic/parser.mly"
  
  (* Ocaml code here*)
  open Ast


# 37 "pfx/basic/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState01 : ('s _menhir_cell0_INT, _menhir_box_program) _menhir_state
    (** State 01.
        Stack shape : INT.
        Start symbol: program. *)

  | MenhirState13 : (('s, _menhir_box_program) _menhir_cell1_command, _menhir_box_program) _menhir_state
    (** State 13.
        Stack shape : command.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_command = 
  | MenhirCell1_command of 's * ('s, 'r) _menhir_state * (Ast.command)

and 's _menhir_cell0_INT = 
  | MenhirCell0_INT of 's * 
# 15 "pfx/basic/parser.mly"
       (int)
# 58 "pfx/basic/parser.ml"


and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_01 =
  fun n ->
    (
# 41 "pfx/basic/parser.mly"
                            ( Push n )
# 69 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_02 =
  fun () ->
    (
# 42 "pfx/basic/parser.mly"
                            ( Pop )
# 77 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_03 =
  fun () ->
    (
# 43 "pfx/basic/parser.mly"
                            ( Swap )
# 85 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_04 =
  fun () ->
    (
# 44 "pfx/basic/parser.mly"
                            ( Add )
# 93 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_05 =
  fun () ->
    (
# 45 "pfx/basic/parser.mly"
                            ( Sub )
# 101 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_06 =
  fun () ->
    (
# 46 "pfx/basic/parser.mly"
                            ( Mul )
# 109 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_07 =
  fun () ->
    (
# 47 "pfx/basic/parser.mly"
                            ( Div )
# 117 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_08 =
  fun () ->
    (
# 48 "pfx/basic/parser.mly"
                            ( Rem )
# 125 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_09 =
  fun () ->
    (
# 37 "pfx/basic/parser.mly"
                            ( [] )
# 133 "pfx/basic/parser.ml"
     : (Ast.command list))

let _menhir_action_10 =
  fun c cs ->
    (
# 38 "pfx/basic/parser.mly"
                            ( c :: cs )
# 141 "pfx/basic/parser.ml"
     : (Ast.command list))

let _menhir_action_11 =
  fun cmds i ->
    (
# 34 "pfx/basic/parser.mly"
                            ( (i, cmds) )
# 149 "pfx/basic/parser.ml"
     : (Ast.program))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | SWAP ->
        "SWAP"
    | SUB ->
        "SUB"
    | REM ->
        "REM"
    | PUSH ->
        "PUSH"
    | POP ->
        "POP"
    | MUL ->
        "MUL"
    | INT _ ->
        "INT"
    | EOF ->
        "EOF"
    | DIV ->
        "DIV"
    | ADD ->
        "ADD"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_11 : type  ttv_stack. ttv_stack _menhir_cell0_INT -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell0_INT (_menhir_stack, i) = _menhir_stack in
      let cmds = _v in
      let _v = _menhir_action_11 cmds i in
      MenhirBox_program _v
  
  let rec _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_command -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell1_command (_menhir_stack, _menhir_s, c) = _menhir_stack in
      let cs = _v in
      let _v = _menhir_action_10 c cs in
      _menhir_goto_commands _menhir_stack _v _menhir_s
  
  and _menhir_goto_commands : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState01 ->
          _menhir_run_11 _menhir_stack _v
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _v
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_command : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_command (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SWAP ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | SUB ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | REM ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | PUSH ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | POP ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | MUL ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | DIV ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | ADD ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | EOF ->
          let _v_0 = _menhir_action_09 () in
          _menhir_run_14 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_05 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_08 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_01 n in
          _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_06 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_07 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_04 () in
      _menhir_goto_command _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          let _menhir_stack = MenhirCell0_INT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SWAP ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | SUB ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | REM ->
              _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | PUSH ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | POP ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | MUL ->
              _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | DIV ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | ADD ->
              _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | EOF ->
              let _v_0 = _menhir_action_09 () in
              _menhir_run_11 _menhir_stack _v_0
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 50 "pfx/basic/parser.mly"
  

# 330 "pfx/basic/parser.ml"
