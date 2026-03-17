
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
# 14 "pfx/basic/parser.mly"
       (int)
# 21 "pfx/basic/parser.ml"
  
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

# 1 "pfx/basic/parser.mly"
  
  (* Ocaml code here*)


# 36 "pfx/basic/parser.ml"

type ('s, 'r) _menhir_state

and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_1 =
  fun i ->
    (
# 32 "pfx/basic/parser.mly"
                   ( i,[] )
# 48 "pfx/basic/parser.ml"
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
  
  let _menhir_run_0 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | EOF ->
              let i = _v in
              let _v = _menhir_action_1 i in
              MenhirBox_program _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_0 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 34 "pfx/basic/parser.mly"
  

# 111 "pfx/basic/parser.ml"
