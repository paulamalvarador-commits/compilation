open BasicPfx

let string_of_token = function
  | Parser.EOF -> "EOF"
  | Parser.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Parser.PUSH -> "PUSH"
  | Parser.POP -> "POP"
  | Parser.SWAP -> "SWAP"
  | Parser.ADD -> "ADD"
  | Parser.SUB -> "SUB"
  | Parser.MUL -> "MUL"
  | Parser.DIV -> "DIV"
  | Parser.REM -> "REM"

let rec print_tokens lexbuf =
  let tok = Lexer.token lexbuf in
  print_endline (string_of_token tok);
  match tok with
  | Parser.EOF -> ()
  | _ -> print_tokens lexbuf

let test_lexer file =
  print_endline ("File " ^ file ^ " is being treated!");
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  Utils.Location.init lexbuf file;
  try
    print_tokens lexbuf
  with Utils.Location.Error (msg, loc) ->
    Printf.printf "Raised error: %s\n" msg;
    Utils.Location.print loc
  ;
  close_in ic

let () =
  Arg.parse [] test_lexer ""