{
  open Parser


  let mk_int lexbuf nb =
  try INT (int_of_string nb)
  with Failure _ ->
    let loc = Utils.Location.curr lexbuf in
    raise (Utils.Location.Error ("Illegal integer " ^ nb, loc))
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { Utils.Location.incr_line lexbuf; token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb { mk_int lexbuf nb }
  (* commands  *)
  | "push"                 { PUSH }
  | "pop"                  { POP }
  | "swap"                 { SWAP }
  | "add"                  { ADD }
  | "sub"                  { SUB }
  | "mul"                  { MUL }
  | "div"                  { DIV }
  | "rem"                  { REM }
  (* illegal characters *)
  | _ as c {
    let loc = Utils.Location.curr lexbuf in
    raise (Utils.Location.Error (Printf.sprintf "Illegal character '%c'" c, loc))
}
