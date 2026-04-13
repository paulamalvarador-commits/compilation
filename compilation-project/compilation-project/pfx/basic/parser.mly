%{
  (* Ocaml code here*)
  open Ast

%}

(**************
 * The tokens *
 **************)

(* enter tokens here, they should begin with %token *)

%token EOF
%token PUSH POP SWAP ADD SUB MUL DIV REM
%token EXEC GET
%token LPAREN RPAREN
%token <int> INT


(******************************
 * Entry points of the parser *
 ******************************)

(* enter your %start clause here *)
%start <Ast.program> program

%%

(*************
 * The rules *
 *************)

(* list all rules composing your grammar; obviously your entry point has to be present *)

program:
  | i=INT cmds=commands EOF { (i, cmds) }

commands:
  |                         { [] }
  | c=command cs=commands   { c :: cs }

command:
  | PUSH n=INT              { Push n }
  | POP                     { Pop }
  | SWAP                    { Swap }
  | ADD                     { Add }
  | SUB                     { Sub }
  | MUL                     { Mul }
  | DIV                     { Div }
  | REM                     { Rem }
  | EXEC                    { Exec }
  | GET                     { Get }
  | LPAREN cmds=commands RPAREN { Code cmds }

%%
