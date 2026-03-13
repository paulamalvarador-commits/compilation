open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  | Push n :: q , stack -> Ok (q, n :: stack)
  | Pop :: q, _ :: stack -> Ok (q, stack)
  | Swap :: q, x :: y :: stack -> Ok (q, y :: x :: stack)
  | Add :: q, x :: y :: stack -> Ok (q, (x + y):: stack)
  | Sub :: q, x :: y :: stack -> Ok (q, (x - y):: stack)
  | Mul :: q, x :: y :: stack -> Ok (q, (x * y):: stack)
  | Div :: q, x :: y :: stack -> 
    if y = 0 then Error ("Division by zero", state)
    else Ok (q, (x / y):: stack)
  | Rem :: q, x :: y :: stack -> 
    if y= 0 then Error ("Remainder by zero", state)
    else Ok (q, (x mod y):: stack)

    (*runtime errors*)
    | Pop :: _, _ -> Error ("Pop on empty stack", state)
    | Swap :: _, _ -> Error ("Swap needs at least two elements", state)
    | Add :: _, _ -> Error ("Add needs at least two elements", state)
    | Sub :: _, _ -> Error ("Sub needs at least two elements", state)
    | Mul :: _, _ -> Error ("MUl needs at least two elements", state)
    | Div :: _, _ -> Error ("Div needs at least two elements", state)
    | Rem :: _, _ -> Error ("Rem needs at least two elements", state)


let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
