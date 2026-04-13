open Ast
open Printf

(* Values stored in the stack: either integers or executable code *)
type value =
  | VInt of int
  | VCode of command list

(* Convert a value to string for debugging/printing *)
let string_of_value = function
  | VInt n -> string_of_int n
  | VCode cmds -> "(" ^ string_of_commands cmds ^ ")"

(* Convert the stack to string *)
let string_of_stack stack =
  sprintf "[%s]" (String.concat ";" (List.map string_of_value stack))


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


  (* Push an executable sequence onto the stack *)
  | Code c :: q, stack -> Ok (q, VCode c :: stack)

  (* Execute a code sequence on top of the stack *)
  | Exec :: q, VCode c :: stack -> Ok (c @ q, stack)

  (* Error: exec applied to non-code *)
  | Exec :: _, _ -> Error ("Exec expects code on top of stack", state)

  (* Get the i-th element of the stack *)
  | Get :: q, VInt i :: stack ->
      (try
         let v = List.nth stack i in
         Ok (q, v :: stack)
       with _ ->
         Error ("Get: index out of bounds", state))

  (* Error: get applied to non-integer *)
  | Get :: _, _ -> Error ("Get expects an integer", state)
  
  (* Push integer onto stack *)
  | Push n :: q, stack -> Ok (q, VInt n :: stack)

  (* Pop top of stack *)
  | Pop :: q, _ :: stack -> Ok (q, stack)

  (* Swap top two elements *)
  | Swap :: q, x :: y :: stack -> Ok (q, y :: x :: stack)

  (* Arithmetic operations: require two integers *)
  | Add :: q, VInt x :: VInt y :: stack -> Ok (q, VInt (x + y) :: stack)

  | Sub :: q, VInt x :: VInt y :: stack -> Ok (q, VInt (x - y) :: stack)

  | Mul :: q, VInt x :: VInt y :: stack -> Ok (q, VInt (x * y) :: stack)

  | Div :: q, VInt x :: VInt y :: stack ->
      if y = 0 then Error ("Division by zero", state)
      else Ok (q, VInt (x / y) :: stack)

  | Rem :: q, VInt x :: VInt y :: stack ->
      if y = 0 then Error ("Remainder by zero", state)
      else Ok (q, VInt (x mod y) :: stack)



    (*runtime errors*)
    | Pop :: _, _ -> Error ("Pop on empty stack", state)
    | Swap :: _, _ -> Error ("Swap needs at least two elements", state)
    | Add :: _, _ -> Error ("Add needs at least two elements", state)
    | Sub :: _, _ -> Error ("Sub needs at least two elements", state)
    | Mul :: _, _ -> Error ("MUl needs at least two elements", state)
    | Div :: _, _ -> Error ("Div needs at least two elements", state)
    | Rem :: _, _ -> Error ("Rem needs at least two elements", state)


let eval_program (numargs, cmds) args =
  (* Convert input arguments to VInt *)
  let args = List.map (fun x -> VInt x) args in

  let rec execute = function
    | [], [] -> Ok None
    | [], v::_ -> Ok (Some v)
    | state ->
        match step state with
        | Ok s -> execute s
        | Error e -> Error e
  in

  if numargs = List.length args then
    match execute (cmds, args) with
    | Ok None -> printf "No result\n"
    | Ok (Some (VInt result)) -> printf "= %i\n" result
    | Ok (Some _) -> printf "Non-integer result\n"
    | Error (msg, s) ->
        printf "Raised error %s in state %s\n" msg (string_of_state s)
  else
    printf "Raised error \nMismatch between expected and actual number of args\n"
