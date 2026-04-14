open Ast
open BinOp

module PAst = BasicPfx.Ast

(* Shift environment: increment all positions by 1 *)
let shift_env env =
  List.map (fun (x, i) -> (x, i + 1)) env

(* Main recursive function with environment *)
let rec generate_aux env expr =
  match expr with
  | Const n ->
      [PAst.Push n]

  | Var x ->
      let i =
        try List.assoc x env
        with Not_found -> failwith ("Unbound variable: " ^ x)
      in
      [PAst.Push i; PAst.Get]

  | Binop (op, e1, e2) ->
      begin
        match op with
        | Badd ->
            generate_aux env e1 @ generate_aux env e2 @ [PAst.Add]

        | Bsub ->
            generate_aux env e1 @ generate_aux env e2 @ [PAst.Swap; PAst.Sub]

        | Bmul ->
            generate_aux env e1 @ generate_aux env e2 @ [PAst.Mul]

        | Bdiv ->
            generate_aux env e1 @ generate_aux env e2 @ [PAst.Swap; PAst.Div]

        | Bmod ->
            generate_aux env e1 @ generate_aux env e2 @ [PAst.Swap; PAst.Rem]
      end

  | Uminus e ->
      [PAst.Push 0]
      @ generate_aux env e
      @ [PAst.Swap; PAst.Sub]

  | Fun (x, e) ->
      let env' = (x, 0) :: shift_env env in
      [PAst.Code (generate_aux env' e)]

  | App (e1, e2) ->
      generate_aux env e2 @   (* argument *)
      generate_aux env e1 @   (* function *)
      [PAst.Exec; PAst.Pop]

(* Wrapper function (used externally) *)
let generate expr =
  generate_aux [] expr