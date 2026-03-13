open Ast
open BinOp

module PAst = BasicPfx.Ast

let rec generate expr =
  match expr with
  | Const n -> [PAst.Push n]
  | Binop(op, e1, e2) ->
      begin
        match op with
        | Badd ->
            generate e1 @ generate e2 @ [PAst.Add]

        | Bsub ->
            generate e1 @ generate e2 @ [PAst.Swap; PAst.Sub]

        | Bmul ->
            generate e1 @ generate e2 @ [PAst.Mul]

        | Bdiv ->
            generate e1 @ generate e2 @ [PAst.Swap; PAst.Div]

        | Bmod ->
            generate e1 @ generate e2 @ [PAst.Swap; PAst.Rem]
      end
  | Uminus e ->
      [PAst.Push 0]
      @ generate e
      @ [PAst.Swap; PAst.Sub]
  | Var x -> failwith ("Variables not supported yet: " ^ x)
