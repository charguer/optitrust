open Ast

(* [insert_if cond tg] expects the target [tg] to point to an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both its then and else branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as the condition in the
    if statement, then this code is transformed and integrated inside the ast.
    Note:
      The code is added as string into the ast as an arbitrary string then  the full ast will be reparsed if 
      [reparse] is set to true so that the added string gets integrated into the current ast
*)
let insert_if ~cond:(cond : trm ) ?(reparse : bool = false) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets (Flow_core.insert_if cond))
  