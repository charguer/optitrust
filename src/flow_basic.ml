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
let insert_if ?(cond : string option) ?(cond_ast : trm option): Target.Transfo.t =
  begin try 
  let arg = combine_strm cond cond_ast in
  let reparse = not (is_trm arg) in
  Target.reparse_after ~reparse (Target.apply_on_targets (Flow_core.insert_if arg))
  with | Ast_and_code_provided -> fail None "insert_if: please choose between cond and cond_ast args"
       | No_ast_or_code_provided -> fail None "insert_if: expected for the code entered as string or the ast of that code"
  end