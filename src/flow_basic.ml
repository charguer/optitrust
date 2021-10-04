open Target

(* [arbitrary_if cond tg] expects the target [tg] to point to an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both it's then and else branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as teh condition in the
    if statement, then this code is transformed and integrated inside the ast.
*)
let arbitrary_if (cond : string) (tg : target) : unit =
  Target.apply_on_targets (Flow_core.arbitrary_if cond) tg;
  Trace.reparse()