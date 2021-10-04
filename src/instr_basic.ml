open Target

(* [replace code tg] expects the target to point at any node of the ast,
    it then removes this node and replaces with the code entered by the user, which is merged into
    the ast automatically by Optitrust.
*)
let replace (code : string) (tg : target) : unit =
  Target.apply_on_targets (Instr_core.replace code) tg;
  Trace.reparse()

(* [replace_fun code tg] expects the target to point to a function call,
    it then replaces the name of the function cal with the one entered
    by the user

    Assumption:
      [name] is the name of an already defined function which hsa the same
      signature as function whose call is targeted by [tg]
*)
let replace_fun (name : string) (tg : target) : unit =
  Target.apply_on_targets (Instr_core.replace_fun name) tg;
  
