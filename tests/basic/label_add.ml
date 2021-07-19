open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Label.add "start" [cVarDef "x"] ;
    !! Label.add "loop" [cFor "i"];
    !! Label.add "cond" [cIf ()];
    !! Label.add "incr_1" [cIf (); sInstr "x++"];
    !! Label.add "incr_2" [cIf (); sInstr "i++" ];
    !! Label.add "stop" [cReturn];
)

(* LATER: ARTHUR: find out whether other languages consider label
   as instructions, that is, items of sequences, and if we should
   follow that view in optitrust. If so, Label.add could take as
   argument a target_between (e.g. Label.add_between).
   Example:  Label.add_between [cAfter; cWhile()]
   to mean add a label after a loop.
   It could be the case that OptiTrust features too kinds of labels
   that is   Trm_label of string * trm option
   - label as a standalone instruction (=> currently encoded as Trm_label ("foo", trm_unit))
   - or label around an instruction.
   *)