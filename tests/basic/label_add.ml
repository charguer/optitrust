open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->
    !! Label_basic.add "start" [cVarDef "x"] ;
    !! Label_basic.add "loop" [cFor "i"];
    !! Label_basic.add "cond" [cIf ()];
    !! Label_basic.add "incr_1" [cIf (); sInstr "x++"];
    !! Label_basic.add "incr_2" [cIf (); sInstr "i++" ];
    !! Label_basic.add "stop" [cReturn];
)

(* LATER: ARTHUR: find out whether other languages consider label
   as instructions, that is, items of sequences, and if we should
   follow that view in optitrust. If so, Label_basic.add could take as
   argument a target_between (e.g. Label_basic.add_between).
   Example:  Label_basic.add_between [cAfter; cWhile()]
   to mean add a Label_basic after a loop.
   It could be the case that OptiTrust features too kinds of labels
   that is   Trm_label of string * trm option
   - label as a standalone instruction (=> currently encoded as Trm_label ("foo", trm_unit))
   - or label around an instruction.
   *)