open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence_basic.intro_on_instr [cVarDef "x"];
    !! Sequence_basic.intro_on_instr [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) (); cVarDef "y"];
    (* FIXED rename Sequence_basic.sub to Sequence_basic.intro
             and Sequence_basic.wrap to Sequence_basic.intro_on_instr
        --- or just think about better names *)
)
