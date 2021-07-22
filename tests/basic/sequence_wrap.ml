open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence.wrap [cVarDef "x"];
    !! Sequence.wrap [cSeq ~args_pred:(Target.target_list_one_st (cVarDef "y")) (); cVarDef "y"];
    (* TODO: update implementation to use sub *)
    (* TODO: rename Sequence.sub to Sequence.intro
             and Sequence.wrap to Sequence.intro_on_instr
        --- or just think about better names *)
)
