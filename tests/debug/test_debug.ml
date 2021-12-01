open Optitrust
open Target


let main = cFunDef "main"

let _ = Run.script_cpp (fun _ -> 

    !! Function.inline [cOr [
       [cFun "vect8_mul"];
       [cFun "cornerInterpolationCoeff"]]];
    !! Sequence.intro ~mark:"fuse" ~start:[main; cVarDef "coeffs2"] ~nb:4();
    !! Loop.fusion_targets [cMark "fuse"];
    !!! Instr.read_last_write [cRead ~addr:[sExpr "coeffs2.v"] ()];
    !!! ();

)