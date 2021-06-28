open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Typedef.insert "typedef struct {int x; int y} vect" [tBefore;cTopFun "main"];
    !! Typedef.insert "typedef struct {vect speed; vect pos} particle" [tBefore;cTopFun "main"];
    
) 