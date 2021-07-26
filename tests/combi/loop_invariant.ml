open Optitrust
open Target

(* TODO: Fix me *)
let _ = Run.script_cpp (fun _ ->
  !! Loop.invariant [cVarDef "x"];
  !! Loop.invariant [cVarDef "x"];
  !! Loop.invariant [cVarDef "s"];
  !! Loop.invariant [cVarDef "s"];

  (* TODO: rename this to invariant_basic *)
  (* TODO: develop at combi level Loop_basic.invariant
        Loop.invariant [cVarDef "s"];
        --> goes out of one loop

        Loop.invariant ~upto:"i" [cVarDef "s"];
        --> meaning goes out of loops until passing loop "i"
        ==> maybe the easiest is to first count the number [nb] of loops
            in the path to the instruction backwards until the loop on "i",
            then repeat nb times "Loop.invariant_basic"
        ==> List.rev on path, walk and count loops until loop "i",
            raise error if not finding loop "i"
  *)
)
