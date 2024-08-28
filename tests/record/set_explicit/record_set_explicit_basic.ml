open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp ( fun _ ->
  !! Record_basic.set_explicit [sInstr "b = p"];
  (* FIXME: !! Record_basic.set_explicit [sInstr "u = a.pos"]; *)
  !! Record_basic.set_explicit [sInstr "t[MINDEX1(2, 0)] = p2"];
  !! Record_basic.set_explicit [sInstr "c = a"];
  !! Record_basic.set_explicit [sInstr "c.pos ="];
  !! Record_basic.set_explicit [sInstr "c.speed ="];


  !! Trace.restore_original();
  !! Record_basic.set_explicit [sInstr "c = a"];
  !! Record_basic.set_explicit [nbMulti;cOr [[sInstr "c.pos ="]; [sInstr "c.speed ="]]];

)
