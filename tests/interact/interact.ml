open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  let show = Generic.target_show in

  show [cVarDef "a"];

  let p = [cVarDef "b"] in
  show p;

  !! Label.add "m1" [cVarDef "a"];
  !! Label.add "m2" [cVarDef "b"];
  !! Label.add "m3" [cVarDef "b"];
  Label.add "m4" [cVarDef "b"];
  !! Label.add "m5" [cVarDef "b"];

  !!! Label.add "m6" [cVarDef "a"];
  !!! Label.add "m7" [cVarDef "a"];

)

