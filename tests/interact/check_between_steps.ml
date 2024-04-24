open Optitrust
open Target


let f ~(reparse: bool) ~(recompute_res: bool) =
  Flags.reparse_between_steps := reparse;
  Flags.recompute_resources_between_steps := recompute_res;

  Label.add "m0" [cVarDef "a"];
  Trace.without_reparsing_between_steps (fun () ->
    Label.add "m1" [cVarDef "b"];
    Label.add "m2" [cVarDef "a"];
    Label.add "m3" [cVarDef "a"];
    Label.add "m4" [cVarDef "a"];
  );
  Trace.without_resource_computation_between_steps (fun () ->
    Label.add "m5" [cVarDef "a"];
    Label.add "m6" [cVarDef "a"];
  );
  Trace.without_reparsing_between_steps (fun () ->
    Trace.without_resource_computation_between_steps (fun () ->
      Label.add "m7" [cVarDef "a"]));

  Label.remove [nbMulti; cLabel ""]


let _ = Run.script_cpp (fun () ->
  bigstep "nothing";
  !! f ~reparse:false ~recompute_res:false;

  bigstep "recompute resources only";
  !! f ~reparse:false ~recompute_res:true;

  bigstep "reparse only";
  !! f ~reparse:true ~recompute_res:false;

  bigstep "both";
  !! f ~reparse:true ~recompute_res:true;
)
