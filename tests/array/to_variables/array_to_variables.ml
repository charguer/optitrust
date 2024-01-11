open Optitrust
open Target


let _ = Run.script_cpp (fun () ->

  !! Arrays_basic.to_variables ["ua";"ub"] [cVarDef "u"];
  !! Arrays_basic.to_variables ["va";"vb"] [cVarDef "v"];

  !! Trace.failure_expected (fun _e -> true) (fun () ->
    Arrays_basic.to_variables ["nca"; "ncb"] [cVarDef "nc1"];
    Arrays_basic.to_variables ["nca"; "ncb"] [cVarDef "nc2"];
  );
)

(* LATER: should support patterns, such as
    Arrays_basic.to_variables (fun base i -> Printf.sprintf "%s%d" base i)
*)
