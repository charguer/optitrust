open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays.to_variables ["ua";"ub"] [cVarDef "u"];
  !! Arrays.to_variables ["va";"vb"] [cVarDef "v"];
  (* TODO: fix  U ua = @; *)
  (* TODO: raise error if find an occurence of "u" that is not an array access *)
)
