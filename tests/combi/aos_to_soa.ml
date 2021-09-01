open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays.aos_to_soa [cVarDef "w"];
  (* TODO: this should be directed by the type "vect2", not by the variable "w" *)
)


