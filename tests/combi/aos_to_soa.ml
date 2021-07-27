open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays.aos_to_soa [cVarDef "w"];
  (* TODO: this should be directed by the type "vect2", not by the variable "w" *)
)


(*
=> LATER: at the combi level,
   high-level combinators to automate the process
   including the feature of "looking whether a copy of the type is needed".
   (in particular when changing only one variable)


*)