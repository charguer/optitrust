open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
    show [cTypDef "vect"];
  

)

