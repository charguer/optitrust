open Optitrust
open Target


let _ = Run.script_cpp (fun _ ->
    !! Arrays_basic.aos_to_soa "vect" "4";
  )
