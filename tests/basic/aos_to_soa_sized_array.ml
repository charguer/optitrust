open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.aos_to_soa "vect2" "N";
)
