open Optitrust
open Target

(* TODO: make it clearer what this transfo is expected to do;
   should it have a different name than aos_to_soa? *)

(* TODO: This transfo does not seem to be correct *)

(* TODO: later add a doc

let _ = Run.script_cpp (fun () ->
  !! Arrays_basic.aos_to_soa "vects" "B";
)
