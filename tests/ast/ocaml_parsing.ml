open Optitrust
open Target

(* reads ocaml_parsing_in.ml *)
let _ = Run.script_ml (fun () ->


  !! Show.ast ~internal:true ();
)
