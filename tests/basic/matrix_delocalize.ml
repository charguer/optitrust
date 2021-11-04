open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~dim:(trm_var "N0") ~index:"i0" ~acc:"sum" [cLabelBody "mark"];
)