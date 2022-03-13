open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix_basic.delocalize ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_append")) [cLabelBody "mark"];

  !! Trace.alternative (fun () ->
    !! Matrix_basic.delocalize ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_append")) [cLabelBody "mark"];
    !!();
  )

)