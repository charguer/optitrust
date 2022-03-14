open Optitrust
open Target
open Ast

let _ = Run.script_cpp (fun _ ->

  !! Matrix.delocalize ~last:true "bagNext" ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"bagKind" ~indices:["idCell"] ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_merge", "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"]];
  !! Trace.alternative (fun () ->
        !! Matrix.delocalize "bagNext" ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_merge", "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"]];
        !!());
)
