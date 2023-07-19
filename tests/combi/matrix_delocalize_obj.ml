open Optitrust
open Target
open Syntax




let _ = Run.script_cpp (fun _ ->

  !! Matrix.delocalize ~last:true "bagNext" ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"bagKind" ~indices:["idCell"] ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_merge", "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext"]];

  let alloc_instr = [cTopFunDef "allocate"; cWriteVar "bagNext1"] in
  !! Matrix.delocalize ~last:true "bagNext1" ~into:"bagNexts1" ~alloc_instr ~labels:["alloc"; ""; "dealloc"] ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"bagKind" ~indices:["idCell"] ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_merge", "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext1"]];
  !! Trace.alternative (fun () ->
        !! Matrix.delocalize "bagNext" ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops:(Local_obj ("bag_init", "bag_merge", "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext"]];
        !!());
)
