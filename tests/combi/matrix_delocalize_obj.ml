open Optitrust
open Target
open Syntax




let _ = Run.script_cpp (fun _ ->
  let ops = Local_obj (
    name_to_var "bag_init",
    name_to_var "bag_merge",
    name_to_var "bag_free"
  ) in
  let fv = find_var_in_current_ast in

  !! Matrix.delocalize ~last:true (fv "bagNext") ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"bagKind" ~indices:["idCell"] ~acc:"sum" ~ops [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext"]];

  let alloc_instr = [cTopFunDef "allocate"; cWriteVar "bagNext1"] in
  !! Matrix.delocalize ~last:true (fv "bagNext1")~into:"bagNexts1" ~alloc_instr ~labels:["alloc"; ""; "dealloc"] ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"bagKind" ~indices:["idCell"] ~acc:"sum" ~ops [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext1"]];
  !! Trace.alternative (fun () ->
        !! Matrix.delocalize (fv "bagNext") ~into:"bagNexts" ~init_zero:true ~acc_in_place:false ~dim:(var "N0") ~index:"i0" ~acc:"sum" ~ops [cFor "idCell" ~body:[cFun "bag_push"; cVar "bagNext"]];
        !!());
)
