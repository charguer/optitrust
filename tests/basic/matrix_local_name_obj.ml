open Optitrust
open Syntax
open Target

let _ = Run.script_cpp (fun _ ->
  let bagNext = find_var_in_current_ast "bagNext" in
  !! Matrix_basic.local_name bagNext ~into:"bagNexts" ~local_ops:(Local_obj (name_to_var "bag_init", name_to_var "bag_merge", name_to_var "bag_free")) [cFor "idCell" ~body:[cFun "bag_push"]];
)

