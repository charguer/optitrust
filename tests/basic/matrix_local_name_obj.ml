open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    !! Matrix_basic.local_name  "bagNext" ~into:"bagNexts" ~local_ops:(Local_obj ("bag_init", "bag_merge")) [cFor "idCell" ~body:[cFun "bag_push"]];
)

