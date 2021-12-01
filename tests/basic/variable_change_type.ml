open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Variable_basic.change_type "long" [cVarDef "a"];
  )
"
int a = 0;
"



(* TODO: typedef_basic.copy should be called insert_copy; it should come with a separate unit test *)

let _ = Run.script_cpp (fun _ ->
  !! Typedef_basic.copy "vect2" [cTypDef "vect"];
  !! Variable_basic.change_type "vect2" [cVarDef "w"];
)