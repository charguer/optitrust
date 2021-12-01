open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Typedef_basic.fold ~at:[cVarDef "c"] [cTypDef "uchar"];
  )
"
typedef unsigned char uchar;

unsigned char c;
"

(* LATER: figure out why if our printing of unsigned/long follows the standard recommendations *)

let _ = Run.script_cpp (fun _ ->
  !! Typedef_basic.fold ~at:[cTypDef "vect"] [cTypDef "uint"];
  !! Typedef_basic.fold ~at:[cVarDef "y1"] [cTypDef "cdouble"];
  !! Typedef_basic.fold [cTypDef "mat2d"] ;
  !! Typedef_basic.fold [cTypDef "mat3d"];
  )
