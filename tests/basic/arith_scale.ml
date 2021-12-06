open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->
    !! Arith_basic.scale (lit "4") [cVar "x"];
  )
"
int x;

int y = x;
"


let _ = Run.script_cpp (fun () ->

   !! Arith_basic.scale ~neg:true (var "i")  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.scale  (var "i") [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.scale (var "i") ~pre_cast:(typ_double ()) [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
   !! Arith_basic.scale (var "i") ~post_cast:(typ_float ()) [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];
)
