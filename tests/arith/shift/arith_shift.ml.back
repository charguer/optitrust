open Optitrust
open Target
open Prelude


let _ = Run.doc_script_cpp (fun _ ->

    !! Arith_basic.shift (lit "4") [cReadVar "x"];

)

"
int x;

int y = x;
"

let _ = Run.script_cpp (fun () ->

   !! Arith_basic.shift ~inv:true (var "i")  [cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift  (var "i") [cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (var "i") ~pre_cast:(typ_double ()) [cCellRead ~base:[cVar "u"] ~index:[cVar "i"] ()];
   !! Arith_basic.shift (var "i") ~post_cast:(typ_float ()) [cCellWrite ~base:[cVar "u"] ~index:[cVar "i"] ()];

)
