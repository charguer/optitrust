open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
    !! Loop_basic.tile "3" ~index:"bi" ~bound:TileBoundDivides [cFor "i"];
       Loop_basic.tile "3" ~index:"bj" ~bound:TileBoundAnd [cFor "j"];
  )
"
int main() {
  for (int i = 0; (i < 9); i++) {
  }
  int r;
  for (int j = 0; (j < 10); j++) {
  }
}
" *)

(* TODO: FIx the problem with the third case *)
let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.tile "2" ~index:"b${id}" ~bound:TileBoundDivides [cFor "x"];
  !! Loop_basic.tile "2" ~bound:TileBoundMin [cFor "y"];
  !! Loop_basic.tile "2" ~bound:TileBoundAnd [cFor "z"];

  !! Loop_basic.tile "2" ~index:"b${id}" ~bound:TileBoundDivides [cFor "i"];
  !! Loop_basic.tile "2" ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile "2" ~bound:TileBoundAnd [cFor "k"];
)
