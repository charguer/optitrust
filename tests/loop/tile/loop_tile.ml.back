open Optitrust
open Prelude

let _ = Run.doc_script_cpp (fun _ ->

  !! Loop_basic.tile (lit "3") ~index:"bi" ~bound:TileDivides [cFor "i"];
  !! Loop_basic.tile (lit "3") ~index:"bj" ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile (lit "3") ~index:"bk" ~bound:TileBoundAnd [cFor "k"];

)

"
int main() {
  for (int i = 0; i < 9; i++) {
  }
  int r;
  for (int j = 0; j < 10; j++) {
  }
  int s;
  for (int k = 0; k < 10; k++) {
  }
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop_basic.tile (lit "2") ~index:"b${id}" ~bound:TileDivides [cFor "x"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundMin [cFor "y"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundAnd [cFor "z"];
  !! Loop_basic.tile (lit "2") ~index:"b${id}" ~bound:TileDivides [cFor "i"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundMin [cFor "j"];
  !! Loop_basic.tile (lit "2") ~bound:TileBoundAnd [cFor "k"];

)
