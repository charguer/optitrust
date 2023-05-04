open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.slide ~size:(trm_int 3) ~step:(trm_int 1) ~index:"bi" [cFor "i"];
)

"
int main() {
  for (int i = 0; i < 9; i++) {
    int j = i;
  }
}
"

let _ = Run.script_cpp (fun _ ->
  (* i = [0; 32[ --> bi = [0; 31[ + i = [bi; bi+2[ *)
  !! Loop_basic.slide ~size:(trm_int 2) ~step:(trm_int 1) ~index:"b${id}" ~bound:TileDivides [cFor "i"];
  (* j = [0; 30[ --> bj = [0; 30[ + j = [bj; bj+2[ *)
  !! Loop_basic.slide ~size:(trm_int 2) ~step:(trm_int 2) [cFor "j"];
  (* k = [0; 15[ by 3
     [0; 3; 6; 9; 12]
   -->
     bk = [0; 12[ by 3 + k = [bk; bk+6[ by 3
     [0; 3]; [3; 6]; [6; 9]; [9; 12]
     *)
  !! Loop_basic.slide ~size:(trm_int 2) ~step:(trm_int 1) [cFor "k"];
)
