open Optitrust
open Syntax
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.extend_range ~start:ExtendToZero ~stop:(ExtendTo (expr "r")) [cFor "i"];
)

"
int main () {
  int x = 0;
  int r = 12;
  for (int i = 2; i < 10; i++){
    x += i;
  }
}
"

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.extend_range ~start:ExtendToZero [cFor "i"];
  !! Loop_basic.extend_range ~stop:(ExtendBy (trm_int 5)) [cFor "j"];
  !! Loop_basic.extend_range ~start:(ExtendBy (expr "ld")) ~stop:(ExtendTo (expr "u")) [cFor "k"];
  (* TODO LATER: combi Loop.shrink_range :
     Loop_basic.split_range + ... + Loop_basic.delete_void *)
)
