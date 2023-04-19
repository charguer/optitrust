open Optitrust
open Ast
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.extend_range ~lower:ShiftToZero ~upper:(ShiftToVal (var "r")) [cFor "i"];
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
  !! Loop_basic.extend_range ~lower:ShiftToZero [cFor "i"];
  !! Loop_basic.extend_range ~upper:(ShiftBy (trm_int 5)) [cFor "j"];
  !! Loop_basic.extend_range ~lower:(ShiftBy (var "ld")) ~upper:(ShiftToVal (var "u")) [cFor "k"];
  (* TODO LATER: combi Loop.shrink_range :
     Loop_basic.split_range + ... + Loop_basic.delete_void *)
)
