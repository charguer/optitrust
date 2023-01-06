open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.shift_to_zero "i2" [cFor "i"];
     Loop_basic.shift "k2" (expr "shift") [cFor "k"];
)

"
int main (){
  int x = 0;
  for (int i = 2; i < 12; i++){
    x += i;
  }
  int shift = 2;
  for (int k = 0; k < 10; k++){
    x += k;
  }
}
"

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.shift "i2" (expr "2") [cFor "i"];
  !! Loop_basic.shift_to_zero "j2" [cFor "j"];
  !! Loop_basic.shift "k2" (expr "shift") [cFor "k"];
)
