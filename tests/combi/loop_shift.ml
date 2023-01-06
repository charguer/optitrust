open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.shift_to_zero "i2" [cFor "i"];
     Loop.shift "k2" (expr "shift") [cFor "k"];
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
  !! Loop.shift "i2" (expr "2") [cFor "i"];
  !! Loop.shift_to_zero "j2" [cFor "j"];
  !! Loop.shift "k2" (expr "shift") [cFor "k"];
)
