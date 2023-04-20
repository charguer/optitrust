open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.shift "i2" StartAtZero [cFor "i"];
     Loop_basic.shift "k2" (ShiftBy (expr "shift")) [cFor "k"];
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
  !! Loop_basic.shift "i2" (ShiftBy (trm_int 2)) [cFor "i"];
  !! Loop_basic.shift "j2" StartAtZero [cFor "j"];
  !! Loop_basic.shift "k2" (ShiftBy (expr "shift")) [cFor "k"];
  !! Loop_basic.shift "l2" (ShiftBy (expr "shift")) [cFor "l"];
  !! Loop_basic.shift "m2" (StopAt (expr "N")) [cFor "m"];
  !! Loop_basic.shift "m3" (StartAt (trm_int 4)) [cFor "m2"];
)
