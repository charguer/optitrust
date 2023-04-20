open Optitrust
open Target
open Ast

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.shift ~index:"i2" StartAtZero [cFor "i"];
     Loop.shift (ShiftBy (expr "shift")) [cFor "k"];
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
  !! Loop.shift ~index:"i_s" (ShiftBy (trm_int 2)) [occFirst; cFor "i"];
  !! Loop.shift (ShiftBy (trm_int 2)) [cFor "i2"];
  !! Loop.shift ~index:"j2" StartAtZero [cFor "j"];
  !! Loop.shift ~index:"k2" ~inline:false (ShiftBy (expr "shift")) [cFor "k"];
  !! Loop.shift (ShiftBy (expr "shift")) [cFor "l"];
  !!! Loop.shift (StopAt (expr "N")) [cFor "m"];
  !!! Loop.shift (StartAt (trm_int 8)) [cFor "m"];
  !! Loop.shift StartAtZero [cFor "i"];
)
