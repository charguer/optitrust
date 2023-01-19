open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.shift_to_zero ~index:"i2" [cFor "i"];
     Loop.shift (expr "shift") [cFor "k"];
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
  !! Loop.shift ~reparse:true ~index:"i_s" (expr "2") [cFor "i"];
  !! Loop.shift ~reparse:true  (expr "2") [cFor "i2"];
  !! Loop.shift_to_zero ~index:"j2" [cFor "j"];
  !! Loop.shift ~index:"k2" ~inline:false (expr "shift") [cFor "k"];
)
