open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop_basic.rename_index "i" [cFor "k"];
)

"
int main (){
  int x = 0;
  for (int k = 0; k < 10; k++){
    x += k;
  }
}
"

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.rename_index "i2" [cFor "i"];
  !! Loop_basic.rename_index "j2" [cFor "j"];
  !! Loop_basic.rename_index "foo" [cFor "k"];
  !! Loop_basic.rename_index "bar" [cFor "l"];
)
