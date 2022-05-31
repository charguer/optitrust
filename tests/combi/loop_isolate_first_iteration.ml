open Optitrust
open Target

let _ = Run.doc_script_cpp (fun () -> 

  !! Loop.isolate_first_iteration [cFor "i"];

)
"
int main(){
  int x = 0;
  for (int i = 0; i < 10; i++){
    x += i;
  }
}
"

let _ = Run.script_cpp (fun () ->

  !! Loop.isolate_first_iteration [cFor "i"];
  !! Loop.isolate_first_iteration [cFor "j"];
  !! Loop.isolate_first_iteration [cFor "k"];
  !! Loop.isolate_first_iteration [cFor "l"];

)