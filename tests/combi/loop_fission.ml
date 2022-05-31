open Optitrust
open Target


let _ = Run.doc_script_cpp (fun () -> 

  !! Loop.fission ~split_between:true [cFor "i"];

)
"
int main(){
  int x, y;
  for (int i = 0; i < 10; i++){
    x = i;
    y = i;
  }
}
"


let _ = Run.script_cpp ( fun _ ->

  !! Loop.fission ~split_between:true [cFor "i"];

)
