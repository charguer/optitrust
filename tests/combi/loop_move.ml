open Optitrust
open Target

let _ = Run.doc_script_cpp (fun () ->

  !! Loop.move [cFor "k"] ~before:[cFor "i"]

)
"
int main () {
  int x = 0;
  for (int i = 0; i  < 10; i++){
    for (int j = 0; j < 10; j++){
      for (int k = 0; k < 10; k++){
        x = i + j + k;
      }
    }
  }
}
"


let _ = Run.script_cpp (fun _ ->
  
  !! Loop.move [cFor "y"] ~before:[cFor "cx"];

)
