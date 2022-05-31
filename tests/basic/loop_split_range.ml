open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->

  !! Loop_basic.split_range ~nb:2 [cFor "i"];
     Loop_basic.split_range ~cut:(expr "cut") [cFor "k"];
)

"
int main (){
  for (int i = 0; i < 10; i++){
    x += i;
  }
  int cut = 2;
  for (int k = 0; k < 10; k++){
    x += k;
  }

}
"

let _ = Run.script_cpp(fun _ ->

  !! Loop_basic.split_range ~nb:5 [cFor "i"];
  !! Loop_basic.split_range ~nb:5 [cFor "j"];

  !! Loop_basic.split_range ~cut:(expr "cut") [cFor "k"];
  !! Loop_basic.split_range ~cut:(expr "cut") [cFor "l"];

)
