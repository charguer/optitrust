open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
    !! Function_basic.dsp_def [cFunDef "f"];
)

"
int f(int x){
  if (x > 0){
    return x;
  }
  else {
    return -x;
  }
}
"


let _ = Run.script_cpp (fun _ -> 

  !! Function_basic.dsp_def [cFunDef "test_simpl"];
  !! Function_basic.dsp_def [cFunDef "test_one_branch"];
  !! Function_basic.dsp_def [cFunDef "test_branches"];
)
