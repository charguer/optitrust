open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ -> 

  !! Apac.parallel_task_group [cFunDef "f"];

)

"
int f(int a, int b){
  return a + b;
}

int main(){}

"


let _ = Run.script_cpp (fun _ ->

  !! Apac.parallel_task_group [cFunDef "g"];

  !! Apac.parallel_task_group [cFunDef "f"];

  !! Apac.parallel_task_group [cFunDef "h"];

  !! Apac.parallel_task_group [cFunDef "main"];

)
