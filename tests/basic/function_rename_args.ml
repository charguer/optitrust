open Optitrust
open Target
open Ast


let _ = Run.doc_script_cpp (fun _ ->

  !! Function_basic.rename_args ["x1"] [cFunDef "f"];

)
"
void f(int x){
 x = 10;
 int y;
 y = x;
 x = y;
}

int main(){}
"

let _ = Run.script_cpp (fun () ->

  !! Function_basic.rename_args [] [cFunDef "test_no_args"];

  !! Function_basic.rename_args [""] [cFunDef "test_one_arg"];
  !! Function_basic.rename_args ["x1"] [cFunDef "test_one_arg"];

  !! Function_basic.rename_args ["x1"; ""] [cFunDef "test_two_args"];
  !! Function_basic.rename_args [""; "y1"] [cFunDef "test_two_args"];
  !! Function_basic.rename_args ["x2"; "y2"] [cFunDef "test_two_args"];

  !! Function_basic.rename_args ["x1"] [cFunDef "test_method_args"];
)