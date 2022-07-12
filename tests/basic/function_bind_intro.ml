open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  
  !! Function_basic.bind_intro ~fresh_name:"a" [cFun "g"];

)

"
int f(int x) { return (x + 1); }

int g(int x) { return (x + 1); }

int main() {
  int b = f(g(1));
}
"

let _ = Run.script_cpp (fun _ ->

  (* Function tests *)
  let fun_tg = cTopFunDef "test_function" in 
  !! Function_basic.bind_intro ~fresh_name:"s" [fun_tg; cFun "h"];
  !! Function_basic.bind_intro ~fresh_name:"b" [fun_tg; sExpr "f(a)"];
  (* same with a mark *)
  let my_mark = "__my_mark" in
  !! Function_basic.bind_intro ~my_mark ~fresh_name:"r" [fun_tg; cFun "g"];
  !! Marks.remove my_mark [fun_tg; cMark my_mark];

  (* Method tests *)
  let meth_tg = cTopFunDef "test_method" in
  !! Function_basic.bind_intro ~fresh_name:"s" [meth_tg; cWriteVar "a"; cFun "g"];


  let my_mark = "__my_mark" in
  !! Function_basic.bind_intro ~my_mark ~fresh_name:"r" [meth_tg; cVarDef "b"; cFun "g"];
  !! Marks.remove my_mark [meth_tg; cMark my_mark];

)
