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

  !! Function_basic.bind_intro ~fresh_name:"s" [cFun "h"];
  !! Function_basic.bind_intro ~fresh_name:"b" [sInstr "f(a)"];
  (* same with a mark *)
  let my_mark = "__my_mark" in
  !! Function_basic.bind_intro ~my_mark ~fresh_name:"r" [cFun "g"];
  !! Marks.remove my_mark [cMark my_mark];
)
