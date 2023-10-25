open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Label_basic.remove [cLabel "mylabel"]

)

"
int main() {
  int a = 0;
  mylabel: a = 1;
  int b = 2;
}
"

let _ = Run.script_cpp ( fun _ ->

  !! Label_basic.remove [cLabel "start"];
  !! List.iter (fun l -> Label_basic.remove [cLabel l]) ["loop";"cond";"incr_1";"incr_2"];
  !! Trace.failure_expected (fun () ->
            Label_basic.remove [cLabel "foo"];)

)
