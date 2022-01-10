open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Label_basic.remove [cLabel "mylabel"]
  )
"
int main() {
  int a = 0;
  mylabel: int b = 1;
  int c = 2;
}
"

let _ = Run.script_cpp ( fun _ ->

      !! Label_basic.remove [cLabel "start"];
      !! Label_basic.remove [cLabel "unit"];
      !! List.iter (fun l -> Label_basic.remove [cLabel l]) ["loop";"cond";"incr_1";"incr_2"];
      !! Tools.failure_expected (fun () ->
            Label_basic.remove [cLabel "foo"];)
)
