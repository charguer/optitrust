open Optitrust
open Target
 
let _ = Run.doc_script_cpp (fun _ ->
     !! Label_basic.add "mylabel" [cVarDef "b"]
  )
"
int main() {
  int a = 0;
  int b = 1;
  int c = 2;
}
"

let _ = Run.script_cpp (fun _ ->
    !! Label_basic.add "start" [cVarDef "x"] ;
    !! Label_basic.add "cond" [cIf ()];
    !! Label_basic.add "incr_1" [cIf (); sInstr "x++"];
    !! Label_basic.add "incr_2" [cIf (); sInstr "x--" ];
    !! Label_basic.add "stop" [cReturn];
)
