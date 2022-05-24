open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->

  !! Sequence_basic.delete [cVarDef "b"];

)

"
int main() {
  int a = 0;
  int b = 1;
  a++;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Sequence_basic.delete [sInstr "a++"];
  !! Sequence_basic.iter_delete [[cVarDef "a"]; [cVarDef "v"]];
  !! Sequence_basic.delete [nbMulti; sInstr "u."];
  !! Tools.failure_expected (fun () ->
       Sequence_basic.delete [nbMulti; cInt 8]);

)
