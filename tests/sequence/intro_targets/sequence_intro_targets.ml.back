open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Sequence.intro_targets [cVarDef ~regexp:true "c."];
  )
"
int main() {
  int a = 0;
  int b = 0;
  int c1 = 0;
  int c2 = 0;
  int d = 0;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Sequence.intro_targets [cVarDef ~regexp:true "c."];

)
