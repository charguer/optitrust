open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Loop.fold_instrs ~index:"k" [sInstr "a +="];
)
"
int main() {
  int a = 0;
  a += 0;
  a += 1;
  a += 2;
  a += 3;
}
"

let _ = Run.script_cpp (fun _ ->

  !! Loop.fold_instrs  ~index:"k" [sInstrRegexp "values\\[.\\] ="];
)