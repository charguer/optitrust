open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Instr_basic.move ~dest:[tAfter; cVarDef "b"] [cVarDef "c"];
  Instr_basic.move ~dest:[tAfter; sInstr "a += 6"] [sInstr "a += 5"];
  )
"
int main() {
  int a = 2;
  int c = 3 + a;
  int b = a;
  int d;
  a += 5;
  a += 6;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Instr_basic.move ~dest:[tAfter; cVarDef "b"] [cVarDef "c"];
  Instr_basic.move ~dest:[tAfter; sInstr "a += 2"] [sInstr "a += 1"];
  )
