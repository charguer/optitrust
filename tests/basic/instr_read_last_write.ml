open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
    !! Instr_basic.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
  )
"
int main() {
  int x = 4;
  x = 7;
  int y = x;
}
"

let _ = Run.script_cpp (fun _->

    !! Instr_basic.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];

    !! Instr_basic.read_last_write ~write:[cCellWrite ~index:[cInt 0] ()] [cWriteVar "a"; dRHS];

    !! Instr_basic.read_last_write ~write:[cVarDef "b"] [cWriteVar "y"; dRHS];
    (* !! Instr_basic.read_last_write ~write:[sInstr "t[0] ="] [sInstr "= t[0]"; dRHS]; *)
)
