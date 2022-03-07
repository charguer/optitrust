open Optitrust
open Target

(* let _ = Run.doc_script_cpp (fun _ ->
  !! Instr_basic.read_last_write ~write:[sInstr "a ="] [cVarDef "b"; dBody];
  )
"
int main() {
  int a = 5;
  a = 6;
  int b = a;
}
" *)

let _ = Run.script_cpp (fun _->

    !! Instr_basic.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
    
    !! Instr_basic.read_last_write ~write:[cCellWrite ~index:[cInt 0] ()] [cWriteVar "a"; dRHS];

    !! Instr_basic.read_last_write ~write:[cVarDef "b"] [cWriteVar "y"; dRHS];
    (* !! Instr_basic.read_last_write ~write:[sInstr "t[0] ="] [sInstr "= t[0]"; dRHS]; *)
)
