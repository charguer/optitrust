open Optitrust
open Target


let _ = Run.doc_script_cpp (fun _ ->
   !! Instr.read_last_write [cVarDef "y"; cVar "x"];

)
"
int main() {
  int x = 0;
  x = 1;
  int y = x;
}
"

let _ = Run.script_cpp (fun _->

    !! Instr.read_last_write [cRead ~addr:[cVar "x" ] ()];
    !! Instr.read_last_write [cWriteVar "a"; dRHS];

    !! Trace.alternative (fun _ ->
      !! Instr.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
      !! Instr.read_last_write ~write:[cCellWrite ~index:[cInt 0] ()] [cWriteVar "a"; dRHS];
      !!();
    );

)
