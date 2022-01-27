open Optitrust
open Target

(* TODO: generalize read_last_write so that if the target is not a read operation,
  it considers the surrounding read operation.
  (it seems to work in inline_last_write, see the doc_script there) *)

let _ = Run.doc_script_cpp (fun _ ->
   !! Instr.read_last_write [cVarDef "y"; cRead ~addr:[cVar "x" ] ()];
    (* TODO should work: !! Instr.read_last_write [cVarDef "y"; sExpr "x"]; *)
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
    !! Instr.read_last_write [sInstr "= t[0]"; dRHS];

    !! Trace.alternative (fun _ ->
      !! Instr.read_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
      !! Instr.read_last_write ~write:[sInstr "t[0] ="] [sInstr "= t[0]"; dRHS];
      !!();
    );

)
