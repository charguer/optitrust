open Optitrust
open Target

(* LATER: when the implementation of this tactic gets fixed, remove ~write:...
   from the doc script. *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Instr.inline_last_write ~write:[sInstr "x = 1"] [cVarDef "y"; sExpr "x"];
  )
"
int main() {
  int x = 0;
  x = 1;
  int y = x;
}
"

let _ = Run.script_cpp (fun _->

    !! Instr.inline_last_write ~write:[cWrite ~rhs:[cInt 7] ()] [cRead ~addr:[cVar "x" ] ()];
)
