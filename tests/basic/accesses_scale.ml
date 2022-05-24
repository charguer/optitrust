open Optitrust
open Target
open Ast

(* Arthur: we should be more consistent with ~addr: and ~base:  => perhpas addr is better?
   Begatim: we use base for array and struct accesses and we use addr for variables *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Accesses_basic.scale (trm_double 5.0) [cReadOrWrite ~addr:[cVar "x"] ()]
  )
"
int main() {
  int x = 2;
  int y = x;
  x = y;
}
"


let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale (trm_double 5.0) [cCellReadOrWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];

)
