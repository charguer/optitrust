open Optitrust
open Target
open Ast

(* TODO: we should be more consistent with ~addr: and ~base:  => perhpas addr is better? *)
(* TODO: should index be optional in cCellRead and cCellReadOrWrite, etc.. ? *)
(* TODO: in the example below, i'm missing [cReadOrWrite ~addr:tg ()] where tg is an optional constraint on the base *)
(*
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
*)

let _ = Run.script_cpp (fun _ ->

  !! Accesses_basic.scale (trm_double 5.0) [cOr [[cCellWrite ~base:[cVar "t"] ~index:[cVar "i"] ()];[cCellRead ~base:[cVar "t"] ~index:[cVar "i"] ()]]]
)
