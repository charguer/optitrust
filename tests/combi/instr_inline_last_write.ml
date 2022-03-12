open Optitrust
open Target

(* LATER: when the implementation of this tactic gets fixed, remove ~write:...
   from the doc script. *)

let _ = Run.doc_script_cpp (fun _ ->
    !! Instr.inline_last_write [cReadVar "x"];
  )
"
int main() {
  int x = 0;
  x = 1;
  int y = x;
}
"

let _ = Run.script_cpp (fun _->

    !! Instr.inline_last_write [cReadVar "x"];
)
