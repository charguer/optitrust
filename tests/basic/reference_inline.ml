open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.inline [cVarDef "x"];
  )
"
int main() {
  const int& x = 3;
  int y = x;
  y++;
}
"

let _ = Run.script_cpp ( fun _ ->
  (* inline at one specific occurence *)
  !! Variable_basic.inline_at [cVarDef "r1"] [cVarDef "y"];
  !! Variable_basic.inline_at [cVarDef "r3"] [cVarDef "b"];
  (* inline at all occurences and delete the reference definition *)
  !! Variable_basic.inline [cVarDef "a"];
  (* inline a reference to a matrix row *)
  (* !! Variable_basic.inline [cVarDef "v"]; *)
)

(* TODO:

  in variable_basic

  fold ?at ?deref tg => no change
  unfold ?at:target tg => does unfold, and no deletion
  inline tg => does unfold + delete the cvardef

*)

