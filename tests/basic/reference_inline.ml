open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Variable_basic.inline [cVarDef "y"];
  )
"
int main() {
  int x = 3;
  int& y = x;
  y++;
}
"

let _ = Run.script_cpp ( fun _ ->
  (* inline at one specific occurence *)
  !! Variable_basic.inline_at [sInstr "y = 9"] [cVarDef "y"];
  !! Variable_basic.inline_at [sInstr "b = 9"] [cVarDef "b"];
  (* inline at all occurences and delete the reference definition *)
  !! Variable_basic.inline [cVarDef "a"];
  (* inline a reference to a matrix row *)
  !! Variable_basic.inline [cVarDef "v"];
)

(* TODO: we need to discuss if we want to make the ~at an optional argument, like is done in pointer_fold.ml,
   or if we should change pointer_fold, or otherwise.  *)

