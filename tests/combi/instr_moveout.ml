open Optitrust
open Target

(* TODO: I think we can rename moveout/move_out to move.
   And we can declare "move_out" as an alias for "move",
   that users can use to explain why this move is legitimate. *)

let _ = Run.doc_script_cpp (fun _ ->
  !! Instr.move_out ~dest:[tBefore; cIf()] [cVarDef "c"];
  )
"
int main() {
  if (1) {
    return 0;
  } else {
    int c = 1;
    return c;
  }
}
"

let _ = Run.script_cpp (fun _ ->

  (* !! Instr.move_out ~dest:[tBefore; cFor "j"] [cVarDef "x"]; *)
  !! Instr.move_out ~dest:[tBefore; cFor "i"] [cVarDef "x"];
  (* NOTE: Use this transformation with care, it deos not check it the
      targeted invariant is an invariant or not.
  *)
  Trace.alternative (fun _ ->
  !! Instr.move_out ~dest:[tBefore; cFunDef "main"] [cVarDef "x"];
  !! ()
  );
)
