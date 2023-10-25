open Optitrust
open Target

let _ = Run.doc_script_cpp (fun _ ->
  !! Instr.move ~dest:[tBefore; cIf()] [cVarDef "c"];
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
  !! Instr.move ~dest:[tBefore; cFor "i"] [cVarDef "x"];
  (* NOTE: Use this transformation with care, it deos not check if the
      targeted invariant is an invariant or not.
  *)
  Trace.alternative (fun _ ->
  !! Instr.move ~dest:[tBefore; cFunDef "main"] [cVarDef "x"];
  !! ()
  );
)
