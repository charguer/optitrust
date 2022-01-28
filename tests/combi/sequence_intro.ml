open Optitrust
open Target

(* LATER: we could add intro_instrs
   and for this we should have a generic mechanism (also used by fold_instrs) that:
   takes a target and resolves it to several (consecutive!) items within a same sequence,
   then return the path to the sequence, a start position and a number of items. *)


let _ = Run.doc_script_cpp (fun _ ->
  !! Sequence.intro ~on:[cVarDef "a"] ();
     Sequence.intro ~start:[cVarDef "c1"] ~nb:2 ();
  )
"
int main() {
  int a = 0;
  int b = 0;
  int c1 = 0;
  int c2 = 0;
}
"

let _ = Run.script_cpp (fun _ ->
  !! Sequence.intro ~on:[cVarDef "a"] ();
  !! Sequence.intro ~start:[tBefore; cVarDef "b"] ~stop:[tAfter; cVarDef "c"] ();
  !! Sequence.intro ~start:[cVarDef "d"] ~nb:2 ();
  !! Sequence.intro ~stop:[cVarDef "g"] ~nb:2 ();
     Trace.alternative (fun _ ->
      !! Sequence.intro ~start:[cVarDef "f"] ();
      !! Sequence.intro ~stop:[cVarDef "e"] ();
      !! (););

  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tBefore; cVarDef "z"] ());
  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tAfter; cVarDef "z"] ());
)
