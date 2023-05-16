(* A test for the parsing of C++ features *)

open Optitrust
open Target

let _ = Flags.use_light_diff := false

let _ = Run.script_cpp (fun () ->
  !! Sequence_basic.insert (stmt "int c = 0;") [tBefore; cVarDef "a"];
  !! Sequence_basic.insert (stmt "int h(int x) {return x;}") [tBefore; cFunDef "main"];
  (* Trace.reparse(); *) (* TODO: transformations that insert C++ (but not C) code *)

)
