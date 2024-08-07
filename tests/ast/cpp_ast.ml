(* A test for the parsing of C++ features *)

open Optitrust
open Prelude

let _ = Flags.use_light_diff := false

let _ = Run.script_cpp (fun () ->
  !! Sequence_basic.insert (stmt "int c = 0;") [tBefore; cVarDef "a"];
  !! Sequence_basic.insert (stmt "int h(int x) {return x;}") [tBefore; cFunDef "main"];
  !!! (); (* TODO: Find how to eliminate this reparse *)
  (* TODO: transformations that insert C++ (but not C) code *)

)
