
(* This is a special test, that performs no transformation but simply dumps the "undecoded" AST.
   See the comments in ast_encoding.cpp.
   When executing this test, make sure to also read the ast_encoding.ast output file *)

open ScriptTools

let _ = run_unit_test ~ast_decode:false (fun () -> ())


(* TODO: there seems to be a few errors:
  - when vect v is a function argument, v.x should be displayed as v.x, not as &(v.x).
  *)
