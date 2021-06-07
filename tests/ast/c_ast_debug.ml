
(* This is a special test, that performs no transformation but simply dumps the "undecoded" AST.
   See the comments in ast_encoding.cpp.
   When executing this test, make sure to also read the ast_encoding.ast output file *)

(* Usage: make ast_encoding.out,  then read stdout or open ast_encoding_out.cpp *)
open Optitrust open Run

(*
let _= run (fun () ->
   show_target [cMulti; ];
   dump();
   )
*)

let _= run_unit_test (*~ast_decode:false*) (fun () ->
   ())
