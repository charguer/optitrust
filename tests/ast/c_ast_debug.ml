(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)

open Optitrust
open Target

(* LATER: Fix the issue with loops of the form for(k = 0; k < 10; k++) *)

let _ = Run.script_cpp (fun () -> 

  show [cFor ""];
)

