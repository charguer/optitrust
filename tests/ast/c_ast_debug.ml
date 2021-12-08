(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)

open Optitrust
open Target


let _ = Run.script_cpp (fun () -> 

  show [cFor ""];
)

