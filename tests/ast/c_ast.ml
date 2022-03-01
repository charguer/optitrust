(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)
open Optitrust
open Target

let _ =
  Flags.use_light_diff := false

let _ = Run.script_cpp  (fun () ->

  !!(); (* press F6 on this line, it should load the diff as a blank page *)
)
