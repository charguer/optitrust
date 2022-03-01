(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)
open Optitrust
open Target

let _ =
  Flags.use_light_diff := false

let parser = (* TODO: activate "All" when it's working *)
  if true then Parsers.Default else Parsers.All

let _ = Run.script_cpp ~parser (fun () ->

  !!(); (* press F6 on this line, it should load the diff as a blank page *)
)
