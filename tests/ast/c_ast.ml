(* This test is just to see if the AST is printed back as expected.
   The c_ast_out_enc.cpp file shows the encoded version. *)
open Optitrust
open Target

let _ =
  Flags.use_light_diff := false

let _ = Run.script_cpp ~parser:Parsers.Clang (fun () ->

  !! Trace.reparse ~parser:Parsers.Clang ();

  !! Trace.reparse ~parser:Parsers.Menhir (); (* F6 on this line shows the difference between Clang and Menhir *)

  !! Trace.alternative (fun () ->
    !! Trace.reparse ~parser:Parsers.All (); (* F6 on this line checks for discrepencies *)
  );

)
