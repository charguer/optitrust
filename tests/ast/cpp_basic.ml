(* This tests checks the parsing of basic C++ features. *)
open Optitrust
open Syntax

let _ = Flags.use_light_diff := false

let _ = Run.script_cpp (fun () ->
            (* Try to insert some code into the AST to see whether the source
               was correctly parsed and translated back to C++. *)
            !! Sequence_basic.insert (stmt "int c = 0;")
              [occFirst; tBefore; cVarDef "vecOfInt"];
          )
