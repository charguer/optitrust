(* A test for the parsing of C++ features *)

open Optitrust
open Target

let _ = Flags.set_dump_clang_ast()

let _ = Flags.dump_ast_details := true

let _ = Flags.use_light_diff := false

let _ = Run.script_cpp (fun () ->
  (* Trace.reparse(); *)
  (* !! Function.inline [cFun "f1"]; *)
  ( )
)