open Optitrust
open Target
open Path
open Ast

(*
let _ = Flags.dump_ast_details := true
let _ = Flags.debug_stringreprs := true
*)

let _ = Run.script_cpp (fun () ->
   !! Arith.(simpl_rec gather_rec) [cVarDef "p"];
)
