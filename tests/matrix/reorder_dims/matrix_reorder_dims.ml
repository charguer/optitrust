open Optitrust
open Target
open Trm
let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_all

let _ = Run.script_cpp (fun _ ->

  !! Matrix.reorder_dims ~order:[2;1;0] [cVarDef "p"];
  (* !! Matrix.reorder_dims ~rotate_n:2 [cVarDef "q"]; *)

)



