open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;Flags.detailed_resources_in_trace := true;
Flags.save_ast_for_steps := Some Steps_all
let _  = Run.script_cpp ( fun x -> !!!());
