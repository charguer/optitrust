open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;
  (* Flags.detailed_resources_in_trace := true; *)
  Flags.save_ast_for_steps := Some Steps_important

let _ = Flags.recompute_resources_between_steps := false
let chunk_len = 512
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun _ ->
      !!! () ;
)

