open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_all

let _ = Flags.recompute_resources_between_steps := true
let chunk_len = 512
let f = cFunDef "generate_prompt_proc"
let _ = Run.script_cpp (fun _ -> Matrix.reorder_dims ~order:[ 1; 0; 2 ] [ f; cVarDef "mha_q" ])

(*  - génerer ghost après alloc
- voir loop_swap / embedded_on
- admitted api dans ressource_trm  *)
(* Gérer les units tests pour unit test  *)

(*  Jeter les ghosts,
delete_annots_on,
Some fun _ -> trm_fail "Can't drop to prove "  t
remove_
Dans rule match, l260 rajouter le cas des access
export standalone trace dans run task  *)

