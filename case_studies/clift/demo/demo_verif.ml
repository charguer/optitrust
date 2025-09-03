open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_important

let _ = Flags.recompute_resources_between_steps := true
let chunk_len = 512
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun _ ->
      !!!();

      !!(Function.inline [ f; cCall "forward" ]);
      (* !!(Variable.inline [f; cVarDef "token"]); *)
      (* !!(Loop.tile ~bound:TileBoundMin (trm_int chunk_len) [ f; cFor "i" ]); *)
      !!Loop.hoist
        [ nbMulti; cFunDef "generate_prompt_proc"; cVarDefs [ "embedding"; "mha_norm"; "mha_q" ] ];
      !!Loop.fission [ f; cForBody "i"; cFor "l"; tBefore ];
      !!Loop.reorder_at ~order:[ "l"; "i" ] [ f; cForBody "l"; dSeqNth 0 ];
      )
