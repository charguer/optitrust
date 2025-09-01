open Optitrust
open Prelude

let _ =
  Flags.check_validity := true;
  Flags.detailed_resources_in_trace := true;
  Flags.save_ast_for_steps := Some Steps_all


let chunk_len = 512
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun _ ->

    !!! ();

      !!(Function.inline [ f; cCall "forward" ]);
      !!(Loop.tile ~bound:TileBoundMin (trm_int chunk_len) [ f; cFor "i" ]);
       !!Loop.hoist
        [
          nbMulti;
          cFunDef "generate_prompt_proc";
          cVarDefs
            [
              "embedding";
              "mha_norm";

            ];
        ];
      !!Loop.fission [ f; cForBody "i"; tBetweenAll ];
      !!Loop.reorder_at ~order:[ "l"; "i" ] [ f; cForBody "l"; dSeqNth 0 ])

