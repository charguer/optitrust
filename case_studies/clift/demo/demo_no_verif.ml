open Optitrust
open Prelude


let _ =
  Flags.check_validity := false;
  Flags.detailed_resources_in_trace := false

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
              "mha_q";
            ];
        ];
      !!Loop.fission [ f; cForBody "i"; cFor "l" ; tBefore];


      !!Loop.reorder_at ~order:[ "l"; "i" ] [ f; cForBody "l"; dSeqNth 0 ];
      !!Loop.fission [ f; cFor"l"; cForBody "i"; tBetweenAll];
       !!Loop.reorder_at ~order:[ "q"; "i" ] [ nbMulti; f; cForBody "q"; dSeqNth 0 ];
      )

