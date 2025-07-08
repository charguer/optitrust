open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun () ->

      !!(Function.inline [ f; cCall "forward" ]);
      !!Loop.hoist
        [
          nbMulti;
          cFunDef "generate_prompt_proc";
          cVarDefs
            [
              "embedding";
              "mha_norm";
              "mha_q";
              "mha_score";
              "mha_att";
              "mha_blend";
              "mha_out";
              "ffn_norm";
              "ffn_up";
              "ffn_fc";
              "ffn_out";
            ];
        ];
      (* !! Sequence.intro ~start:[f; cVarDef "embedding"; tBefore] ~stop:[f; cVarDef "ffn_up"; tAfter] (); *)
  !! Loop.fission [f; cForBody "i"; tBetweenAll];
  !! Loop.reorder_at ~order:["l";"i"] [f; cForBody "l"; dSeqNth 0];
  !! Loop.fission [f; cForBody "l"; cForBody "i";tBetweenAll];
  !! Loop.reorder_at ~order:["q";"i"] [nbMulti;f; cForBody "q"; dSeqNth 0];
  !! Loop.reorder_at ~order:["h";"i"] [nbMulti;f; cForBody "h"; dSeqNth 0];
  !! Matrix.reorder_dims ~order:[1;0;2] [nbMulti;f;cVarDefs ["mha_q";"mha_score";"mha_blend"]];

  (* let (q_head_per_kv,_) = find_var "q_head_per_kv_head_count" [f] in
  !! Arrays.tile q_head_per_kv [f; cVarDef "mha_q"]; *)
  )
