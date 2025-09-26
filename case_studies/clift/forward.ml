open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true

let f = cFunDef "generate_prompt_proc"


let chunk_len = 512

let _ =
  Run.script_cpp (fun () ->
      !!(Function.inline [ f; cCall "forward" ]);
      !!(Loop.tile ~bound:TileBoundMin (trm_int chunk_len) [ f; cFor "i" ]);
      !!Loop.hoist
        [nbMulti;cFunDef "generate_prompt_proc";
          cVarDefs [ "embedding"; "mha_norm"; "mha_q"; "mha_score"; "mha_att";
          "mha_blend"; "mha_out"; "ffn_norm"; "ffn_up"; "ffn_fc"; "ffn_out" ]; ];
      (* !!Loop.fission [ f; cForBody "i"; tBetweenAll ];
      !!Loop.reorder_at ~order:[ "l"; "i" ] [ f; cForBody "l"; dSeqNth 0 ];
      !!Loop.fission [ f; cForBody "l"; cForBody "i"; tBetweenAll ];
      !!Loop.reorder_at ~order:[ "q"; "i" ] [ nbMulti; f; cForBody "q"; dSeqNth 0 ];
      !!Loop.reorder_at ~order:[ "h"; "i" ] [ nbMulti; f; cForBody "h"; dSeqNth 0 ];
      !!Matrix.reorder_dims ~order:[ 1; 0; 2 ] [ nbMulti; f; cVarDefs [ "mha_q"; "mha_score"; "mha_blend" ] ];
      let q_head_per_kv_head_count = trm_find_var "q_head_per_kv_head_count" [ cFunDefAndDecl "generate_prompt_proc" ] in
      let kv_headcount = trm_find_var "kv_head_count" [ cFunDefAndDecl "generate_prompt_proc" ] in
      !!Matrix.tile ~block_size:q_head_per_kv_head_count ~nb_blocks:kv_headcount ~index_dim:0
        [ nbMulti; f; cVarDefs [ "mha_q"; "mha_score"; "mha_blend" ] ];
      !!Loop_basic.grid_enumerate [ ("h2", kv_headcount); ("q2", q_head_per_kv_head_count) ] [ nbMulti; f; cFor "q" ];
      !!Variable.inline [ nbMulti; f; cVarDef "q" ];
      !!Rewrite.equiv_at "int i; int j ; int k; ==> (i*j +k) /j == i" ~indepth:true [ f ];
      !!Rewrite.equiv_at "int i; int j ; int k; ==> (i*j +k) %j == k" ~indepth:true [ f ];
      !!Function.inline [ nbMulti; f; cCall "matvec" ];
      !!Function.uninline
        ~f:[ cFunDef "matmul" ]
        [ occIndices [ 0; 3; 4; 5 ]; f; cFor "i" ~body:[ cSeq ~instrs_pred:(target_list_one_st [ cFor "j" ]) () ] ] *)
    )
