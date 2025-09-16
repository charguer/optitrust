open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun () ->
      let q_head_per_kv_head_count =
        trm_find_var "q_head_per_kv_head_count" [ cFunDefAndDecl "generate_prompt_proc" ]
      in
      let kv_headcount = trm_find_var "kv_head_count" [ cFunDefAndDecl "generate_prompt_proc" ] in
      !!Matrix.tile ~block_size:q_head_per_kv_head_count ~nb_blocks:kv_headcount ~index_dim:0
        [ nbMulti; f; cVarDefs [ "mha_q"; "mha_score"; "mha_blend" ] ];

      !!Loop_basic.grid_enumerate
        [ ("h2", kv_headcount); ("q2", q_head_per_kv_head_count) ]
        [ nbMulti; f; cFor "q" ];
      !!Variable.inline [ nbMulti; f; cVarDef "q" ];
      !!Rewrite.equiv_at "int i; int j ; int k; ==> (i*j +k) /j == i" ~indepth:true [ f ];
      !!Rewrite.equiv_at "int i; int j ; int k; ==> (i*j +k) %j == k" ~indepth:true [ f ])
