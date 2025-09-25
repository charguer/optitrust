open Optitrust
open Prelude


let _ =

  (* Flags.detailed_resources_in_trace := true; *)
  Flags.save_ast_for_steps := Some Steps_important

let f = cFunDef "generate_prompt_proc"
let chunk_len = 512

let _ =
  Run.script_cpp (fun _ ->
    !!();
      let q_head_per_kv_head_count = trm_find_var "q_head_per_kv_head_count" [f ] in
      let kv_head_count = trm_find_var "kv_head_count" [ f ] in
      !!Matrix.tile ~block_size:q_head_per_kv_head_count ~nb_blocks:kv_head_count ~index_dim:0
        [ nbMulti; f; cVarDefs [ "mha_q";] ];
      !!Loop_basic.grid_enumerate [ ("h2", kv_head_count); ("q2", q_head_per_kv_head_count) ] [ nbMulti; f; cFor "q" ])
