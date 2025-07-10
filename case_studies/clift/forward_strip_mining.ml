open Optitrust
open Prelude

let _ = Flags.pretty_matrix_notation := true
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun () ->
      (* let (q_head_per_kv_head_count,_) = find_var "q_head_per_kv_head_count" [f] in
   let (kv_headcount,_) = find_var "kv_head_count" [f] in
  (* !! Loop_basic.grid_enumerate [("h2", trm_var kv_headcount);("q2", trm_var q_head_per_kv_head_count)] [nbMulti; f; cFor "q"]; *)
  !!! ();
  !! Arrays.tile q_head_per_kv_head_count [f; cVarDef "mha_q"]; *)
      Function.inline [ nbMulti; cCall "matmul" ];
      (* Function.uninline ~f:[ cFunDef "matmul_real" ] [nbMulti; cForBody "q" ; dSeqNth 0]) *)
  )
