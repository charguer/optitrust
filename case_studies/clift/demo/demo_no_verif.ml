open Optitrust
open Prelude

let _ =
  Flags.check_validity := false;
  Flags.detailed_resources_in_trace := false

let chunk_len = 512
let f = cFunDef "generate_prompt_proc"

let _ =
  Run.script_cpp (fun _ ->
      !!!();

      !!(Function.inline [ f; cCall "forward" ]);
      (* !!(Loop.tile ~bound:TileBoundMin (trm_int chunk_len) [ f; cFor "i" ]); *)
      !!Loop.hoist
        [ nbMulti; cFunDef "generate_prompt_proc"; cVarDefs [ "embedding"; "mha_norm"; "mha_q" ] ];
      !!Loop.fission [ f; cForBody "i"; cFor "l"; tBefore ];

      !!Loop.reorder_at ~order:[ "l"; "i" ] [ f; cForBody "l"; dSeqNth 0 ];
      !!Loop.fission [ f; cFor "l"; cForBody "i"; tBetweenAll ];
      !!Loop.reorder_at ~order:[ "q"; "i" ] [ nbMulti; f; cForBody "q"; dSeqNth 0 ];


      !!Matrix.reorder_dims ~order:[ 1; 0; 2 ] [ nbMulti; f; cVarDefs [ "mha_q" ] ];

      !!Function.inline [ f; cCall "matvec" ];
      !!Matrix.simpl_access_of_access ~indepth:true [ f ];
      !!Matrix.simpl_index_add
        [ nbMulti; f; cCellAccess ~base:[ cVar ~substr:true "mha_" ] (); cBinop Binop_add ];
      !!Rewrite.equiv_at "int j; ==> 0 + j == j" [ nbMulti; f ] ~indepth:true;
      !!Function.uninline ~f:[ cFunDef "matmul" ] [ f; cForBody "q"; dSeqNth 0 ])
