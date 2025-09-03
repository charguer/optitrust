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
    !!Matrix.simpl_access_of_access ~indepth:true [ f ;cCall "matvec"];
      (* !!Matrix.simpl_index_add
        [ nbMulti; f; cCellAccess ~base:[ cVar ~substr:true "mha_" ] (); cBinop Binop_add ];
      !!Rewrite.equiv_at "int j; ==> 0 + j == j" [ nbMulti; f ] ~indepth:true; *)
  );
