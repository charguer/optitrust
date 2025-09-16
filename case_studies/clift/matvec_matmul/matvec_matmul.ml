open Optitrust
open Prelude

let _ =
  Flags.pretty_matrix_notation := true;
  Flags.print_optitrust_syntax := true

let f = cFunDef "iter_matvec"

let _ =
  Run.script_cpp (fun () ->
      !!Function.inline [ cCall "matvec" ];
      !!Matrix.simpl_access_of_access ~indepth:true [ f ];

      !!Matrix.simpl_index_add [ nbMulti; f; cCellAccess ~base:[ cVar "x" ] (); cBinop Binop_add ];
      !!Matrix.simpl_index_add [ nbMulti; f; cCellAccess ~base:[ cVar "y" ] (); cBinop Binop_add ];

      !!Rewrite.equiv_at "int j; ==> 0 + j == j" [ nbMulti; cFunDef " iter_matvec" ] ~indepth:true;
      !!Function.uninline ~f:[ cFunDef "matmul" ] [ cFunBody "iter_matvec"; dSeqNth 0 ]
    )
