open Optitrust
open Prelude

let _ =
  Flags.pretty_matrix_notation := true;
  Flags.print_optitrust_syntax := true

let print_trm_on t =
  Printf.printf "YOYOYO %s  \n" (Ast_to_text.ast_to_string t);
  t

let f = cFunDef "iter_matvec"
let print_trm tg = apply_at_target_paths print_trm_on tg

let _ =
  Run.script_cpp (fun () ->
      !!Function.inline [ cCall "matvec" ];
      (* !! Matrix.simpl_access_of_access [cFunDef "iter_matvec";cArrayRead "y"]; *)
      !!Matrix.simpl_access_of_access ~indepth:true [f];
      (* !!Matrix.simpl_access_of_access
        [ nbMulti; f; cWrite () ~lhs:[ cVar "x" ]; dLHS ];
      !!Matrix.simpl_access_of_access [ nbMulti; f; cArrayRead "y"; dArg 0 ]; *)
      (* !!Matrix.simpl_index_add
        [ nbMulti; f; cCellAccess ~base:[ cVar "x" ] (); cStrict ]; *)
      !!Matrix.simpl_index_add
        [ nbMulti; f; cCellAccess ~base:[ cVar "x" ] (); cBinop Binop_add ];
      !!Matrix.simpl_index_add
        [ nbMulti; f; cCellAccess ~base:[ cVar "y" ] (); cBinop Binop_add ];

      !!Rewrite.equiv_at "int j; ==> 0 + j == j"
        [ nbMulti; cFunDef "iter_matvec" ]
        ~indepth:true;
      !!Function.uninline
        ~f:[ cFunDef "matmul" ]
        [ cFunBody "iter_matvec"; dSeqNth 0 ]
        (* Matrix.  *)
      (* !! Matrix.simpl_access_of_access [cFunDef "matmul";cVarInit "q" ]; *)
      (* !! Function.uninline ~f:[cFunDef "matmul"] [cFunDef "iter_matvec"] *))
