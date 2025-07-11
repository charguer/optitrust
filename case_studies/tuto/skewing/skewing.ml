open Optitrust
open Prelude

let _ = Flags.check_validity := false
let _ =
  Run.script_cpp (fun _ ->


      !!Variable_basic.bind ~const:true "k"
        [
          cForBody "i";
          cWrite ~lhs:[ cVar "c" ] ();
          cBinop ~lhs:[ cVar "i" ] Binop_add;
        ];
      !!Loop.loop_single [ cVarDefReg "k" ];
      !!Loop.extend_range_array_size [ cVarInit "c" ] [ cFor "k" ];

      (* !!Marks.add "test" [ cFor "k"; cStrict; cIf () ];  *)
      !!Rewrite.equiv_at
        "const int i;const int j;const int k ==> (i + j <= k && k < i + j + 1) \
         == (j == k-i)" [ cFor "k"; cStrict; cIf (); dCond ]; (***)
      (* !!Expr_basic.replace_with_code "j == k-i" [ nbMulti; cIf (); dCond ]; *)
      !!Loop.reorder ~order:[ "k"; "i"; "j" ] [ cFor "i" ];
      !!Loop.if_loop_switch [ cFor "j" ];
      !!Loop.elim_loop_single [ cFor "j" ];
      (* !!Sequence.elim_instr [ cForBody "i"; dSeqNth 0 ]; *)
      !!Variable.inline [ cVarDef "j" ];
      !!Rewrite.equiv_at ~indepth:true "int i, k; ==> (k - i >= 0) == (i <= k)"
        [ cFor "i" ];
      !!Rewrite.equiv_at ~indepth:true "int i, k, N; ==> (k - i < N) == (i > k-N)"
        [ cFor "i" ];
      !!Loop.refactor_if_in_loop [ cFor "i" ])
