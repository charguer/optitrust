open Optitrust
open Target
open Syntax

(* let _ = Flags.check_validity := true *)
let _ = Flags.pretty_matrix_notation := true

let _ = Run.script_cpp (fun () ->
  !! Variable.insert_and_fold ~typ:(ty "uint32_t") ~name:"sum" ~value:(expr "A[i * N + k] + A[k * N + j]") [tBefore; cIf ()];

  (* Store the values in the kth-row needed to compute distances in a contiguous manner *)
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i" ] "kj" ~indep:["i"] [occIndex 1; cArrayRead "A"];
  (* Update them properly when i == k *)
  !! Variable.insert ~name:"dummy" ~value:(lit "42") [tAfter; cArrayWrite "A"];
  !! Flow_basic.insert_if ~cond:(expr "i == k") ~no_else_branch:true [cVarDef "dummy"];
  !! Expr_basic.replace (stmt "kj[j] = sum;") [cVarDef "dummy"];

  (* Do the same for the k-th column *)
  !!! Loop.hoist_expr ~dest:[occIndex 0; tAfter; cFor "j"] "ik" ~indep:["j"] [cFor "i"; occIndex 0; cArrayRead "A"];
  !! Variable.insert ~name:"dummy" ~value:(lit "42") [tAfter; cArrayWrite "A"];
  !! Flow_basic.insert_if ~cond:(expr "j == k") ~no_else_branch:true [cVarDef "dummy"];
  !! Expr_basic.replace (stmt "ik[i] = sum;") [cVarDef "dummy"];
  !!! ();

  (*!! Types.replace "auto*" "uint32_t*" [cVarDef "ik"];*)
  (*!! Loop_basic.move_out [cVarDef "kj"];*)

  (* Split ranges, such that the ifs can be removed *)
  !! Loop_basic.split_range ~cut:(expr "k") [occIndex 1; cFor "i"];
  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "i == k"] ()];

  !! Loop_basic.split_range ~cut:(expr "k+1") [occIndex 2; cFor "i"];

  !! Sequence_basic.intro ~mark:"k=i-loop" 1 [occIndex 2; cFor "i"];
  !! Loop.isolate_first_iteration [cMark "k=i-loop"; cFor "i"]; (*cTarget*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "k=i-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "k=i-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "k=i-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "k=i-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "k=i-loop"; sExpr "k + 0"]; (*multi*)
  !! Sequence_basic.delete [cMark "k=i-loop"; cFor "i"];
  !! Instr.move ~dest:[tBefore; cMark "k=i-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "k=i-loop"; cArrayWrite "kj"];
  !! Sequence_basic.delete [cMark "k=i-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence_basic.elim [cMark "k=i-loop"];
  !!! ();

  !! Loop_basic.split_range ~cut:(expr "k") [occIndex 1; cFor "j"];
  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];
  !! Loop_basic.split_range ~cut:(expr "k+1") [occIndex 2; cFor "j"];

  !! Loop.fission_all_instrs ~nest_of:1 [occIndex 1; cFor "i"];

  !! Sequence_basic.intro ~mark:"j=k-loop" 1 [occIndex 2; cFor "j"];
  !! Loop.isolate_first_iteration [cMark "j=k-loop"; cFor "j"];
  !! Sequence_basic.delete [cMark "j=k-loop"; cFor "j"];
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Instr.move ~dest:[tBefore; cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "j=k-loop"; cArrayWrite "ik"];
  !! Sequence_basic.delete [cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence_basic.elim [cMark "j=k-loop"];
  !!! ();

  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];

  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "i == k"] ()];

  !! Loop_basic.split_range ~cut:(expr "k") [occIndex 4; cFor "j"];
  !! Loop_basic.split_range ~cut:(expr "k+1") [occIndex 5; cFor "j"];
  !! Loop.fission_all_instrs ~nest_of:1 [occIndex 4; cFor "i"];

  !! Sequence_basic.delete [occIndex 1; cIf ~cond:[sExpr "j == k"] ()];

  !! Sequence_basic.intro ~mark:"j=k-loop" 1 [occIndex 5; cFor "j"];
  !! Loop.isolate_first_iteration [cMark "j=k-loop"; cFor "j"];
  !! Sequence_basic.delete [cMark "j=k-loop"; cFor "j"];
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Expr_basic.replace (expr "k") [occIndex 0; cMark "j=k-loop"; sExpr "k + 0"]; (*multi*)
  !! Instr.move ~dest:[tBefore; cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "j=k-loop"; cArrayWrite "ik"];
  !! Sequence_basic.delete [cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence_basic.elim [cMark "j=k-loop"];

  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];
  !! Sequence_basic.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];



  (*!! Loop.move_out [occIndex 1; cFor "i"; cFor "j"];*)
  (*current_ast_at_path*)
  )
