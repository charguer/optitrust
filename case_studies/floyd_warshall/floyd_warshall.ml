open Optitrust
open Target
open Syntax

(* let _ = Flags.check_validity := true *)
let _ = Flags.pretty_matrix_notation := true

let _ = Run.script_cpp (fun () ->
  !! Variable.insert_and_fold ~typ:(ty "uint32_t") ~name:"sum" ~value:(expr "A[i * N + k] + A[k * N + j]") [tBefore; cIf ()];

  bigstep "contiguous allocation of accessed memory";
  (* Store the values in the kth-row needed to compute distances in a contiguous manner *)
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i" ] "kj" ~indep:["i"] [occIndex 1; cArrayRead "A"];
  (* Update them properly when i == k *)
  !! Sequence.insert (stmt "if (i == k) { kj[j] = sum;}") [tAfter; cArrayWrite "A"];

  (* Do the same for the k-th column *)
  !!! Loop.hoist_expr ~dest:[occIndex 0; tAfter; cFor "j"] "ik" ~indep:["j"] [cFor "i"; occIndex 0; cArrayRead "A"];
  !! Sequence.insert (stmt "if (j == k) { ik[i] = sum;}") [tAfter; cArrayWrite "A"];
  !!! ();

  (*!! Types.replace "auto*" "uint32_t*" [cVarDef "ik"];*)
  (*!! Loop_basic.move_out [cVarDef "kj"];*)

  bigstep "split to top and bottom";
  (* Split ranges, such that the ifs can be removed *)
  !! Loop.split_range ~cut:(expr "k") [occIndex 1; cFor "i"];
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "i == k"] ()];

  !! Loop.split_range ~cut:(expr "k+1") [occIndex 2; cFor "i"];

  bigstep "optimize loop k=i";
  !! Sequence.intro ~mark:"k=i-loop" ~start:[tBefore; occIndex 2; cFor "i"] ~stop:[tAfter; occIndex 2; cFor "i"] ();
  !! Loop.isolate_first_iteration [cMark "k=i-loop"; cFor "i"]; (*cTarget*)
  !! Arith.(simpl_rec gather_rec) [];
  !! Sequence.delete [cMark "k=i-loop"; cFor "i"];
  !! Instr.move ~dest:[tBefore; cMark "k=i-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "k=i-loop"; cArrayWrite "kj"];
  !! Sequence.delete [cMark "k=i-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence.elim [cMark "k=i-loop"];
  !!! ();

  bigstep "split top to left and right";
  !! Loop.split_range ~cut:(expr "k") [occIndex 1; cFor "j"];
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];
  !! Loop.split_range ~cut:(expr "k+1") [occIndex 2; cFor "j"];

  !! Loop.fission_all_instrs ~nest_of:1 [occIndex 1; cFor "i"];

  bigstep "optimize loop j=k on top";
  !! Sequence.intro ~mark:"j=k-loop" ~start:[tBefore; occIndex 2; cFor "j"] ~stop:[tAfter; occIndex 2; cFor "j"] ();
  !! Loop.isolate_first_iteration [cMark "j=k-loop"; cFor "j"];
  !! Sequence.delete [cMark "j=k-loop"; cFor "j"];
  !! Arith.(simpl_rec gather_rec) [];
  !! Instr.move ~dest:[tBefore; cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "j=k-loop"; cArrayWrite "ik"];
  !! Sequence.delete [cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence.elim [cMark "j=k-loop"];
  !!! ();

  bigstep "delete unnecessary ifs";
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "i == k"] ()];

  bigstep "split bottom to left and right";
  !! Loop.split_range ~cut:(expr "k") [occIndex 4; cFor "j"];
  !! Loop.split_range ~cut:(expr "k+1") [occIndex 5; cFor "j"];
  !! Loop.fission_all_instrs ~nest_of:1 [occIndex 4; cFor "i"];

  !! Sequence.delete [occIndex 1; cIf ~cond:[sExpr "j == k"] ()];

  bigstep "optimize loop j=k on bottom";
  !! Sequence.intro ~mark:"j=k-loop" ~start:[tBefore; occIndex 5; cFor "j"] ~stop:[tAfter; occIndex 5; cFor "j"] ();
  !! Loop.isolate_first_iteration [cMark "j=k-loop"; cFor "j"];
  !! Sequence.delete [cMark "j=k-loop"; cFor "j"];
  !! Arith.(simpl_rec gather_rec) [];
  !! Instr.move ~dest:[tBefore; cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()] [cMark "j=k-loop"; cArrayWrite "ik"];
  !! Sequence.delete [cMark "j=k-loop"; cIf ~cond:[sExpr "k == k"] ()];
  !! Sequence.elim [cMark "j=k-loop"];

  bigstep "delete unnecessary ifs";
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];
  !! Sequence.delete [occIndex 0; cIf ~cond:[sExpr "j == k"] ()];



  (*!! Loop.move_out [occIndex 1; cFor "i"; cFor "j"];*)
  (*current_ast_at_path*)
  )
