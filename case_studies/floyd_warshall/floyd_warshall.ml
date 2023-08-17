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

  (*!! Types.replace "auto*" "uint32_t*" [cVarDef "ik"];*)
  (*!! Loop_basic.move_out [cVarDef "kj"];*)

  (* Split ranges, such that the ifs can be removed *)
  !! Loop_basic.split_range ~cut:(expr "k") [occIndex 1; cFor "i"];
  !! Sequence_basic.delete [occIndex 1; cIf ()];(*~cond:(expr "i == k") ()];*)

  !! Loop_basic.split_range ~cut:(expr "k+1") [occIndex 2; cFor "i"];

  !! Sequence_basic.intro ~mark:"k=i-loop" 1 [occIndex 2; cFor "i"];
  !! Loop.isolate_first_iteration [cMark "k=i-loop"; cFor "i"];
  !! Sequence_basic.delete [cMark "k=i-loop"; cFor "i"];
  (*!! Expr_basic.replace (stmt "ik[k] = sum;") [cMark "j==k-if"];*)

  (*!! Sequence_basic.elim [cMark "k=i-loop"];*)
  (*!! Loop.move_out [occIndex 1; cFor "i"; cFor "j"];*)
  )
