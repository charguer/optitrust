open Optitrust
open Syntax

(* let _ = Flags.check_validity := true *)
let _ = Flags.pretty_matrix_notation := true

(*
  1. improve data locality by storing values in contiguous memory allocations.
    - note: requires inserting conditional updates for correctness, should this be done before hoisting?
    - note: is this needed for 'kj'?
  2. split loop ranges to eliminate conditional branching
  *)

let _ = Run.script_cpp (fun () ->
  bigstep "store kth values in contiguous memory allocations";
  !! Variable.insert_and_fold ~typ:(ty "uint32_t") ~name:"sum" ~value:(expr "A[i * N + k] + A[k * N + j]") [tBefore; cIf ()];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i" ] "kj" ~indep:["i"] [occIndex 1; cArrayRead "A"];
  !! Loop.hoist_expr ~dest:[tBefore; cFor "i"] "ik" ~indep:["j"] [cFor "i"; occIndex 0; cArrayRead "A"];
  !! Sequence.insert (stmt "if (i == k) { kj[j] = sum;}") [tAfter; cArrayWrite "A"];
  !! Sequence.insert (stmt "if (j == k) { ik[i] = sum;}") [tAfter; cArrayWrite "A"];
  !! Loop.hoist_alloc ~indep:["k"] ~dest:[tBefore; cFor "k"] [multi cVarDef ["kj"; "ik"]];

  bigstep "split to top and bottom";
  !! Loop.split_range ~cut:(expr "k") [occIndex 1; cFor "i"];
  !! Loop.split_range ~cut:(expr "k+1") [occIndex 2; cFor "i"];
  !!! (
    Loop.shift StartAtZero [cFor ~stop:[sExpr "k + 1"] "i"];
    Loop.unroll [cFor ~stop:[sExpr "1"] "i"]
  );

  bigstep "split top to left and right";
  !! Loop.split_range ~cut:(expr "k") [occIndex 1; cFor "j"];
  !! Loop.split_range ~cut:(expr "k+1") [occIndex 2; cFor "j"];
  !! Loop.fission_all_instrs ~nest_of:1 [cFor ~stop:[sExpr "k"] "i"];
  !!! (
    Loop.shift StartAtZero [cFor ~stop:[sExpr "k + 1"] "j"];
    Loop.unroll [cFor ~stop:[sExpr "1"] "j"]
  );

  bigstep "split bottom to left and right";
  !! Loop.split_range ~cut:(expr "k") [occIndex 4; cFor "j"];
  !! Loop.split_range ~cut:(expr "k+1") [occIndex 5; cFor "j"];
  !! Loop.fission_all_instrs ~nest_of:1 [occIndex 4; cFor "i"];
  !!! (
    Loop.shift StartAtZero [cFor ~stop:[sExpr "k + 1"] "j"];
    Loop.unroll [cFor ~stop:[sExpr "1"] "j"]
  );

  bigstep "delete unnecessary ifs";
  !!! If.elim_false [nbMulti;
   cOr [[cFor ~start:[sExpr "k + 1"] "i"];
        [cFor ~stop:[sExpr "k"] "i"]];
   cIf ~cond:[sExpr "i == k"] ()];
  !! If.elim_false [nbMulti;
   cOr [[cFor ~start:[sExpr "k + 1"] "j"];
        [cFor ~stop:[sExpr "k"] "j"]];
   cIf ~cond:[sExpr "j == k"] ()];
  !! If.elim_true [nbMulti; cIf ~cond:[sExpr "k == k"] ()];
  )
