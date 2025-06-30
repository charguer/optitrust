open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true
let _ = Flags.disable_stringreprs := true

(* Starting from two nested for loops for i - for j, we want to tile and color
   on each of the two dimensions, to obtain the following:
   ci - cj - bi - bj - i - j. *)

let _ = Run.script_cpp (fun _ ->
  bigstep "Creation and coloration of blocks";

  (* Tile on i to obtain bi - i - j. *)
  !! Loop_basic.tile (trm_int 4) ~index:"bi" ~bound:TileDivides [cFunDef "matrix2_copy"; cFor "i"];
  (* Then, swap to obtain bi - j - i. *)
  !! Loop.swap [cFor "i"];
  (* Tile on j to obtain bi - bj - j - i. *)
  !! Loop_basic.tile (trm_int 4) ~index:"bj" ~bound:TileDivides [cFunDef "matrix2_copy"; cFor "j"];
  (* Swap again to have bi - bj - i - j. *)
  !! Loop.swap [cFor "j"];

  (* Now, start the coloring with the bi loop, and get ci - bi - bj - i - j. *)
  !! Loop_basic.color (trm_int 2) ~index:"ci" [cFor "bi"];

  (* The swap transformation does currently (June 2025) not handle the present
     case, with several ghosts alterning between pure ones and linear ones not
     written as a symmetric pair. For this reason, we perform a sequence of ghost
     and loop fissions before being able to swap to obtain ci - bj - bi - i - j. *)
  !! Ghost_pure.fission ~mark_between:"pure_fission" [cFunDef "matrix2_copy"; cFor "bj"; tAfter];
  !! Ghost_pure.fission ~mark_between:"pure_fission" [cFunDef "matrix2_copy"; cFor "bj"; tBefore];
  !! Loop_basic.fission_basic ~mark_between_loops:"bi_loop_fission" [nbMulti; cFunBody "matrix2_copy"; cMark "pure_fission"];
  (* Perform the swap. *)
  !! Loop.swap [cFor "bi" ~body: [cFor "bj"]];

  (* Now, color on bj to obtain ci - cj - bj - bi - i - j. *)
  !! Loop_basic.color (trm_int 2) ~index:"cj" [cFor "bj"];
  (* Final swap to have ci - cj - bi - bj - i - j. *)
  !! Loop.swap [cFor "bj" ~body: [cFor "bi"]];

  (* With the performed fissions, we have three bi loops, the middle one containing
     the bj - i - j loops.
     Let us change the first and third ones, which are loops containing ghost code
     only, into ghost on loop. *)
  !! Ghost.embed_loop [occFirst ; cFor "bi"];
  !! Ghost.embed_loop [occLast ; cFor "bi"];
)
