open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true

(* Reproducing a TVM schedule from:
   https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html

   1. improve data locality by blocking the computation of C and preloading B with a packed memory layout
   2. unroll loops and introduce parallelism with vectorization and multi-threading
*)

(* FIXME:
   1. seems weird
   2. also triggers on writes? *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let cArrayWrite (x : var) : constr =
  cWrite ~lhs:[cCellAccess ~base:[cVar x] ()] ()

(* TODO: but in lib *)
let cPlusEqVar (name : string) : constr =
  cPrimFun ~args:[[cVar name]; [cTrue]] (Prim_compound_assgn_op Binop_add)

let foreach (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

(*
  STATIC ANALYSIS:
    - add divides hypothesis to mm function 'List.forall (divides 32) [n; m; p]'
    - check hypothesis that loop iterations are disjoint for reorder/fission
      - pfor { for { } } --> for { pfor { } }
    - parallel for:
      - (1) add ghost instr; (2) loop invariant; (3) tag loop as parallel in logic
      - !! Loop.writes_to_tiles ~parallel:true ... [cFor "bj"];

  TODO:
    - allocate one 'sum' accumulator per thread?
      - hoist = create array + reuse space
    - '*' '/' to bitshift
    - "local name" for sum
    - memset/memcpy insertions

    - allow unrolling without requiring shift?
    - allow SIMD before unroll
    - avoid requiring an explicit reparse to get types for shift_to_zero
    - convenient loop hoist with static analysis inference
    - define 'Loop.multi_tile' to replace foreach?
      Loop.multi_tile (trm_int size) ~index:"b${index}" ~bound:TileDivides
            [("i", 32); ("j", 32); ("k", 4)] [cLabel "C"; cFor index_to_split]

  NOTES:
  - !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEqVar "sum"];
    =
    !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cFor "bi"];
    !! Loop.hoist ~nb_loops:2 [cVarDef "sum"];
    !! Loop.fission_all_instrs ~nb_loops:2 [cFor "i"];
    !! Loop.reorder ~order:["bk"; "i"; "k"; "j"] [cFor ~body:[cPlusEqVar "sum"] "i"];
  - !!! Loop.hoist_expr "pB" [0; 1; 1; 0; 1; 1] [cArrayRead "B"];
    =
    !!! Variable.bind "pB" [cArrayRead "B"];
    !! Loop.hoist_alloc [0; 1; 1; 0; 1; 1] [cVarDef "pB"];
    !! Loop.hoist_instr [0; 1; 1; 0; 1; 1] [cArrayWrite "pB"];

  PLAN 1:
    - precompute the transposed of B
    - apply blocking in the computation of the transposed of B
      + need to propagate blocking to storage layout as well
      + Matrix.tile_storage (trim_int 32) ~dim:0 [cVarDef "Bt"];
      + Matrix.reorder_storage ~order:[0; 2; 1] [cVarDef "Bt"];
    - apply blocking in the main computation of the product matrix
    - unroll loops and introduce parallelism

  PLAN 2:
    - apply blocking in the main computation of the product matrix
    - precompute the transposed of B
    - unroll loops and introduce parallelism
  *)

let (~~) f a b = f b a
(* TODO: je préfères utiliser un combinateur qui conserve le nom "List.iter" *)

(* TODO:  déplacer tous les commentaires dans un fichier auxiliaire
   TODO: déplacer les fonctions cArrayRead etc.. au bon endroit *)

(* TODO: j'ai des erreurs en output:

  tmp_rule.cpp:33:70: error: subscripted value is not an array, pointer, or vector *)


let _ = Run.script_cpp (fun () ->
  !! ~~List.iter [("i", 32); ("j", 32); ("k", 4)] (fun (loop_id, tile_size) ->
    Loop.tile (trm_int tile_size) ~index:("b" ^ loop_id)
      ~bound:TileDivides [cFor loop_id]);
  !! Loop.reorder_at ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cPlusEqVar "sum"];
  !!! Loop.hoist_expr ~dest:[tBefore; cFor "bi"] "pB" ~indep:["bi"; "i"] [cArrayRead "B"];
  !! Function.inline ~delete:true [cFun "mm"];
  (*!! Arith.(simpl compute) [];  TODO: ça serait sympa de retirer les exact div ici *)
  !!! Matrix.stack_copy ~var_from:"sum" ~var_to:"s" ~fixed_dims:1 [cFor ~body:[cPlusEqVar "sum"] "k"];
  !! Matrix.elim_mops [];
  !! Loop.unroll [cFor ~body:[cPlusEqVar "s"] "k"];
  !! Omp.simd [nbMulti; cFor ~body:[cPlusEqVar "s"] "j"];
  !! Omp.parallel_for [nbMulti; cFunDef "mm1024"; dBody; cStrict; cFor ""];
  (* TODO: on a deux fois #include "omp.h" *)
)