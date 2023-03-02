open Optitrust
open Target
open Ast

(* Reproducing a TVM schedule from:
   https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html *)

(* FIXME:
   1. seems weird
   2. also triggers on writes? *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let cArrayWrite (x : var) : constr =
  cWrite ~lhs:[cCellAccess ~base:[cVar x] ()] ()

let cPlusEqVar (name : string) : constr =
  cPrimFun ~args:[[cVar name]; [cTrue]] (Prim_compound_assgn_op Binop_add)
    
let foreach (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  bigstep "apply blocking to the computation of C to improve data locality";
  !! foreach [("i", 32); ("j", 32); ("k", 4)] (fun (index_to_split, size) ->
    Loop.tile (trm_int size) ~index:("b" ^ index_to_split)
      ~bound:TileDivides [cFor index_to_split]);
  (* add divides hypothesis to mm function
     List.forall (divides 32) [n; m; p]
      *)
  (* check hypothesis that loop iterations are disjoint *)
  !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cFor "bi"];
  !! Loop.hoist ~nb_loops:2 [cVarDef "sum"];
  !! Loop.fission_all_instrs ~nb_loops:2 [cFor "i"];
  (* check pfor { for { } } *)
  !! Loop.reorder ~order:["bk"; "i"; "k"; "j"] [cFor ~body:[cPlusEqVar "sum"] "i"];

  (*

  Polyhedral?

  for (int bi = 0; bi < m; bi += 32) {
    for (int bj = 0; bj < n; bj += 32) {
      for (int bk = 0; bk < p; bk += 4) {
        for (int i = 0; i < 32; i++) {
          for (int k = 0; k < 4; k++) {
            for (int j = 0; j < 32; j++) {
              ...
            }
          }
        }
      }
    }
  }
  *)

  bigstep "preload B with a different memory layout to improve access patterns";
  !!! Variable_basic.bind "pB" [cArrayRead "B"];
  !! Loop.hoist_alloc [0; 1; 1; 0; 1; 1] [cVarDef "pB"];
  !! Loop.hoist_instr [0; 1; 1; 0; 1; 1] [cArrayWrite "pB"];

  bigstep "unroll loops and introduce parallelism";
  !! Rewrite.equiv_at ~ctx:true "int d1, d2, i1, i2; ==> MINDEX2(d1, d2, i1, i2) == (i2 + i1 * d2)" [nbMulti; cMindex ~d:2 ()];
  !! Rewrite.equiv_at ~ctx:true "int d1, d2, d3, d4, i1, i2, i3, i4; ==> MINDEX4(d1, d2, d3, d4, i1, i2, i3, i4) == (i4 + i3 * d4 + i2 * d3 * d4 + i1 * d2 * d3 * d4)" [nbMulti; cMindex ~d:4 ()];
  !! Loop.unroll [cFor ~body:[cPlusEqVar "sum"] "k"];
  !! Omp.simd [nbMulti; cFor "j"];

  (* ghost -> loop invariant *)
  (* !! Loop.tile_writes  [cFor "bj"]; *)

(* tag as parallel logic + semantic *)
  (* !! Loop.semantic_parallel *)
  !! Omp.parallel_for [nbMulti; cFunDef "mm"; dBody; cStrict; cFor ""];

  (*
    TODO:
     - allow unrolling without requiring shift?
     - allow SIMD before unroll
     - avoid requiring an explicit reparse to get types for shift_to_zero
     - convenient combination of loop hoist / move out depending on static analysis
     - define 'Loop.multi_tile' to replace foreach?
       Loop.multi_tile (trm_int size) ~index:"b${index}" ~bound:TileDivides
              [("i", 32); ("j", 32); ("k", 4)] [cLabel "C"; cFor index_to_split]
     - allocate one 'sum' accumulator per thread?
     - can use ~inline:["../../include/optitrust.h"] to inline MINDEX ops, but it does not work yet 
       + note the inline that we use in pic_demo does not work:
         !! Function.inline [nbMulti; cMindex ()];
         --using occFirst does not help either.
         and even if it did work it would be very inefficient
       + we could have a function Matrix.elim_mops, symmetrix to Matrix.intro_mops,
         that would generate the formulae for the accesses directly;
     - !! Arith.(simpl_rec gather_rec) [cFunDef "mm"];
       FIXME: (p+3)/4*4 != p+3   

    NOTES:
    - the loop nest is not 'perfect' enough for Halide/TVM-like reorder:
      - 'blocking'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
      - 'loop-perm'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];

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
)