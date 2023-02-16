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

let foreach (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  bigstep "compute 'Bt' (transposed B) first";
  !! Variable_basic.bind "Bt" [cArrayRead "B"];
  (* TODO: 6 following steps could be done by a single hoisting that would also hoist
           the computation of the init value, not just the allocated storage. 

    Loop.hoist_with_init [0; 1; 1] [cFunDef "mm"; cVarDef "Bt"];

    0 = move_out both storage and init
    1 = hoist storage and use move + fission to hoist init
    *)
  !! Loop.hoist ~nb_loops:2 [cVarDef "Bt"];
  !! Loop.move_out [cVarDef "Bt"];
  !! Loop.fission_all_instrs [cFor "k"];
  !! Instr.move ~dest:[tBefore; cVarDef "sum"] [cFor ~body:[cArrayWrite "Bt"] "k"];
  !! Loop.fission [tBefore; cVarDef "sum"];
  !! Loop.move_out [cFor ~body:[cArrayWrite "Bt"] "j"];

  bigstep "apply blocking on Bt loops";
  !! Sequence.intro_on_instr ~label:"Bt" [cFor ~body:[cArrayWrite "Bt"] "j"];
  !! Sequence.intro_on_instr ~label:"C" [cFor ~body:[cVarDef "sum"] "i"];
  !! Loop.tile (trm_int 32) ~index:"bj" ~bound:TileDivides [cLabel "Bt"; cFor "j"];
  !! Loop.reorder ~order:["bj"; "k"; "j"] [cLabel "Bt"; cFor "bj"];

  bigstep "apply blocking on C loops";
  !! foreach [("i", 32); ("j", 32); ("k", 4)] (fun (index_to_split, size) ->
    Loop.tile (trm_int size) ~index:("b" ^ index_to_split)
      ~bound:TileDivides [cLabel "C"; cFor index_to_split]
  );
  !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cLabel "C"; cFor "bi"];
  !! Loop.hoist ~nb_loops:2 [cLabel "C"; cVarDef "sum"];
  !! Loop.fission_all_instrs ~nb_loops:2 [cLabel "C"; cFor "i"];
  !! Loop.reorder ~order:["bk"; "i"; "k"; "j"]
       [cLabel "C"; cFor ~body:[sExpr"+="] "i"];

  bigstep "unroll loops and introduce parallelism";
  !! Loop.unroll [cLabel "C"; cFor ~body:[sExpr"+="] "k"];
  !! Omp.simd [nbMulti; cFor "j"];
  !! Omp.parallel_for [cOr [
    [cLabel "Bt"; cFor "bj"];
    [cLabel "C"; cFor "bi"]
  ]];

  !! Sequence.elim [cOr [[cLabel "Bt"]; [cLabel "C"]]];

  (*
    TODO:
     - allow unrolling without requiring shift? 
     - allow SIMD before unroll 
     - avoid requiring an explicit reparse to get types for shift_to_zero
     - convenient combination of loop hoist / move out depending on static analysis
     - allow using marks for 'sum_loops' and 'bt_loops'
     - define 'Loop.multi_tile' to replace foreach?
     - replace 'sExpr"+="' constraints with labels?
     - replace 'Sequence.intro_on_instr' with a 'Loop.add_label'?
       but it adds a sequence, not just a label, otherwise could use a 'Label.add'.
    NOTES:
    - the loop nest is not 'perfect' enough for Halide/TVM-like reorder:
      - 'blocking'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
      - 'loop-perm'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];
    *)
)