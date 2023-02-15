open Optitrust
open Target
open Ast

(* FIXME: seems weird *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let foreach (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  bigstep "---- compute 'bt' (transposed b) first ----";

  !! Variable_basic.bind "bt" [cFunDef "mm"; cArrayRead "b"];

  (* TODO: 6 following steps could be done by a single hoisting that would also hoist
           the computation of the init value, not just the allocated storage. 

    Loop.hoist_with_init [0; 1; 1] [cFunDef "mm"; cVarDef "bt"];

    0 = move_out both storage and init
    1 = hoist storage and use move + fission to hoist init
    *)
  !! Loop.hoist ~nb_loops:2 [cFunDef "mm"; cVarDef "bt"];
  !! Loop.move_out [cFunDef "mm"; cVarDef "bt"];
  !! Loop.fission_all_instrs [cFunDef "mm"; cFor "k"];
  !! Instr.move ~dest:[tBefore; cVarDef "sum"]
       [cFunDef "mm"; cFor ~body:[sInstrRegexp "bt.* ="] "k"];
  !! Loop.fission [tBefore; cVarDef "sum"];
  !! Loop.move_out [cFunDef "mm"; cFor ~body:[sInstrRegexp "bt.* ="] "j"];

  !! Sequence.intro_on_instr ~label:"bt_loops" [cFunDef "mm"; occFirst; cFor "j"];
  !! Sequence.intro_on_instr ~label:"sum_loops" [cFunDef "mm"; occFirst; cFor "i"];
  let bt_loops = [cFunDef "mm"; cLabel "bt_loops"] in
  let sum_loops = [cFunDef "mm"; cLabel "sum_loops"] in

  bigstep "---- split i/j/k loops ----";

  !! Loop.tile (trm_int 32) ~index:"bj" ~bound:TileBoundDivides
       (bt_loops @ [cFor "j"]);

  !! foreach [("i", 32); ("j", 32); ("k", 4)] (fun (index_to_split, size) ->
    Loop.tile (trm_int size) ~index:("b" ^ index_to_split)
      ~bound:TileBoundDivides (sum_loops @ [cFor index_to_split])
  );

  bigstep "---- hoist sum out of i/j loops ----";

  !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] (sum_loops @ [cFor "bi"]);

  !! Loop.hoist ~nb_loops:2 (sum_loops @ [cVarDef "sum"]);

  bigstep "---- reorder bt and sum loops ----";

  !! Loop.reorder ~order:["bj"; "k"; "j"] (bt_loops @ [cFor "bj"]);
  !! Loop.fission_all_instrs ~nb_loops:2 (sum_loops @ [cFor "i"]);
  !! Loop.reorder ~order:["bk"; "i"; "k"; "j"]
       (sum_loops @ [cFor ~body:[sExpr"+="] "i"]);

  bigstep "---- unroll sum loops on blocks of k ----";

  !! Loop.unroll (sum_loops @ [cFor ~body:[sExpr"+="] "k"]);

  bigstep "---- introduce vector and thread parallelism ----";

  !! Omp.simd [nbMulti; cFunDef "mm"; cFor "j"];

  !! Omp.parallel_for [cOr [
    bt_loops @ [cFor "bj"];
    sum_loops @ [cFor "bi"]
  ]];

  !! Sequence.elim [cOr [sum_loops; bt_loops]];

  (*
    TODO:
     - allow unrolling without requiring shift? 
     - allow SIMD before unroll 
     - avoid requiring an explicit reparse to get types for shift_to_zero
     - convenient combination of loop hoist / move out depending on static analysis
     - allow using marks for 'sum_loops' and 'bt_loops'
    NOTES:
    - the loop nest is not 'perfect' enough for Halide/TVM-like reorder:
      - 'blocking'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
      - 'loop-perm'
      !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];
    *)
)