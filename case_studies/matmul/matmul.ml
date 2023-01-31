open Optitrust
open Target
open Ast

(* FIXME: seems weird *)
let cArrayRead (x : var) : constr =
  cAccesses ~base:[cStrict; cCellAccess ~base:[cVar x] ()] ()

let foreach (_doc : string) (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  bigstep "---- compute 'bt' (transposed b) first ----";

  !! Variable_basic.bind "bt" [cFunDef "mm"; cArrayRead "b"];
  !! Loop.hoist_old [cFunDef "mm"; cVarDef "bt"];
  !! Loop.hoist_old [cFunDef "mm"; cVarDef "bt_step"];
  !! Loop.move_out [cFunDef "mm"; cVarDef "bt_step_step"];
  !! Instr.delete [cWriteVar "bt_step"];
  !! Variable.inline [cFunDef "mm"; cVarDef "bt_step"];
  !! Variable.inline [cFunDef "mm"; cVarDef "bt"];
  !! Variable.rename ~into:"bt" [cFunDef "mm"; cVarDef "bt_step_step"];

  !! Loop.fission_all_instrs [cFunDef "mm"; cFor "k"];
  !! Instr.move ~dest:[tBefore; cVarDef "sum"] [cFunDef "mm"; cFor ~body:[sInstrRegexp "bt.* ="] "k"];
  !! Loop.fission [tBefore; cVarDef "sum"];
  !! Loop.move_out [cFunDef "mm"; cFor ~body:[sInstrRegexp "bt.* ="] "j"];

  !! Sequence.intro_on_instr [cFunDef "mm"; dBody; dSeqNth 1];
  !! Sequence.intro_on_instr [cFunDef "mm"; dBody; dSeqNth 2];
  let bt_loops = [cFunDef "mm"; dBody; dSeqNth 1; dSeqNth 0] in
  let sum_loops = [cFunDef "mm"; dBody; dSeqNth 2; dSeqNth 0] in
  show bt_loops;
  show sum_loops;

  bigstep "---- split i/j/k loops ----";

  !! Loop_basic.tile (lit "32") ~index:"bj" ~bound:TileBoundDivides (bt_loops @ [cFor "j"]);
  show (bt_loops @ [cFor "j"]);
  (* FIXME: need explicit reparse for types *)
  !!! Loop.shift_to_zero ~inline:true (bt_loops @ [cFor "j"]);
   
  !! foreach "splits" [(32, "i"); (32, "j"); (4, "k")] (fun (size, index) ->
    Loop_basic.tile (lit (string_of_int size)) ~index:("b" ^ index)
      ~bound:TileBoundDivides (sum_loops @ [cFor index])
  );

  (* FIXME: need explicit reparse for types *)
  !!! foreach "to_shift" ["i"; "j"; "k"] (fun index ->
    Loop.shift_to_zero ~inline:true (sum_loops @ [cFor index])
  );

  bigstep "---- hoist sum out of i/j loops ----";

  (* -- Reorder Loops -- *)
  (* DISCUSS: The loop nest is not 'perfect' enough for:
  - 'blocking'
  !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
  - 'loop-perm'
  !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];
  *)
  !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] (sum_loops @ [cFor "bi"]);

  (* TODO: hoist_inline helper? *)
  (* FIXME: hoist bugged without previous shift. array size + init, *)
  (*        ~array_size:(Some (expr "32")) *)
  (* TODO:
  !! Loop.hoist ~inline:true ~nb_loops:2 [cFunDef "mm"; cVarDef "sum"];
  *)
  !! Loop.hoist_old (sum_loops @ [cVarDef "sum"]);
  !! Loop.hoist_old (sum_loops @ [cVarDef "sum_step"]);
  !! Instr.delete [cWriteVar "sum_step"];
  !! Variable.inline (sum_loops @ [cVarDef "sum_step"]);
  !! Variable.inline (sum_loops @ [cVarDef "sum"]);
  !! Variable.rename ~into:"sum" (sum_loops @ [cVarDef "sum_step_step"]);

  bigstep "---- reorder bt and sum loops ----";

  !! Loop.reorder ~order:["bj"; "k"; "j"] bt_loops;

  !! Loop.fission_all_instrs ~nb_loops:2 (sum_loops @ [cFor "i"]);

  (* TODO: cleanup reorder and move implementations *)
  !! Loop.reorder ~order:["bk"; "i"; "k"; "j"] (sum_loops @ [cFor ~body:[sExpr"+="] "i"]);

  bigstep "---- unroll sum loops on blocks of k ----";

  (* -- Loop Unroll -- *)
  (* TODO: unroll without requiring shift? *)
  (* !! Loop.shift_to_zero ~inline:true [cFunDef "mm"; cFor "k"]; *)
  !! Loop.unroll (sum_loops @ [cFor ~body:[sExpr"+="] "k"]);

  bigstep "---- introduce vector and thread parallelism ----";

  (* FIXME: does not work before unroll *)
  (* -- Vectorize -- *)
  !! Omp.header ();
  !! Omp.simd [nbMulti; cFunDef "mm"; cFor "j"];

  (* -- Multi-thread -- *)
  !! Omp.parallel_for (bt_loops @ [cFor "bj"]);
  !! Omp.parallel_for (sum_loops @ [cFor "bi"]);

  !! Sequence.elim (sum_loops |> Xlist.unlast |> fst);
  !! Sequence.elim (bt_loops |> Xlist.unlast |> fst);
)