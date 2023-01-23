open Optitrust
open Target
open Ast

let foreach (_doc : string) (l : 'a list) (f: 'a -> unit) : unit =
  List.iter f l

let _ = Run.script_cpp (fun () ->
  (* TODO: avoid regexp? *)
  (* !! Variable_basic.bind "bt" [sExprRegexp "b\\[.*\\]"]; *)

  bigstep "---- split i/j/k loops ----";
   
  !! foreach "splits" [(32, "i"); (32, "j"); (4, "k")] (fun (size, index) ->
    Loop_basic.tile (lit (string_of_int size)) ~index:("b" ^ index) ~bound:TileBoundDivides [cFunDef "mm"; cFor index]
  );

  (* FIXME: need explicit reparse for types *)
  !!! foreach "to_shift" ["i"; "j"; "k"] (fun index ->
    Loop.shift_to_zero ~inline:true [cFunDef "mm"; cFor index]
  );

  bigstep "---- hoist sum out of i/j loops ----";

  (* -- Reorder Loops -- *)
  (* DISCUSS: The loop nest is not 'perfect' enough for:
  - 'blocking'
  !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
  - 'loop-perm'
  !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];
  *)
  !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cFunDef "mm"; cFor "bi"];

  (* TODO: hoist_inline helper? *)
  (* FIXME: hoist bugged without previous shift. array size + init, *)
  (*        ~array_size:(Some (expr "32")) *)
  (* TODO:
  !! Loop.hoist ~inline:true ~nb_loops:2 [cFunDef "mm"; cVarDef "sum"];
  *)
  !! Loop.hoist_old [cFunDef "mm"; cVarDef "sum"];
  !! Loop.hoist_old [cFunDef "mm"; cVarDef "sum_step"];
  !! Instr.delete [cWriteVar "sum_step"];
  !! Variable.inline [cFunDef "mm"; cVarDef "sum_step"];
  !! Variable.inline [cFunDef "mm"; cVarDef "sum"];
  !! Variable.rename ~into:"sum" [cFunDef "mm"; cVarDef "sum_step_step"];

  bigstep "---- reorder sum loops ----";

  !! Loop.fission_all_instrs ~nb_loops:2 [cFunDef "mm"; cFor "i"];

  (* TODO: cleanup reorder and move implementations *)
  !! Loop.reorder ~order:["bk"; "i"; "k"; "j"] [cFunDef "mm"; cFor ~body:[sExpr"+="] "i"];
  (* same as:
  !! Loop.swap [cFunDef "mm"; cFor "i"; cFor ~body:[sExpr"+="] "j"];
  !! Loop.swap [cFunDef "mm"; cFor ~body:[sExpr"+="] "i"];
  !! Loop.swap [cFunDef "mm"; cFor ~body:[sExpr"+="] "j"];
  *)

  (* TODO *)
  (* -- Array Packing -- *)
  (* Store Temporary Array (alloc + copy loops) *)
  (* replace x := a[i] * 2 by x := t[i] *)
  (* Variable.bind? t = x * 2; hoist t *)
  (* insert new array + bind to it *)

  bigstep "---- unroll sum loops on blocks of k ----";

  (* -- Loop Unroll -- *)
  (* TODO: unroll without requiring shift? *)
  (* !! Loop.shift_to_zero ~inline:true [cFunDef "mm"; cFor "k"]; *)
  !! Loop.unroll [cFunDef "mm"; cFor ~body:[sExpr"+="] "k"];

  bigstep "---- introduce vector and thread parallelism ----";

  (* FIXME: does not work before unroll *)
  (* -- Vectorize -- *)
  !! Omp.header ();
  !! Omp.simd [nbMulti; cFunDef "mm"; cFor "j"];

  (* -- Multi-thread -- *)
  !! Omp.parallel_for [cFunDef "mm"; cFor "bi"];
  (* show [cFunDef "main"]; *)
)