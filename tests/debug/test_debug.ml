open Optitrust
open Target
open Path

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun () ->
   (* -- Test Targets -- *)
   show [ cFunDef "mm" ];

   show [ cFor "i" ];
   (* nbAny; nbExact 1; *)
   show [ cFunDef "mm"; cFor "i" ];

   show [ cFor "j" ];
   show [ cFor "i"; cFor "j" ];
   (* DISCUSS: unexpected outcome: *)
   (* show [ cForNestedAtDepth 0 ]; *)

   show [ cFor "k" ];
   show [ cFor "i"; cFor "j"; cFor "k" ];

   (* DISCUSS: unexpected outcome: DepthAt 0 ? *)
   (* show [ cInDepth; cReadVar "a" ]; *)
   show [ cCellAccess () ];
   (* not working?: *)
   show [ cCellRead () ]; show [ cRead () ];
   show [ cCellWrite () ];

   (* -- reduction rewrites -- *)
   (* bigstep "liftReduce1";

   show [cFunDef "reduction"; cVarDef "sum"];
   !! Loop.hoist [cFunDef "reduction"; cVarDef "sum"];
   !! Variable.inline [cFunDef "reduction"; cVarDef "sum"];
   !! Variable.rename ~into:"sum" [cFunDef "reduction"; cVarDef "sum_step"];
   !! Loop.fission ~split_between:true [cFunDef "reduction"; cFor "i"];
   !! Loop.swap [cFunDef "reduction"; cFor ~body:[cFor "j"] "i"];
   (* !! Variable.inline_and_rename *)
   *)

   (* -- MM rewrite -- *)
   bigstep "matrix";

   (* -- Split Loops -- *)
   !! Loop_basic.tile (lit "32") ~index:"bi" ~bound:TileBoundDivides [cFunDef "mm"; cFor "i"];
   !! Loop_basic.tile (lit "32") ~index:"bj" ~bound:TileBoundDivides [cFunDef "mm"; cFor "j"];
   !! Loop_basic.tile (lit "4") ~index:"bk" ~bound:TileBoundDivides [cFunDef "mm"; cFor "k"];

   (* -- Reorder Loops -- *)
   (* DISCUSS: The loop nest is not 'perfect' enough for:
   - 'blocking'
   !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "j"; "k"] [cFor "bi"];
   - 'loop-perm'
   !! Loop.reorder ~order:["bi"; "bj"; "bk"; "i"; "k"; "j"] [cFor "bi"];
   *)
   !! Loop.reorder ~order:["bi"; "bj"; "i"; "j"] [cFunDef "mm"; cFor "bi"];

   (* TODO: hoist_inline helper? *)
   (* FIXME: hoist bugged? array size + init *)
   (* TODO:
   !! Loop.hoist ~inline:true ~nb_loops:2 [cFunDef "mm"; cVarDef "sum"];
   *)
   !! Loop.hoist ~array_size:(Some (expr "32")) ~inline:true [cFunDef "mm"; cVarDef "sum"];
   !! Loop.hoist ~array_size:(Some (expr "32")) ~inline:true [cFunDef "mm"; cVarDef "sum"];
   !! Instr.delete [cWriteVar "sum_step"];
   !! Variable.rename ~into:"sum" [cFunDef "mm"; cVarDef "sum_step_step"];
   !! Loop.fission ~split_between:true [cFunDef "mm"; cFor "j"];
   !! Loop.fission ~split_between:true [cFunDef "mm"; cFor "i"];
   show [cFunDef "mm"; cFor ~body:[cFor ~body:[cFor ~body:[cFor "k"] "bk"] "j"] "i"];
   show [cFunDef "mm"; cFor ~body:[sExpr"+="] "i"];
   show [cFunDef "mm"; cFor ~body:[cVar "a"] "i"];
   (* FIXME: reorder target is wrong? *)
   (* !! Loop.reorder ~order:["bk"; "i"; "k"; "j"]
    [cFunDef "mm"; cFor ~body:[cFor ~body:[cFor ~body:[cFor "k"] "bk"] "j"] "i"]; *)
   !! Loop.swap [cFunDef "mm"; cFor "i"; cFor ~body:[sExpr"+="] "j"];
   !! Loop.swap [cFunDef "mm"; cFor ~body:[sExpr"+="] "i"];
   !! Loop.swap [cFunDef "mm"; cFor ~body:[sExpr"+="] "j"];

   (* -- Vectorize -- *)
   (* OMP pragma *)
   (* pic_demo.ml *)

   (* -- Array Packing -- *)
   (* Store Temporary Array (alloc + copy loops) *)
   (* replace x := a[i] * 2 by x := t[i] *)
   (* Variable.bind? t = x * 2; hoist t *)
   (* insert new array + bind to it *)

   (* -- Loop Unroll -- *)

   (* -- Parallelize -- *)
   (* OMP pragma *)
)
