open Optitrust
open Target
open Ast

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Delocalize_arith (Lit_double 0., Binop_add)

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->

  (* Part: inlining of the bag iteration *) (* skip #1 *)
  (* TODO:
    - see todo in file particle_chunk.h, next to function  bag_ho_iter_basic
    - see todo in file pic_demo.cpp on how to write the loop code, just above "while (true)"
    - see todo in file tests/basic/function_uninline and pattern_replace.ml and function_beta.ml
    - the script here should be like:
       Pattern.replace ~source:[cFunDef "bag_ho_iter_basic"] ~target:["cFunDef bag_ho_iter_chunk"]  [cVarDef "it"];
    - internally, the transformation would be equivalent to performing those steps:
        Sequence.intro ~mark:"iter" ~nb:2 [cVarDef "it"];
        Function.uninline ~fct:[cFunDef "bag_ho_iter_basic"] [cMark "iter"];
        Expr.replace_fun "bag_ho_iter_chunk" [main; cFun "bag_ho_iter_basic"];
        Function.inline [main; cFun "bag_ho_iter_chunk"];
        Function.beta [cFor "i"; cAppFun()];  // where cAppFun() = "cApp ~base:[cStrict; cFunDef]()"
        Mark.rem "iter"  // a shorthand for Mark.remove [cMark "iter"]
        // maybe also with a sequence.inline on the body

  *)


  !!! ();
  (* Part 0: Labelling the main loop*)
  !! Label.add "core" [cFor "idCell" ~body:[cWhile ()]];

  (* Part1: space reuse *)
  !! Variable.reuse ~space:(expr "p.speed") [main; cVarDef "speed2"];
     Variable.reuse ~space:(expr "p.pos") [main; cVarDef "pos2"];

  (* Part: Introducing an if-statement for slow particles *)
  !! Variable.bind_intro ~fresh_name:"b2" [main; cFun "bag_push"; sExpr "&bagsNext"];
  !! Flow.insert_if [main; cFun "bag_push"]; 
  !! Instr.replace_fun "bag_push_serial" [main; cIf(); dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; cIf(); dElse; cFun "bag_push"];
  !! Function.inline [main; cOr [[cFun "bag_push_serial"]; [cFun "bag_push_concurrent"]]];
    (* LATER: try to not inline the bag_push operations, but to modify the code inside those functions *)

  (* Part: optimization of vect_matrix_mul *)
  let ctx = cTopFunDef "vect_matrix_mul" in
  !! Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
     (* LATER: !! Loop.fission [nbMulti; tAllInBetween; ctx; cFor "k"; cSeq]; *)
  !! Loop.fission [nbMulti; tAfter; ctx; cFor "k"; cFieldWrite ~base:[cVar "res"] ~regexp:true ~field:"[^z]" ()];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp "res.*\\[0\\]"];
  !! Function.inline [cFun "vect_matrix_mul"]; (* LATER: check if it is needed *)

  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  let ctxf = cTopFunDef "cornerInterpolationCoeff" in
  let ctx = cChain [ctxf; sInstr "r.v"] in
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti; ctx; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; ctxf; cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ;" ~pattern_aux_vars:"double rX, rY, rZ;"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ);"
      [nbMulti; ctx; dRHS];
  !! Loop.fold_instrs ~index:"k" [ctx];

  (* Part: reveal fields *) 
  !! Function.inline [main; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]]; !!!();
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"particle" ()];
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"vect" ()];
  !! Variable.inline [main; cVarDef "p2"];
  !! Variable.inline [main; cVarDef "p"];
  !! Struct.to_variables [main; cVarDef "fieldAtPos"];

  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [cOr [
       [cFun "vect8_mul"];
       [cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
       [cFun "accumulateChargeAtCorners"]]];
     Function.inline ~vars:(AddSuffix "2") [cFun "idCellOfPos"];
     Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "cornerInterpolationCoeff"];
  !! Variable.elim_redundant [nbMulti; cVarDef ~regexp:true "\\(coef\\|sign\\).1"];
  !! Sequence.intro ~mark:"fuse" ~start:[main; cVarDef "coeffs2"] ();
     Loop.fusion_targets [cMark "fuse"];
     

!!! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="] [main; cRead ~addr:[sExpr "coeffs2.v"] ()]; (* The issue is coming from function inline *)
    Instr.inline_last_write ~write:[sInstr "deltaChargeOnCorners.v[k] ="] [main; cRead ~addr:[sExpr "deltaChargeOnCorners.v"] ()];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
     Struct.inline "pos" [cTypDef "particle"];

  (* Part: scaling of field, speeds and positions *)
  !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factor", "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), ("factor / cell" ^ d))))
     [tBefore; cVarDef "nbSteps"];
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation after simplifier *)
  !! Variable.inline [cVarDef "accel"];
  !!! Variable.inline [nbMulti; cVarDef ~regexp:true "factor?."];
  (* LATER: variable.inline_at which takes only the occurrence and finds automatically the source *)
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("stepDuration / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1. / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  !!! ();
  

  (* Part: simplify expressions *)
  !! Arith.simplify [nbMulti;cFieldWrite ~regexp:true ~field:"\\(speed\\|pos\\)." (); dRHS];

  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [cLabelBody "core"];

  (* Part: ARTHUR ; maybe not needed: !! iter_dims (fun d ->
    Instr.inline_last_write ~write:[cWriteVar ("fieldAtPos" ^ d)] [nbMulti; cRead ~addr:[cVar ("fieldAtPos" ^ d)] ()]); *)
  (* TODO :ARTHUR : see how to inline the zero for fieldatpos in the simplest way *)
  (* !! Variable.inline [cVarDef ~regexp:true "fieldAtPos."]; *)

  (* Part: Introduce names for new positions *)
  !! iter_dims (fun d ->
      Variable.bind_intro ~fresh_name:("p" ^ d) [cFor "i"; cStrict; cFieldWrite ~field:("pos"^d) (); dRHS]);

  !! Instr.(gather_targets ~dest:(GatherAtFirst)) [main;cVarDef ~regexp:true "\\(i.2\\|p.\\)"];

  (* Part: Make positions relative, and convert sortage to float *)
  !! iter_dims (fun d ->
      Accesses.shift ~neg:true ~factor:(var ("i" ^ d)) [cVarDef ("p" ^ d); cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d )] ()]
  );
  !! iter_dims (fun d ->
    Accesses.shift ~neg:true ~factor:(var ("i" ^ d ^ "2")) [cWrite ~lhs:[sExpr ("(c->items)[i].pos"^d)] () ]);
  !! Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"]; (* TODO: ARTHUR remove substr and try [sExprRegexp "p. - i.."]; *)
  !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];   
  !!! ();

  (* Part: introduce matrix operations, and mark a key loop *)
  !! Matrix.intro_mops (var "nbCells") [main; cVarDef "nextCharge"];
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.local_name ~my_mark:"charge" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [cLabel "charge"];
  !! Matrix_basic.delocalize (*~init_zero:true*) ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cMark "charge"];
  (* TODO: Matrix.delocalize = local_name + delocalize + remove mark + reorder if ~last:true
      : first dimension goes to last *) (* list_rotate n l = let (l1,l2) = split n l in l2 ++ l1      with n=1 *)
  !! Specialize.any "k" [main; cAny];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextCharge"] ~index:[] ~rhs:[cDouble 0.] ()]]; (* TODO: ARTHUR shorter?*)

  (* Part: bijection to group corners of a same cell nearby in memory *)
  let my_bij_code =
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int iX = coord.iX;
      int iY = coord.iY;
      int iZ = coord.iZ;
      int res[] = {
        cellOfCoord(iX, iY, iZ),
        cellOfCoord(iX, iY, wrap(gridZ,iZ-1)),
        cellOfCoord(iX, wrap(gridY,iY-1), iZ),
        cellOfCoord(iX, wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
        cellOfCoord(wrap(gridX,iX-1), iY, iZ),
        cellOfCoord(wrap(gridX,iX-1), iY, wrap(gridZ,iZ-1)),
        cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), iZ),
        cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
  !! Sequence.insert (stmt my_bij_code) [tBefore; main];
  !! Matrix.biject "mybij" [main; cVarDef "nextChargeCorners"];
  !! Variable.inline [main; cVarDef "indices"];
  show [sExpr "mybij(nbCorners, nbCells, k, indicesOfCorners(idCell2).v[k])"]; (* TODO : fix sExpr , then use this target *)
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)") [cLabel "charge"; cFun "mybij"];
    (* TODO: after ~last:true [sExpr "mybij(nbCells, nbCorners, indicesOfCorners(idCell2).val[k], k)"]; *)

    (* TODO: ARTHUR: simplify mybij calls in the sum *)


  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbThreads" ~typ:"int" ~value:(lit "8") [tBefore; main];
  !! Matrix.local_name ~my_mark:"cores" ~var:"nextChargeCorners" ~local_var:"nextChargeThreadCorners" ~indices:["idThread";"idCell"] [cLabel "charge"];
  !! Matrix_basic.delocalize ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cMark "cores"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextChargeCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];
  !! Instr.move_out ~dest:[tBefore; main; cLabel "core"] [nbMulti; main; cVarDef ~regexp:true "nextCharge."];
     Instr.move_out ~dest:[tAfter; main; cLabel "core"] [nbMulti; main; cFun "MFREE"];
  !! Omp.get_thread_num "idThread" [tBefore; cLabel "charge"];
  !! Specialize.any "idThread" [main; cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Instr.move_out ~dest:[tBefore; main] [nbMulti; main; cVarDef ~regexp:true "\\(coef\\|sign\\).0"];
  !! Loop.hoist [cVarDef "idCell2"];
  !! Loop.fission [tBefore; main; cVarDef "pX"];
  (* !! Loop.fission [tBefore; main; cVarDef "idCell2"]; *) (* TODO: Find the right place where the second split should be done *)

  (* Part: introduction of the computation *)
  !! Variable.insert_list ~defs:[("int","blockSize","2"); ("int","dist","blockSize / 2")] [tBefore; cVarDef "nbCells"];
  !! Variable.insert ~typ:"bool" ~name:"distanceToBlockLessThanHalfABlock" ~value:(trm_ands (map_dims (fun d -> expr ~vars:[d] "i${0} >= bi${0} - dist && i${0} < bi${0} + blockSize + dist"))) [tAfter; main; cVarDef "iZ2"];
  !! Specialize.any "distanceToBlockLessThanHalfABlock" [main; cFun "ANY_BOOL"];

  (* Part: Coloring *)
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor ("i" ^ d)];
    Loop.color color ~index:("ci"^d) [cFor bd]
    in
  !! iter_dims (fun d -> colorize "blockSize" "blockSize" d);
    Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [cFor "ciX"];

  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)


)
