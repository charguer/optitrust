open Optitrust
open Target
open Ast

let main = cFunDef "main"

let dims = ["X";"Y";"Z"]
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->

  (* Part: inlining of the bag iteration *) (* skip #1 *) (* ARTHUR *)

  (* Part1: space reuse *)
  !! Variable.reuse "p.speed" [cVarDef "speed2"];
     Variable.reuse ~reparse:true "p.pos" [cVarDef "pos2"]; (* LATER: avoid reparse using new parser *)

  (* Part: Introducing an if-statement for slow particles *)
  !! Variable.bind_intro ~fresh_name:"b2" [cFun "bag_push"; dArg 0];
     Flow.insert_if ~cond_ast:(trm_apps (trm_var "ANY_BOOL") []) [main; cFun "bag_push"];
     Instr.replace_fun "bag_push_serial" [main; dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; dElse; cFun "bag_push"];
     Function.inline [main; cOr [[cFun "bag_push_serial"];[cFun "bag_push_concurrent"]]];
    (* ARTHUR: try to not inline the bag_push operations, but to modify the code inside those functions *)

  (* Part: optimization of vect_matrix_mul *)
  let pre = cFunDef "vect_matrix_mul" in
  !! Function.inline  [pre; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
     Struct.set_explicit [nbMulti; pre; cWriteVar "res"];
     (* LATER: !! Loop.fission [nbMulti; tAllInBetween; pre; cFor "k"; cSeq]; *)
     Loop.fission [nbMulti; tAfter; pre; cFor "k"; cFieldWrite ~base:[cVar "res"] ~regexp:true ~field:"[^z]" ()];
     Loop.unroll [nbMulti; pre; cFor "k"];
     Instr.accumulate ~nb:8 [nbMulti; pre; sInstrRegexp "res.*\\[0\\]"];
     Function.inline [cFun "vect_matrix_mul"];

  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti; cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS; cVar ~regexp:true "r."];
     Variable.inline [nbMulti; cFunDef "cornerInterpolationCoeff";cVarDef ~regexp:true "c."];
     Variable.intro_pattern_array "double coef_x, sign_x, coef_y, sign_y, coef_z, sign_z; ==>  double rx, ry, rz; ==> (coef_x + sign_x * rx) * (coef_y + sign_y * ry) * (coef_z + sign_z * rz);" [nbMulti; cFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS]; (* TODO:  Pattern.({ vars = "double ..."; context = "..."; pattern = "... " }) *)
     Loop.fold_instrs ~index:"k" [cFunDef "cornerInterpolationCoeff"; sInstr "r.v"];

  (* Part: reveal fields *)
  !! Function.inline [main; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]]; (* TODO: again? *)
     Struct.set_explicit [nbMulti; main; cWrite ~typ:"particle" ()]; (* TODO: cOR ? *)
     Struct.set_explicit [nbMulti; main; cWrite ~typ:"vect" ()];
     Variable.inline [cOr [[cVarDef "p2"]; [cVarDef "p"]]];
  (* TODO:FIx me! *)
  !!! Struct.to_variables [cVarDef "fieldAtPos"];

  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [cOr [
       [cFun "vect8_mul"];
       [cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
       [cFun "accumulateChargeAtCorners"]; [cFun "idCellOfPos"]]];
     Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "cornerInterpolationCoeff"];
     (* LATER: try a pattern of the form: \\(coef|sign\))_.0 *)
  !! Variable.elim_redundant ~source:[nbMulti; main; cVarDef ~regexp:true ~substr:true "_.0"] [nbMulti; main; cVarDef ~regexp:true ~substr:true "_.1"];

  (* LATER: ARTHUR: look at this *)
  !! Instr.move ~dest:[tBefore; cVarDef "rx0"] [nbMulti; cVarDef ~regexp:true ~substr:true "i.0"];
     Instr.move ~dest:[tBefore; cVarDef "rx1"] [nbMulti; cVarDef ~regexp:true ~substr:true "i.1"];
     Instr.move ~dest:[tBefore; main;cVarDef "coeffs2"] [cOr [ [main;cVarDef "indices"];[cVarDef "deltaChargeOnCorners"]]];

  (* TODO ARTHUR: nbCorners vs 8 *)
  !! Loop.fusion ~nb:3 [main; cFor "k" ~body:[sInstr "coeffs2.v[k] ="]];
  (* TODO ARTHUR: see how to improve this part *)
  !!! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="] [cRead ~addr:[sExpr "coeffs2.v"] ()];
     Instr.inline_last_write ~write:[sInstr "deltaChargeOnCorners.v[k] ="] [cRead ~addr:[sExpr "deltaChargeOnCorners.v"] ()];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
     Struct.inline "pos" [cTypDef "particle"];

  (* Part: scaling of speeds and positions #7 *)
  !! Variable.insert_list ~reparse:true ~typ:"const double"
        ~defs:(("factor", "particleCharge * stepDuration * stepDuration /particleMass / cellX")
              :: map_dims (fun d -> ("factor" ^ d), ("factor / cell" ^ d)) ) [tBefore; cVarDef "nbSteps"];

  (* Part: scaling of speeds and positions *)
  !! iter_dims (fun d ->
       Accesses.scale ~factor_ast:(Ast.trm_var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]);
     iter_dims (fun d ->
       Accesses.scale ~factor_ast:(Ast.trm_var ("stepDuration / cell" ^ d)) [nbMulti;cFieldReadOrWrite ~field:("speed" ^ d) ()]);
     iter_dims (fun d ->
       Accesses.scale ~factor_ast:(Ast.trm_var ("1. / cell" ^ d)) [nbMulti;cFieldReadOrWrite ~field:("pos" ^ d) ()]);

  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d,"grid" ^ d))) [cFor "idCell" ~body:[cWhile ()]];

  (* Part: Shifting of positions*)
  !! iter_dims (fun d ->
    Instr.inline_last_write ~write:[cWriteVar ("fieldAtPos" ^ d)] [cVarDef "accel"; cRead ~addr:[cVar ("fieldAtPos" ^ d)] ()]);


  (* TODO :ARTHUR : see how to inline the zero for fieldatpos in the simplest way *)
  !! Variable.inline [cOr [[cVarDef ~regexp:true "fieldAtPos."]; [cVarDef "accel"]]];
  !! iter_dims (fun d ->
      Variable.bind_intro ~fresh_name:("p" ^ d) [cFor "i"; cStrict; cFieldWrite ~field:("pos"^d) ();  dRHS]);

  !! Instr.(gather ~dest:(GatherAt [tBefore; sInstr "= pX"])) [main;cVarDef ~regexp:true "p."];


  !! iter_dims (fun d ->
    Accesses.shift ~factor_ast:(Ast.trm_var ("i" ^  d)) [cOr [[cWrite ~lhs:[sExpr ("(c->items)[i].pos"^d)] ()]; [cVarDef ("p" ^ d); cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d )] ()]]];);

  (* Part: convert pos fields to float *)
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sExprRegexp ~substr:true "\\(p. \+ i.\\)"];

  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [main;cVarDef "nextCharge"];
     Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1;main; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" [cMark "first_local"];
     Variable.inline [main; cVarDef "indices"];
     Specialize.any "k" [cAny];
  let my_bij_code =
    "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
      coord coord = coordOfCell(idCell);
      int ix = coord.ix;
      int iy = coord.iy;
      int iz = coord.iz;
      int res[] = {
        cellOfCoord(ix, iy, iz),
        cellOfCoord(ix, iy, wrap(gridZ,iz-1)),
        cellOfCoord(ix, wrap(gridY,iy-1), iz),
        cellOfCoord(ix, wrap(gridY,iy-1), wrap(gridZ,iz-1)),
        cellOfCoord(wrap(gridX,ix-1), iy, iz),
        cellOfCoord(wrap(gridX,ix-1), iy, wrap(gridZ,iz-1)),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), iz),
        cellOfCoord(wrap(gridX,ix-1), wrap(gridY,iy-1), wrap(gridZ,iz-1)),
      };
     return MINDEX2(nbCells, nbCorners, res[idCorner], idCorner);
     }" in
     Sequence.insert (Ast.code my_bij_code) [tBefore;main];
     Matrix.biject "mybij" [occIndex 0;main; cFor "k" ; cFun "MINDEX2"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];
     Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2,k)") [cFun "mybij"];

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
     Matrix.local_name ~my_mark:"second_local" ~var:"nextChargeCorners" ~local_var:"nextChargeProCorners" ~indices:["idProc";"idCell"] [occIndex 2;main; cFor "k"];
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbProcs") ~index:"k" ~acc:"sum" [cMark "second_local"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]];
     Specialize.any "k" [cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main;cVarDef "coef_x0"] ~nb:6 ();
     Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"];
     Sequence.elim [cMark "temp_seq"];
     Loop.fission [tBefore; cVarDef "px"];
     Loop.fission [tBefore; main; cVarDef "ix"];
     Loop.hoist [cVarDef "idCell2"];


  (* Part: Coloring *)
     let sized_dims = [("ix", "gridX"); ("iy", "gridY"); ("iz", "gridZ")] in
     let dims = List.map fst sized_dims in
     let colorize (tile : string) (color : string) (d:string) : unit =
      let bd = "b" ^ d in
      Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
      Loop_basic.color color ~index:("c"^d) [cFor bd]
        in
  !! List.iter (colorize "2" "2") dims;
     Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cix"];

  (* Introduction of the computation *)

  !! Variable.insert_list ~defs:[("blockSize","2");("2","blockSize / 2")] ~typ:"int" [tBefore; cVarDef "nbCells"];
     Variable.insert ~name:"distanceToBlockLessThanHalfABlock" ~typ:"bool"  ~value:"(ix >= bix + d && ix < bix + blockSize + d)&& (iy >= biy + d && iy < biy + blockSize + d) && (iz >= biz + d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"];
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bix"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)


)

