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

  (* Part: optimization and inlining of [matrix_vect_mul] *)
    let ctx = cTopFunDef "matrix_vect_mul" in
  !^ Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission [nbMulti; tAfter; ctx; cFor "k"; sInstrRegexp "res\\.[x-y]"];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp "res.*\\[0\\]"];
  !! Function.inline [cFun "matrix_vect_mul"];

  (* Part: vectorization in [cornerInterpolationCoeff] *)
    let ctxf = cTopFunDef "cornerInterpolationCoeff" in
     let ctx = cChain [ctxf; sInstr "r.v"] in
  !^ Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti; ctx; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; ctxf; cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array ~pattern_aux_vars:"double rX, rY, rZ;"
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ;"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ);"
      [nbMulti; ctx; dRHS];
  !! Loop.fold_instrs ~index:"k" [ctx];

  (* Part: update particles in-place instead of in a local variable *)
  !^ Variable.reuse ~space:(expr "p->speed") [main; cVarDef "speed2" ];
  !! Variable.reuse ~space:(expr "p->pos") [main; cVarDef "pos2"];

  (* Part: reveal write operations involved manipulation of particles and vectors *)
  !^ Trace.reparse();
  let ctx = cOr [[cFunDef "bag_push_serial"]; [cFunDef "bag_push_concurrent"]] in
  !! List.iter (fun typ -> Struct.set_explicit [nbMulti; ctx; cWrite ~typ ()]) ["particle"; "vect"];
  !! Function.inline [main; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"vect" ()];

  (* Part: inlining of [cornerInterpolationCoeff] and [accumulateChargeAtCorners] *)
  !^ Function.inline [cOr [
       [cFun "vect8_mul"];
       [cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
       [cFun "accumulateChargeAtCorners"]]];
  !! Function.inline ~vars:(AddSuffix "2") [cFun "idCellOfPos"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "cornerInterpolationCoeff"];
  !! Variable.elim_redundant [nbMulti; cVarDef ~regexp:true "\\(coef\\|sign\\|i\\).1"];
  !! Instr.move_out ~dest:[tBefore; main] [nbMulti; main; cVarDef ~regexp:true "\\(coef\\|sign\\).0"];

  (* Part: optimization of charge accumulation *)
  !^ Sequence.intro ~mark:"fuse" ~start:[main; cVarDef "coeffs2"] ();
     Loop.fusion_targets [cMark "fuse"];
  !! Trace.reparse();
  !! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="]
       [main; sInstr "deltaChargeOnCorners.v[k] ="; sExpr "coeffs2.v[k]"];
  !! Instr.inline_last_write ~write:[sInstr "deltaChargeOnCorners.v[k] ="]
       [main; sInstr "nextCharge[indices"; sExpr "deltaChargeOnCorners.v[k]"];

  (* Part: AOS-to-SOA *)
  !^ Variable.inline [main; cVarDef "p"];
  !! Variable.simpl_deref [main];
  !^ Struct.set_explicit [main; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; main; sInstr "p2."];
  !! Trace.reparse();
  !! List.iter (fun f -> Struct.inline f [cTypDef "particle"]) ["speed"; "pos"];

  (* Part: prepare the stage for scaling (move definitions and introduce constants) *)
  !^ Instr.move ~dest:[tBefore; main] [nbMulti; cFunDef ~regexp:true "bag_push.*"];
  !! Struct.to_variables [main; cVarDef "fieldAtPos"];
  !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factorC", "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), ("factorC / cell" ^ d))))
     [tBefore; cVarDef "nbSteps"];

  (* Part: scaling of electric field *)
  !^ iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]);
  !! Variable.inline [nbMulti; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl expand) [nbMulti; main; cVarDef "accel"; cStructInit; cStrict; Arith.constr];

  (* Part: scaling of speed and positions *)
  !^ iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("stepDuration / cell" ^ d))
      [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1. / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  !! Trace.reparse();
  !! Variable.inline [cVarDef "accel"];
  !! Trace.reparse();
  !! Arith.(simpl expand) [nbMulti; main; cFun "int_of_double"; dArg 0];
     Arith.(simpl expand) [nbMulti; main; cVarDef ~regexp:true "r.[01]"; dInit];
     Sequence.apply ~start:[tAfter; main; cWrite ~lhs:[cVar "fieldAtPosZ"]()]
      ~stop:[tAfter; main; cVarDef "coeffs2"] (fun m ->
       Arith.(simpl expand) [nbMulti; main; cMark m; cWrite(); dRHS; cStrictNew; Arith.constr];);

  (* Part: enumerate grid cells by coordinates *)
  !^ Label.add "core" [cFor "idCell" ~body:[cFor "i"]];
     Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [cLabelBody "core"];

  (* Part: Make positions relative *)
  !^ iter_dims (fun d ->
      Variable.bind ~const:true ("p" ^ d) [main; sInstr ("(c->items)[i].pos" ^ d ^ " ="); dRHS]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [main; cVarDef ~regexp:true "p[X-Z]"];
  !! iter_dims (fun d ->
       Accesses.shift ~neg:true ~factor:(var ("i" ^ d)) [main; cVarDef ("p" ^ d); cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d)]()]);
  !! Instr.read_last_write [nbMulti; main; cVarDef ~regexp:true "i[X-Z]2"; cRead ~addr:[sExpr "(c->items)[i].pos"]()];
  !! Instr.(gather_targets ~dest:(GatherAt [tAfter; main; cVarDef "pZ"])) [main; cVarDef ~regexp:true "i[X-Z]2"];
  !! iter_dims (fun d ->
       Accesses.shift ~neg:true ~factor:(var ("i" ^ d ^ "2")) [main; cWrite ~lhs:[sExpr ("(c->items)[i].pos" ^ d)] ()];
       Accesses.shift ~neg:true ~factor:(var ("i" ^ d ^ "2")) [main; cVarDef ~regexp:true "r[X-Z]1"; cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d)] ()];
       );
  !! Arith.(simpl expand) [nbMulti; main; cVarDef ~regexp:true "r[X-Z]1"; dInit];

  (* Part: Convert storage of relative locations to float *)
  !^ Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"];
  !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];
  (* !! Trace.reparse (); *)

  (* Part: introduce matrix operations, and prepare loop on charge deposit *)
  !^ Matrix.intro_mops (var "nbCells") [main; cVarDef "nextCharge"];
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];
  !! Variable.inline [main; cVarDef "indices"];

  (* Part: duplicate the charge of a corner for the 8 surrounding cells *)
  !^ Matrix.delocalize "nextCharge" ~into:"nextChargeCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "core"];
  !! Specialize.any "k" [nbMulti; main; cAny];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextCharge"] ~index:[] ~rhs:[cDouble 0.] ()]];

  (* Part: apply a bijection on the array storing charge to vectorize charge deposit *)
  let mybij_def =
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
  !^ Sequence.insert (stmt mybij_def) [tBefore; main];
  !! Matrix.biject "mybij" [main; cVarDef "nextChargeCorners"];
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)")
      [main; cLabel "charge"; cFun "mybij"];

  (* Part: insert thread number and thread id *)
  !^ Sequence.insert ~reparse:false (stmt "int omp_get_thread_num();") [tBefore; main];
  !! Variable.insert ~name:"nbThreads" ~typ:"int" ~value:(lit "8") [tBefore; main];
  !! Omp.get_thread_num "idThread" [tBefore; cLabel "charge"];

  (* Part: duplicate the charge of a corner for each of the threads *)
  !^ Matrix.delocalize "nextChargeCorners" ~into:"nextChargeThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "core"];
  !! Specialize.any "idThread" [nbMulti; main; cAny];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextChargeCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];

  (* Part: make the new matrices persistent across iterations *)
  !! Instr.move_out ~dest:[tBefore; main; cFor "step"] [nbMulti; main; cVarDef ~regexp:true "nextCharge."];
     Instr.move_out ~dest:[tAfter; main; cFor "step"] [nbMulti; main; cFun "MFREE"];

  (* Part: coloring *)
  !^ Variable.insert_list ~defs:[("int","block","2"); ("int","halfBlock","block / 2")] [tBefore; cVarDef "nbCells"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [main; cFor ("i" ^ d)];
    Loop.color color ~index:("ci"^d) [main; cFor bd]
    in
  !! iter_dims (fun d -> colorize "block" "block" d);
  !! Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [main; cFor "ciX"];

  (* Part: introduce atomic push operations, but only for particles moving more than one cell away *)
  !^ Variable.insert ~const:true ~typ:"coord" ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; main; cVarDef "idCell2"];
  !! Variable.bind "b2" [main; cFun "bag_push"; sExpr "&bagsNext"];
  !! Variable.insert ~const:true ~typ:"bool" ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d ->
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; main; cVarDef "b2"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") [main; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_serial" [main; cIf(); dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; cIf(); dElse; cFun "bag_push"];

  (* Part: loop splitting to separate processing of speeds, positions, and charge deposit *)
  !^ Instr.move ~dest:[tBefore; main; cVarDef "p2"] [main; cVarDef "idCell2"];
  !! Loop.hoist [main; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; main; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  !! Variable.insert ~typ:"int&" ~name:"idCell2" ~value:(expr "idCell2_step[i]") [tBefore; main; cVarDef "p2"];

  (* Part: Parallelization *)
  !^ Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

      )