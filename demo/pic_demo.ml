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

  (* Part: optimization and inlining of [matrix_vect_mul] *)
  !^ let ctx = cTopFunDef "matrix_vect_mul" in
     Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission [nbMulti; tAfter; ctx; cFor "k"; sInstrRegexp "res\\.[^z]"];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp "res.*\\[0\\]"];
  !! Function.inline [cFun "matrix_vect_mul"];

  (* Part: vectorization in [cornerInterpolationCoeff] *)
  !^ let ctxf = cTopFunDef "cornerInterpolationCoeff" in
     let ctx = cChain [ctxf; sInstr "r.v"] in
     Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti; ctx; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; ctxf; cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array ~pattern_aux_vars:"double rX, rY, rZ;"
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ;"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ);"
      [nbMulti; ctx; dRHS];   (* TODO: add ?(const:bool=true) to this operation, to generate const vars *)
  !! Loop.fold_instrs ~index:"k" [ctx];

  (* Part: update particles in-place instead of in a local variable *)
  !^ Variable.reuse ~space:(expr "p.speed") [main; cVarDef "speed2" ];
     Variable.reuse ~space:(expr "p.pos") [main; cVarDef "pos2"];

  (* Part: reveal write operations involved manipulation of particles and vectors *)
  !^ Trace.reparse();
  !! let ctx = cOr [[cFunDef "bag_push_serial"]; [cFunDef "bag_push_concurrent"]] in
     List.iter (fun typ -> Struct.set_explicit [nbMulti; ctx; cWrite ~typ ()]) ["particle"; "vect"];
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

  (* Part: optimization of charge accumulation *)
  !^ Sequence.intro ~mark:"fuse" ~start:[main; cVarDef "coeffs2"] ();
     Loop.fusion_targets [cMark "fuse"];
  !! Trace.reparse(); (* required to get the parentheses right;
        TODO: using the printing system with priorities,
        we should be able to print t.v[k] instead of (t.v)[k]; *)
  !! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="]
       [main; sInstr "deltaChargeOnCorners.v[k] ="; sExpr "coeffs2.v[k]"];
  !! Instr.inline_last_write ~write:[sInstr "deltaChargeOnCorners.v[k] ="]
       [main; sInstr "nextCharge[indices"; sExpr "deltaChargeOnCorners.v[k]"];

  (* Part: AOS-to-SOA *)
  !^ Variable.inline [main; cVarDef "p"];
  !^ Struct.set_explicit [main; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; main; sInstr "p2."];
  !! Trace.reparse(); (* required to get the types right *)
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
       Accesses.scale ~factor:(var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [nbMulti; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl expand) [nbMulti; main; cVarDef "accel"; cStructInit; cStrict; Arith.constr];

  (* Part: scaling of speed and positions *)
  !^ iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("stepDuration / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1. / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  !! Trace.reparse(); (* required for the terms to be visible to the simplifier *)
  !! Variable.inline [cVarDef "accel"];
  !! Trace.reparse();
     (* required to get the types right, for [simpl_proj] to work in inlining o;
      TODO: why are the types not there? it should be sufficient for the trm_struct to have
      the right type; and this type should be known because it was available in the variable
      definition that we inlined just before. *)
  !! Arith.(simpl expand) [nbMulti; main; cFun "int_of_double"; dArg 0];
     Arith.(simpl expand) [nbMulti; main; cVarDef ~regexp:true "r.[01]"; dInit];
     Sequence.apply ~start:[tAfter; main; cWrite ~lhs:[cVar "fieldAtPosZ"]()] ~stop:[tAfter; main; cVarDef "coeffs2"] (fun m ->
       Arith.(simpl expand) [nbMulti; main; cMark m; cWrite(); dRHS; cStrictNew; Arith.constr];
       (* LATER: Function.use_infix_ops [cMark m] *));
      (* ARTHUR: also missing simplifications in bag_push_concurrent *)

  (* Part: enumerate grid cells by coordinates *)
  !^ Label.add "core" [cFor "idCell" ~body:[cWhile ()]];
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
  !^ Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"]; (* TODO: ARTHUR remove substr and try [sExprRegexp "p. - i.."]; *)
  !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];
  (* !! Trace.reparse (); *)


  (* Part: introduce matrix operations, and mark a key loop *)
  !^ Matrix.intro_mops (var "nbCells") [main; cVarDef "nextCharge"];
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];

  (* Part: duplication of corners for vectorization of change deposit *)
  !^ Matrix.delocalize "nextCharge" ~into:"nextChargeCorners" ~indices:["idCell"] ~init_zero:true ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "charge"];
  !! Variable_basic.inline [main; cVarDef "indices"];
  !! Specialize.any "k" [main; cAny];
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
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextCharge"] ~index:[] ~rhs:[cDouble 0.] ()]];
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)") [cFun "mybij"]; (* TODO: Check with Arthur *)
  (* TODO: ARTHUR: simplify mybij calls in the sum *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !^ Variable.insert ~name:"nbThreads" ~typ:"int" ~value:(lit "8") [tBefore; main];
  !! Matrix.delocalize "nextChargeCorners" ~into:"nextChargeThreadCorners" ~indices:["idThread";"idCell"] ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~last:true [cLabel "charge"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextChargeCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];
  !! Instr.move_out ~dest:[tBefore; main; cLabel "core"] [nbMulti; main; cVarDef ~regexp:true "nextCharge."];
     Instr.move_out ~dest:[tAfter; main; cLabel "core"] [nbMulti; main; cFun "MFREE"];
  !! Omp.get_thread_num "idThread" [tBefore; cLabel "charge"];
  !! Specialize.any "idThread" [main; cAny];

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !^ Instr.move_out ~dest:[tBefore; main] [nbMulti; main; cVarDef ~regexp:true "\\(coef\\|sign\\).0"];
  !! Loop.hoist [cVarDef "idCell2"];
  !! Loop.fission [tBefore; main; cVarDef "pX"];
  !! Loop.fission [tBefore; main; cVarDef "iX1"]; (* TODO: Check with Arthur *)


  (* Part: Introducing an if-statement for slow particles *)
  !^ Variable.bind "b2" [main; cFun "bag_push"; sExpr "&bagsNext"];
        (* TODO: above, ~const:true  should create not a [const bag*]  but a [bag* const] *)
  !! Flow.insert_if [main; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_serial" [main; cIf(); dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; cIf(); dElse; cFun "bag_push"];

  (* Part: introduction of the computation *)
  !^ Variable.insert_list ~defs:[("int","blockSize","2"); ("int","dist","blockSize / 2")] [tBefore; cVarDef "nbCells"];
  !! Variable.insert ~typ:"bool" ~name:"distanceToBlockLessThanHalfABlock" ~value:(trm_ands (map_dims (fun d -> expr ~vars:[d] "i${0} >= bi${0} - dist && i${0} < bi${0} + blockSize + dist"))) [tAfter; main; cVarDef "iZ2"];
  !! Specialize.any "distanceToBlockLessThanHalfABlock" [main; cFun "ANY_BOOL"];

  (* Part: Coloring *)
  !^ let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor ("i" ^ d)];
    Loop.color color ~index:("ci"^d) [cFor bd]
    in
    iter_dims (fun d -> colorize "blockSize" "blockSize" d);
  !! Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [cFor "ciX"];

  (* Part: Parallelization *)
  !^ Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)

)


(* LATER:
     !! Loop.fission [nbMulti; tAfter; ctx; cFor "k"; sInstrRegexp "res\\.[^z]"];
     could be
     !! Loop.fission [nbMulti; tAllInBetween; ctx; cFor "k"; cSeq]; *)

(* LATER:
    !! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="] ..
    we could automate better the inference of the last write performed on the same expression *)

(* LATER:
    !! Arith.(simpl expand) [nbMulti; main; cVarDef "accel"; cStructInit; cStrict; Arith.constr];
    we need an easier way to target an instruction and mark all outermost arithmetic constructors.
*)

(* LATER: variable.inline_at which takes only the occurrence and finds automatically the source *)

(* LATER:
    simplify [int x = 0; ...(no use of x)...; x = 4]  into [int x = 4; ... ]
    by defining a combi transformation that does
    detach to get [int x; x = 0; ... ; x = 4]
    then remove dead code to get [int x; ... ; x = 4]
    then move write [int x; x = 4; ...]
    then attach ot get [int x = 4; ...]
*)

(* LATER: simplify and move this example into a unit test for Sequence.Apply
          Sequence.apply ~start:[tAfter; main; cWrite ~lhs:[cVar "fieldAtPosZ"]()] ~stop:[tAfter; main; cVarDef "coeffs2"] (fun m ->
          Arith.(simpl expand) [nbMulti; main; cMark m; cWrite(); dRHS; cStrictNew; Arith.constr]);
        which is equivalent to:
          Sequence.intro ~mark:"simplify" ~start:[tAfter; main; cWrite ~lhs:[cVar "fieldAtPosZ"]()] ~stop:[tAfter; main; cVarDef "coeffs2"] ();
          Arith.(simpl expand) [nbMulti; cMark "simplify"; cWrite(); dRHS; cStrictNew; Arith.constr];
          Sequence.elim [cMark "simplify"]; *)

(* LATER: we need a revert to use_infix_ops
   maybe name the function    Function.infix_ops_intro and infix_ops_elim *)

(* LATER: cWrite has ~lhs and ~rhs, but cRead has ~addr, this is not coherent, cRead should have ~arg or cWrite should have ~addr and ~arg *)
