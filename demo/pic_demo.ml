open Optitrust
open Target
open Ast


let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices
let step = cFunDef "step"
let stepLF = cFunDef "stepLeapFrog"
let steps = cOr [[step];[stepLF]]
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Local_arith (Lit_double 0., Binop_add)
let delocalize_obj = Local_obj ("bag_init_initial", "bag_append", "bag_free_initial")

let doublepos = true

let _ = Run.script_cpp ~parser:Parsers.Menhir ~inline:["pic_demo.h";"bag.hc";"particle.hc";"bag_atomics.h";"bag.h-"] (fun () ->

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Trace.time "step" (fun () -> Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]]);
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission ~split_between:true [ctx; cFor "k"];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp ~substr:true "res.*\\[0\\]"];
  !! Function.inline ~delete:true [nbMulti;cFun "matrix_vect_mul"];

  bigstep "Vectorization in [cornerInterpolationCoeff]";
  let ctx = cTopFunDef "cornerInterpolationCoeff" in
  let ctx_rv = cChain [ctx; sInstr "r.v"] in
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a)" [nbMulti; ctx_rv; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; ctx; cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array ~const:true ~pattern_aux_vars:"double rX, rY, rZ"
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ)"
      [nbMulti; ctx_rv; dRHS];
  !! Instr.move_out ~dest:[tBefore; ctx] [nbMulti; ctx; cVarDef ~regexp:true "\\(coef\\|sign\\)."];
  !! Trace.reparse(); (* Note: when using menhir, !!! does not seem to work fine *)
  !! Loop.fold_instrs ~index:"k" [cFunDef "cornerInterpolationCoeff"; sInstr "r.v"];

  bigstep "Update particles in-place instead of in a local variable "; (* LATER: it might be possible to change the script to postpone this step *)
  !! Variable.reuse ~space:(expr "p->speed") [step; cVarDef "speed2" ];
  !! Variable.reuse ~space:(expr "p->pos") [step; cVarDef "pos2"];

  bigstep "Reveal write operations involved manipulation of particles and vectors";
  let ctx = cOr [[cFunDef "bag_push_serial"]; [cFunDef "bag_push_concurrent"]] in
  !! List.iter (fun typ -> Struct.set_explicit [nbMulti; ctx; cWrite ~typ ()]) ["particle"; "vect"];
  !! Function.inline [steps; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; steps; cWrite ~typ:"vect" ()];

  bigstep "inlining of [cornerInterpolationCoeff] and [accumulateChargeAtCorners]";
  !! Function.inline [nbMulti; cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
  !! Function.inline [step; cFun "accumulateChargeAtCorners"];
  !! Function.inline ~vars:(AddSuffix "2") [step; cFun "idCellOfPos"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "cornerInterpolationCoeff"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [stepLF; cFun "cornerInterpolationCoeff"];

  bigstep "Optimization of charge accumulation";
  !! Sequence.intro ~mark:"fuse" ~start:[step; cVarDef "contribs"] ();
  !! Loop.fusion_targets [cMark "fuse"];
  !! Trace.reparse();
  !! Instr.inline_last_write [step; cWrite (); cCellRead ~base:[cFieldRead ~base:[cVar "contribs"] ()] ()];

  bigstep "Low level iteration on chunks of particles";
  !! Sequence.intro ~mark:"loop" ~start:[steps; cVarDef "bag_it"] ~nb:2 ();
  !! Sequence.intro_on_instr [steps; cMark "loop"; cFor_c ""; dBody];
  !! Function_basic.uninline ~fct:[cFunDef "bag_iter_ho_basic"~body:[cVarDef "it"]] [steps; cMark "loop"];
  !! Expr.replace_fun "bag_iter_ho_chunk" [steps; cFun "bag_iter_ho_basic"];
  !! Function.inline [steps; cFun "bag_iter_ho_chunk"];
  (* Arthur: Why I can't use steps target? *)
  !! Function.beta ~indepth:true [step];
  !! Function.beta ~indepth:true [stepLF];
  !! Variable.init_detach [steps; cVarDef "p"];
  !! Struct.set_explicit [nbMulti;step; sInstr "p->pos ="];
  !! Struct.set_explicit [nbMulti;step; sInstr "p->speed ="];
  !! Instr.inline_last_write ~write:[step; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; step; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)
  !! Instr.inline_last_write ~write:[stepLF; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; stepLF; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)
  !! Instr.delete [nbMulti; cTopFunDef ~regexp:true "bag_iter.*"];

  bigstep "AOS-TO-SOA";
  !! Struct.set_explicit [step; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; step; cFieldWrite ~base:[cVar "p2"] ()];
  !! Function.inline  ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "wrapArea"];
  !! Variable.inline [nbMulti; step; cVarDef ~regexp:true "[x,y,z]."];
  !! List.iter (fun f -> Struct.inline f [cTypDef "particle"]) ["speed"; "pos"];
  !! Struct.inline "items" [cTypDef "chunk"];

  bigstep "Prepare the stage for scaling (move definitions and introduce constants)";
  !! Struct.to_variables [step; cVarDef "fieldAtPos"];
  !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factorC", expr "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), expr ("factorC / cell" ^ d))))
     [tBefore; step; cVarDef "field_at_corners"];
  (* !! Function.use_infix_ops ~indepth:true [step; dBody]; *)

  bigstep "Scaling of electric field";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [step; cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [nbMulti; step; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cVarDef "accel"];

  bigstep "Scaling of speed and positions";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("(stepDuration / cell"^d^")"))
         [nbMulti; step; (* sInstrRegexp ~substr:true ("\\[i\\] = c->itemsSpeed" ^ d); *)
         cOr [ [ sExprRegexp ~substr:true ("c->itemsSpeed" ^ d ^ "\\[i\\]")] ;
               [ sExpr ("p2.speed" ^d) ] ] ] );
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("(1 / cell"^d^")"))
         [nbMulti; step; (*sInstrRegexp ~substr:true ("\\[i\\] = c->itemsPos" ^ d); *)cOr [
            [sExprRegexp ~substr:true ("c->itemsPos" ^ d ^ "\\[i\\]")];
            [sExpr ("p2.pos"^d)]
          ] ]);
  !! Trace.reparse();
  !! Variable.inline [step; cVarDef "accel"];
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cFor "i"; cOr [ [cCellWrite ~index:[cVar "i"] ()]; [sInstr "p2."]; [cVarDef ~regexp:true "..[0-2]"] ] ];
  (* !! Function.use_infix_ops ~indepth:true [step]; *)

  bigstep "Enumerate grid cells by coordinates";
  !! Variable.to_const [nbMulti; cVarDef ~regexp:true "grid."];
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [step; cFor "idCell" ~body:[cFor "k"]];
  
  bigstep "Make positions relative and store them using float"; (* LATER: it might be possible to perform this transformation at a higher level, using vect operations *)
  (* let citemsposi d = "c->itemsPos" ^ d ^ "[i]" in *)
  !! iter_dims (fun d ->
    Variable.reuse ~space:(var ("i" ^ d ^ "2")) [step; cVarDef ("i" ^ d ^ "1")]);
  !! Trace.reparse();
  !! iter_dims (fun d ->
      Variable.bind ~const:true ("p" ^ d ^ "2") [occLast;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS];
      Variable.bind ~const:true ("p" ^ d ) [occFirst;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS]);
  !! iter_dims (fun d ->
    Instr.read_last_write [step; cVarDef ~regexp:true "i.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
    Instr.inline_last_write [step; cVarDef ~regexp:true "p.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [step; cVarDef ~regexp:true "..2"];
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore; step; cVarDef "p2"])) [step; cVarDef ~regexp:true "r.1"];
  !! Instr.move ~dest:[tAfter; step; cVarDef "iZ2"][step; cVarDef "idCell2"]; (* TODO: Use regex *)
  !! iter_dims (fun d ->
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "0")) [step; cVarDef ~regexp:true "r.0"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d)) [step; cVarDef ~regexp:true ("p" ^ d); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "2")) [step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "2")) [step; cVarDef ("r" ^ d ^ "1"); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]); 
  !! Trace.reparse();
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cVarDef ~regexp:true "r.."; dInit];
  !! Instr.delete [nbMulti; step; cVarDef ~regexp:true "i.0"];
  !! Variable.fold ~at:[cFieldWrite ~base:[cVar "p2"] ()] [nbMulti; step; cVarDef ~regexp:true "r.1"];
  
  if not doublepos then begin
    bigstep "Turn positions into floats";
    !! Cast.insert (atyp "float") [sExprRegexp  ~substr:true "p.2 - i.2"];
    !! Struct.update_fields_type "itemsPos." (atyp "float") [cTypDef "chunk"];
  end;

  bigstep "Introduce matrix operations, and prepare loop on charge deposit"; (* LATER: might be useful to group this next to the reveal of x/y/z *)
  !! Label.add "core" [step; cFor "iX" ];
  !! Matrix_basic.intro_mmalloc [nbMulti; cFunDef "allocateStructures";cFun "malloc"];
  !! Matrix.intro_mindex (expr "nbCells") [step; cCellAccess ~base:[cVar "deposit"] ()];
  !! Matrix.intro_mindex (expr "nbCells") [nbMulti;step; cCellAccess ~base:[cVar "bagsNext"] ()];
  !! Label.add "charge" [step; cFor "k" ~body:[cVar "deposit"]];
  !! Variable.inline [step; cVarDef "indices"];

  let alloc_instr = [cFunDef "allocateStructures";cWriteVar "deposit"] in
  bigstep "Duplicate the charge of a corner for the 8 surrounding cells";
  !! Matrix.delocalize "deposit" ~into:"depositCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~dim:(expr "8") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~use:(Some (expr "k")) ~alloc_instr [cLabel "core"];

  bigstep "Apply a bijection on the array storing charge to vectorize charge deposit";
  let mybij_def =
      "int mybij(int nbCells, int nbCorners, int idCell, int idCorner) {
        coord coord = coordOfCell(idCell);
        int iX = coord.iX;
        int iY = coord.iY;
        int iZ = coord.iZ;
        int res[8] = {
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
      }" in (* LATER: menhir should support "res[]" syntax *)
  !! Sequence.insert (stmt mybij_def) [tBefore; step];
  !! Matrix.biject "mybij" [step; cVarDef "depositCorners"];
  !! Expr.replace ~reparse:false (expr "MINDEX2(nbCells, 8, idCell2, k)")
      [step; cLabel "charge"; cFun "mybij"];
      (* LATER: use: sExpr "mybij(nbCorners, nbCells, indicesOfCorners(idCell2).v[k], k)" *)

       (* ARTHUR: simplify mybij calls in the sum *)

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Sequence.insert (expr "#include \"omp.h\"") [tBefore; step];
  !! Variable.insert ~const:true ~name:"nbThreads" ~typ:(atyp "int") ~value:(expr "omp_get_num_threads()")[tFirst; step; dBody];
  !! Variable.insert ~const:false ~name:"idThread" ~typ:(typ_int()) ~value:(expr "omp_get_thread_num()") [tBefore; cLabel "charge" ];
  !! Trace.reparse();

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~use:(Some (expr "idThread")) [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "depositCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];

  bigstep "Coloring";
  !! Variable.insert_list ~const:true ~defs:[("int","block",lit "2"); ("int","halfBlock",expr "block / 2")] [tBefore; cVarDef "nbCells"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [step; cFor ("i" ^ d)];
    Loop.color (expr color) ~index:("ci"^d) [step; cFor bd]
    in
  !! iter_dims (fun d -> colorize "block" "2" d);
  !! Loop.reorder ~order:((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims) [step; cFor "ciX"];

  bigstep "Delocalize objects";
  let alloc_instr = [cFunDef "allocateStructures";cWriteVar "bagsNext"] in
  !! Matrix.delocalize "bagsNext" ~into:"bagsNexts" ~dim:(lit "2") ~indices:["idCell"] ~alloc_instr ~index:"bagsKind" ~ops:delocalize_obj [cLabel "core"];
  !! Variable.insert_list ~reparse:false ~defs:(
    ["const int", "PRIVATE", lit "0"; "const int", "SHARED", lit "1"]) [tBefore; step; cVarDef "field_at_corners"];
  !! Instr.delete [step; cFor "idCell" ~body:[cFun "bag_swap"]];
  !! Variable.exchange "bagsNext" "bagsCur" [nbMulti; step; cFor "idCell"];


  bigstep "Introduce atomic push operations, but only for particles moving more than one cell away";
  !! Variable.insert ~const:true ~typ:(atyp "coord") ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; step; cVarDef "idCell2"];
  !! Variable.insert ~const:true ~typ:(atyp "bool") ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d ->
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; step; cFun "bag_push"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") ~mark:"push" [step; cFun "bag_push"];
  !! Specialize.any (expr "PRIVATE") [cMark "push"; dThen; cAny];
  !! Specialize.any (expr "SHARED") [cMark "push"; dElse; cAny];
  (* !! Variable.bind "b2" ~const:true ~is_ptr:true [nbMulti; step; cFun "bag_push"; dArg 0]; *)
  !! Expr.replace_fun "bag_push_serial" [cMark "push"; dThen; cFun "bag_push"];
  !! Expr.replace_fun "bag_push_concurrent" [cMark "push"; dElse; cFun "bag_push"];
     Marks.remove "push" [cMark "push"];

  bigstep "Loop splitting to separate processing of speeds, positions, and charge deposit";
  !! Loop.hoist [step; cVarDef "idCell2"];
  !! Instr.move ~dest:[step; tAfter; cVarDef "p2"] [step; cVarDef "co"];
  !! Instr.copy ~dest:[tAfter; step; cVarDef "p2"] [step;cVarDef "idCell2"];
  !! Instr.copy ~dest:[tAfter; step; cVarDef "idThread"] [occFirst; step;cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; step; cOr[[cVarDef "iX2"];[cVarDef "p2"];[cVarDef "iX1"]]];
  (* LATER: fission should automatically do the duplication of references when necessary *)

  bigstep "Parallelization";
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];
)

(* TODO:
  instead of
    const int nbThread = 8
  we want to do:

  int nbTread;

  int main() {
    num_threads = omp_get_num_threads();
    ..
  }
*)
(* TODO:
  the transformation that introduces float for pos needs to take place before "AOS-TO-SOA" else we have double itemsPosX
  Introduce before "aos-to-soa" a separate bigstep "Turn positions into float", and move there the
   Cast.insert and the Struct.update_field.
*)
(* TODO
  introduce a flag at the top of the file to deactivate the floating point positions
   (this is used for the checker and to demonstrate conditional transformations)

    let doublepos = false // I'll later make it a command line argument

    then:

    if not doublepos then begin
      bigstep "Turn positions into float";
      !! Cast.insert ..
      !! Struct.update_field...
    end

*)
(* TODO
  !! Function.inline [nbMulti;cFun "matrix_vect_mul"];
   could become
   !! Function.inline ~delete:true [nbMulti;cFun "matrix_vect_mul"];
   to delete the toplevel function definition, since we no longer need it (makes reparsing faster)
*)

(* TODO:
    for (int biX = ciX * block; biX < gridX; biX += block * block) {
    I don't think this is right; it should be biX += 2*block  I think
    I know we  block=2, but it's not quite the same.
    The coloring is always with parameter 2, whereas the tiling is with parameter block
*)
(* TODO
      c->itemsSpeedX[i] / (cellX * cellX) + fieldAtPosX;
    The factor used for scaling has been inversed. All the cellX are meant to cancel out.
    See the scan that I sent the other day.
    It it's not obvious how to fix, we'll discuss it.
*)
(* TODO
    The split for the loops should be:
    1) after c->itemsSpeedZ[i] =
    2) after c->itemsPosZ[i]
    There is no split between push and deposit.
    There is a recomputation of coefs with respect to the corners of idCell2 that has vanished somehow
    (we can reuse the coefs associated with idCell).

*)
(* TODO

  bag *bagsNexts = ( bag * ) MMALLOC2(2, nbCells, sizeof(bag));
  for (int i1 = 0; i1 < nbCells; i1++) {
    for (int i = 0; i < 2; i++) {

  it'd be better to make the dimension "2" be the last one.
  The indices should be name "idCell" and "bagKind"
*)
(* TODO: the arbitrary if statement needs to apply to bag_push
  before we name b2, so that we get:

      if (isDistFromBlockLessThanHalfABlock) {
        bag *const b2 = &bagsNexts[MINDEX2(2, nbCells, ANY(2), idCell2)];
        bag_push(b2, p2);
      } else {
        bag *const b2 = &bagsNexts[MINDEX2(2, nbCells, ANY(2), idCell2)];
        bag_push(b2, p2);
      }

  It the then branch, we specialize cAny to PRIVATE
  It the else branch, we specialize cAny to SHARED

  you need to introduce beforehand
  const int PRIVATE = 0;
  const int SHARED = 1;
*)
(* TODO
     bag *bagsNexts = (bag* )MMALLOC2(2, nbCells, sizeof(bag));
     and the initialization loop that follows
  needs to be moved into the allocation function (eg at tLast)

     MFREE(bagsNexts);
  needs to be moved into the deallocation function (eg at tLast)
*)
(* TODO
  because you do the variable.exchange for  bagCur and bagNext
  you can delete the loop
    for (int idCell = 0; idCell < nbCells; idCell++)
      bag_swap(&bagsCur[idCell], &bagsNext[MINDEX1(nbCells, idCell)]);

  as this is already done implicitly, now .
*)
(* TODO:
  all instructions involving bagNext (without s) can be deleted;
  at the very least, the malloc and the free and bag_init_initial(&bagsNext[idCell]);
  *)
(*
   TODO:
   the outler loop on idCell with     sum += depositThreadCorners
   and the outer loop on idCell (currently i1) with  bag_append
   should be made parallel
*)

(* LATER:
  const int idCell = (iX * gridY + iY) * gridZ + iZ;
  could be
  cellOfCoord(iX, iY, iZ)
  if the user provides the name for this function
*)
(* TODO:
  const int PRIVATE = 0;
  at top level
  *)
(* TODO:
  delete varDef "p"
  *)