open Optitrust
open Target
open Ast

let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices
let step = cFunDef "step"
let stepLF = cFunDef "stepLeapFrog"
let steps = cOr [[step]; [stepLF]]
let dims = ["X"; "Y"; "Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_sum = Local_arith (Lit_double 0., Binop_add)
let delocalize_bag = Local_obj ("bag_init_initial", "bag_append", "bag_free_initial")

let doublepos = true (* LATER: ARTHUR make this command line argument *)

let _ = Run.script_cpp ~parser:Parsers.Menhir ~inline:["pic_demo.h";"bag.hc";"particle.hc";"bag_atomics.h";"bag.h-"] (fun () ->

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
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
  !! Loop.fold_instrs ~index:"k" [cTopFunDef "cornerInterpolationCoeff"; cCellWrite ~base:[cVar "r"] ()];

  bigstep "Update particles in-place instead of in a local variable ";
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
  !! List.iter (fun f -> Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; f; cFun "cornerInterpolationCoeff"])
    [stepLF; step];

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
  (* LATER !! Function.beta ~indepth:true [dRoot]; try in a unit test with two beta reductions to do *)
  !! List.iter (fun f -> Function.beta ~indepth:true [f]) [step; stepLF];
  !! Instr.delete [nbMulti; cTopFunDef ~regexp:true "bag_iter.*"];

  bigstep "Elimination of pointer p, to prepare for aos-to-soa";
  !! Variable.init_detach [steps; cVarDef "p"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "wrapArea"];
  !! Struct.set_explicit [nbMulti; step; cFieldWrite ~base:[ cVar "p"] ()];
  !! Variable.inline [nbMulti; step; cVarDef ~regexp:true "[xyz]0"];
  !! Instr.inline_last_write [nbMulti; steps; cRead ~addr:[cStrictNew; cVar "p"] ()];
  !! Instr.delete [steps; cVarDef "p"];

  bigstep "AOS-TO-SOA";
  !! Struct.set_explicit [step; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; step; cFieldWrite ~base:[cVar "p2"] ()];
  !! List.iter (fun f -> Struct.inline f [cTypDef "particle"]) ["speed"; "pos"];
  !! Struct.inline "items" [cTypDef "chunk"];

  bigstep "Scaling for the electric field";
  !! Struct.to_variables [step; cVarDef "fieldAtPos"];
  !! Variable.insert_list ~reparse:true ~defs:(List.rev ( (* TODO *)
         ["const double", "factorC", expr "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), expr ("factorC / cell" ^ d)))))
     [tFirst; step; dBody];
  (* TODO: res should be field_at_corners *)
  !! Function.inline [step; cFun "getFieldAtCorners"];
  !! Struct.set_explicit [step; cFor "k"; cCellWrite ~base:[cFieldRead ~base:[cVar "res"] ()] ()];
  !! iter_dims (fun d ->
      let d1 = String.lowercase_ascii d in
      Accesses.scale ~factor:(var ("factor" ^ d)) [step; cFor "k"; cFieldWrite ~field:d1 ()]);
(* DONE:
1) inline getFieldAtCorners in function step
2) do a set explicit  field_at_corners.v[k] = field[indices.v[k]];
3) replace
  field_at_corners.v[k].x = field[indices.v[k]].x;
with
  field_at_corners.v[k].x = field[indices.v[k]].x * factorX;
using
  Accesses.scale ~neg:true ~factor:(var ("factor" ^ d))  [ cWrite ~base:[sExpr ("field_at_corners.v[k]." ^ d)] ]
    ==> equiv to [sInstr "field_at_corners.v[k] = field[indices.v[k]]"]
*)
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [step; cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]);
  !! Variable.inline [nbMulti; step; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cVarDef "accel"];
  (* LATER: simpl_rec an alias for simpl ~indepth:true *)

  bigstep "Scaling of speed and positions";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("(stepDuration / cell"^d^")"))
         [nbMulti; step; cOr [ [ sExprRegexp ~substr:true ("c->itemsSpeed" ^ d ^ "\\[i\\]")] ;
               [ sExpr ("p2.speed" ^ d) ] ] ] );
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("(1 / cell"^d^")"))
         [nbMulti; step; cOr [ [sExprRegexp ~substr:true ("c->itemsPos" ^ d ^ "\\[i\\]")];
            [sExpr ("p2.pos" ^ d)]
          ] ]);
  !! Trace.reparse();
  !! Variable.inline [step; cVarDef "accel"];
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cFor "i"; cOr [
      [cCellWrite ~index:[cVar "i"] ()]; [sInstr "p2."];
      [cVarDef ~regexp:true "[ir][XYZ][0-2]"] ] ];

  bigstep "Enumerate grid cells by coordinates";
  !! Variable.to_const [nbMulti; cVarDef ~regexp:true "grid."]; (* NOT OK *)
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [step; cFor "idCell" ~body:[cFor "k"]];

  bigstep "Make positions relative to the cell corner";
  !! iter_dims (fun d ->
    Variable.reuse ~space:(var ("i" ^ d ^ "2")) [step; cVarDef ("i" ^ d ^ "1")]);
  !! iter_dims (fun d ->
      Variable.bind ~const:true ~typ:(Some (atyp "double")) ("p" ^ d ^ "2") [occLast;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS];
      Variable.bind ~const:true ("p" ^ d ) ~typ:(Some (atyp "double")) [occFirst;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS]);
  !! iter_dims (fun d ->
    Instr.read_last_write [step; cVarDef ~regexp:true "i.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
    Instr.inline_last_write [step; cVarDef ~regexp:true "p.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [step; cVarDef ~regexp:true ("\\(i.*2\\|p.2\\|i.2\\)")];
  !! Instr.(gather_targets  ~dest:(GatherAt [tBefore; cVarDef "p2"])) [step; cVarDef ~regexp:true "r.1"];
  !! iter_dims (fun d ->
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "0")) [step; cVarDef ~regexp:true "r.0"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d)) [step; cVarDef ~regexp:true ("p" ^ d); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "2")) [step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "2")) [step; cVarDef ("r" ^ d ^ "1"); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);
  !! Trace.reparse();
  !! Arith.(simpl ~indepth:true expand) [nbMulti; step; cVarDef ~regexp:true "r.."; dInit];
  !! Instr.delete [nbMulti; step; cVarDef ~regexp:true "i.0"];
  !! Variable.fold ~at:[cFieldWrite ~base:[cVar "p2"] ()] [nbMulti; step; cVarDef ~regexp:true "r.1"];

  if (*not*) doublepos then begin
    bigstep "Turn positions into floats";
    !! Cast.insert (atyp "float") [sExprRegexp ~substr:true "p.2 - i.2"];
        (* LATER: target the [sRexegp "c->itemsPos[[.]] = ")  or iter_dims  or   ==>best: cOr (map_dims .. ) *)
    !! Struct.update_fields_type "itemsPos." (atyp "float") [cTypDef "chunk"];
    (* LATER: type particle would need to be converted too
       const vect pos = {x, y, z};
       would need cast around the values
    *)
  end;

  (* TODO: bigstep "Simplification of fwrap";
  !! Rewrite.equiv_at "double a; double b; double c; double x; ==> fwrap(a,b*c) == fwrap(a/x, (b*c)/x)" [nbMulti; cVarDef "pX2"; cInit ()]; *)

  bigstep "Introduce matrix operations, and prepare loop on charge deposit"; (* LATER: might be useful to group this next to the reveal of x/y/z *)
  !! Label.add "core" [step; cFor "iX" ];
  !! Matrix_basic.intro_mmalloc [nbMulti; cFunDef "allocateStructures";cFun "malloc"];
  !! Matrix.intro_mindex (expr "nbCells") [nbMulti; step; cCellAccess ~base:[cOr [[cVar "deposit"]; [cVar "bagsNext"]]]() ];
  !! Label.add "charge" [step; cFor "k" ~body:[cVar "deposit"]];
  !! Variable.inline [occLast; step; cVarDef "indices"];

  bigstep "Duplicate the charge of a corner for the 8 surrounding cells";
  let alloc_instr = [cFunDef "allocateStructures"; cWriteVar "deposit"] in
  !! Matrix.delocalize "deposit" ~into:"depositCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~dim:(expr "8") ~index:"k" ~acc:"sum" ~ops:delocalize_sum ~use:(Some (expr "k")) ~alloc_instr [cLabel "core"];

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
  !! Sequence.insert (expr "#include \"omp.h\"") [tFirst; dRoot];
  !! Variable.insert ~const:false ~name:"nbThreads" ~typ:(atyp "int") [tBefore; cVarDef "nbCells"]; 
  !! Omp.declare_num_threads "nbThreads";
  !! Omp.get_num_threads "nbThreads" [tFirst; step; dBody];
  !! Omp.get_thread_num "idThread" [tBefore; cLabel "charge"];
  !! Trace.reparse();

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(expr "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_sum ~use:(Some (expr "idThread")) [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "depositCorners"] ~rhs:[cDouble 0.] ()]];

  bigstep "Coloring";
  !! Variable.insert_list ~const:true ~defs:[("int","block",lit "2"); ("int","halfBlock",expr "block / 2")] [tBefore; cVarDef "nbCells"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "b" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:("b"^d) [step; cFor ("i" ^ d)];
    Loop.color (expr color) ~index:("c"^d) [step; cFor bd]
    in
  !! iter_dims (fun d -> colorize "block" "2" d);
  !! Loop.reorder ~order:((add_prefix "c" dims) @ (add_prefix "b" dims) @ idims) [step; cFor "cX"];
  !! Instr.move_out ~dest:[step; tBefore; cFor "iX"] [step; cVarDef "idThread"]; 

  bigstep "Delocalize bags";
  !! Matrix.delocalize "bagsNext" ~into:"bagsNexts" ~dim:(lit "2") ~indices:["idCell"]
    ~alloc_instr:[cFunDef "allocateStructures"; cWriteVar "bagsNext"]
    ~index:"bagsKind" ~ops:delocalize_bag [cLabel "core"];
  !! Variable.insert_list_same_type (atyp "const int") [("PRIVATE", lit "0"); ("SHARED", lit "1")] [tFirst; step; dBody];
  !! Instr.delete [step; cFor "idCell" ~body:[cFun "bag_swap"]];
  !! Variable.exchange "bagsNext" "bagsCur" [nbMulti; step; cFor "idCell"];
  !! Instr.move_out ~dest:[tBefore; cTopFunDef "step"] [step; cOr [[cVarDef "PRIVATE"]; [cVarDef "SHARED"]]];
  (* LATER !! Instr.delete [cOr[[cVarDef "bagsNext"];[ cKindInstr; cVar "bagsNext"]]]; *)
  (* LATER !! Variable.delete [cVarDef "bagsNext"] ==> shorthand for above *)
  !! Instr.delete [cOr[[cVarDef "bagsNext"];[cWriteVar "bagsNext"];[cFun ~regexp:true "\\(free\\|bag.*\\)" ~args:[[cVar "bagsNext"]]]]];
  (* bigstep "Cleanup"; *)
  let dep_and_bags = "\\(deposit.*\\|bagsNexts\\)" in
  !! Variable.init_detach [nbMulti; step; cVarDef ~regexp:true dep_and_bags];
  (* TODO: delocalize_obj ~labels:["allocBagsNext","","deallocBagsNext"] => creates a sequence with malloc and init loop *)
  (* TODO: move allocBagsNext and deallocBagsNext labelled blocks *)
  !! Instr.move_out ~dest:[tAfter; cVarDef "deposit"] [nbMulti; step; cVarDef ~regexp:true dep_and_bags];
  !! Instr.move_out ~dest:[tAfter; cTopFunDef "allocateStructures"; cWriteVar "deposit"] [nbMulti; cWriteVar ~regexp:true dep_and_bags];
  !! Instr.move_out ~dest:[tAfter; cTopFunDef "deallocateStructures"; cFun "free" ~args:[[cVar "field"]]] [nbMulti; step; cFun "MFREE"];
  !! Instr.move_out ~dest:[tAfter; cTopFunDef "allocateStructures"; cFor ""] [nbMulti;step; cFor "idCell" ~body:[cFun "bag_init_initial"]];
  !! Instr.move_out ~dest:[tAfter; cTopFunDef "deallocateStructures"; cFor ""] [nbMulti;step; cFor "idCell" ~body:[cFun "bag_free_initial"]];
  !! Function.use_infix_ops ~indepth:true [step; dBody];

  bigstep "Introduce atomic push operations, but only for particles moving more than one cell away";
  !! Variable.insert ~typ:(atyp "coord") ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; step; cVarDef "idCell2"];
  !! Variable.insert ~typ:(atyp "bool") ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d -> (* ARTHUR: add support for wraparound here *)
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; step; cFun "bag_push"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") ~mark:"push" [step; cFun "bag_push"];
  !! Specialize.any (expr "PRIVATE") [cMark "push"; dThen; cAny];
  !! Specialize.any (expr "SHARED") [cMark "push"; dElse; cAny];
  !! Expr.replace_fun "bag_push_serial" [cMark "push"; dThen; cFun "bag_push"];
  !! Expr.replace_fun "bag_push_concurrent" [cMark "push"; dElse; cFun "bag_push"];
     Marks.remove "push" [cMark "push"];

  bigstep "Loop splitting to separate processing of speeds, positions, and charge deposit";
  !! Variable.to_nonconst [step; cVarDef "idCell2"];
  !! Loop.hoist ~array_size:(Some (expr "CHUNK_SIZE")) [step; cVarDef "idCell2"];
  let dest = [tBefore; step; cVarDef "isDistFromBlockLessThanHalfABlock"] in
  !! Instr.copy ~dest [step; cVarDef "idCell2"];
  !! Instr.move ~dest [step; cVarDef "co"];
  !! Loop.fission [nbMulti; tBefore; step; cOr [[cVarDef "pX"]; [cVarDef "rX1"]]];
  (* LATER: fission should automatically do the duplication of references when necessary *)

  bigstep "Parallelization";
  !! Omp.parallel_for [Shared ["idCell"]] [occFirst; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["idCell"]] [occLast; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["biX";"biY";"biZ"]] [tBefore; cFor "biX"];
  (* !! Omp.simd [] [tBefore; step;cFor "i"]; *)(* TODO: Fix the issue with the last loop *)
  !! Omp.simd [] [occFirst; tBefore; step; cFor "i"]; (* TODO: occurences 0 and 1 for this line and the next *)
  !! Omp.simd [] [occIndex 1; tBefore; step; cFor "i"];
  
)





(*
   CHECK:
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
(* LATER:
  grid_enumerate on i<X*Y*Z
*)

(* TODO:
  #include <stdalign.h>
  alignas(16)  print to front of type
*)
(*
  Align.header => adds  #include <stdalign.h> to the top of the ast
  Align.assume "t" =>
     t = __builtin_assume_aligned(t, VEC_ALIGN);
  Align.def 16 [cVarDef "x"]
    => add the attribute to the type of the definition

*)
(* TODO
  Flags.print_coumpound_expressions
*)
(* TODO =>
  make optitrust
  make pic_demo_out.cpp
  cd ../case_study/pic/scripts
  ./compile.sh pic_optimized.c
  ./check.sh pic_demo.c pic_optimized.c
*)


(*
TODO
#pragma omp parallel for collapse(3)

bix -> bx
cix -> cx

TODO
 #pragma omp simd aligned(coeffs_x, coeffs_y, coeffs_z, signs_x, signs_y, signs_z:VEC_ALIGN)
 on the charge: loop


#pragma omp parallel for
on idcell
*)
(* ARTHUR

#pragma omp parallel for
  for (int idCell = 0; idCell < nbCells; idCell++) {
    for (int idCorner = 0; idCorner < 8; idCorner++) {
      int sum = 0;
      for (int k = 0; k < nbThreads; k++) {
        sum += depositThreadCorners[MINDEX3(nbThreads, nbCells, 8, k, idCell,
                                            idCorner)];
      }
      depositCorners[MINDEX2(nbCells, 8, idCell, idCorner)] = sum;
    }
  }
  *)
  (* TODO

  eliminate the loop
                          double_nbCorners coeffs;
                        for (int k = 0; k < 8; k++) {
                          coeffs.v[k] = (coefX[k] + signX[k] * rX0) *
                                        (coefY[k] + signY[k] * rY0) *
                                        (coefZ[k] + signZ[k] * rZ0);
                        }

*)