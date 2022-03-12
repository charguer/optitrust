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


let _ = Run.script_cpp ~parser:Parsers.Menhir ~inline:["pic_demo.h";"bag.hc";"particle.hc";"bag_atomics.h";"bag.h-"] (fun () ->

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Trace.time "step" (fun () -> Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]]);
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission ~split_between:true [ctx; cFor "k"];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp ~substr:true "res.*\\[0\\]"];
  !! Function.inline [nbMulti;cFun "matrix_vect_mul"];

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
  !! Loop.fold_instrs ~index:"k" [sInstr "r.v"];

  bigstep "Update particles in-place instead of in a local variable "; (* LATER: it might be possible to change the script to postpone this step *)
  !! Variable.reuse ~space:(expr "p->speed") [step; cVarDef "speed2" ];
  !! Variable.reuse ~reparse:true ~space:(expr "p->pos") [step; cVarDef "pos2"];

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
  (* !! Instr.read_last_write [step; cFor "k"; sInstr "+="; cCellRead ~index:[cVar "k"] ()];  *)

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
  !! Instr.inline_last_write ~write:[step; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; step; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)
  !! Instr.inline_last_write ~write:[stepLF; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; stepLF; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)
  !! Instr.delete [nbMulti; cTopFunDef ~regexp:true "bag_iter.*"];

  bigstep "AOS-TO-SOA";
  !! Struct.set_explicit [step; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; step; sInstr "p2."];
  !! Function.inline  ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "wrapAround"];
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
       Accesses.scale ~factor:(expr ("stepDuration / cell"^d))
         [nbMulti; step; sInstrRegexp ~substr:true ("\\[i\\] = c->itemsSpeed" ^ d); sExprRegexp ~substr:true ("c->itemsSpeed" ^ d ^ "\\[i\\]")]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1 / cell"^d))
         [nbMulti; step; sInstrRegexp ~substr:true ("\\[i\\] = c->itemsPos" ^ d); sExprRegexp ~substr:true ("c->itemsPos" ^ d ^ "\\[i\\]")]);

  !! Variable.inline [step; cVarDef "accel"];
  (* !! Arith.(simpl ~indepth:true expand) [nbMulti; step]; *)
  
  bigstep "Make positions relative and store them using float"; (* LATER: it might be possible to perform this transformation at a higher level, using vect operations *)
  !! Instr.inline_last_write [nbMulti; cFun "fmod"; cCellRead ~index:[cVar "i"] ()];
  
  (* show [cFun "fmod"; cRead ~addr:[sExpr "c->itemsPosX[i]"] ()]; *)
  (* Instr.read_last_write [cFun "fmod"; sExpr "c->itemsPosX[i]"]; *)
  
  
  
  bigstep "Make positions relative and store them using float"; (* LATER: it might be possible to perform this transformation at a higher level, using vect operations *)
  let citemsposi d = "c->itemsPos" ^ d ^ "[i]" in
  !! iter_dims (fun d ->
      Variable.bind ~const:true ("p" ^ d) [step; sInstr (citemsposi d ^ " = ("); dRHS]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [step; cVarDef ~regexp:true "p[X-Z]"];
  !! iter_dims (fun d ->
      Accesses.shift ~neg:true ~factor:(expr ("i" ^ d ^ "0")) [step; cVarDef ("p" ^ d); sExpr (citemsposi d)]);
  !! Instr.(gather_targets ~dest:(GatherAt [tAfter; step; cVarDef "pZ"])) [step; cVarDef ~regexp:true "i[X-Z]1"];
  !! iter_dims (fun d ->
      Accesses.shift ~factor:(expr ("i" ^ d ^ "1")) [step; sInstr (citemsposi d ^ " = p")];);
  !! Arith.(simpl expand) [nbMulti; step; cVarDef ~regexp:true "r[X-Z]1"; dInit];
  !! Cast.insert (atyp "float") [sExprRegexp  ~substr:true "p. \\+ i."];
  !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];


  bigstep "Enumerate grid cells by coordinates";
  !! Variable.to_const [nbMulti; cVarDef ~regexp:true "grid."];
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [step; cFor "idCell" ~body:[cFor "k"]];

  bigstep "Introduce matrix operations, and prepare loop on charge deposit"; (* LATER: might be useful to group this next to the reveal of x/y/z *)
  let alloc_tg = cChain [cFunDef "allocateStructures";cWriteVar "deposit"; cFun "malloc"] in
  !! Label.add "core" [step; cFor "iX" ];
  !! Matrix_basic.intro_mmalloc [nbMulti; alloc_tg]; (* TODO: Fix the combi version *)
  !! Matrix.intro_mindex (expr "nbCells") [step; cCellAccess ~base:[Target.cVar "deposit"] ()];
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
  !! Instr.replace ~reparse:false (expr "MINDEX2(nbCells, 8, idCell2, k)")
      [step; cLabel "charge"; cFun "mybij"];
      (* LATER: use: sExpr "mybij(nbCorners, nbCells, indicesOfCorners(idCell2).v[k], k)" *)

       (* ARTHUR: simplify mybij calls in the sum *)

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Sequence.insert (expr "#include \"omp.h\"") [tBefore; step];
  !! Variable.insert ~const:true ~name:"nbThreads" ~typ:(atyp "int") ~value:(lit "8") [tBefore; step]; (* TODO: remove ~value, see comment in Variable.insert *)
  !! Variable.insert ~const:false ~name:"idThread" ~typ:(typ_int()) ~value:(expr "omp_get_thread_num()") [tBefore; cLabel "charge" ];
  !! Trace.reparse();

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~use:(Some (expr "idThread")) [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "depositCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];
  (* TODO: Move to allocate(deallocate)Structures malloc(free) instructions *)

  bigstep "Coloring";
  !! Variable.insert_list ~const:true ~defs:[("int","block",lit "2"); ("int","halfBlock",expr "block / 2")] [tBefore; cVarDef "nbCells"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [step; cFor ("i" ^ d)];
    Loop.color (expr color) ~index:("ci"^d) [step; cFor bd]
    in
  !! iter_dims (fun d -> colorize "block" "block" d);
  !! Loop.reorder ~order:((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims) [step; cFor "ciX"];

  bigstep "Introduce atomic push operations, but only for particles moving more than one cell away";
  !! Variable.insert ~const:true ~typ:(atyp "coord") ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; step; cVarDef "idCell2"];
  !! Variable.bind "b2" ~const:true ~is_ptr:true [step; cFun "bag_push"; dArg 0];
  !! Variable.insert ~const:true ~typ:(atyp "bool") ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d ->
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; step; cVarDef "b2"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") ~mark:"push" [step; cFun "bag_push"];
  !! Expr.replace_fun "bag_push_serial" [cMark "push"; dThen; cFun "bag_push"];
     Expr.replace_fun "bag_push_concurrent" [cMark "push"; dElse; cFun "bag_push"];
     Marks.remove "push" [cMark "push"];

  bigstep "Loop splitting to separate processing of speeds, positions, and charge deposit";
  !! Loop.hoist [step; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; step; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  (* !! Loop.fission [nbMulti; tBefore; step; cOr [[cVarDef "p2"]; [cVarDef "iX2"]]];

  !! Instr.move ~dest:[tAfter; step; cVarDef "iZ2"] [nbMulti; step; cVarDef ~regexp:true "r.1"];
  !! Loop.hoist [step; cVarDef "idCell2"];
  !! Instr.copy ~dest:[tBefore; cVarDef "co"] [step; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tAfter; step; cWriteVar "idCell2"];
   *)

  (* Discuss *)
  (* !! Instr.move ~dest:[tBefore; step; cVarDef "p2"] [step; cVarDef "idCell2"];
  !! Loop.hoist [step; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; step; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  !! Variable.insert ~typ:(atyp "int&") ~name:"idCell2" ~value:(expr "idCell2_step[i]") [tBefore; step; cVarDef "p2"]; *)
    (* TODO: above, we could use Instr.copy to improve the scipt, before the feature describe below gets implemented *)
    (* LATER: fission should automatically do the duplication of references when necessary *)

  bigstep "Parallelization";
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

)
