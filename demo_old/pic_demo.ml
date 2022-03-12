open Optitrust
open Target
open Ast


let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Local_arith (Lit_double 0., Binop_add)

let _ = Parsers.(select Menhir)

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission ~split_between:true [ctx; cFor "k"];
  !! Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp ~substr:true "res.*\\[0\\]"];
  !! Function.inline [main; cFun "matrix_vect_mul"];

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
  !! Variable.reuse ~space:(expr "p->speed") [main; cVarDef "speed2" ];
  !! Variable.reuse ~reparse:true ~space:(expr "p->pos") [main; cVarDef "pos2"];

  bigstep "Reveal write operations involved manipulation of particles and vectors";
  let ctx = cOr [[cFunDef "bag_push_serial"]; [cFunDef "bag_push_concurrent"]] in
  !! List.iter (fun typ -> Struct.set_explicit [nbMulti; ctx; cWrite ~typ ()]) ["particle"; "vect"];
  !! Function.inline [main; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"vect" ()];

  bigstep "inlining of [cornerInterpolationCoeff] and [accumulateChargeAtCorners]";
  !! Function.inline [nbMulti; cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
  !! Function.inline [cOr [
       [cFun "vect8_mul"];
       [cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
       [cFun "accumulateChargeAtCorners"]]];
  !! Function.inline ~vars:(AddSuffix "2") [cFun "idCellOfPos"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "cornerInterpolationCoeff"];
  !! Variable.elim_redundant [nbMulti; cVarDef ~regexp:true "i.1"];

  bigstep "Optimization of charge accumulation";
  !! Sequence.intro ~mark:"fuse" ~start:[main; cVarDef "coeffs2"] ();
     Loop.fusion_targets [cMark "fuse"];
  !! Instr.inline_last_write ~write:[sInstr "coeffs2.v[k] ="]
       [main; sInstr "deltaChargeOnCorners.v[k] ="; sExpr "coeffs2.v[k]"];
  !! Instr.inline_last_write ~write:[cCellWrite ~base:[cVar "deltaChargeOnCorners"] ()]
       [cCellRead ~base:[cVar "deltaChargeOnCorners"] ()];

  (* TODO: Find out why this is not working *)
  (* !! Instr.inline_last_write ~write:[sInstr "deltaChargeOnCorners.v[k] ="]
       [main; sExpr "deltaChargeOnCorners.v[k]"]; *)

  bigstep "Low level iteration on chunks of particles";
  !! Sequence.intro ~mark:"loop" ~start:[main; cVarDef "bag_it"] ~nb:2 ();
  !! Sequence.intro_on_instr [main; cMark "loop"; cFor_c ""; dBody]; (* LATER: will be integrated in uninline *)
  !! Function_basic.uninline ~fct:[cFunDef "bag_iter_ho_basic"] [main; cMark "loop"];
  !! Expr.replace_fun "bag_iter_ho_chunk" [main; cFun "bag_iter_ho_basic"];
  !! Function.inline [main; cFun "bag_iter_ho_chunk"]; (* LATER: uninline+replace+inline+beta *)
  !! Function.beta ~indepth:true [main];
  (* LATER/ why is   show [nbMulti; main; cRead ~addr:[cVar "p"] ()];  not the same as show [nbMulti; main; cReadVar "p"] ? *)
  !! Variable.init_detach [main; cVarDef "p"];
  !! Instr.inline_last_write ~write:[main; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; main; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)
  (* !! Variable.to_const [main; cVarDef "p"];  LATER: does not work, because write in p->pos *)
  (* LATER: read_last_write/inline_last_write should be able to target the write in an initialization, this would avoid the detach *)
  !! Instr.delete [nbMulti; cTopFunDef ~regexp:true "bag_iter.*"];

  bigstep "Struct inline";
  (* !! Variable.inline [main; cVarDef "p"]; *)
  (* !! Variable.simpl_deref [main]; *)
  !! Struct.set_explicit [main; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; main; sInstr "p2."];
  !! List.iter (fun f -> Struct.inline f [cTypDef "particle"]) ["speed"; "pos"];

  bigstep "Aos-to-soa";
  !! Struct.inline "items" [cTypDef "chunk"];

  bigstep "Prepare the stage for scaling (move definitions and introduce constants)";
  !! Instr.move ~dest:[tBefore; main] [nbMulti; cFunDef ~regexp:true "bag_push.*"];
  !! Struct.to_variables [main; cVarDef "fieldAtPos"];
  !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factorC", expr "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), expr ("factorC / cell" ^ d))))
     [tBefore; cVarDef "nbSteps"];

  bigstep "Scaling of electric field";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [nbMulti; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl expand) [nbMulti; main; cVarDef "accel"; cStructInit; cStrict; Arith.constr];

  bigstep "Scaling of speed and positions";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("stepDuration / cell" ^ d))
         [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1. / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  !! Trace.reparse(); (* required for the terms to be visible to the simplifier *)
  !! Variable.inline [cVarDef "accel"];
  !! Arith.(simpl expand) [nbMulti; main; cFun "int_of_double"; dArg 0]; (* TODO: does nothing? *)
  !! Arith.(simpl expand) [nbMulti; main; cVarDef ~regexp:true "r.."; dInit ];
  !! Sequence.apply ~start:[tAfter; main; cWrite ~lhs:[cVar "fieldAtPosZ"]()]
       ~stop:[tAfter; main; cVarDef "coeffs2"] (fun m ->
       Arith.(simpl expand) [nbMulti; main; cMark m; cWrite(); dRHS; cStrictNew; Arith.constr];);
       (* LATER: Function.use_infix_ops [cMark m] *)
      (* ARTHUR: also missing simplifications in bag_push_concurrent *)

  bigstep "Enumerate grid cells by coordinates";
  !! Label.add "core" [cFor "idCell" ~body:[cFor "k"]];
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [cLabelBody "core"];

  (* TEMPORARY PREVIOUS SCRIPT
    bigstep "Make positions relative and store them using float"; (* LATER: it might be possible to perform this transformation at a higher level, using vect operations *)
    !! iter_dims (fun d ->
        Variable.bind ~const:true ("p" ^ d) [main; sInstr ("c->items[i].pos" ^ d ^ " ="); dRHS]);
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
    !! Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"]; (* TODO: ARTHUR remove substr and try [sExprRegexp "p. - i.."]; *)
    !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];
    (* !! Trace.reparse (); *)
  *)
  (* TEMPORARY ARTHUR WIP
          bigstep "Make positions relative and store them using float"; (* LATER: it might be possible to perform this transformation at a higher level, using vect operations *)
          let citemsposi d = "c->itemsPos" ^ d ^ "[i]" in
          !! iter_dims (fun d ->
              Variable.bind ~const:true ("p" ^ d) [main; sInstr (citemsposi d ^ " ="); dRHS]);
          !! Instr.(gather_targets ~dest:GatherAtFirst) [main; cVarDef ~regexp:true "p[X-Z]"];
          (* TODO  FIX : !! iter_dims (fun d ->
              Accesses.shift ~neg:true ~factor:(var ("i" ^ d)) [main; cVarDef ("p" ^ d); sExpr (citemsposi d)]);
          !! Instr.read_last_write [nbMulti; main; cVarDef ~regexp:true "i[X-Z]2"; cRead ~addr:[sExpr (citemsposi d)]()]; *)
          !! Instr.(gather_targets ~dest:(GatherAt [tAfter; main; cVarDef "pZ"])) [main; cVarDef ~regexp:true "i[X-Z]2"];
          (* TODO FIX !! iter_dims (fun d ->
              Accesses.shift ~neg:true ~factor:(var ("i" ^ d ^ "2")) [main; cWrite ~lhs:[sExpr (citemsposi d)] ()];
              Accesses.shift ~neg:true ~factor:(var ("i" ^ d ^ "2")) [main; cVarDef ~regexp:true "r[X-Z]1"; cRead ~addr:[sExpr (citemsposi d)] ()];
              ); *)
          !! Arith.(simpl expand) [nbMulti; main; cVarDef ~regexp:true "r[X-Z]1"; dInit];
          (* TODO FIX !! Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"]; *) (* TODO: ARTHUR remove substr and try [sExprRegexp "p. - i.."]; *)
          !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];
          (* !! Trace.reparse ();  LATER: needed? *)
  *)



  bigstep "Introduce matrix operations, and prepare loop on charge deposit"; (* LATER: might be useful to group this next to the reveal of x/y/z *)
  !! Matrix.intro_mops (var "nbCells") [main; cVarDef "nextCharge"];
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];
  !! Variable.inline [main; cVarDef "indices"];

  bigstep "Duplicate the charge of a corner for the 8 surrounding cells";
  !! Matrix.delocalize "nextCharge" ~into:"nextChargeCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~use:(Some (expr "k")) [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextCharge"] ~index:[] ~rhs:[cDouble 0.] ()]];

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
  !! Sequence.insert (stmt mybij_def) [tBefore; main];
  !! Matrix.biject "mybij" [main; cVarDef "nextChargeCorners"];
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)")
      [main; cLabel "charge"; cFun "mybij"];
      (* LATER: use: sExpr "mybij(nbCorners, nbCells, indicesOfCorners(idCell2).v[k], k)" *)

       (* ARTHUR: simplify mybij calls in the sum *)

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Sequence.insert (expr "#include \"omp.h\"") [tBefore; main];
  !! Variable.insert ~const:true ~name:"nbThreads" ~typ:(atyp "int") ~value:(lit "8") [tBefore; main]; (* TODO: remove ~value, see comment in Variable.insert *)
  !! Variable.insert ~const:false ~name:"idThread" ~typ:(typ_int()) ~value:(expr "omp_get_thread_num()") [tBefore; cLabel "charge" ];

  bigstep "Duplicate the charge of a corner for each of the threads";
  !! Matrix.delocalize "nextChargeCorners" ~into:"nextChargeThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add ~use:(Some (expr "idThread")) [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextChargeCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];

  bigstep "Make the new matrices persistent across iterations"; (* LATER: would be cleaner to do earlier, near the corresponding delocalize *)
  !! Instr.move_out ~dest:[tBefore; main; cFor "step"] [nbMulti; main; cVarDef ~regexp:true "nextCharge.*"];
  !! Instr.move_out ~dest:[tAfter; main; cFor "step"] [nbMulti; main; cFun "MFREE"];

  bigstep "Coloring";
  !! Variable.insert_list ~const:true ~defs:[("int","block",lit "2"); ("int","halfBlock",expr "block / 2")] [tBefore; cVarDef "nbCells"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [main; cFor ("i" ^ d)];
    Loop.color (expr color) ~index:("ci"^d) [main; cFor bd]
    in
  !! iter_dims (fun d -> colorize "block" "block" d);
  !! Loop.reorder ~order:((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims) [main; cFor "ciX"];

  bigstep "Introduce atomic push operations, but only for particles moving more than one cell away";
  !! Variable.insert ~const:true ~typ:(atyp "coord") ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; main; cVarDef "idCell2"];
  !! Variable.bind "b2" ~const:true ~is_ptr:true [main; cFun "bag_push"; dArg 0];
  !! Variable.insert ~const:true ~typ:(atyp "bool") ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d ->
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; main; cVarDef "b2"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") ~mark:"push" [main; cFun "bag_push"];
  !! Expr.replace_fun "bag_push_serial" [cMark "push"; dThen; cFun "bag_push"];
     Expr.replace_fun "bag_push_concurrent" [cMark "push"; dElse; cFun "bag_push"];
     Marks.remove "push" [cMark "push"];


  bigstep "Loop splitting to separate processing of speeds, positions, and charge deposit";
  !! Variable.to_const [main; cVarDef "nb"];
  !! Loop.hoist [main; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; main; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  (* !! Loop.fission [nbMulti; tBefore; main; cOr [[cVarDef "p2"]; [cVarDef "iX2"]]];

  !! Instr.move ~dest:[tAfter; main; cVarDef "iZ2"] [nbMulti; main; cVarDef ~regexp:true "r.1"];
  !! Loop.hoist [main; cVarDef "idCell2"];
  !! Instr.copy ~dest:[tBefore; cVarDef "co"] [main; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tAfter; main; cWriteVar "idCell2"];
   *)

  (* Discuss *)
  (* !! Instr.move ~dest:[tBefore; main; cVarDef "p2"] [main; cVarDef "idCell2"];
  !! Loop.hoist [main; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; main; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  !! Variable.insert ~typ:(atyp "int&") ~name:"idCell2" ~value:(expr "idCell2_step[i]") [tBefore; main; cVarDef "p2"]; *)
    (* TODO: above, we could use Instr.copy to improve the scipt, before the feature describe below gets implemented *)
    (* LATER: fission should automatically do the duplication of references when necessary *)

  bigstep "Parallelization";
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
  !! Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)

)

(* LATER:
    define the function
        type path_with_trms = (dir * trm) list
        trms_in_path : trm -> path -> path_with_trms
    which takes an AST, a path in that AST, and returns a path decorated with all intermediate terms.

     TODO: generalize the transformer argument in apply_on_transformed_targets
       (transformer : path -> 'a)
    to
       (transformer : trm -> path -> 'a)
       )

    TODO: define the transformers
      get_surrounding_access
      get_surrounding_read
      get_surrounding_write

     which can be used as transformers in apply_on_transformed_targets;

    TODO: use these transformers in Access.scale, to automatically take the path
    given by the user and compute the surrounding paths.
    For example, a scale on    sExpr "t[i]"  should operate on both the read and write in
      t[i] = t[i] + 1
*)

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

(* LATER: we need a reverse to use_infix_ops
   maybe name the function    Function.infix_ops_intro and infix_ops_elim *)

(* LATER: cWrite has ~lhs and ~rhs, but cRead has ~addr, this is not coherent, cRead should have ~arg or cWrite should have ~addr and ~arg *)


(*  LATER: compute should perform simplification recursively in atoms, see example of [w];
   to implement using trm_map. *)
