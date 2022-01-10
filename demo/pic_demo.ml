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

  (* Part: inlining of the bag iteration *)
  (* TODO ARTHUR:
  !^ Sequence.intro ~mark:"loop" ~start:[cVarDef "bag_it"] ~nb:3 ();
  !! Sequence.intro_on_instr [cMark "loop"; cFor_c ""; dBody]; (* LATER: will be integrated in uninline *)
  !! Function_basic.uninline ~fct:[cFunDef "bag_ho_iter_basic"] [cMark "loop"];
  !! Instr.replace_fun "bag_ho_iter_chunk" [main; cFun "bag_ho_iter_basic"]; (* LATER: why don't we also have Expr.replace_fun ? *)
  !! Marks.add "iter" [cFun "bag_ho_iter_chunk"];
  !! Variable_basic.unfold ~accept_functions:true [cFunDef "bag_ho_iter_chunk"];
  show [main; cFunDef ""];
  !! Function_basic.beta [cMark "iter"];
  (* !! Function.inline [main; cFun "bag_ho_iter_chunk"]; *)
  (*!! Function.beta [cFor "i"; cAppFun()];  // where cAppFun() = "cApp ~base:[cStrict; cFunDef]()"
  Mark.rem "iter"  // a shorthand for Mark.remove [cMark "iter"]
  // maybe also with a sequence.inline on the body*)
  *)

  (* Part: optimization and inlining of [matrix_vect_mul] *)
  !^ let ctx = cTopFunDef "matrix_vect_mul" in
     Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission [nbMulti; tAfter; ctx; cFor "k"; sInstrRegexp "res\\.[x-y]"]; (* TODO: split between *)
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
  !^ Variable.reuse ~space:(expr "p->speed") [main; cVarDef "speed2" ];
  !! Variable.reuse ~space:(expr "p->pos") [main; cVarDef "pos2"];

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
  !! Instr.move_out ~dest:[tBefore; main] [nbMulti; main; cVarDef ~regexp:true "\\(coef\\|sign\\).0"];

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
  !! Variable.simpl_deref [main];
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
  !^ Cast.insert (atyp "float") [sExprRegexp  ~substr:true "\\(p. - i.\\)"]; (* TODO: ARTHUR remove substr and try [sExprRegexp "p. - i.."]; *)
  !! Struct.update_fields_type "pos." (atyp "float") [cTypDef "particle"];
  (* !! Trace.reparse (); *)

  (* Part: introduce matrix operations, and prepare loop on charge deposit *)
  !^ Matrix.intro_mops (var "nbCells") [main; cVarDef "nextCharge"];
  !! Label.add "charge" [main; cFor "k" ~body:[cVar "nextCharge"]];
  !! Variable.inline [main; cVarDef "indices"];

  (* Part: duplicate the charge of a corner for the 8 surrounding cells *)
  !^ Matrix.delocalize "nextCharge" ~into:"nextChargeCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~dim:(var "nbCorners") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "core"];
  !! Specialize.any "k" [nbMulti; main; cAny]; (* TODO: Why nbMulti needed *) (* TODO: exploit the ~use argument in delocalize *)
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextCharge"] ~index:[] ~rhs:[cDouble 0.] ()]];

  (* Part: apply a bijection on the array storing charge to vectorize charge deposit *)
  !^ let mybij_def =
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
     Sequence.insert (stmt mybij_def) [tBefore; main];
  !! Matrix.biject "mybij" [main; cVarDef "nextChargeCorners"];
  !! Instr.replace ~reparse:true (stmt "MINDEX2(nbCells, nbCorners, idCell2, k)")
      [main; cLabel "charge"; cFun "mybij"];
      (* LATER: use: sExpr "mybij(nbCorners, nbCells, indicesOfCorners(idCell2).v[k], k)" *)

       (* ARTHUR: simplify mybij calls in the sum *)

  (* Part: insert thread number and thread id *)
  !^ Sequence.insert ~reparse:false (stmt "int omp_get_thread_num();") [tBefore; main]; (* TODO: use a include instead *)
  !! Variable.insert ~name:"nbThreads" ~typ:"int" ~value:(lit "8") [tBefore; main]; (* TODO: remove ~value, see comment in Variable.insert *)
  !! Omp.get_thread_num "idThread" [tBefore; cLabel "charge"]; (* TODO: there is an extra semi-column appearing *)
       (* TODO: this could be just   Variable.insert ~name"idThread" ~value:(Omp.get_thread_num())
           where get_thread_num returns the term that corresponds to the function call; this would be more uniform. *)

  (* Part: duplicate the charge of a corner for each of the threads *)
  !^ Matrix.delocalize "nextChargeCorners" ~into:"nextChargeThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "core"];
  !! Specialize.any "idThread" [nbMulti; main; cAny]; (* TODO: why nbMulti here? *) (* TODO: exploit the ~use argument in delocalize *)
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "nextChargeCorners"] ~index:[] ~rhs:[cDouble 0.] ()]];

  (* Part: make the new matrices persistent across iterations *) (* LATER: would be cleaner to do earlier, near the corresponding delocalize *)
  !! Instr.move_out ~dest:[tBefore; main; cFor "step"] [nbMulti; main; cVarDef ~regexp:true "nextCharge."];
     Instr.move_out ~dest:[tAfter; main; cFor "step"] [nbMulti; main; cFun "MFREE"];

  (* Part: coloring *)
  !^ Variable.insert_list ~defs:[("int","block","2"); ("int","halfBlock","block / 2")] [tBefore; cVarDef "nbCells"];
  !! let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "bi" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [main; cFor ("i" ^ d)];
    Loop.color color ~index:("ci"^d) [main; cFor bd]
    in
    iter_dims (fun d -> colorize "block" "block" d);
  !! Loop.reorder ~order:(Tools.((add_prefix "c" idims) @ (add_prefix "b" idims) @ idims)) [main; cFor "ciX"];

  (* Part: introduce atomic push operations, but only for particles moving more than one cell away *)
  !^ Variable.insert ~const:true ~typ:"coord" ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; main; cVarDef "idCell2"];
  !! Variable.bind "b2" [main; cFun "bag_push"; sExpr "&bagsNext"];
        (* TODO: above, ~const:true  should create not a [const bag*]  but a [bag* const] *)
  !! Variable.insert ~const:true ~typ:"bool" ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:(trm_ands (map_dims (fun d ->
         expr ~vars:[d] "co.i${0} - bi${0} >= - halfBlock && co.i${0} - bi${0} < block + halfBlock")))
      [tBefore; main; cVarDef "b2"];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") [main; cFun "bag_push"];
       (* TODO: in insert_if, allow for an optional mark argument, to be attached to the new if statement; use this mark in the targets below *)
  !! Instr.replace_fun "bag_push_serial" [main; cIf(); dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; cIf(); dElse; cFun "bag_push"];

  (* Part: loop splitting to separate processing of speeds, positions, and charge deposit *)
  !^ Instr.move ~dest:[tBefore; main; cVarDef "p2"] [main; cVarDef "idCell2"];
  !! Loop.hoist [main; cVarDef "idCell2"];
  !! Loop.fission [nbMulti; tBefore; main; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  !! Variable.insert ~typ:"int&" ~name:"idCell2" ~value:(expr "idCell2_step[i]") [tBefore; main; cVarDef "p2"];
    (* LATER: fission should automatically do the duplication of references when necessary *)

  (* Part: Parallelization *)
  !^ Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore; cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)

)








(* TODO: insert the right include (using Instr.insert) to eliminate the error
   pic_demo_debug.cpp:616:30: error: use of undeclared identifier 'omp_get_thread_num'
   *)

(* TODO: modify trm_add_mark so that it does not add any mark if the argument is "" *)

(* TODO: generalize Specialize.any so that it takes a trm and not a string as argument.

  TODO: Variable_core.delocalize_aux should take as argument a mark (possibly "", in which case it leaves no mark)
  to annotate all the ANY that it introduces.  (use the modified trm_add_mark for this)

  TODO: add an argument ?(use:trm) to (Variable and Matrix) delocalize to specialize the any on the fly;
  if the argument is "Some t", then pass a fresh mark to delocalize_aux, then call Specialize.any on that mark with the trm t. *)

(* TODO:
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
   instead of generating coefX0 and coefX1  using AddSuffix ${occ}
   we could generate coefX and coefX2       using Pattern (fun i s -> if i = 0 then s else s ^ string_of_int (i+1))
   this would avoid having "0" all around the place for redundant definitions. *)

(* LATER:
  replace
    !^ Variable.inline [main; cVarDef "p"];
    !! Variable.simpl_deref [main];

  with

  !^ Variable.inline ~simpl_deref:true [main; cVarDef "p"];

   if the definition of the variable inlined is of the form &t1, and the simpl_deref flag is activated,
    the transformation should be invoked on the parent path of the variable occurence
    (the operation will automatically be a noop if this parent path does not correspond to a get operation) *)

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

(* LATER  Variable.insert ~const:true
    we should make const:true the default
    and possibly introduce Variable.insert_mut   as a shorthand for   insert ~const:false *)