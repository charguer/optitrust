open Optitrust
open Target
open Ast
open String

(* Parameters from the command line:
   "-usechecker": for enabling checker code
   "-usesingle": for enabling single precision of positions *)
let usechecker = ref false
let usesingle = ref false
let _= Run.process_cmdline_args
  [("-usechecker", Arg.Set usechecker, " use -DCHECKER as preprocessor flag");
   ("-usesingle", Arg.Set usesingle, " make positions single precision")]
let usechecker = !usechecker
let usesingle = !usesingle
let onlychecker p = if usechecker then [p] else []
let _ = (* Print the values of the flags passed *)
  if usesingle && usechecker then failwith "-usingle and -usechecker are incompatible";
  Printf.printf "CHECKER=%d\n" (if usechecker then 1 else 0);
  Printf.printf "SINGLE=%d\n" (if usesingle then 1 else 0)

(* Other parameters  *)
let align = 64
let grid_dims_power_of_2 = true

(* Short names for top level functions *)
let step = cTopFunDef "step"
let stepLF = cTopFunDef "stepLeapFrog"
let repPart = cTopFunDef "reportParticlesState"
let addPart = cTopFunDef "addParticle"
let stepsl = [stepLF; step]
let stepFuns = (if usechecker then [repPart] else []) @ stepsl
let stepsReal = cOr (List.map (fun f -> [f]) stepsl)
let steps = cOr (List.map (fun f -> [f]) stepFuns)

(* Operations for iterating over dimensions *)
let dims = ["X"; "Y"; "Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)

(* Definition of the monoids used for the "delocalize" operations *)
let delocalize_sum = Local_arith (Lit_double 0., Binop_add)
let delocalize_bag = Local_obj ("bag_init", "bag_append", "bag_free")

(* Part 0: parsing the input files, with inlining of a specific subset of the auxiliary headers and sources *)

let prepro = onlychecker "-DCHECKER"
let prepro = ["-DPRINTPERF"; "-DPRINTSTEPS"] @ prepro
let _ = Run.script_cpp ~parser:Parsers.Menhir ~prepro ~inline:["pic_demo.h";"bag.hc";"particle.hc";"optitrust.h";"bag_atomics.h";"bag.h-"] (fun () ->

  (* Part 1: sequential optimizations *)

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Function.inline [ctx; cFuns ["vect_mul"; "vect_add"]];
  !! Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
  !! Loop.fission ~split_between:true [ctx; cFor "idCorner"];
  !! Loop.unroll [nbMulti; ctx; cFor "idCorner"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp ~substr:true "res.*\\[0\\]"];
  !! Function.inline ~delete:true [nbMulti;cFun "matrix_vect_mul"];

  bigstep "Optimization in [cornerInterpolationCoeff] debug";
  let ctx = cTopFunDef "cornerInterpolationCoeff" in
  !! Rewrite.equiv_at "double a; ==> 1. - a == (1. + (-1.) * a)" [nbMulti; ctx; cVarDefReg "c."; dVarInit];
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a)" [nbMulti; cWrite(); cVarReg "r[X-Z]"];
  !! Variable.inline [nbMulti; ctx; cVarDefReg "c."];
  !! Variable.intro_pattern_array ~const:true ~pattern_aux_vars:"double rX, rY, rZ"
      ~pattern_vars:"double coefX, signX, coefY, signY, coefZ, signZ"
      ~pattern:"(coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ)"
      [nbMulti; ctx; cWrite(); dRHS];
  !! Instr.move ~dest:[tBefore; ctx] [nbMulti; ctx; cVarDefReg "\\(coef\\|sign\\)."];
  !! Loop.fold_instrs ~index:"idCorner" [ctx; cWrite()];

  bigstep "Eliminate an intermediate storage by reusing an existing one";
  !! Variable.reuse (expr "p->speed") [step; cVarDef "speed2" ];
  !! Variable.reuse (expr "p->pos") [step; cVarDef "pos2"];
  !! Trace.reparse();

  bigstep "reveal_field write operations involved in the manipulation of particles and vectors";
  !! Function.inline [steps; cFuns ["vect_mul"; "vect_add"]];
  let tg = cOr [[steps]; [cTopFunDefReg "bag_push_.*"]] in
  !! List.iter (fun typ -> Struct.set_explicit [nbMulti; tg; cWrite ~typ ()]) ["particle"; "vect"];
  !! Function.inline ~delete:true ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "wrapArea"];
  !! Variable.inline [nbMulti; step; cVarDefReg "[xyz]."];

  bigstep "Inlining of [cornerInterpolationCoeff] and [accumulateChargeAtCorners]";
  !! Function.inline [nbMulti; cTopFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
  !! Function.inline [step; cFun "accumulateChargeAtCorners"];
  !! Function.inline ~vars:(AddSuffix "2") [step; cFun "idCellOfPos"];
  !! List.iter (fun f -> Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; f; cFun "cornerInterpolationCoeff"])
     stepsl;
  !! iter_dims (fun d -> Variable.reuse (var ("i" ^ d ^ "2")) [step; cVarDef ("i" ^ d ^ "1")]);
  !! Trace.reparse();

  bigstep "Simplification of the deposit of charge";
  !! Sequence.intro ~mark:"fuse" ~start:[step; cVarDef "contribs"] ();
  !! Loop.fusion_targets [cMark "fuse"];
  !! Instr.inline_last_write [step; cCellRead ~base:[cFieldRead ~base:[cVar "contribs"] ()] ()];

  bigstep "Low level iteration on chunks of particles";
  !! Function.inline [steps; cFuns ["bag_iter_begin"; "bag_iter_destructive_begin"]];
  !! Loop.change_iter ~src:"bag_iter_ho_basic" ~dst:"bag_iter_ho_chunk" [steps; cVarDef "bag_it"];
  !! Instr.delete [nbMulti; cTopFunDefAndDeclReg "bag_iter.*"];

  bigstep "Elimination of the pointer on a particle, to prepare for aos-to-soa";
  !! Instr.inline_last_write [nbMulti; steps; cReadVar "p"];

  bigstep "Preparation for AOS-TO-SOA";
  !! Struct.set_explicit [step; cVarDef "p2"];
  !! Struct.set_explicit [nbMulti; step; cFieldWrite ~base:[cVar "p2"] ~regexp:true ~field:"\\(speed\\|pos\\)" ()];

  bigstep "AOS-TO-SOA";
  !! Struct.reveal_fields ["speed"; "pos"] [cTypDef "particle"];
  !! Struct.reveal_field "items" [cTypDef "chunk"];

  bigstep "Apply scaling factors on the electric field";
  !! Struct.to_variables [steps; cVarDef "fieldAtPos"];
  !! Variable.insert_list_same_type ~reparse:true (ty "const double") (["factorC", expr "particleCharge * stepDuration * stepDuration / particleMass"]
      @ (map_dims (fun d -> ("factor" ^ d, expr ("factorC / cell" ^ d))))) [tBefore; steps; cFor "idCell" ~body:[cFor "i"]];
  !! Function.inline ~delete:true [steps; cFun "getFieldAtCorners"];
  !! Variable.rename ~into:"field_at_corners" [step; cVarDef "res"];
  !! Struct.set_explicit [steps; cFor "idCorner"; cCellWrite ~base:[cFieldRead ~base:[cVar "field_at_corners"] ()] ()];
  !! iter_dims (fun d ->
      Accesses.scale ~factor:(var ("factor" ^ d)) [steps; cFor "idCorner"; cFieldWrite ~field:(lowercase_ascii d) ()];
      Accesses.scale ~factor:(var ("factor" ^ d)) [steps; cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]);
  !! Variable.unfold [step; cVarDef  "factorC"];
  !! Variable.unfold ~at:[cVarDef "accel"] [nbMulti; step; cVarDefReg "factor."];
  !! Arith.(simpl_rec expand) [nbMulti; steps; cVarDef "accel"];

  bigstep "Applying a scaling factor on speeds";
  !! Struct.set_explicit [addPart; cVarDef "p"];
  !! iter_dims (fun d ->
      Accesses.scale ~factor:(expr ("(cell"^d^"/stepDuration)")) [addPart; cFieldRead ~field:(lowercase_ascii d) ~base:[cVar "speed"] ()];
      Accesses.scale ~factor:(expr ("(stepDuration / cell"^d^")"))
      [nbMulti; steps; cFieldWrite ~base:[cVar "c"] (); sExprRegexp ~substr:true ("c->itemsSpeed" ^ d ^ "\\[i\\]")]);
  if usechecker then (!! iter_dims (fun d ->
        Accesses.scale ~inv:true ~factor:(expr ("(cell"^d^"/stepDuration)")) [repPart; cVarInit ("speed"^d)]));

  bigstep "Applying a scaling factor on positions";
  !! iter_dims (fun d ->
      Accesses.scale ~factor:(var_mut ("cell"^d)) [addPart; cFieldRead ~field:(lowercase_ascii d) ~base:[cVar "pos"] ()];
      Accesses.scale ~inv:true ~factor:(var_mut ("cell"^d)) [nbMulti; steps; cOr [
        [sExprRegexp ("c->itemsPos" ^ d ^ "\\[i\\]")];
        [cFieldWrite ~field:("pos"^d)()]]]);
  !! Trace.reparse();

  bigstep "Simplify arithmetic expressions after scaling";
  !! Variable.inline [steps; cVarDef "accel"];
  !! Arith.with_nosimpl [nbMulti; steps; cFor "idCorner"] (fun () ->
       Arith.(simpl_rec expand) [nbMulti; steps]);

  bigstep "Enumerate grid cells by coordinates";
  let tg = cTarget [step; cFor "idCell" ~body:[cFor "i"]] in
  !! Instr.read_last_write ~write:[cWriteVar "nbCells"] [tg; dForStop; cReadVar "nbCells"];
  !! Loop.grid_enumerate ~indices:(map_dims (fun d -> "i"^d)) [step; cFor "idCell" ~body:[cFor "idCorner"]];

  bigstep "Code cleanup in preparation for shifting of positions";
  !! iter_dims (fun d ->
      Variable.bind ~const:true ~typ:(Some (ty "double")) ("p" ^ d ^ "2") [occLast;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS];
      Variable.bind ~const:true ("p" ^ d ) ~typ:(Some (ty "double")) [occFirst;step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] (); dRHS]);
  !! iter_dims (fun d ->
      Instr.inline_last_write [step; cVarDefReg "p.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);
  !! iter_dims (fun d ->
      Instr.read_last_write [step; cVarDefReg "i.2"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [step; cVarDefReg ("\\(i.*2\\|p.2\\|i.2\\)")];
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cVarDef "p2"])) [step; cVarDefReg "r.1"];

  bigstep "Shifting of positions: make positions relative to the containing cell";
  !! Instr.move ~dest:[tBefore; addPart; cVarDef "p"] [addPart; cVarDef "idCell"];
  !! List.iter (fun tg ->
      Variable.insert ~typ:(ty "coord") ~name:"co" ~value:(expr "coordOfCell(idCell)") tg)
      ([ [tAfter; addPart; cVarDef "idCell"] ] @ onlychecker [tFirst; repPart; cFor "idCell"; dBody]);
  !! iter_dims (fun d ->
      Accesses.shift ~inv:true ~factor:(expr ("co.i"^d)) [cOr (
        [[addPart; cFieldWrite ~field:("pos"^d) ()]] @
        (onlychecker [repPart; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()] ()]) )] );
  !! iter_dims (fun d ->
      Accesses.shift ~inv:true ~factor:(var ("i" ^ d ^ "0")) [stepsReal; cVarDefReg "r.0"; cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~inv:true ~factor:(var ("i" ^ d)) [step; cVarDefReg ("p" ^ d); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~inv:true ~factor:(var ("i" ^ d ^ "2")) [step; cCellWrite ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()];
      Accesses.shift ~inv:true ~factor:(var ("i" ^ d ^ "2")) [step; cVarDef ("r" ^ d ^ "1"); cCellRead ~base:[cFieldRead ~field:("itemsPos" ^ d) ()]()]);

  bigstep "Simplify arithmetic expressions after shifting of positions";
  !! Rewrite.equiv_at ~ctx:true "double x, y, z; ==> (fwrap(x,y)/z) == (fwrap(x/z, y/z))" [cVarDefReg "p.2"; dVarInit];
  !! iter_dims (fun d ->
      Instr.read_last_write ~write:[cTopFunDef "computeConstants"; cWriteVar ("cell"^d)] [nbMulti;step; cFun "fwrap";cReadVar ("cell"^d)];);
  !! Arith.with_nosimpl [nbMulti; stepsReal; cFor "idCorner"] (fun () ->
       Arith.(simpl_rec expand) [nbMulti; stepsReal]);
  !! Variable.inline [stepsReal; cVarDefReg "r.."];
  !! Instr.delete [nbMulti; stepsReal; cVarDefReg "i.0"];

  if usesingle then begin
    bigstep "Turn positions into floats, decreasing precision but allowing to fit a larger number of particles";
    !! Cast.insert (ty "float") [sExprRegexp ~substr:true "p.2 - i.2"];
    !! Struct.update_fields_type "itemsPos." (ty "float") [cTypDef "chunk"];
  end;

  bigstep "Replacement of the floating-point wrap-around operation with an integer wrap-around";
   let fwrapInt = "double fwrapInt(int m, double v) {
      const int q = int_of_double(v);
      const double r = v - q;
      const int j = wrap(m, q);
      return j + r;
    }" in
  !! Function.insert ~reparse:true fwrapInt [tBefore; step];
  !! Expr.replace_fun "fwrapInt" [nbMulti; step; cFun "fwrap"];
  !! iter_dims (fun d ->
      Function.inline ~vars:(AddSuffix d) [step; cVarDef ("p"^d^"2"); cFun "fwrapInt"]);
  if grid_dims_power_of_2 then
    !! Rewrite.equiv_at ~ctx:true "int a, b; ==> wrap(a,b) == (b & (a -1))" [nbMulti; step; cFun "wrap"];

  bigstep "Simplification of computations for positions and destination cell";
  !! iter_dims (fun d -> Expr_basic.replace (var ("j"^d)) [step; cVarInit ("i"^d^"2")];);
  !! Variable.inline_and_rename [nbMulti; step; cVarDefReg "i.2" ];
  !! Variable.inline [nbMulti; step; cVarDefReg "p.2"];
  !! Arith.(simpl_rec expand) [nbMulti; step; cCellWrite ~base:[cFieldRead ~regexp:true ~field:("itemsPos.") ()] ()];

  bigstep "Introduce matrix operations, and prepare loop on charge deposit";
  !! Label.add "core" [step; cFor "iX" ];
  !! Matrix.intro_mops (var_mut "nbCells") [nbMulti; cVarDefs ["deposit"; "bagsNext"]];
  !! Label.add "charge" [step; cFor "idCorner" ~body:[cVar "deposit"]];
  !! Variable.inline [occLast; step; cVarDef "indices"];

  bigstep "Duplicate the charge of a corner for the 8 surrounding cells";
  let alloc_instr = [cTopFunDef "allocateStructures"; cWriteVar "deposit"] in
  !! Matrix.delocalize "deposit" ~into:"depositCorners" ~last:true ~indices:["idCell"] ~init_zero:true
     ~labels:["alloc"; ""; "dealloc"] ~dealloc_tg:(Some [cTopFunDef ~regexp:true "dealloc.*"; cFor ""])
     ~dim:(var "nbCorners") ~index:"idCorner" ~acc:"sum" ~ops:delocalize_sum ~use:(Some (var "idCorner")) ~alloc_instr [cLabel "core"];

  bigstep "Apply a bijection on the array storing charge to vectorize charge deposit";
  let bij_def =
      "int bij(int nbCells, int nbCorners, int idCell, int idCorner) {
        coord coord = coordOfCell(idCell);
        int iX = coord.iX;
        int iY = coord.iY;
        int iZ = coord.iZ;
        int bijection[8] = {
          cellOfCoord(iX, iY, iZ),
          cellOfCoord(iX, iY, wrap(gridZ,iZ-1)),
          cellOfCoord(iX, wrap(gridY,iY-1), iZ),
          cellOfCoord(iX, wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
          cellOfCoord(wrap(gridX,iX-1), iY, iZ),
          cellOfCoord(wrap(gridX,iX-1), iY, wrap(gridZ,iZ-1)),
          cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), iZ),
          cellOfCoord(wrap(gridX,iX-1), wrap(gridY,iY-1), wrap(gridZ,iZ-1)),
        };
      return MINDEX2(nbCells, nbCorners, bijection[idCorner], idCorner);
      }" in
  !! Function.insert bij_def [tBefore; step];
  !! Matrix.biject "bij" [cVarDef "depositCorners"];
  !! Expr.replace ~reparse:false (expr "MINDEX2(nbCells, nbCorners, idCell2, idCorner)")
      [step; sExpr "bij(nbCells, nbCorners, indicesOfCorners(idCell2).v[idCorner], idCorner)"];

  (* Part 2: parallelization *)

  bigstep "Decompose the loop to allow for parallelization per blocks";
  !! Variable.insert_list_same_type (ty "const int") [("block", lit "2"); ("halfBlock", (lit "1"))] [tBefore; cVarDef "nbCells"];
  !! iter_dims (fun d -> let index = "b"^d in
      Loop.tile (var_mut "block") ~bound:TileBoundDivides ~index [step; cFor ("i"^d)];
      Loop.color (lit "2") ~index:("c"^d) [step; cFor index] );
  !! Loop.reorder ~order:Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ idims) [step; cFor "cX"];
  (* TODO: Fix thes issue with label passing *)
  !! Label.add "core" [step; cFor "cX"];
  !! Expr.replace_fun "bag_push_concurrent" [step; cFun "bag_push"];
  !! Instr.set_atomic [step; cLabel "charge"; cWrite ()];
  !! Omp.parallel_for ~collapse:3 [step; cFor "bX"];

  bigstep "Introduce nbThreads and idThread";
  !! Omp.header ();
  !! Variable.insert ~const:false ~name:"nbThreads" ~typ:(ty "int") [tBefore; cVarDef "nbCells"];
  !! Omp.get_thread_num "idThread" [step; cFor "iX"];
  !! Omp.set_num_threads ("nbThreads") [tFirst; cTopFunDef "main"; dBody];
  !! Trace.reparse();

  bigstep "Duplicate the charge of a corner for each of the threads";
  let alloc_instr = [cTopFunDef "allocateStructures"; cWriteVar "depositCorners"] in
  !! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var_mut "nbThreads") ~index:"idThread" ~acc_in_place:true ~ops:delocalize_sum ~use:(Some (var "idThread"))
      ~labels:["alloc"; ""; "dealloc"] ~alloc_instr ~dealloc_tg:(Some [cTopFunDef ~regexp:true "dealloc.*"; cFor ""])
      [cLabel "core"];
  !! Instr.delete [cFor "idCell" ~body:[cCellWrite ~base:[cVar "depositCorners"] ~rhs:[cDouble 0.] ()]];
  !! Instr.unset_atomic [step; cLabel "charge"; cOmp()];

  bigstep "Introduce private and shared bags, and use shared ones only for particles moving more than one cell away";
  !! Matrix.delocalize "bagsNext" ~into:"bagsNexts" ~dim:(lit "2") ~indices:["idCell"] ~last:true
    ~alloc_instr:[cTopFunDef "allocateStructures"; cWriteVar "bagsNext"]
    ~labels:["alloc"; ""; "dealloc"] ~dealloc_tg:(Some [cTopFunDef ~regexp:true "dealloc.*"; cFor ""])
    ~index:"bagsKind" ~ops:delocalize_bag [cLabel "core"];
  !! Variable.insert_list_same_type (ty "const int") [("PRIVATE", lit "0"); ("SHARED", lit "1")] [tBefore; step];
  !! Instr.delete [step; cFor "idCell" ~body:[cFun "bag_swap"]];
  !! Variable.exchange "bagsNext" "bagsCur" [nbMulti; step; cFor "idCell"];
  !! Instr.delete [cOr[[cVarDef "bagsNext"];[cWriteVar "bagsNext"];[cFun ~regexp:true "\\(free\\|bag.*\\)" ~args:[[cVar "bagsNext"]]]]];
  !! Variable.insert ~typ:(ty "coord") ~name:"co" ~value:(expr "coordOfCell(idCell2)") [tAfter; step; cVarDef "idCell2"];
  let pushop = cFun "bag_push_concurrent" in
  let push_cond = trm_ands (map_dims (fun d ->
         expr ~vars:[d] "(co.i${0} >= b${0} - halfBlock && co.i${0} < b${0} + block + halfBlock)
                      || (b${0} == 0 && co.i${0} >= grid${0} - halfBlock)
                      || (b${0} == grid${0} - block && co.i${0} < halfBlock)")) in
  !! Variable.insert ~typ:(ty "bool") ~name:"isDistFromBlockLessThanHalfABlock"
      ~value:push_cond [tBefore; step; pushop];
  !! Flow.insert_if ~cond:(var "isDistFromBlockLessThanHalfABlock") [step; pushop];
  !! Specialize.any (var_mut "PRIVATE") [step; cIf(); dThen; pushop; cAny];
  !! Specialize.any (var_mut "SHARED") [step; cIf(); dElse; pushop; cAny];
  !! Expr.replace_fun "bag_push_serial" [step; cIf(); dThen; pushop];
  !! Trace.reparse();

  bigstep "Parallelize and optimize loops that process bags";
  !! Loop.fusion ~nb:2 [step; cFor "idCell" ~body:[cFun "bag_append"]];
  !! Omp.parallel_for [nbMulti;stepsReal; cFor "idCell"];
  !! Function.use_infix_ops ~indepth:true [step; dBody];

  (* Part 4: Vectorization *)

  bigstep "Loop splitting: process speeds, process positions, deposit particle and its charge";
  !! Loop.unroll [occFirst; step; cFor "i"; cFor "idCorner"];
  !! Loop.unroll [stepLF; cFor "i"; cFor "idCorner"];
  !! Instr.inline_last_write [nbMulti; steps; cCellRead ~base:[cFieldRead ~base:[cVar "coeffs"] ()] ()];
  !! iter_dims (fun d ->
       Instr.inline_last_write [steps; cCellWrite ~base:[cFieldRead ~field:("itemsSpeed"^d) ()] (); cReadVar ("fieldAtPos"^d)];);
  !! Variable.inline [nbMulti; steps; cVarDefReg "fieldAt.*"];
  !! Arith.with_nosimpl [nbMulti; stepsReal; cFor "idCorner"] (fun () ->
       Arith.(simpl_rec expand) [nbMulti; stepsReal; cFor "i"]);
  !! Variable.to_nonconst [step; cVarDef "idCell2"];
  !! Loop.hoist ~array_size:(Some (var "CHUNK_SIZE")) [step; cVarDef "idCell2"];
     let dest = [tBefore; step; cVarDef "isDistFromBlockLessThanHalfABlock"] in
  !! Instr.copy ~dest [step; cVarDef "idCell2"];
  !! Instr.move ~dest [step; cVarDef "co"];
  !! Loop.fission [nbMulti; tBefore; step; cOr [[cVarDef "pX"]; [cVarDef "p2"]]];
  !! Variable.inline [nbMulti; step; cVarDef "idCell2"];
  !! Instr.delete [step; cVarDef "contribs"];

  bigstep "Data alignment";
  !! Align.def (lit "64") [nbMulti; cOr [[cStrict; cVarDefReg "\\(coef\\|sign\\)."];
                                         [step; cVarDef "idCell2_step"];
                                         [cStrict; cVarDef ~substr:true "deposit"]]];
  !! Struct.align_field (lit "64") ("items.") [cTypDef "chunk"];
  !! Function.inline [step; cFun "cellOfCoord"];
  !! Align.alloc (lit "64") [nbMulti; cTopFunDef "allocateStructures"; cMalloc ()];

  bigstep "Function inlining on loops to be vectorized";
  let ctx = cFor "idCorner" ~body:[cVar "depositCorners"] in
  !! Function.bind_intro ~fresh_name:"temp_var_${occ}" [nbMulti; ctx; cMindex ()];
  !! Function.inline [nbMulti; ctx; cMindex ()];
  !! Variable.inline [nbMulti; ctx; cVarDefReg "temp_var_."];

  bigstep "Vectorization";
  !! Align.header ();
  !! Loop.fission [tBefore; occLast; step; cFor "idCell"; cFor "idCorner"; cFor "idThread"];
  !! Loop.swap [occLast; cFor "idCell"; cFor "idCorner" ~body:[cFor "idThread"]];
  !! Omp.simd [nbMulti; cOr [
    [cFor "idCell" ~body:[cFor "bagsKind"]; cFor "idCorner"];
    [stepLF; cFor "i"];
    [cDiff [[step; cFor "i"]] [[step; cFor "i"  ~body:[cFor "idCorner"]]] ];
    ]];
  !! Omp.simd ~clause:[Aligned (["coefX"; "coefY"; "coefZ"; "signX"; "signY"; "signZ"], align)] [step; cLabel "charge"];
  !! Label.remove [step; cLabel "charge"];

)
