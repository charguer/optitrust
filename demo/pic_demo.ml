open Optitrust
open Target
open Ast

let main = cTopFunDef "main" 

let dims = ["X";"Y";"Z"]
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims

let _ = Run.script_cpp ~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"] (fun () ->

  (* Part: inlining of the bag iteration *) (* skip #1 *) (* ARTHUR *)

  (* Part1: space reuse *)
  !! Variable.reuse ~space_ast:(trm_access (trm_var "p") "speed") [main; cVarDef "speed2"];
     Variable.reuse ~space_ast:(trm_access (trm_var "p") "pos") [main; cVarDef "pos2"];

  (* Part: Introducing an if-statement for slow particles *)
  !! Variable.bind_intro ~fresh_name:"b2" [main; cFun "bag_push"; sExpr "&bagsNext" ];
  !! Flow.insert_if ~cond_ast:(trm_apps (trm_var "ANY_BOOL") []) [main; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_serial" [main; cIf(); dThen; cFun "bag_push"];
     Instr.replace_fun "bag_push_concurrent" [main; cIf(); dElse; cFun "bag_push"];
  !! Function.inline [main; cOr [[cFun "bag_push_serial"]; [cFun "bag_push_concurrent"]]]; (**)
    (* ARTHUR: try to not inline the bag_push operations, but to modify the code inside those functions *)

  (* Part: optimization of vect_matrix_mul *)
  let ctx = cTopFunDef "vect_matrix_mul" in
  !! Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
     Struct.set_explicit [nbMulti; ctx; cWriteVar "res"];
     (* LATER: !! Loop.fission [nbMulti; tAllInBetween; ctx; cFor "k"; cSeq]; *)
     Loop.fission [nbMulti; tAfter; ctx; cFor "k"; cFieldWrite ~base:[cVar "res"] ~regexp:true ~field:"[^z]" ()];
     Loop.unroll [nbMulti; ctx; cFor "k"];
  !! Instr.accumulate ~nb:8 [nbMulti; ctx; sInstrRegexp "res.*\\[0\\]"];
  !! Function.inline [cFun "vect_matrix_mul"];

  (* Part: vectorization of cornerInterpolationCoeff #2 *)
  let ctx = cTopFunDef "cornerInterpolationCoeff" in
  !! Rewrite.equiv_at "double a; ==> a == (0. + 1. * a);" [nbMulti; ctx; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS; cVar ~regexp:true "r."];
  !! Variable.inline [nbMulti; ctx; cVarDef ~regexp:true "c."];
  !! Variable.intro_pattern_array "double coefX, signX, coefY, signY, coefZ, signZ; ==>  double rX, rY, rZ; ==> (coefX + signX * rX) * (coefY + signY * rY) * (coefZ + signZ * rZ);" [nbMulti; cTopFunDef "cornerInterpolationCoeff"; cFieldWrite ~base:[cVar "r"] ~field:""(); dRHS]; (* TODO:
        PatternArray.({ vars = "double ...";
          context = "...";
          pattern = "... " }) *)
          (* ARTHUR: check if with the new parser we could do "double coef_x, sign_x, coef_y, sign_y, coef_z, sign_z;"  and "(coef_x + sign_x * _) * (coef_y + sign_y * _) * (coef_z + sign_z * _);" *)
  !! Loop.fold_instrs ~index:"k" [ctx; sInstr "r.v"];

  (* Part: reveal fields *)
  !! Function.inline [main; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]]; !!!();
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"particle" ()];
  !! Struct.set_explicit [nbMulti; main; cWrite ~typ:"vect" ()];
  !! Variable.inline [cOr [[cVarDef "p2"]; [cVarDef "p"]]];
  !! Struct.to_variables [cVarDef "fieldAtPos"];

  (* Part: optimization of accumulateChargeAtCorners *)
  !! Function.inline [cOr [
       [cFun "vect8_mul"];
       [cTopFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
       [cFun "accumulateChargeAtCorners"]]];
  !! Function.inline ~vars:(AddSuffix "2") [cFun "idCellOfPos"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; cFun "cornerInterpolationCoeff"];
  !! Variable.elim_redundant [nbMulti; cVarDef ~regexp:true "\\(coef\\|sign\\).1"];
  (* !! Instr.(gather_targets ~dest:GatherAtFirst) [nbMulti; cVarDef ~regexp:true ~substr:true "i.0"]; *)
  (* !! Instr.(gather_targets ~dest:GatherAtFirst) [nbMulti; cVarDef ~regexp:true ~substr:true "i.1"]; *)
  
  (* TODO: Fix the issue with fusion_targets *)
  (* Seq.split ~marks:["";"loops"] [cVarDef "coeffs2"];
     Loop.fusion_targets [cMark "loops"; cFor "k"]; ---gather+fusion *)
  !! Instr.(gather_targets ~dest:(GatherAt [tBefore; cVarDef "coeffs2"])) [main;cVarDef ~regexp:true "\\(delta\\|indice\\)."];
  !! Loop.fusion ~nb:3 [main; cFor "k" ~body:[sInstr "coeffs2.v[k] ="]];

  (* TODO:  if the read is on an access  P  then search above in the same trm_seq  for a write at P
          (when the write argument is not provided) *)
!!! Instr.inline_last_write ~delete:true ~write:[sInstr "coeffs2.v[k] ="] [main; cRead ~addr:[sExpr "coeffs2.v"] ()]; 
    Instr.inline_last_write ~delete:true ~write:[sInstr "deltaChargeOnCorners.v[k] ="] [main; cRead ~addr:[sExpr "deltaChargeOnCorners.v"] ()];

  (* Part: AOS-SOA *)
  !! Struct.inline "speed" [cTypDef "particle"];
     Struct.inline "pos" [cTypDef "particle"];

  (* Part: scaling of speeds and positions #7 *)
   !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factor", "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), ("factor / cell" ^ d))))
     [tBefore; cVarDef "nbSteps"];

  (* Part: scaling of field, speeds and positions *)
  !! iter_dims (fun d ->
       Accesses.scale ~factor_ast:(Ast.trm_var ("factor" ^ d)) [cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [cOr [[cVarDef "accel"]; [cVarDef ~regexp:true "factor."] (*; [cVarDef "factor"]*)]];
  !!! Variable.inline [cVarDef "factor"]; (* TODO: see why occurrence not found on the previous line *)
  (* LATER: variable.inline_at which takes only the occurrence and finds automatically the source *)
  !! iter_dims (fun d ->
       Accesses.scale (* TODO ~reparse:false *) ~factor:("stepDuration / cell" ^ d) [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale (* TODO ~reparse:false *) ~factor:("1. / cell" ^ d) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  (* TODO: do the reparse here, only once *)


  (* Part: grid_enumeration *)
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [cFor "idCell" ~body:[cWhile ()]]; (* TODO: add a label on this loop "main_loop" at the very first step *)

  (* Part: ARTHUR ; maybe not needed: !! iter_dims (fun d ->
    Instr.inline_last_write ~write:[cWriteVar ("fieldAtPos" ^ d)] [nbMulti; cRead ~addr:[cVar ("fieldAtPos" ^ d)] ()]); *)
  (* TODO :ARTHUR : see how to inline the zero for fieldatpos in the simplest way *)
  (* !! Variable.inline [cVarDef ~regexp:true "fieldAtPos."]; *)

  (* Part: Introduce names for new positions *)
  !! iter_dims (fun d ->
      Variable.bind_intro ~fresh_name:("p" ^ d) [cFor "i"; cStrict; cFieldWrite ~field:("pos"^d) (); dRHS]);
  !! Instr.(gather_targets ~dest:GatherAtFirst) [main; cVarDef ~regexp:true "p."]; (* TODO: fix order of gather at first *) (* TODO: probably should add ~substr:false to be more robust; or do "p[x-z]" as regexp *)

  (* Part: Make positions relative, and convert sortage to float *)
  !! iter_dims (fun d ->
    Accesses.shift (* TODO: neg:true instead of minus *) (*~factor_ast:(Ast.trm_var ("i" ^  d))*) ~factor:("- i"^d) [cVarDef ("p" ^ d); cRead ~addr:[sExpr ("(c->items)[i].pos" ^ d )] ()]);
       (* TODO: above the prototype so that we can write:  Access.shift ~get:true (var ("i"^d)) [cVarDef ...; ...] *)
  !! iter_dims (fun d ->
    Accesses.shift (* TODO: neg:true *) ~factor_ast:(Ast.trm_var ("i" ^ d ^ "2")) [cWrite ~lhs:[sExpr ("(c->items)[i].pos"^d)] () ]);
  !! Cast.insert ~typ_ast:(Ast.typ_float ()) [sExprRegexp ~substr:true "\\(p. \\+ i.\\)"];
  (* Above, change to:  Cast.insert (Ast.typ_float()) ,   and in the future we will be able to write equivalently:   Cast.insert (atyp "float"). *)
  (* TODO:   double posX;
             double posY;
             double posZ;
        - Typdef.change_fields "float" [map_dims (fun d -> "pos"^d)] [cTypdef "particle"]
    *)

  (* TODO: replace Ast.trm_var with just [var] everywhere;
      to that end, in ast.ml, we define a module [AstParsers] with definitions such as
         [let var = Ast.trm_var] and [let expr = Ast.code] etc...
      and then in [target.ml] we can do [include AstParser], so that we don't need to do [open AstParser] in each file. *)
  (* Part: duplication of corners for vectorization of change deposit *)
  !! Matrix.intro_mops (Ast.trm_var "nbCells") [main;cVarDef "nextCharge"];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1;main; cFor "k"]; (* TODO: place a label earlier on, on the relevant loop *)
  (* TODO: read_last_write   on  .. = nextCharge[MINDEX1(nbCells, idCell)]    to become  .. = 0 *)
  (* TODO: below Lit_int 0 should be Lit_double 0 *)
  (* TODO: put in some library:   let delocalize_double_add = Delocalize_arith (Lit_double 0, Binop_add)
     then use [delocalize_double_add] here and further on as argument *)
  !! Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add)) [cMark "first_local"]; (* TODO: ~init_zero:true
       so no need to generate nextChargeCorners[MINDEX2(nbCorners, nbCells, 0, idCell)] = nextCharge[MINDEX1(nbCells, idCell)]; *)
  !! Variable.inline [main; cVarDef "indices"];
  !! Specialize.any "k" [cAny];
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
  !! Sequence.insert (Ast.code my_bij_code) [tBefore; main];
  !! Matrix.biject "mybij" [occIndex 0; main; cFor "k"; cFun "MINDEX2"]; (* TODO: target should be  cellReadOrWrite ~base:"nextChargeCorners"  ->  on the base argument of the read/write -> check it is a mindex_ then replace it *)
  !! Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextCharge["]];  (* TODO:  cLabel "initNextCharge"  ;  assuming ~labels:["initNextCharge",""] to be given to delocalize on nextCharnge *)
  !! Instr.replace (Ast.code "MINDEX2(nbCells, nbCorners, idCell2, k)") [cFun "mybij"]; (* ARTHUR: fixed when the rest is updated *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)
  !! Variable.insert ~name:"nbProcs" ~typ:"int" ~value:"8" [tBefore; main];
  !! Matrix.local_name ~my_mark:"first_local" ~var:"nextCharge" ~local_var:"nextChargeCorners" ~indices:["idCell"] [occIndex 1; main; cFor "k"]; (* TODO: use a label that should be on that loop *)
     Matrix_basic.delocalize ~dim:(Ast.trm_var "nbCorners") ~index:"k" ~acc:"sum" ~ops:(Delocalize_arith (Lit_int 0, Binop_add))[cMark "first_local"];
     Instr.delete [occIndex 0; cFor "idCell" ~body:[sInstr "nextChargeCorners["]]; (* TODO: use a label that should be on that loop, introduced by the earlier delocalize *)
     Specialize.any "k" [cAny]; (* this should be specialized not to k but to [myThread] *)

(* TODO: capitalize rest of the script *)

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
  !! Sequence.intro ~mark:"temp_seq" ~start:[main; cVarDef "coef_x0"] ~nb:6 (); (* TODO: replace 6 with (2*nb_dims),  where let nb_dims = List.length dims should be define at the top of the file *)
     Instr.move_invariant ~dest:[tBefore; main] [cMark "temp_seq"]; (* TODO: rename "move_invariant" to "move_out" *)
     Sequence.elim [cMark "temp_seq"]; (* TODO: rename "temp_seq" to "coefs" *)
     (* TODO: move_invariant would often apply to sequences, thus we could add an optional argument ?(elim_seq:bool=false) to perform the sequence elimination on the fly *)
  !! Loop.fission [tBefore; cVarDef "px"];
     Loop.fission [tBefore; main; cVarDef "ix"];
     Loop.hoist [cVarDef "idCell2"]; (* TODO: hoisting before fission *)

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

  !! Variable.insert_list ~defs:[("int","blockSize","2"); ("int","2","blockSize / 2")] [tBefore; cVarDef "nbCells"]; (* TODO: put in the form ~defs[("int", ...] *)
      (* TODO: "2","blockSize / 2" does not seem right, because "2" is not a variable name...was it d? *)
     Variable.insert ~typ:"bool" ~name:"distanceToBlockLessThanHalfABlock" ~value:"(ix >= bix - d && ix < bix + blockSize + d)&& (iy >= biy - d && iy < biy + blockSize + d) && (iz >= biz - d && iz < biz + blockSize + d)" [tAfter; main; cVarDef "iz"];
     (* TODO  assume "d" is rename to "dist";  then we can make above shorter:
         Variable.insert (Ast.trm_ands (map_dims (fun d -> expr ~vars:[d] "(i${1} >= bi${1} - dist && i${1} < bi${1} + blockSize + dist)"))))
         where the "value" argument needs not use a label since it has type trm directly
         and where trm_ands is a smart construction for building a conjunction from a list of terms *)
     Instr.replace (Ast.trm_var "distanceToBlockLessThanHalfABlock") [cFun "ANY_BOOL"];


  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["idCell"]] [nbMulti; tBefore;cFor "idCell" ~body:[sInstr "sum +="]];
     Omp.parallel_for [Shared ["bX";"bY";"bZ"]] [tBefore; cFor "biX"];

  (* Part: optimize chunk allocation *)  (* ARTHUR *)
  (* skip #16 *)



)

