open Optitrust
open Target
open Ast


let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices
let step = cFunDef "step"
let stepLF = cFunDef "stepLeapFrog"
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Delocalize_arith (Lit_double 0., Binop_add)


let _ = Run.script_cpp ~parser:Parsers.Menhir ~inline:["pic_demo.h";"bag.hc";"particle.hc";"bag_atomics.h";"bag.h-"] (fun () ->

  show [cTopFunDef "main"];

  bigstep "Optimization and inlining of [matrix_vect_mul]";
  let ctx = cTopFunDef "matrix_vect_mul" in
  !! Function.inline [ctx; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
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
  !! Function.inline [step; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Function.inline [stepLF; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Struct.set_explicit [nbMulti; step; cWrite ~typ:"vect" ()];
  !! Struct.set_explicit [nbMulti; stepLF; cWrite ~typ:"vect" ()];

  bigstep "inlining of [cornerInterpolationCoeff] and [accumulateChargeAtCorners]";
  !! Function.inline [nbMulti; cFunDef "cornerInterpolationCoeff"; cFun ~regexp:true "relativePos."];
  !! Function.inline [step; cFun "accumulateChargeAtCorners"];
  !! Function.inline ~vars:(AddSuffix "2") [step; cFun "idCellOfPos"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [nbMulti; step; cFun "cornerInterpolationCoeff"];
  !! Function.inline ~vars:(AddSuffix "${occ}") [stepLF; cFun "cornerInterpolationCoeff"];
  (* !! Variable.elim_redundant [nbMulti; step; cVarDef ~regexp:true "..1"]; *) (* Doesn't this break the code ? *)

  bigstep "Optimization of charge accumulation";
  !! Sequence.intro ~mark:"fuse" ~start:[step; cVarDef "contribs"] ();
  !! Loop.fusion_targets [cMark "fuse"];
  !! Instr.inline_last_write ~write:[sInstr "contribs.v[k] ="]
       [step; sInstr "+= contribs.v[k]"; dRHS];

  bigstep "Low level iteration on chunks of particles";
  !! Sequence.intro ~mark:"loop" ~start:[step; cVarDef "bag_it"] ~nb:2 ();
  !! Sequence.intro_on_instr [step; cMark "loop"; cFor_c ""; dBody]; 
  !! Function_basic.uninline ~fct:[cFunDef "bag_iter_ho_basic"~body:[cVarDef "it"]] [step; cMark "loop"];
  !! Expr.replace_fun "bag_iter_ho_chunk" [step; cFun "bag_iter_ho_basic"];
  !! Function.inline [step; cFun "bag_iter_ho_chunk"]; 
  !! Function.beta ~indepth:true [step];
  !! Variable.init_detach [step; cVarDef "p"];
  !! Instr.inline_last_write ~write:[step; cWrite ~lhs:[cStrictNew; cVar "p"] ()] [nbMulti; step; cRead ~addr:[cStrictNew; cVar "p"] ()]; (**)  (*LATER: does not work, because access operations *)

  !! Sequence.intro ~mark:"loop" ~start:[stepLF; cVarDef "bag_it"] ~nb:2 ();
  !! Sequence.intro_on_instr [stepLF; cMark "loop"; cFor_c ""; dBody]; 
  !! Function_basic.uninline ~fct:[cFunDef "bag_iter_ho_basic"~body:[cVarDef "it"]] [stepLF; cMark "loop"];
  !! Expr.replace_fun "bag_iter_ho_chunk" [stepLF; cFun "bag_iter_ho_basic"];
  !! Function.inline [stepLF; cFun "bag_iter_ho_chunk"]; 
  !! Function.beta ~indepth:true [stepLF];
  !! Variable.init_detach [stepLF; cVarDef "p"];
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
  !! Struct.to_variables [stepLF; cVarDef "fieldAtPos"];
  !! Variable.insert_list ~reparse:true ~defs:(
         ["const double", "factorC", expr "particleCharge * stepDuration * stepDuration / particleMass"]
       @ (map_dims (fun d -> "const double", ("factor" ^ d), expr ("factorC / cell" ^ d))))
     [tBefore; step; cVarDef "field_at_corners"];

  bigstep "Scaling of electric field";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(var ("factor" ^ d)) [step; cVarDef "accel"; cReadVar ("fieldAtPos" ^ d)]); (* ARTHUR: needs compensation *)
  !! Variable.inline [nbMulti; step; cVarDef ~regexp:true "factor."];
  !! Arith.(simpl expand) [nbMulti; step; cVarDef "accel"; cStructInit; cStrict; Arith.constr];

  bigstep "Scaling of speed and positions";
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("stepDuration / cell" ^ d))
         [nbMulti; cFieldReadOrWrite ~field:("speed" ^ d) ()]);
  !! iter_dims (fun d ->
       Accesses.scale ~factor:(expr ("1. / cell" ^ d)) [nbMulti; cFieldReadOrWrite ~field:("pos" ^ d) ()]);
  !! Trace.reparse(); (* required for the terms to be visible to the simplifier *)
  !! Variable.inline [step; cVarDef "accel"];
  !! Arith.(simpl expand) [nbMulti; step; cFun "int_of_double"; dArg 0]; (* TODO: does nothing? *)
  !! Arith.(simpl expand) [nbMulti; step; cVarDef ~regexp:true "r.."; dInit ];
  !! Sequence.apply ~start:[tAfter; step; cWrite ~lhs:[cVar "fieldAtPosZ"]()]
       ~stop:[tAfter; step; cVarDef "contribs"] (fun m ->
       Arith.(simpl expand) [nbMulti; step; cMark m; cWrite(); dRHS; cStrictNew; Arith.constr];);
       (* LATER: Function.use_infix_ops [cMark m] *)
      (* ARTHUR: also missing simplifications in bag_push_concurrent *)

  bigstep "Enumerate grid cells by coordinates";
  !! Label.add "core" [step; cFor "idCell" ~body:[cFor "k"]];
  !! Loop.grid_enumerate (map_dims (fun d -> ("i" ^ d, "grid" ^ d))) [cLabelBody "core"];

)
