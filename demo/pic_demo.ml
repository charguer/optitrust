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
  (* show [cVarDef "particle"]; *)
  !! Function.inline [nbMulti; step; cFun "wrapAround"];
  !! List.iter (fun f -> Struct.inline f [cTypDef "particle"]) ["speed"; "pos"];
  !! Struct.inline "items" [cTypDef "chunk"];

)
