open Optitrust
open Target

let _ = Run.script_cpp ~inline:["particle.h";"particle_chunk_alloc.h";"particle_chunk.h"] (fun () ->

  (* Part: inlining of the bag iteration *)
  (* skip #1 *)


  (* Part: vectorization of cornerInterpolationCoeff #2
  Rewrite.equiv_at "double a; a ==> (0. + 1. * a)" [cFun "cornerInterpolationCoeff"; cReturn; cVar ~regexp:true "r."];
*)


  (* Part: optimize chunk allocation *)
  (* skip #16 *)

  (* PART: Inlining of arithmetic operations *)
  (* TODO: the intermediate names should be inserted then inlined automatically *)

  (* LATER: !! Function.bind_intro ~fresh_name:"r${occ}" ~const:true [nbMulti; cFun "vect_mul"]; *)
  !! Function.bind_intro ~fresh_name:"r0" ~const:true [cFunDef "vect_matrix_mul"; cFun "vect_mul"];
    !!!();
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [tIndex ~nb:3 0; cFunDef "main"; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:3 1; cFunDef "main"; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r3" ~const:true [tIndex ~nb:3 2; cFunDef "main"; cFun "vect_mul"];
  !! Function.inline [cFunDef "main"; cOr [[cFun "vect_mul"]]];
  (*!! Function.bind_intro [cFunDef "vect_matrix_mul"; cFun "vect_add"; dArg 2]; *)
  !! Function.inline [cFunDef "main"; cOr [[cFun "vect_add"]]];
  !! Variable.inline [nbMulti; cFunDef "main"; cVarDef"accel"];
  !!! Variable.inline [nbMulti; cFunDef "main"; cVarDef"r1"];

  (* Part: Naming the target bag *)
  !! Function.inline ~args:["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Variable.insert_and_fold "k" "int&" "b2.nb" [tAfter; cVarDef "b2"];

  (* Part: Inlining of structure assignements *)
  !! Struct.set_explicit [nbMulti; cOr [[cVarDef "speed2"]; [cVarDef "pos2"]]];
  !! Function.inline [cFunDef "bag_transfer"; cFun "bag_push"];
  !!! Struct.set_explicit [nbMulti;cWrite ~typ:"particle"()];
  !!! Struct.set_explicit [nbMulti;cWrite ~typ:"vect"()];
  !!! Variable.inline [cOr [[cVarDef "p"]; [cVarDef "p2"]]];

  (* Part: AOS-TO-SOA *)
  !!! Struct.inline "pos" [cTypDef "particle"];
  !!! Struct.inline "speed" [cTypDef "particle"];
  !!! Struct.inline "items" [cTypDef "bag"];



  (* Part: space reuse for storing updated speeds and positions #5 *)

  (* Part: optimization of computation of speeds #6 *)

  (* Part: scaling of electric field -- LATER ARTHUR: check if we need this *)

  (* Part: scaling of speeds and positions #7 *)

  (* Part: shifting of positions #8 #9 *)

  (* Part: optimization of accumulateChargeAtCorners #4 #10 *)

  (* Part: introduction of matrix macros #12 *)

  (* Part: duplication of corners for vectorization of charge deposit #13 *)

  (* Part: duplication of corners for thread-independence of charge deposit #14 *)

  (* Part: loop splitting for treatments of speeds and positions and deposit #11 *)

    (* TODO: adapt the script below; the hoisting should be on the variable idCell2 *)
    !! Struct.to_variables [cVarDef "speed2"];
    !! Loop.hoist ~name:"${var}_at" [nbMulti; cVarDef ~regexp:true "speed2_."];
    (* remove the dollar to see the error *)
    !! Variable.inline [nbMulti; cVarDef ~regexp:true "speed2_.$"];
    !! Loop.fission [tBefore; cVarDef "pos2"];

  (* Part: Coloring *)
  (* TODO: find a better target *)
  let sized_dims = [("x", "gridX"); ("y", "gridY"); ("z", "gridZ")] in
  let dims = List.map fst sized_dims in
  !! Loop.grid_enumerate sized_dims [tIndex ~nb:2 0; cFor "idCell"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "b" ^ d in
    Loop_basic.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
    Loop_basic.color color ~index:("c"^d) [cFor bd]
    in
  !! List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cx"];

  (* Part: Parallelization *)
  !! Omp.parallel_for [Shared ["bx";"by";"bz"]] [tBefore; cFor "bx"];
  (* TODO: a few more parallel_for to add *)

  )

