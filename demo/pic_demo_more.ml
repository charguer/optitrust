open Optitrust
open Target

let _ = Run.script_cpp (fun () ->

  (* Part: inlining of the bag iteration *)
  (* skip *)

  (* Part: optimize chunk allocation *)
  (* skip *)

  (* PART: Inlining of arithmetic operations *)
  (* TODO: the intermediate names should be inserted then inlined automatically *)
  !! Function.inline [cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];

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

  (* Part: vectorization of cornerInterpolationCoeff *)

  (* Part: space reuse for storing updated speeds and positions *)

  (* Part: optimization of computation of speeds *)

  (* Part: optimization of accumulateChargeAtCorners *)

  (* Part: scaling of electric field -- LATER ARTHUR: check if we need this *)

  (* Part: scaling of speeds *)

  (* Part: scaling of positions *)

  (* Part: shifting of positions *)

  (* Part: simplifications in charge deposit *)

  (* Part: vectorization in charge deposit *)

  (* Part: introduction of thread-independence in charge deposit *)

  (* Part: loop splitting for treatments of speeds and positions and deposit *)
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

