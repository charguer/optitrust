open Optitrust
open Target

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun _ ->
  (* TODO: split this files into one file for each type of simplification *)

  (*-----------------------------------------------------*)

  (* DEBUG for Arthur *)
  (* Show reification without inlined atoms *)
  (*!! Arith_basic.debug_without_inlined_atoms := true;
  !! Arith_basic.show ~normalized:false [nbMulti; cWriteVar "rei"; dRHS];
  !! Arith_basic.debug_without_inlined_atoms := false;

  (* Show reification with inlined atoms *)
  !! Arith_basic.show [nbMulti; cWriteVar "rej"; dRHS];

  (* Show reification then remove printed information *)
  !! Arith_basic.show [nbMulti; cWriteVar "rek"; dRHS];
  !! Arith_basic.remove_show [nbMulti; cWriteVar "rek"; dRHS];*)

  (* LATER: add a test with a subterm non-deletable but redundant,
     and one deletable but non-redundant. *)

  (*-----------------------------------------------------*)

  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "ra"; dRHS];

  !! Arith_basic.(simpl normalize) [nbMulti; cWriteVar "re"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "re"; dRHS];
  !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "rf"; dRHS];

  !! Arith_basic.(simpl expand_rec) [nbMulti; cWriteVar "eu"; dRHS];
  !! Arith_basic.(simpl euclidian) [nbMulti; cWriteVar "eu"; dRHS];
  !! Arith_basic.(simpl (compose [expand_rec; euclidian; gather_rec])) [nbMulti; cWriteVar "eur"; dRHS];

  !! Arith_basic.(simpl compute) [nbMulti; cWriteVar "ci"; dRHS];
  !! Arith_basic.(simpl compute) [nbMulti; cWriteVar "cd"; dRHS];

  (* !! Arith_basic.simplify ~indepth:true [dRoot]); *) (* Test of all at once: *)

  !! Arith_basic.nosimpl [nbMulti; cFunDef "simpl_in_depth"; cVarDef "x"; cCall "g"];
  !! Arith_basic.simplify ~indepth:true [nbMulti; cFunDef "simpl_in_depth"; cVarDef "x"];
  !! Arith_basic.clear_nosimpl [];

  !! Arith_basic.simplify ~indepth:false [nbMulti; cFunDef "simpl_in_depth"]; (* do nothing *)
  !! Arith_basic.simplify ~indepth:true [nbMulti; cFunDef "simpl_in_depth"];

  !! Arith_basic.(simpl identity) [nbMulti; cWriteVar "x"; dRHS];
  !! Arith_basic.(simpl normalize) [nbMulti; cWriteVar "x"; dRHS]; (* empty diff *)
  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "y"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "z"; dRHS];
  !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "t"; dRHS];
  !! Arith_basic.(simpl expand_rec) [nbMulti; cWriteVar "u"; dRHS];
  !! Arith_basic.(simpl expand) [nbMulti; cWriteVar "v"; dRHS];

  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "f"; dRHS];

  !! Arith_basic.(simpl gather) [nbMulti; cWriteVar "ls"; dRHS];
  !! Arith_basic.(simpl gather) [nbMulti; cFor "ls2"; dForStop];
  !! Arith_basic.(simpl gather) [nbMulti; cFor "ls2"; dForStart];

  !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "q"; dRHS];
  !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "p"; dRHS];
  !! Arith_basic.(simpl compute) [nbMulti; cWriteVar "q"; dRHS; cBinop Binop_exact_div]

)

(* For testing one line, add a line in the source "r = ...;" and use:
    !! Arith_basic.(simpl gather_rec) [nbMulti; cWriteVar "r"; dRHS];
*)

(* For testing all lines concerning one variable:
    !! (for i = 0 to 5 do
         Tools.debug "===%d====" i;
         Arith_basic.(simpl gather_rec) [occIndex i; cWriteVar "t"; dRHS];
    done);
*)

(* For debugging apply_bottom_up:
     Arith_basic.(simpl (apply_bottom_up_if_debug true true)) [occIndex i; cWriteVar "t"; dRHS];
     Arith_basic.(simpl apply_bottom_up_debug) [occIndex i; cWriteVar "t"; dRHS];
*)
