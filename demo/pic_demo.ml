open Optitrust
open Target

let _ = Run.script_cpp ~check_exit_at_end:false (fun () ->

  (* PART: Inlining of arithmetic operations *)

  !! Function.bind_intro ~fresh_name:"r1" ~const:true [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:2 1; cFun "vect_mul"];

  !! Generic.add_mark "foo1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline [cMark "foo1"];

  !! Function.inline [nbMulti; cFun "vect_mul"];

  !! Function.inline [nbMulti; cFun "vect_add"];

  !! Variable.inline [cVarDef "r1"];
  !! Variable.inline [cVarDef "r2"];

  (* Part: Naming the target bag *)
  !! Function.bind_args ["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "main"; cFun "bag_push"];
  !! Variable.insert "k" "int&" "b2.nb;" [tAfter; cVarDef "b2"];
  !! Variable.fold ~nonconst:true [cVarDef "k"];

  (* Part: Inlining of structure assignements *)
  !! Struct.set_explicit [cVarDef "speed2"];
  !! Struct.set_explicit [cVarDef "pos2"];
  !! Function.inline [cFunDef "bag_transfer"; cFun "bag_push"];
  !! Struct.set_explicit [nbMulti; cSet ~typ:(Some "particle")()];
  !! Struct.set_explicit [nbMulti; cSet ~typ:(Some "vect")()];

  !!();
)

let _ = Run.script_cpp (fun () ->

  (* PART: Inlining of arithmetic operations *)
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:2 1; cFun "vect_mul"];
  !! Function.inline [cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Variable.inline [nbMulti; cVarDef ~regexp:true "r."];

  (* Part: Naming the target bag *)
  !! Function.inline ~args:["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Variable.insert_and_fold "k" "int&" "b2.nb" [tAfter; cVarDef "b2"];

  (* Part: Inlining of structure assignements *)
  !! Struct.set_explicit [nbMulti; cOr [[cVarDef "speed2"]; [cVarDef "pos2"]]];
  !!! Function.inline [cFunDef "bag_transfer"; cFun "bag_push"];
  !!! Struct.set_explicit [nbMulti;cSet ~typ:(Some "particle")()];
  !!! Struct.set_explicit [nbMulti;cSet ~typ:(Some "vect")()];

  (* Part: AOS-TO-SOA *)
  !!! Struct.inline "pos" [cTypDef "particle"];
  !!! Struct.inline "speed" [cTypDef "particle"];
  !! Variable.inline [cOr [[cVarDef "p"]; [cVarDef "p2"]]];
  !! Struct.inline "items" [cTypDef "bag"];

   (* Part: Splitting the loop, with hoisting *)
   !! Struct.to_variables [cVarDef "speed2"];
   !! Loop.hoist ~patt_name:"var_step" [nbMulti; cVarDef ~regexp:true "speed2_."];
   !! Loop.fission [tBefore; cVarDef "pos2"];

  (* Part: Coloring *)
  !! Loop.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
  !! Loop.pic_coloring 2 2 ["x";"y";"z"] [cFor "step"]; 

  (* PART : to be continued with concurrent bags, and delocalized sums *)

  )




  (* DONE: replace Trace.call *)

    (* LATER:   !! Function.bind_intro ~fresh_name:"r${int}" ~const:false [cFun "vect_mul"]; *)

      (* Example use of [Trace.call] -- keep this code as a basic test, illustration for the working of [iteri_on_targets] *)
  (* !! Trace.call (fun _t ->
    Generic.add_mark "foo1" [tIndex ~nb:2 0; cFun "vect_mul"];
    Trace.step();
    Generic.add_mark "foo2" [tIndex ~nb:2 1; cFun "vect_mul"];
    Trace.step();
    Function.inline [nbMulti; cMark "foo1"];
    Trace.step();
    Generic.remove_mark "foo1" [cMark "foo1"];
    Trace.step();
    Function.inline [nbMulti; cMark "foo2"];
    Trace.step();
    Generic.remove_mark "foo2" [cMark "foo2"];
  ); *)


(*
  !! Generic.add_mark "foo1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Generic.add_mark "foo2" [tIndex ~nb:2 1; cFun "vect_mul"];
  !! Function.inline [nbMulti; cMark "foo1"];
  !! Function.inline [nbMulti; cMark "foo2"];
  *)

(* LATER: see why !!! above does not work before Struct.set_explicit *)

  (* DONE:  Variable.insert_and_fold "int" "k" "b2.nb" [tAfter; cVarDef "b2"]; *)
  (*  Variable.insert_and_fold ~const:true "int" "k" "b2.nb" [tAfter; cVarDef "b2"];*)
  (*   Variable.insert_and_fold "const int" "k" "b2.nb" [tAfter; cVarDef "b2"];*)

  (* DONE: remove extra braces in loop fission *)


        (* TODO: cVarDef ~regexp:true "(speed2|pos2)_."  *) (* TODO4 *)

        (* DONE:  reimplement  !! Loop.pic_coloring 2 2 ["x";"y";"z"] [cFor "step"];  *)


  (* details on loop fission; useful for debugging extra braces
   !! Loop.fission [tBefore;cSet ~lhs:[sExpr "pos2_x"] ()];
   !! Loop.fission [tBefore;cVarDef "idCell2"];
   *)

(* TODO 4bis: in
   !! Variable.fold ~nonconst:true [cVarDef "k"];
   the ~nonconst:true should not be required when the variable is a reference.
  *)
