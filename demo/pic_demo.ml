open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* PART 1: Inlining *)

    (* NOTE: please keep commented versions here for a few days. *)

  (*details:*)
  !! Function.bind_intro ~fresh_name:"r1" ~const:false [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" [tIndex ~nb:2 1; cFun "vect_mul"];
  (* test: both at once works!
  !! iteri_on_targets (fun i _t p ->
     Function.bind_intro ~fresh_name:("r"^string_of_int (i+1)) (target_of_path p);
  )  [nbMulti; cFun "vect_mul"];
  *)

  (* all inlining at once! *)
  !! Function.inline [nbMulti; cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  (* details:
  !! Function.inline [nbMulti; cFun "vect_mul"];
  !! Function.inline [nbMulti; cFun "vect_add"];
  *)

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
  ); *) (*

  !! Generic.add_mark "foo1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Generic.add_mark "foo2" [tIndex ~nb:2 1; cFun "vect_mul"];
  !! Function.inline [nbMulti; cMark "foo1"];
  !! Function.inline [nbMulti; cMark "foo2"];
  *)

  (* previous workaround for inlining vect_mul, in two steps
  !! Function.inline [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [tIndex ~nb:2 0; cFun "vect_add"];
  !! Function.inline [cFun "vect_add"];
  *)

  !! Variable.inline ~delete:true [cOr [[cVarDef "r1"];[cVarDef "r2"]]];

  (* Part 2 AOS-TO-SOA *)
  !! Struct.set_explicit [nbMulti;cVarDef ~typ:(Some "vect") ~substr:true "2"];
  !! Function.bind_args ["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "bag_transfer"; cFun "bag_push"];
  !! Struct.set_explicit [nbMulti;cSet ~typ:(Some "particle")()];
  !! Struct.set_explicit [nbMulti;cSet ~typ:(Some "vect")()];
  !! Sequence.insert "int k = b2.nb;" [tAfter; cVarDef "b2"];
  !! Variable.fold ~nonconst:true [cVarDef "k"];
  !! Struct.inline "pos" [cTypDef "particle"];
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.set_explicit [cVarDef "p"];
  !! Struct.inline "items" [cTypDef "bag"];

   (* PART 3 Splitting computations *)
   !! Struct.to_variables [nbMulti;cVarDef ~typ:(Some "vect") ~substr:true "2"];
   !! Loop.extract_variable [nbMulti;cVarDef ~typ:(Some "double") ~substr:true "2"];
   !! Loop.fission [tBefore;cSet ~lhs:[sExpr "pos2_x"] ()];
   !! Loop.fission [tBefore;cVarDef "idCell2"];

  (* PART4  Coloring *)
   !! Loop.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
   !! Loop.pic_coloring 2 2 ["x";"y";"z"] [cFor "step"];

  (* PART 5 Concurrency, LATER: Arthur*)
)