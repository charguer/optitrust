open Optitrust
open Target


let coloring (ds : string list) (tg : target) : unit =
  let bs = List.map (fun s -> "b" ^ s) ds in
  let cs = List.map (fun s -> "c" ^ s) ds in
  List.iter2 (fun d b -> Loop_basic.tile "2" ~index:b (tg @ [cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color "2" ~index:c (tg @ [cFor b])) bs cs



let _ = Run.script_cpp (fun () ->

  (* PART 1: Inlining *)
  !! Function.bind_intro ~fresh_name:"r1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" [tIndex ~nb:2 1; cFun "vect_mul"];
show [nbMulti; cFun "vect_mul"];
  !! Function.inline [nbMulti; cFun "vect_mul"];
  !! Function.inline [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline [cFun "vect_mul"];
  !! Function.inline [tIndex ~nb:2 0; cFun "vect_add"];
  !! Function.inline [cFun "vect_add"];
  !! Variable.inline ~delete:true [cOr [[cVarDef "r1"];[cVarDef "r2"]]];

  (* PART 2: Explicit assignments *)
  !! Struct.set_explicit [nbMulti;cVarDef ~typ:(Some "vect") ~substr:true "2"];
  !! Function.bind_args ["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "bag_transfer"; cFun "bag_push"];
  !! Struct.set_explicit [nbMulti;cSet ~typ:(Some "particle")()];
  !! Struct.set_explicit [nbMulti;cSet ~typ:(Some "vect")()];

  (* Part 2 AOS-TO-SOA *)
  !! Sequence.insert "int k = b2.nb;" [tAfter; cVarDef "b2"];
  !! Variable.fold ~nonconst:true [cVarDef "k"];
  !! Struct.inline "pos" [cTypDef "particle"];
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.inline "items" [cTypDef "bag"]; (* FIX ME! *)

   (* PART 3 Splitting computations *)
   !! Struct.to_variables [cVarDef "speed2"];
   !! Loop.extract_variable [cVarDef "speed2_x"]; (* TODO: try to do the 3 at once *)
   !! Loop.extract_variable [cVarDef "speed2_y"];
   !! Loop.extract_variable [cVarDef "speed2_z"];

    (* TODO: this is a hoist of a struct which goes as named arrays; factorizable as
         Loop.extract_struct ~names:["pos2_x";"pos2_y";"pos2_z"] [cVarDef "pos2"]; *)
   !! Struct.to_variables [cVarDef "pos2"];
   !! Loop.extract_variable [cVarDef "pos2_x"]; (* TODO: try to do the 3 at once with pos2_* *)
   !! Loop.extract_variable [cVarDef "pos2_y"];
   !! Loop.extract_variable [cVarDef "pos2_z"];
   !! Loop.fission [tBefore;sInstr "pos2_x[idParticle] = "];
   !! Loop.fission [tBefore;cVarDef "idCell2"];

  (* PART4  Coloring *)
   !! Loop.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
   !! coloring ["x";"y";"z"] [cFor "step"];
   (* coloring function is replacing the six commented lines below *)
   (* !! Loop_basic.tile "2" ~index:"bx" [cFor "x"];
   !! Loop_basic.tile "2" ~index:"by" [cFor "y"];
   !! Loop_basic.tile "2" ~index:"bz" [cFor "z"];
   !! Loop_basic.color "2" ~index:"cx" [cFor "bx"];
   !! Loop_basic.color "2" ~index:"cy" [cFor "by"];
   !! Loop_basic.color "2" ~index:"cz" [cFor "bz"]; *)
   !! Loop.move "x" ~after:"bz";
   !! Loop.move "y" ~after:"x";
   !! Loop.move "cy" ~before:"bx";
   !! Loop.move "cz" ~before:"bx"; (* TODO: keep this in a switch, and introduce a function to do color+moves *)
  (* PART 5 Concurrency, TODO: Arthur*)

)


