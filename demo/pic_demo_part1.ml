open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (* PART 1: Inlining *)
  !! Function_basic.bind_intro ~fresh_name:"r1" [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline_call [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline_call [tIndex ~nb:2 0; cFun "vect_add"];
  !! Variable_basic.inline ~delete:true [cVarDef "r1"];
  !! Function_basic.bind_intro ~fresh_name:"r2" [cFun "vect_mul"];
  !! Function.inline_call [cFun "vect_mul"];
  !! Function.inline_call [cFun "vect_add"];
  !! Variable_basic.inline ~delete:true [cVarDef "r2"];
  !! Struct.set_explicit [cVarDef "speed2"];
  !! Struct.set_explicit [cVarDef "pos2"];
  !! Function.bind_args ["b2";""] [cTopFun "main"; cFun "bag_push"];
  !! Function.inline_call [cTopFun "main"; cFun "bag_push"];
  !! Function.inline_call [cTopFun "bag_transfer"; cFun "bag_push"];
  !! Struct_basic.set_explicit [sInstr " = p2"];
  !! Struct_basic.set_explicit [sInstr " = b2.items[i]"];
  !! Struct.set_explicit [sInstr " = p2.pos"];
  !! Struct.set_explicit [sInstr " = b2.items[i].pos"];
  !! Struct.set_explicit [sInstr " = p2.speed"];
  !! Struct.set_explicit [sInstr " = b2.items[i].speed"];
  
  (* Part 2 AOS-TO-SOA *)
  !! Sequence_basic.insert "int k = b2.nb;" [tAfter; cVarDef "b2"];
  !! Variable.fold ~nonconst:true [cVarDef "k"];
  !! Struct_basic.inline "pos" [cTypDef "particle"];
  !! Struct_basic.inline "speed" [cTypDef "particle"];
  !! Struct_basic.set_explicit [cTopFun "bag_push"; sInstr "= p"];
  !! Variable_basic.inline ~delete:true [cVarDef "p"];
  !! Struct_basic.inline "items" [cTypDef "bag"];

  (*  PART 3 Splitting computations *)
   !! Struct_basic.to_variables [cVarDef "speed2"];
   !! Loop_basic.extract_variable [cVarDef "speed2_x"];
   !! Loop_basic.extract_variable [cVarDef "speed2_y"];
   !! Loop_basic.extract_variable [cVarDef "speed2_z"];

   !! Struct_basic.to_variables [cVarDef "pos2"];
   !! Loop_basic.extract_variable [cVarDef "pos2_x"];
   !! Loop_basic.extract_variable [cVarDef "pos2_y"];
   !! Loop_basic.extract_variable [cVarDef "pos2_z"];
   
   !! Loop_basic.fission [tBefore;sInstr "pos2_x[idParticle] = "];
   !! Loop_basic.fission [tBefore;cVarDef "idCell2"];
   
  (* PART4  Coloring *)
   !! Loop_basic.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
   !! Loop_basic.tile "2" ~index:"bx" [cFor "x"];
   !! Loop_basic.tile "2" ~index:"by" [cFor "y"];
   !! Loop_basic.tile "2" ~index:"bz" [cFor "z"];
   !! Loop_basic.color "2" ~index:"cx" [cFor "bx"];
   !! Loop_basic.color "2" ~index:"cy" [cFor "by"];
   !! Loop_basic.color "2" ~index:"cz" [cFor "bz"];
   !! Loop.move "x" ~after:"bz";
   !! Loop.move "y" ~after:"x";
   !! Loop.move "cy" ~before:"bx";
   !! Loop.move "cz" ~before:"bx";
  (* PART 5 Concurrency, TODO:*)
)


