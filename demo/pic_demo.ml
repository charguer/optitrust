open Optitrust
open Target


let coloring (ds : string list) (tg : target) : unit =
  let bs = List.map (fun s -> "b" ^ s) ds in
  let cs = List.map (fun s -> "c" ^ s) ds in
  List.iter2 (fun d b -> Loop_basic.tile "2" ~index:b (tg @ [cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color "2" ~index:c (tg @ [cFor b])) bs cs
  


let _ = Run.script_cpp (fun () ->

  (* PART 1: Inlining *)
  !! Function.inline_call ~name_result:"res1" [tIndex ~nb:2 0; cFun "vect_mul"];
  (* !! Function_basic.bind_intro ~fresh_name:"r1" [tIndex ~nb:2 0; cFun "vect_mul"]; *)
  !! Function.inline_call ~name_result:"res2" [cFun "vect_mul"];
  !! Function.inline_call [tIndex ~nb:2 0; cFun "vect_add"];
  (* !! Function.inline_call [cFun "vect_add"]; *)
  !! Variable_basic.inline ~delete:true [cVarDef "res1"];
  !! Variable_basic.inline ~delete:true [cVarDef "res2"];
  !! Struct.set_explicit [nbMulti; cOr [[cVarDef "speed2"]; [cVarDef "pos2"]]]; 
  !! Function.bind_args ["&b2";""] [cTopFun "main"; cFun "bag_push"]; 
  !! Function.inline_call [cTopFun "main"; cFun "bag_push"];
  !! Function.inline_call [cTopFun "bag_transfer"; cFun "bag_push"];
  !! Struct.set_explicit [nbMulti; cOr [[sInstr " = p2"];[sInstr " = b2.items[i]"]]];
  
  (* show [nbMulti; cFunDef "bag_transfer"; cFor ""; dBody; sInstr " = "];*)
  (* TODO: try  Struct.set_explicit [nbMulti; cFunDef "bag_transfer"; cFor "i"; dBody; sInstr " = "];  
     make sure that the nobraces are left until the end, for targets to remain valid*)
  !! Struct.set_explicit [nbMulti; cFunDef "bag_transfer"; cFor "i"; dBody; sInstr " = "];  
  (* !! Struct.set_explicit [sInstr " = p2.pos"]; 
  !! Struct.set_explicit [sInstr " = b2.items[i].pos"];
  !! Struct.set_explicit [sInstr " = p2.speed"];
  !! Struct.set_explicit [sInstr " = b2.items[i].speed"]; *)
  
  (* Part 2 AOS-TO-SOA *)
  !! Sequence_basic.insert "int k = b2.nb;" [tAfter; cVarDef "b2"]; 
  !! Variable.fold ~nonconst:true [cVarDef "k"]; (* TODO: this should be earlier *) (* If I apply before there is a conflict with struct inline *)
  !! Struct_basic.inline "pos" [cTypDef "particle"];
  !! Struct_basic.inline "speed" [cTypDef "particle"]; (* TODO: remove _Basic wherever possible *)
  !! Struct_basic.set_explicit [cTopFun "bag_push"; sInstr "= p"];
  !! Variable_basic.inline ~delete:true [cVarDef "p"];
  !! Struct_basic.inline "items" [cTypDef "bag"];

   (* PART 3 Splitting computations *) 
   !! Struct_basic.to_variables [cVarDef "speed2"];
   !! Loop_basic.extract_variable [cVarDef "speed2_x"];
   !! Loop_basic.extract_variable [cVarDef "speed2_y"];
   !! Loop_basic.extract_variable [cVarDef "speed2_z"];

    (* TODO: this is a hoist of a struct which goes as named arrays; factorizable as
         Loop.extract_struct ~names:["pos2_x";"pos2_y";"pos2_z"] [cVarDef "pos2"]; *)
   !! Struct_basic.to_variables [cVarDef "pos2"];
   !! Loop_basic.extract_variable [cVarDef "pos2_x"];
   !! Loop_basic.extract_variable [cVarDef "pos2_y"];
   !! Loop_basic.extract_variable [cVarDef "pos2_z"];
   
   !! Loop_basic.fission [tBefore;sInstr "pos2_x[idParticle] = "];
   !! Loop_basic.fission [tBefore;cVarDef "idCell2"];
   
  (* PART4  Coloring *)
   !! Loop_basic.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
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


