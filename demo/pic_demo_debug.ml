open Optitrust
open Target
open Ast 

let _ = Run.script_cpp (fun () ->
  
  (* PART: Inlining of arithmetic operations *)
  !! Function.bind_intro ~fresh_name:"r1" ~const:true [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.bind_intro ~fresh_name:"r2" ~const:true [tIndex ~nb:2 1; cFun "vect_mul"];
  !! Function.inline [cOr [[cFun "vect_mul"]; [cFun "vect_add"]]];
  !! Variable.inline [nbMulti; cVarDef ~regexp:true "r."];
  !! Function.(inline ~vars:(AddSuffix "2"))[cFun "idCellOfPos"];

  (* Part: Coloring *)
  !! Loop.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [tIndex ~nb:2 0;cFor "idCell"];
  let colorize (tile : string) (color : string) (d:string) : unit =
    let bd = "b" ^ d in
    Loop.tile tile ~bound:TileBoundDivides ~index:"b${id}" [cFor d]; (* DONE: ~index:"b${id}" *)
    Loop.color color ~index:("c"^d) [cFor bd]
    in
  (*!! colorize "2" "2" "x";*)
  let dims = ["x";"y";"z"] in
  !! List.iter (colorize "2" "2") dims;
  !! Loop.reorder ~order:(Tools.((add_prefix "c" dims) @ (add_prefix "b" dims) @ dims)) [cFor "cx"];
  
  (* Introduction of the computation *)
  !! Variable.insert "d" "int" "blockSize/2" [tAfter;cVarDef "blockSize"];
  !! Variable.insert "distanceToBlockLessThanHalfABlock" "bool"  
       "(x2 >= bx + d && x2 < bx + blockSize + d)
    && (y2 >= by + d && y2 < by + blockSize + d)" [tAfter; cVarDef "p2"];
  !! Flow.insert_if "distanceToBlockLessThanHalfABlock" [cFunDef "main"; cFun "bag_push"];
  !! Instr.replace_fun "bag_push_atomic" [cFunDef "main"; cIf ();dElse; cFun "bag_push"];
  
  (* Delocalize of bagsNext *)
  !! Sequence.intro_between ~mark:"next" [tAfter; cVarDef "bagsNext"] [tBefore; cFor ~body:[cFun "bag_transfer"] "idCell"];
  let ops = Ast.Delocalize_obj ("bag_create", "bag_transfer") in
  !! Variable.delocalize_in_vars  ~local_vars:["bagsNextPrivate";"bagsNextShared"]~old_var:"bagsNext" ~new_var:"bagsNextLocal" ~var_type:Ast.(typ_ptr Ptr_kind_mut (typ_constr "bag")) ~array_size:"N" ~dl_ops:ops  [cMark "next"];
  !! Specialize.choose "bagsNextPrivate" [cIf();dThen; cChoose];
  !! Specialize.choose "bagsNextShared" [cIf();dElse; cChoose];
  !! Sequence.elim [cMark "next"];

  (* Inlining of structure assignements *)
  !! Struct.set_explicit [nbMulti; cOr [[cVarDef "speed2"]; [cVarDef "pos2"]]];
  !! Function.inline [cFunDef "bag_transfer"; cFun "bag_push"];
  !! Struct.set_explicit [nbMulti;cSet ~typ:"particle"()];
  !! Struct.set_explicit [nbMulti;cSet ~typ:"vect"()];
  !! Function.inline [cFunDef "main";cOr [[cFun "bag_push"]; [cFun "bag_push_atomic"]]];

  (* !! Function.inline ~args:["&b2";""] [cFunDef "main"; cFun "bag_push"]; *)
  !! Variable.inline [cOr [[cVarDef "p"]; [cVarDef "p2"]]];
  

  (* AOS-TO-SOA *)
  !! Struct.inline "pos" [cTypDef "particle"];
  !! Struct.inline "speed" [cTypDef "particle"];
  

  (* let shift_coord d = 
      let f = "pos_" ^ d in
        Arith.shift (code (d ^ " * cellSize")) [nbAny;cFunDef "main";cFieldGet f];
        (* TODO: After inlining bag_push *)
        (* Arith.shift (code (d ^ "2 * cellSize")) [nbAny;cFunDef "main";cFieldGet f]  *)
      in
   !! List.iter shift_coord dims;


  !! Struct.inline "items" [cTypDef "bag"]; *)


  
  (* Relative positions *)
  
  
    

)