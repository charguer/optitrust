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
  
  !! Function.inline [tIndex ~nb:2 0; cFun "vect_mul"];
  !! Function.inline [cFun "vect_mul"];

  !! Function.inline [tIndex ~nb:2 0; cFun "vect_add"];
  !! Function.inline [cFun "vect_add"];

  !! Variable.inline ~delete:true [cOr [[cVarDef "r1"];[cVarDef "r2"]]];
  
  (* !! Struct.set_explicit [nbMulti; cSet ~] *)
  !! Struct.set_explicit [nbMulti;cVarDef ~typ:(Some "vect")""];
  !! Function.bind_args ["&b2";""] [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "main"; cFun "bag_push"];
  !! Function.inline [cTopFunDef "bag_transfer"; cFun "bag_push"];
  (* TODO:  Struct.set_explicit [nbMulti; cSet ~typ:"particle" ]
      yet to implement: cSet and cGet should have a ~typ argument *)
  !! Struct.set_explicit [cOr [[sInstr " = p2"];[sInstr " = b2.items[i]"]]];
  !! Struct.set_explicit [nbMulti; cFunDef "bag_transfer"; cFor "i"; dBody; sInstr " = "];
  !! Struct.set_explicit [nbMulti; sInstr "= p2."];
  (* TODO:  Struct.set_explicit [nbMulti; cSet ~typ:"vect" ] *)
  (* implement as:   cCall ~args_pred:(args_pred_list [[]; [cHasType "vect"] ]) TODO: implement cHasType
  where cHasType is a "Constr_has_typ of typconstr") that checks the type of a term.
    type typconstr = Typconstr_string of string | Typconstr_typ of typ

    for example
      [cHasType "bool"]   -> Constr_has_typ (Typconstr_string "bool")
      [cHasTypConstr "particle"]   -> Constr_has_typ (Typconstr_typ { typ_desc = Typ_constr "particle";  ..)
      [cHasTyp (typ_bool())] -> Constr_has_typ (Typconstr_typ ...)
      [cHasTypDesc Typ_double] -> Typconstr_typ (Typconstr_typ {typ_desc = Typ_double; ..dummy })

  *)

(* TODO:

example: my combi transfo
  int x
-> basic1
  int x1
  int x2
-> basic2
  int x1
  int x2a
  int x2b

 int x
 ->
  { int x1;
    int x2 }
->
  { int x1;
    int x2a;
    int x2b; }


  all transformation that may break paths of others have ?(braces:bool=false) as argument,
  including basic ones

  in combi level, typical pattern is:

  Internal.nobrace_remove_after ~unless:braces (fun _ ->
  ==> TODO: rename to
  Internal.nobrace_scope ~braces (fun _ ->
    Target.apply_on_targets tg (fun p t ->
        Transfo_basic.basic1 ~braces:true p;
        Transfo_basic.basic2 (p ++ [Dir_nth 1]);
        ))
-->

in Target:
   let apply_on_targets_scoped ~braces tg f =
     Internal.nobrace_scope ~braces (fun _ ->
       Target.apply_on_targets tg f)

  let apply_on_transformed_targets_scoped ..
*)

  (* Part 2 AOS-TO-SOA *)
  !! Sequence.insert "int k = b2.nb;" [tAfter; cVarDef "b2"];
  !! Variable.fold ~nonconst:true [cVarDef "k"]; (* TODO: this should be earlier *) (* If I apply before there is a conflict with struct inline *)

  !! Struct.inline "pos" [cTypDef "particle"];
  !! Struct.inline "speed" [cTypDef "particle"];
  !! Struct.set_explicit [cTopFunDef "bag_push"; sInstr "= p"];
  !! Variable.inline ~delete:true [cVarDef "p"];
  !! Struct.inline "items" [cTypDef "bag"];

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


