open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  (* let show = Tr.target_show in *)
  let show = Generic.target_show in

  (* Constants *)
  show [ cInt 8 ];

  (* Types *)
  show [ cTypDef "vect" ];
  show [ cTypDef "intstar" ];

  (* Var/fun occurences *)
  show [ cVar "u" ];
  show [ cVar "r2" ];
  show [ cVar "g" ];

  (* Loops *)
  show [ cFor "i" ];
  show [ cFor "j" ];
  show [ cFor ~cond:[cExpr "j < 5"] "" ];

  (* Abort *)
  show [ cBreak ];
  show [ cContinue ];
  show [ cReturn ];

  (* Labels *)
  show [ cLabel "lbl1" ];
  show [ cLabel "lbl2" ];

  (* Calls *)
  show [ cCall ~args:[cInt 2] "" ]; 

  (* Var/Fun definitions *)
  show [ cFunDef "main" ];
  show [ cFunDef "f" ];

  (*show [ cFunDef ~args:[cTrue;cVarDef "varg"] "" ];(* This doesn't work' *)*)
  (* show [ cFunDef ~args_pred:((fun i -> [cTrue]),(fun bs -> List.length bs = 2)) "" ]; (* This doesn't work' *) *)

  (* Regexp *)
  show [cExpr "j <"];
  show [cNb 0; cInstr "j <"];

  show [cInstr "+= 2"];
  show [cNb 0; cExpr ~substr:false "+= 2"];
  show [cNb 0; cInstr ~substr:false "+= 2"];
  show [cInstr (* default value: ~substr:true *) "r += 2"];

  show [cMulti; cInstrRegexp ~substr:true "int . = ."]; (* TODO: should match is ; part of it or not? *)
  show [cMulti; cInstrRegexp ~substr:true ". = ."];
  show [cMulti; cInstrRegexp ~substr:false ". = ."]; (* should not match something with several characters TODO:*)

 (*
  show [cInstr ~substr:true "vect v2" ];
  show [cInstrRegexp ~substr:true "vect v2" ];*)
  show [cNb 1; cExpr "vect v2" ];

  show [cNb 1; cExpr "int r = 3"];(* using int r = 3; resolve to the main function!!!! *)
  show [cInstr "int r = 3"];(* TODO:? using int r = 3; resolve to the main function!!!! *)

  show [cMulti; cInstr "i++" ]; (* TODO: i++ in loop should be either an instruction or an expression, not both? *)
  (* Works, in general but fails here because there are more then one occurrences of i++ *)
  show [cMulti; cExprRegexp "f\\(.\\)" ]; (* Finds all the occurrences of the f function call, somehow it matches the for loop!!*)
  (* TODO: why an hidden match? *)

)

  (* LATER: Implement cDef constructor *)
  (* show [ cDef "f" ]; *)
  (* show [ cDef "s" ]; *)
  (* show [ cDef "p2" ]; *)


(* LATER: smart constructors for checking calls to builtin operations such as get/set/compare/incr, etc *)

(* LATER: show [ cFunDefDef ~args:[[cTrue]; [cOfTyp "vect*"]] "" ]; *)

(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)

(* LATER: match types using a function of their list of fields *)
