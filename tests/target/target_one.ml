open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
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
  show [ cFor ~cond:[sExpr "j < 5"] "" ];

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

  (*show [ cFunDef ~args:[bTrue;cVarDef "varg"] "" ];(* This doesn't work' *)*)
  (* show [ cFunDef ~args_pred:((fun i -> [bTrue]),(fun bs -> List.length bs = 2)) "" ]; (* This doesn't work' *) *)

  (* Regexp *)
  (* TODO: ARTHUR: specify different what should be "instructions" *)
  show [sExpr "j <"];
  show [nbEx 0; sInstr "j <"];

  show [sInstr "+= 2"];
  show [nbEx 0; sExpr ~substr:false "+= 2"];
  show [nbEx 0; sInstr ~substr:true "+= 2"]; (* TODO : false or true ? *)
  show [sInstr (* default value: ~substr:true *) "r += 2"];

  show [nbMulti; sExprRegexp ~substr:true "int . = .."]; (* TODO: should match is ; part of it or not? *)
  show [nbMulti; sInstrRegexp ~substr:true ". = ."];
  show [nbMulti; sInstrRegexp ~substr:false ". = ."]; (* should not match something with several characters TODO:*)

 (*
  show [sInstr ~substr:true "vect v2" ];
  show [sInstrRegexp ~substr:true "vect v2" ];*)
  show [nbEx 1; sExpr "vect v2" ];

  show [nbEx 1; sExpr "int r = 3"];(* using int r = 3; resolve to the main function!!!! *)
  show [nbEx 0; sInstr "int r = 3"];(* TODO:? using int r = 3; resolve to the main function!!!! *)

  show [nbMulti; sInstr "i++" ]; (* TODO: i++ in loop should be either an instruction or an expression, not both? *)
  (* Works, in general but fails here because there are more then one occurrences of i++ *)
  show [nbMulti; sExprRegexp "f\\(.\\)" ]; (* Finds all the occurrences of the f function call, somehow it matches the for loop!!*)
  (* TODO: why an hidden match? *)

)

  (* LATER: Implement cDef constructor *)
  (* show [ cDef "f" ]; *)
  (* show [ cDef "s" ]; *)
  (* show [ cDef "p2" ]; *)


(* LATER: smart constructors for checking calls to builtin operations such as get/set/compare/incr, etc *)

(* LATER: show [ cFunDefDef ~args:[[bTrue]; [cOfTyp "vect*"]] "" ]; *)

(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)

(* LATER: match types using a function of their list of fields *)
