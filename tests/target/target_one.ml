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
  show [ cFor ~stop:[cInt 5] "" ];

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
  (* show [sInstr "j <"]; *) (* We can match only inside the body of the loop now*)
  show [nbExact 0; sInstr ~substr:false "j <"];

  show [sInstr "+= 2"];
  show [nbExact 0; sExpr ~substr:false "+= 2"];
  show [nbExact 0; sInstr ~substr:false "+= 2"]; 
  show [sInstr "r += 2"];
  show [sInstr "i++"];
  show [nbMulti; sInstrRegexp "int . = .."]; 
  show [nbMulti; sInstrRegexp ~substr:true ". = ."];
  show [nbMulti; sInstrRegexp ~substr:false ". = ."]; 
)
 (*
  show [sInstr ~substr:tru  (* TODO: ARTHUR: specify different what should be "instructions" *)

  show [nbExact 1; sInstr "int r = 3"];(* using int r = 3; resolve to the main function!!!! *)
  show [nbExact 0; sExpr "int r = 3"];(* TODO:? using int r = 3; resolve to the main function!!!! *)

  show [nbMulti; sInstr "i++" ]; (* TODO: i++ in loop should be either an instruction or an expression, not both? *)
  (* Works, in general but fails here because there are more then one occurrences of i++ *)
  show [nbMulti; sExprRegexp "f\\(.\\)" ]; (* Finds all the occurrences of the f function call, somehow it matches the for loop!!*)


  (* LATER: Implement cDef constructor *)
  (* show [ cDef "f" ]; *)
  (* show [ cDef "s" ]; *)
  (* show [ cDef "p2" ]; *)


(* LATER: smart constructors for checking calls to builtin operations such as get/set/compare/incr, etc *)

(* LATER: show [ cFunDefDef ~args:[[bTrue]; [cOfTyp "vect*"]] "" ]; *)

(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)

(* LATER: match types using a function of their list of fields *)
*)
