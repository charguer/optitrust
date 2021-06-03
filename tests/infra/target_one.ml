open Optitrust

let _ = run_unit_test (fun () ->
  (** There should be exactly one result to each of the commands;
      if it is not the case, we'll get an error. *)
  (* let show = Tr.target_show in *)
  let show = show_target in

  (* Constants *)
  show [ cInt 8 ];

  (* Types *)
  show [ cTypDef "vect" ];
  show [ cTypDef "intstar" ];

  (* Var/fun occurences *)
  show [ cVar "u" ]; (* Doesn't work properly*)
  show [ cVar "r2" ]; (* Doesn't work properly*)
  show [ cVar "f" ]; (* Doesn't work properly*)
  show [ cVar "g" ]; (* Doesn't work properly*)

  (* Loops *)
  show [ cFor "i" ];
  show [ cFor "j" ];
  show [ cFor ~cond:[cInstr "j < 5"] "" ];

  (* Abort *)
  show [ cBreak ];
  show [ cContinue ];
  show [ cReturn ];

  (* Labels *)
  show [ cLabel "lbl1" ];
  show [ cLabel "lbl2" ];

  (* Calls *)
  show [ cCall "f" ];
  show [ cCall ~args:[cInt 2] "" ];

  (* Var/Fun definitions *)
  show [ cFunDef "main" ];
  show [ cFunDef "f" ];
  (* show [ cFunDef ~args:[cTrue;cVarDef "varg"] "" ]; *)
  (* show [ cFunDef ~args_pred:((fun i -> [cTrue]),(fun bs -> List.length bs = 2)) "" ]; *)

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

(* LATER: Str/Regexp -- i still not to work on the specification of what is expected to match or not match
  show [ cInstr "+= 2" ];
  show [ cExpr "j <" ]; (* Does not work *)
  show [ cExpr "vect v2" ]; (* Does not work *)
  show [ cStrFull "int r = 3;" ]; (* with or without the ; ? *)
  show [ cInstr "i++" ];
  show [ cInstr "+=" ];
  show [ c ~sub:false "+=" ];
  show [ cRegexp "f\\(.\\)" ];
  *)



