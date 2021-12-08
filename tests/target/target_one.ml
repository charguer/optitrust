open Optitrust
open Target


(* LATER: add unit test for each constraint function *)
(*
  show [cFieldRead "x" ()];
  show [cFieldWrite "x" ()];
  show [cFieldAccess "x" ()];
  show [cFieldWrite "x" ];
  show [cFieldRead "y" ];
  show [cFieldWrite "y" ];
  show [cFieldRead "pos" ];
  show [cFieldWrite "pos" ];*)


let _ = Run.script_cpp (fun () ->

  (* Root *)
  show [];
  show [dRoot];

  (* Constants *)
  show [ cInt 8 ];
  show [ cInt 13 ];

  (* Types *)
  show [ cTypDef "vect" ];
  show [ cTypDef "intstar" ];

  (* Var/fun occurences *)
  show [ cVar "u" ];
  show [ cVar "r2" ];
  show [ cVar "g" ];

  (* Vardef/initializer *)
  show [ cVarDef "r" ];
  show [ cVarDef "r" ; cPrimNew() ];
  show [ cVarDef "r" ; cPrimNew(); dArg 0 ];
  show [ cVarDef "r" ; cInit () ];
  show [ cVarDef "r" ; dInit ];
  show [ cInit () ];
  show [ cVarDef ""; cInit ~arg:[cStrict; cLit] () ];
  show [ cVarDef ""; cStrict; cInit ~arg:[ cStrict; cLit] () ]; (* TODO: not resolving to the good ones *)

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
  show [ cCall ~args:[[cInt 2]] "" ];


  (* Var/Fun definitions *)
  show [ cFunDef "main" ];
  show [ cFunDef "f" ];
  (* find functions of 2 arguments *)
  show [ cFunDef "" ~args:[[];[]] ];
  (* find functions of 2 arguments, one named t *)
  show [ cFunDef "" ~args:[[cArg "t"];[]] ];
  show [ cFunDef "" ~args:[[cTrue];[cArg "varg"]]];

  (* Regexp *)
  (* show [sInstr "j <"]; *) (* We can match only inside the body of the loop now*)

  show [nbExact 0; sInstr ~substr:false "j <"];

  show [sInstr "+= 2"];
  show [nbExact 0; sExpr ~substr:false "+= 2"];
  show [nbExact 0; sInstr ~substr:false "+= 2"];
  show [sInstr "r += 2"];
  show [sInstr "i++"];
  show [nbExact 6; sInstrRegexp "int . = .."];
  show [ sInstrRegexp ~substr:true ". = ."];
  show [nbExact 1; sInstrRegexp ~substr:false ". = ."];
  show [nbExact 1; sInstr "int r = 3"];
  show [nbExact 0; sExpr "int r = 3"];
  show [sInstr "i++" ];
  show [nbExact 2; sInstrRegexp "f\\(.\\)" ]; (* Finds all the occurrences of the f function call, somehow it matches the for loop!!*)
  show [cVarDef ~regexp:true "r|s"];

  (* Declarations *)
  show [cDef "s"];
  show [cDef "p2"];

)
(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)
(* LATER: match types using a function of their list of fields *)

