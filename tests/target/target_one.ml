open Optitrust
open Target

let _ = Flags.execute_show_even_in_batch_mode := true

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
  show [ cVarInit "r"];
  show [ cVarInit "r"];
  (* show [ cInit () ]; *)
  (* TODO FIX show [ cVarDef ""; dInit ~arg:[cStrict; cLit] () ];*)
  (* show [ cVarDef ""; cStrict; dInit ~arg:[ cStrict; cLit] () ]; *)

  (* Loops *)
  show [ cFor "i" ];
  show [ cFor "j" ];
  show [ cFor ~stop:[cInt 5] "" ];

  (* Abort *)
  show [ cBreak ];
  show [ cContinue ];
  show [ cReturn ];

  (* Labels *)
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


  (* Function calls are
    - instructions if return type is unit
    - expressions otherwise *)
  show [sInstr "g("];
  show [sExpr "f("];
  (* Regexp *)

  (* We can match only inside the body of the loop now*)
  show [sInstr "+= 2"];
  show [nbExact 0; sExpr ~substr:false "+= 2"];
  show [nbExact 0; sInstr ~substr:false "+= 2"];
  show [sInstr "r += 2"];
  show [sInstr "i++"];

  (* show [nbExact 2; sInstrRegexp ~substr:false "int . = .."]; *)
  show [nbExact 8; sInstrRegexp "int . = .."];
  show [nbExact 6; sInstrRegexp ~substr:true " .. ="];

  (* show [nbExact 1; sInstr "int r = 3"]; *)
  show [nbExact 0; sExpr "int r = 3"];
  show [sInstr "i++" ];
  show [sExpr "f"];
  show [sInstr "f" ];

  show [nbExact 1; cVarDef "r"];
  show [nbExact 4; cVarDef ~substr:true "r"];
  show [nbExact 2; cVarDef ~regexp:true "p."];
  show [nbExact 1; cVarDef ~regexp:true "r"];
  show [nbExact 5; cVarDef ~regexp:true "r\\|n"];

  (* Declarations *)
  show [cDef "s"];
  show [cDef "p2"];

)
(* LATER: match typedef using a function over the body of the type definition *)
(* LATER: match a typedef struct using of a function over the list fields [(var*typ)list->bool] *)
(* LATER: match types using a function of their list of fields *)

