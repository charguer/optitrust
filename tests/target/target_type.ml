open Optitrust
open Target

let ty_double : typ_constraint =
  function { typ_desc = Typ_double; _ } -> true | _ -> false

let _ = Run.script_cpp (fun () ->

  (* This is a demo for the [hasType] constraints and variants *)

  (* Type of terms *)
  show [ nbExact 13; cFor "j"; cHasType "int" ]; (* not all marks are visible in the diff *)
  show [ nbExact 5; cHasTypePred ty_double ];

  (* Type of variables occurrence *)
  show [ nbMulti; cAnd [ [cVar ""]; [ cHasType "int" ] ] ];
  show [ nbExact 5; cVar ~typ:"int" "" ];
  show [ nbExact 3; cVar ~typ_pred:ty_double "" ];

  (* Type of variables definitions *)
  show [ nbExact 2; cVarDef "" ];
  show [ nbExact 1; cVarDef ~typ:"double" "" ];
  show [ nbExact 1; cVarDef ~typ_pred:ty_double "" ];

  (* Type of arguments *)
  show [ nbExact 3; cSet () ];
  show [ nbExact 10; cSet (); cHasType "int" ];
  show [ nbExact 4; cSet (); cStrict; cHasType "int" ];
  show [ nbExact 3; cPrimFun ~args:[[]; []] (Prim_binop Binop_set) ];
  show [ nbExact 3; cPrimFun ~args:[[cVar ""]; []] (Prim_binop Binop_set) ];
  show [ nbExact 1; cPrimFun ~args:[[cVar "i"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 1; cPrimFun ~args:[[cStrict; cVar "i"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cHasType "int"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cStrict; cHasType "int"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cAnd [[]; [cHasType "int"]]]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cSet ~typ:"int" () ];
  show [ nbExact 1; cSet ~typ_pred:ty_double () ];

  (* Type of arguments *)
  show [ nbExact 1; cFunDef ~args:[ [cArg ~typ:"int" ""]; [cArg ""] ] "" ];
  show [ nbExact 2; cFunDef ~args_pred:(target_list_one_st [ cArg ~typ:"double" "" ]) "" ];
  )
  (* LATER: direct constraints to target arguments?
     LATER: constraint to target between arguments, e.g. for inserting a new argument to a function
  show [ nbMulti; cArg ~typ:int ];
  show [ nbMulti; cArg ~typ_ast:(typ_double()) ]; *)

