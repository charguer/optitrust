open Optitrust
open Target

let ty_double : typ_constraint =
  function { typ_desc = Typ_double; _ } -> true | _ -> false

let ty_bool : typ_constraint =
  function { typ_desc = Typ_bool; _ } -> true | _ -> false



let _ = Run.script_cpp (fun () ->


  (* This is a demo for the [hasType] constraints and variants *)

  (* Type of terms *)
  show [cFor ""];
  show [ nbMulti; cFor "j"; cHasType "int" ];  (*LATER: ARTHUR: at some step it says "mark disappears"..*)
  show [ nbExact 5; cHasTypePred ty_double ];

  (* Type of variables occurrence *)
  show [ nbMulti; cAnd [ [cVar ""]; [ cHasType "int" ] ] ];
  (* show [ nbExact 5; cVar ~typ:"int" "" ];
  show [ nbExact 3; cVar ~typ_pred:ty_double "" ]; *)

  (* Type of variables definitions *)

  show [ nbExact 3; cVarDef "" ];
  show [ nbExact 1; cVarDef ~typ:"double" "" ];
  show [ nbExact 1; cVarDef ~typ_pred:ty_double "" ];

  (* Type of arguments *)
  show [ nbExact 3; cWrite () ];
  show [ nbExact 5; cWrite (); cHasType "int" ];
  show [ nbExact 3; cWrite (); cStrict; cHasType "int" ];
  show [ nbExact 3; cPrimFun ~args:[[]; []] (Prim_binop Binop_set) ];
  (* LATER: fixme  show [ nbExact 3; cPrimFun ~args:[[cVar ""]; []] (Prim_binop Binop_set) ];*)
  show [ nbExact 2; cPrimFun ~args:[[cVar "i"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cHasType "int"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cAnd [[]; [cHasType "int"]]]; []] (Prim_binop Binop_set) ];
  (* TODO: BUG: there are not one but occ... i = i+1 should be targeted
     show [ nbExact 2; cWrite ~typ:"int" () ]; *)
  show [ cWrite ~typ:"int" () ];
  show [ nbExact 1; cWrite ~typ_pred:ty_double () ];

  (* Type of arguments *)
  show [ nbExact 1; cFunDef ~args:[ [cArg ~typ:"int" ""]; [cArg ""] ] "" ];
  show [ nbExact 2; cFunDef ~args_pred:(target_list_one_st [ cArg ~typ:"double" "" ]) "" ];

  (* Type of funcions *)
  show [nbExact 2; cFunDef "" ~ret_typ:"int"];
  show [nbExact 1; cFunDef "" ~ret_typ_pred:ty_bool];
  show [nbExact 2; cTopFunDef "" ~ret_typ:"int"];
  show [nbExact 1; cTopFunDef "" ~ret_typ_pred:ty_bool];
)

  (* LATER: direct constraints to target arguments?
     LATER: constraint to target between arguments, e.g. for inserting a new argument to a function
  show [ nbMulti; cArg ~typ:int ];
  show [ nbMulti; cArg ~typ_ast:(typ_double()) ]; *)

