open Optitrust
open Target

let ty_double : typ_constraint =
  function { typ_desc = Typ_double; _ } -> true | _ -> false

let ty_bool : typ_constraint =
  function { typ_desc = Typ_bool; _ } -> true | _ -> false


let _ = Flags.keep_marks_added_by_target_show := true

let _ = Run.script_cpp (fun () ->

  (* This is a demo for the [hasType] constraints and variants *)

  (* Type of terms *)
  show [ nbExact 3; cFor ""; cVar "";cHasType "int" ];
  show [ nbExact 2; cFor "j"; cVar "";cHasType "int" ];
  show [ nbExact 5; cHasTypePred ty_double ];

  (* Type of variables occurrence *)
  show [ nbExact 7; cVar ""; cHasType "int" ]; (* use cHasType to find all trms of type int *)
  show [ nbExact 7; cVar ~typ:"int" ""]; (* use named argument ~typ for variable occurrences to replace cHasType *)

  show [ nbExact 3; cVar ""; cHasTypePred ty_double]; (* use cHasTypePred to find all trms whose type satisfies a given predicate*)
  show [ nbExact 3; cVar ~typ_pred:ty_double "" ]; (* use named argument ~typ_pred for variable occurrences to replace cHasTypPred *)
  (* Note: If you pass both ~typ and ~typ_pred arguments to the constructor cVar, the target resolution will fail *)

  (* Type of variables definitions *)
  show [ nbExact 3; cVarDef "" ];
  show [ nbExact 1; cVarDef ~typ:"double" "" ];
  show [ nbExact 1; cVarDef ~typ_pred:ty_double "" ];

  (* Type of arguments *)
  show [ nbExact 1; cWrite ~typ:"int" () ];
  show [ nbExact 1; cWrite ~typ:"double" () ];
  show [ nbExact 0; cWrite ~typ:"bool" () ];

  show [ nbExact 1; cPrimFun ~args:[[cHasType "int"]; []] (Prim_binop Binop_set) ];
  show [ nbExact 2; cPrimFun ~args:[[cAnd [[]; [cHasType "int"]]]; []] (Prim_binop Binop_set) ];

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

