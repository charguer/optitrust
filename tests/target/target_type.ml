open Optitrust
open Target

let ty_double : typ_constraint =
  function { typ_desc = Typ_double; _ } -> true | _ -> false

let _ = Run.script_cpp (fun () ->

  (* This is a demo for the [hasType] constraints and variants *)

  (* Type of terms *)
  show [ nbExact 13; cFor "j"; cHasType "int" ]; (* TODO: not all marks are visible in the diff *)
  show [ nbExact 2; cHasTypePred ty_double ];

  (* Type of subterms *)
  show [ nbMulti; cSet ~typ:"int" () ];
  show [ nbMulti; cSet ~typ_pred:ty_double () ];
  show [ nbMulti; cVar ~typ:"int" "" ];
  show [ nbMulti; cVarDef ~typ:"int" "" ];
  show [ nbMulti; cVarDef ~typ_pred:ty_double "" ];

  (* Type of arguments *)
  show [ nbMulti; cFunDef ~args:[ [cArg ~typ:"int" ""]; [cArg ""] ] "" ];
  show [ nbMulti; cFunDef ~args_pred:(target_list_one_st [ cArg ~typ:"int" "" ]) "" ];

  (* TODO: direct constraints on arguments?
  show [ nbMulti; cArg ~typ:int ];
  show [ nbMulti; cArg ~typ_ast:(typ_double()) ]; *)
  )
