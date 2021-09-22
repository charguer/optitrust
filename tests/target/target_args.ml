open Optitrust
open Target

(* TODO: Fix cSeq *)

let _ = Run.script_cpp (fun () ->
  
  (* find sequences of exactly 2 items, first with var def x, second with var def y *)
  show [ nbExact 2; cSeq ~args:[[cVarDef "x"]; [cVarDef "y"]] () ];
  (* find sequences of exactly 2 var defs *)
  show [ nbExact 2; cSeq ~args:[[cVarDef ""]; [cVarDef ""]] () ];
  (* find sequences of 3 instructions, the last one being a var def z *)
  show [ cSeq ~args:[[]; []; [cVarDef "z"]] () ];

  (* find all sequences *)
  show [ cSeq () ];
  (* find all sequences with at least one var def x *)
  show [ nbExact 5; cSeq ~args_pred:(target_list_one_st [cVarDef ""]) () ];
(* find all sequences with only var defs *)
  (* show [ nbExact 3; cSeq ~args_pred:(target_list_all_st [cVarDef ""]) () ]; *)


  (* TODO
  show [ nbMulti; cSeq ~args_pred:(target_list_all_st [cVarDef ""]) () ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st [cVarDef "x"])() ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st [cVarDef "z"])() ];
  show [ nbMulti; cFunDef ~args_pred:(target_list_one_st [cVar "x"]) "" ];
  show [nbMulti; cFun ~args_pred:(target_list_all_st [cLit ]) "" ];
  show [nbMulti; cFun ~args_pred:(target_list_one_st [cLit ]) "" ];
  *)
  )