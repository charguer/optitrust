open Optitrust
open Target

let _ = Flags.execute_show_even_in_batch_mode := true

let _ = Run.script_cpp (fun () ->

  show [cFunDef "main"; dBody];
  (* find sequences of exactly 2 items, first with var def x, second with var def y *)
  show [ nbExact 2; cSeq ~args:[[cVarDef "x"]; [cVarDef "y"]] () ];
  (* find sequences of exactly 2 var defs *)
  show [ nbExact 2; cSeq ~args:[[cVarDef ""]; [cVarDef ""]] () ];
  (* find sequences of 3 instructions, the last one being a var def z *)
  show [ cSeq ~args:[[]; []; [cVarDef "z"]] () ];

  (* find all sequences *)
  show [ cSeq ()];

  (* find all sequences with at least one var def x *)
  show [ nbExact 5; cSeq ~args_pred:(target_list_one_st [cVarDef ""]) () ];
  (* find all sequences with only var defs *)
  show [ nbExact 3; cSeq ~args_pred:(target_list_all_st [cVarDef ""]) () ];

  show [ cSeq ~args_pred:(target_list_all_st [cVarDef ""]) () ];

  show [ cSeq ~args_pred:(target_list_one_st [cVarDef "x"])() ];

  show [ cSeq ~args_pred:(target_list_one_st [cVarDef "z"])() ];

  show [ cFunDef ~args_pred:(target_list_one_st [cArg "x"]) "" ];

  show [ cFun ~args_pred:(target_list_all_st [cLit ]) "" ];

  show [ cFun ~args_pred:(target_list_one_st [cLit ]) "" ];

)