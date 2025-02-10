open Optitrust
open Prelude
let show = Show.add_marks_for_target_unit_tests


let _ = Run.script_cpp (fun () ->

  !! show [cFunBody "main"];
  (* find sequences of exactly 2 items, first with var def x, second with var def y *)
  !! show [ nbExact 2; cSeq ~instrs:[[cVarDef "x"]; [cVarDef "y"]] () ];
  (* find sequences of exactly 2 var defs *)
  !! show [ nbExact 2; cSeq ~instrs:[[cVarDef ""]; [cVarDef ""]] () ];
  (* find sequences of 3 instructions, the last one being a var def z *)
  !! show [ cSeq ~instrs:[[]; []; [cVarDef "z"]] () ];

  (* find all sequences *)
  !! show [ cSeq ()];

  (* find all sequences with at least one var def x *)
  !! show [ cSeq ~instrs_pred:(target_list_one_st [cVarDef ""]) () ];
  (* find all sequences with only var defs *)
  !! show [ cSeq ~instrs_pred:(target_list_all_st [cVarDef ""]) () ];

  !! show [ cSeq ~instrs_pred:(target_list_one_st [cVarDef "x"])() ];

  !! show [ cSeq ~instrs_pred:(target_list_one_st [cVarDef "z"])() ];

  !! show [ cFunDef ~args_pred:(target_list_one_st [cArg "x"]) "" ];

  (* Be careful this matches function calls with zero arguments, such as hidden calls to ref *)
  !! show [ cCall ~args_pred:(target_list_all_st [cStrictNew; cLit]) "" ];

  !! show [ cCall ~args_pred:(target_list_one_st [cStrictNew; cLit]) "" ];

)
