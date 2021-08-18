open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  show [ nbMulti; cSeq ~args:[cVarDef "x"; cVarDef "y"] () ];
  show [ nbMulti; cSeq ~args_pred:(target_list_all_st (cVarDef "")) () ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st (cVarDef "x"))() ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st (cVarDef "z"))() ];
  show [ nbMulti; cFunDef ~args_pred:(target_list_one_st (cVar "x")) "" ];
  show [nbMulti; cFun ~args_pred:(target_list_all_st (cLit )) "" ];
  show [nbMulti; cFun ~args_pred:(target_list_one_st (cLit )) "" ];
  
  )