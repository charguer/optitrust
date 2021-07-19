open Optitrust
open Target

let _ = Run.script_cpp (fun () ->
  show [ nbMulti; cSeq ~args:[cVarDef "x"; cVarDef "y"] () ];
  show [ nbMulti; cSeq ~args_pred:(target_list_all_st (cVarDef "")) () ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st (cVarDef "x"))() ];
  show [ nbMulti; cSeq ~args_pred:(target_list_one_st (cVarDef "z"))() ];
  (* TODO: add a few more tests, including cFunDef
      - at least one argument named x
      - only arguments of type int
    and cFun with
      - only litteral as arguments
      - at least one litteral as argument


     matching in depth:
      f(a1, ...g(..) ..., a3)
      => cFun ~args_pred:(target_list_one_st (cTargetInDepth [cFun "g"])
      TODO: define this:
         let cTargetInDepth (tg : target) : constr =
            Const_chain (Constr_depth DepthAny :: tg)
    *)
  )