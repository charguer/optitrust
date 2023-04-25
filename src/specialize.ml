open Ast
open Target
include Specialize_basic



let function_arg (spec_name : string) (args_to_keep : bool list) (tg : target) : unit = 
  Ast_data.fill_fun_defs_tbl (get_ast());
  iter_on_targets (fun t p -> 
    let tg_trm = Path.get_trm_at_path p t in
    match tg_trm.desc with 
    | Trm_apps ({desc = Trm_var (_, qf)} as call, args) -> 
      let opt_trms = List.map2 (fun arg arg_k -> 
        if not arg_k then Some arg else None 
      ) args args_to_keep in 
      
      
      
      let call_clang_id = Ast_data.get_cursor_of_trm_unsome call in 

      let clang_id = 
        match Ast_data.get_cursor_of_trm call with 
        | Some clang_id  -> 
          Some (Clang.get_cursor_referenced call_clang_id)
        | None -> None
          in
      Specialize_basic.funcalls spec_name args_to_keep (target_of_path p);
      
      Specialize_basic.fundefs spec_name opt_trms [cTopFunDef ~clang_id qf.qvar_var]
      
    | _ -> fail tg_trm.loc "Specialize.function_arg: expected a target to a function call."
  
  ) tg
