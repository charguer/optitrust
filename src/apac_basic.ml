open Ast
open Target


(* [use_goto_for_return mark]: *)
let use_goto_for_return ?(mark : mark = "") : Transfo.t =
  apply_on_targets (Apac_core.use_goto_for_return mark)

let insert_tasks (sa : sorted_arg_deps) (tg : target) : unit =
  let deps = [In sa.dep_in; Out sa.dep_out; Inout sa.dep_inout; Outin sa.dep_outin] in 
  transfo_on_targets (trm_add_pragma (Task [(Ast.Depend deps)])) tg



type taskable = (var * arg_deps) list  (* var is a mangled name. *)



let identify_taskable_functions (tg : target) : taskable = 
  let tg_trm = get_trm_at tg in 
  match tg_trm with 
  | Some t when trm_is_mainfile t -> 
    begin match trm_seq_inv t with 
    | Some tl -> 
      Mlist.fold_left (fun acc t -> 
        match t.desc with 
        | Trm_let_fun (qn, ty, args, body) -> 
          (qn.qvar_var, get_arg_dependencies t) :: acc
        | _ -> acc
       ) [] tl
    | None -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."
    end
  | _ -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."

  
  