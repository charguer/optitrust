open Ast
open Target


(* [use_goto_for_return mark]: *)
let use_goto_for_return ?(mark : mark = "") : Transfo.t =
  apply_on_targets (Apac_core.use_goto_for_return mark)

let insert_task (sa : sorted_arg_deps) (tg : target) : unit =
  let deps = [In sa.dep_in; Out sa.dep_out; Inout sa.dep_inout; Outin sa.dep_outin] in 
  transfo_on_targets (trm_add_pragma (Task [(Ast.Depend deps)])) tg



type taskable = (var * arg_deps) list  (* var is a mangled name. *)


(* Note: Naive implementation. *)
let identify_taskable_functions (tg : target) : taskable = 
  let tg_trm = get_trm_at tg in 
  match tg_trm with 
  | Some t when trm_is_mainfile t -> 
    begin match trm_seq_inv t with 
    | Some tl -> 
      Mlist.fold_left (fun acc t -> 
        match t.desc with 
        | Trm_let_fun (qn, ty, args, body) when qn.qvar_var <> "main" -> 
          (qn.qvar_var, get_arg_dependencies t) :: acc
        | _ -> acc
       ) [] tl
    | None -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."
    end
  | _ -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."

  
(* let bind_taskable_calls (tsk : taskable) (tg : target) : unit =
  iter_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun i t (path_to_seq, local_path, i1)  -> 
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
      let path_to_call = path_to_instruction @ local_path in
      let call = Path.get_trm_at_path path_to_call in 
      match call.desc with 
      | Trm_apps ()
      
      
    ) tg *)