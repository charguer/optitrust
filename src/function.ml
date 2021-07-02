open Ast


let bind_intro ?(fresh_name : var = "a") : Target.Transfo.t =
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  Function_core.bind_intro i fresh_name p_local t p)


let bind_args (fresh_names : var list) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  
  Tools.foldi (fun n t fresh_name -> 
     let p_local = if fresh_name <> "" then p_local @ [Dir_nth n] else p_local in
     Function_core.bind_intro i fresh_name p_local t p) t fresh_names)





