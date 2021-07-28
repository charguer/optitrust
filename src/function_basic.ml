open Ast


let bind_intro ?(fresh_name : var = "a") ?(const : bool = true) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
  (fun (p, p_local, i) t ->  Function_core.bind_intro i fresh_name const p_local t p)

let inline_call  ?(label : var = "body") : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
   (fun (p, p_local, i) t ->
    Function_core.inline_call i label t p_local t p)

let elim_body (rename : string -> string) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t  -> Function_core.elim_body rename i t p)



