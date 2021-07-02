open Ast


let bind_intro ?(fresh_name : var = "a") : Target.Transfo.t =
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  Function_core.bind_intro i fresh_name p_local t p)


let bind_args (fresh_names : var list) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  
  (* Tools.printf "printed path %s\n" (Path.path_to_string p_local); *)
  Tools.foldi (fun n t fresh_name -> 
     if fresh_name <> "" then
     let ()  = counter := !counter+1 in 
     Function_core.bind_intro (i + !counter)  fresh_name (p_local @ [Dir_arg n]) t p
     else t) t fresh_names)






