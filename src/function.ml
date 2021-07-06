open Ast

let bind_intro ?(fresh_name : var = "a") ?(const : bool = true) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  Function_core.bind_intro i fresh_name const p_local t p)


let bind_args (fresh_names : var list) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
  (fun (p, p_local, i) t ->  
  (* Tools.printf "printed path %s\n" (Path.path_to_string p_local); *)
  Tools.foldi (fun n t fresh_name -> 
     if fresh_name <> "" then
     let ()  = counter := !counter+1 in 
     Function_core.bind_intro (i + !counter)  fresh_name true (p_local @ [Dir_arg n]) t p
     else t) t fresh_names)

(* TODO: Support better the case when the target depends on the context or on the argumetns *)
let bind (fresh_name : string) (inner_fresh_names : var list) (tg : Target.target) : unit =
  bind_args inner_fresh_names tg;
  bind_intro ~const:false ~fresh_name tg

let inline_call ?(name_result : var = "res") ?(label : var = "body") : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
   (fun (p, p_local, i) t -> 
    Function_core.inline_call i name_result label t p_local t p)

let elim_body (rename : string -> string): Target.Transfo.t = 
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t  -> Function_core.elim_body rename i t p)

let inline ?(name_result : string = "r") ?(label : string = "body") ?(rename : string -> string = fun s -> s ^ "1") (tg : Target.target) : unit =
  inline_call ~name_result ~label tg;
  elim_body rename [Target.cLabel label];
  Generic.var_init_attach [Target.cVarDef name_result];
  Variable.inline ~delete_decl:true [Target.cVarDef name_result];