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

let bind (fresh_name : string) (inner_fresh_names : var list) (tg : Target.target) : unit =
  bind_args inner_fresh_names tg;
  bind_intro ~const:false ~fresh_name tg;



let inline_call ?(name : string = "result") ?(label : string = "body") ?(renames: string -> string  = fun s -> s ^ "1" ) : Target.Transfo.t =
  Target.apply_on_transformed_targets(Generic_core.get_call_in_surrounding_seq)
   (fun (p, p_local, i) t -> 
    (* Needed for finding the declaration of the function *)
    let top_path = Constr.resolve_target [cRoot] t in
    let top_ast, _ = Path.resolve_path top_path t in
    Function_core.inline_call i name label renames p_local top_ast t p)

