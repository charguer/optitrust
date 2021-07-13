open Ast
open Path
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

let counter = ref (-1)

let bind1 (fresh_name : string) (inner_fresh_names : var list) (bind_args : bool): Target.Transfo.t  =
  Target.apply_on_transformed_targets(Generic_core.get_call_in_surrounding_seq)
    (fun (p, p_local, i) t ->
     if not bind_args then Function_core.bind_intro i fresh_name false p_local t p
     else
     let t = Function_core.bind_intro i fresh_name false p_local t p in
     Tools.foldi (fun n t fresh_name ->
     if fresh_name <> "" then
     let ()  = counter := !counter+1 in
     Function_core.bind_intro (i + !counter)  fresh_name true ([Dir_body] @ [Dir_arg 0 ] @ [Dir_arg n]) t p
     else t) t inner_fresh_names)



let bind (fresh_name : string) (inner_fresh_names : var list) (tg : Target.target) : unit =
  bind_args inner_fresh_names tg;
  bind_intro ~const:false ~fresh_name tg

let inline_call  ?(label : var = "body") : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
   (fun (p, p_local, i) t ->
    Function_core.inline_call i label t p_local t p)

let elim_body (rename : string -> string): Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t  -> Function_core.elim_body rename i t p)

let  inline ?(name_result : string = "") ?(label : string = "body") ?(rename : string -> string = fun s -> s ^ "1") ?(bind_args : bool = false) ?(inner_fresh_names : var list = []) (tg : Target.target) : unit =
  bind1 name_result inner_fresh_names bind_args tg;
  inline_call ~label tg;
  elim_body rename [Target.cLabel label];
  if name_result <> ""
    then begin
         Generic.var_init_attach [Target.cVarDef name_result];
         Variable.inline ~delete_decl:true [Target.cVarDef name_result];
         if List.length inner_fresh_names = 0
          then () else List.iter (fun binded_arg ->
            if binded_arg <> ""
              then (Variable.inline ~delete_decl:true [Target.cVarDef binded_arg])
              else ()) inner_fresh_names
         end
    else ()
    (**)

let smart_inline ?(name_result : string = "") ?(label : string = "body") ?(rename : string -> string = fun s -> s ^ "1") ?(inner_fresh_names : var list) (tg : target) : unit = 
  Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
    (fun (p, p_local, i) t ->
      (* Counter needed to keep track on the change of indices *)
      let counter = ref (-1) in
      let (tg_trm, _) = Path.resolve_path (p @ [Dir_seq_nth i] @ p_local) t in
      let (tg_out_trm, _) = Path.resolve_path (p @ [Dir_seq_nth i]) t in
      let bind_res_needed = (* Checking the type of the target *)
      begin match tg_trm.desc with 
      (* Instruction of the form int r = f(..) *)
      | Trm_let (vk, (x, tx), init) -> 
        if name_result <> "" && name_result <> x then fail tg_trm.loc "smart_inline: no need to enter the result name in this case"
          else false
      (* A function call f(..) *)
      | Trm_apps(f, [base]) -> if name_result <> "" then fail tg_trm.loc "smart_inline: no need to enter the result name for void functions" 
          else
          begin match tg_out_trm.desc with 
          | Trm_let (_, (_, _), {desc = Trm_apps(f, [base]);_}) when base = tg_trm-> false
          | Trm_apps _ when tg_trm = tg_out_trm -> false
          | _ -> fail tg_out_trm.loc "smart_inline: expected a variable declaration of a function call, none where provided"
          end
      | _ -> true
      end in
      let t = begin match bind_res_needed with 
              | true -> if name_result = "" then 
                          let rnd_nb = Random.int 1000 in let nam_result = "temp" ^ (string_of_int rnd_nb) in
                          Function_core.bind_intro i name_result false p_local t p 
                        else 
                          Function_core.bind_intro i name_result false p_local t p

              | false -> t 
              end in
      let nb_args_to_bind = (List.length (List.filter (fun x -> x <> "") inner_fresh_names)) in
      let bind_args = match nb_args_to_bind with
                      | 0 -> false
                      | _ -> true  in
      let t = if bind_args then Tools.foldi (fun n t fresh_name ->
                if fresh_name <> "" then
                  let () = counter := !counter+1 in
                  Function_core.bind_intro (i + !counter) fresh_name true ([Dir_body] @ [Dir_arg 0] @ [Dir_arg n]) t p
                else t) t inner_fresh_names
              else t in
      (* TODO: Fix me!!*)
      let t = Function_core.inline_call (i + !counter + 1) label t (List.tl p_local) t p in
      let t = Function_core.elim_body reanme (i + !counter + 2) t p in
      if bind_res_needed 
        then let t = Generic_core.var_init_attach false (i + nb_args_to_bind) t p in
            let t = Variable_core.inline true [[]] (i + nb_args_to_bind) t p in
          if (List.length inner_fresh_names) = 0
            then t 
            else Tools.foldi (fun n t1 _ -> Variable_core.inline true [[]] (i + n) t1 p) t (List.filter (fun x -> x <> "") inner_fresh_names)
        else t
    ) tg
