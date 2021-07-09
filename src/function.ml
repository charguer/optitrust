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

let [@warning "-47"] inline ?(name_result : string = "") ?(label : string = "body") ?(rename : string -> string = fun s -> s ^ "1") ?(bind_args : bool = false) ?(inner_fresh_names : var list = []) (tg : Target.target) : unit =
  (*bind1 name_result inner_fresh_names bind_args tg;*)
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

let smart_inline ?(name_result : string = "") ?(label : string = "body") ?(rename : string -> string = fun s -> s ^ "1") ?(bind_args : bool = false) ?(inner_fresh_names : var list = []) (tg : Target.target) : unit =
  Target.apply_on_transformed_targets (Generic_core.get_call_in_surrounding_seq)
    (fun (p, p_local, i) t ->
      let t = Function_core.bind_intro i name_result false p_local t p in
      let t = if bind_args then Tools.foldi (fun n t fresh_name ->
        if fresh_name <> "" then
        let () = counter := !counter+1 in
        Function_core.bind_intro (i + !counter) fresh_name true ([Dir_body] @ [Dir_arg 0] @ [Dir_arg n]) t p
        else t) t inner_fresh_names
        else t in
      let t = Function_core.inline_call (i + !counter+1) label t p_local t p in
      let t = Function_core.elim_body rename (i + !counter + 2) t p in
      if name_result <> "" then
          let t = Generic_core.var_init_attach  false (i+(List.length (List.filter (fun x -> x <> "") inner_fresh_names))) t p in
          let t = Variable_core.inline true [[]] (i+(List.length (List.filter (fun x -> x <> "") inner_fresh_names))) t p in
          if  (List.length inner_fresh_names) = 0
            then t
            else Tools.foldi (fun n t1 _ ->  Variable_core.inline true [[]] (i + n) t1 p) t (List.filter (fun x -> x <> "") inner_fresh_names)
        else t
    ) tg