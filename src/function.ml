open Ast
open Path
let counter = ref (-1)

let bind_args (fresh_names : var list) : Target.Transfo.t =
 let counter = ref (-1) in
 Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
  (fun (p, p_local, i) t ->
   if List.length fresh_names = 0 then t 
    else 
      Tools.foldi (fun n t fresh_name ->
      if fresh_name <> "" then
        let ()  = counter := !counter+1 in
        Function_core.bind_intro (i + !counter)  fresh_name true (p_local @ [Dir_arg n]) t p
      else t) t fresh_names)


let elim_body ?(renames : rename = Postfix "") (tg : Target.target) : unit =
  let tg_body = if List.mem Target.dBody tg then tg else (tg @ [Target.dBody]) in
  Variable_basic.rename renames tg_body;
  Sequence_basic.elim tg


let bind ?(fresh_name : string = "res") ?(inner_fresh_names : var list =  []) (tg : Target.target) : unit =
  bind_args inner_fresh_names tg;
  Function_basic.bind_intro ~const:false ~fresh_name tg


let bind1 (fresh_name : string) (inner_fresh_names : var list) (bind_args : bool): Target.Transfo.t  =
  Target.apply_on_transformed_targets(Internal.get_call_in_surrounding_sequence)
    (fun (p, p_local, i) t ->
     if not bind_args then Function_core.bind_intro i fresh_name false p_local t p
     else
     let t = Function_core.bind_intro i fresh_name false p_local t p in
     Tools.foldi (fun n t fresh_name ->
     if fresh_name <> "" then
     let ()  = counter := !counter+1 in
     Function_core.bind_intro (i + !counter)  fresh_name true ([Dir_body] @ [Dir_arg 0 ] @ [Dir_arg n]) t p
     else t) t inner_fresh_names)

let inline_call ?(name_result = "") ?(label:var = "body") ?(renames : rename = Postfix "1") ?(inner_fresh_names : var list = []) ?(_no_control_structures : bool = true) (tg : Target.target) : unit =
  let t = Trace.get_ast() in
  let name_result = ref name_result in
  let tg_path = Target.resolve_target_exactly_one tg t in
  let (path_to_seq,local_path, i) = Internal.get_call_in_surrounding_sequence tg_path in
  let (tg_trm, _) = Path.resolve_path (path_to_seq @ [Dir_seq_nth i] @ local_path) t in
  let (tg_out_trm, _) = Path.resolve_path (path_to_seq @ [Dir_seq_nth i]) t in
  let inlining_needed = 
  begin match tg_out_trm.desc with 
  | Trm_let (_, (x, _), _) -> 
    let init1 = get_initializatin_trm tg_out_trm in
    if !name_result <> "" && init1 = tg_trm then fail tg_trm.loc "inline_call: no need to enter the result name in this case"
      else if init1 = tg_trm then begin name_result := x; false end
      else
           begin match !name_result with 
           | "" ->  
              let rnd_nb = Random.int 1000 in 
              name_result := ("temp" ^ (string_of_int rnd_nb));true
           | _ -> Function_basic.bind_intro ~fresh_name:!name_result tg;true
           end
  | Trm_apps _ -> ();false
  | _ -> fail None "inline_call: expected a variable declaration or a function call"
  end in
  if List.length inner_fresh_names <> 0 then bind_args inner_fresh_names tg else (); 
  Function_basic.inline_call ~label tg;
  elim_body ~renames [Target.cLabel label];
   if List.length inner_fresh_names <> 0 
    then List.iter (fun x -> Variable_basic.inline ~delete:true [Target.cVarDef x]) (List.filter (fun x -> x <> "")inner_fresh_names) 
    else ();
  if _no_control_structures && (!name_result <> "") 
    then 
      begin 
      Variable_basic.init_attach [Target.cVarDef !name_result];
      if inlining_needed then Variable_basic.inline ~delete:true [Target.cVarDef !name_result] else ()
      end
    else () 
