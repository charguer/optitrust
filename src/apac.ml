
open Ast
open Target
include Apac_core
include Apac_basic

(* [parallel_task_group ~mark tg]: expects the target [ŧg] to point at a taskable function definition,
    then it will insert  #pragma omp parallel #pragma omp master #pragma omp taskgroup in front of that definition.*)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    List.iter (fun prg -> transfo_on_targets (trm_add_pragma prg) (target_of_path p)) [Taskgroup; Master; Parallel [] ]
)

(* [bind_taskable tsk tg]: expects the target [ŧg] to be pointing at a a sequence. 
    Then it will bind a variable to all the calls to the taskable functions [tsk]. 
    That are descendants of the trms associated to the target [tg]. *)
let bind_taskable_calls ?(indepth : bool = true) (tak : taskable) : Transfo.t =
  iter_on_targets (fun t p -> 
    
    let tg_trm = Path.get_trm_at_path p t in 

    (* get all the function names whose calls are descendants of tg_trm. *)
    let occ = get_fun_occurrences tg_trm in 
    let occ_functions = Hashtbl.fold (fun k v acc -> 
      match Hashtbl.find_opt tak k with 
      | Some _ -> k :: acc
      | None -> acc
    ) occ [] in 
    

    let fixed_tg = 
      if indepth then (target_of_path p ) @ [nbAny; cFuns occ_functions]
      else target_of_path p
      in
    iteri_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
      (fun i t (path_to_seq, local_path, i1)  -> 
        let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
        let path_to_call = path_to_instruction @ local_path in
        let tg_out_trm = Path.resolve_path path_to_instruction t in 
        let path_call_len = List.length local_path in
        let tg_call = target_of_path path_to_call in
       match tg_out_trm.desc with 
       | Trm_let (vk, _, _)  when path_call_len <= 2 -> 
           if vk = Var_mutable 
             then Variable.init_detach (target_of_path path_to_instruction)
             else ()
       | Trm_let _ -> Function.bind_intro ~const:false ~my_mark:"bind_tskbl" ~fresh_name:("res__" ^ (string_of_int i1)) tg_call;
                      begin try Variable.init_detach [cVarDef "" ~body:[cMark "bind_tskbl"]] with | TransfoError _ -> () end; 
                      Marks.remove "bind_tskbl" [nbAny;cMark "bind_tskbl"]
       
       | Trm_apps (_,[ls; rhs]) when is_set_operation tg_out_trm -> 
           if path_call_len >= 2 
             then  Function.bind_intro ~const:false ~fresh_name:("res__" ^ (string_of_int i1)) tg_call
             else ()
       | Trm_apps _ when path_call_len = 0 -> ()
       
       | _ -> fail tg_out_trm.loc "Apac_basic.bind_taskable_calls: the main target should either a function call, or any trm that 
                  contains some function calls provided that the argument [indepth] is set to true. "
     ) fixed_tg
)


(* let insert_tasks_for_taskable ?(indepth : bool = true) (tsk : taskable) : Transfo.t =
  iter_on_targets ( fun t p -> 
    let tg_trm = Path.get_trm_at_path p t in 
    match tg_trm.desc with 
    | Trm_apps ({desc = Trm_var (_, qv); _}, _) -> 

    | _ -> 

  )  *)


type vars_arg = int String_map.t
type arg_const = {
  mutable is_const : bool;
  mutable dependency_of : (string * int) list;
}
type args_const = arg_const list
type fun_args_const = (string, args_const) Hashtbl.t

let rec get_binop_set_left_var (t : trm) : trm option =
  match t.desc with
  | Trm_var _ -> Some(t)
  | Trm_apps (_, args) ->
    begin match List.nth_opt args 0 with
    | Some(t) ->get_binop_set_left_var t
    | None -> None
    end
  | _ -> None

(* wip*)
let constify : unit = 
  let fac : fun_args_const = Hashtbl.create 10 in 

  let _init_fac (fac : fun_args_const) : Transfo.t =
    iter_on_targets(fun t p ->
      let tg_trm = Path.get_trm_at_path p t in
      match tg_trm.desc with
      | Trm_let_fun (qv, _, args, _) -> 
        let acs : args_const = List.init (List.length args) 
        (fun _ -> { is_const = true; dependency_of = [] }) in
        Hashtbl.add fac qv.qvar_str acs
      | _ -> fail None "Expect function definition"
    )
  in

  (* TODO : [va] not update in trm_iter*)
  let rec update_fac_dep_of_aux (va : vars_arg) (cur_fun : string) (t : trm) : unit =
    match t.desc with
    | Trm_seq _ -> trm_iter (update_fac_dep_of_aux va cur_fun) t
    (* funcall *)
    | Trm_apps ({ desc = Trm_var (_ , fname); _ }, args) -> 
      List.iteri (fun i t -> 
        match t.desc with
        | Trm_var (_, arg_name) ->
            begin match String_map.find_opt arg_name.qvar_str va with
            | Some(j) -> 
              let acs = Hashtbl.find fac fname.qvar_str in
              let ac = List.nth acs (i-1) in 
              ac.dependency_of <- (fname.qvar_str, j) :: ac.dependency_of ;
            | None -> ()
            end
        | _ -> ()) args;
      trm_iter (update_fac_dep_of_aux va cur_fun) t
    (* ref/ptr assign *)
    | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [{ desc = Trm_var (_, rname); _ }]); _ }) when is_reference ty || is_typ_ptr (get_inner_const_type ty) ->
      begin match String_map.find_opt rname.qvar_str va with 
      | Some(i) -> trm_iter (update_fac_dep_of_aux (String_map.add lname i va) cur_fun) t
      | None -> trm_iter (update_fac_dep_of_aux va cur_fun) t 
      end
    | _ -> trm_iter (update_fac_dep_of_aux va cur_fun) t
  in
  
  let _update_fac_dep_of : Transfo.t =
    iter_on_targets(fun t p ->
      let tg_trm = Path.get_trm_at_path p t in
      match tg_trm.desc with
      | Trm_let_fun (qv, _, args, body) ->
        let (va, _) = List.fold_left (fun (va, i) (name, _) ->
          if name = "" then (va,i+1) else (String_map.add name i va, i+1)) (String_map.empty, 0) args in
        trm_iter (update_fac_dep_of_aux va (qv.qvar_str)) body
      | _ -> fail None "Expect function definition"
    )
  in

  let rec _unconstify_propagate (to_process : (string * int) list) : unit =
    match to_process with
    | [] -> ()
    | (fname, nth) :: t -> 
      let acs : args_const = Hashtbl.find fac fname in
      let ac : arg_const = List.nth acs nth in
      if ac.is_const 
        then begin ac.is_const <- false; _unconstify_propagate (List.rev_append to_process ac.dependency_of) end
        else _unconstify_propagate t
  in

  let unconstify_aux (t : trm) : unit =
    ()
  in

  let _unconstify : Transfo.t =
    iter_on_targets(fun t p ->
      let tg_trm = Path.get_trm_at_path p t in
      match tg_trm.desc with
      | Trm_let_fun (qv, _, _, body) -> 
        trm_iter (unconstify_aux) body
      | _ -> fail None "Expect function definition"
    )
  in

  (* TODO : assign *)
  (* | Trm_apps (_, [ls; rhs]) when is_set_operation t -> 
    begin match get_binop_left_var ls with
    | Some({desc=Trm_var (_, name); _}) when String_map.mem name.qvar_str va -> ()
    | _ -> ()
    end *)

(*   
  init_fac fac [nbMulti; cTopFunDefAndDecl ""];
  update_fac_dep_of [nbMulti; cTopFunDefAndDecl ""]; *)
  ()