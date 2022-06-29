
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


type vars_arg = (string, int) Hashtbl.t
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
let constify : Transfo.t = 
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let fac : fun_args_const = Hashtbl.create 10 in 
    let to_process = Stack.create () in

    (* get the list of function declarations trms *)
    let fun_decls : trms = match tg_trm.desc with
    | Trm_seq ml -> Mlist.fold_left (fun acc t -> 
      match t.desc with
      | Trm_let_fun _ -> t :: acc
      | _ -> acc 
      ) [] ml
    | _ -> fail None "Expect dRoot target"
    in

    (* init fac *)
    List.iter (fun t ->
      match t.desc with
      | Trm_let_fun (qv, _, args, _) ->
        Printf.printf "ok\n";
        let acs : args_const = List.init (List.length args) 
        (fun _ -> { is_const = true; dependency_of = [] }) in
        Hashtbl.add fac qv.qvar_str acs
      | _ -> fail None "Should not happen"
      ) fun_decls;


    (* update fac dependency_of *)
    (* TODO : handle other new scope if, while ... *)
    let rec update_fac_dep_of_aux (va : vars_arg) (new_scope : bool) (cur_fun : string) (t : trm) : unit =
      let va = if new_scope then Hashtbl.copy va else va in
      match t.desc with
      (* new scope *)
      | Trm_seq _ -> trm_iter (update_fac_dep_of_aux va true cur_fun) t
      (* funcall *)
      | Trm_apps ({ desc = Trm_var (_ , fname); _ }, args) when Hashtbl.mem fac fname.qvar_str -> 
        List.iteri (fun i t -> 
          match t.desc with
          (* TODO : add only if it is ref or ptr in funcall def *)
          | Trm_var (_, arg_name) ->
              begin match Hashtbl.find_opt va arg_name.qvar_str with
              | Some(j) -> 
                let acs = Hashtbl.find fac fname.qvar_str in
                let ac = List.nth acs (i-1) in 
                ac.dependency_of <- (fname.qvar_str, j) :: ac.dependency_of ;
              | None -> ()
              end
          | _ -> ()) args;
        trm_iter (update_fac_dep_of_aux va false cur_fun) t
      (* ref/ptr assign *)
      (* ignore the first ptr when *)
      | Trm_let (_, (lname, ty), { desc = Trm_apps (_, [{ desc = Trm_var (_, rname); _ }]); _ }) when is_reference ty || is_typ_ptr (get_inner_const_type ty) ->
        begin match Hashtbl.find_opt va rname.qvar_str with 
        | Some(i) -> Hashtbl.add va lname i; trm_iter (update_fac_dep_of_aux va false cur_fun) t
        | None -> trm_iter (update_fac_dep_of_aux va false cur_fun) t 
        end
      | _ -> trm_iter (update_fac_dep_of_aux va false cur_fun) t
    in

    List.iter (fun t ->
      let va = Hashtbl.create 10 in
      match t.desc with
      | Trm_let_fun (qv, _, args, body) ->
        List.iteri (fun i (name, _) -> if name <> "" then Hashtbl.add va name i) args ;
        trm_iter (update_fac_dep_of_aux va false (qv.qvar_str)) body
      | _ -> fail None "Should not happen"
      ) fun_decls;

    
    (* unconstify *)
    (* TODO *)
    let rec unconstify_aux (to_process : (string * int) Stack.t) (va : vars_arg) (new_scope : bool) (t : trm) : unit =
      let va = if new_scope then Hashtbl.copy va else va in
      match t.desc with
      | Trm_seq _ -> trm_iter (unconstify_aux to_process va true) t
      | Trm_apps (_, [ls; rhs]) when is_set_operation t -> 
        begin match get_binop_set_left_var ls with
        | Some({ desc=Trm_var (_, name); _ }) when Hashtbl.mem va name.qvar_str-> ()
        | _ -> ()
        end 
      | _ -> trm_iter (unconstify_aux to_process va false) t
    in

    List.iter (fun t ->
      let va = Hashtbl.create 10 in
      match t.desc with
      | Trm_let_fun (qv, _, _, body) -> 
        trm_iter (unconstify_aux to_process va false) body
      | _ -> fail None "Expect function definition"
      ) fun_decls;
    
    let rec unconstify_propagate (to_process : (string * int) Stack.t) : unit =
      match Stack.pop_opt to_process with
      | None -> ()
      | Some (fname, nth) -> 
        let acs : args_const = Hashtbl.find fac fname in
        let ac : arg_const = List.nth acs nth in
        if ac.is_const then begin ac.is_const <- false; List.iter (fun e -> Stack.push e to_process) ac.dependency_of end;
        unconstify_propagate to_process
    in
    unconstify_propagate to_process;

    (* TODO : make change in the ast *)
    ()
  )

  (* 

  (* TODO : assign *)
  (* | Trm_apps (_, [ls; rhs]) when is_set_operation t -> 
    begin match get_binop_left_var ls with
    | Some({desc=Trm_var (_, name); _}) when String_map.mem name.qvar_str va -> ()
    | _ -> ()
    end *)
  *)
