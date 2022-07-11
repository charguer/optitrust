
open Ast
open Target
include Apac_core
include Apac_basic

(* [parallel_task_group ~mark tg]: expects the target [ŧg] to point at a taskable function definition,
    then it will insert  #pragma omp parallel #pragma omp master #pragma omp taskgroup in front of that definition.*)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    transfo_on_targets (trm_add_pragmas [Parallel []; Master ; Taskgroup]) (target_of_path p))


(* [insert_task sag tg]: expects the target [tg] to be pointing at an instruction or a sequence.
    then based on [sag] it will insert an OpenMP directive on the trm that [tg] points to. *)
let insert_task (sag : sorted_arg_deps) : Transfo.t =
  iter_on_targets (fun t p -> 
    let dl = match sag.dep_in with [] -> [] | _ -> [In sag.dep_in] in 
    let dl = match sag.dep_out with [] -> dl | _ ->  (Out sag.dep_out) :: dl in
    let dl = match sag.dep_inout with [] -> dl | _ -> (Inout sag.dep_inout) :: dl in 
    let dl = match sag.dep_outin with [] -> dl | _ -> (Outin sag.dep_outin) :: dl in 
    let dl = match sag.dep_sink with [] -> dl | _ -> (Sink sag.dep_sink) :: dl in 
    let dl = match sag.dep_source with [] -> dl | _ -> Source :: dl in 
    Omp_basic.task ~clause:[Depend dl] (target_of_path p))

(* [bind_taskable tsk tg]: expects the target [ŧg] to be pointing at a a sequence. 
    Then it will bind a variable to all the calls to the taskable functions [tsk]. 
    That are descendants of the trms associated to the target [tg]. *)
let bind_taskable_calls ?(indepth : bool = true) (tak : taskable) : Transfo.t =
  iter_on_targets (fun t p -> 
    
    let tg_trm = Path.get_trm_at_path p t in 

    (* get all the function names whose calls are descendants of tg_trm. *)
    let occ = get_fun_occurrences tg_trm in 
    let occ_functions = Tools.hashtbl_keys_to_list occ in 
    
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

(* [insert_tasks_for_taskable ~indepth tsk tg]: expects the target [tg] to be pointing at a function call 
      of the full file ast. If the targeted trm is a function call to a taskable function, then
        it considers first the arg_deps of that function, then it will replace the arguments from the definition with 
        the ones from the call and sort them. Finally it will call [inser_task] transformation to insert an OpenMP
        pragma just before the instruction that contains the targeted function call.
      [indepth] - controls whether to recurse in all the subterms, *)
let insert_tasks_for_taskable ?(indepth : bool = false) (tsk : taskable) (tg : target) : unit = 
  let fun_arg_deps = get_function_defs () in 
  let occ_functions = Tools.hashtbl_keys_to_list fun_arg_deps in 
  let fixed_tg = 
        if indepth 
          then tg @ [nbAny; cFuns occ_functions] 
          else tg
        in
  iter_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence) 
    (fun t (path_to_seq, local_path, i1) -> 
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in 
      let path_to_call = path_to_instruction @ local_path in 
      let call_trm = Path.get_trm_at_path path_to_call t in
      match call_trm.desc with 
      | Trm_apps ({desc = Trm_var (_, qn); _}, call_args) ->
        begin match Hashtbl.find_opt fun_arg_deps qn.qvar_var with 
        | Some arg_deps -> 
          (* replacing function definition args with function call args. *)
          let upd_arg_deps = List.map2 (fun arg_dep call_arg -> 
            begin match (get_operation_arg call_arg).desc with 
            | Trm_var (_, qn) -> {arg_dep with arg_dep_var = qn.qvar_var}
            | _ -> arg_dep
            end
          ) arg_deps call_args in 
          let srt_arg_deps = sort_arg_dependencies upd_arg_deps in 
          insert_task srt_arg_deps (target_of_path path_to_instruction)
        | None -> ()
        end
      | _ -> ()
  ) fixed_tg


(* [vars_arg]: hashtable that stores variables that refer to an argument and the pointer depth of that argument.
    A list is used because of how dependencies of a new pointer are checked :
    we currently take all the variables refering to the argument on the left side of the write operation. *)
type vars_arg = (string, (int list * int)) Hashtbl.t

(* [arg_const]: record that stores information to constify or not the argument
    and other arguments that depend on this argument. *)
type arg_const = {
  is_ptr_or_ref : bool;
  mutable is_const : bool;
  mutable dependency_of : (string * int) list;
}

(* [args_const]: a list of args_const *)
type args_const = arg_const list

(* [fun_args_const]: hashtable that store the [args_const] of functions and the pointer depth of the return type. *)
type fun_args_const = (string, (args_const * int)) Hashtbl.t

(* [get_binop_set_left_var_opt t]: returns the variable on the left side of the . *)
let get_binop_set_left_var_opt (t : trm) : trm option =
  let rec aux (t : trm) : trm option =
    match t.desc with
    | Trm_var _ -> Some(t)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _ }, [t; _]) -> aux t
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address | Unop_struct_access _))); _ }, [t]) -> aux t
    | _ -> None
  in

  match t.desc with
  | Trm_apps (_, [ls; _]) when is_set_operation t -> aux ls
  | _ -> None

(* [get_args_idx_in_apps va t]: returns a list of argument's index which are used in Trm_apps recursively. *)
let get_args_idx_in_apps (va : vars_arg) (t : trm) : int list =
  let rec aux (acc : int list) (t : trm) : int list =
      match t.desc with
      | Trm_apps (_, ts) -> List.fold_left aux acc ts
      | Trm_var (_, qv) when Hashtbl.mem va qv.qvar_str -> 
        let (l, _) = Hashtbl.find va qv.qvar_str in
        List.rev_append l acc
      | _ -> acc
  in
  aux [] t

(* [is_unary_mutation t]: checks if [t] is a primitive unary operaiton that mutates a variable. *)
let is_unary_mutation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, _) ->
    begin match uo with 
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> true
    | _ -> false
    end
  | _ -> false

(* [get_unary_mutation_qvar t]: return the fully qualified name of the variable behind unary mutatation operator. *)
let get_unary_mutation_qvar (t : trm) : qvar =
  let rec aux (t : trm) : qvar =
    match t.desc with 
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (Unop_get | Unop_address))); _}, [t]) -> aux t
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _}, [t; _]) -> aux t
    | Trm_var (_, name)-> name
    | _ -> empty_qvar
  in

  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, [tr]) ->
    begin match uo with
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> aux tr
    | _ -> empty_qvar
    end
  | _ -> empty_qvar

(* [get_cptr_depth ty]: returns the number of C pointer of the type [ty]. *)
let get_cptr_depth (ty : typ) : int =
  let rec aux (depth : int) (ty : typ) : int =
    match ty.typ_desc with
    | Typ_const ty -> aux depth ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux (depth) ty 
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (depth+1) ty
    | Typ_array (ty, _) -> aux (depth+1) ty
    | Typ_constr _ when Apac_core.is_typdef_alias ty -> 
      begin match Apac_core.get_inner_typedef_alias ty with
      | Some (ty) -> aux depth ty
      | None -> assert false
      end
    | _ -> depth
  in
  aux 0 ty
    
(* [constify_functions_arguments tg]: expect target [tg] to point at the root,
    then it will add "const" keyword whenever it is possible in functions arguments *) 
(* wip*)
let constify_functions_arguments : Transfo.t = 
  (* TODO : handle include file *)
  (* TODO : handle return ref/ptr argument *)
  (* TODO : handle namespace *)
  (* TODO : handle let_mult *)
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let fac : fun_args_const = Hashtbl.create 10 in 
    (* store the function's arguments (fname, nth arg) to unconst *)
    let to_process : (string * int) Stack.t = Stack.create () in

    (* get the list of function declarations trms *)
    let fun_decls : trms = match tg_trm.desc with
    | Trm_seq ml -> Mlist.fold_left (fun acc t -> 
      match t.desc with
      | Trm_let_fun _ -> t :: acc
      | _ -> acc 
      ) [] ml
    | _ -> fail None "Expect dRoot target"
    in

    (* helper function : add element to process in to_process *)
    let add_elt_in_to_process (va : vars_arg) (cur_fun : string) (name : string) : unit =
      let (idx_args, _) = Hashtbl.find va name in
      List.iter (fun i -> 
        Stack.push (cur_fun, i) to_process;
        ) idx_args;
    in

    (* helper function : if the right side of the assignment is a pointer and it is related to 
       an argument of a function, returns the index of the corresponding argument. *)
    let _get_binop_right_cptr_args (va : vars_arg) (t: trm) : int list =
      let rec aux (depth : int) (t: trm) : int list option =
        match t.desc with
        (* unop : progress deeper + update depth *)
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop uo)); _}, [t]) ->
          begin match uo with
          | Unop_get -> aux (depth-1) t
          | Unop_address -> aux (depth+1) t
          | _ -> None
          end

        (* binop array acces : progress deeper + update depth *)
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _}, 
            [t; _]) -> aux (depth-1) t

        (* binop : progress deeper + resolve left and right sides *)
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop _ )); _}, [lhs; rhs]) ->
          begin match (aux depth lhs, aux depth rhs) with
          | Some(res), None -> Some(res)
          | None, Some(res) -> Some(res)
          | None, None -> None
          | Some(_), Some(_) -> fail None "Should not happen : Binary operator between pointers" 
          end

        (* funcall : check type, can progress deeper *)
        (* TODO : when handle return ref/ptr argument done, use it instead for more precision *)
        | Trm_apps ({desc = Trm_var (_, fname); _}, args) -> 
          begin match Hashtbl.find_opt fac fname.qvar_str with 
          | Some (_, d) when (d + depth) > 0 ->
            begin match get_args_idx_in_apps va t with
            | [] -> None
            | args_idx -> Some (args_idx)
            end
          | _ -> None
          end

        (* variable : resolve variable *)
        | Trm_var (_ ,qv) ->
          begin match Hashtbl.find_opt va qv.qvar_str with
          | Some (args_idx, d) when (d + depth) > 0 -> Some (args_idx)
          | _ -> None
          end
        
        | _ -> None
      in

      match aux 0 t with
      | Some (res) -> res
      | None -> []
    in


    (* init fac *)
    List.iter (fun t ->
      match t.desc with
      | Trm_let_fun (qv, ty, args, _) ->
        let acs : args_const = List.map 
        (fun (_, ty) -> { 
          is_ptr_or_ref = is_cptr_or_ref ty ;
          is_const = true; 
          dependency_of = [] }) args in
        Hashtbl.add fac qv.qvar_str (acs, get_cptr_depth ty)
      | _ -> fail None "Should not happen"
      ) fun_decls;


    (* update fac dependency_of and fill to_process *)
    let rec update_fac_and_to_process (va : vars_arg) (cur_fun : string) (t : trm) : unit =
      match t.desc with
      (* new scope *)
      | Trm_seq _ | Trm_for _ | Trm_for_c _ -> 
        trm_iter (update_fac_and_to_process (Hashtbl.copy va) cur_fun) t
      (* the syntax allows to declare variable in the condition statement 
         but Optitrust currently cannot parse it *)
      | Trm_if _ | Trm_switch _ | Trm_while _ ->
        trm_iter (update_fac_and_to_process (Hashtbl.copy va) cur_fun) t
      
      (* funcall : update dependecy_of *)
      | Trm_apps ({ desc = Trm_var (_ , funcall_name); _ }, args) when Hashtbl.mem fac funcall_name.qvar_str -> 
        List.iteri (fun i t -> 
          match t.desc with
          | Trm_var (_, arg_name) when Hashtbl.mem va arg_name.qvar_str ->
              let (arg_pos, _) = Hashtbl.find va arg_name.qvar_str in
              let (acs, _) = Hashtbl.find fac funcall_name.qvar_str in
              let ac = List.nth acs i in 
              if ac.is_ptr_or_ref then
                List.iter (fun i -> ac.dependency_of <- (cur_fun, i) :: ac.dependency_of) arg_pos
          | _ -> ()) args;
        trm_iter (update_fac_and_to_process va cur_fun) t
      
      (* declare new ref/ptr that refer/point to argument : update vars_arg *)
      (* TODO : handle multiple variable declaration *)
      | Trm_let (_, (lname, ty), tr) when trm_has_cstyle Reference t || is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) ->
        (* take all arguments on the right side of the "=" operator *)
        (* TODO : find a better method to determine the variable pointed/refered *)
        begin match get_args_idx_in_apps va tr with
        | [] -> ()
        | args_idx -> Hashtbl.add va lname (args_idx, get_cptr_depth ty)
        end;
        trm_iter (update_fac_and_to_process va cur_fun) t
      
      (* assignment & compound assignment to argument : update to_process *)
      (* TODO : change vars_arg in pointer assignement if it stills a pointer after dereferencing *)
      | Trm_apps _ when is_set_operation t -> 
        begin match get_binop_set_left_var_opt t with
        | Some({ desc = Trm_var (_, name); _ }) when Hashtbl.mem va name.qvar_str ->
          add_elt_in_to_process va cur_fun name.qvar_str;
        | _ -> ()
        end;
        trm_iter (update_fac_and_to_process va cur_fun) t
          
      (* mutable unary operator (++, --) : update to_process *)
      | Trm_apps _ when is_unary_mutation t ->
        let name = get_unary_mutation_qvar t in
        if Hashtbl.mem va name.qvar_str then add_elt_in_to_process va cur_fun name.qvar_str;
        trm_iter (update_fac_and_to_process va cur_fun) t

      (* return argument : update to_process if return ref or ptr*)
      | Trm_abort (Ret (Some tr)) -> trm_iter (update_fac_and_to_process va cur_fun) t
      
      | _ -> trm_iter (update_fac_and_to_process va cur_fun) t
    in

    List.iter (fun t ->
      let va = Hashtbl.create 10 in
      match t.desc with
      | Trm_let_fun (qv, ty, args, body) ->
        List.iteri (fun i (name, _) -> if name <> "" then Hashtbl.add va name ([i], get_cptr_depth ty)) args ;
        trm_iter (update_fac_and_to_process va (qv.qvar_str)) body
      | _ -> fail None "Should not happen"
      ) fun_decls;
    
    
    (* propagate argument unconstification through function call *)
    let rec unconstify_propagate (to_process : (string * int) Stack.t) : unit =
      match Stack.pop_opt to_process with
      | None -> ()
      | Some (fname, nth) -> 
        let (acs, _) = Hashtbl.find fac fname in
        let ac : arg_const = List.nth acs nth in
        if ac.is_const then begin 
          ac.is_const <- false; 
          List.iter (fun e -> Stack.push e to_process) ac.dependency_of 
        end;
        unconstify_propagate to_process
    in
    unconstify_propagate to_process;


    (* make change in the ast *)
    Hashtbl.iter (fun fname (v, _) -> 
      let is_const : bool list = List.map (fun { is_const; _ } -> is_const) v in
      constify_args ~is_const [nbMulti; cTopFunDefAndDecl fname]
      ) fac
  )

  
type decl_cptrs = (var, bool) Hashtbl.t

let get_delete_task (ptrs : decl_cptrs) : trm option =
  let vars = Tools.hashtbl_keys_to_list ptrs in
  match vars with
  | [] -> None
  | _ ->
    let deps : deps = List.map (fun var -> Dep_ptr (Dep_var var) ) vars in
    let delete_trms : trms = List.map (fun var -> trm_delete (Hashtbl.find ptrs var) (trm_var var)) vars in
    Some (trm_add_pragmas [Task [Depend [Inout deps]; FirstPrivate vars]] (trm_seq_nomarks delete_trms))

let heapify_nested_seq : Transfo.t =
  (* TODO : handle let mult *)
  (* TODO : add firstprivate in tasks *)
  iter_on_targets (fun t p ->

    (* heapifies variables and adds delete tasks before certain Trm_abort*)
    let rec aux (ptrs : decl_cptrs) (first_depth : bool) (t : trm) : trm =
      match t.desc with
      (* new scope *)
      | Trm_seq _ -> trm_map (aux (Hashtbl.copy ptrs) first_depth) t
      | Trm_for _ | Trm_for_c _  -> trm_map (aux (Hashtbl.copy ptrs) false) t 
      | Trm_while _ | Trm_switch _ -> trm_map (aux ptrs false) t

      | Trm_let (_, (var, ty), _) -> 
        if Hashtbl.mem ptrs var 
          (* remove variable from occurs when declaring them again *)
          then begin Hashtbl.remove ptrs var; trm_map (aux ptrs first_depth) t end
          (* heapify new variable *)
          else begin 
            let tr = Apac_core.stack_to_heap_aux t in
            Hashtbl.add ptrs var (is_typ_array (get_inner_ptr_type ty));
            trm_map (aux ptrs first_depth)  tr 
          end
      
      (* dereference heapified variables *)
      | Trm_var (kind, qv) when Hashtbl.mem ptrs qv.qvar_str -> trm_get t 
      
      (* add delete task before : *)
      (* return : everytime *)
      (* break, continue : only the current loop not deeper *)
      | Trm_abort _ when is_return t || first_depth ->
        begin match get_delete_task ptrs with
        | Some (tr) -> trm_seq_no_brace [tr; trm_map (aux ptrs first_depth) t]
        | _ -> t
        end
      
      | _ -> trm_map (aux ptrs first_depth) t
    in

    (* add a delete task a the end of the sequence if there is not Trm_abort at the end *)
    let add_end_delete_task (ptrs : decl_cptrs) (t :trm) : trm =
      match t.desc with
      | Trm_seq tl ->
        begin match List.rev (Mlist.to_list tl) with
        | tr :: _ when is_trm_abort tr -> t
        | _ -> 
          begin match get_delete_task ptrs with
          | Some (tr) -> trm_seq_add_last (tr) t
          | _ -> t
          end
        end
      | _ -> t
    in

    let decl_vars = Hashtbl.create 10 in
    let tg_trm = Path.get_trm_at_path p t in
    match tg_trm.desc with
    | Trm_seq tl -> Internal.nobrace_remove_after (fun _ -> 
      transfo_on_targets (trm_map (aux decl_vars true)) (target_of_path p));
      transfo_on_targets (add_end_delete_task decl_vars) (target_of_path p)
    | _ -> fail None "Expects target to point at a sequence"
  )