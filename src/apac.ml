
open Ast
open Target
include Apac_core
include Apac_basic

(* [parallel_task_group ~mark tg]: expects the target [ŧg] to point at a taskable function definition,
    then it will insert  #pragma omp parallel #pragma omp master #pragma omp taskgroup in front of that definition.*)
let parallel_task_group ?(mark : mark = "") : Transfo.t =
  iter_on_targets ( fun t p -> 
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    transfo_on_targets (trm_add_pragmas [Parallel []; Master ; Taskgroup]) (target_of_path p)
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


(* [vars_arg]: hashtable that stores variables that refer to argument
    A list is used because of how dependencies of a new pointer are check :
    we currently takes all variable refering to argument on the left side of the "=" *)
type vars_arg = (string, int list) Hashtbl.t

(* [arg_const]: record that stores information to constify or not the argument
    and other argument that depend on this argument *)
type arg_const = {
  is_ptr_or_ref : bool;
  mutable is_const : bool;
  mutable dependency_of : (string * int) list;
}

(* [args_const]: a list of args_const *)
type args_const = arg_const list

(* [fun_args_const]: hashtable that store the [args_const] of functions *)
type fun_args_const = (string, args_const) Hashtbl.t

(* [get_binop_set_left_var_opt t]: return the variable on the left side of "=" *)
let rec get_binop_set_left_var_opt (t : trm) : trm option =
  match t.desc with
  | Trm_var _ -> Some(t)
  | Trm_apps (_, args) ->
    begin match List.nth_opt args 0 with
    | Some(t) -> get_binop_set_left_var_opt t
    | None -> None
    end
  | _ -> None

(* [get_args_idx_in_apps]: return a list of argument's index which are used in Trm_apps recursively*)
let get_args_idx_in_apps (va : vars_arg) (t : trm) : int list =
  let rec aux (acc : int list) (t : trm) : int list =
      match t.desc with
      | Trm_apps (_, ts) -> List.fold_left aux acc ts
      | Trm_var (_, qv) when Hashtbl.mem va qv.qvar_str -> List.rev_append (Hashtbl.find va qv.qvar_str) acc
      | _ -> acc
  in
  aux [] t

let is_unary_mutation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop uo)); _}, _) ->
    begin match uo with 
    | Unop_post_dec | Unop_post_inc | Unop_pre_dec | Unop_pre_inc -> true
    | _ -> false
    end
  | _ -> false

let is_cptr_or_ref (ty : typ) : bool =
  match (get_inner_const_type ty).typ_desc with
  | Typ_ptr _ | Typ_array _-> true
  | _ -> false

(* wip *)
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


(* [constify_functions_arguments tg]: expect target [tg] to point at the root,
    then it will add "const" keyword whenever it is possible in functions arguments *) 
(* wip*)
let constify_functions_arguments : Transfo.t = 
  (* TODO : handle include file *)
  iter_on_targets (fun t p ->
    let tg_trm = Path.get_trm_at_path p t in
    let fac : fun_args_const = Hashtbl.create 10 in 
    (* store the function's arguments (fname, nth arg) to unconst *)
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

    (* helper function : add element to process in to_process *)
    let add_elt_in_to_process (va : vars_arg) (cur_fun : string) (name : string) : unit =
      let acs = Hashtbl.find fac cur_fun in
          let idx_args = Hashtbl.find va name in
          List.iter (fun i -> 
            let ac = List.nth acs i in 
            if ac.is_ptr_or_ref then 
              Stack.push (cur_fun, i) to_process;
            ) idx_args;
    in


    (* init fac *)
    List.iter (fun t ->
      match t.desc with
      | Trm_let_fun (qv, _, args, _) ->
        let acs : args_const = List.map 
        (fun (_, ty) -> { 
          is_ptr_or_ref = is_cptr_or_ref ty ;
          is_const = true; 
          dependency_of = [] }) args in
        Hashtbl.add fac qv.qvar_str acs
      | _ -> fail None "Should not happen"
      ) fun_decls;


    (* update fac dependency_of and fill to_process *)
    let rec update_fac_and_to_process (to_process : (string * int) Stack.t) (va : vars_arg) (new_scope : bool) (cur_fun : string) (t : trm) : unit =
      let va = if new_scope then Hashtbl.copy va else va in
      match t.desc with
      (* new scope *)
      | Trm_seq _ | Trm_for _ | Trm_for_c _ -> 
        trm_iter (update_fac_and_to_process to_process va true cur_fun) t
      (* the syntax allows to declare variable in the condition statement 
         but Optitrust currently cannot parse it *)
      | Trm_if _ | Trm_switch _ | Trm_while _ ->
        trm_iter (update_fac_and_to_process to_process va true cur_fun) t
      
      (* funcall : update dependecy_of *)
      | Trm_apps ({ desc = Trm_var (_ , funcall_name); _ }, args) when Hashtbl.mem fac funcall_name.qvar_str -> 
        List.iteri (fun i t -> 
          match t.desc with
          | Trm_var (_, arg_name) ->
              begin match Hashtbl.find_opt va arg_name.qvar_str with
              | Some(arg_pos) ->
                  let acs = Hashtbl.find fac funcall_name.qvar_str in
                  let ac = List.nth acs i in 
                  if ac.is_ptr_or_ref then
                    List.iter (fun i -> ac.dependency_of <- (cur_fun, i) :: ac.dependency_of) arg_pos
              | None -> ()
              end
          | _ -> ()) args;
        trm_iter (update_fac_and_to_process to_process va false cur_fun) t
      
      (* ref/ptr assignment : update vars_arg *)
      | Trm_let (_, (lname, ty), tr) when trm_has_cstyle Reference t || is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) ->
        (* take all arguments on the right side of the "=" operator *)
        (* TODO : find a better method to determine the variable pointed/refered *)
        begin match get_args_idx_in_apps va tr with
        | [] -> ()
        | args_idx -> Hashtbl.add va lname args_idx
        end;
        trm_iter (update_fac_and_to_process to_process va false cur_fun) t
      
      (* assignment & compound assignment to argument : update to_process *)
      (* TODO : change vars_arg in pointer assignement if it stills a pointer after dereferencing *)
        | Trm_apps (_, [ls; rhs]) when is_set_operation t -> 
          begin match get_binop_set_left_var_opt ls with
          | Some({ desc = Trm_var (_, name); _ }) when Hashtbl.mem va name.qvar_str ->
            add_elt_in_to_process va cur_fun name.qvar_str;
            | _ -> ()
          end;
          trm_iter (update_fac_and_to_process to_process va false cur_fun) t
          
      (* mutable unary operator (++, --) : update to_process *)
      | Trm_apps _ when is_unary_mutation t ->
        let name = get_unary_mutation_qvar t in
        if Hashtbl.mem va name.qvar_str then add_elt_in_to_process va cur_fun name.qvar_str;
        trm_iter (update_fac_and_to_process to_process va false cur_fun) t
      
      | _ -> trm_iter (update_fac_and_to_process to_process va false cur_fun) t
    in

    List.iter (fun t ->
      let va = Hashtbl.create 10 in
      match t.desc with
      | Trm_let_fun (qv, _, args, body) ->
        List.iteri (fun i (name, _) -> if name <> "" then Hashtbl.add va name [i]) args ;
        trm_iter (update_fac_and_to_process to_process va false (qv.qvar_str)) body
      | _ -> fail None "Should not happen"
      ) fun_decls;
    
    
    (* propagate argument unconstification through function call *)
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


    (* make change in the ast *)
    Hashtbl.iter (fun fname v -> 
      let is_const : bool list = List.map (fun { is_const; _ } -> is_const) v in
      constify_args ~is_const [nbMulti; cTopFunDefAndDecl fname]
      ) fac
  )