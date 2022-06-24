open Ast
open Target
include Apac_core

(* [use_goto_for_return mark]: expects the target [tg] to point at a function definition,
    then it will transform the body of that function definition as follows.

    First of all wraps the body of the function into a sequence and marks it with [mark] if 
    [mark] <> "". Then it considers two cases.

    Case1:
      Function is of type void:
        1) Replaces each return statement inside the new sequence with goto __exit
        2) After the wrapped sequence inserts an empty label "__exit".
    Case2:
      Function is returns a value of type [T] then:
        1) Inserts a declaration "T __res" just befor the introduced sequence.
        2) Replaces each return statement inside the wrapped sequence with "__res = x; goto __exit"
        3) Add after the new sequence, adds the labelled statement "__exit; return __res;" *)
let use_goto_for_return ?(mark : mark = "") : Transfo.t =
  apply_on_targets (Apac_core.use_goto_for_return mark)


(* [insert_task sad tg]: expects the target [tg] to be pointing at an insturction or a sequence.
    Then it will insert an OpenMP pragma at that insturction. *)
let insert_task (sad : sorted_arg_deps) (tg : target) : unit =
  let deps = [In sad.dep_in; Out sad.dep_out; Inout sad.dep_inout; Outin sad.dep_outin] in 
  transfo_on_targets (trm_add_pragma (Task [(Ast.Depend deps)])) tg

(* [taskable]: a Hashtable used for storing all the taskable functions. *)
type taskable = (var , arg_deps) Hashtbl.t

(* Note: Naive implementation. *)
let identify_taskable_functions (tg : target) : taskable =
  let tg_trm  = get_trm_at tg in 
  match tg_trm with 
  | Some t when trm_is_mainfile t -> 
    begin match trm_seq_inv t with 
    | Some tl -> 
      let ht = Hashtbl.create 100 in 
      Mlist.iter (fun t -> 
        match t.desc with 
        | Trm_let_fun (qn, ty, args, body) when qn.qvar_var <> "main" -> 
          if Hashtbl.mem ht qn.qvar_var then ()
            else Hashtbl.add ht qn.qvar_var (get_arg_dependencies t)
        | _ -> ()
      ) tl;
      ht
    | None -> fail None "Apac_basic.identify_taskable_functions:expected a target to the main file sequence."
    end 
  | _ -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."

(* [occurs]: a Hashtable used for storing all the functions whose calls occur at a given trm. *)
type occurs = (string, unit) Hashtbl.t

(* [get_fun_occurrences t]: returns all the function call occurrences inside [t]. *)
let get_fun_occurrences (t : trm) : occurs =
    let tsk = Hashtbl.create 1000 in 
    let rec aux (t : trm) : unit = 
      match t.desc with 
      | Trm_apps ({desc = Trm_var (vk, qv); _}, args) ->
        trm_iter aux t;
        let fun_name = qv.qvar_var in 
        if Hashtbl.mem tsk fun_name 
          then ()
          else Hashtbl.add tsk fun_name ()
      | _ -> trm_iter aux t
    in aux t;
    tsk

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
             then Variable_basic.init_detach (target_of_path path_to_instruction)
             else ()
       | Trm_let _ -> Function.bind_intro ~const:false ~my_mark:"bind_tskbl" ~fresh_name:("res__" ^ (string_of_int i1)) tg_call;
                      begin try Variable_basic.init_detach [cVarDef "" ~body:[cMark "bind_tskbl"]] with | TransfoError _ -> () end; 
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
