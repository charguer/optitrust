open Ast
open Target


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
      let tm = Fun_map.empty in 
      let ht = Hashtbl.create 100 in 
      Mlist.folf_left (fun acc t -> 
        match t.desc with 
        | Trm_let_fun (qn, ty, args, body) when qn.qvar_var <> "main" -> 
          Hashtbl.add acc f.qvar_var (get_arg_dependencies t)
        | _ -> acc
      ) ht tl
    | None -> fail None "Apac_basic.identify_taskable_functions:expected a target to the main file sequence."
    end 
  | _ -> fail None "Apac_basic.identify_taskable_functions: expected a target to the main file sequence."



let bind_taskable_calls (tsk : taskable) (tg : target) : unit =
  iter_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun i t (path_to_seq, local_path, i1)  -> 
      let path_to_instruction = path_to_seq @ [Dir_seq_nth i1] in
      let path_to_call = path_to_instruction @ local_path in
      let call = Path.get_trm_at_path path_to_call in 
      let tg_out_trm = Path.resolvea_path path_to_instruction t in 
      match tg_out_trm.desc with 
      | Trm_let _ -> 
      | Trm_apps (_,|ls; rhs) when is_set_operation tg_out_trm -> 
      | _ -> Functin.bind (target_of_path path_to_call)
      
) tg
