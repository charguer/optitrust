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
let use_goto_for_return ?(mark : mark = "") (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ -> 
    apply_on_targets (Apac_core.use_goto_for_return mark) tg)
  
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


(* [constify_args ~is_const tg]: expect the target [tg] to point at a function definition.
   Then it will add the "const" keyword where is it possible in the type of the argument. 
   The list [is_args_const] determines which argument to constify. *)
let constify_args ?(is_args_const : bool list = []) ?(is_method_const : bool = false): Transfo.t =
  apply_on_targets (Apac_core.constify_args is_args_const is_method_const)

(* [constify_args ~is_args_const tg]: expect the target [tg] to point at a function definition.
   Then in the body, it will add the "const" keyword where is it possible in variables that 
   are pointer or reference to the constified arguments of the function..
   The list [is_args_const] determines which argument is constified. *)
let constify_args_alias ?(is_args_const : bool list = []) : Transfo.t =
  apply_on_targets (Apac_core.constify_args_alias is_args_const)

(* [stack_to_heap tg]: expect the target [tg] to point at a variable declaration. 
    Then the variable declared will be declared on the heap. *)
let stack_to_heap : Transfo.t =
  apply_on_targets (Apac_core.stack_to_heap)

(* [unfold_let_mult tg]: expects the target [tg] to point at a multiple variable declaration.
    Then it will be replace by a sequence of simple variable declarations. *)
let unfold_let_mult (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Apac_core.unfold_let_mult) tg)