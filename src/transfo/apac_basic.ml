open Ast
open Target
open Path
include Apac_core

(* [use_goto_for_return_aux mark t]: transforms the body of the funciton
      declaration in such a way that all return statements are replaced with
      gotos,
    [mark] - mark used to mark the introduced sequence.
    [t] - ast of the function definition. *)
let use_goto_for_return_aux (mark : mark) (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (qn, ret_ty, args, body) ->
    let seq_to_insert, _ = Internal.replace_return_with_assign ~check_terminal:false ~exit_label:"__exit" "__res" body in
    let seq_to_insert = trm_seq_add_last (trm_add_label "__exit" (trm_unit())) seq_to_insert in
    let new_body =
      begin match ret_ty.typ_desc with
      | Typ_unit ->
        trm_add_mark mark (trm_seq_nomarks [seq_to_insert;])
      | _ ->
        let new_decl = trm_let_mut ("__res", ret_ty) (trm_uninitialized ()) in
        trm_add_mark mark (trm_seq_nomarks [
          new_decl;
          seq_to_insert;
          trm_ret (Some (trm_var_get "__res"))
        ])
      end in
      trm_alter ~desc:(Trm_let_fun (qn, ret_ty, args, new_body)) t
  | _ -> fail t.loc "Apac_basic.use_goto_for_return_aux: expected a target to a function definition."

(* [use_goto_for_return mark]: expects the target [tg] to point at a function
    definition, then it will transform the body of that function definition as
    follows.

    First of all wraps the body of the function into a sequence and marks it
    with [mark] if [mark] <> "". Then it considers two cases.

    Case1:
      Function is of type void:
        1) Replaces each return statement inside the new sequence with
           goto __exit.
        2) After the wrapped sequence inserts an empty label "__exit".
    Case2:
      Function is returns a value of type [T] then:
        1) Inserts a declaration "T __res" just befor the introduced sequence.
        2) Replaces each return statement inside the wrapped sequence with
           "__res = x; goto __exit".
        3) Add after the new sequence, adds the labelled statement
           "__exit; return __res;". *)
let use_goto_for_return ?(mark : mark = "") (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_at_target_paths (use_goto_for_return_aux mark) tg
  )
  
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

(* DOES NOT WORK : cause different variable encoding between Trm_let, Trm_let_mult and function's arguments *)
(* [unfold_let_mult tg]: expects the target [tg] to point at a multiple variable declaration.
    Then it will be replace by a sequence of simple variable declarations. *)
let unfold_let_mult (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_targets (Apac_core.unfold_let_mult) tg)

(* [mark_taskable_function]: expects the target [tg] to point at a function definition.
    Then it may add the mark [mark] if the function is taskable. *)
let mark_taskable_function (mark : mark) : Transfo.t =
  apply_on_targets (Apac_core.mark_taskable_function mark)