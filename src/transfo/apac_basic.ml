open Syntax
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