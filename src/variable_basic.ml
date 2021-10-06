open Target
open Ast
include Variable_core.Rename
include Variable_core


(* [fold ~as_reference ~at tg] expects [tg] to point to a variable declaration
    [as_reference] - denotes a flag whether the declaration initialization contains a
      variable reference or not.
    [at] - denotes a target where the folding is done. If empty the
      folding operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = []
*)
let fold ?(as_reference : bool = false) ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.fold as_reference at i t p)

(* internal function *)
(* [inline_common delete at tg] expects [tg] to point to a variable declaration
    it then find all the occurrences of the variable and replaces them with it's assigned value.
   [delete] ~ denotes a flag whether the declaration should be kept or not
   [at] - denotes a target where inlining is done. If empty the
    inlining operation is performed on all the ast nodes in the same level as the declaration
    or deeper
*)
let inline_common (delete : bool) (at : target) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete at i t p)

(* [inline tg] expects [tg] to point to a variable declaration
    it then find all the occurrences of the variable and replaces them with it's assigned value. *)
let inline : Target.Transfo.t =
  inline_common true []

(* [inline_at at tg] expects [tg] to point to a variable declaration
    it then find the occurrences pointed at by the [at] target,
    and replaces them with it's assigned value. *)
let inline_at (at : target) : Target.Transfo.t =
  inline_common false at

(* [reanme ~list ~func tg] expects [tg] to point to a sequence.
    [list] - denotes a list of pairs where each pair has the
      current variable and the one which is going to replace it.
      By default this list is empty.
    [func] - a function which is going to replace all the variables
      inside the targeted sequence. By default this function is the one
      which adds "_1" to each declared variable inside the sequence.
*)
let rename (rename : rename) : Target.Transfo.t =
  Target.apply_on_targets (Variable_core.rename rename)

(* [init_detach tg] expects the target to point to a variable initialization.
   It then splits the instruction into a variable declaration and a set operation.
*)

let init_detach (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
    Target.apply_on_targets (Variable_core.init_detach) tg
  )

(* let init_detach : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Variable_core.init_detach i t p) *)

(* [init_attach const tg] expects the target to point to a variable declaration,
    Then it will search inside the sequence which contains the variable declaration.
    For an unique assigment. The it will replace that assignment with a new initialized
    variable declaration.
    [const] -denotes a booleean to decide if the new declaration is constant or not.
*)

let init_attach ?(const : bool = false) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.init_attach const i t p )
(* [const_non_const tg] expects the target to point to an initialized variable declaration.
    It then checks the variable mutability. It it is mutable then make it un-mutable by adding
    a const in front. Otherwise make it mutable by removing the const.
*)
let const_non_const : Target.Transfo.t =
  Target.apply_on_targets (Variable_core.const_non_const)


(* [local_other_name var_type old_var new_var tg] expectes target [tg] to point to a marked
      sequence. Then it will declare a new variable with name [new_name] and replace all
      the occurences of [old_var] with [new_var]. The user needs to give the type of the
      variable for which we want to change the name.

      Example:
        T a                     ->->->       T a
       mark:{                                marlk:{
          for (int i = 0; i < 10; i++){       T x = a
             a++;                             for(int i = 0; i < 10; i++){
          }                                       x++;
       }@nobrace                                }
                                                a = x;
                                              }@nobrace
*)
let local_other_name ?(mark : mark = "section_of_interest") ~var_type:(vt : typ) ~old_var:(ov : var) ~new_var:(nv : var) (tg : Target.target) : unit =
  Internal.nobrace_enter();
  Target.apply_on_targets (Variable_core.local_other_name mark vt ov nv) tg


(* [delocalize array_size neutral_element fold_operation tg] expects target [tg] to point to
    a block of code of the following form
      T a

   { T x = a; // mendatory format for first instruction

      for (int i = ...)
         x++;

      a = x;  // mendatory format for last instruction
   }@nobrace then
   Then it will transform it into:
       T a

   {
      { T x[N];
         x[0] = a;
         for (k = 1; k < N; k++)
            x[k] = 0;
      }@nobrace

      for (int i = ...)
         x[my_core_id]++;

      { a = 0;
         for (k = 1; k < N; k++)
            a = a + x[k];  // could be a += if exists
         }@nobrace

   }@nobrace.

   [array_size] - denotes the size of the array inside the block
   [neutral_element] - denotes the neutral element for the folding operation
   [fold_operation] - denotes a reduction operation over all the elements
    of the array declared inside the block
*)
let delocalize ?(loop_index : string = "dl_k") ~array_size:(arr_s : string) ~dl_ops:(dl_o : delocalize_ops) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets (Variable_core.delocalize arr_s dl_o loop_index ) tg)


(* [change_type new_type tg] expects [tg] to point to variable declaration
    then it will change the type of that variable with [new_type].
*)
let change_type (new_type : typvar) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Variable_core.change_type new_type i t p)






(*
TODO:


BASIC STEPS

 ---> this can be use in an "alternative" in the combi unit test

  let label = "scope" in
  Sequence.intro ~start:... ~stop:... ~label;
  Variable_basic.local_other_name ~label T "a" ~array_name:"x";
  Variable_basic.delocalize ~label "N"



  let occ_a = resolve_target_exactly_one [cLabel label; cVar "a"]
   .. get the type of the term, so you have T


  Variable.delocalize ~start:.. ~stop:.. "a" ~array_name:"x" ~array_size:"N"


  LATER: FOR OTHER PURPOSES we will need a mechanism for computing the explicit path to
    a variable definition, given a path to an occurence of the variable

*)

let insert ?(const : bool = false) (name : string) (typ : string ) (value : string) (tg : Target.target) : unit =
  Target.apply_on_targets_between (fun t (p,i) -> Variable_core.insert i const name typ value t p) tg;
  Trace.reparse()