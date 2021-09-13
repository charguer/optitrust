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

(* [inline ~delete ~at tg] expects [tg] to point to a variable declaration
    it then find all the occurrences of the variable and replaces them with it's assigned value.
   [delete] ~ denotes a falg whether the declaration should be kept or not
   [at] - denotes a target where inlining is done. If empty the
    inlining operation is performed on all the ast nodes in the same level as the declaration
    or deeper, by default [at] = []
*)
let inline ?(delete : bool = false) ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p,i) t -> Variable_core.inline delete at i t p)

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


(* [local_other_name var_type old_var new_var tg] expectes target [tg] to point to a labelled
      sequence. Then it will declare a new variable with name [new_name] and replace all
      the occurences of [old_var] with [new_var]. The user needs to give the type of the
      variable for which we want to change the name.

      Example:
        T a                     ->->->    T a

       sectionofinterest:{                sectionofinterest:{
          for (int i = 0; i < 10; i++){      T x = a
             a++;                            for(int i = 0; i < 10; i++){
          }                                     x++;
       }@nobrace                              }
                                              a = x;
                                            }@nobrace
*)
let local_other_name (var_type : typ) (old_var : var) (new_var : var) : Target.Transfo.t =
  Target.apply_on_targets (Variable_core.local_other_name var_type old_var new_var)

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