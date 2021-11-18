open Target
open Ast
include Variable_core.Rename
include Variable_core


(* [fold ~as_reference ~at tg] expects the target [tg] to point to a variable declaration
    [as_reference] - denotes a flag whether the declaration initialization contains a
      variable reference or not.
    [at] - denotes a target where the folding is done. If empty the
      folding operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = []
*)
let fold ?(as_reference : bool = false) ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.fold as_reference at i t p)

(* internal function *)
(* [inline_common delete at tg] expects the target [tg] to point to a variable declaration
     it then will find all the occurrences of the variable and replaces them with it's assigned value.
   [delete] ~ denotes a flag whether the declaration should be kept or not
   [at] - denotes a target where inlining is done. If empty the
    inlining operation is performed on all the ast nodes in the same level as the declaration
    or deeper
*)
let inline_common (delete : bool) (at : target) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.inline delete at i t p)

(* [inline tg]: it's a specialization of [inline_common] with the flag [delete] set to true. 
    an the target [at] left empty.
*)
let inline : Target.Transfo.t =
  inline_common true []

(* [inline tg]: it's a specialization of [inline_common] with the flag [delete] set to false. 
    an the target [at] should be given by the user.
*)
let inline_at (at : target) : Target.Transfo.t =
  inline_common false at

(* [rename_on_block ~list ~func tg] expects [tg] to point to a sequence.
    [list] - denotes a list of pairs where each pair has the
      current variable and the one which is going to replace it.
      By default this list is empty.
    [func] - a function which is going to replace all the variables
      inside the targeted sequence. By default this function is the one
      which adds the suffix 1 to each declared variable inside the sequence.
*)
let rename_on_block (rename : rename) : Target.Transfo.t =
  Target.apply_on_targets (Variable_core.rename_on_block rename)

(* [init_detach tg] expects the target [tg] to point to a variable initialization.
   It then splits the instruction into a variable declaration and a set operation.
*)
let init_detach (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
    Target.apply_on_targets (Variable_core.init_detach) tg
  )

(* [init_attach const tg] expects the target [tg] to point to a variable declaration,
    Then it will search inside the sequence which contains the variable declaration
    for an unique assigment. Then it will replace that assignment with a new initialized
    variable declaration.
    [const] -denotes a booleean to decide if the new declaration is constant or not.
*)
let init_attach ?(const : bool = false) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.init_attach const i t p )


(* [local_name var_type var local_var tg] expectes target [tg] to point to a marked
      sequence. Then it will declare a new variable with name [new_name] and replace all
      the occurences of [var] with [local_var]. The user needs to give the type of the
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
let local_name ?(mark : mark = "") ~var_type:(vt : typ) ~var:(ov : var) ~local_var:(nv : var) (tg : Target.target) : unit =
  Internal.nobrace_enter();
  Target.apply_on_targets (Variable_core.local_name mark vt ov nv) tg


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
         a++;

      { a = 0;
         for (k = 1; k < N; k++)
            a = a + x[k];  // could be a += if exists
         }@nobrace

   }@nobrace.
   [index]: denotes the index of the two added loops
   [array_size] - denotes the size of the array inside the block
   [ops]: the delocalize operation, it can be an arithmetic delocalization or an object delocalization
    of the array declared inside the block
*)
let delocalize ?(index : string = "dl_k") ~array_size:(arr_s : string) ~ops:(dl_o : delocalize_ops) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets (Variable_core.delocalize arr_s dl_o index ) tg)


(* [change_type new_type tg] expects [tg] to point to variable declaration
    then it will change the type of that variable with [new_type].
*)
let change_type (new_type : typvar) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Variable_core.change_type new_type i t p)



(* [insert ~constr name typ value tg] expects the target [tg] to point to a location in a sequence
    then it wil insert a new variable declaration with name [name] type [typ] and initialization value [value]
*)
let insert ?(const : bool = false) ?(reparse : bool = false) ~name:(name : string) ~typ:(typ : string ) ~value:(value : string) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets_between (fun t (p,i) -> Variable_core.insert i const name typ value t p))

(* [replace_occurrences name ~space tg]] expects the target [tg] to point to any node ast which could contain 
    an occurrence of the variable [name], then it will all the nodes which come after the node targeted by target [tg]
*)
let replace_occurrences ?(reparse : bool = false) ~subst:(name : var) ~put:(put : strm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Variable_core.replace_occurrences name put)
  )

let bind_intro ?(fresh_name : var = "__OPTITRUST___VAR") ?(const : bool = false) ?(my_mark : mark = "") : Target.Transfo.t =
  Target.applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ  t (p, p_local, i) -> 
      let fresh_name = Str.global_replace (Str.regexp_string "${occ}") (string_of_int occ) fresh_name in
      Variable_core.bind_intro my_mark i fresh_name const p_local t p)