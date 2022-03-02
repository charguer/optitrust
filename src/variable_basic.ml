open Target
open Ast


(* [fold ~at tg] expects the target [tg] to point to a variable declaration
    [at] - denotes a target where the fold_lefting is done. If empty the
      fold_lefting operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = []
*)
let fold ?(at : target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.fold at i t p)


(* [unfold ~mark ~accept_functions ~at tg] expects the target [tg] to be pointing at an initialized
     variable declaration, then it will  find all the occurrences of that variable and replace them with its
     initial value. The argument [accept_functions] is a flag to decide if we want to inline functions in the
     beta way. Ex Suppose we have
     void f(int x) { ... }
     int main () {
       f(3);
       ....
     }
     Then it will transform it into;
     void f(int x) {}
     int main () {
       (void f (int x) { ...}) (3);

       ...
     }

     Assumption:
      The targeted variable is a const variable,
*)
let unfold ?(mark : mark = "") ?(accept_functions : bool = true) ?(at : Target.target = []) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.unfold false accept_functions mark at i t p)

(* [inline] documentation *)
let inline ?(mark : mark = "") ?(accept_functions : bool = true) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p, i) -> Variable_core.unfold true accept_functions mark [] i t p)

(* [rename ~into tg] expects the target [tg] to be pointing at a declaration, then it will
    rename its declaration and all its occurrences
*)
let rename ~into:(new_name : var) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Variable_core.rename new_name i t p)

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


(* [local_name var_type ~mark var ~into tg] expectes target [tg] to be pointing at a marked
      sequence. Then it will declare a new variable with name [new_name] and replace all
      the occurences of [var] with [into]. The user needs to provide the new local name.
      If the arg [mark] is provided then after the transformation the local scope will be marked
      with that mark.

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
let local_name ?(mark : mark = "") (var : var) ~into:(nv : var) (tg : Target.target) : unit =
  Internal.nobrace_enter();
  Target.apply_on_targets (Variable_core.local_name mark var nv) tg


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


(* [insert ~constr ~name ~typ ~value tg] expects the target [tg] to point to a location in a sequence
    then it wil insert a new variable declaration with name [name] type [typ] and initialization value [value] if provided if not it will be left without an intialization value
*)
let insert ?(const : bool = false) ?(reparse : bool = false) ?(value : trm = trm_lit (Lit_uninitialized)) ~name:(name : string) ~typ:(typ : typ) : Target.Transfo.t =
  Target.reparse_after ~reparse (Target.apply_on_targets_between (fun t (p,i) -> Variable_core.insert i const name typ value t p))


(* [subst name ~space tg]] expects the target [tg] to point to any node ast which could contain
    an occurrence of the variable [name], then it will check for occurrences of the variable [subst] and replace is  with [put].
*)
let subst ?(reparse : bool = false) ~subst:(name : var) ~put:(put : trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (
    Target.apply_on_targets (Variable_core.subst name put)
  )

(* [bind ~const ~mark fresh_name tg] expects the target [tg] to be pointing at any ast node then it will insert a variable declaration
      with name [fresh_name] just before the instruction that contains the target [tg]. And replace the targeted node with an occurrence
      of the variable [fresh_name].
*)
let bind ?(const : bool = false) ?(mark : mark = "") (fresh_name : var) : Target.Transfo.t =
  Target.applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ  t (p, p_local, i) ->
      let fresh_name = Tools.string_subst "${occ}" (string_of_int occ) fresh_name in
      Variable_core.bind mark i fresh_name const p_local t p)

(* [to_const tg] expects the target [tg] to be pointing at a variable declaration, then it will search inside the same scope if there are
      any write operations on that variable. If this is the case then the tranformation will fail, because of the safety of this operation.
      Otherwise, first switch the mutability of that variable and then replace all get operations on that variable with its intialization
      value.

  @correctness: always correct if the result type checks. *)
let to_const : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
     ( fun t (p, i) -> Variable_core.from_to_const true i t p)


(* [to_nonconst tg] expects the target [tg] to be pointing at a variable declaration, 
      If the variable is mutable then do nothing, otherwise change the mutability of the targeted variable to a mutable one,
      and replace all the variable occurrences with a get operation containing that occurrence.*)
let to_nonconst : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
     ( fun t (p, i) -> Variable_core.from_to_const false i t p)




(* [simpl_deref ~indepth tg] if [tg] = [] then the transformation will start from the root of the ast
    otherwise it will search for expressions of the form *(&b) &( *b) in depth if [indepth] is set to true
    or at the give target if [indepth] is false.*)
let simpl_deref ?(indepth : bool = false) : Target.Transfo.t =
  Target.apply_on_targets (Variable_core.simpl_deref indepth)
