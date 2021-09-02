open Ast
open Target

(* [replace code tg] expects the target to point at any node of the ast,
    it then removes this node and replaces with the code entered by the user, which is merged into
    the ast automatically by Optitrust.
*)
let replace (code : string) (tg : target) : unit =
  Target.apply_on_target (Generic_core.replace code) tg;
  Trace.reparse()

(* [from_one_to_many names tg] expects the target to point to a declaration(
    a variable declaration, array declaration etc.) It the chechs the list of new variables
    and removes the curren declaration and replaces it with a list of declarations.
    [names] - denotes the list of names entered by the user. Furthermore, every instruction which
    contains an occurrence of the initial variable will be replace with a list of instructions for
    each new variable entered by the user.
*)
let from_one_to_many (names : var list) (tg : Target.target) : unit =
  Internal.nobrace_enter();
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.from_one_to_many names i t p) tg;
  Internal.nobrace_remove_and_exit ()

(* TODO: use a combinator when it is possible

let from_one_to_many (names : var list) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun () ->
    Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
      (fun (p, i) t -> Generic_core.from_one_to_many names i t p) tg)

in Internal:

   let nobrace_remove_after (f:unit->unit) : unit =
     Internal.nobrace_enter();
     f();
     Internal.nobrace_remove_and_exit ()
*)

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
let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.local_other_name var_type old_var new_var)


(* [arbitrary_if cond tg] expects the target [tg] to point to an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both it's then and else branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as teh condition in the
    if statement, then this code is transformed and integrated inside the ast.
*)
let arbitrary_if (cond : string) (tg : target) : unit =
  Target.apply_on_target (fun t p -> Generic_core.arbitrary_if cond t p) tg;
  (* Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.arbitrary_if single_branch i cond t p) tg; *)
  Trace.reparse()


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
            a = 0;
      }@nobrace

      parallel for (int i = ...)
         x[my_core_id]++;

      { a = 0;
         for (k = 0; k < N; k++)
            a = a + x[k];  // could be a += if exists
         }@nobrace

   }@nobrace.

   [array_size] - denotes the size of the array inside the block
   [neutral_element] - denotes the neutral element for the folding operation
   [fold_operation] - denotes a reduction operation over all the elements
    of the array declared inside the block
*)
let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) (tg : Target.target) : unit =
  Internal.nobrace_enter ();
  Target.apply_on_target (Generic_core.delocalize array_size neutral_element fold_operation) tg;
  Internal.nobrace_remove_and_exit ()

(* [change_type new_type tg] expects [tg] to point to variable declaration
    then it will change the type of that variable with [new_type].
*)
let change_type (new_type : typvar) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.change_type new_type i t p)

(* ********************************************************* *)
(* Create an instance of the pattern *)
(* let pattern_instantiate (t : trm) (p : pat) : instatiation option =
  let rec aux p t =
      match p, t with
      | var x, _ ->
         if Fmap.mem x !inst then begin
            if same_trm (Fmap.get x !inst) t
               then ()
               else raise Mismatch
         end else
            inst := Fmap.add x t !inst
         end
      | trm_if p0 p1 p2, trm_if t0 t1 t2 ->
         aux p0 t0;
         aux p1 t1;
         aux p2 t2
      | trm_for_c(int i = ..)   | trm_for_c(int j = ...) =>
      | _, _ -> (* different constructors *)
  in
  try Some(aux p t) with Mismatch -> None
 *)



