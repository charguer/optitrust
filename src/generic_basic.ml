open Ast
open Target

(* [const_non_const tg] expects the target to point to an initialized variable declaration.
    It then checks the variable mutability. It it is mutable then make it un-mutable by adding 
    a const in front. Otherwise make it mutable by removing the const.
*)
let const_non_const : Target.Transfo.t =
  Target.apply_on_target (Generic_core.const_non_const)

(* [remove_instruction tg] expects the target to point to an instruction inside a sequence. 
    It then removes this instruction from the sequence, basically it is the same as sequence_delete.
*)
let remove_instruction : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.remove_instruction i t p)

(* [remove_instructions tgs] expects a list of targeted instructions to be removed. Basically it is just a list
    fold over the target list by applying remove insruction transformation.
*)
let remove_instructions (tgs : target list) : unit =
  List.fold_left(fun () x ->
      remove_instruction x
    ) () tgs

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
(* [local_other_name var_type old_var new_var tg] TODO: Add the docs for this function
*)
let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.local_other_name var_type old_var new_var)


(* [arbitrary_if cond tg] expects the target tg to point to an instruction 
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

(* [change_occcurence new_name tg] expects the target tg to point to a variable occurrence.
    [new_name] - denotes the new name which is going replace the variable occurrence.
    Assumption: [new_name] is the name of an already defined function or variable.
 *)
let change_occurrence (new_name : var) : Target.Transfo.t =
  Target.apply_on_target (fun p t -> Generic_core.change_occurrence new_name p t)

(* TODO: Add the docs for this function *)
let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) (tg : Target.target) : unit =
  Internal.nobrace_enter ();
  Target.apply_on_target (Generic_core.delocalize array_size neutral_element fold_operation) tg;
  Internal.nobrace_remove_and_exit ()

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
      | trm_for_c(int i = ..)   | trm_for_c(int j = ...) => LATER: support this, not now
      | _, _ -> (* different constructors *)
  in
  try Some(aux p t) with Mismatch -> None
 *)


