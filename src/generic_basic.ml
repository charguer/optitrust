open Ast
open Target

(* [replace code tg] expects the target to point at any node of the ast,
    it then removes this node and replaces with the code entered by the user, which is merged into
    the ast automatically by Optitrust.
*)
let replace (code : string) (tg : target) : unit =
  Target.apply_on_targets (Generic_core.replace code) tg;
  Trace.reparse()

(* [from_one_to_many names tg] expects the target to point to a declaration(
    a variable declaration, array declaration etc.) It the chechs the list of new variables
    and removes the curren declaration and replaces it with a list of declarations.
    [names] - denotes the list of names entered by the user. Furthermore, every instruction which
    contains an occurrence of the initial variable will be replace with a list of instructions for
    each new variable entered by the user.
*)
let from_one_to_many (names : var list) (tg : Target.target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
    Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.from_one_to_many names i t p) tg)

(* [arbitrary_if cond tg] expects the target [tg] to point to an instruction
    inside a sequence. Then it will create an if statement with the condition entered by the user
      and both it's then and else branches will contain the same instruction.
    [cond] - denotes a string representing the code which will appear as teh condition in the
    if statement, then this code is transformed and integrated inside the ast.
*)
let arbitrary_if (cond : string) (tg : target) : unit =
  Target.apply_on_targets (Generic_core.arbitrary_if cond) tg;
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
let delocalize (array_size : string) (dl_ops : delocalize_ops) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets (Generic_core.delocalize array_size dl_ops) tg)

(* [change_type new_type tg] expects [tg] to point to variable declaration
    then it will change the type of that variable with [new_type].
*)
let change_type (new_type : typvar) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.change_type new_type i t p)


let data_shift ?(neg : bool = true) ?(pre_cast : typ = typ_unit ()) ?(post_cast : typ = typ_unit ()) (u : trm) (tg : Target.target) : unit =
  Target.apply_on_targets (Generic_core.data_shift neg pre_cast post_cast u ) tg;
  Trace.reparse()


let add_mark (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Generic_core.add_mark m)

let remove_mark (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Generic_core.remove_mark m)

let set_mark (m : mark) : Target.Transfo.t =
  Target.apply_on_targets (Generic_core.set_mark m)

let clear_marks : Target.Transfo.t = 
  Target.apply_on_targets (Generic_core.clear_marks)

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



