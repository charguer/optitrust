open Ast
open Target


let var_init_detach : Target.Transfo.t =
  Target.apply_on_target ( Generic_core.var_init_detach)

let var_init_atttach : Target.Transfo.t =
  Target.apply_on_target (Generic_core.var_init_attach)

let const_non_const : Target.Transfo.t =
  Target.apply_on_target (Generic_core.const_non_const)

let remove_instruction : Target.Transfo.t =
  Target.apply_on_target (Generic_core.remove_instruction)

let remove_instructions (tgs : target list) : unit =
  List.fold_left(fun () x ->
      remove_instruction x
    ) () tgs

let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.local_other_name var_type old_var new_var)


(* This one used special smart constructors like tBefore and tAfter instead of giving target_befoer or target_after *)
let insert_trm_new  (t_insert : trm) (tg : target) : unit =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Generic_core.insert_trm t_insert i t p) tg


let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.delocalize array_size neutral_element fold_operation)


let add_atribute(a : attribute) : Transfo.t =
  Target.apply_on_target (Generic_core.add_attribute a)


let ast_show ?(file:string="_ast.txt") ?(to_stdout:bool=true) (tg : target) : unit  =
  Target.applyi_on_target (fun i t p -> Generic_core.ast_show file to_stdout i t p) tg


let eliminate_goto_next (_ : unit) : unit =
  Trace.apply (fun _ -> Generic_core.eliminate_goto_next)



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
      | trm_for (int i = ..)   | trm_for (int j = ...) => LATER: support this, not now
      | _, _ -> (* different constructors *)
  in
  try Some(aux p t) with Mismatch -> None
 *)



(* Check if rule is applicable *)
(* let is_rule_applicable (t : trm) (p : pat) : bool =
 let rec aux (t : trm) : bool =
  match t.desc with
  | Trm *)


(* Rewrite rule transformation  *)
(* let rewrite (pl : target) (rule : base)  *)