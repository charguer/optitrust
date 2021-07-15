open Ast
open Target


let var_init_detach : Target.Transfo.t =
  Target.apply_on_target ( Generic_core.var_init_detach)


let var_init_attach ?(const : bool = false) : Target.Transfo.t = 
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Generic_core.var_init_attach const i t p )

let const_non_const : Target.Transfo.t =
  Target.apply_on_target (Generic_core.const_non_const)

let remove_instruction : Target.Transfo.t =
  Target.apply_on_target (Generic_core.remove_instruction)

let remove_instructions (tgs : target list) : unit =
  List.fold_left(fun () x ->
      remove_instruction x
    ) () tgs

let replace_with_arbitrary (code : string) (tg : target) : unit =
  Trace.apply( fun ctx t ->
    let ps = resolve_target tg t in
    List.fold_left (fun p t -> Generic_core.replace_with_arbitrary ctx code p t) t ps
  )

let from_one_to_many (names : var list) : Target.Transfo.t =
  Target.apply_on_transformed_targets (Generic_core.isolate_last_dir_in_seq)
    (fun (p, i) t -> Generic_core.from_one_to_many names i t p)

let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.local_other_name var_type old_var new_var)


let arbitrary_if (cond : string) (tg : target) : unit =
  Trace.apply(fun ctx t ->
    let ps = resolve_target tg t in 
    List.fold_left (fun p t -> Generic_core.arbitrary_if ctx cond p t) t ps
  )

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


