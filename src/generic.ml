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


(* This one used special smart constructors like cBefore and cAfter instead of giving target_befoer or target_after *)
let insert_trm_new  (t_insert : trm) (tg : target) : unit =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Generic_core.insert_trm t_insert i t p) tg


let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.delocalize array_size neutral_element fold_operation)


let add_atribute(a : attribute) : Transfo.t =
  Target.apply_on_target (Generic_core.add_attribute a)

let target_show ?(debug_ast : bool = false) ?(keep_previous : bool = false) (tg : target) : unit =
  Generic_core.without_repeat_io (fun () ->
    Target.applyi_on_target (fun i t p -> 
    let t = if not keep_previous then Generic_core.delete_target_decorators t
    else t
    in 
    Generic_core.target_show debug_ast i t p) tg
  )

let target_between_show ?(debug_ast : bool = false) ?(keep_previous : bool = false) (tg : target) : unit =
  Target.apply_on_target_between (fun (p,i) t ->
  (* TODO: Talk to Arthur, if we should remove the semicolons after *)
  let t = if not keep_previous then Generic_core.delete_target_decorators t 
  else t in
  Generic_core.target_between_show debug_ast i t p) tg
  

let ast_show ?(file:string="_ast.txt") ?(to_stdout:bool=true) (tg : target) : unit  =
  Target.applyi_on_target(fun i t p -> Generic_core.ast_show file to_stdout i t p) tg


(* TODO: Move apply to top function to trace.ml *)
let clean_target_decorators () : unit =
    Trace.apply_to_top ~replace_top:false (fun _ -> Generic_core.delete_target_decorators)


let eliminate_goto_next ?(replace_top : bool = false) (_ : unit) : unit =
  Trace.apply_to_top ~replace_top (fun _ -> Generic_core.eliminate_goto_next)

let group_decl_init ?(replace_top : bool = false) (_ : unit) : unit =
  Trace.apply_to_top ~replace_top (fun _ -> Generic_core.group_decl_init)



(* TODO: Remove this function after dealing with all the transformations which use this function *)
(*
  insert t_inserted either before the position pointed at by insert_before or
  after the position pointed at by insert_after in t
  both must be resolved as paths to a seq element
 *)
let insert_trm ?(insert_before : target = [])
  ?(insert_after : target = []) (t_inserted : trm) (t : trm) : trm =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> insert_after
    | _ :: _, [] -> insert_before
    | [], [] -> fail t.loc "insert_trm: please specify an insertion point"
    | _ -> fail t.loc "insert_trm: cannot insert both before and after"
  in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "insert_trm: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t' dl ->
         match List.rev dl with
         | Dir_nth n :: dl' ->
            begin match insert_before, insert_after with
            (* insert after *)
            | [], _ :: _ -> Generic_core.insert_trm_after dl t_inserted t'
            (* insert before: replace n with n - 1 *)
            | _ :: _, [] ->
               Generic_core.insert_trm_after (List.rev (Dir_nth (n - 1) :: dl')) t_inserted
                 t'
            | [], [] ->
               fail t'.loc "insert_trm: please specify an insertion point"
            | _ -> fail t'.loc "insert_trm: cannot insert both before and after"
            end
         | _ -> fail t'.loc "insert_trm: bad insertion target"
       )
       t
       epl


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