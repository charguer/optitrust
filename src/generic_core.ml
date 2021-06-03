open Ast
open Target
open Clang_to_ast
open Ast_to_c
(* INTERNAL FUNCTIONS *)
(* *********************************************** *)
(*replace occurrences of t_before with t_after in t
  paths point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  assumption: t_before and t_after are equivalent (in terms of value and of side
  effects)
 *)
let change_trm ?(change_at : target list = [[]]) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  (* change all occurences of t_before in t' *)
  let rec apply_change (t' : trm) =
    (* necessary because of annotations that may be different *)
    if ast_to_string t' = ast_to_string t_before then t_after
    else
      match t'.desc with
      (*
        particular case for heap allocation: do not change the lhs of the
        initialisation
       *)
      | Trm_seq [t_decl; {desc = Trm_apps (_, [lhs; init]); loc; _}]
           (* when t'.annot = Some Heap_allocated *) ->
         trm_seq ~annot:t'.annot ~loc:t'.loc ~add:t'.add
           ~attributes:t'.attributes
           [
             t_decl;
             trm_set (* ~annot:(Some Initialisation_instruction) *) ~loc lhs
               (apply_change init)
           ]
      | _ -> trm_map apply_change t'
  in
  List.fold_left
    (fun t' tr ->
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t'.loc "change_trm: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at



(* same as change_trm but for types *)
let change_typ ?(change_at : target list = [[]]) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  (* change all occurences of ty_before in ty *)
  let rec change_typ (ty : typ) : typ =
    (* necessary because of annotations in trms that may be different *)
    if typ_to_string ty = typ_to_string ty_before then ty_after
    else Ast.typ_map change_typ ty
  in
  (* change all occurrences of ty_before in type annotations in t *)
  let rec replace_type_annot (t : trm) : trm =
    let t =
      {t with typ = match t.typ with
                    | None -> None
                    | Some ty' -> Some (change_typ ty')
      }
    in
    trm_map replace_type_annot t
  in
  (* change all occurences of ty_before in t *)
  let apply_change (t : trm) : trm =
    let rec aux (t : trm) : trm =
      (* only match nodes where typs occur *)
      match t.desc with
      | Trm_val (Val_prim (Prim_new ty)) ->
         trm_prim ~annot:t.annot ~loc:t.loc ~add:t.add
           (Prim_new (change_typ ty))
      | Trm_val (Val_prim (Prim_unop (Unop_cast ty))) ->
         trm_unop ~annot:t.annot ~loc:t.loc ~add:t.add
           (Unop_cast (change_typ ty))
      | Trm_let (vk,(y,ty),init) ->
        trm_let ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add vk (y,change_typ ty) (aux init)
      | Trm_let_fun (f, ty, args, body) ->
         trm_let_fun ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
           ~attributes:t.attributes f (change_typ ty)
                     (List.map (fun (y, ty) -> (y, change_typ ty)) args)
                     (aux body)
      | Trm_typedef (Typedef_abbrev (y, ty)) ->
         trm_typedef  ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~attributes:t.attributes 
          (Typedef_abbrev (y, change_typ ty))
      | _ -> trm_map aux t
    in
    replace_type_annot (aux t)
  in
  List.fold_left
    (fun t' tr ->
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t'.loc "change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t

    change_at











(* ********************************************** *)






(* [var_init_detach_aux t]: This is an auxiliary function for var_init_detach
    params:
      t: an ast subterm
    return:
      a sequence which contains the declaration of the variable and a set operations for that variable
*)
let var_init_detach_aux (t : trm) : trm =
  match t.desc with 
  | Trm_let(vk,(x, tx), init) ->
    begin match vk with 
    | Var_immutable -> fail t.loc "var_init_detach_aux: const declarations cannot be detached"
    | _ ->
      let init = 
        begin match init.desc with 
        | Trm_apps(_,[init]) -> init
        | _ -> fail t.loc "var_init_detach_aux: expected a heap allocated variable declaration"
        end in
      trm_seq [
        trm_let vk (x, tx) (trm_prim (Prim_new tx));
        trm_set (trm_var x) init
      ]
    end
  | _ -> fail t.loc "var_init_detach_aux: variable could not be matched, make sure your path is correct"


let var_init_detach : Target.Transfo.local =
  Target.apply_on_path(var_init_detach_aux ) 

(* [var_init_attach_aux t]: This is an auxiliary function for var_init_attach
    params:
      t: an ast subterm
    return
      the updated
*)
let var_init_attach_aux (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl -> 
    (* Assumption: The sequence is of the form
      {
        int x;
        x = 5;
      }
    *)
    let var_decl = List.nth tl 0 in
    let var_set = List.nth tl 1 in
    let var_kind, var_name, var_type = begin match var_decl.desc with 
    | Trm_let (vk,(x,tx),_) -> vk, x, tx
    | _ -> fail t.loc "var_init_attach_aux: sequence does not satisfy the assumption described above"
    end
    in
    let var_init = begin match var_set.desc with 
    | Trm_apps(_, [_;init]) -> init
    | _ -> fail t.loc "var_init_attach_aux: sequence does not satisfy the assumtion that the second term of the sequence is the set operations"
    end
    in
    trm_let ~loc:t.loc var_kind (var_name, var_type) (trm_apps (trm_prim ~loc:t.loc (Prim_new var_type)) [var_init])
  | _ -> fail t.loc "var_init_attach_aux: sequence was not matched, make sure the path is correct"

(* [var_init_attach t]: Change a sequence of the form {int x; x = 5;} to int x = 5 
    params:
      path_to_seq: path to the sequence which satisfy the assumtion above
      t: ast 
    return
      the updated ast
*)
let var_init_attach : Target.Transfo.local =
  Target.apply_on_path(var_init_attach_aux)


(* [const_non_const_aux t]: This is an auxiliary function for const_non_const
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let const_non_const_aux (t : trm) : trm =
  match t.desc with 
  | Trm_let (vk, (x,tx), init) ->
    begin match vk with
     (* If variable is a constant than whe remove the const and we perform the heap allocation  *)
    | Var_immutable ->  
      trm_let Var_mutable (x, typ_ptr tx) (trm_apps (trm_prim ~loc: t.loc (Prim_new tx)) [init])
    | _ ->
      let var_type = begin match tx.ty_desc with 
      | Typ_ptr t -> t
      | _ -> fail t.loc "const_non_const_aux: expected a pointer type"
      end
      in
      let var_init = begin match init.desc with 
      | Trm_apps(_, [_; init]) -> init
      | _ -> fail t.loc "const_non_const_aux: expected a something of the form 'new ()'"
      end
      in
      trm_let Var_immutable (x,var_type) var_init 
    end
  | _ -> fail t.loc "const_non_const_aux: variable declaration was not matched, make sure the path is correct"

let const_non_const : Target.Transfo.local =
  apply_on_path(const_non_const_aux)


(* [remove_instruction_aux t]: This is an auxiliary function for remove_instruction
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let remove_instruction_aux (_t : trm) : trm =
  (* Replace the current t with an empty sequence *)
  trm_seq ~annot:(Some No_braces) []

let remove_instruction : Target.Transfo.local=
  apply_on_path(remove_instruction_aux)

(* [local_other_name var_type old_var new_var t]: This is an auxiliary function for local_other_name
    params:
      var_type: type of the var
      old_var: old variable for which we want to chang the local name
      new_var: new_variable 
      t: an ast subterm
    return:
      the updated ast
*)
let local_other_name_aux (var_type : typvar) (old_var : var) (new_var : var) (t : trm) : trm =
    begin match t.desc with
    | Trm_seq [no_braces] ->
      begin match no_braces.desc with
        | Trm_seq [f_loop;del_inst] ->
          begin match f_loop.desc with
          | Trm_for (init, cond, step, body) ->
            let new_type = typ_var var_type (get_typedef var_type) in
            let new_decl = trm_let Var_mutable (new_var, new_type) (trm_apps (trm_prim (Prim_new new_type)) [trm_var old_var])
        
            in
            let new_set_old = trm_set (trm_var old_var) (trm_var new_var) in
            (* let new_del_inst = trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ())) ~is_statement:true (trm_unop (Unop_delete false)) [trm_var new_var] in *)
            let new_loop = trm_seq (* ~annot:(Some Delete_instructions) *) [trm_for init cond step (change_trm (trm_var old_var)(trm_var new_var) body);del_inst] in
            (* trm_seq ~annot:(Some Delete_instructions) [
              trm_seq ~annot:(Some No_braces) [
                new_decl;new_loop;new_set_old
              ]; new_del_inst
            ] *)
            trm_seq (* ~annot:(Some Delete_instructions) *) [
              trm_seq (* ~annot:(Some No_braces) *) [
                new_decl;new_loop;new_set_old
              ]]
          | _ -> fail t.loc "local_other_name_aux: expected a for loop"
          end
      | _ -> fail t.loc "local_other_name_aux: expected the sequnece which contains the for loop"
      end
    | _ -> fail t.loc "local_other_name_aux: expected the no brace sequence"
    end







