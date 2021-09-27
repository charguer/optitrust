open Ast
open Target

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [fold_aux as_reference fold_at]: fold the variable declarations t
    params:
      as_reference: a flag for telling if the variable on the assignment 
        has the address operator or not
      fold_at: target where folding should be performed, if left empty 
        then folding is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast 
*)

(* This type is used for variable renaming, the user can choose between renaming all the variables 
    on one block, by giving the suffix to add after or he can also  give the list of variables to 
    be renamed together with their new name.
*)
module Rename = struct
  type t = | AddSuffix of string | ByList of (string * string) list
end
type rename = Rename.t

let fold_aux (as_reference : bool) (fold_at : target) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    begin match d.desc with
    | Trm_let (vk, (x, _), dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
            begin match vk with 
            | Var_immutable -> 
              if as_reference 
                then {dx with add = List.filter (fun x -> x <> Add_address_of_operator) dx.add}
                else dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> 
                    if as_reference 
                      then {init with add = List.filter (fun x -> x <> Add_address_of_operator) init.add}
                      else init
                   | _ -> fail t.loc "fold_aux: expected a new operation"
                   end
            end in
        let lback = Mlist.map(Internal.change_trm ~change_at:[fold_at] def_x t_x) lback
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        in
        let new_tl = Mlist.merge lfront lback in
        let new_tl = Mlist.insert_at index d new_tl in
        trm_seq new_tl 

     | _ -> fail t.loc "fold_decl: expected a variable declaration"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (as_reference : bool) (fold_at : target) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux as_reference fold_at index)


(* [inline_aux inline_at]: inline variable defined in term t
    params:
      delete_decl: delete or don't delete the declaration of the variable after inlining
      inline_at: target where inlining should be performed, if empty inlining is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast
*)
let inline_aux (delete_decl : bool) (inline_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with
    | Trm_let (vk, (x,tx), dx) ->
      let t_x = begin match vk with 
                | Var_immutable -> trm_var x
                | _ -> trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [trm_var x] 
                end in
      let def_x = 
      begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "inline_aux: expected a new operation"
                   end
      end in
      let init = get_init_val dl in
      let lback = 
      begin match init.desc with 
      | Trm_struct field_init ->
        let tyid = Internal.get_typid_from_typ tx in
        let typid_to_typedef_map = Clang_to_ast.(!ctx_typedef) in
        let struct_def = Typ_map.find tyid typid_to_typedef_map in
        let field_list = fst (List.split (Internal.get_field_list struct_def)) in
        Mlist.map (fun t1 ->
          List.fold_left2 (fun acc t2 f2 ->  Internal.change_trm ~change_at:[inline_at] 
            (trm_apps (trm_unop (Unop_struct_field_get f2)) [trm_var x]) t2 acc
          ) t1 (Mlist.to_list field_init) field_list) lback 
      | _ -> Mlist.map (Internal.change_trm ~change_at:[inline_at] t_x def_x) lback 
      end in
      let new_tl = Mlist.merge lfront lback in
      let new_tl = if delete_decl then new_tl else Mlist.insert_at index dl new_tl in
      trm_seq ~annot:t.annot new_tl
    | _ -> fail t.loc "inline_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


let inline (delete_decl : bool) (inline_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_aux delete_decl inline_at index)

(* [rename_aux new_name index t] rename a variable, change its declaration
      and all its occurrences
   params:
    rename: a type covering both the case when a prefix is given or the list of variables to change
      together with their new name
   return:
    updated ast 
*)
let rename_aux (rename : Rename.t) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    Mlist.fold_left (fun acc t1 ->
        match t1.desc with
        | Trm_let (vk,(x, tx), init) ->
          begin match rename with 
          | AddSuffix post_fix ->
            let func = fun x -> x ^ post_fix in 
            let acc = Internal.change_trm t1 (trm_let vk ((func x), tx) init) acc in
            Internal.change_trm (trm_var x) (trm_var (func x)) acc
          | ByList list -> 
            if List.mem_assoc x list then
            begin 
            let new_var = List.assoc x list in
            let acc = Internal.change_trm t1 (trm_let vk (new_var, tx) init) acc in
             Internal.change_trm (trm_var x) (trm_var new_var) acc
            end
            else
              acc 
          end
        | _ -> acc
      ) t tl 
  | _ -> fail t.loc "rename_aux: expected the sequence block"

let rename (rename : Rename.t) : Target.Transfo.local =
  Target.apply_on_path (Internal.apply_on_path_targeting_a_sequence (rename_aux rename) "var_rename")

(* [init_detach_aux t]: replace an initialized variable declaration with an
    uninitialized declaration and an assignment.
    params:
      index: 
      t: ast of the surrounding sequence of the variable declaration
    return:
      the updated ast of the outer sequence which contains the declaration of the variable 
      and a set operations for that variable
*)
let init_detach_aux  (t : trm) : trm =
  match t.desc with
  | Trm_let(vk,(x, tx), init) ->
      begin match vk with
      | Var_immutable -> fail t.loc "init_detach_aux: const declarations cannot be detached"
      | _ ->
        let init =
          begin match init.desc with
          | Trm_apps(_,[init]) -> init
          | _ -> fail t.loc "init_detach_aux: expected a heap allocated variable declaration"
          end in
        let var_type = get_inner_ptr_type tx in
        let var_type = begin match  var_type.typ_desc  with
        | Typ_ptr {inner_typ = ty;_} -> ty
        | _ -> var_type
        end in
        let new_tx = typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut var_type in
        let var_decl = trm_let vk (x, new_tx) (trm_prim (Prim_new new_tx)) in
        (* Check if variable was declared as a reference *)
        
        let var_assgn = trm_set (trm_var ~typ:(Some var_type) x) {init with typ = (Some var_type)} in
        trm_seq_no_brace [var_decl; var_assgn] 
      end
    | _ -> 
    fail t.loc "init_detach_aux: variable could not be matched, make sure your path is correct"

let init_detach : Target.Transfo.local =
  Target.apply_on_path(init_detach_aux )




(* [init_attach_aux t]: replace an uninitialized variable declaration with an initialized one.
    params:
      const: a boolean to decide if the attached variable should be mutable or not
      t: ast of the surrounding sequence of the variable declaration
    return
      the updated ast of the outer sequence which contains now the initialized variable declaration
    raises:
      - Init_attach_no_occurrences if no variable set operations are found
      - Init_attach_occurrence_below_control if more than one variable set operation are found 
        
*)
exception Init_attach_no_occurrences
exception Init_attach_occurrence_below_control

let init_attach_aux (const : bool ) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, trm_to_change, lback = Internal.get_trm_and_its_relatives index tl in
    begin match trm_to_change.desc with 
    | Trm_let (_, (x, tx), _) ->
        let nb_sets = Internal.nb_inits x (trm_seq lback) in
        if nb_sets < 1 then raise Init_attach_no_occurrences
          else if nb_sets > 1 then raise Init_attach_occurrence_below_control;
        let init_index = Mlist.foldi (fun i acc t1 -> 
          match t1.desc with 
          | Trm_apps(_,[ls;_]) ->
            begin match ls.desc with 
            | Trm_var y when y = x -> 
              Some i 
            | _ -> acc
            end
          | _ -> acc
        ) None lback in
        let index1  = match init_index with 
        | Some index -> index
        | _ -> raise Init_attach_occurrence_below_control
          in
        let lfront1, assgn_to_change, lback1 = Internal.get_trm_and_its_relatives index1 lback in
        begin match assgn_to_change.desc with 
        | Trm_apps(_, [_; rhs]) ->
          let vk = if const then Var_immutable else Var_mutable in
          let inner_type = 
          begin match tx.typ_desc with
          | Typ_ptr {ptr_kind=Ptr_kind_mut; inner_typ = ty} -> ty
          | _ -> tx
          end in
          let tx = if const then typ_const inner_type else typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut inner_type in
          let init = if const then rhs else (trm_apps (trm_prim (Prim_new inner_type)) [rhs]) in 
          let new_trm = trm_let ~marks:trm_to_change.marks vk (x, tx)  init in
          let new_front = Mlist.merge lfront lfront1 in
          let new_back = Mlist.insert_at 0 new_trm lback1 in
          let new_tl = Mlist.merge new_front new_back in
          trm_seq ~annot:t.annot ~marks:t.marks new_tl
        | _ -> 
          fail assgn_to_change.loc "init_attach: something went wrong"
        end
    | _ -> fail t.loc "init_attach_aux: target_doesn't point to the right trm, expected a trm_let"
    end
  | _ -> fail t.loc "init_attach_axu: expected the surrounding sequence"


let init_attach (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path(init_attach_aux const index )


(* [const_non_const_aux t]: transform a const declaration to a non-const one or vice-versa
    params:
      t: ast of the variable declaration 
    return:
      the updated ast of the declaration
*)
let const_non_const_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x,tx), init) ->
    begin match vk with
     (* If variable is a constant than whe remove the const and we perform the heap allocation  *)
    | Var_immutable ->
      trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut tx) (trm_apps (trm_prim ~loc: t.loc (Prim_new tx)) [init])
    | _ ->
      let var_type = begin match tx.typ_desc with
      | Typ_ptr {inner_typ = t; _} -> t
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
  apply_on_path (const_non_const_aux)


(* [local_other_name_aux var_type old_var new_var t] add a local name and replace all the 
      occurrences of a variable inside a sequence.
    params:
      var_type: the type of the variable
      old_var: the previous name of the variable, this is used to find all the occurrences
      new_var: the name of the variable to be declared and replace all the occurrences of old_var
      t: ast of the labelled sequence.
    return:
      the updated ast of the targeted sequence with the new local name

*)
let local_other_name_aux (var_type : typ) (old_var : var) (new_var : var) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let fst_instr = trm_let Var_mutable (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut var_type) (trm_apps (trm_prim (Prim_new var_type)) [trm_var old_var]) in
    let lst_instr = trm_set (trm_var old_var) (trm_apps ~annot:[Mutable_var_get] ( trm_prim (Prim_unop Unop_get)) [trm_var new_var]) in
    let tl = Mlist.map (Internal.change_trm (trm_var old_var) (trm_var new_var)) tl in
    let new_tl = Mlist.insert_at 0 fst_instr tl in
    let new_tl = Mlist.insert_at ((Mlist.length tl) - 1) lst_instr new_tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "local_other_name_aux: expected a sequence"


let local_other_name (var_type : typ) (old_var : var) (new_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_other_name_aux var_type old_var new_var)
