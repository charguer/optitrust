open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)


(* [replace_aux code index t]: replace any node of the ast with an arbitrary code
      tranformed later into an ast subtree
    params:
      code: string representing the code which will appear in place of the targeted trm
      t: ast of the trm going to be replaced
    return:
      updated ast with the replaced trm
 *)
let replace_aux (cd : string) (t : trm) : trm =
  match t.desc with 
  | Trm_var _ -> trm_var cd
  | _ ->  code cd
  
let replace (code : string) : Target.Transfo.local =
  Target.apply_on_path (replace_aux code)

(* [replace_one_with_mane]: change all the instructions containing the occurrence of the 
      variable into a list of instructions, the list of instructions contains one instruction 
      per variable.
    params:
      x: the name of the variable to be whose occurrence is going to be replaced
      names: a list of new variables to replace the current variable
      t: an ast node located in the same level as the variable declaration or deeper
    return:
      updated ast nodes which are in the same level with the variable declaration or deeper
*)
let replace_one_with_many (x : var) (names : var list) (t : trm) : trm = 
  let rec aux (global_trm : trm) (t : trm) : trm = 
    match t.desc with 
    | Trm_let (vk, (y, ty), init) ->
      if contains_variable x init 
        then trm_seq_no_brace (List.mapi (fun i name ->
         trm_let vk (y ^ "_" ^(string_of_int i), ty) (Internal.change_trm (trm_var x) (trm_var name) init)) names)
        else t
    | Trm_apps (_, _) -> 
      if contains_variable x t then
        trm_seq_no_brace (List.map (fun name -> Internal.change_trm (trm_var x) (trm_var name) t) names)
        else t 
    | _ -> trm_map (aux global_trm) t
  in aux t t 

(* [from_one_to_many_aux names index t]: transform one variable declaration to a list of variable 
      declarations, change all the instructions containing an occurrence of the declared variable
      with a list of instructions with the occurrence replaced by the variable on the list entered by the user.
      There is a bijective correspondence between the instructionss added and the list of variables.
    params:
      names: a list of variable names which are going to replace the curren variable
      index: index of the variable declaration inside the sequence containing it
      t: ast of the outer sequence containing the declaration
    return:
      updated ast of the surrounding sequence with all the changes performed
*)
let from_one_to_many_aux (names : var list) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let decl_to_change, lback = Tools.split_list_at 1 lback in
    let decl_to_change = match decl_to_change with 
      | [dclt] -> dclt
      | _ -> fail t.loc "from_one_to_many_aux: expected a list with only one trm" in
    begin match decl_to_change.desc with 
    | Trm_let (vk, (x, tx), init) -> 
      let trms_to_add = List.map (fun name -> trm_let vk (name, tx) init) names in
      let lback = List.map (replace_one_with_many x names) lback in
      trm_seq ~annot:t.annot (lfront @ trms_to_add @ lback)
    | _ -> fail decl_to_change.loc "from_one_to_many_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "from_one_to_many_aux: expected the surrounding sequence"

let from_one_to_many (names : var list) (index : int) : Target.Transfo.local =
  Target.apply_on_path (from_one_to_many_aux names index)


(* [arbitrary_if single_branch index cond t]: take one or two instructions and create an if statement
      or an if else statment if [single_brnach] is true.
    params:
      cond: condition of the if statement given as string code
      t: ast of the outer sequence containing the instruction
    return:
      updated ast of the surrounding sequence with the added if statement
 *)
let arbitrary_if_aux (cond : string) (t : trm) : trm =
  trm_if (code cond) t t
   
let arbitrary_if (cond : string) : Target.Transfo.local =
  Target.apply_on_path (arbitrary_if_aux cond)


(* [delocalize_aux array_size neutral_element fold_operation t] add local array to apply
      the operation inside the for loop in parallel.
    params:
      array_size: size of the arrays to be declared inside the targeted sequence
      neutral_element: the neutral element used when applying the [fold_operation]
      fold_operation: reduction over all the elements of the declared array
      t: the ast of the @nobrace sequence
    return:
      the updated ast of the targeted sequence      
*)

let delocalize_aux (array_size : string) (dl_ops : delocalize_ops) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    if List.length tl <> 3 then fail t.loc "delocalize_aux: the targeted sequence does not have the correct shape";
    let def = List.nth tl 0 in
    let middle_instr = List.nth tl 1 in
    begin match def.desc with 
    | Trm_let (vk, (x, tx), _) ->
      let new_var = x in
      let old_var_trm = get_init_val def in
      let var_type = (get_inner_ptr_type tx) in
      let new_decl = trm_seq_no_brace[
      trm_let vk (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array var_type (Trm (trm_var array_size)))) (trm_prim (Prim_new (typ_array var_type (Trm (trm_var array_size)))));
      trm_for "k" DirUp (trm_lit (Lit_int 1)) (trm_var array_size) (trm_lit (Lit_int 1))
      (trm_seq ~annot:[] [trm_set (old_var_trm) (trm_lit (Lit_int 0))])] in
      let new_snd_instr = Internal.change_trm (trm_var new_var) (trm_var ~annot:[Any] "0" ) middle_instr  in
      let accum = begin match dl_ops with 
                  | Delocalize_arith (li, op) ->
                    trm_seq_no_brace [
                      trm_set (old_var_trm) (trm_lit li);
                      trm_for "k" DirUp (trm_lit (Lit_int 0)) (trm_var array_size) (trm_lit (Lit_int 1))
                        (trm_seq [
                            trm_set ~annot:[App_and_set] (old_var_trm)
                            (trm_apps (trm_binop op)[
                             old_var_trm;
                              trm_apps (trm_binop Binop_array_cell_addr)[trm_var new_var; trm_var "k"]]) ])
                     ]
                  | Delocalize_obj (clear_f, transfer_f) -> 
                    trm_seq_no_brace [
                      trm_apps (trm_var clear_f) [old_var_trm];
                      trm_for "k" DirUp (trm_lit (Lit_int 0)) (trm_var array_size) (trm_lit (Lit_int 1))
                        (
                          trm_seq [
                            (trm_apps (trm_var transfer_f)[
                             old_var_trm;
                              trm_apps (trm_binop Binop_array_cell_addr)[trm_var new_var; trm_var "k"]]) ]
                              )]
                  end in
      trm_seq ~annot:t.annot [new_decl; new_snd_instr; accum]

    | _ -> fail t.loc "delocalize_aux: first instruction in the sequence should be the declaration of local variable"
    end
  | _ -> fail t.loc "delocalize_aux: expected the nobrace sequence"


let delocalize (array_size : string) (dl_ops : delocalize_ops) : Target.Transfo.local =
  Target.apply_on_path (delocalize_aux array_size dl_ops)

(* [change_type_aux new_type t]:  change the current type of the variable to new_type
    params: 
      new_type: the new type replacing the old one
      t: ast of the declaration 
    return: 
      the updated ast of the declaration
*)
let change_type_aux (new_type : typvar) (index : int) (t : trm) : trm =
  let constructed_type = typ_constr new_type in
  match t.desc with 
  | Trm_seq tl ->
    let lfront, decl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match decl.desc with 
    | Trm_let (vk, (x, tx), init) ->
      let new_type = 
        begin match (get_inner_ptr_type tx) .typ_desc with 
        | Typ_const _ -> typ_const constructed_type 
        | Typ_ptr {ptr_kind = pk; _} -> typ_ptr pk constructed_type
        | Typ_array (_, sz) -> typ_array constructed_type sz
        | _ -> constructed_type
        end in
      let new_decl = begin match vk with 
      | Var_mutable -> 
        trm_let vk (x, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut new_type ) (Internal.change_typ (get_inner_ptr_type tx) (new_type) init) 
      | Var_immutable ->
        trm_let vk (x, new_type) (Internal.change_typ (get_inner_ptr_type tx) (new_type) init) 
      end in
      let lback = List.map (Internal.change_typ (get_inner_ptr_type tx) new_type ~change_at:[[Target.cVar x]]) lback in
      trm_seq ~annot:t.annot (lfront @ [new_decl] @ lback)
    | _ -> fail t.loc "change_type_aux: expected a variable or a function declaration"
    end
  | _ -> fail t.loc "change_type_aux: expected the surrounding sequence"

let change_type (new_type : typvar) (index : int) : Target.Transfo.local =
  Target.apply_on_path (change_type_aux new_type index)



(* [data_shif_aux neg pre_cast post_cast u t]: shift the right hand side of a set operation with term [u]
    params:
      new: a flag for the sine of shifting
      pre_cast: casting of type [pre_cast] performed on the right hand side of the set operation before shifting is applied
      post_cast: casting of type [post_cast] performed after shifting is done
      u: shift size
      t: the ast of teh set operation
    return:
      the updated set operation 
*)
let data_shift_aux (neg : bool) (pre_cast : typ) (post_cast : typ) (u : trm) (t : trm) : trm =
    let binop_op = if neg then Binop_sub else Binop_add in
    begin match pre_cast.typ_desc, post_cast.typ_desc with 
    | Typ_unit , Typ_unit -> trm_apps (trm_binop binop_op) [t; u]
    | Typ_unit, _ -> trm_cast post_cast (trm_apps (trm_binop binop_op) [t; u])
      | _, Typ_unit -> trm_apps (trm_binop binop_op) [trm_cast pre_cast t; u]
      | _ -> fail t.loc "data_shift_aux: can'd do both precasting and postcasting"
      end 
  

let data_shift (neg : bool) (pre_cast : typ) (post_cast : typ) (u : trm) : Target.Transfo.local =
  Target.apply_on_path (data_shift_aux neg pre_cast post_cast u)

let add_mark_aux (m : mark) (t : trm) : trm =
  trm_add_mark m t

let add_mark (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_mark_aux m)

let remove_mark_aux (m : mark) (t : trm) : trm =
  trm_remove_mark m t

let remove_mark (m : mark) : Target.Transfo.local =
  Target.apply_on_path (remove_mark_aux m)

let set_mark_aux (m : mark) (t : trm) : trm =
  trm_set_mark m t

let set_mark (m : mark) : Target.Transfo.local =
  Target.apply_on_path (set_mark_aux m)


let clear_marks_aux (t : trm) : trm =
  trm_clear_marks t
let clear_marks : Target.Transfo.local = 
  Target.apply_on_path (clear_marks_aux)