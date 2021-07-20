open Ast
open Target

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [var_init_detach_aux t]: replace an initialized variable declaration with an
    uninitialized declaration and an assignment.
    params:
      index: 
      t: ast of the surrounding sequence of the variable declaration
    return:
      the updated ast of the outer sequence which contains the declaration of the variable 
      and a set operations for that variable
*)
let var_init_detach_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let ldecl, lback = Tools.split_list_at 1 lback in
    let decl = match ldecl with 
      | [dl] -> dl
      | _ -> fail t.loc "var_init_detach_aux: wrong target" in
    begin match decl.desc with 
    | Trm_let(vk,(x, tx), init) ->
      begin match vk with
      | Var_immutable -> fail t.loc "var_init_detach_aux: const declarations cannot be detached"
      | _ ->
        let init =
          begin match init.desc with
          | Trm_apps(_,[init]) -> init
          | _ -> fail t.loc "var_init_detach_aux: expected a heap allocated variable declaration"
          end in
        let var_decl = trm_let vk (x, tx) (trm_prim (Prim_new tx)) in
        let var_assgn = trm_set (trm_var x) init in
        trm_seq ~annot:t.annot (lfront @ [var_decl; var_assgn] @ lback)
      end
    | _ -> fail decl.loc "var_init_detach_aux: variable could not be matched, make sure your path is correct"
    end
  | _ -> fail t.loc "var_init_detach_aux: expected the surrounding sequence"

let var_init_detach (index : int) : Target.Transfo.local =
  Target.apply_on_path(var_init_detach_aux index )

(* [var_init_attach_aux t]: replace an uninitialized variable declaration with an initialized one.
    params:
      const: a boolean to decide if the attached variable should be mutable or not
      t: ast of the surrounding sequence of the variable declaration
    return
      the updated ast of the outer sequence which contains now the initialized variable declaration
*)
let var_init_attach_aux (const : bool ) (index : int) (t : trm) : trm =
  let counter = ref 0 in
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let trm_to_change, lback = Tools.split_list_at 1 lback in
    let trm_to_change = List.hd trm_to_change in
    begin match trm_to_change.desc with 
    | Trm_let (_, (x, tx), _) ->
        let init_index = Tools.foldi (fun i acc t1 -> 
          match t1.desc with 
          | Trm_apps(_,[ls;_]) ->
            begin match ls.desc with 
            | Trm_var y when y = x -> 
              if !counter <= 1 then Some i else fail t1.loc "var_init_attach_aux: cases with more than one occurence are not supported"
            | _ -> acc
            end
          | _ -> acc
        ) None lback in
        let index1  = match init_index with 
        | Some index -> index
        | _ -> fail trm_to_change.loc (Tools.sprintf("var_init_attach_aux: no assignment was found to the given variable %s") x)
          in
        let lfront1,lback1 = Tools.split_list_at index1 lback in
        let assgn_to_change,lback1  = Tools.split_list_at 1 lback1 in
        let assgn_to_change = List.hd assgn_to_change in
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
          let new_trm = trm_let vk (x, tx)  init in
          trm_seq ~annot:t.annot (lfront @ lfront1 @ [new_trm] @ lback1)
        | _ -> fail assgn_to_change.loc "var_init_attach: something wen't wrong"
        end
    | _ -> fail t.loc "var_init_attach_aux: target_doesn't point to the right trm, expected a trm_let"
    end
  | _ -> fail t.loc "var_init_attach_axu: expected the surrounding sequence"

let var_init_attach (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path(var_init_attach_aux const index )


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


(* [remove_instruction_aux t]: delete an instruction inside the sequence 
    params:
      index: index of the instruction inside the sequence
      t: ast of the sequence containing the instruction to remove
    return:
      updated ast of the outer sequence with one less trm
*)
let remove_instruction_aux (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let _instruction_to_remove, lback = Tools.split_list_at 1 lback in
    trm_seq ~annot:t.annot (lfront @ lback)
  | _ -> fail t.loc "remove_instruction_aux: expected the surrounding sequence of the instruciton"

let remove_instruction (index : int) : Target.Transfo.local=
  apply_on_path (remove_instruction_aux index)

(* TODO: Add the docs for this function *)
let local_other_name_aux (var_type : typvar) (old_var : var) (new_var : var) (t : trm) : trm =
     match t.desc with
    | Trm_seq [f_loop] ->
          begin match f_loop.desc with
          | Trm_for (index, direction, start, stop, step, body) ->
            let ty = typ_var var_type in
            let fst_instr = trm_let Var_mutable (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut ty) (trm_var old_var) in
            let lst_instr = trm_set (trm_var old_var) (trm_var new_var) in
            let new_loop = trm_for index direction start stop step (Internal.change_trm (trm_var old_var) (trm_var new_var) body) in
            trm_seq ~annot:t.annot [fst_instr; new_loop;lst_instr]
          | _ -> fail t.loc "local_other_name_aux: expected a for loop"
          end
    | _ -> fail t.loc "local_other_name_aux: expected the no brace sequence"

let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_other_name_aux var_type old_var new_var)


(* [replace_with_arbitrary_aux code index t]: replace any node of the ast with an arbitrary code
      tranformed later into an ast subtree
    params:
      code: string representing the code which will appear in place of the targeted trm
      index: index of the trageted trm inside the sequence
      t: ast of the surrounding sequence which contains the trm going to be replaced
    return:
      updated ast of the urrounding sequence which contains now the replaced trm
 *)
let replace_with_arbitrary_aux (code : string)(index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let _, lback = Tools.split_list_at 1 lback in
    trm_seq ~annot:t.annot (lfront @ [trm_arbitrary code] @ lback)
  | _ -> fail t.loc "replace_with_arbitrary_aux: expected the surrounding sequence"
  
let replace_with_arbitrary (code : string) (index : int): Target.Transfo.local =
  Target.apply_on_path (replace_with_arbitrary_aux code index)

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
        then trm_seq ~annot:(Some No_braces) (List.mapi (fun i name ->
         trm_let vk (y ^ "_" ^(string_of_int i), ty) (Internal.change_trm (trm_var x) (trm_var name) init)) names)
        else t
    | Trm_apps (_, _) -> 
      if contains_variable x t then
        trm_seq ~annot:(Some No_braces) (List.map (fun name -> Internal.change_trm (trm_var x) (trm_var name) t) names)
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
      single_branch: a boolean indicating whether there is an else branch or not
      index: index of the instruction inside it's surrounding sequence
      cond: condition of the if statement given as string code
      t: ast of the outer sequence containing the instruction
    return:
      updated ast of the surrounding sequence with the added if statement
 *)
let arbitrary_if_aux (single_branch : bool) (index : int) (cond : string) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    begin match single_branch with 
    | true ->
      let branch, lback = Tools.split_list_at 1 lback in
      let then_branch = begin match branch with 
                        | [then_] -> then_
                        | _ -> fail t.loc "arbitrary_if_aux: expected a list with only one element"
                        end in
      let new_if = trm_if (trm_arbitrary cond) then_branch (trm_lit (Lit_unit)) in
      trm_seq ~annot:t.annot (lfront @ [new_if] @ lback)
    | false ->
      let branches, lback = Tools.split_list_at 2 lback in
      let new_if = trm_if (trm_arbitrary cond) (List.nth branches 0) (List.nth branches 1) in
      trm_seq ~annot:t.annot (lfront @ [new_if] @ lback)
    end
  | _ -> fail t.loc "arbitrary_if_aux: expected the surrounding sequence"
  
let arbitrary_if (single_branch : bool) (index : int) (cond : string) : Target.Transfo.local =
  Target.apply_on_path (arbitrary_if_aux single_branch index cond)


(* [change_occurrence_aux new_name t]: change a variable occurrence or a function call with a new 
      variable occurrence of another function call.
    params:
      new_name: the name of the variable which is going to replace the current occurrence
      t: ast of the variable occurrence going to be replaced
    return:
      updated ast of the variable occurrence
*)
let change_occurrence_aux (new_name : var) (t : trm) : trm =
  match t.desc with 
  | Trm_var _ -> trm_var new_name
  | _ -> fail t.loc "change_occurrence_aux: expected a variable occurrence"

let change_occurrence (new_name : var) : Target.Transfo.local =
  Target.apply_on_path (change_occurrence_aux new_name)

(* TODO: Add the docs for this function *)
let delocalize_aux (array_size : string) (neutral_element : int) (fold_operation : string) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let def = List.nth tl 0 in
    let vk,new_var,old_var =
    begin match def.desc with
    | Trm_let (vk,(x, _),init) ->
      begin match init.desc with
      | Trm_apps(_, [base]) ->
        begin match base.desc with
        | Trm_apps (_, [base1]) ->
          begin match base1.desc with
          | Trm_var y -> (vk,y, x)
          | _ -> fail t.loc "delocalize_aux: expected a variable"
          end
        | Trm_var y -> (vk,y,x)
        | _ -> fail t.loc "delocalize_aux: expected a get or a simple variable"
        end

      | Trm_var y -> (vk,y, x)
      | _ -> fail t.loc "delocalize_aux: expected something"
      end
    | _ -> fail t.loc "delocalize_aux: expected a varaible declaration"
    end in
    let new_decl = trm_seq ~annot:(Some No_braces)[
      trm_let vk (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array (typ_var "T" ) (Trm (trm_var array_size)))) (trm_prim (Prim_new (typ_array (typ_var "T") (Trm (trm_var array_size)))));
      trm_for "k" DirUp (trm_lit (Lit_int 1)) (trm_var array_size) (trm_lit (Lit_int 1))
      (trm_seq ~annot:(None)[
        trm_set (trm_var old_var) (trm_lit (Lit_int 0))
      ]
      )]
    in
    let for_loop = List.nth tl 1 in
    let parallel_for =  
      begin match for_loop.desc  with
      | Trm_for ( index, direction, start, stop, step, body) ->
        trm_for index direction start stop step(
            Internal.change_trm (trm_var new_var) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var new_var; trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get) [trm_any (trm_var "my_core_id")]]) body)
      | _ -> fail t.loc "delocalize_aux: expected a simple for loop"
      end in
    let operation = match fold_operation with
      | "+" -> Binop_add
      | "-" -> Binop_sub
      | "*" -> Binop_mul
      | "/" -> Binop_div
      | _ -> fail t.loc "delocalize_aux: this operation is not suported"
    in
    let accum = trm_seq ~annot:(Some No_braces) [
      trm_set (trm_var old_var) (trm_lit (Lit_int neutral_element));
      trm_for "k" DirUp (trm_lit (Lit_int 0)) (trm_var array_size) (trm_lit (Lit_int 1))
      (trm_seq [
          trm_set ~annot:(Some App_and_set) (trm_var old_var)
          (trm_apps (trm_binop operation)[
              trm_var old_var;
              trm_apps (trm_binop Binop_array_cell_addr)[trm_var new_var; trm_var "k"]]) ])] in 
      
      trm_seq ([new_decl] @ [parallel_for] @ [accum])

  | _ -> fail t.loc "delocalize_aux: expected the nobrace sequence"

  let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.local =
    Target.apply_on_path (delocalize_aux array_size neutral_element fold_operation)



