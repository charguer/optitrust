open Ast

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* TODO: Add the docs for this function *)
let local_other_name_aux (var_type : typvar) (old_var : var) (new_var : var) (t : trm) : trm =
     match t.desc with
    | Trm_seq [f_loop] ->
          begin match f_loop.desc with
          | Trm_for (index, direction, start, stop, step, body) ->
            let tid = next_typid () in
            let ty = typ_var var_type tid in
            let fst_instr = trm_let Var_mutable (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut ty) (trm_var old_var) in
            let lst_instr = trm_set (trm_var old_var) (trm_var new_var) in
            let new_loop = trm_for index direction start stop step (Internal.change_trm (trm_var old_var) (trm_var new_var) body) in
            trm_seq ~annot:t.annot [fst_instr; new_loop;lst_instr]
          | _ -> fail t.loc "local_other_name_aux: expected a for loop"
          end
    | _ -> fail t.loc "local_other_name_aux: expected the no brace sequence"

let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_other_name_aux var_type old_var new_var)


(* [replace_aux code index t]: replace any node of the ast with an arbitrary code
      tranformed later into an ast subtree
    params:
      code: string representing the code which will appear in place of the targeted trm
      t: ast of the trm going to be replaced
    return:
      updated ast with the replaced trm
 *)
let replace_aux (code : string) (t : trm) : trm =
  match t.desc with 
  | Trm_var _ -> trm_var code
  | _ ->  trm_arbitrary code
  
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
  trm_if (trm_arbitrary cond) t t
   
let arbitrary_if (cond : string) : Target.Transfo.local =
  Target.apply_on_path (arbitrary_if_aux cond)


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
    let tid = next_typid () in
    
    let new_decl = trm_seq_no_brace[
      trm_let vk (new_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array (typ_var "T" tid) (Trm (trm_var array_size)))) (trm_prim (Prim_new (typ_array (typ_var "T" tid) (Trm (trm_var array_size)))));
      trm_for "k" DirUp (trm_lit (Lit_int 1)) (trm_var array_size) (trm_lit (Lit_int 1))
      (trm_seq ~annot:[] [
        trm_set (trm_var old_var) (trm_lit (Lit_int 0))
      ]
      )]
    in
    let for_loop = List.nth tl 1 in
    let parallel_for =  
      begin match for_loop.desc  with
      | Trm_for ( index, direction, start, stop, step, body) ->
        trm_for index direction start stop step(
            Internal.change_trm (trm_var new_var) (trm_apps (trm_binop Binop_array_cell_addr) [trm_var new_var; trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [trm_var ~annot:[Any] "my_core_id"]]) body)
      | _ -> fail t.loc "delocalize_aux: expected a simple for loop"
      end in
    let operation = match fold_operation with
      | "+" -> Binop_add
      | "-" -> Binop_sub
      | "*" -> Binop_mul
      | "/" -> Binop_div
      | _ -> fail t.loc "delocalize_aux: this operation is not suported"
    in
    let accum = trm_seq_no_brace [
      trm_set (trm_var old_var) (trm_lit (Lit_int neutral_element));
      trm_for "k" DirUp (trm_lit (Lit_int 0)) (trm_var array_size) (trm_lit (Lit_int 1))
      (trm_seq [
          trm_set ~annot:[App_and_set] (trm_var old_var)
          (trm_apps (trm_binop operation)[
              trm_var old_var;
              trm_apps (trm_binop Binop_array_cell_addr)[trm_var new_var; trm_var "k"]]) ])] in 
      
      trm_seq ([new_decl] @ [parallel_for] @ [accum])

  | _ -> fail t.loc "delocalize_aux: expected the nobrace sequence"

  let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.local =
    Target.apply_on_path (delocalize_aux array_size neutral_element fold_operation)

