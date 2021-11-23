open Ast
open Target

(* *********************************************************************************** 
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* This type is used for variable renaming, the user can choose between renaming all the variables 
    on one block, by giving the suffix to add after or he can also give the list of variables to 
    be renamed where the list should be a a list of string pairs ex. (current_name, new_name).
*)
module Rename = struct
  type t = | AddSuffix of string | ByList of (string * string) list
end
type rename = Rename.t

let map f = function 
| Rename.AddSuffix v -> Rename.AddSuffix (f v)
| ByList kvs -> ByList (List.map (fun (k,v) -> (k, f v)) kvs)


(* [fold_aux as_reference fold_at index t]: fold the variable declarations [t]
    params:
      [as_reference]: a flag for telling if the variable on the assignment 
        has the address operator or not
      [fold_at]: target where folding should be performed, if left empty 
        then folding is applied everywhere
      [t]: ast of the variable declaration
    return:
      updated ast of the block which contained the variable declaration [t]
*)
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
                then {dx with add = List.filter (fun x -> x <> Address_operator) dx.add}
                else dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> 
                    if as_reference 
                      then {init with add = List.filter (fun x -> x <> Address_operator) init.add}
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


(* [inline_aux delete_decl inline_at index t]: inline variable the variable declaraed in [t] at target [tg]
    params:
      [delete_decl]: delete or don't delete the declaration of the variable after inlining
      [inline_at]: target where inlining should be performed, if empty inlining is applied everywhere
      [t]: ast of the variable declaration
    return:
      the ast of the updated sequence which contains the declaration ast [t]
*)

let inline_aux (delete_decl : bool) (inline_at : target) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl -> 
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with 
    | Trm_let (vk, (x, tx), dx) -> 
      let def_x = match get_init_val dx with 
      | Some init1 -> init1
      | _ -> fail dl.loc "inline_aux: can't inline a variable with a detached declaration" in
      let t_x = begin match vk with 
      | Var_immutable -> trm_var x 
      | Var_mutable ->  trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [trm_var x] end in
         let lback = 
          begin match def_x.desc with 
          | Trm_struct field_init ->
            let tyid = Internal.get_typid_from_typ tx in
            let struct_def = 
              if tyid <> -1
                 then match Context.typid_to_typedef tyid with 
                  | Some td -> td 
                  | _ -> fail t.loc "inline_aux: could not get the declaration of the struct"
                 else 
                  fail t.loc "inline_aux: there is something wrong with type of the variable you are trying to inline"
            in
            let field_list = fst (List.split (Internal.get_field_list struct_def)) in
            let lback = Mlist.map (fun t1 ->
              List.fold_left2 (fun acc t2 f2 -> Internal.change_trm ~change_at:[inline_at]
                (trm_apps (trm_unop (Unop_struct_field_addr f2)) [trm_var x]) t2 acc ) t1 (Mlist.to_list field_init) field_list) lback  in
            Mlist.map (Internal.change_trm ~change_at:[inline_at] t_x def_x) lback 
          | _ -> Mlist.map (Internal.change_trm ~change_at:[inline_at] t_x def_x) lback 
          end in
          let new_tl = Mlist.merge lfront lback in
          let new_tl = if delete_decl then new_tl else Mlist.insert_at index dl new_tl in
          trm_seq ~annot:t.annot ~marks:t.marks new_tl
    | _ -> fail t.loc "inline_aux: expected a target to a variable declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"

let inline (delete_decl : bool) (inline_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_aux delete_decl inline_at index)


(* [rename_on_block_aux new_name index t] rename the variable declared in [t] and all its occurrences
   params:
     [rename]: a type covering both the case when a prefix is given or the list of variables to change
        together with their new name
     [t]: ast of the declaration
   return:
    updated ast of the sequence which contains the declaration
*)
let rename_on_block_aux (rename : Rename.t) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    (* first change the declarations*)
    let t_new_dl = Mlist.fold_left (fun acc t1 ->
        match t1.desc with
        | Trm_let (vk,(x, tx), init) ->
          begin match rename with 
          | AddSuffix post_fix ->
            let new_name = x ^ post_fix  in
            Internal.change_trm t1  {t1 with desc = Trm_let (vk, (new_name, tx), init)} acc 
          | ByList list -> 
            if List.mem_assoc x list then
              let new_name = List.assoc x list in
              Internal.change_trm t1  {t1 with desc = Trm_let (vk, (new_name, tx), init)} acc 
            else
              acc 
          end
        | _ -> acc
      ) t tl 
       in
     (* then all the variable occurrences *)
     Mlist.fold_left (fun acc t1 ->
          match t1.desc with
          | Trm_let (_,(x, _), _) ->
            begin match rename with 
            | AddSuffix post_fix ->
              let new_name = x ^ post_fix  in
              Internal.change_trm (trm_var x) (trm_var new_name) acc
          | ByList list -> 
            if List.mem_assoc x list then
              let new_name = List.assoc x list in
              Internal.change_trm (trm_var x) (trm_var new_name) acc
            else
              acc 
          end
        | _ -> acc
      ) t_new_dl tl 
  | _ -> fail t.loc "rename_on_block_aux: expected the sequence block"

let rename_on_block (rename : Rename.t) : Target.Transfo.local =
  Target.apply_on_path (Internal.apply_on_path_targeting_a_sequence (rename_on_block_aux rename) "var_rename")

(* [replace_occurrences_aux name space t]: replace all occurrences of [name] with [space] 
      params:
        [name]: name of the variable whose occurrences are going to be replaced
        [space]:trm which is going to replace all the occurrences of [name]
        [t]: any node in the ast which could contain an occurrence of [name]
      return:
        updated [t] with all the replaced occurrences
*)

let rec replace_occurrences_aux (name : var) (space : trm) (t : trm) : trm = 
  match t.desc with 
  | Trm_var y when y = name -> space
  | _ -> trm_map (replace_occurrences_aux name space) t

let replace_occurrences (name : var)(space : trm) : Target.Transfo.local = 
  Target.apply_on_path (replace_occurrences_aux name space)



(* [init_detach_aux t]: replace an initialized variable declaration with an
      uninitialized declaration and a set operation.
    params:
      [index]: 
      [t]: ast of the surrounding sequence of the variable declaration
    return:
      the updated ast of the sequence which contains [t]
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
          | _ -> fail t.loc "init_detach_aux: can't detach an uninitialized declaration"
          end in
        let var_type = get_inner_ptr_type tx in
        let var_type = begin match  var_type.typ_desc  with
        | Typ_ptr {inner_typ = ty;_} -> ty
        | _ -> var_type
        end in
        let new_tx = typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut var_type in
        let var_decl = trm_let ~annot:t.annot ~marks:t.marks vk (x, new_tx) (trm_prim (Prim_new new_tx)) in
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
      [const]: a boolean to decide if the attached variable should be mutable or not
      [t]: ast of the surrounding sequence of the variable declaration
    return
      the updated ast of the sequence which contains [t]
    raises:
      - Init_attach_no_occurrences if no variable set operations are found
      - Init_attach_occurrence_below_control if more than one variable set operation are found 
        
*)
exception Init_attach_no_occurrences
exception Init_attach_occurrence_below_control

let init_attach_aux (const : bool) (index : int) (t : trm) : trm = 
  match t.desc with
  | Trm_seq tl -> 
    let lfront, trm_to_change, lback  = Internal.get_trm_and_its_relatives index tl in
    begin match trm_to_change.desc with
    | Trm_let (_, (x, tx), _) -> 
      let tg = [nbAny;cSeq (); cStrict;cWriteVar x] in
      let new_tl = Mlist.merge lfront lback in
      let new_t = trm_seq ~annot:t.annot ~marks:t.marks new_tl in
      let ps = resolve_target tg new_t in
      let nb_occs = List.length ps in
      if nb_occs < 0 then raise Init_attach_no_occurrences;
      Tools.foldi (fun i acc p ->
        if i = 0 then begin 
        apply_on_path (fun t1 -> 
          begin match t1.desc with
          | Trm_apps (_, [_;rs]) ->
            if const then trm_let_immut ~marks:trm_to_change.marks (x,tx) rs else trm_let_mut ~marks:trm_to_change.marks (x, (get_inner_ptr_type tx)) rs
          | _ -> t1
          end
        ) acc p
        end
        else acc
      ) new_t ps
    | _ -> fail trm_to_change.loc "init_attach_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "init_attach_axu: expected the surrounding sequence"

let init_attach (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path(init_attach_aux const index )


(* [local_name_aux var_type curr_var local_var t]: add a local name and replace all the 
      occurrences of a variable inside a sequence.
    params:
      [mark]: a mark to mark the producesd nobrace sequence
      [var_type]: the type of the variable
      [curr_var]: the previous name of the variable, this is used to find all its occurrences
      [local_var]: the name of the variable to be declared and replace all the occurrences of curr_var
      [t]: ast of the trm which contains curr_var.
    return:
      the ast of a marked(clean) nobrace sequence depending on the flag [mark] which contains
        the term [t] the new declaration and a set operation at the end

*)
let local_name_aux (mark : mark) (var_type : typ) (curr_var : var) (local_var : var) (t : trm) : trm =
  let fst_instr = trm_let Var_mutable (local_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut var_type) (trm_apps (trm_prim (Prim_new var_type)) [trm_var curr_var]) in
  let lst_instr = trm_set (trm_var ~typ:(Some var_type) curr_var) (trm_apps ~annot:[Mutable_var_get] ( trm_prim (Prim_unop Unop_get)) [trm_var ~typ:(Some var_type) local_var]) in
  let new_t = Internal.change_trm (trm_var curr_var) (trm_var local_var) t in
  let final_trm = trm_seq_no_brace [fst_instr;new_t;lst_instr] in
  if mark <> "" then trm_add_mark mark final_trm else final_trm

let local_name (mark : mark) (var_type : typ) (curr_var : var) (local_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_name_aux mark var_type curr_var local_var)

(* [delocalize_aux array_size ops index t]: after introduced the local_name transformation
      transform the newly declared local variable [local_var]  into an array of size [array size]
      and add to loops: the first one to initialize the elements of the added array and the last one 
      to reduce the array into a single value, the inittialization value and the reducing(folding operation)
      is given through [ops].
    params:
      [array_size]: size of the arrays to be declared inside the targeted sequence
      [ops]: delocalize operation representing the unitary lement used for initialization 
        and the folding operation used for the reduction
      [index]: the index for the two added loops
      [t]: the ast of the sequence generated after applying the local_name transformation
    return:
      the update ast of [t]
*)
let delocalize_aux (array_size : string) (ops : delocalize_ops) (index : string) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    if Mlist.length tl <> 3 then fail t.loc "delocalize_aux: the targeted sequence does not have the correct shape";
    let def = Mlist.nth tl 0 in
    let snd_instr = Mlist.nth tl 1 in
    begin match def.desc with
    | Trm_let (vk, (x, tx), init) ->
      let local_var = x in
      let curr_var_trm = match get_init_val init with
      | Some init1 -> init1
      | _ -> fail def.loc "delocalize_aux: couldn't get the value of the current variable " in
      let var_type = (get_inner_ptr_type tx) in
      let add_star_if_ptr (t : trm) : trm = 
        if is_typ_ptr (get_inner_ptr_type tx)  then add_star t
          else t
          in 
      let init_trm, op = begin match ops with 
      | Delocalize_arith (li, op) ->
          trm_lit li, (trm_set ~annot:[App_and_set] (curr_var_trm)
                            (trm_apps (trm_binop op) [
                             curr_var_trm;
                              trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_var index]])) 
      | Delocalize_obj (clear_f, transfer_f) ->
          trm_apps ~typ:(Some (typ_unit ())) (trm_var clear_f) [], 
          trm_apps ~typ:(Some (typ_unit())) (trm_var transfer_f) 
            [add_star_if_ptr curr_var_trm ; 
            add_star_if_ptr  (trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_var index])]
      end in
      let new_first_trm = trm_seq_no_brace[
          trm_let vk (local_var, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_array var_type (Trm (trm_var array_size)))) (trm_prim (Prim_new (typ_array var_type (Trm (trm_var array_size)))));
          trm_set (trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_lit (Lit_int 0)]) curr_var_trm;
          trm_for index DirUp (trm_lit (Lit_int 1)) (trm_var array_size) (trm_lit (Lit_int 1))
         (trm_seq_nomarks [trm_set (trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_var index]) init_trm])]
          in
      let new_snd_instr = Internal.change_trm (trm_var local_var)  (trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_apps (trm_var "ANY") [trm_var array_size] ]) snd_instr  in
      let new_thrd_trm = trm_seq_no_brace [
                      trm_set (curr_var_trm) (trm_apps (trm_binop Binop_array_cell_addr)[trm_var local_var; trm_lit (Lit_int 0)]);
                      (* trm_omp_directive (Parallel_for [Reduction (Plus,["a"])]); *)
                      trm_for index DirUp (trm_lit (Lit_int 1)) (trm_var array_size) (trm_lit (Lit_int 1))
                        (trm_seq_nomarks [op])
                     ] in
      let new_tl = (Mlist.of_list [new_first_trm; new_snd_instr; new_thrd_trm]) in
      { t with desc = Trm_seq new_tl}
      (* trm_seq ~annot:t.annot ~marks:t.marks (Mlist.of_list [new_first_trm; new_snd_instr; new_thrd_trm]) *)

    | _ -> fail t.loc "delocalize_aux: first instruction in the sequence should be the declaration of local variable"
    end
  | _ -> fail t.loc "delocalize_aux: expected the nobrace sequence"


let delocalize (array_size : string) (ops : delocalize_ops) (index : string) : Target.Transfo.local =
  Target.apply_on_path (delocalize_aux array_size ops index )



(* [insert_aux index const name typ value t] insert a variaable declaration at index [index] with 
    name [name], type [typ] and initial value [value].
    params:
      [index]: index inside the sequence where the insertion is performed
      [const]: a flag on the mutability of the variable [name]
      [typ]: the type of the inserted variable entered as a string
      [value]: the initial value of the inserted variable [name] entered as a string
      [t]: the ast of the sequence where the insertion is performed
    return:
      the updated [t] with the newly inserted declaration []
  *)
let insert_aux (index : int) (const : bool) (name : string) (typ : string) (value : string) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let vk = if const then Var_immutable else Var_mutable  in
    let new_typ = if const then typ_const (typ_str typ) else typ_str typ in
    let new_trm = trm_let vk (name, new_typ) (code value) in
    let new_tl = Mlist.insert_at index new_trm tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "insert_aux: expected the sequence which is going to contain the variable declaration" 

let insert (index : int) (const : bool) (name : string) (typ : string) (value : string) : Target.Transfo.local =
  Target.apply_on_path (insert_aux index const name typ value)




(* [change_type_aux new_type t]:  change the current type of the variable declared at the node with index [i] 
      of the sequence with ast [t] to [new_type]
    params:
      [new_type]: the new type replacing the current one entered as a string
      [t]: ast of the sequence which contains the declaration
    return:
      the updated ast of the sequence which contains the declaration
*)
let change_type_aux (new_type : typvar) (index : int) (t : trm) : trm =
  let constructed_type = typ_str new_type in
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
        trm_let vk (x, typ_const new_type) (Internal.change_typ (get_inner_ptr_type tx) (new_type) init)
      end in
      let lback = Mlist.map (Internal.change_typ (get_inner_ptr_type tx) new_type ~change_at:[[Target.cVar x]]) lback in
      let tl = Mlist.merge lfront lback in
      let tl = Mlist.insert_at index new_decl tl in
      trm_seq ~annot:t.annot ~marks:t.marks tl
    | _ -> fail t.loc "change_type_aux: expected a variable or a function declaration"
    end
  | _ -> fail t.loc "change_type_aux: expected the surrounding sequence"

let change_type (new_type : typvar) (index : int) : Target.Transfo.local =
  Target.apply_on_path (change_type_aux new_type index)


(* [bind_intro_aux index fresh_name const p_local t]: bind the variable [fresh_name] to the function_call
    params:
      [my_mark]: a mark to be left in the targeted node
      [index]: index of the instruction containing the targeted function call
      [fresh_name]: name of the variable which going to be binded to the function call
      [const]: a flag for the mutability of the binded variable
      [p_local]: the local path from the instruction containing the targeted node
        to the targeted node
      [t]: ast of the sequence containing the targeted node
    return:
      the updated sequence with the new generated binding
*)
let bind_intro_aux (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (p_local : path) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl -> 
    let lfront, instr, lback = Internal.get_trm_and_its_relatives index tl in
    let targeted_node, _ = Path.resolve_path p_local instr in
    let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
    let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
    let node_to_change = Internal.change_trm targeted_node (trm_var fresh_name) instr in
    let targeted_node = if my_mark <> "" then trm_add_mark my_mark targeted_node else targeted_node in
    let node_type = match targeted_node.typ with
    | Some ty -> ty
    | _ -> typ_auto() in
    let decl_to_insert =
      begin match targeted_node.desc with
      | Trm_array tl -> 
        let node_type = begin match node_type.typ_desc with 
        | Typ_array (ty, _) -> ty
        | _ -> typ_auto ()
        end in
        let sz = (Mlist.length tl)  in
        if const 
          then 
            trm_let_array Var_immutable (fresh_name, node_type) (Const sz) targeted_node
          else 
            trm_let_array Var_mutable (fresh_name, node_type) (Const sz) targeted_node
      | _ ->
        if const 
          then trm_let_immut (fresh_name, node_type) targeted_node  
          else trm_let_mut (fresh_name, node_type) targeted_node
      end in
      let new_tl = Mlist.merge lfront (Mlist.of_list ([decl_to_insert] @ [node_to_change])) in
      let new_tl = Mlist.merge new_tl lback in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "bind_intro_aux: expected the surrounding sequence"


let bind_intro (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_intro_aux my_mark index fresh_name const p_local)
