open Ast
open Target

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [fold_aux as_reference fold_at index t]: fold the variable declarations [t]
    params:
      [fold_at]: target where fold_lefting should be performed, if left empty
        then fold_lefting is applied everywhere
      [index]: the index of the targeted instruction inside its surrounding sequence
      [t]: ast of the variable declaration
    return:
      updated ast of the block which contained the variable declaration [t]
*)
let fold_aux (fold_at : target) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, d, lback = Internal.get_trm_and_its_relatives index tl in
    begin match d.desc with
    | Trm_let (vk, (x, tx), dx) ->
        (* check if the declaration is of the form int*x = &y *)
        let as_reference = is_typ_ptr (get_inner_ptr_type tx) && not (trm_annot_has Reference d) in
        let t_x =
          if as_reference then trm_var_get x
          else trm_var x
        in
        let def_x = 
            begin match vk with
            | Var_immutable -> dx
            | _ -> begin match dx.desc with
                   | Trm_apps(_, [init]) -> init
                   | _ -> dx
                   end
            end in
        let lback = Mlist.map(Internal.change_trm ~change_at:[fold_at] def_x t_x) lback
         in
        let new_tl = Mlist.merge lfront lback in
        let new_tl = Mlist.insert_at index d new_tl in
        trm_seq ~annot:t.annot ~marks:t.marks new_tl

     | _ -> fail t.loc "fold_decl: expected a variable declaration"
     end
  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (fold_at : target) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux fold_at index)


(* [inline_aux delete_decl accept_functions mark unfold_at index t]: unfold a constant variable or reference declared in [t]
    params:
      [delete_decl]: delete or don't delete the declaration of the variable after inlining
      [accept_functions]: if true then this function will consider function declarations as variable declarations
      [mark]: add a mark at the intialization trm of the declaratin
      [unfold_at]: target where inlining should be performed, if empty all the occurrences
        of the variable are replaced with its initialization value
      [index]: index of the declaration inside its surrounding sequence
      [t]: ast of the variable declaration
    return:
      the ast of the updated sequence which contains the declaration ast [t]
*)

let unfold_aux (delete_decl : bool) (accept_functions : bool) (mark : mark) (unfold_at : target) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    let aux (new_lback : trm mlist) : trm = 
        let new_tl = Mlist.merge lfront new_lback in
        let new_tl = if delete_decl then new_tl else Mlist.insert_at index dl new_tl in
        trm_seq ~annot:t.annot ~marks:t.marks new_tl
      in 
    begin match dl.desc with
    | Trm_let (vk, (x, _), init) ->
      let init = if mark = "" then init else trm_add_mark mark init in
      
      begin match vk with
      | Var_immutable ->
        let new_lback = begin match unfold_at with
        | [] -> Mlist.map (Internal.subst_var x init) lback
        | _ -> Mlist.map (Internal.change_trm ~change_at:[unfold_at] (trm_var x) init) lback
        end
         in aux new_lback 
        
      | Var_mutable -> if trm_annot_has Reference dl then 
          let new_lback = begin match unfold_at with
          | [] -> Mlist.map (Internal.subst_var x init) lback
          | _ -> Mlist.map (Internal.change_trm ~change_at:[unfold_at] (trm_var x) init) lback
          end
           in aux new_lback 
          else fail dl.loc "unfold_aux: only const variables are safe to unfold"
      end
    | Trm_let_fun (f, _, _, _) ->
      if accept_functions then
        let new_lback = Mlist.map (Internal.subst_var f dl) lback in 
          aux new_lback
      else fail dl.loc "unfold_aux: to replace function calls with their declaration you need to set accept_functions arg to true"
    | _ -> fail t.loc "unfold_aux: expected a target to a variable declaration"
    end
  | _ -> fail t.loc "unfold_aux: expected the surrounding sequence"


let unfold (delete_decl : bool) (accept_functions : bool) (mark : mark) (unfold_at : target) (index : int) : Target.Transfo.local =
  Target.apply_on_path(unfold_aux delete_decl accept_functions mark unfold_at index)


(* [rename_aux index new_name t] rename the variable declared in [t] and all its occurrences
   params:
     [new_name]: the new name for the targeted variable
     [t]: ast of the declaration
   return:
    updated sequence with the renamed occurrences of the targeted variable
*)
let rename_aux (index : int) (new_name : var) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with
    | Trm_let (vk, (x, tx), init) ->
      let rec aux (t : trm) : trm =
        match t.desc with
        | Trm_var (vk, y) when y = x -> {t with desc = Trm_var (vk, new_name)}
        | _ -> trm_map aux t
       in
      let lback = Mlist.map aux lback in
      let new_dl = trm_let ~annot:dl.annot  ~marks:dl.marks vk (new_name, tx) init in
      let new_tl = Mlist.merge lfront lback in
      let new_tl = Mlist.insert_at index new_dl new_tl in
      trm_seq ~annot:t.annot ~marks:t.marks new_tl
    | _ -> fail t.loc "rename_aux: expected a declaration"
    end
  | _ -> fail t.loc "rename_aux: expected the surrounding sequence of the targeted declaration"


let rename (new_name : var) (index : int): Target.Transfo.local =
  Target.apply_on_path (rename_aux index new_name)

(* [subst_aux name space t]: replace all occurrences of [name] with [space]
      params:
        [name]: name of the variable whose occurrences are going to be replaced
        [space]:trm which is going to replace all the occurrences of [name]
        [t]: any node in the ast which could contain an occurrence of [name]
      return:
        updated [t] with all the replaced occurrences
*)

let rec subst_aux (name : var) (space : trm) (t : trm) : trm =
  match t.desc with
  | Trm_var (_, y) when y = name -> space
  | _ -> trm_map (subst_aux name space) t

let subst (name : var)(space : trm) : Target.Transfo.local =
  Target.apply_on_path (subst_aux name space)



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
        let var_decl = trm_let_mut (x, var_type) (trm_uninitialized ()) in
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
      if nb_occs = 0 then raise Init_attach_no_occurrences
       else if nb_occs >= 2 then raise Init_attach_occurrence_below_control;
      Tools.fold_lefti (fun i acc p ->
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
      [curr_var]: the previous name of the variable, this is used to find all its occurrences
      [local_var]: the name of the variable to be declared and replace all the occurrences of curr_var
      [t]: ast of the trm which contains curr_var.
    return:
      the ast of a marked(clean) nobrace sequence depending on the flag [mark] which contains
        the term [t] the new declaration and a set operation at the end

*)
let local_name_aux (mark : mark) (curr_var : var) (local_var : var) (t : trm) : trm =
  let vardef_trm = Target.get_trm_at [Target.cVarDef curr_var] in
  let var_type = match trm_var_def_inv vardef_trm with
  | Some (_, _, ty, _) -> ty
  | _ -> fail vardef_trm.loc "local_name: make sure the name of the current var is entered correctly" in
  let fst_instr = trm_let_mut (local_var, var_type) (trm_var curr_var) in
  let lst_instr = trm_set (trm_var ~typ:(Some var_type) curr_var) (trm_var_possibly_mut ~typ:(Some var_type) local_var) in 
  let new_t = Internal.change_trm (trm_var curr_var) (trm_var local_var) t in
  let final_trm = trm_seq_no_brace [fst_instr;new_t;lst_instr] in
  if mark <> "" then trm_add_mark mark final_trm else final_trm

let local_name (mark : mark) (curr_var : var) (local_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_name_aux mark curr_var local_var)

(* [delocalize_aux array_size ops index t]: after introduced the local_name transformation
      transform the newly declared local variable [local_var]  into an array of size [array size]
      and add to loops: the first one to initialize the elements of the added array and the last one
      to reduce the array into a single value, the inittialization value and the reducing(fold_lefting operation)
      is given through [ops].
    params:
      [array_size]: size of the arrays to be declared inside the targeted sequence
      [ops]: delocalize operation representing the unitary lement used for initialization
        and the fold_lefting operation used for the reduction
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
          trm_lit li,  (trm_prim_compound_encoded_as_set op
                             curr_var_trm
                              (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_var index]))
      | Delocalize_obj (clear_f, transfer_f) ->
          trm_apps ~typ:(Some (typ_unit ())) (trm_var clear_f) [],
          trm_apps ~typ:(Some (typ_unit())) (trm_var transfer_f)
            [add_star_if_ptr curr_var_trm ;
            add_star_if_ptr  (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_var index])]
      end in
      let new_first_trm = trm_seq_no_brace[
          trm_let_array vk (local_var, var_type) (Trm (trm_var array_size)) (trm_uninitialized ());
          trm_set (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_lit (Lit_int 0)]) curr_var_trm;
          trm_for index (trm_int 1)  DirUp (trm_var array_size) Post_inc
         (trm_seq_nomarks [trm_set (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_var index]) init_trm])]
          in
      let new_snd_instr = Internal.subst_var local_var  (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_apps (trm_var "ANY") [trm_var array_size] ]) snd_instr  in
      let new_thrd_trm = trm_seq_no_brace [
                      trm_set (curr_var_trm) (trm_apps (trm_binop Binop_array_access)[trm_var local_var; trm_lit (Lit_int 0)]);
                      (* trm_omp_directive (Parallel_for [Reduction (Plus,["a"])]); *)
                      trm_for index (trm_int 1) DirUp (trm_var array_size) Post_inc
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
      [typ]: the type of the inserted variable
      [value]: the initial value of the inserted variable [name] entered as a string
      [t]: the ast of the sequence where the insertion is performed
    return:
      the updated [t] with the newly inserted declaration []
  *)

let insert_aux (index : int) (const : bool) (name : string) (typ : typ) (value : trm) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_decl = if const then trm_let_immut (name, typ) value else trm_let_mut (name, typ) value in
    let new_tl = Mlist.insert_at index new_decl tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "insert_aux: expected the sequence where the declaration is oing to be inserted"

let insert (index : int) (const : bool) (name : string) (typ : typ) (value : trm) : Target.Transfo.local =
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
  let new_type = atyp new_type in
  match t.desc with
  | Trm_seq tl ->
    let lfront, decl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match decl.desc with
    | Trm_let (vk, (x, tx), init) ->
      let new_decl = Internal.change_typ tx new_type decl in
      let lback = Mlist.map (Internal.change_typ (get_inner_ptr_type tx) new_type ~change_at:[[Target.cVar x]]) lback in
      let tl = Mlist.merge lfront lback in
      let tl = Mlist.insert_at index new_decl tl in
      trm_seq ~annot:t.annot ~marks:t.marks tl
    | _ -> fail t.loc "change_type_aux: expected a variable or a function declaration"
    end
  | _ -> fail t.loc "change_type_aux: expected the surrounding sequence"

let change_type (new_type : typvar) (index : int) : Target.Transfo.local =
  Target.apply_on_path (change_type_aux new_type index)


(* [bind_aux index fresh_name const p_local t]: bind the variable [fresh_name] to the function_call
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
let bind_aux (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (p_local : path) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, instr, lback = Internal.get_trm_and_its_relatives index tl in
    let targeted_node = Path.resolve_path p_local instr in
    let has_reference_type = if (Str.string_before fresh_name 1) = "&" then true else false in
    let fresh_name = if has_reference_type then (Str.string_after fresh_name 1) else fresh_name in
    let node_type = match targeted_node.typ with
    | Some ty -> ty
    | _ -> typ_auto() in
    let node_to_change = Internal.change_trm targeted_node (trm_var_possibly_mut ~const ~typ:(Some node_type) fresh_name) instr in 

    let targeted_node = if my_mark <> "" then trm_add_mark my_mark targeted_node else targeted_node in
    
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
  | _ -> fail t.loc "bind_aux: expected the surrounding sequence"


let bind (my_mark : mark) (index : int) (fresh_name : var) (const : bool) (p_local : path) : Target.Transfo.local =
  Target.apply_on_path (bind_aux my_mark index fresh_name const p_local)


(* [to_const_aux index t] change the mutability of a variable, and replace all the get operations on that variable
    with an occurrence of that variable
    params:
      [index]: the index of the targeted declaration inside its englobing sequence
      [t]: ast of the surrounding sequence of the targeted instruction
    return:
      the update [t]
*)

let from_to_const_aux (const : bool) (index : int) (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl -> 
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with 
    | Trm_let (vk, (x, tx), init) ->
      let aux (new_dl : trm) (new_lback : trm mlist) : trm = 
      let new_tl = Mlist.merge lfront new_lback in 
      let new_tl = Mlist.insert_at index new_dl new_tl in 
        trm_seq ~annot:t.annot ~marks:t.marks new_tl
       in
      begin match vk with 
      | Var_immutable -> 
        if const then t
           else begin
            let init_val = match get_init_val init with 
            | Some init1 -> init1
            | _ -> fail dl.loc "to_const_aux: const variables should always be initialized" in
            let init_type = get_inner_const_type tx in 
            let new_dl = trm_let_mut ~marks:dl.marks (x, init_type) init_val in 
            (* replace all x with get(x) *)
            let new_lback = Mlist.map (Internal.change_trm (trm_var x) (trm_var_possibly_mut ~typ:(Some init_type) x)) lback in 
            aux new_dl new_lback    
           
            end
      | Var_mutable -> 
        if trm_annot_has Reference dl then fail dl.loc "from_to_const_aux: const references are not supported"
         else if not const then t
         else begin 
         (* search if there are any write operations inside the same scope *)
          Mlist.iter (fun t1 ->
            begin match t1.desc with
            | Trm_apps (_, [ls; _rs]) when is_set_operation t1 ->
              begin match ls.desc with
              | Trm_var (_, y) when y = x -> fail ls.loc "to_const_aux: can't convert a variable to a const variable if there are other write operations besides the first initalization"
              | _ -> ()
              end
            | _ -> ()
            end
          ) lback;
          (* replace all get(x) with x *)
          let init_val = match get_init_val init with
          | Some init1 -> init1
          | _ -> fail dl.loc "to_const_aux: can't convert to const a non intialized variable"
          in
          let init_type = get_inner_ptr_type tx in
          let new_dl = trm_let_immut ~marks:dl.marks (x, init_type) init_val in
          let new_lback = Mlist.map (Internal.change_trm (trm_var_possibly_mut ~typ:(Some init_type) x) (trm_var x)) lback in 
          aux new_dl new_lback
          end
    end
    | _ -> fail dl.loc "from_to_const_aux: expected a target to a variable declaration"
    end
  | _ -> fail t.loc "from_to_const_aux: expected the sequence that contains the targeted declaration"


let from_to_const (const : bool) (index : int) : Target.Transfo.local =
  Target.apply_on_path (from_to_const_aux const index )

let to_const_aux (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, dl, lback = Internal.get_trm_and_its_relatives index tl in
    begin match dl.desc with
    | Trm_let (vk, (x, tx), init) ->
      begin match vk with
      | Var_immutable -> t
      | Var_mutable ->
        (* first search if there are any write operations inside the same scope *)
        Mlist.iter (fun t1 ->
          begin match t1.desc with
          | Trm_apps (_, [ls; _rs]) when is_set_operation t1 ->
            begin match ls.desc with
            | Trm_var (_, y) when y = x -> fail ls.loc "to_const_aux: can't convert a variable to a const variable if there are other write operations besides the first initalization"
            | _ -> ()
            end
          | _ -> ()
          end
        ) lback;
        (* replace all get(x) with x *)
        let init_val = match get_init_val init with
        | Some init1 -> init1
        | _ -> fail dl.loc "to_const_aux: can't convert to const a non intialized variable"
        in
        let init_type = get_inner_ptr_type tx in
        let new_dl = trm_let_immut ~marks:dl.marks (x, init_type) init_val in
        let new_lback = Mlist.map (Internal.change_trm (trm_var_possibly_mut ~typ:(Some init_type) x) (trm_var x)) lback in 
        let new_tl = Mlist.merge lfront new_lback in
        let new_tl = Mlist.insert_at index new_dl new_tl in
        trm_seq ~annot:t.annot ~marks:t.marks new_tl
      end

    | _ -> fail t.loc "to_const_aux: expected a target to variable declaration"
    end

  | _ -> fail t.loc "to_const_aux: expected the sequence that contains the targeted declaration"

let to_const (index : int) : Target.Transfo.local =
  Target.apply_on_path (to_const_aux index)

(* [simpl_deref_aux t] check if [t] is of the form *(&b) or *(&b), if that is the case
      then simplify that expression and return it, otherwise if [indepth] is true then
      search in depth of [t] else return [t]
    params:
      [t]: a node that represents one of the epxression *(&b) or &( *b)
    return:
      updated ast
*)

let simpl_deref_aux (indepth : bool) (t : trm) : trm =
  let aux = trm_simplify_addressof_and_get in 
  if indepth then trm_map aux t else aux t

let simpl_deref (indepth : bool) : Target.Transfo.local =
  Target.apply_on_path (simpl_deref_aux indepth)