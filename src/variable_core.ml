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
      fold_at: targets where folding should be performed, if left empty 
        then folding is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast 
*)
let fold_aux (as_reference : bool) (fold_at : target list) (index : int) (t : trm) : trm=
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d, lback = Tools.split_list_at 1 lback in
    let d = List.hd d in
    begin match d.desc with
    | Trm_let (vk, (x, _), dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
          if not as_reference then 
            begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "fold_aux: expected a new operation"
                   end
            end
          else
            match dx.add with
            | Add_address_of_operator :: addl -> {dx with add = addl}
            | _ -> fail d.loc "fold_decl: expected a reference"
        in
        let lback = List.map(Internal.change_trm ~change_at:fold_at def_x t_x) lback
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        in

        trm_seq (lfront @ [d] @ lback)

     | _ -> fail t.loc "fold_decl: expected a variable declaration"
     end


  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (as_reference : bool) (fold_at : target list) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux as_reference fold_at index)


(* [inline_aux inline_at]: inline variable defined in term t
    params:
      delete_decl: delete or don't delete the declaration of the variable after inlining
      inline_at: targets where inlining should be performed, if empty inlining is applied everywhere
      t: ast of the variable declaration
    return:
      updated ast
*)
let inline_aux (delete_decl : bool) (inline_at : target list) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let dl, lback = Tools.split_list_at 1 lback in
    let dl = List.hd dl in
    begin match dl.desc with
    | Trm_let (vk, (x, _), dx) ->
      let t_x = 
      begin match vk with 
      | Var_immutable -> trm_var x
      | _ -> trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [trm_var x] 
      end
       in
      let def_x = 
      begin match vk with 
            | Var_immutable -> dx
            | _ -> begin match dx.desc with 
                   | Trm_apps(_, [init]) -> init
                   | _ -> fail t.loc "inline_aux: expected a new operation"
                   end
      end 
       in
      let lback = List.map (Internal.change_trm ~change_at:inline_at t_x def_x) lback in
      let tl =
        if delete_decl then lfront @ lback
        else lfront @ [dl] @ lback
      in
      trm_seq ~annot:t.annot tl
    | _ -> fail t.loc "inline_aux: expected a variable declaration"
    end
  | _ -> fail t.loc "inline_aux: expected the surrounding sequence"


let inline (delete_decl : bool) (inline_at : target list) (index : int) : Target.Transfo.local =
  Target.apply_on_path(inline_aux delete_decl inline_at index)

(* [rename_aux new_name index t] rename a variable, change its declaration
      and all its occurrences
   params:
    new_name: string used to replace current name and occurrences
    index: index of the declaration inside the sequence it belongs to
   return:
    updated ast 
*)
let rename_aux (list : (string * string) list) (func : string -> string) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    List.fold_left (fun acc t1 ->
        match t1.desc with
        | Trm_let (vk,(x, tx), init) ->
          begin match list with 
          | [] -> 
            let acc = Internal.change_trm t1 (trm_let vk ((func x), tx) init) acc in
            Internal.change_trm (trm_var x) (trm_var (func x)) acc
          | _ -> 
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

let rename (list : (string * string) list) (func : string -> string) : Target.Transfo.local =
  Target.apply_on_path (rename_aux list func)

