open Ast
open Target
open Path_constructors
(* [fold_aux as_reference fold_at]: This is an auxiliary function for fold
    params:
      as_reference
*)
let fold_aux (as_reference : bool) (fold_at : target list) (index : int) (t : trm) = 
  match t.desc with 
  | Trm_seq tl ->
    let lfront, lback = Tools.split_list_at index tl in
    let d, lback = Tools.split_list_at 0 lback in
    let d = List.hd d in
    begin match d.desc with 
    | Trm_let (_,(x,_),dx) ->
        let t_x =
          if as_reference then trm_apps (trm_unop Unop_get) [trm_var x]
          else trm_var x
        in
        let def_x =
          if not as_reference then dx
          else
            match dx.add with
            | Add_address_of_operator :: addl -> {dx with add = addl}
            | _ -> fail d.loc "fold_decl: expected a reference"
        in
        let lback = List.map(Generic_core.change_trm ~change_at:fold_at def_x t_x) lback
        (*
          def_x might have been replaced with x in the definition of x
          -> replace it again with def_x
         *)
        in
        let change_at =
         (* TODO: Fix later this temporary hack *)
          [[cVarDef x ~body:[cVar x ]; cBody]]
        in
        let lback = List.map (Generic_core.change_trm ~change_at t_x def_x) lback in
        trm_seq (lfront @ [d] @ lback)
     
     (* typedef *)
     | Trm_typedef td -> 
       begin match td with 
       | Typedef_abbrev (x,dx) -> 
        let ty_x = typ_var x (Clang_to_ast.get_typedef x) in 
        let lback = List.map(Generic_core.change_typ ~change_at:fold_at dx ty_x) lback in        
        let change_at = [[cTypDef x]] in
        let lback = List.map(Generic_core.change_typ ~change_at ty_x dx) lback in
        trm_seq (lfront @ [d] @ lback)
       | _ -> fail t.loc "fold_decl: expected a typedef"
       end
     (* fun decl *)
     | Trm_let_fun _ ->
        fail t.loc "fold_decl: fun declaration folding is unsupported"
     | _ -> fail t.loc "fold_decl: expected a definition"
     end
      
    
  | _ -> fail t.loc "fold_aux: expected the surrounding sequence"

let fold (as_reference : bool) (fold_at : target list) (index) : Target.Transfo.local =
  Target.apply_on_path(fold_aux as_reference fold_at index)

(* [insert_aux const as_reference x dx t]: This is an auxiliary function for insert
    params:
      const: boolean for the mutability of the declaration
      as_reference: boolean about the type of allocation
      x: name of the variable
      dx: value of the variable
    return:
      the updated ast
*)
let insert_aux (const : bool) (as_reference : bool) (x : var) (dx : trm) (index : int) (t : trm) : trm =
  match t.desc with 
  | Trm_seq tl ->
    let tx = match dx.typ with 
    | None -> fail t.loc "insert_aux: cannot find definition type"
    | Some tx -> if as_reference then typ_ptr tx else tx
    in
    let def_x = 
      if as_reference then {dx with add = Add_address_of_operator :: dx .add} 
      else dx
    in
    let t_insert =
      if const then trm_let Var_immutable (x,tx) def_x
      else
        trm_let Var_mutable (x, tx) (trm_apps (trm_prim (Prim_new tx)) [def_x])

    in
    let tl = Tools.list_insert (index) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_aux: expected the surrounding sequence"

let insert(const : bool) (as_reference : bool) (x : var) (dx : trm) (index : int) : Target.Transfo.local =
  Target.apply_on_path (insert_aux const as_reference x dx index)
