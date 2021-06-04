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

