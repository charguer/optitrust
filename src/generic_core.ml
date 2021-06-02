open Ast
open Target

(* var_init_detach_aux: This is an auxiliary function for var_init_detach
    params:
      subt: an ast subterm
    return:
      a sequence which contains the declaration of the variable and a set operations for that variable
*)
let var_init_detach_aux (subt : trm) : trm =
  match subt.desc with 
  | Trm_let(vk,(x, tx), init) ->
    begin match vk with 
    | Var_immutable -> fail subt.loc "var_init_detach_aux: const declarations cannot be detached"
    | _ ->
      let init = 
        begin match init.desc with 
        | Trm_apps(_,[init]) -> init
        | _ -> fail subt.loc "var_init_detach_aux: expected a heap allocated variable declaration"
        end in
      trm_seq [
        trm_let vk (x, tx) (trm_prim (Prim_new tx));
        trm_set (trm_var x) init
      ]
    end
  | _ -> fail subt.loc "var_init_detach_aux: variable could not be matched, make sure your path is correct"


(* var_init_detach: Split a variable declaration like int x = 5; into int x; x = 5;
    params:
      path_to_decl: path to the variable declaration
      t: ast
    return:
      the updated ast
*)

let var_init_detach (path_to_decl : path) (t : trm) : trm =
  apply_local_transformation(var_init_detach_aux ) t path_to_decl

(* var_init_attach_aux: This is an auxiliary function for var_init_attach
    params:
      subt: an ast subterm
    return
      the updated
*)
let var_init_attach_aux (subt : trm) : trm =
  match subt.desc with 
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
    | _ -> fail subt.loc "var_init_attach_aux: sequence does not satisfy the assumption described above"
    end
    in
    let var_init = begin match var_set.desc with 
    | Trm_apps(_, [_;init]) -> init
    | _ -> fail subt.loc "var_init_attach_aux: sequence does not satisfy the assumtion that the second term of the sequence is the set operations"
    end
    in
    trm_let ~loc:subt.loc var_kind (var_name, var_type) (trm_apps (trm_prim ~loc:subt.loc (Prim_new var_type)) [var_init])
  | _ -> fail subt.loc "var_init_attach_aux: sequence was not matched, make sure the path is correct"

(* var_init_attach: Change a sequence of the form {int x; x = 5;} to int x = 5 
    params:
      path_to_seq: path to the sequence which satisfy the assumtion above
      t: ast 
    return
      the updated ast
*)
let var_init_attach (path_to_seq : path) (t : trm) : trm =
  apply_local_transformation(var_init_attach_aux) t path_to_seq


(* const_non_const_aux: This is an auxiliary function for const_non_const
    params:
      subt: an ast subterm
    return:
      the updated ast
*)
let const_non_const_aux (subt : trm) : trm =
  match subt.desc with 
  | Trm_let (vk, (x,tx), init) ->
    begin match vk with
     (* If variable is a constant than whe remove the const and we perform the heap allocation  *)
    | Var_immutable ->  
      trm_let Var_mutable (x, typ_ptr tx) (trm_apps (trm_prim ~loc: subt.loc (Prim_new tx)) [init])
    | _ ->
      let var_type = begin match tx.ty_desc with 
      | Typ_ptr t -> t
      | _ -> fail subt.loc "const_non_const_aux: expected a pointer type"
      end
      in
      let var_init = begin match init.desc with 
      | Trm_apps(_, [_; init]) -> init
      | _ -> fail subt.loc "const_non_const_aux: expected a something of the form 'new ()'"
      end
      in
      trm_let Var_immutable (x,var_type) var_init 
    end
  | _ -> fail subt.loc "const_non_const_aux: variable declaration was not matched, make sure the path is correct"


(* const_non_const: change a const declaration to a non const declaration and vice_versa
    params:
      path_to_decl: path to the declaration
      t: ast
    return: 
      the updated ast
*)
let const_non_const (path_to_decl : path) (t : trm) : trm =
  apply_local_transformation(const_non_const_aux ) t path_to_decl


(* remove_instruction_aux: This is an auxiliary function for remove_instruction
    params:
      subt: an ast subterm
    return:
      the updated ast
*)
let remove_instruction_aux (_subt : trm) : trm =
  (* Replace the current subt with an empty sequence *)
  trm_seq ~annot:(Some No_braces) []

(* remove_instructon: delete an instruction
    params: 
      path_to_instr: path to the targeted instruction
      t: ast
    return
      the updated ast
*)
let remove_instruction (path_to_inst : path) (t : trm) : trm =
  apply_local_transformation(remove_instruction_aux ) t path_to_inst

