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
    return:
      the updated ast
*)

let var_init_detach (path_to_decl : path) (t : trm) : trm =
  apply_local_transformation(var_init_detach_aux ) t path_to_decl