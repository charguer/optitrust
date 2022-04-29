open Ast

(* [update f t]: replaces an expression [t] with [f t]. *)
let update (f : trm -> trm) : Target.Transfo.local =
  Target.apply_on_path f


(* [replace_fun name t]: change the current function call to another 
      function call where the name has been changed to [name]
    params:
      [name]: name of the function replacing the targeted one
      [t]: ast of the function call trm
    return:
      updated ast with the replaced trm *)

let replace_fun_aux (name : string) (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, args) ->
    trm_apps ~annot:t.annot ~marks:t.marks ~typ:t.typ (trm_var name) args
  | _ -> fail t.loc "replace_fun: expected a function call"


(* [replace_fun name t p]: apply [replace_fun_aux] at trm [t] with path [p] *)
let replace_fun (name : string) : Target.Transfo.local =
  Target.apply_on_path (replace_fun_aux name)
