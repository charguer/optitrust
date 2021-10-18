open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [apply_aux op arg t]: apply binary_operation op on [t] with the second arguement of the operation being [arg]
    params:
      [op]: the binary operation going to be applied
      [arg]: the second argument after [t] in the performed operation
      [t]: the first argument in the performed operation
    return:
      the ast of the binary operation
*)
let apply_aux (op : binary_op) (arg : trm) (t : trm) : trm = 
  trm_apps (trm_binop op) [t; arg]


let apply (op : binary_op) (arg : trm) : Target.Transfo.local =
  Target.apply_on_path (apply_aux op arg)