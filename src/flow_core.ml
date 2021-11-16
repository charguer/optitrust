open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [insert_if single_branch index cond t]: take one or two instructions and create an if statement
      or an if else statment if [single_brnach] is true.
    params:
      [cond]: condition of the if statement given as string code
      [t]: ast of the outer sequence containing the instruction
    return:
      updated ast of the surrounding sequence with the added if statement
 *)
let insert_if_aux (cond : trm) (t : trm) : trm =
  begin match t.desc with 
  | Trm_seq _ -> trm_if cond t t
  | _ -> trm_if cond (trm_seq_nomarks [t]) (trm_seq_nomarks [t])
  end 

let insert_if (cond : trm) : Target.Transfo.local =
  Target.apply_on_path (insert_if_aux cond)

