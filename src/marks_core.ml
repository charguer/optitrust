open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)


let add_aux (m : mark) (t : trm) : trm =
  trm_add_mark m t

let add (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_aux m)

let remove_aux (m : mark) (t : trm) : trm =
  trm_remove_mark m t

let remove (m : mark) : Target.Transfo.local =
  Target.apply_on_path (remove_aux m)

let clean_aux (t : trm) : trm =
  trm_remove_marks t

let clean : Target.Transfo.local =
  Target.apply_on_path (clean_aux)