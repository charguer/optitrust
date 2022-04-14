open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [add_aux m t] add mark m to the node t
    params:
      [m]: the mark which is addded
      [t]: the ast of the trm where the mark is added to
    return:
      trm [t] with mark m *)
let add_aux (m : mark) (t : trm) : trm =
  trm_add_mark m t

let add (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_aux m)


(* [add_between_aux index m t] add mark m at [index] in sequence [t]
    params:
      [index]: the relative index where the mark should be added
      [m]: the mark that is addded
      [t]: the ast of the trm where the mark is added to
    return:
      trm [t] with mark m *)
let add_between_aux (index : int) (m : mark) (t : trm) : trm =
  trm_add_mark_between index m t

let add_between (index : int) (m : mark) : Target.Transfo.local =
  Target.apply_on_path (add_between_aux index m)

(* [remove_aux m t] remove mark m from node t
    params:
      [m]: the mark which is removed
      [t]: the ast of the trm where the mark is removed
    return:
      trm [t] without the mark m *)
let remove_aux (m : mark) (t : trm) : trm =
  trm_remove_mark (m) t

let remove (m : mark) : Target.Transfo.local =
  Target.apply_on_path (remove_aux m)

(* [clean_aux m t] remove all marks from note t²
    params:
      [t]: the ast where the marks are removed from
    return:
      trm [t] without any mark *)
let clean_aux (t : trm) : trm =
  trm_remove_marks t

let clean : Target.Transfo.local =
  Target.apply_on_path (clean_aux)