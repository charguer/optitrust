open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [data_shif_aux neg pre_cast post_cast u t]: shift the right node t by term [u]
    params:
      [neg]: a flag for the sine of shifting
      [pre_cast]: casting of type [pre_cast] performed on the trm t before applying
       the shifting operation
      [post_cast]: casting of type [post_cast] performed on the trm t after applying 
        the shifting operation
      [u]: shift size
      [t]: the ast of the trm which is going to be shiften
    return:
      the updated ast of t shifted by u
*)
let shift_aux (neg : bool) (pre_cast : typ option) (post_cast : typ option) (u : trm) (t : trm) : trm =
    let binop_op = if neg then Binop_sub else Binop_add in
    begin match pre_cast, post_cast with
    | None , None -> trm_apps (trm_binop binop_op) [t; u]
    | None, Some ty -> trm_cast ty (trm_apps (trm_binop binop_op) [t; u])
    | Some ty, None -> trm_apps (trm_binop binop_op) [trm_cast ty t; u]
    | _ -> fail t.loc "shift_aux: can'd do both pre-casting and post-casting"
    end


let shift (neg : bool) (pre_cast : typ option) (post_cast : typ option) (u : trm) : Target.Transfo.local =
  Target.apply_on_path (shift_aux neg pre_cast post_cast u)
