open Ast

(* ***********************************************************************************
 * Note: All the intermediate functions which are called from [sequence.ml] file      *
 * have only one purpose, and that is targeting the trm in which we want to apply the *
 * transformation. That's why there is not need to document them.                     *
 *)

(* [data_shif_aux neg pre_cast post_cast u t]: shift the right hand side of a set operation with term [u]
    params:
      new: a flag for the sine of shifting
      pre_cast: casting of type [pre_cast] performed on the right hand side of the set operation before shifting is applied
      post_cast: casting of type [post_cast] performed after shifting is done
      u: shift size
      t: the ast of teh set operation
    return:
      the updated set operation
*)
let shift_aux (neg : bool) (pre_cast : typ) (post_cast : typ) (u : trm) (t : trm) : trm =
    Tools.printf "%s\n" (Ast_to_c.ast_to_string t);
    let binop_op = if neg then Binop_sub else Binop_add in
    begin match pre_cast.typ_desc, post_cast.typ_desc with
    | Typ_unit , Typ_unit -> trm_apps (trm_binop binop_op) [t; u]
    | Typ_unit, _ -> trm_cast post_cast (trm_apps (trm_binop binop_op) [t; u])
      | _, Typ_unit -> trm_apps (trm_binop binop_op) [trm_cast pre_cast t; u]
      | _ -> fail t.loc "shift_aux: can'd do both precasting and postcasting"
      end


let shift (neg : bool) (pre_cast : typ) (post_cast : typ) (u : trm) : Target.Transfo.local =
  Target.apply_on_path (shift_aux neg pre_cast post_cast u)
