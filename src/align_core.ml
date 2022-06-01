open Ast

(* [deg_aux vec_align t]: adds the alignas attribute to the  declaration [t],
      [vec_align] - alignment size,
      [t] - ast of the declaration. *)
let def_aux (vec_align : trm) (t : trm) : trm =
  let error = "Alig_core.def_aux: expected a target at a variable declaration" in
  let (vk, x, tx, init) = trm_inv ~error trm_let_inv t in
  let tx2 = typ_map (fun ty -> typ_align vec_align ty) (get_inner_ptr_type tx) in
    { t with desc = Trm_let (vk, (x, typ_ptr_generated tx2), init) }


(* [def vec_align t p]: applies [def_aux] at the trm with path [p]. *)
let def (vec_align : trm) : Target.Transfo.local =
  Target.apply_on_path (def_aux vec_align)
