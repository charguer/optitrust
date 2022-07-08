open Ast

(* [deg_aux vec_align t]: adds the alignas attribute to the  declaration [t],
      [vec_align] - alignment size,
      [t] - ast of the declaration. *)
let def_aux (vec_align : trm) (t : trm) : trm =
  let error = "Alig_core.def_aux: expected a target at a variable declaration" in
  let (vk, x, tx, init) = trm_inv ~error trm_let_inv t in
  (* let tx1 = typ_map (fun ty -> typ_align vec_align ty) tx in *)
  let tx1 = typ_align vec_align tx in 
  { t with desc = Trm_let (vk, (x, tx1), init) }


(* [def vec_align t p]: applies [def_aux] at the trm with path [p]. *)
let def (vec_align : trm) : Target.Transfo.local =
  Target.apply_on_path (def_aux vec_align)
