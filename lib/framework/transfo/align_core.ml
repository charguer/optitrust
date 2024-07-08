open Prelude

(** [def_on vec_align t]: adds the alignas attribute to the  declaration [t],
      [vec_align] - alignment size,
      [t] - ast of the declaration. *)
let def_on (vec_align : trm) (t : trm) : trm =
  let error = "Alig_core.def_on: expected a target at a variable declaration" in
  let (x, tx, init) = trm_inv ~error trm_let_inv t in
  let tx1 = trm_map (fun ty -> typ_align vec_align ty) tx in
  (* let tx1 = typ_align vec_align tx in  *)
  { t with desc = Trm_let ((x, tx1), init) }
