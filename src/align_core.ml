open Ast



let def_aux (vec_align : trm) (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x, tx), init) ->
    let tx2 = { tx with typ_attributes = (Alignas vec_align) :: tx.typ_attributes } in
    { t with desc = Trm_let (vk, (x, tx2), init) }
  | _ -> fail t.loc "def_aux: expected a variable declaration as a target"

let def (vec_align : trm) : Target.Transfo.local =
  Target.apply_on_path (def_aux vec_align)