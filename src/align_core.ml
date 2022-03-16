open Ast



let def_aux (vec_align : trm) (t : trm) : trm = 
  match t.desc with 
  | Trm_let (vk, (x, tx), init) -> 
    trm_attr_add (Alignas vec_align) t
  | _ -> fail t.loc "def_aux: expected a variable declaration as a target"

let def (vec_align : trm) : Target.Transfo.local = 
  Target.apply_on_path (def_aux vec_align)