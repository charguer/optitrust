open Optitrust
open Target
open Ast

let factor = trm_int 2
let get t = Arith_core.apply_aux Binop_div factor t
let set t = Arith_core.apply_aux Binop_mul factor t

let _ = Run.script_cpp (fun _ ->


   let arg = Struct_basic.Struct_modif.(
      let f_get = arg_keep_annot (fun _aux t -> get t) in
      let f_struct_get = arg_identity in
      let f_access = arg_must_not_happen in
      let f_set = arg_keep_annot (fun aux t ->
        match set_struct_access_inv t with
        | None -> assert false
        | Some (field, base, rhs) ->  (* t is [set(access(base,field), rhs)] *)
              let base = aux base in
              let rhs = aux rhs in
             trm_set (struct_access field base ) (Arith_core.apply_aux Binop_mul factor rhs)
        ) in
      { f_fields = Fun.id; f_get; f_set; f_struct_get; f_access; }) in
   !! Struct_basic.struct_modif arg [cTypDef "vect"];
)
