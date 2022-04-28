open Optitrust
open Target
open Ast

let fieldtochange =
  "x"
let factor =
  trm_int 2
let get_div_by_factor t =
  Arith_core.apply_aux Binop_div factor t
let set_mul_by_factor t =
  Arith_core.apply_aux Binop_mul factor t

let mytransfo tg =
  let arg = Struct_basic.Struct_modif.(
      let f_struct_get = arg_identity in
      let f_access = arg_must_not_happen in
      let f_get = arg_keep_annot (fun _aux t ->
        match get_struct_access_inv t with
        | None -> assert false
        | Some (field, base) ->  (* t is [get(access(base,field))] *)
           (* Here the code to wrap around read operations *)
            if field <> fieldtochange then t else begin
              get_div_by_factor t
            end) in
      let f_set = arg_keep_annot (fun aux t ->
        match set_struct_access_inv t with
        | None -> assert false
        | Some (field, base, rhs) ->  (* t is [set(access(base,field), rhs)] *)
             (* Here the code to wrap around write operations *)
             if field <> fieldtochange then t else begin
               let base = aux base in
               let rhs = aux rhs in
               trm_set (struct_access field base) (set_mul_by_factor rhs)
             end
        ) in
      { f_fields = Fun.id; f_get; f_set; f_struct_get; f_access; }) in
  Struct_basic.struct_modif arg tg

let _ = Run.script_cpp (fun _ ->

   !! mytransfo [cTypDef "vect"];
)
