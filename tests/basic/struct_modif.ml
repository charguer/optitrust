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

let myscaling tg =
  let arg = Struct_basic.Struct_modif.(
    let f_fields = fields_identity in (* fields names and types are not modified *)

    let f_access _aux _t = (* t is [access(base,field)] *)
      assert false in (* accesses on double fields must be part of a get or set operation *)

    let f_struct_get aux t = (* t is [struct_get(base,field)] *)
      let (field, base) = struct_get_inv_some t in
      let base = aux base in
      let t' = reuse_annot_of t (trm_struct_get base field) in
      if field = fieldtochange then get_div_by_factor t' else t'
      in

    let f_get aux t = (* t is [get(access(base,field))] *)
      let (field, base) = get_struct_access_inv_some t in
      let base = aux base in
      let t' = reuse_annot_of t (trm_get (struct_access field base)) in
      if field = fieldtochange then get_div_by_factor t' else t'
      in

    let f_set aux t = (* t is [set(access(base,field), rhs)] *)
      let (field, base, rhs) = set_struct_access_inv_some t in
      let base = aux base in
      let rhs = aux rhs in
      let rhs' = if field = fieldtochange then set_mul_by_factor rhs else rhs in
      reuse_annot_of t (trm_set (struct_access field base) rhs') in

    let f_alloc (oldfields,_newfields) aux t : trm = (* t is [trm_struct[t1;..;tn]] *)
      let sl = struct_init_inv_some t in
      List.iter (fun (s,_) -> Printf.printf "%s " s) oldfields;
      Mlist.iter  (fun s -> Printf.printf "%s " (AstC_to_c.ast_to_string s)) sl;
      assert (Mlist.length sl = List.length oldfields); (* else trm_struct could not have the targeted type *)
      let fix_field i ti =
        let (field,_tfield) = List.nth oldfields i in
        let ti = aux ti in
        if field = fieldtochange then set_mul_by_factor ti else ti
        in
      trm_struct (Mlist.mapi fix_field sl) in

    { f_fields; f_get; f_set; f_struct_get; f_access; f_alloc }) in
  Struct_basic.struct_modif arg tg
(*
let mysuffix (suffix : string) tg =
  let arg = Struct_basic.Struct_modif.(

    let f_fields = List.map (fun (x,t) -> (x^suffix,t)) in
    let f_struct_get = arg_identity in
    let f_access = arg_must_not_happen in
    let f_get = arg_keep_annot (fun _aux t ->
      let (field, base) get_struct_access_inv_some t with
      | None -> assert false
      | Some  ->  (* t is [get(access(base,field))] *)
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
    let f_alloc = arg_keep_annot (fun _aux t ->
      match struct_init_inv t with
      | None -> assert false
      | Some sl ->
          trm_struct (Mlist.map set_mul_by_factor sl)
      ) in
    { f_fields; f_get; f_set; f_struct_get; f_access; f_alloc}) in
  Struct_basic.struct_modif arg tg

*)

let _ = Run.script_cpp (fun _ ->

   !! myscaling [cTypDef "vect"];
(*   !! mysuffix "_foo" [cTypDef "vect"];*)
)
