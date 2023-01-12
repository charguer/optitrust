open Optitrust
open Target
open Ast

(* Example struct_modif transformation #1: scaling field "x" by a factor 2 *)

let fieldtochange =
  "x"
let factor =
  trm_int 2
let get_div_by_factor t =
  trm_div t factor
  (* variant: trm_shiftr t (trm_int 1) *)

let set_mul_by_factor t =
  trm_mul t factor

let myscaling tg =
  let arg = Record_basic.Struct_modif.(
    let f_fields = fields_identity in (* fields names and types are not modified *)

    let f_access _aux _t = (* t is [access(base,field)] *)
      assert false in (* accesses on double fields must be part of a get or set operation *)

    let f_struct_get aux t = (* t is [struct_get(base,field)] *)
      let (base, field) = struct_get_inv_some t in
      let base = aux base in
      let t' = reuse_annot_of t (trm_struct_get base field) in
      if field = fieldtochange then get_div_by_factor t' else t'
      in

    let f_get aux t = (* t is [get(access(base,field))] *)
      let (base, field) = get_struct_access_inv_some t in
      let base = aux base in
      let t' = reuse_annot_of t (trm_get (struct_access field base)) in
      if field = fieldtochange then get_div_by_factor t' else t'
      in

    let f_set aux t = (* t is [set(access(base,field), rhs)] *)
      let (base, field, rhs) = set_struct_access_inv_some t in
      let base = aux base in
      let rhs = aux rhs in
      let rhs' = if field = fieldtochange then set_mul_by_factor rhs else rhs in
      reuse_annot_of t (trm_set (struct_access field base) rhs') in

    let f_alloc (oldfields,_newfields) aux t : trm = (* t is [trm_record[t1;..;tn]] *)
      let sl = struct_init_inv_some t in
      assert (Mlist.length sl = List.length oldfields); (* else trm_record could not have the targeted type *)
      let fix_field i (lb, ti) =
        let (field,_typ_field) = List.nth oldfields i in
        let ti = aux ti in
        if field = fieldtochange then (lb, set_mul_by_factor ti) else (lb, ti) in
      trm_record (Mlist.mapi fix_field sl) in

    { f_fields; f_get; f_set; f_struct_get; f_access; f_alloc }) in
  Record_basic.struct_modif arg tg


(* Example struct_modif transformation #2: adding a suffix to all field names *)

let mysuffix (suffix : string) tg =
  let arg = Record_basic.Struct_modif.(
    let f_fields fields : fields =
      List.map (fun (x,t) -> (x^suffix, t)) fields in

    let f_access aux t = (* t is [access(base,field)] *)
      let (field, base) = struct_get_inv_some t in
      let base = aux base in
      reuse_annot_of t (struct_access (field^suffix) base) in

    let f_struct_get aux t = (* t is [struct_get(base,field)] *)
      let (field, base) = struct_get_inv_some t in
      let base = aux base in
      reuse_annot_of t (trm_struct_get base (field^suffix)) in

    let f_get aux t = (* t is [get(access(base,field))]   that is   "base.field" *)
      let (field, base) = get_struct_access_inv_some t in
      let base = aux base in
      reuse_annot_of t (trm_get (struct_access (field^suffix) base)) in

    let f_set aux t = (* t is [set(access(base,field), rhs)]  that is   "base.field = rhs"   *)
      let (field, base, rhs) = set_struct_access_inv_some t in
      let base = aux base in
      let rhs = aux rhs in
      reuse_annot_of t (trm_set (struct_access (field^suffix) base) rhs) in

    let f_alloc (oldfields,_newfields) aux t : trm = (* t is [trm_record[t1;..;tn]] *)
      let sl = struct_init_inv_some t in
      assert (Mlist.length sl = List.length oldfields); (* else trm_record could not have the targeted type *)
      let fix_field i (lb, ti) =
        let (_field,_typ_field) = List.nth oldfields i in (* not needed here *)
         (lb, aux ti) 
       in
      reuse_annot_of t (trm_record (Mlist.mapi fix_field sl)) in

    { f_fields; f_get; f_set; f_struct_get; f_access; f_alloc }) in
  Record_basic.struct_modif arg tg


let _ = Run.script_cpp (fun _ ->

   !! myscaling [cTypDef "vect"];
   !! mysuffix "_foo" [cTypDef "vect"];
)


(* LATER: trm_record will be equipped with labels, simplifying the fix_field operation *)

(* FOR DEBUG
      List.iter (fun (s,_) -> Printf.printf "%s " s) oldfields;
      Mlist.iter  (fun s -> Printf.printf "%s " (AstC_to_c.ast_to_string s)) sl;
*)