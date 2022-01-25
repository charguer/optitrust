open Ast
open Tools

(* map with keys variables and values their type 
  used for loops that do not declare their counter
*)

let ctx_var : typ varmap ref = ref String_map.empty

let ctx_tconstr : typconstrid varmap ref = ref String_map.empty

let ctx_typedef : typedef typmap ref = ref Typ_map.empty

let ctx_label : typconstrid varmap ref = ref String_map.empty

let ctx_constr : typconstrid varmap ref = ref String_map.empty

let debug_typedefs = false

let ctx_var_add (tv : typvar) (t : typ) : unit =
  ctx_var := String_map.add tv t (!ctx_var)

let ctx_tconstr_add (tn : typconstr) (tid : typconstrid) : unit =
  if debug_typedefs then printf "Type %s has been added into map with typconstrid %d\n" tn tid;
  ctx_tconstr := String_map.add tn tid (!ctx_tconstr)

let ctx_typedef_add (tn : typconstr) (tid : typconstrid) (td : typedef) : unit =
  if debug_typedefs then printf "Typedef for %s has been registered\n" tn;
  ctx_typedef := Typ_map.add tid td (!ctx_typedef)

let ctx_label_add (lb : label) (tid : typconstrid) : unit =
  ctx_label := String_map.add lb tid (!ctx_label)

let ctx_constr_add (c : constrname) (tid : typconstrid) : unit =
  ctx_constr := String_map.add c tid (!ctx_constr)

(* [get_ctx] returns the current context *)
let get_ctx () : ctx =
  { ctx_var = !ctx_var;
    ctx_tconstr = !ctx_tconstr;
    ctx_typedef = !ctx_typedef;
    ctx_label = !ctx_label;
    ctx_constr = !ctx_constr; }

(* [get_typid_from_trm ty] *)
let get_typid_from_trm (tv : typvar) : int  =
   let tid = String_map.find_opt tv !ctx_tconstr in
   begin match tid with
   | Some id -> id
   | None -> -1
   end

(* names for overloaded operators (later matched for printing) *)
let string_of_overloaded ?(loc : location = None) (op : C.binary_operator) : string = 
  match op with 
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Oassign -> "=" 
  | Oadd_assign -> "+=" 
  | Osub_assign -> "-="
  | Omul_assign -> "*="
  | _ -> fail loc "string_of_overloaded_op: non supported operator"
  

(* [overloaded op ~loc ~ctx op] *)
let overloade_op ?(loc : location = None) ?(ctx : ctx option = None) (op : C.binary_operator) : trm = 
  match op with 
  | Oadd -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_add))
  | Osub -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_sub))
  | Omul -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_mul))
  | Oassign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_set))
  | Oadd_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_add))
  | Osub_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_sub))
  | Omul_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_mul))
  | _ -> fail loc "overloaded_op: non supported operator"


(* [wrap_const ~const t] wrap the type [t] into a const typ if const is true *)
let wrap_const ?(const : bool = false) (t : typ) : typ = 
  if const then typ_const t else t 


  