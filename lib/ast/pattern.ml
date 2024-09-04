open Ast
open Trm
open Typ

(** Raise this exception to check the next pattern of a pattern matching.
    It can be used both inside a pattern combinator and a pattern continuation. *)
exception Next

(** Exception raised when none of the patterns are matched. *)
exception Failed

let rec pattern_match (v: 'a) (ks: ('a -> unit -> 'b) list): 'b =
  match ks with
  | k :: ks ->
    begin try
      k v ()
    with Next -> pattern_match v ks
    end
  | [] -> raise Failed

let pattern_match_opt (v: 'a) (ks: ('a -> unit -> 'b) list): 'b option =
  try
    Some (pattern_match v ks)
  with Failed -> None

let when_ (cond : bool) : unit = if not cond then raise Next

let (!) (inside: 'a -> 't -> 'b) (k:'t -> 'a) (v: 't): 'b = inside (k v) v
let __ (k: 'a) (v: 't): 'a = k
let (^|) (p1: 'a -> 't -> 'b) (p2: 'a -> 't -> 'b) (k: 'a) (v: 't) =
  try p1 k v with Next -> p2 k v

let check (f: 't -> bool) (k: 'a) (v: 't): 'a =
  if f v then k else raise Next
let eq (x: 't) : 'a -> 't -> 'a = check ((=) x)

let trm_let (var: 'a -> var -> 'b) (typ: 'b -> typ -> 'c) (body: 'c -> trm -> 'd) (k: 'a) (t: trm): 'd =
  match trm_let_inv t with
  | Some (tvar, ttyp, tbody) ->
    let k = var k tvar in
    let k = typ k ttyp in
    let k = body k tbody in
    k
  | None -> raise Next

let trm_seq (fn: 'a -> trm mlist -> 'b) (k: 'a) (t: trm): 'b =
  match trm_seq_inv t with
  | Some seq -> fn k seq
  | None -> raise Next

let trm_apps (fn: 'a -> trm -> 'b) (args: 'b -> trm list -> 'c) (ghost_args: 'c -> resource_item list -> 'd) (k: 'a) (t: trm): 'd =
  match t.desc with
  | Trm_apps (f, a, ga) ->
    let k = fn k f in
    let k = args k a in
    let k = ghost_args k ga in
    k
  | _ -> raise Next

let trm_var (var: 'a -> var -> 'b) (k: 'a) (t: trm): 'b =
  match trm_var_inv t with
  | Some v -> var k v
  | None -> raise Next

let var_eq (v: var): 'a -> var -> 'a = check (var_eq v)

(* Probably useless? *)
let trm_apps_specific_var (v: var) args = trm_apps (trm_var (var_eq v)) args __

let nil (k: 'a) (l: _ list) : 'a =
  match l with
  | [] -> k
  | _ -> raise Next

let (^::) (fh: 'a -> 't -> 'b) (ft: 'b -> 't list -> 'c) (k: 'a) (l: 't list): 'c =
  match l with
  | h :: t ->
    let k = fh k h in
    let k = ft k t in
    k
  | _ -> raise Next

let trm_apps0 fn = trm_apps fn nil __
let trm_apps1 fn arg1 = trm_apps fn (arg1 ^:: nil) __
let trm_apps2 fn arg1 arg2 = trm_apps fn (arg1 ^:: arg2 ^:: nil) __
let trm_apps3 fn arg1 arg2 arg3 = trm_apps fn (arg1 ^:: arg2 ^:: arg3 ^:: nil) __

let trm_int (f: 'a -> int -> 'b) (k: 'a) (t: trm): 'b =
  match trm_int_inv t with
  | Some x -> f k x
  | _ -> raise Next

let trm_fun args rettyp body spec k t =
  match t.desc with
  | Trm_fun (targs, tret_type, tbody, tspec) ->
    let k = args k targs in
    let k  = rettyp k tret_type in
    let k = body k tbody in
    let k = spec k tspec in
    k
  | _ -> raise Next

let trm_fun_with_contract args body contract k t =
  match t.desc with
  | Trm_fun (targs, tret_type, tbody, FunSpecContract tcontract) ->
    let k = args k targs in
    let k = body k tbody in
    let k = contract k tcontract in
    k
  | _ -> raise Next

let trm_let_fun name ret args body spec k t =
  match t.desc with
  | Trm_let_fun (tname, tret, targs, tbody, tspec) ->
    let k = name k tname in
    let k = ret k tret in
    let k = args k targs in
    let k = body k tbody in
    let k = spec k tspec in
    k
  | _ -> raise Next

let trm_for range body spec k t =
  match t.desc with
  | Trm_for (trange, tbody, tspec) ->
    let k = range k trange in
    let k = body k tbody in
    let k = spec k tspec in
    k
  | _ -> raise Next

let trm_string f k t =
  match trm_lit_inv t with
  | Some (Lit_string str) -> f k str
  | _ -> raise Next

let trm_unop unop ft k t =
  match trm_unop_inv unop t with
  | Some t0 -> ft k t0
  | None -> raise Next

let trm_binop binop ft1 ft2 k t =
  match trm_binop_inv binop t with
  | Some (t1, t2) ->
    let k = ft1 k t1 in
    let k = ft2 k t2 in
    k
  | None -> raise Next

let trm_compound_assign binop ft1 ft2 k t =
  match trm_compound_assign_inv binop t with
  | Some (t1, t2) ->
    let k = ft1 k t1 in
    let k = ft2 k t2 in
    k
  | None -> raise Next

let trm_set ft1 ft2 = trm_binop Binop_set ft1 ft2

let trm_add ft1 ft2 = trm_binop Binop_add ft1 ft2
let trm_sub ft1 ft2 = trm_binop Binop_sub ft1 ft2
let trm_mul ft1 ft2 = trm_binop Binop_mul ft1 ft2
let trm_div ft1 ft2 = trm_binop Binop_div ft1 ft2

let trm_lt ft1 ft2 = trm_binop Binop_lt ft1 ft2
let trm_le ft1 ft2 = trm_binop Binop_le ft1 ft2
let trm_gt ft1 ft2 = trm_binop Binop_gt ft1 ft2
let trm_ge ft1 ft2 = trm_binop Binop_ge ft1 ft2
let trm_eq ft1 ft2 = trm_binop Binop_eq ft1 ft2
let trm_neq ft1 ft2 = trm_binop Binop_neq ft1 ft2

let trm_get ft k t =
  match trm_get_inv t with
  | Some t' -> ft k t'
  | None -> raise Next

let trm_struct_get ft ffield k t =
  match trm_struct_get_inv t with
  | Some (t, field) ->
    let k = ft k t in
    let k = ffield k field in
    k
  | None -> raise Next

let trm_struct_access ft ffield k t =
  match trm_struct_access_inv t with
  | Some (t, field) ->
    let k = ft k t in
    let k = ffield k field in
    k
  | None -> raise Next

let trm_array_get fbase findex k t =
  match trm_array_get_inv t with
  | Some (base, index) ->
    let k = fbase k base in
    let k = findex k index in
    k
  | None -> raise Next

let trm_array_access fbase findex k t =
  match trm_array_access_inv t with
  | Some (base, index) ->
    let k = fbase k base in
    let k = findex k index in
    k
  | None -> raise Next

let trm_arbitrary fa k t =
  match t.desc with
  | Trm_arbitrary a -> fa k a
  | _ -> raise Next

let trm_struct_access fbase ffield k t =
  match trm_struct_access_inv t with
  | Some (ttrm, tfield) ->
    let k = fbase k ttrm in
    let k = ffield k tfield in
    k
  | None -> raise Next

let trm_struct_get fbase ffield k t =
  match trm_struct_get_inv t with
  | Some (ttrm, tfield) ->
    let k = fbase k ttrm in
    let k = ffield k tfield in
    k
  | None -> raise Next

let typ_var = trm_var
let typ_apps ft fargs k ty =
  match typ_apps_inv ty with
  | Some (var, args) ->
    let k = ft k var in
    let k = fargs k args in
    k
  | None -> raise Next

let typ_unit k = typ_var (var_eq typ_unit_var) k
let typ_auto k = typ_var (var_eq typ_auto_var) k
let typ_int k = typ_var (var_eq typ_int_var) k
let typ_uint k = typ_var (var_eq typ_uint_var) k
let typ_usize k = typ_var (var_eq typ_usize_var) k
let typ_isize k = typ_var (var_eq typ_isize_var) k
let typ_f32 k = typ_var (var_eq typ_f32_var) k
let typ_f64 k = typ_var (var_eq typ_f64_var) k
let typ_bool k = typ_var (var_eq typ_bool_var) k
let typ_char k = typ_var (var_eq typ_char_var) k
let typ_i8 k = typ_var (var_eq typ_i8_var) k
let typ_u8 k = typ_var (var_eq typ_u8_var) k
let typ_i16 k = typ_var (var_eq typ_i16_var) k
let typ_u16 k = typ_var (var_eq typ_u16_var) k
let typ_i32 k = typ_var (var_eq typ_i32_var) k
let typ_u32 k = typ_var (var_eq typ_u32_var) k
let typ_i64 k = typ_var (var_eq typ_i64_var) k
let typ_u64 k = typ_var (var_eq typ_u64_var) k

let typ_const fty k ty =
  match typ_const_inv ty with
  | Some ty -> fty k ty
  | None -> raise Next

let typ_ptr fty k ty =
  match typ_ptr_inv ty with
  | Some ty -> fty k ty
  | None -> raise Next

let typ_ref fty k ty =
  match typ_ref_inv ty with
  | Some ty -> fty k ty
  | None -> raise Next

let typ_array fty fsz k ty =
  match typ_array_inv ty with
  | Some (ty, sz) ->
    let k = fty k ty in
    let k = fsz k sz in
    k
  | None -> raise Next

let typ_fun fargs fres k ty =
  match typ_fun_inv ty with
  | Some (args, res) ->
    let k = fargs k args in
    let k = fres k res in
    k
  | None -> raise Next

let typ_builtin fbuiltin k ty =
  match typ_builtin_inv ty with
  | Some bt -> fbuiltin k bt
  | None -> raise Next

(* FIXME: This construction is slightly weird because it confuses types and type constructors. It is here for transitionning old code that also makes this confusion. *)
let typ_constr ftv k ty =
  (typ_var !__ ^| typ_apps !__ __) (fun var -> ftv k var) ty

let trm_with_typ fty k t =
  match t.typ with
  | Some ty -> fty k ty
  | None -> raise Next

let mlist f k t = f k (Mlist.to_list t)

let pair f1 f2 k (x1, x2) =
  let k = f1 k x1 in
  let k = f2 k x2 in
  k

let some f k xo =
  match xo with
  | Some x -> f k x
  | None -> raise Next

let none k xo =
  match xo with
  | None -> k
  | Some _ -> raise Next

let strict_loop_contract k lc =
  if lc.strict then k else raise Next
