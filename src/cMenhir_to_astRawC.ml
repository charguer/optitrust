open! Ast
open Tools



let ctx_tconstr : typconstrid varmap ref = ref String_map.empty

let ctx_typedef : typedef typmap ref = ref Typ_map.empty

let ctx_label : typconstrid varmap ref = ref String_map.empty

let ctx_constr : typconstrid varmap ref = ref String_map.empty

let debug_typedefs = false

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
  { ctx_var = String_map.empty; 
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
let string_of_overloaded ?(loc : Ast.location = None) (op : C.binary_operator) : string =
  match op with
  | C.Oadd -> "+"
  | C.Osub -> "-"
  | C.Omul -> "*"
  | C.Oassign -> "="
  | C.Oadd_assign -> "+="
  | C.Osub_assign -> "-="
  | C.Omul_assign -> "*="
  | _ -> fail loc "string_of_overloaded_op: non supported operator"


(* [overloaded op ~loc ~ctx op] *)
let overloaded_op ?(loc : Ast.location = None) ?(ctx : ctx option = None) (op : C.binary_operator) : trm =
  match op with
  | C.Oadd -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_add))
  | C.Osub -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_sub))
  | C.Omul -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_mul))
  | C.Oassign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_set))
  | C.Oadd_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_add))
  | C.Osub_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_sub))
  | C.Omul_assign -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_mul))
  | _ -> fail loc "overloaded_op: non supported operator"


(* [wrap_const ~const t] wrap the type [t] into a const typ if const is true *)
let wrap_const (att : C.attributes)(ty : Ast.typ) : Ast.typ =
  let const = List.mem C.AConst att in
  if const then typ_const ty else ty


let rec tr_type  (ty : C.typ) : Ast.typ =
  match ty with
  | C.TPtr (ty1, att) ->
    let ty = tr_type ty1 in
    wrap_const att (typ_ptr Ptr_kind_mut ty)
  | C.TRef ty1->
    let ty = tr_type ty1 in
    typ_ptr Ptr_kind_ref ty
  (* Only const arrays are supported for the moment *)
  | C.TArray (ty1, sz, att) ->
    let ty = tr_type ty1 in
    begin match sz with
    | None -> wrap_const att (typ_array ty Undefined)
    | Some (_, e) -> wrap_const att (typ_array ty (Trm (tr_expr e)))
    end
  | C.TInt (ik, att) ->
    begin match ik with
    | C.IBool ->  wrap_const att (typ_bool ())
    | C.IChar | C.ISChar | IUChar -> wrap_const att (typ_char ())
    | C.IInt -> wrap_const att (typ_int ())
    | C.IUInt -> wrap_const att (typ_int ~annot:[Unsigned] ())
    | C.ILong -> wrap_const att (typ_int ~annot:[Long] ())
    | C.IULong -> wrap_const att (typ_int ~annot:[Unsigned; Long] ())
    | C.ILongLong -> wrap_const att (typ_int ~annot:[Long; Long] ())
    | C.IULongLong -> wrap_const att (typ_int ~annot:[Unsigned; Long; Long] ())
    | _ -> fail None "tr_type: ikind not supported for integers"
    end
  | C.TFloat (fk, att) ->
    begin match fk with
    | FFloat -> wrap_const att (typ_float ())
    | FDouble -> wrap_const att (typ_double ())
    | FLongDouble -> wrap_const att (typ_double ~annot:[Long] ())
    end
  | C.TFun (ty1, params, _, att) ->
    let ty = tr_type ty1 in
    begin match params with
    | None -> typ_fun [] ty
    | Some pl ->
      let tl = List.map (fun (_, ty1) -> tr_type ty1 ) pl in
      typ_fun tl ty
    end
  | C.TNamed ({name = n;_}, att) ->
    let typ_to_add = typ_constr n ~tid:(get_typid_from_trm n) in
    wrap_const att (typ_to_add)
  | C.TStruct (_idn, _att) | C.TUnion (_idn, _att) ->
      fail None "OptiTrust does not support inline use of struct or union; you must use a typedef"
  | C.TEnum ({name = n; _}, att) ->
    typ_constr n ~tid:(get_typid_from_trm n)
  | C.TVoid _ -> typ_unit ()

and tr_stmt (s : C.stmt) : trm =
  (* TODO: loc like for expr *)
  match s.sdesc with
  | Sif (cond, then_, else_) ->
     let tc = tr_expr cond in
     let tt = tr_stmt then_ in
     begin match else_.sdesc with
     | Sskip -> trm_if tc tt (trm_lit Lit_unit)
     | _ ->
      let te = tr_stmt else_ in
      trm_if tc tt te
     end
  | Swhile (cond, body) ->
    let tc = tr_expr cond in
    let ts = tr_stmt body in
    trm_while tc ts
  | Sdowhile (body, cond) ->
    let tc = tr_expr cond in
    let ts = tr_stmt body in
    trm_do_while ts tc
  | Sfor (init, cond, step, body) ->
    let tr_stmt_opt (so : C.stmt) : trm =
      match so.sdesc with
      | Sskip -> trm_lit Lit_unit
      | _ -> tr_stmt so
      in
    let init = tr_stmt_opt init in
    let cond = tr_expr cond in 
    let step = tr_stmt_opt step in
    let body = tr_stmt body in
    trm_for_of_trm_for_c (trm_for_c init cond step body)
  | Sbreak ->
    trm_abort (Break None)
  | Scontinue ->
    trm_abort (Continue None)
  | Slabeled (label, body) ->
    begin match label with
    | Slabel lb ->
      let t = tr_stmt body in
      trm_labelled lb t
    | _ -> fail None "tr_stmt: switch clauses are not yet supported in OptiTrust"
    end
  | Sgoto lb ->
    trm_goto lb
  | Sreturn init_opt ->
    begin match init_opt with
    | Some re -> 
       let t = tr_init re in 
       trm_abort (Ret (Some t))
    |_ -> trm_abort (Ret None)
    end
  | Sblock sl ->
    let tl = List.map tr_stmt sl in
    trm_seq_nomarks tl
  | Sdecl (_stor, {name = n; _}, ty, init_opt) ->
    let tt = tr_type ty in 
    let te = begin match init_opt with 
             | None -> trm_lit Lit_uninitialized
             | Some init -> tr_init init
             end
      in
    let mut = if is_typ_const tt then Var_immutable else Var_mutable in 
    trm_let ~is_statement:true mut (n, tt) te

  | _ -> fail None "tr_stmt: statment not supported"


and tr_init (i : C.init) : trm = 
  match i with 
  | Init_single e -> tr_expr e 
  | Init_array il -> trm_array (Mlist.of_list (List.map tr_init il))
  | Init_struct (_, il) -> trm_struct (Mlist.of_list (List.map (fun (_, init) -> tr_init init) il))
  | Init_union _ -> fail None "tr_init: union not supported yet"


and tr_constant (c : C.constant) : trm = 
  match c with 
  | C.CInt (i, ik, _)->
    let i = Int64.to_int i in
    begin match ik with 
    | C.IBool -> 
      let ib = Tools.int_to_bool i in
      trm_lit (Lit_bool ib)
    | C.IChar | C.ISChar | C.IUChar->
      trm_lit (Lit_string (string_of_int i))
    | _ -> 
      trm_lit (Lit_int i)
    end
  | C.CFloat ({intPart = inp; fracPart = fp;_}, fk) ->
    trm_lit (Lit_double (float_of_string (inp ^ "." ^ fp)))
  | CStr s -> 
    trm_lit (Lit_string s)
  | _  -> fail None "tr_const: constant expression is not supported"

and tr_expr ?(is_statement : bool = false) (e : C.exp) : trm =
  (* TODO: let loc = tr_loc e.C.eloc
    in tr_loc   => end_column : use start_column+1 *)
  match e.edesc with
  | EConst c -> fail None "tr_expr: " (* TODO :tr_constant *)
  | ESizeof ty ->
    let ty = tr_type ty in
    trm_var ("sizeof(" ^ Ast_to_rawC.typ_to_string ty ^ ")")
  | EAlignof ty ->
     let ty = tr_type ty in
     trm_var ("_Alignas(" ^ Ast_to_rawC.typ_to_string ty ^ ")")
  | EVar {name = n; _} -> trm_var n
  | EUnop (unop, e) ->
    let t = tr_expr e in
    let trm_apps1 unop t1 = trm_apps (trm_unop unop) [t1] in
    begin match unop with
    | Ominus ->
      trm_apps1 Unop_minus t
    | Oplus	->
      trm_apps1 Unop_plus t 
    | Olognot	->
      trm_apps1 Unop_neg t
    | Onot ->
      trm_apps1 Unop_bitwise_neg t
    | Oderef ->
      trm_apps1 Unop_get t
    | Oaddrof ->
      trm_apps1 Unop_address t
    | Opreincr ->
      trm_apps1 Unop_pre_inc t
    | Opredecr ->
      trm_apps1 Unop_pre_dec t
    | Opostincr ->
      trm_apps1 Unop_post_inc t
    | Opostdecr ->
      trm_apps1 Unop_post_dec t
    | Odot s ->
      trm_apps1 (Unop_struct_get s)  t
    | Oarrow s ->
      trm_apps ~annot:[Display_arrow] (trm_unop (Unop_struct_get s)) [t]
    end
  | EBinop (binop, le, re, _) ->
    let tl = tr_expr le in
    let tr = tr_expr re in
    let trm_prim_c binop tl tr = trm_prim_compound binop tl tr in
    begin match binop with
    | Oadd -> trm_add tl tr
    | Osub -> trm_sub tl tr
    | Omul -> trm_mul tl tr
    | Odiv -> trm_div tl tr
    | Omod -> trm_mod tl tr
    | Oand -> trm_and tl tr
    | Oor -> trm_or tl tr
    | Oxor -> trm_xor tl tr
    | Oshl -> trm_shiftl tl tr
    | Oshr -> trm_shiftr tl tr
    | Oeq -> trm_eq tl tr
    | One -> trm_neq tl tr
    | Olt -> trm_lt tl tr
    | Ogt -> trm_gt tl tr
    | Ole -> trm_le tl tr
    | Oge -> trm_ge tl tr
    | Oindex -> trm_apps (trm_binop (Binop_array_get) ) [tl; tr]
    | Oassign -> trm_set tl tr
    | Oadd_assign -> trm_prim_c Binop_add tl tr
    | Osub_assign -> trm_prim_c Binop_sub tl tr
    | Omul_assign -> trm_prim_c Binop_mul tl tr
    | Odiv_assign -> trm_prim_c Binop_div tl tr
    | Omod_assign -> trm_prim_c Binop_mod tl tr
    | Oand_assign -> trm_prim_c Binop_and tl tr
    | Oor_assign -> trm_prim_c Binop_or tl tr
    | Oxor_assign -> trm_prim_c Binop_xor tl tr
    | Oshl_assign -> trm_prim_c Binop_shiftl tl tr
    | Oshr_assign -> trm_prim_c Binop_shiftr tl tr
    | Ocomma -> fail None "tr_expr: OptiTrust does not support the comma operator"
    | Ologand -> trm_bit_and tl tr
    | Ologor -> trm_bit_or tl tr
    end
  | EConditional (cond, then_, else_) ->
    let t_cond = tr_expr cond in
    let t_then = tr_expr then_ in
    let t_else = tr_expr else_ in
    trm_apps (trm_prim Prim_conditional_op) [t_cond; t_then; t_else]
  | ECast (ty, e) ->
    let ty = tr_type ty in
    let te = tr_expr e in
    trm_apps (trm_unop (Unop_cast ty)) [te]
  | ECompound _ -> fail None "tr_expr: Not supported for the moment" 
  | ECall (f, el) ->
    let tf = tr_expr f in
    begin match tf.desc with
    | Trm_var (_, x) when Str.string_match (Str.regexp "overloaded=") x 0 ->
      begin match el with
      | [tl; tr] -> trm_set (tr_expr tl) (tr_expr tr)
      | _ -> fail None "tr_expr: overloaded= expects two arguments"
      end
    | _ -> trm_apps tf (List.map tr_expr el)
    end




