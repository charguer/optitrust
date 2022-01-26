open Ast
open C
open Tools

(* map with keys variables and values their type 
  used for loops that do not declare their counter
*)

let ctx_var : Ast.typ varmap ref = ref String_map.empty

let ctx_tconstr : typconstrid varmap ref = ref String_map.empty

let ctx_typedef : typedef typmap ref = ref Typ_map.empty

let ctx_label : typconstrid varmap ref = ref String_map.empty

let ctx_constr : typconstrid varmap ref = ref String_map.empty

let debug_typedefs = false

let ctx_var_add (tv : typvar) (t : Ast.typ) : unit =
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
let string_of_overloaded ?(loc : Ast.location = None) (op : C.binary_operator) : string = 
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
let overloade_op ?(loc : Ast.location = None) ?(ctx : ctx option = None) (op : C.binary_operator) : trm = 
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
let wrap_const (att : C.attributes)(ty : Ast.typ) : Ast.typ = 
  let const = List.mem AConst att in 
  if const then typ_const ty else ty 


  
(* TODO: Add location later when it is fixed in C.ml *)
let rec tr_type  (ty : C.typ) : Ast.typ = 
  match ty with 
  | TPtr (ty1, att) -> 
    let ty = tr_type ty1 in 
    wrap_const att (typ_ptr Ptr_kind_mut ty)
  | TRef (ty1, att) -> 
    let ty = tr_type ty1 in 
    wrap_const att (typ_ptr Ptr_kind_ref ty)
  | TArray (ty1, sz, att) ->
    let ty = tr_type ty1 in 
    begin match sz with 
    | None -> wrap_const att (typ_array ty Undefined)
    | Some n -> wrap_const att (typ_array ty (Const (Int64.to_int n)))
    end
  | TInt (ik, att) -> 
    begin match ik with 
    | IInt -> wrap_const att (typ_int ())
    | IUInt -> wrap_const att (typ_int ~annot:[Unsigned] ())
    | ILong -> wrap_const att (typ_int ~annot:[Long] ())
    | IULong -> wrap_const att (typ_int ~annot:[Unsigned; Long] ())
    | ILongLong -> wrap_const att (typ_int ~annot:[Long; Long] ())
    | IULongLong -> wrap_const att (typ_int ~annot:[Unsigned; Long; Long] ())
    | _ -> fail None "tr_type: ikind not supported for integers" 
    end 
  | TFloat (fk, att) -> 
    begin match fk with 
    | FFloat -> wrap_const att (typ_float ())
    | FDouble -> wrap_const att (typ_double ())
    | FLongDouble -> wrap_const att (typ_double ~annot:[Long] ())
    end
  | TFun (ty1, params, _, att) -> 
    let ty = tr_type ty1 in 
    begin match params with 
    | None -> typ_fun [] ty
    | Some pl -> 
      let tl = List.map (fun (_, ty1) -> tr_type ty1 ) pl in 
      typ_fun tl ty
    end
  | TNamed ({name = n;_}, att) -> 
    let typ_to_add = typ_constr n ~tid:(get_typid_from_trm n) in 
    wrap_const att (typ_to_add)
  | TStruct (idn, att) | TUnion (idn, att) -> fail None "discuss with Arthur"
  | TEnum ({name = n; _}, att) -> 
    typ_constr n ~tid:(get_typid_from_trm n)
  | TVoid _ -> typ_unit ()
  

and tr_stmt (s : stmt) : trm = 
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
  (* | Sfor (init, cond, step, body) -> 
    let tr_stmt_opt (so : stmt) : trm = 
      match so.sdesc with 
      | Sskip -> trm_lit Lit_unit
      | _ -> tr_stmt so
      in
    let init = tr_stmt_opt init in 
    let cond = match cond.edesc with 
    | Sskip -> trm_lit (Lit_bool true)
    | _ -> tr_expr cond
      in 
    let step = tr_stmt_opt step in 
    let body = tr_stmt body in 
    trm_for_of_trm_for_c (trm_for_c init cond step body) *)
  | Sbreak -> 
    trm_abort (Break None)
  | Scontinue -> 
    trm_abort (Continue None)
  | Slabeled (label, body) -> 
    begin match label with 
    | Slabel lb ->
      let t = tr_stmt body in 
      trm_labelled lb t
    | _ -> fail None "tr_stmt: Ast Arthur"
    end
  | Sgoto lb -> 
    trm_goto lb
  | Sreturn init_opt -> 
    begin match init_opt with 
    | Some re ->
     begin match re with 
     | Init_single e -> 
       let t = tr_expr e in 
       trm_abort (Ret (Some t))
     | _ -> fail None "tr_stmt: ast Arthur"
     end
    |_ -> trm_abort (Ret None)
    end
  | Sblock sl -> 
    let tl = List.map tr_stmt sl in 
    trm_seq_nomarks tl
  | _ -> fail None "tr_stmt: statment not supported" 



and tr_expr ?(is_statement : bool = false) (e : exp) : trm = 
  match e.edesc with 
  | EConst c -> fail None "tr_expr: Ask Arthur"
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
      trm_apps1 Unop_opp t
    | Oplus	-> 
      trm_apps1 Unop_opp t (* Arthur: What should I do here *)
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
    | Ocomma -> fail None "tr_expr: Ast Arthur"
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
  | ECompound _ -> fail None "tr_expr:Ask Arthur"
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
  



