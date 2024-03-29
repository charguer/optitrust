open Compcert_parser
open Ast
open Trm
open Typ
open Mark
open Tools

(* [loc_of_node cloc ]: transforms a C.location into Ast.location

    Note: For the moment only the line of the node is known, hence the
    start column and end column are set to 0 *)
let loc_of_cloc (cloc : C.location) : location =
  match cloc with
  | ("", -1) -> None
  | (file, line) -> Some {loc_file = file; loc_start = {pos_line = line; pos_col = 0}; loc_end = {pos_line = line; pos_col = 0}} (* LATER: Find the correct location *)

(* maps from [typ_ctx] *)
let ctx_tconstr : typconstrid Qualified_map.t ref = ref Qualified_map.empty
let ctx_typedef : typedef typmap ref = ref Typ_map.empty
let ctx_label : typconstrid labelmap ref = ref String_map.empty
let ctx_constr : typconstrid constrnamemap ref = ref String_map.empty

(* [debug_typedefs]: flag for debugging typedefs *)
let debug_typedefs = false

(* [ctx_tconstr_add tn tid]: adds constructed type [tc] with id [tid] in map [ctx_tconstr] *)
let ctx_tconstr_add (tc : typconstr) (tid : typconstrid) : unit =
  if debug_typedefs then Printf.printf "Type %s has been added into map with typconstrid %d\n" (Tools.document_to_string (Ast_to_text.print_typconstr tc)) tid;
  ctx_tconstr := Qualified_map.add tc tid (!ctx_tconstr)

(* [ctx_typedef_add tn tid td]: adds typedef [td] with id [tid] in map [ctx_typedef] *)
let ctx_typedef_add (tn : typconstr) (tid : typconstrid) (td : typedef) : unit =
  if debug_typedefs then Printf.printf "Typedef for %s has been registered\n" (Tools.document_to_string (Ast_to_text.print_typconstr tn));
  ctx_typedef := Typ_map.add tid td (!ctx_typedef)

(* [ctx_label_add lb tid]: adds label [lb] with id [tid] in map [ctx_label] *)
let ctx_label_add (lb : label) (tid : typconstrid) : unit =
  ctx_label := String_map.add lb tid (!ctx_label)

(* [ctx_constr_add c tid]: adds constr [c] with id [tid] in map [ctx_constr_add] *)
let ctx_constr_add (c : constrname) (tid : typconstrid) : unit =
  ctx_constr := String_map.add c tid (!ctx_constr)

(* [get_ctx]: get the current context *)
let get_ctx () : ctx =
  typing_ctx {
    ctx_var = Qualified_map.empty;
    ctx_tconstr = !ctx_tconstr;
    ctx_typedef = !ctx_typedef;
    ctx_label = !ctx_label;
    ctx_constr = !ctx_constr;
  }

(* CHECK: #type-id *)
let name_to_typconstr (n : string) : typconstr =
  [], n

(* [get_typid_for_type ty]: gets the type id for type [tv]*)
let get_typid_from_trm (tv : typvar) : int  =
   let tid = Qualified_map.find_opt ([], tv) !ctx_tconstr in
   begin match tid with
   | Some id -> id
   | None -> -1
   end

(* [tr_attribute att]: translates Menhir attribute [att] to OptiTrust attribute *)
let tr_attribute (att : C.attribute) : attribute =
  (* DEBUG *)
  (* let c_attribute_to_string (att : C.attribute) : string =
    match att with
    | AConst -> "const"
    | AVolatile -> "volatile"
    | ARestrict -> "restrict"
    | AAlignas _ -> "alignas"
    | Attr _ -> "attr" in *)
  match att with
  | C.AAlignas n -> Ast.Alignas (trm_int n)
  | _ ->
      (* DEBUG:Printf.printf "Warning unknown attributes passed to the parser, %s \n" (c_attribute_to_string att); *)
      Others

  (* LATER; support others *)

(* [wrap_const att ty]: wraps type [ty] into a const type if const is one of the attributes *)
let wrap_const (att : C.attributes) (ty : Ast.typ) : Ast.typ =
  let const = List.mem C.AConst att in
  let att = List.filter (fun a -> a <> C.AConst) att in
  let ty = { ty with typ_attributes = List.map tr_attribute att } in
  if const then typ_const ty else ty

(* [tr_type ty]: translates C.typ to Ast.typ *)
let rec tr_type  (ty : C.typ) : Ast.typ =
  match ty with
  | C.TPtr (ty1, att) ->
    let ty = tr_type ty1 in
    (* Temporary hack for function types, discus with Arthur why Menhir adds the pointer type automatically *)
    if is_typ_fun ty then wrap_const att ty else wrap_const att (typ_ptr Ptr_kind_mut ty)
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
    | _ -> fail None "CMenhir_to_astRawC.tr_type: ikind not supported for integers"
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
    let typ_to_add = typ_constr (name_to_typconstr n) ~tid:(get_typid_from_trm n) in
    wrap_const att (typ_to_add)
  | C.TStruct ({name = n;_}, att) ->
      let typ_to_add = typ_record Struct (typ_constr (name_to_typconstr n) ~tid:(next_typconstrid())) in
      wrap_const att (typ_to_add)
  | C.TUnion ({name = n;_}, att) ->
      fail None "CMenhir_to_astRawC.OptiTrust does not support inline use of struct or union; you must use a typedef"
  | C.TEnum ({name = n; _}, att) ->
    typ_constr (name_to_typconstr n) ~tid:(get_typid_from_trm n)
  | C.TVoid _ -> typ_unit ()

(* [tr_stmt s]: translates C.stms to Ast.stmt *)
and tr_stmt (s : C.stmt) : trm =
  let loc = loc_of_cloc s.sloc in
  let ctx = get_ctx () in
  match s.sdesc with
  | Sdo e -> tr_expr e
  | Sif (cond, then_, else_) ->
     let tc = tr_expr ~is_boolean:true cond in
     let tt = tr_stmt then_ in
     begin match else_.sdesc with
     | Sskip -> trm_if ?loc ~ctx tc tt (trm_lit Lit_unit)
     | _ ->
      let te = tr_stmt else_ in
      trm_if tc tt te
     end
  | Swhile (cond, body) ->
    let tc = tr_expr cond in
    let ts = tr_stmt body in
    trm_while ?loc ~ctx tc ts
  | Sdowhile (body, cond) ->
    let tc = tr_expr cond in
    let ts = tr_stmt body in
    trm_do_while ?loc ~ctx ts tc
  | Sfor (init, cond, step, body) ->
    let tr_stmt_opt (so : C.stmt) : trm =
      match so.sdesc with
      | Sskip -> trm_lit ?loc ~ctx Lit_unit
      | _ -> tr_stmt so
      in
    let init = tr_stmt_opt init in
    let init = begin match init.desc with
    | Trm_seq tl ->
      if Mlist.length tl = 1 then Mlist.nth tl 0 else init
    |_ -> init
    end in
    let cond = tr_expr cond in
    let step = tr_stmt_opt step in
    let body = tr_stmt body in
    trm_for_of_trm_for_c (trm_for_c ?loc ~ctx init cond step body)
  | Sbreak ->
    trm_abort ?loc ~ctx (Break None)
  | Scontinue ->
    trm_abort ?loc ~ctx (Continue None)
  | Slabeled (label, body) ->
    begin match label with
    | Slabel lb ->
      let t = tr_stmt body in
      trm_add_label lb t
    | _ -> fail loc "CMenhir_to_astRawC.tr_stmt: switch clauses are not yet supported in OptiTrust"
    end
  | Sgoto lb ->
    trm_goto ?loc ~ctx lb
  | Sreturn init_opt ->
    begin match init_opt with
    | Some re ->
       let t = tr_init re in
       trm_abort ?loc ~ctx (Ret (Some t))
    |_ -> trm_abort ?loc ~ctx (Ret None)
    end
  | Sblock sl ->
    (* LATER: put back naive code:*)
    let tl = List.map tr_stmt sl in
    (* let rec handle_pragma acc sl =
      match sl with
      | [] -> List.rev acc
      | { C.sdesc = Spragma (p, s1); C.sloc = loc } :: sl1 ->
          handle_pragma ((tr_stmt s1)::(tr_pragma ?loc:(loc_of_cloc loc) p)::acc) sl1
      | s1 :: sl1 -> handle_pragma (tr_stmt s1 :: acc) sl1
      in
    trm_seq_nomarks ?loc ~ctx (handle_pragma [] sl) *)
    trm_seq_nomarks ?loc ~ctx tl
  | Sdecl (_stor, {name = n; _}, ty, init_opt) ->
    let tt = tr_type ty in
    let te = begin match init_opt with
             | None -> trm_lit ?loc Lit_uninitialized
             (* FIXME: #odoc why is annotation required? *)
             | Some init -> (tr_init : ?loc:trm_loc -> C.init -> trm) ?loc init
             end
      in
    let mut = if is_typ_const tt then Var_immutable else Var_mutable in
    trm_let ?loc mut (name_to_var n, tt) te
  | Spragma (p, s1) ->
    (* pragmas are parsed as annotations to the proceeding instruction *)
    let tp = tr_pragma p in
    let ts1 = tr_stmt s1 in
    trm_add_pragma tp ts1
  | _ -> fail loc "CMenhir_to_astRawC.tr_stmt: statment not supported"
   (* LATER: should not use catch all pattern, here and elsewhere *)

(* [tr_pragma ?loc p]: translates C.pragma into OptiTrust pragmas *)
and tr_pragma ?(loc : location) (p : string) : cpragma =
  match p with
  | "omp_simd" -> Simd []
  | "omp atomic" -> Atomic None
  | "omp parallel for" -> Parallel_for []
  | "omp parallel" -> Parallel []
  | "omp single" -> Single []
  | _ ->
    try Scanf.sscanf p "omp parallel for collapse(%d)" (fun n -> (Parallel_for [Collapse n]))
     with Scanf.Scan_failure _ ->
      fail loc (Printf.sprintf "tr_pragma: unsupported pragma: '%s'" p)

(* and tr_pragma1 ?(loc : location) (p : string) : trm =
  match p with
  | "omp simd" -> trm_omp_directive (Simd [])
  | "omp atomic" -> trm_omp_directive (Atomic None)
  | "omp parallel for" -> trm_omp_directive (Parallel_for [])
  | "omp parallel" -> trm_omp_directive (Parallel [])
  | "omp single" -> trm_omp_directive (Single [])
  | _ ->
     try Scanf.sscanf p "omp parallel for collapse(%d)" (fun n ->
       trm_omp_directive (Parallel_for [Collapse n]))
     with Scanf.Scan_failure _ ->
      fail loc (Printf.sprintf "tr_pragma: unsupported pragma: '%s'" p) *)

(* [tr_init i]: translates C.inti into OptiTrust trm *)
and tr_init ?(loc : trm_loc option) (i : C.init) : trm =
  match i with
  | Init_single e -> tr_expr e
  | Init_array il -> trm_array ?loc (Mlist.of_list (List.map tr_init il))
  | Init_struct ((id, ty), il) ->
    let ty = tr_type ty in
    trm_record ?loc ~typ:ty (Mlist.of_list (List.map (fun (_, init) -> (None, tr_init init)) il))
  | Init_union _ -> fail loc "CMenhir_to_astRawC.tr_init: union not supported yet"

(* [tr_constant c]: translates C.constant into OptiTrust trm *)
and tr_constant ?(loc : trm_loc option) ?(typ : typ option) ?(is_boolean : bool = false) (c : C.constant) : trm =
  match c with
  | C.CInt (i, ik, s)->
    let i = Int64.to_int i in
    begin match ik with
    | C.IBool ->
      let ib = Tools.int_to_bool i in
      trm_lit ~typ ?loc (Lit_bool ib)
    | C.IChar | C.ISChar | C.IUChar->

      trm_lit ~typ ?loc (Lit_string (string_of_int i))
    | _ ->
      if is_boolean
        then
          let b =
          begin match s with
          | "0" -> false | "1" -> true
          | _ -> fail None "CMenhir_to_astRawC.tr_constant: expected a constant boolean expression"
          end in
          trm_lit ~typ ?loc (Lit_bool b)
        else
      trm_lit ~typ ?loc (Lit_int i)
    end
  | C.CFloat ({intPart = inp; fracPart = fp;_}, fk) ->
    trm_lit ~typ ?loc (Lit_double (float_of_string (inp ^ "." ^ fp)))
  | CStr s ->
    trm_lit ~typ ?loc (Lit_string s)
  | _  -> fail loc "CMenhir_to_astRawC.tr_const: constant expression is not supported"

(* [tr_expr ~is_stement e]: translates C.exp into OptiTrust trm *)
and tr_expr ?(is_boolean : bool = false) (e : C.exp) : trm =
  let loc = loc_of_cloc e.eloc in
  let typ = tr_type e.etyp in
  let ctx = get_ctx () in
  match e.edesc with
  (* FIXME: #odoc why is annotation required? *)
  | EConst c -> (tr_constant : (?loc:trm_loc -> ?typ:typ -> ?is_boolean:bool -> C.constant -> trm)) ~typ ?loc ~is_boolean c
  | ESizeof ty ->
    let ty = tr_type ty in
    trm_var ?loc (name_to_var ("sizeof(" ^ AstC_to_c.typ_to_string ty ^ ")"))
  | EAlignof ty ->
     let ty = tr_type ty in
     trm_var ?loc ~typ (name_to_var ("_Alignas(" ^ AstC_to_c.typ_to_string ty ^ ")"))
  | EVar {name = n; _} ->
    (* Booleans are parsed as const variables, here we need to convert them into literals *)
    begin match Tools.bool_of_var n with
    | Some b ->
      trm_lit ?loc (Lit_bool b)
    | None -> trm_var ?loc ~ctx ~typ (name_to_var n)
    end
  | EUnop (unop, e) ->
    let t = tr_expr e in
    let trm_apps1 unop t1 = trm_apps ?loc ~typ ~ctx (trm_unop ?loc unop) [t1] in
    begin match unop with
    | Ominus ->
      trm_apps1 Unop_minus t
    | Oplus	->
      trm_apps1 Unop_plus t
    | Olognot	->
      let t = tr_expr ~is_boolean:true e in
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
      let get_op = trm_unop ?loc (Unop_struct_get s) in
      let get_op = if is_get_operation t then trm_add_cstyle Display_no_arrow get_op else get_op in
      trm_apps ?loc ~ctx ~typ get_op [t]
    | Oarrow s ->
      trm_apps ?loc ~ctx ~typ (trm_unop (Unop_struct_get s) ) [trm_get t]
    end
  | EBinop (binop, le, re, _) ->
    let tl = tr_expr le in
    let tr = tr_expr re in
    let trm_prim_c binop tl tr =
       trm_prim_compound ?loc ~ctx binop  tl tr in
    begin match binop with
    | Oadd -> trm_add ?loc ~ctx ~typ tl tr
    | Osub -> trm_sub ?loc ~ctx ~typ  tl tr
    | Omul -> trm_mul ?loc ~ctx ~typ  tl tr
    | Odiv -> trm_div ?loc ~ctx ~typ  tl tr
    | Omod -> trm_mod ?loc ~ctx ~typ  tl tr
    | Ologand ->
      let tl = tr_expr ~is_boolean:true le in
      let tr = tr_expr ~is_boolean:true re in
      trm_and ?loc ~ctx ~typ  tl tr
    | Ologor ->
      let tl = tr_expr ~is_boolean:true le in
      let tr = tr_expr ~is_boolean:true re in
      trm_or ?loc ~ctx ~typ  tl tr
    | Oxor -> trm_xor ?loc ~ctx ~typ  tl tr
    | Oshl -> trm_shiftl ?loc ~ctx ~typ  tl tr
    | Oshr -> trm_shiftr ?loc ~ctx ~typ  tl tr
    | Oeq -> trm_eq ?loc ~ctx ~typ  tl tr
    | One -> trm_neq ?loc ~ctx ~typ  tl tr
    | Olt -> trm_lt ?loc ~ctx ~typ  tl tr
    | Ogt -> trm_gt ?loc ~ctx ~typ tl tr
    | Ole -> trm_le ?loc ~ctx ~typ  tl tr
    | Oge -> trm_ge ?loc ~ctx ~typ  tl tr
    | Oindex -> trm_apps ?loc ~ctx ~typ (trm_binop ?loc ~ctx (Binop_array_get) ) [tl; tr]
    | Oassign -> trm_set ?loc ~ctx tl tr
    | Oadd_assign -> trm_prim_c Binop_add tl tr
    | Osub_assign -> trm_prim_c Binop_sub tl tr
    | Omul_assign -> trm_prim_c Binop_mul tl tr
    | Odiv_assign -> trm_prim_c Binop_div tl tr
    | Omod_assign -> trm_prim_c Binop_mod tl tr
    | Oand_assign -> trm_prim_c Binop_bitwise_and tl tr
    | Oor_assign -> trm_prim_c Binop_bitwise_or tl tr
    | Oxor_assign -> trm_prim_c Binop_xor tl tr
    | Oshl_assign -> trm_prim_c Binop_shiftl tl tr
    | Oshr_assign -> trm_prim_c Binop_shiftr tl tr
    | Ocomma -> fail loc "CMenhir_to_astRawC.tr_expr: OptiTrust does not support the comma operator"
    | Oand -> trm_bit_and ?loc ~ctx ~typ tl tr
    | Oor -> trm_bit_or ?loc ~ctx ~typ tl tr
    end
  | EConditional (cond, then_, else_) ->
    let t_cond = tr_expr cond in
    let t_then = tr_expr then_ in
    let t_else = tr_expr else_ in
    trm_apps ?loc ~typ ~ctx (trm_prim ?loc ~ctx Prim_conditional_op) [t_cond; t_then; t_else]
  | ECast (ty, e1) ->
    let ty = tr_type ty in
    let te = tr_expr e1 in
    if is_null_pointer ty te (* Menhir encodes NULL as [( void* ) 0], we decode it here *)
      then trm_null ?loc ~ctx ()
      else  trm_apps ?loc ~ctx ~typ (trm_unop ?loc ~ctx (Unop_cast ty)) [te]
  (* FIXME: #odoc why is annotation required? *)
  | ECompound (_, init) -> (tr_init : ?loc:trm_loc -> C.init -> trm) init ?loc
  | ECall (f, el) ->
    let tf = tr_expr f in
    begin match tf.desc with
    | Trm_var (_, x) when Str.string_match (Str.regexp "overloaded=") x.name 0 ->
      begin match el with
      | [tl; tr] -> trm_set ?loc ~ctx (tr_expr tl) (tr_expr tr)
      | _ -> fail loc "CMenhir_to_astRawC.tr_expr: overloaded= expects two arguments"
      end
    | _ -> trm_apps ?loc ~ctx ~typ tf (List.map tr_expr el)
    end

(* [tr_globdef d]: transaltes C.globdecl into OptiTrust trm *)
and tr_globdef (d : C.globdecl) : trm =
  let loc = loc_of_cloc d.gloc in
  let ctx = get_ctx () in
  match d.gdesc with
  | C.Gdecl (_stor, {name = n; _}, ty, init_opt) ->
    let tt = tr_type ty in
    let te = begin match init_opt with
             | None -> trm_lit Lit_uninitialized
             | Some init -> tr_init init
             end
      in
    (* A temporary hack for function prototypes *)
    if is_typ_fun tt
      then begin match ty with
      | C.TFun (ty1, params, _, att) ->
        let tt = tr_type ty1 in
        begin match params with
        | None -> trm_let_fun (name_to_var n) tt [] (trm_lit (Lit_uninitialized))
        | Some pl ->
          let get_args (tv : C.ident * C.typ) : (var * typ) =
            let (id, ty) = tv in
            let ty = tr_type ty in
            (name_to_var id.name, ty)
          in
          let args = List.map get_args pl in
          trm_let_fun ?loc ~ctx (name_to_var n) tt args (trm_lit (Lit_uninitialized))
        end
      | _ -> fail None "CMenhir_to_astRawC.tr_globdef: this function prototype is not supported"
      end
      else
        let mut = if is_typ_const tt then Var_immutable else Var_mutable in
        trm_let ?loc ~ctx mut (name_to_var n, tt) te
  | C.Gfundef {fd_storage = _; fd_inline = inline; fd_name = {name = n;_}; fd_attrib = _att; fd_ret = ty; fd_params = po; fd_body = bo; _} ->
    let tt = tr_type ty in
    let tb = tr_stmt bo in
    let res =
      begin match po with
      | [] ->
        trm_let_fun (name_to_var n) tt [] tb
      | _ ->
        let get_args (tv : C.ident * C.typ) : (var * typ) =
          let (id, ty) = tv in
          let ty = tr_type ty in
          (name_to_var id.name, ty)
         in
        let args = List.map get_args po in
        trm_let_fun ?loc ~ctx (name_to_var n) tt args tb
      end in
    if inline then trm_add_cstyle Fun_inline res else res
  | C.Genumdef ({C.name = tn}, att, enum_list) ->
    let el = List.map (fun ({C.name = constant_name; }, _, exp_opt) ->
      match exp_opt with
      | None -> (name_to_var constant_name, None)
      | Some e ->
        let t_init = tr_expr e in
        (name_to_var constant_name, Some t_init)
    ) enum_list in
    let tc = name_to_typconstr tn in
    let tid = next_typconstrid () in
    ctx_tconstr_add tc tid;
    let td = {
      typdef_loc = None;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_enum el
    } in
    ctx_typedef_add tc tid td;
    trm_typedef ?loc ~ctx td
  | C.Gtypedef ({C.name = tn}, ty) ->
    let tc = name_to_typconstr tn in
    let tid = next_typconstrid () in
    ctx_tconstr_add tc tid;
    let ty = tr_type ty in
    let td = {
      typdef_loc = loc;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_alias ty
    }
    in
    ctx_typedef_add tc tid td;
    trm_typedef ?loc ~ctx td;
  | _ -> fail loc "CMenhir_to_astRawC.tr_globdef: declaration not supported"

(* [tr_typedef] translates a typedef (struct only for the moment) *)
let tr_typedef struct_is_named loc (sn : string) fl ty =
  let tc = name_to_typconstr sn in
  let tid = next_typconstrid () in
  ctx_tconstr_add tc tid;
  let prod_list = List.map (fun {C.fld_name = fr; fld_typ = ft; _} -> (Record_field_member (fr, tr_type ft), Access_unspecified)) fl in
  let td = {
    typdef_loc = loc;
    typdef_typid = tid;
    typdef_tconstr = sn;
    typdef_vars = [];
    typdef_body = Typdef_record prod_list
  } in
  let ctx = get_ctx () in
  ctx_typedef_add tc tid td;
  trm_typedef ?loc ~ctx td


(* [tr_globdefs gs]: translates a list of C.globdefs into a list of OptiTrust declarations *)
let tr_globdefs (gs : C.globdecl list) : trms =
  let rec aux acc gs =
    match gs with
    | [] -> acc
    |    {C.gdesc = C.Gcompositedef (su1, {C.name = sn1},_, fl ); C.gloc = loc }
      :: {C.gdesc = C.Gtypedef ({C.name = sn2},ty)} :: gs' ->
        let loc = loc_of_cloc loc in
        if su1 <> Struct
          then fail loc "CMenhir_to_astRawC.tr_globdefs: only struct records are supported";
        if sn1 <> sn2
          then fail loc (Printf.sprintf "CMenhir_to_astRawC.tr_globdefs: the struct name (%s) must match the typdef name (%s).\n" sn1 sn2);
        let td = tr_typedef false loc sn1 fl ty in
        aux (td::acc) gs'
    |    {C.gdesc = C.Gcompositedecl (su, {C.name = sn;_}, _)}
      :: {C.gdesc = C.Gcompositedef (su1, {C.name = sn1},_, fl ); C.gloc = loc }
      :: {C.gdesc = C.Gtypedef ({C.name = sn2},ty)} :: gs' ->
        let loc = loc_of_cloc loc in
        if su <> Struct
          then fail loc "CMenhir_to_astRawC.tr_globdefs: only struct records are supported";
        if su <> su1
          then fail loc (Printf.sprintf "CMenhir_to_astRawC.tr_globdefs: the declaration (%s) and the definition (%s) must be consistent.\n" sn sn2);
        if sn <> sn1 || sn <> sn2
          then fail loc (Printf.sprintf "CMenhir_to_astRawC.tr_globdefs: the struct name (%s) must match the typdef name (%s).\n" sn sn2);
        let td = tr_typedef true loc sn1 fl ty in
        aux (td::acc) gs'

    | ({C.gdesc = C.Gcompositedecl _; _} | {C.gdesc = C.Gcompositedef _; _}) :: _ ->
        fail None "CMenhir_to_astRawC.tr_globdefs: struct and unions are not supported"
    | g :: gs' -> aux (tr_globdef g :: acc) gs'
  in
    List.rev (aux [] gs)

(* [tr_ast tl]: translates a C.program into OptiTrust AST *)
let tr_ast (tl : C.program) : trm =
  let tl = tr_globdefs tl in
  trm_set_mainfile (trm_seq_nomarks tl)