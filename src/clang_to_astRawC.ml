open Clang.Ast
open Clang.Bindings
open Ast
open Tools

(* [loc_of_node n] get location of node [n]*)
let loc_of_node (n : 'a node) : location =
  let start_location_of_node = Clang.get_range_start (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n))in
  let end_location_of_node = Clang.get_range_end (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n )) in
  let (filename, start_row,start_column) = Clang.get_presumed_location start_location_of_node in
  let (_, end_row,end_column) = Clang.get_presumed_location end_location_of_node in
  Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}}

(* file which contains the node *)
let file_of_node (n : 'a node) : string =
  match loc_of_node n with
  | Some {loc_file = filename; _} -> filename
  | _ -> fail None "file_of_node: bad location"

type scope_kind =
  | For_scope
  | While_scope
  | Do_scope
  | Switch_scope
  | Other_scope

(*
  map from variables to their type
  used for loops that do not declare their counter
  heap allocated variables are mapped to the type of the variables if they were
  not heap allocated
 *)

(* For the context, see the documentation of type [ctx] in [ast.ml]. *)

(* NOTE at the moment ctx_var is global, meaning that bindings are not
   removed when we leave a scope; T: would be cleaner to have ctx_var
   as argument to the recursive function. *)

(* TODO (but only after all other TODOs are handled and commited):
  Could we first build the full optitrust AST without any variable context,
  and then have a totally separate function that traverses the term AST to
  produce the same term but decorated with contexts?
  In the long term, we could also do this for typedef definitions,
  but then we would need to rebuild all the types to insert typconstrid,
  so I'd rather avoid that.
  I thus suggest, for the moment, to only remove ctx_var from this file.
  We can set the ctx_var separately in another function, using an
  environment like done in stackelim. *)

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
 let string_of_overloaded_op ?(loc : location = None)
    (op : clang_ext_overloadedoperatorkind) : string =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Equal -> "="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | StarEqual -> "*="
  | _ -> fail loc "string_of_overloaded_op: non supported operator"

(* names for overloaded operators (later matched for printing) *)
 let overloaded_op ?(loc : location = None) ?(ctx : ctx option = None) (op : clang_ext_overloadedoperatorkind) : trm =
  match op with
  | Plus -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_add))
  | Minus -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_sub))
  | Star -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_mul))
  | Equal -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_binop Binop_set))
  | PlusEqual -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_add))
  | MinusEqual -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_sub))
  | StarEqual -> trm_prim ~loc ~ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_mul))
  | _ -> fail loc "overloaded_op: non supported operator"

(* [wrap_const ~const t] wrap type [t] into a const type *)
let wrap_const ?(const : bool = false) (t : typ) : typ =
  if const then typ_const t else t


let rec tr_type_desc ?(loc : location = None) ?(const : bool = false) ?(tr_record_types : bool = true) (d : type_desc) : typ =
  match d with
  | Pointer q ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    let ty = typ_ptr Ptr_kind_mut t in
    wrap_const ~const ty
  | LValueReference  q ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    wrap_const ~const (typ_ptr Ptr_kind_ref t)
  | RValueReference  q ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    wrap_const ~const (typ_ptr Ptr_kind_ref (typ_ptr Ptr_kind_ref t))
  | ConstantArray {element = q; size = n; size_as_expr = eo} ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    begin match eo with
      | None -> typ_array t (Const n)
      | Some e ->
        let s = tr_expr e in
        wrap_const ~const (typ_array t (Trm s))
    end
  | VariableArray {element = q; size = eo} ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    let s = tr_expr eo in
    wrap_const ~const (typ_array t (Trm s))
  | IncompleteArray q ->
    let t = tr_qual_type ~loc ~tr_record_types q in
    
    wrap_const ~const (typ_array t Undefined)
  | Auto ->
    typ_auto ()
  | BuiltinType b ->
    begin match b with
      | Void -> typ_unit ()
      | Bool -> wrap_const ~const (typ_bool ())
      | Int -> wrap_const ~const (typ_int ())
      | UInt -> wrap_const ~const (typ_int ~annot:[Unsigned] ())
      | Long -> wrap_const ~const (typ_int ~annot:[Long] ())
      | ULong -> wrap_const ~const (typ_int ~annot:[Unsigned; Long] ())
      | LongLong -> wrap_const ~const (typ_int ~annot:[Unsigned; Long] ())
      | ULongLong -> wrap_const ~const (typ_int ~annot:[Unsigned; Long; Long] ())
      | Float -> wrap_const ~const (typ_float ())
      | Double -> wrap_const ~const (typ_double ())
      | LongDouble -> wrap_const ~const (typ_double ~annot:[Long] ())
      | Char_S -> wrap_const ~const (typ_char ())
      | UChar -> wrap_const ~const (typ_char ~annot:[Unsigned] ())
      | Short -> wrap_const ~const (typ_int ~annot:[Short] ())
      | UShort -> wrap_const ~const (typ_int ~annot:[Unsigned; Short] ())
      | _ -> fail loc "tr_type_desc: builtin type not implemented"
    end
  | FunctionType {calling_conv = _; result = qr; parameters = po;
                  exception_spec = _; _} ->
    let tr = tr_qual_type ~loc qr in
    begin match po with
      | None -> typ_fun [] tr
      | Some {non_variadic = pl; variadic = _} ->
        let tl =
          List.map
            (fun (p : parameter) -> tr_qual_type ~loc p.desc.qual_type)
            pl
        in
        typ_fun tl tr
    end
  | Typedef {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
        let typ_to_add = typ_constr n ~tid:(get_typid_from_trm n)  in
        wrap_const ~const typ_to_add
      | _ -> fail loc ("tr_type_desc: only identifiers are allowed in type definitions")
    end
  | Elaborated {keyword = k; nested_name_specifier = _; named_type = q} ->
    begin match k with
      | Struct -> if tr_record_types then typ_record Struct (tr_qual_type ~loc q) else (tr_qual_type ~loc q)
      | Union ->  if tr_record_types then typ_record Union (tr_qual_type ~loc q) else (tr_qual_type ~loc q)
      | _ ->
        fail loc "tr_type_desc: only struct allowed in elaborated type"
    end
  | Record {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
         typ_constr n ~tid:(get_typid_from_trm n)
      | _ -> fail loc ("tr_type_desc: only identifiers are allowed in records")
    end
  | Enum {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
         typ_constr n ~tid:(get_typid_from_trm n)
      | _ -> fail loc ("tr_type_desc: only identifiers are allowed in enums")
    end
  | TemplateTypeParm name ->
    typ_template_param name
  | _ -> fail loc "tr_type_desc: not implemented"

and is_qual_type_const (q : qual_type) : bool =
  let {const;_} = q in const

and tr_qual_type ?(loc : location = None) ?(tr_record_types : bool = true) (q : qual_type) : typ =
  let ({desc = d; const = c; _} : qual_type) = q in
  tr_type_desc ~loc ~const:c ~tr_record_types d

and tr_ident (id : ident_ref node) : string =
  let {decoration = _; desc = {nested_name_specifier = _; name = n; _}} = id in
  match n with
  | IdentifierName s -> s
  | _ ->
    let loc = loc_of_node id in
    fail loc "tr_ident_ref: not implemented"

and tr_stmt (s : stmt) : trm =
  let loc = loc_of_node s in
  let ctx = Some (get_ctx ()) in
  match s.desc with
  | Compound sl -> trm_seq_nomarks ~loc ~ctx (List.map tr_stmt sl)
  | If {init = None; condition_variable = None; cond = c; then_branch = st;
        else_branch = seo} ->
    let tc = tr_expr c in
    let tt = tr_stmt st in
    begin match seo with
      | None -> trm_if ~loc ~ctx tc tt (trm_lit Lit_unit)
      | Some se ->
        let te = tr_stmt se in
        trm_if ~loc ~ctx tc tt te
    end
  | If _ ->
    fail loc "tr_stmt: variable declaration forbidden in if conditions"
  | While {condition_variable = _; cond = c; body = s} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_while ~loc ~ctx tc ts
  | Do {body = s; cond = c;} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_do_while ~loc ~ctx ts tc
  | For {init = inito; condition_variable = None; cond = condo; inc = stepo;
         body} ->
    let tr_stmt_opt (so : stmt option) : trm =
      match so with
      | None -> trm_lit ~loc ~ctx  Lit_unit
      | Some s -> tr_stmt s
      in
    let init = tr_stmt_opt inito in
    let cond =
      match condo with
      (* no condition is equivalent to true *)
      | None -> trm_lit ~annot:[Empty_cond] ~loc ~ctx (Lit_bool true)
      | Some e -> tr_expr e
    in
    let step = tr_stmt_opt stepo in
    let body = tr_stmt body in
    trm_for_of_trm_for_c(trm_for_c~loc ~ctx init cond step body)
  | For _ ->
    fail loc "tr_stmt: variable declaration forbidden in for conditions"
  | Return eo ->
    begin match eo with
      | None -> trm_abort ~loc ~ctx (Ret None)
      | Some e ->
        let t = tr_expr e in
        trm_abort ~loc ~ctx (Ret (Some t))
    end
  | Break -> trm_abort ~loc ~ctx (Break None)
  | Continue -> trm_abort ~loc ~ctx (Continue None)
  | Decl dl ->
    begin match dl with
      | [] -> fail loc "tr_stmt: empty declaration list"
      | [d] -> tr_decl d
      | _ -> trm_seq_nomarks ~annot:[Multi_decl] ~loc ~ctx (tr_decl_list dl)
    end
  | Expr e -> tr_expr ~is_statement:true e
  | Label {label = l; body = s} ->
    let t = tr_stmt s in
    trm_labelled ~loc ~ctx l t
  | Null -> trm_lit ~loc ~ctx Lit_unit
  | Switch {init = None; condition_variable = None; cond = c; body = s} ->
    begin match s.desc with
      | Compound sl -> tr_switch loc c sl
      | _ -> fail loc "tr_stmt: switch cases must be in a compound statement"
    end
  | Switch _ ->
    fail loc "tr_stmt: variable declaration forbidden in switch conditions"
  | Goto l -> trm_goto ~loc ~ctx l
  | _ ->
    fail loc ("tr_stmt: the following statement is unsupported: " ^
              Clang.Stmt.show s)

(*
  translation of switch statements:
  - nested cases: only full sharing allowed
      case bla: case bli: … break allowed
      case bla: …; case bli: … break forbidden
  - warning about clangml ast:
      case bla: instr1; instr2; break represented as
      Case {lhs = bla; body = instr1}; instr2; break in case list
 *)
and tr_switch (loc : location) (cond : expr) (cases : stmt list) : trm =
  let t = tr_expr cond in
  let rec aux (loc : location) = function
    | [] -> []
    | s :: sl ->
      let loc' = loc_of_node s in
      begin match s.desc with
        | Case _ ->
          let (casel, s') = compute_cases [] s in
          let (body, sl') = compute_body loc [] (s' :: sl) in
          (casel, body) :: (aux loc' sl')
        | Default s' ->
          let (body, sl') = compute_body loc' [] (s' :: sl) in
          ([], body) :: (aux loc' sl')
        | _ -> fail loc "tr_switch: case or default expected"
      end
  in
  trm_switch ~loc ~ctx:(Some (get_ctx ())) t  (aux loc cases)

(*
  compute the list of nested cases described by s in reverse order and the first
  instruction of their body
 *)
and compute_cases (case_acc : trms) (s : stmt) : trms * stmt =
  let loc = loc_of_node s in
  match s.desc with
  | Case {lhs = e; rhs = None; body = s'} ->
    let t = tr_expr e in
    compute_cases (t :: case_acc) s'
  | Case _ -> fail loc "compute_cases: case ranges forbidden"
  | Default _ ->
    fail loc "compute_cases: please replace nested cases with default only"
  | _ -> (List.rev case_acc, s)

(*
  compute the body of a (nested) case starting from the beginning of the list
  stop at first break or fail if none
  return the rest of the case list too
  to find the first break, cases must be written without compound statements
    -> no variable declaration in cases
*)
and compute_body (loc : location) (body_acc : trms)
    (sl : stmt list) : trm * (stmt list) =
  match sl with
  | [] -> fail loc "compute_body: cases must end with break"
  | s :: sl ->
    begin match s.desc with
      | Case _ | Default _ ->
        fail loc "compute_body: nested cases must share their whole content"
      | Break ->
        begin match List.rev body_acc with
          | [t] -> (t, sl)
          | tl -> (trm_seq_nomarks ~annot:[No_braces (Nobrace.current ())] ~loc ~ctx:(Some (get_ctx ())) tl, sl)
        end
      | _ ->
        let t = tr_stmt s in
        compute_body loc (t :: body_acc) sl
    end

and tr_expr ?(is_statement : bool = false)
    (e : expr) : trm =
  (* let aux = tr_expr *)
  let loc = loc_of_node e in
    (* TODO: define a function [trm_apps'] for
         trm_apps ~loc ~is_statement ~typ ~ctx
       Beware that in a few places there is a [let loc] that
       modifies the loc, so in those case [trm_apps'] should be
       either not used or redefined locally for the new loc *)
  let typ : typ option =
    let q = Clang.Type.of_node e in
    try Some (tr_qual_type ~loc q) with
    | _ ->
      print_info loc "tr_expr: unable to translate type %s\n"
        (Clang.Type.show q);
      None
  in
  let ctx = Some (get_ctx()) in
  match e.desc with
  | ConditionalOperator {cond; then_branch = Some e_then;
                         else_branch = e_else} ->
    let t_cond = tr_expr cond in
    let t_then = tr_expr e_then in
    let t_else = tr_expr e_else in
    trm_apps ~loc ~is_statement ~typ ~ctx (trm_prim ~loc ~ctx Prim_conditional_op)
      [t_cond; t_then; t_else]
  | ConditionalOperator _ ->
    fail loc
      "tr_expr: conditional operators without then branch unsupported"
  | IntegerLiteral i ->
    begin match i with
      | Int i -> trm_lit ~loc ~ctx (Lit_int i)
      | _ -> fail loc "tr_expr: only int literal allowed"
    end
  | BoolLiteral b -> trm_lit ~loc ~ctx (Lit_bool b)
  | FloatingLiteral f ->
    begin match f with
      | Float f -> trm_lit ~loc ~ctx (Lit_double f)
      | _ -> fail loc "tr_expr: only float literal allowed"
    end
  | StringLiteral {byte_width = _; bytes = s; string_kind = _} ->
    trm_lit ~loc ~ctx (Lit_string s)

  | InitList el ->
    (* maybe typ is already the value of tt ---let tt = tr_qual_type ~loc t in *)
    let tt = match typ with
      | None -> fail loc ("unable to obtain type of an initialization list")
      | Some ty -> ty
    in
    let tl = List.map tr_expr el in
    let tl = Mlist.of_list tl in
    begin match get_typ_kind (get_ctx()) tt with
    | Typ_kind_array -> trm_array ~loc ~ctx ~typ:(Some tt) tl
    | Typ_kind_prod -> trm_struct ~loc ~ctx ~typ:(Some tt) tl
    | Typ_kind_undefined -> trm_struct ~loc ~ctx ~typ:(Some tt) tl
    | _ ->
        fail loc ("tr_decl: initialisation lists only " ^
                  "allowed for struct and array")
    end
  | UnaryExpr {kind = k; argument = a} ->
    begin match k with
      | SizeOf ->
        begin match a with
          | ArgumentExpr e ->
            let t = tr_expr e in
            trm_apps ~loc ~typ ~ctx (trm_var ~loc "sizeof") [t]
          | ArgumentType q ->
            let ty = tr_qual_type q in
            trm_var ~loc ~typ ~ctx ("sizeof(" ^ Ast_to_c.typ_to_string ty ^ ")")
        end
      | _ -> fail loc "tr_expr: unsupported unary expr"
    end
  | UnaryOperator {kind = k; operand = e} ->
     (* TODO: move this out into a separate function earlier in the file: [loc_of_unary_operator] *)
    let loc = (* deduce location of infix symbol *)
      match loc, loc_of_node e with
      | Some {loc_file = file; loc_start = {pos_line = start_row1; pos_col = start_column1}; _}, Some {loc_start = {pos_line = start_row2; pos_col = start_column2}; _}->
        Some {loc_file = file; loc_start = {pos_line = start_row1; pos_col = start_column1}; loc_end = {pos_line = start_row2; pos_col = start_column2}}
      | _ -> None
     in
    begin match k with
      | AddrOf -> (* expectation: e is not a const variable *)
        (* We are translating a term of the form that involves [&p],
           for example, [int p = 3; f(&p)]. In our AST, [p] represents
           the address of the cell at which [3] is stored, thus the
           call is actually [f(p)]. In other words we drop the [&] operator. *)
        let t = tr_expr e in
        trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop Unop_address) [t]
      | _ ->
        let t = tr_expr e in 
        let trm_apps1 unop t1 = trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc unop) [t1] in
        begin match k with
          | PostInc ->
            trm_apps1 Unop_post_inc t
            (* trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc Unop_post_inc) [t] *)
          | PostDec ->
            trm_apps1 Unop_post_dec t
          | PreInc ->
            (* trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc Unop_pre_inc) [t] *)
            trm_apps1 Unop_pre_inc t
          | PreDec ->
            (* trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc Unop_pre_dec) [t] *)
            trm_apps1 Unop_pre_dec t
          | Deref ->
            (* trm_apps ~loc ~typ ~ctx (trm_unop ~loc Unop_get) [t] *)
            trm_apps1 Unop_get t
          | Minus ->
            (* trm_apps ~loc ~typ ~ctx (trm_unop ~loc ~ctx Unop_opp) [t] *)
            trm_apps1 Unop_opp t
          | Not ->
            trm_apps1 Unop_bitwise_neg t
          | LNot ->
            trm_apps1 Unop_neg t
          | _ -> fail loc "tr_expr: unary operator not implemented"
        end
    end
  | BinaryOperator {lhs = le; kind = k; rhs = re} ->
    let loc = (* deduce location of infix symbol *)
      match loc_of_node le, loc_of_node re with
      | Some {loc_file = file; loc_end = {pos_line = end_row1; pos_col = end_column1}; _}, Some {loc_start = {pos_line = start_row2; pos_col = start_column2}; _}->
        Some {loc_file = file; loc_start = {pos_line = end_row1; pos_col = end_column1}; loc_end = {pos_line = start_row2; pos_col = start_column2}}
      | _ -> None
      in
    let tr = tr_expr re in
    let tl = tr_expr le in
    let trm_prim_c binop tl tr = trm_prim_compound ~loc ~is_statement ~ctx ~typ binop tl tr in
    begin match k with
      | Assign ->
        trm_set ~loc ~ctx ~is_statement tl tr
      | AddAssign ->
        trm_prim_c Binop_add tl tr
      | SubAssign ->
        trm_prim_c Binop_sub tl tr
      | MulAssign ->
         trm_prim_c Binop_mul tl tr
      | DivAssign ->
         trm_prim_c Binop_div tl tr
      | RemAssign ->
         trm_prim_c Binop_mod tl tr
      | ShlAssign ->
         trm_prim_c Binop_shiftl tl tr
      | ShrAssign ->
         trm_prim_c Binop_shiftr tl tr
      | AndAssign ->
         trm_prim_c Binop_and tl tr
      | OrAssign ->
         trm_prim_c Binop_or tl tr
      | XorAssign ->
         trm_prim_c Binop_xor tl tr
      | _ ->
        begin match k with 
          | Mul -> trm_mul ~loc ~ctx ~typ tl tr
          | Div -> trm_div ~loc ~ctx ~typ tl tr 
          | Add -> trm_add ~loc ~ctx ~typ tl tr
          | Sub -> trm_sub ~loc ~ctx ~typ tl tr
          | LT ->  trm_lt ~loc ~ctx ~typ tl tr
          | GT ->  trm_gt ~loc ~ctx ~typ tl tr
          | LE ->  trm_le ~loc ~ctx ~typ tl tr
          | GE ->  trm_ge ~loc ~ctx ~typ tl tr
          | EQ ->  trm_eq ~loc ~ctx ~typ tl tr
          | NE ->  trm_neq ~loc ~ctx ~typ tl tr
          | And -> trm_and ~loc ~ctx ~typ tl tr
          | LAnd -> trm_bit_and ~loc ~ctx ~typ tl tr
          | Or ->  trm_or ~loc ~ctx ~typ tl tr
          | LOr -> trm_bit_or ~loc ~ctx ~typ tl tr
          | Shl -> trm_shiftl ~loc ~ctx ~typ tl tr
          | Shr -> trm_shiftr ~loc ~ctx ~typ tl tr
          | Rem -> trm_mod ~loc ~ctx ~typ tl tr
          | Xor -> trm_xor ~loc ~ctx ~typ tl tr
          | _ -> fail loc "tr_expr: binary operator not implemented"
        end
    end
  | Call {callee = f; args = el} ->
    let tf = tr_expr f in
    begin match tf.desc with
    | Trm_var (_, x) when Str.string_match (Str.regexp "overloaded=") x 0 ->
        begin match el with
        | [tl;tr] -> trm_set ~loc ~ctx  ~is_statement (tr_expr tl) (tr_expr tr)
        | _ -> fail loc "tr_expr: overloaded= expects two arguments"
        end
    | _-> trm_apps ~loc ~ctx  ~is_statement ~typ tf (List.map tr_expr el)
    end
  | DeclRef {nested_name_specifier = _; name = n; _} -> (* Occurrence of a variable *)
    begin match n with
      | IdentifierName s ->
        (*
          particular case for variables:
          we look at the type given in their declaration to take into account
          type aliases
         *)
        let typ : typ option =
          (*
            clangml version: does not work (q = InvalidType for several vars in
            picGoal.cpp)
           *)
          (* let q = Clang.Type.of_cursor (Clang.Expr.get_definition e) in
           * Some (tr_qual_type ~loc q) *)
          (* hack with ctx_var *)
          String_map.find_opt s !ctx_var
        in
        trm_var ~loc ~ctx ~typ s
      | OperatorName op -> overloaded_op ~loc ~ctx  op
      | _ -> fail loc "tr_expr: only identifiers allowed for variables"
    end
  | Member {base = eo; arrow = has_arrow; field = f} ->
    begin match eo with
    | None -> fail loc "tr_expr: field accesses should have a base"
    | Some e ->
      begin match f with
      | FieldName id ->
        let f = tr_ident id in
        let base = tr_expr e in
        if has_arrow then
          trm_apps (trm_unop ~annot:[Display_arrow] (Unop_struct_get f) ) [trm_get base]
        else
          trm_apps (trm_unop (Unop_struct_get f) ) [base]
      | _ -> fail loc "tr_expr: fields should be accessed by names"
      end
    end
  | ArraySubscript {base = e; index = i} ->
    let ti = tr_expr i in
    let te = tr_expr e in
    (*
       override typ to account for typedefs:
       if e's type is x*, make sure typ is x (it is not always the case if x is
       declared through a typedef)
      *)
    let typ =
      match te.typ with
      | Some {typ_desc = Typ_array (ty, _); _} -> Some ty
      | Some {typ_desc = Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty}; _} -> Some ty
      (* should not happen *)
      | _ -> None
    in
      trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx (Binop_array_get)) [te; ti]

  | Construct {qual_type = _; args = el} ->
    (* only known use case: return of a struct variable *)
    begin match el with
      | [e] -> tr_expr ~is_statement  e
      | _ -> fail loc "tr_expr: unsupported construct"
    end
  | Cast {kind = k; qual_type = q; operand = e'} ->
    begin match k with
      | CStyle | Static ->
        let t = tr_qual_type ~loc q in
        let te' = tr_expr e' in
        trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx (Unop_cast t)) [te']
      | _ -> fail loc "tr_expr: only static casts are allowed"
    end
  | New {placement_args = _; qual_type = q; array_size = seo; init = ieo} ->
    let tq = tr_qual_type ~loc q in
    begin match ieo with
      | None -> ()
      | Some _ ->
        print_info loc
          "tr_expr: ignoring initialisation in new statement\n"
    end;
    begin match seo with
      | None -> trm_prim ~loc ~ctx (Prim_new tq)
      | Some se ->
        begin match tr_expr se with
          | {desc = Trm_val (Val_lit (Lit_int n)); loc; _} ->
            trm_prim ~loc ~ctx (Prim_new (typ_array tq (Const n)))
          | {desc = Trm_var (kind, x); loc; _} ->
           trm_prim ~loc ~ctx (Prim_new (typ_array tq (Trm (trm_var ~loc ~ctx ~kind x))))
          | _ ->
            fail loc ("tr_expr: new array size must be either " ^
                      "constant or variable")
        end
    end
  | UnexposedExpr ImplicitValueInitExpr ->
    print_info loc "tr_expr: implicit initial value\n";
    trm_lit ~loc ~ctx Lit_uninitialized
  (* sometimes Null is translated like this *)
  | UnknownExpr (GNUNullExpr, GNUNullExpr) -> trm_null ~loc ~ctx ()
  | ImplicitValueInit _ -> trm_lit ~loc ~ctx Lit_uninitialized
  | _ ->
    fail loc
      ("tr_expr: the following expression is unsupported: " ^
       Clang.Expr.show e)

and tr_attribute (loc : location) (a : Clang.Ast.attribute) : attribute =
  match a.desc with
  | Aligned {spelling = _; alignment_expr = e} -> Aligned (tr_expr e)
  | _ -> fail loc "tr_attribute: unsupported attribute"

and tr_decl_list (dl : decl list) : trms =
  let loc =
    (* some recursive calls might be on the empty list *)
    match dl with
    | d :: _ -> loc_of_node d
    | _ -> None
  in
  match dl with
  | [] -> []
  | [{decoration = _; desc = RecordDecl _}] ->
    fail loc ("tr_decl_list: record declarations must be followed by " ^
              "type definitions")
  | [d] -> [tr_decl d]
  | {decoration = _; desc = RecordDecl {keyword = k; attributes = _;
                                        nested_name_specifier = _; name = rn;
                                        bases = _; fields = fl; final = _;
                                        complete_definition = _;_ }} ::
    ({desc = Var _;_} as d1) ::
    dl' ->
       let prod_list = List.map (fun (d : decl) ->
      let loc = loc_of_node d in
      match d with
      | {decoration = _; desc = Field {name = fn; qual_type = q; attributes = al;_}} ->
        let ft = tr_qual_type ~loc q in
        let al = List.map (tr_attribute loc) al in
        let ty = {ft with typ_attributes = al} in
        (* LATER: Fix this *) (* TODO: you can define in ast.ml
            let var_mutability_unknown = Var_mutable  --as an arbitrary value
            and then use [var_mutability_unknown] below, to document that it is a dummy
            value until we call the function stackvar_elim *)
        (fn, ty)
      | _ ->
        fail loc "tr_decl_list: only fields are allowed in record declaration"
    ) fl in
      let kw = match k with
      | Struct -> Struct
      | Union -> Union
      | Class -> Class
      | _ -> fail loc "tr_decl_list: special records are not supported" in
      let tl' = tr_decl_list dl' in
      trm_let_record rn kw (List.rev prod_list) (tr_decl d1) :: tl'

  | {decoration = _; desc = RecordDecl {keyword = k; attributes = _;
                                        nested_name_specifier = _; name = rn;
                                        bases = _; fields = fl; final = _;
                                        complete_definition = _; _}} ::
    {decoration = _; desc = TypedefDecl {name = tn; underlying_type = _q}} ::
    dl' ->
    begin match k with
      | Struct ->
        (* typedef struct rn { int x,y; } tn;
           is only allowed if rn is empty or same as tn. *)
        if rn <> "" && rn <> tn
          then fail loc (sprintf "Typedef-struct: the struct name (%s) must match the typedef name (%s).\n" tn rn);

        (* First add the constructor name to the context, needed for recursive types *)
        let tid = next_typconstrid () in
        ctx_tconstr_add tn tid;

        (* Second, parse the fields names and types *)
        let prod_list = List.map ( fun (d : decl) ->
          let loc = loc_of_node d in
          match d with
          | {decoration = _; desc = Field {name = fn; qual_type = q;
                                              bitwidth = _; init = _;
                                              attributes = al}} ->
            ctx_label_add fn tid;
            let ft = tr_qual_type ~loc q in
            let al = List.map (tr_attribute loc) al in
            let ty = {ft with typ_attributes = al} in
            (fn, ty)
          | _ -> fail loc "tr_decl_list: only fields are allowed in struct declaration"

        ) fl in
        (* Third, add the typedef to the context *)
        let two_names = if rn = "" then false else true in
        let td = {
          typdef_loc = loc;
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_prod (two_names, (List.rev prod_list))
          } in
        ctx_typedef_add tn tid td;
        let trm_td = trm_typedef ~loc ~ctx:(Some (get_ctx())) td in
        let tl' = tr_decl_list dl' in
        trm_td :: tl'

      | _ -> fail loc "tr_decl_list: only struct records are allowed"
    end
  | d :: d' :: dl ->
    let td = tr_decl d in
    let tl = tr_decl_list (d' :: dl) in
    td :: tl

and tr_decl (d : decl) : trm =
  let loc = loc_of_node d in
  let ctx = Some (get_ctx ()) in
  let res = match d.desc with
  | EnumDecl {name = tn; constants; _} ->
    let enum_constant_l =
      List.map
        (fun ({desc = {constant_name; constant_init}; _} : enum_constant) ->
           match constant_init with
           | None -> ((constant_name, None) : var * (trm option))
           | Some e ->
             let t_init = tr_expr e in
             (constant_name, Some t_init)
        )
        constants
    in
    let tid = next_typconstrid () in
    ctx_tconstr_add tn tid;
    let td = {
      typdef_loc = loc;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_enum enum_constant_l
    } in
    ctx_typedef_add tn tid td;
    trm_typedef ~loc ~ctx td
  | Function {linkage = _; function_type = t; nested_name_specifier = _;
              name = n; body = bo; deleted = _; constexpr = _; _} ->
    let s =
      begin match n with
        | IdentifierName s -> s
        | OperatorName op -> string_of_overloaded_op ~loc op
        | _ ->
          fail loc ("tr_decl: only identifiers and overloaded " ^
                    "operators allowed for functions")
      end
    in
    let {calling_conv = _; result = _; parameters = po;
         exception_spec = _; _} = t in
    let tt = tr_type_desc ~loc (FunctionType t) in
    begin match tt.typ_desc with
      | Typ_fun (args_t, out_t) ->
        begin match po with
          | None ->
            if List.length args_t != 0 then
              fail loc "tr_decl: wrong size of argument list";
            let tb =
              match bo with
              | None -> trm_lit ~loc Lit_uninitialized
              | Some s -> tr_stmt s
            in
            trm_let_fun ~loc s out_t  [] tb
          | Some {non_variadic = pl; variadic = _} ->
            let args =
              List.combine
                (List.map
                   (fun {decoration = _;
                         desc = {qual_type = _; name = n; default = _}} -> n)
                   pl
                )
                args_t
            in
            List.iter (fun (y, ty) -> ctx_var_add y ty) args;
            let tb =
              match bo with
              | None -> trm_lit ~loc Lit_uninitialized
              | Some s -> tr_stmt s
            in
            trm_let_fun ~loc s out_t  args tb
        end
      |_ -> fail loc "tr_decl: should not happen"
    end

  | Var {linkage = _; var_name = n; var_type = t; var_init = eo; constexpr = _; _} ->

    let rec contains_elaborated_type (q : qual_type) : bool =
      let {desc = d;const = _;_} = q in
      match d with
      | Pointer q -> contains_elaborated_type q
      | LValueReference q -> contains_elaborated_type q
      | RValueReference q -> contains_elaborated_type q
      | ConstantArray {element = q;_} -> contains_elaborated_type q
      | VariableArray {element = q; _} -> contains_elaborated_type q
      | IncompleteArray q -> contains_elaborated_type q
      | Elaborated _ -> true
      | _ -> false

    in
    let tt = if contains_elaborated_type t then tr_qual_type ~loc ~tr_record_types:false t else tr_qual_type ~loc t in

    let const = is_typ_const tt in
    let te =
      begin match eo with
      | None -> trm_lit ~loc Lit_uninitialized
      | Some e ->
        begin match e.desc with
        | InitList el -> (* {e1,e2,e3} *)(* Array(struct intstantiation) declaration  with initialization *)
          let tl = List.map tr_expr el in
          let tl = Mlist.of_list tl in
          begin match get_typ_kind (get_ctx()) tt with
          | Typ_kind_array -> trm_array ~loc ~typ:(Some tt) tl
          | Typ_kind_prod -> trm_struct ~loc ~typ:(Some tt) tl
          | Typ_kind_undefined -> trm_struct ~loc ~typ:(Some tt) tl
          | _ -> fail loc ("tr_decl: initialisation lists only " ^ "allowed for struct and array")
          end

        | _ -> tr_expr e
        end
      end
      in
    ctx_var_add n tt;
    (* TODO: we can use [var_mutability_unknown] here again *)
    let mut = if const then Var_immutable else Var_mutable in
    trm_let ~loc ~is_statement:true mut (n,tt) te
  | TypedefDecl {name = tn; underlying_type = q} ->
    let tid = next_typconstrid () in
    ctx_tconstr_add tn tid;
    let tq = tr_qual_type ~loc q in
    let td = {
      typdef_loc = loc;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_alias tq
    }
    in
    ctx_typedef_add tn tid td;
    trm_typedef ~loc ~ctx td
  | TypeAlias {ident_ref = id; qual_type = q} ->
    begin match id.name with
      | IdentifierName tn ->
        let tid = next_typconstrid () in
        ctx_tconstr_add tn tid;
        let tq = tr_qual_type ~loc q in
        let td = {
          typdef_loc = loc;
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_alias tq;} in
        ctx_typedef_add tn tid td;
        trm_typedef ~loc ~ctx td
      | _ -> fail loc "tr_decl: only identifiers allowed for type aliases"
    end
  | LinkageSpec {language = lang;decls = dl} ->

     let dls = tr_decl_list dl in
     let lang = match lang with
     | C -> "C"
     | CXX -> "C++"
     | _ -> "" in
     trm_extern lang dls
  | RecordDecl {keyword = k; name = n; fields = fl;_} ->
    let prod_list = List.map (fun (d : decl) ->
      let loc = loc_of_node d in
      match d with
      | {decoration = _; desc = Field {name = fn; qual_type = q; attributes = al;_}} ->
        let ft = tr_qual_type ~loc q in
        let al = List.map (tr_attribute loc) al in
        let ty = {ft with typ_attributes = al} in
        (fn, ty)
        (* trm_let ~loc  Var_mutable (fn,typ_ptr_generated ty) (trm_prim ~loc (Prim_new ty)) *)
      | _ -> fail loc "tr_decl_list: only fields are allowed in record declaration"
    ) fl in
      let kw = match k with
      | Struct -> Struct
      | Union -> Union
      | Class -> Class
      | _ -> fail loc "tr_decl_list: special records are not supported" in
      trm_let_record n kw (List.rev prod_list) (trm_lit (Lit_unit))
  | Namespace {name = n; declarations = dl; inline = b} ->
    let dls = tr_decl_list dl in
    trm_namespace n (trm_seq_nomarks dls) b

  | TemplateDecl {parameters = {list = pl;_}; decl = d} ->
    let dl = tr_decl d in
    let pl = List.map (fun {decoration = _;desc = {parameter_name = n; parameter_kind = pk; parameter_pack = b};_} ->
      let tpk =  begin match pk with
        | Class {default = opt_q} ->
         begin match opt_q with
         | Some q -> Type_name (Some (tr_qual_type ~loc q))
         | None -> Type_name None
         end
        | NonType {parameter_type = q; default = opt_expr} ->
         begin match opt_expr with
         | Some e -> NonType (tr_qual_type ~loc q, Some (tr_expr e ))
         | None -> NonType (tr_qual_type ~loc q, None)
         end
        | _ -> fail loc "tr_decl: nested templates are not supported" (* LATER: Add support for nested templates *)
        end in
        (n, tpk, b)
    ) pl in
    trm_template pl dl

  | _ -> fail loc "tr_decl: not implemented" in
     res
module Include_map = Map.Make(String)
type 'a imap = 'a Include_map.t

let filter_out_include (filename : string)
    (dl : decl list) : ((decl list) imap) * (decl list) =
  let rec aux (include_map : (decl list) imap) (dl : decl list) =
    match dl with
    | [] -> (include_map, [])
    | d :: dl ->
      let (include_map, file_decls) = aux include_map dl in
      let file = file_of_node d in
      (* keep only project included files *)
      if Filename.dirname file <> Filename.dirname filename
      then (include_map, file_decls)
      else
      if file <> filename
      then
        (Include_map.update (Filename.basename file)
           (fun dlo ->
              match dlo with
              | None -> Some [d]
              | Some dl -> Some (d :: dl)
           )
           include_map,
         file_decls

        )
      else (include_map, d :: file_decls)
  in
  aux (Include_map.empty) dl

(* For debugging: set this variable to true for obtaining a file where to
   see the clang AST that gets passed as input to the conversion to our AST. *)
let dump_clang_ast = false

let dump_clang_file = "clang_ast.ml"

let tr_ast (t : translation_unit) : trm =
  (* Initialize id_counter *)
  init_typconstrid ();
  let {decoration = _; desc = {filename = filename; items = dl}} = t in
  print_info None "tr_ast: translating %s's AST...\n" filename;
  let (include_map, file_decls) = filter_out_include filename dl in
  if dump_clang_ast then begin
    let out_ast = open_out dump_clang_file in
    Include_map.iter
      (fun h dl ->
         Printf.fprintf out_ast "(* Include %s: *)\n" h;
         List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d))
           dl
      )
      include_map;
    Printf.fprintf out_ast "(* Main file: *)\n";
    List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d))
      file_decls;
    close_out out_ast;
  end;
  let loc = loc_of_node t in
  let tinclude_map =
    Include_map.mapi
      (fun h dl ->
         trm_seq_nomarks ~annot:[Include h] (tr_decl_list dl))
      include_map in
    trm_seq_nomarks ~loc ~annot:[Main_file] ((Include_map.fold (fun _ t tl -> t :: tl) tinclude_map []) @ tr_decl_list file_decls)
