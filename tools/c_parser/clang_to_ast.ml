open Clang.Ast
open Clang.Bindings
open Optitrust_utils
open Optitrust_ast
open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Tools
open Flags

(* One special uninitialized variable that must completely disappear after encoding *)
let var_uninitialized = toplevel_var "__uninitialized"
let trm_uninitialized = trm_var var_uninitialized

let qual_type_to_string (typ: qual_type): string =
  Format.asprintf "%a" Clang.Printer.qual_type typ

let warned_array_subscript_not_supported = resetable_ref String_set.empty

let warn_array_subscript_not_supported ?(loc: location) (t : typ option) : unit =
  let str = Option.to_string Ast_to_text.typ_to_string t in
  if not (String_set.mem str !warned_array_subscript_not_supported) then begin
    warned_array_subscript_not_supported := String_set.add str !warned_array_subscript_not_supported;
    verbose_warn "%s: does not support array subscript base type '%s'" (loc_to_string loc) str;
  end

(** [loc_of_node n]: gets the location of node [n] *)
let loc_of_node (n : 'a node) : location =
  let start_location_of_node = Clang.get_range_start (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n))in
  let end_location_of_node = Clang.get_range_end (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n )) in
  let (filename, start_row,start_column) = Clang.get_presumed_location start_location_of_node in
  let (_, end_row,end_column) = Clang.get_presumed_location end_location_of_node in
  Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column};
                             loc_end = {pos_line = end_row; pos_col = end_column}}


(** [loc_from_to start_l end_l]: gets the location from the begining of [start_l] to [end_l]
    if one of the locations is unknown then it will return [None] *)
let loc_from_to (start_l : location) (end_l : location) : location =
  match start_l, end_l with
  | Some {loc_file = file; loc_start = {pos_line = start_row1; pos_col = start_column1}; _},
    Some {loc_start = {pos_line = start_row2; pos_col = start_column2};_} ->
      let loc_start = {pos_line = start_row1; pos_col = start_column1} in
      let loc_end = {pos_line = start_row2; pos_col = start_column2} in
      Some {loc_file = file; loc_start; loc_end }
  | _ -> None

(** [file_of_node n]: gets the filename that contains node [n] *)
let file_of_node (n : 'a node) : string =
  match loc_of_node n with
  | Some {loc_file = filename; _} -> filename
  | _ -> failwith "Clang_to_astRawC.file_of_node: bad location"

(*
  map from variables to their type
  used for loops that do not declare their counter
  heap allocated variables are mapped to the type of the variables if they were
  not heap allocated *)

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


(* maps from [typ_ctx] *)
let ctx_var : typ Qualified_map.t ref = ref Qualified_map.empty

let ctx_reset () : unit =
  ctx_var := Qualified_map.empty

(** [ctx_var_add tv]: adds variable [v] with type [t] in map [ctx_var] *)
let ctx_var_add (tv : var) (t : typ) : unit =
  ctx_var := Qualified_map.add (tv.namespaces, tv.name) t (!ctx_var)

(** [string_of_overloaded_op ?loc op]: gets names of overloaded operators (later matched for printing) *)
 let string_of_overloaded_op ?(loc : location)
    (op : clang_ext_overloadedoperatorkind) : string =
  "operator" ^ match op with
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Equal -> "="
  | PlusEqual -> "+="
  | MinusEqual -> "-="
  | StarEqual -> "*="
  | Slash -> "/"
  | Percent -> "%"
  | Amp -> "&"
  | Pipe -> "|"
  | Less -> "<"
  | Greater -> ">"
  | SlashEqual -> "/="
  | PercentEqual -> "%="
  | AmpEqual -> "&="
  | PipeEqual -> "|="
  | LessLess -> "<<"
  | GreaterGreater -> ">>"
  | LessLessEqual -> "<<="
  | GreaterGreaterEqual -> ">>="
  | EqualEqual -> "=="
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | AmpAmp -> "&&"
  | PipePipe -> "||"
  | PlusPlus -> "++"
  | MinusMinus -> "--"
  | Subscript -> "[]"
  | Call -> "()"
  | _ -> loc_fail loc "Clang_to_astRawC.string_of_overloaded_op: non supported operator"

(** [wrap_const ~const t]: wrap type [t] into a const type if [const] is true *)
let wrap_const ~(const : bool) (t : typ) : typ =
  if const then typ_const t else t

(* ARTHUR *)
(** [wrap_atomic ~atomic t]: wrap type [t] into an atomic type if [atomic] is true *)
let wrap_atomic ~(atomic : bool) (t : typ) : typ =
  if atomic then typ_atomic t else t


(** [trm_for_c_inv_simple_init init]: checks if the init loop component is simple. If that's the case then return
   initial value of the loop index.
  Ex.:
    int x = a -> Some (x, a)
    x = a -> None *)
let trm_for_c_inv_simple_init (init : trm) : (var * trm) option =
  match init.desc with
  | Trm_let ((x, _), init) ->
    (* init has not been encoded at this stage so there is no ref to unwrap *)
    Some (x, init)
  | _ -> None

(** [trm_for_c_inv_simple_stop stop]: checks if the loop bound is simple.  If that's the case return that bound. *)
let trm_for_c_inv_simple_stop (loop_index: var) (stop_expr : trm) : (loop_dir * trm) option =
  (* Using eq instead of the more efficient var_eq because var ids are not computed at this step *)
  Pattern.pattern_match_opt stop_expr [
    Pattern.(trm_lt (trm_var (eq loop_index)) !__) (fun n () -> (DirUp, n));
    Pattern.(trm_le (trm_var (eq loop_index)) !__) (fun n () -> (DirUpEq, n));
    Pattern.(trm_gt (trm_var (eq loop_index)) !__) (fun n () -> (DirDown, n));
    Pattern.(trm_ge (trm_var (eq loop_index)) !__) (fun n () -> (DirDownEq, n))
  ]

(** [trm_for_c_inv_simple_step step]: checks if the loop step is simple. If that's the case then return that step. *)
let trm_for_c_inv_simple_step (loop_index: var) (loop_dir: loop_dir) (step_expr : trm) : trm option =
  match loop_dir with
  | DirUp | DirUpEq ->
    (* Using eq instead of the more efficient var_eq because var ids are not computed at this step *)
    Pattern.pattern_match_opt step_expr [
      Pattern.(trm_unop Unop_post_incr (trm_var (eq loop_index))) (fun () -> trm_step_one_post ());
      Pattern.(trm_unop Unop_pre_incr (trm_var (eq loop_index))) (fun () -> trm_step_one_pre ());
      Pattern.(trm_compound_assign Binop_add (trm_var (eq loop_index)) !__) (fun x () -> x);
    ]
  | DirDown | DirDownEq ->
    Pattern.pattern_match_opt step_expr [
      Pattern.(trm_unop Unop_post_decr (trm_var (eq loop_index))) (fun () -> trm_step_one_post ());
      Pattern.(trm_unop Unop_pre_decr (trm_var (eq loop_index))) (fun () -> trm_step_one_pre ());
      Pattern.(trm_compound_assign Binop_sub (trm_var (eq loop_index)) !__) (fun x () -> x);
    ]

(** [trm_for_of_trm_for_c t]: checks if loops [t] is a simple loop or not, if yes then return the simple loop else returns [t]. *)
let trm_for_of_trm_for_c (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, cond, step, body, _) ->
    let open Option.Monad in
    let simple_loop_opt =
      let* index, start = trm_for_c_inv_simple_init init in
      let* direction, stop = trm_for_c_inv_simple_stop index cond in
      let* step = trm_for_c_inv_simple_step index direction step in
      Some (trm_for { index; start; direction; stop; step } body)
    in
    Option.value ~default:t simple_loop_opt
  | _ -> trm_fail t "Ast.trm_for_of_trm_for_c: expected a for loop"


(** [redundant_template_definition_type]: Set by [tr_type_desc] when the type corresponds to a redundant template definition. *)
let redundant_template_definition_type = ref false

(* Raised by [tr_decl] when the current declaration is a redundant template definition. *)
exception RedundantTemplateDefinition

(* ARTHUR *)
(** [tr_type_desc ?loc ~const]: translates ClanML C/C++ type decriptions to OptiTrust type descriptions,
    [loc] gives the location of the type in the file that has been translated,
    if [const] is true then it means [d] is a const type, similarly if [const] is false then [d] is not a const type *)
(* FIXME: #odoc why is annotation required on callees? *)
let rec tr_type_desc ?(loc : location) ?(namespaces = []) (d : type_desc) : typ =
  match d with
  | Pointer q ->
    let t = tr_qual_type ?loc  q in
    typ_ptr t
  | LValueReference  q ->
    let t = tr_qual_type ?loc q in
    typ_ref t
  | RValueReference q ->
    failwith "Unsupported rvalue references"
  | ConstantArray {element = q; size = n; size_as_expr = eo} ->
    let t = tr_qual_type ?loc q in
    begin match eo with
      | None -> typ_array t ~size:(trm_int n)
      | Some e ->
        let size = tr_expr e in
        typ_array t ~size
    end
  (* ARTHUR: see clang__ast *)
  | Atomic q ->
    let t = tr_qual_type ?loc q in
    typ_atomic t
  | VariableArray {element = q; size = eo} ->
    let t = tr_qual_type ?loc q in
    let size = tr_expr eo in
    typ_array t ~size
  | IncompleteArray q ->
    let t = tr_qual_type ?loc q in
    typ_array t
  | Auto -> typ_auto
  | BuiltinType b ->
    begin match b with
      | Void -> typ_unit
      | Bool -> typ_bool
      | Int -> typ_int
      | UInt -> typ_uint
      | Float -> typ_f32
      | Double -> typ_f64
      | LongDouble -> loc_fail loc "Clang_to_ast.tr_type_desc: long double is not implemented, behaviour is too much platform dependant"
      | Char_S | Char_U -> typ_char
      | UChar | SChar | Short | UShort | Long | ULong | LongLong | ULongLong ->
        loc_fail loc "Clang_to_ast.tr_type_desc: short or long types are too much platform dependant, use int if you don't care about the size, and int#_t if you want a precise size for integers"
      | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: builtin type not implemented"
    end
  | FunctionType {calling_conv = _; result = qr; parameters = po;
                  exception_spec = _; _} ->
    let tr = tr_qual_type ?loc qr in
    begin match po with
      | None -> typ_fun [] tr
      | Some {non_variadic = pl; variadic = false} ->
        let tl =
          List.map
            (fun (p : parameter) -> tr_qual_type ?loc p.desc.qual_type)
            pl
        in
        typ_fun tl tr
      | Some {variadic = true; _} -> loc_fail loc "Clang_to_ast.tr_type_desc: variadic arguments are not supported"
    end
  | Typedef {nested_name_specifier = nns; name = n; _} ->
    begin match n with
      | IdentifierName n ->
        let qpath = namespaces @ tr_nested_name_specifier ?loc nns in
        if qpath = [] || qpath = ["std"] then
          (* Map standard sized types to builtins *)
          begin match n with
          | "size_t" -> typ_usize
          | "ptrdiff_t" -> typ_isize
          | "int8_t" -> typ_i8
          | "uint8_t" -> typ_u8
          | "int16_t" -> typ_i16
          | "uint16_t" -> typ_u16
          | "int32_t" -> typ_i32
          | "uint32_t" -> typ_u32
          | "int64_t" -> typ_i64
          | "uint64_t" -> typ_u64
          | _ -> typ_var (name_to_typvar ~namespaces:qpath n)
          end
        else
          typ_var (name_to_typvar ~namespaces:qpath n)
      | _ -> loc_fail loc "tr_type_desc: only identifiers are allowed in type definitions"
    end
  | Elaborated {keyword = k; nested_name_specifier = nns; named_type = q} ->
    let namespaces = namespaces @ tr_nested_name_specifier ?loc nns in
    let tr_ty = tr_qual_type ?loc ~namespaces q in
    begin match k with
      | Struct -> trm_add_cstyle Struct tr_ty
      | Class -> trm_add_cstyle Class tr_ty
      | NoKeyword -> tr_ty
      | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: this elaborated type is not supported."
    end
  | Record {nested_name_specifier = nns; name = n; _} ->
    let qpath = namespaces @ tr_nested_name_specifier ?loc nns in
    begin match n with
      | IdentifierName n -> typ_var (name_to_typvar ~namespaces:qpath n)
      | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: only identifiers are allowed in records"
    end
  | Enum {nested_name_specifier = nns; name = n; _} ->
    let qpath = namespaces @ tr_nested_name_specifier ?loc nns in
    begin match n with
      | IdentifierName n -> typ_var (name_to_typvar ~namespaces:qpath n)
      | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: only identifiers are allowed in enums"
    end
  | TemplateTypeParm name -> typ_var (name_to_typvar name)
  | TemplateSpecialization {name = NameTemplate nm; args = tyl } ->
    let tl = (List.map (fun (targ : template_argument) ->
      match targ with
      | Type q -> tr_qual_type ?loc q
      | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: only simple types are supported as template arguments") tyl) in
    typ_apps (typ_var (name_to_typvar ~namespaces nm)) tl
  | Decltype e ->
    let tr_e = tr_expr e in
    typ_typeof tr_e
  | SubstTemplateTypeParm tys ->
    redundant_template_definition_type := true;
    typ_var (name_to_typvar tys)
  | InjectedClassName {desc = q_d; _} ->
    let tq = tr_type_desc ?loc q_d in
    trm_add_cstyle InjectedClassName tq
  | _ -> loc_fail loc "Clang_to_ast.tr_type_desc: not implemented"

(** [is_qual_type_const q]: checks if [q] is a const type or not *)
and is_qual_type_const (q : qual_type) : bool =
  let {const;_} = q in const

(** [tr_qual_type ?loc ~tr_record_types q]: translates  ClanML C/C++ types into OptiTrust types *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_qual_type ?(loc : location) ?(namespaces = []) (q : qual_type) : typ =
  let ({desc; const; _} : qual_type) = q in
  wrap_const ~const (tr_type_desc ?loc ~namespaces desc)

(** [tr_nested_name_specifier ?loc name_specs]: converts Clang.name_specifier to a string list. *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_nested_name_specifier ?(loc : location) name_specs : string list =
  match name_specs with
  | Some nms ->
      List.map (fun name_spec ->
        match name_spec with
        | NamespaceName nm -> nm
        | TypeSpec q -> qual_type_to_string q
        | _ -> loc_fail loc "Clang_to_astRawC.tr_nested_name_specifier: name specifier not supported."
       ) nms
  | None -> []

(** [tr_ident id]: translates identifier [id] into a string *)
and tr_ident (id : ident_ref node) : string =
  let {decoration = _; desc = {nested_name_specifier = _; name = n; _}} = id in
  match n with
  | IdentifierName s -> s
  | _ ->
    let loc = loc_of_node id in
    loc_fail loc "Clang_to_astRawC.tr_ident_ref: not implemented"

(** [tr_stmt s]: translates statement [s] into an OptiTrust trm *)
and tr_stmt (s : stmt) : trm =
  let loc = loc_of_node s in
  match s.desc with
  | Compound sl ->
    let instrs = List.map tr_stmt sl in
    let open Option in
    let instrs, result, typ =
      if instrs = [] then
        [], None, None
      else
        let first_instrs, last_instr = List.unlast instrs in
        begin match trm_var_inv last_instr with
        | Some r -> first_instrs, Some r, last_instr.typ
        | None -> instrs, None, None
        end
    in
    trm_seq ?loc ?result ?typ (Mlist.of_list instrs)
  | If {init = None; condition_variable = None; cond = c; then_branch = st;
        else_branch = seo} ->
    let tc = tr_expr c in
    let tt = tr_stmt st in
    begin match seo with
      | None -> trm_if ?loc ~typ:typ_unit tc tt (trm_lit Lit_unit)
      | Some se ->
        let te = tr_stmt se in
        trm_if ?loc ~typ:typ_unit tc tt te
    end
  | If _ ->
    (* Add support for this feature. *)
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in if conditions"
  | While {condition_variable = _; cond = c; body = s} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_while ?loc tc ts
  | Do {body = s; cond = c;} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_do_while ?loc ts tc
  | For {init = inito; condition_variable = None; cond = condo; inc = stepo;
         body} ->
    let tr_stmt_opt (so : stmt option) : trm =
      match so with
      | None -> trm_lit ?loc Lit_unit
      | Some s -> tr_stmt s
      in
    let init = tr_stmt_opt inito in
    let cond =
      match condo with
      (* no condition is equivalent to true *)
      | None -> trm_add_cstyle Empty_cond (trm_lit ?loc (Lit_bool true))
      | Some e -> tr_expr e
    in
    let step = tr_stmt_opt stepo in
    let body = tr_stmt body in
    trm_for_of_trm_for_c (trm_for_c ?loc init cond step body)
  | For _ ->
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in for conditions"
  | Return eo ->
    begin match eo with
      | None -> trm_abort ?loc (Ret None)
      | Some e ->
        let t = tr_expr e in
        trm_abort ?loc (Ret (Some t))
    end
  | Break -> trm_abort ?loc (Break None)
  | Continue -> trm_abort ?loc (Continue None)
  | Decl dl ->
    begin match dl with
      | [] -> loc_fail loc "Clang_to_astRawC.tr_stmt: empty declaration list"
      | [d] -> tr_decl d
      | _ ->
       let dls = tr_decl_list dl in
       let bindings = List.fold_right (fun t1 acc ->
        begin match t1.desc with
        | Trm_let ((x, ty), init) -> (((x, ty), init) :: acc)
        | _ -> loc_fail loc "Clang_to_astRawC.tr_stmr: expected a multip declaration statemnt"
        end
       ) dls [] in
       trm_let_mult ?loc bindings
    end
  | Expr e ->
    tr_expr e
  | Label {label = l; body = s} ->
    let t = tr_stmt s in
    trm_add_label l t
  | Null -> trm_lit ?loc Lit_unit
  | Switch {init = None; condition_variable = None; cond = c; body = s} ->
    begin match s.desc with
      | Compound sl -> tr_switch loc c sl
      | _ -> loc_fail loc "Clang_to_astRawC.tr_stmt: switch cases must be in a compound statement"
    end
  | Switch _ ->
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in switch conditions"
  | Goto l -> trm_goto ?loc l
  | _ ->
    loc_fail loc ("Clang_to_astRawC.tr_stmt: the following statement is unsupported: " ^
              Clang.Stmt.show s)

(** [tr_switch s]: translates switch statement [s] into an OptiTrust trm
    translation of switch statements:
  - nested cases: only full sharing allowed
      case bla: case bli: … break allowed
      case bla: …; case bli: … break forbidden
  - warning about clangml ast:
      case bla: instr1; instr2; break represented as
      Case {lhs = bla; body = instr1}; instr2; break in case list *)
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
        | _ -> loc_fail loc "Clang_to_astRawC.tr_switch: case or default expected"
      end
  in
  trm_switch ?loc t (aux loc cases)

(** [compute_cases case_acc s]: computes a list of nested cases described by s in reverse order
    and the first instruction of their body *)
and compute_cases (case_acc : trms) (s : stmt) : trms * stmt =
  let loc = loc_of_node s in
  match s.desc with
  | Case {lhs = e; rhs = None; body = s'} ->
    let t = tr_expr e in
    compute_cases (t :: case_acc) s'
  | Case _ -> loc_fail loc "Clang_to_astRawC.compute_cases: case ranges forbidden"
  | Default _ ->
    loc_fail loc "Clang_to_astRawC.compute_cases: please replace nested cases with default only"
  | _ -> (List.rev case_acc, s)



(** [compute_body loc body_acc]: computes the body of a (nested) case starting from the beginning
    of the list , stop at first break or fail if none
    return the rest of the case list too
    to find the first break, cases must be written without compound statements
     -> no variable declaration in case *)
and compute_body (loc : location) (body_acc : trms)
    (sl : stmt list) : trm * (stmt list) =
  match sl with
  | [] -> loc_fail loc "Clang_to_astRawC.compute_body: cases must end with break"
  | s :: sl ->
    begin match s.desc with
      | Case _ | Default _ ->
        loc_fail loc "Clang_to_astRawC.compute_body: nested cases must share their whole content"
      | Break ->
        begin match List.rev body_acc with
          | [t] -> (t, sl)
          | tl -> trm_add_cstyle (No_braces 0) (trm_seq_nomarks ?loc tl), sl
        end
      | _ ->
        let t = tr_stmt s in
        compute_body loc (t :: body_acc) sl
    end


(** [tr_init_list ?loc ty el]: translates [el] into a trm, based on type [ty] the initialization list kind
     is determined. *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_init_list ?(loc : location) (ty : typ) (el : expr list) : trm =
  match el with
  | [] -> trm_null ty
  | _ ->
    match typ_array_inv ty with
    | Some (elem_typ, _) ->
      let tl = List.map tr_expr el in
      trm_array ?loc ~elem_typ tl
    | None ->
      (*let tl = List.map (fun (e : expr) ->
        match e.desc with
        | DesignatedInit { designators = dl; init = e } ->
          begin match dl with
          | [FieldDesignator f] ->
            (Some f, tr_expr e)
          | _ -> loc_fail loc "Clang_to_astRawC.tr_init_list: struct initialization with multiple designators per field are not supported"
          end
        | _ -> (None, tr_expr e)) el
      in*)
      let tl = List.map tr_expr el in
      trm_record ?loc ~typ:ty tl

(** [tr_expr e]: translates expression [e] into an OptiTrust trm *)
and tr_expr ?(cast_typ: typ option) (e : expr) : trm =
  (* let aux = tr_expr *)
  let loc = loc_of_node e in
  let typ : typ option =
    let q = Clang.Type.of_node e in
    try Some (tr_qual_type ?loc q) with
    | exn ->
      verbose_warn "%s: tr_expr: unable to translate type %s: %s" (loc_to_string loc) (Printexc.to_string exn)
        (Clang.Type.show q);
      None
  in
  match e.desc with
  | ConditionalOperator {cond; then_branch = Some e_then;
                         else_branch = e_else} ->
    let t_cond = tr_expr cond in
    let t_then = tr_expr e_then in
    let t_else = tr_expr e_else in
    let typ = Option.unsome typ in
    trm_add_cstyle Ternary_cond (trm_if ?loc ~typ t_cond t_then t_else)
  | ConditionalOperator _ ->
    loc_fail loc
      "tr_expr: conditional operators without then branch unsupported"
  | CompoundLiteral {init = init_expr; qual_type = _q} -> tr_expr init_expr
  | IntegerLiteral i ->
    begin match i with
      | Int i -> trm_int ?typ ?loc i
      | CXInt _ -> loc_fail loc "Clang_to_astRawC.tr_expr: found an integer literal that does not fit in OCaml's int"
    end
  | BoolLiteral b -> trm_bool ?loc b
  | FloatingLiteral f ->
    begin match f with
      | Float f -> trm_float ?typ ?loc f
      | CXFloat _ -> loc_fail loc "Clang_to_astRawC.tr_expr: found a float literal that does not fit in OCaml's float"
    end
  | StringLiteral {byte_width = _; bytes = s; string_kind = _} ->
    trm_string ?loc s


  | InitList el ->
    (* maybe typ is already the value of tt ---let tt = tr_qual_type ?loc t in *)
    let tt = match typ with
      | None -> loc_fail loc "unable to obtain type of an initialization list"
      | Some ty -> ty
    in
    tr_init_list ?loc tt el

  | UnaryExpr {kind = k; argument = a} ->
    begin match k with
      | SizeOf ->
        let q = match a with
          | ArgumentExpr e -> Clang.Type.of_node e
          | ArgumentType q -> q
        in
        let typ = tr_qual_type q ?loc in
        trm_sizeof typ
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: unsupported unary expr"
    end

  | UnaryOperator {kind = k; operand = e} ->
    let loc = loc_from_to loc (loc_of_node e) in
    let t = tr_expr e in
    let typ = Option.unsome_or_else typ (fun () -> failwith "%s: Missing type on unary expression" (loc_to_string t.loc)) in
    begin match k with
      | AddrOf ->
        let arg_typ = Option.unsome_or_else (typ_ptr_inv typ) (fun () -> failwith "%s: Type of the address-of expression is not a pointer" (loc_to_string t.loc)) in
        trm_address_of ~arg_typ t
      | Deref -> trm_get ~typ t
      | PostInc -> trm_post_incr ~typ t
      | PostDec -> trm_post_decr ~typ t
      | PreInc -> trm_pre_incr ~typ t
      | PreDec -> trm_pre_decr ~typ t
      | Minus -> trm_minus ~typ t
      | Plus -> trm_plus ~typ t
      | Not -> trm_bitwise_neg ~typ t
      | LNot -> trm_neg t
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: unary operator not implemented"
    end

  | BinaryOperator {lhs = le; kind = k; rhs = re} ->
    let loc = loc_from_to (loc_of_node le) (loc_of_node re) in
    let tl = tr_expr le in
    let tr = tr_expr re in
    let compound_assign binop =
      let ltyp = Option.unsome_or_else tl.typ (fun () -> failwith "%s: Missing type on the l-value of an assignment" (loc_to_string tl.loc)) in
      trm_compound_assign ?loc ~typ:ltyp binop tl tr
    in
    let find_binop_typ ?(cmp = false) () =
      let rec unwrap_operand_typ ty_opt loc =
        let ty = Option.unsome_or_else ty_opt (fun () -> failwith "%s: Missing type on arithmetic operand" (loc_to_string loc)) in
        Pattern.pattern_match ty [
          Pattern.(typ_builtin !__) (fun builtin () -> builtin);
          Pattern.(typ_const !__) (fun ty () -> unwrap_operand_typ (Some ty) loc);
          Pattern.(typ_ref !__) (fun ty () -> unwrap_operand_typ (Some ty) loc);
          Pattern.(typ_ptr __) (fun () ->
            (* When performing comparison, pointers are the same as usize *)
            if cmp then Typ_size Unsigned else
            failwith "%s: Direct arithmetic on pointers is not supported (use &p[i] syntax instead of (p+i))" (loc_to_string loc));
          Pattern.__ (fun () -> failwith "%s: Arithmetic operand has a non standard type (%s)" (loc_to_string loc) (Ast_to_text.typ_to_string ty))
        ]
      in

      let tyl = unwrap_operand_typ tl.typ tl.loc in
      let tyr = unwrap_operand_typ tr.typ tr.loc in

      (* Here we define custom coercion rules that are inspired but do not perfectly match C rules:
      - integers coerce to pointer size since computation on isize never need more precision
      - as the unspecified width type, int is "bigger" that other specified width types
      - there is no small type that is always converted to at least int32
      *)
      let btyp = match tyl, tyr with
      | Typ_float fl, Typ_float fr -> Typ_float (max fl fr)
      | Typ_float f, _ | _, Typ_float f -> Typ_float f
      | Typ_size Unsigned, _ | _, Typ_size Unsigned -> Typ_size Unsigned
      | Typ_size Signed, _ | _, Typ_size Signed -> Typ_size Signed
      | Typ_int Unsigned, _ | _, Typ_int Unsigned -> Typ_int Unsigned
      | Typ_int Signed, _ | _, Typ_int Signed -> Typ_int Signed
      | Typ_fixed_int (sl, nl), Typ_fixed_int (sr, nr) when nl > nr -> Typ_fixed_int (sl, nl)
      | Typ_fixed_int (sl, nl), Typ_fixed_int (sr, nr) when nl < nr -> Typ_fixed_int (sr, nr)
      | Typ_fixed_int (Unsigned, n), Typ_fixed_int _ | Typ_fixed_int _, Typ_fixed_int (Unsigned, n) -> Typ_fixed_int (Unsigned, n)
      | Typ_fixed_int (s, n), _ | _, Typ_fixed_int (s, n) -> Typ_fixed_int (s, n)
      | Typ_char, _ | _, Typ_char -> Typ_char
      | Typ_bool, Typ_bool -> Typ_bool
      in
      typ_builtin btyp
    in
    let arith_binop binop =
      trm_arith_binop ?loc ~typ:(find_binop_typ ()) binop tl tr
    in
    let cmp_binop binop =
      trm_cmp_binop ?loc ~typ:(find_binop_typ ~cmp:true ()) binop tl tr
    in
    begin match k with
      | Assign -> trm_set ?loc tl tr
      | AddAssign -> compound_assign Binop_add
      | SubAssign -> compound_assign Binop_sub
      | MulAssign -> compound_assign Binop_mul
      | DivAssign ->
        if is_typ_float (find_binop_typ ()) then
          compound_assign Binop_exact_div
        else
          compound_assign Binop_trunc_div
      | RemAssign -> compound_assign Binop_trunc_mod
      | ShlAssign -> compound_assign Binop_shiftl
      | ShrAssign -> compound_assign Binop_shiftr
      | AndAssign -> compound_assign Binop_bitwise_and
      | OrAssign -> compound_assign Binop_bitwise_or
      | XorAssign -> compound_assign Binop_xor

      | Add -> arith_binop Binop_add
      | Sub -> arith_binop Binop_sub
      | Mul -> arith_binop Binop_mul
      | Div ->
        if is_typ_float (find_binop_typ ()) then
          arith_binop Binop_exact_div
        else
          arith_binop Binop_trunc_div
      | Rem -> arith_binop Binop_trunc_mod
      | And -> arith_binop Binop_bitwise_and
      | Or ->  arith_binop Binop_bitwise_or
      | Shl -> arith_binop Binop_shiftl
      | Shr -> arith_binop Binop_shiftr
      | Xor -> arith_binop Binop_xor

      | LT ->  cmp_binop Binop_lt
      | GT ->  cmp_binop Binop_gt
      | LE ->  cmp_binop Binop_le
      | GE ->  cmp_binop Binop_ge
      | EQ ->  cmp_binop Binop_eq
      | NE ->  cmp_binop Binop_neq

      | LAnd -> trm_and ?loc tl tr
      | LOr -> trm_or ?loc tl tr

      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: binary operator not implemented"
    end

  | Call {callee = f; args = el} ->
    let tf = tr_expr f in
    let el = List.filter (fun (e : expr) -> match e.desc with DefaultArg -> false | _ -> true) el in
    (* DEPREACTED let tf = trm_add_cstyle_clang_cursor (cursor_of_node f) tf in*)
    begin match tf.desc with
    | Trm_var x when var_has_name "exact_div" x ->
      begin match List.map tr_expr el with
      | [n; b] ->
        let typ = Option.unsome_or_else typ (fun () -> failwith "%s: Missing type on exact_div" (loc_to_string loc)) in
        trm_exact_div ?loc ~typ n b
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: 'exact_div' expects two arguments"
      end
    | Trm_var x when var_has_name "operator=" x ->
        (* FIXME: For C++ this only works if operator= is the default one *)
        begin match el with
        | [tl;tr] ->
          let tl = tr_expr tl in
          let tr = tr_expr tr in
          trm_set ?loc tl tr
        | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: operator= expects two arguments"
        end
    | Trm_var x when var_has_name "operator()" x ->
      let t_args = List.map tr_expr el in
      let call_name, call_args = List.uncons t_args in
      trm_apps ?loc ?typ call_name call_args
    | _ ->
      trm_apps ?loc ?typ tf (List.map tr_expr el)
    end
  | DeclRef {nested_name_specifier = nns; name = n; template_arguments = targs} -> (* Occurrence of a variable *)
    let qpath = tr_nested_name_specifier ?loc nns in
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
           * Some (tr_qual_type ?loc q) *)
          (* hack with ctx_var *)
          Qualified_map.find_opt (qpath, s) !ctx_var
        in
        let res = trm_var ?loc ?typ (name_to_var ~namespaces:qpath s) in
        (* DEPRECATED? get cursor info with locations of variables
        let unified_symbol_resolution1 = Clang.get_cursor_usr (cursor_of_node e) in
        let unified_symbol_resolution2 = Clang.get_cursor_usr (Clang.get_cursor_definition (cursor_of_node e)) in
        printf "%s ---- %s\n" unified_symbol_resolution1 unified_symbol_resolution2;
        *)
        let targs = List.map (fun (targ : template_argument) -> begin match targ with | Type q -> tr_qual_type ?loc q | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: something went wrong." end) targs in
        begin match targs with
        | [] -> res
        | _ -> trm_add_cstyle (Typ_arguments targs) res
        end
      | OperatorName op -> trm_var ?loc ?typ (name_to_var ~namespaces:qpath (string_of_overloaded_op op))
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: only identifiers allowed for variables"
    end
  | Member {base = eo; arrow = has_arrow; field = f} ->
    begin match eo with
    | None ->
      if has_arrow
        then
          begin match f with
          | FieldName id ->
            let f = tr_ident id in
            (* TODO: give type to trm_this *)
            let t_this = trm_get (trm_add_cstyle Implicit_this (trm_this ())) in
            trm_struct_get ?loc ?field_typ:typ ~struct_typ:typ_auto t_this f
          | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accesses by names"
          end
        else loc_fail loc "Clang_to_astRawC.tr_expr: field accesses should have a base"
    | Some e ->
      let f = match f with
        | FieldName id -> tr_ident id
        | DependentScopeMember {ident_ref = id; template_arguments = _} ->
          begin match id.name with
          | IdentifierName f -> f
          | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accessed by names"
          end
        | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accessed by names"
      in
      let base = tr_expr e in
      if has_arrow then
        let struct_typ = match Option.bind (Option.map get_inner_const_type base.typ) typ_ptr_inv with
        | Some typ -> typ
        | None -> failwith "Clang_to_ast.tr_expr: could not guess the type of struct access base '%s'" (Ast_to_text.ast_to_string base)
        in
        trm_struct_get ?loc ?field_typ:typ ~struct_typ (trm_get ~typ:struct_typ base) f
      else
        let struct_typ = Option.unsome ~error:"Clang_to_ast.tr_expr: could not guess the type of struct access" base.typ in
        let get_op = trm_struct_get ?loc ?field_typ:typ ~struct_typ base f in
        if is_get_operation base then
          trm_add_cstyle No_struct_get_arrow get_op
        else get_op
    end
  | ArraySubscript {base = e; index = i} ->
    let ti = tr_expr i in
    let te = tr_expr e in
    (*(*
       override typ to account for typedefs:
       if e's type is x*, make sure typ is x (it is not always the case if x is
       declared through a typedef)
      *)
    let typ = Option.bind te.typ typ_of_get in
    if typ = None then begin
      (* happens for, e.g., typedef T = int*; *)
      warn_array_subscript_not_supported ?loc te.typ;
    end;*)
    trm_array_get ?loc ?typ te ti

  | Construct {qual_type = _; args = el} ->
    (* only known use case: return of a struct variable *)
    let el = List.filter (fun (e : expr) -> match e.desc with DefaultArg -> false | _ -> true) el in
    begin match el with
      | [e] -> tr_expr e
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: unsupported construct"
    end
  | Cast {kind = k; qual_type = q; operand = e'} ->
    begin match k with
      | CStyle | Static | Functional ->
        let t = tr_qual_type ?loc q in
        let te' = tr_expr e' in
        trm_cast ?loc t te'
      | Implicit -> (* Ignore implicit casts to get the real internal type *) tr_expr ?cast_typ:(Option.or_ cast_typ typ) e'
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: only static casts are allowed"
    end
  | New {placement_args = _; qual_type; array_size; init} ->
    let elem_ty = tr_qual_type ?loc qual_type in
    let alloc_ty = match array_size with
      | None -> elem_ty
      | Some size -> typ_array elem_ty ~size:(tr_expr size)
    in
    begin match init with
    | None -> trm_new_uninit ?loc alloc_ty
    | Some init ->
      let tinit = tr_expr init in
      trm_new ?loc alloc_ty tinit
    end
  | Delete {global_delete = _; array_form; argument = e} ->
    let te = tr_expr e in
    (*let typ = typ_or_auto (Option.bind te.typ typ_ptr_inv) in
    let typ = if array_form then typ_array typ else typ in*)
    trm_delete ?loc te
  | Lambda {capture_default = ByRef; captures = _; is_mutable = _; parameters = po; result_type = rt; body = b} ->
    let tt = match rt with
      | Some ty -> tr_qual_type ?loc ty
      | None -> typ_auto
    in
    let tb = tr_stmt b in
    let pl = match po with | Some pl -> pl | None -> [] in
    let args = List.map (fun {decoration = _;
      desc = {qual_type = q; name = n; default = _}} -> (name_to_var n, tr_qual_type ?loc q)) pl in
    List.iter (fun (y, ty) -> ctx_var_add y ty) args;
    trm_fun args tt tb
  | This -> trm_this ?loc ?typ ()

  | UnexposedExpr ImplicitValueInitExpr
  | ImplicitValueInit _ ->
    let typ = Option.unsome ~error:"Need a type for implicit zero initialization" typ in
    trm_null typ

  | NullPtrLiteral
  | UnknownExpr (GNUNullExpr, GNUNullExpr) (* sometimes NULL is translated with UnknownExpr (GNUNullExpr, GNUNullExpr). LATER: in which condition? is it actually used sometimes? *)
  | UnexposedExpr (GNUNullExpr) ->
    begin match Option.or_ typ cast_typ with
    | Some typ -> trm_null ~uppercase:(e.desc <> NullPtrLiteral) ?loc typ
    | None -> loc_fail loc "found an occurence of null without a type set"
    end;

  | UnresolvedConstruct {qual_type = q; args = args} | TemporaryObject {qual_type = q; args = args} ->
    let args = List.filter (fun (d : expr) -> match d.desc with | TemplateRef _ -> false | _ -> true) args in
    let tr_args = List.map tr_expr args in
    let f_name = qual_type_to_string q in
    trm_apps (trm_var (name_to_var f_name)) tr_args
  | ParenList el ->
    begin match el with
    | [e] -> tr_expr e
    | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: inheritance not yet supported."
    end

  | StmtExpr stmt -> tr_stmt stmt

  | Atomic { op = C11_atomic_fetch_add; args = [t_pointer; _t_mode; t_arg ] } ->
    let tr_args = List.map tr_expr [t_pointer; t_arg] in
    trm_apps (trm_binop typ_auto Binop_faa) tr_args

  | Atomic { op = C11_atomic_fetch_add; args } ->
    failwith "Clang_to_ast.ml.tr_expr: This atomic operation is not supported."

  | UnexposedExpr RecoveryExpr ->
    loc_fail loc "Clang_to_ast.tr_expr: parsing failure"

  | _ ->
    loc_fail loc
      ("Clang_to_ast.tr_expr: the following expression is unsupported: " ^
       Clang.Expr.show e)

(** [tr_attribute loc a]: translates an attribute [a] to an OptiTrust attribute *)
and tr_attribute (loc : location) (a : Clang.Ast.attribute) : attribute =
  match a.desc with
  | Aligned {spelling = _; alignment_expr = e} -> Alignas (tr_expr e)
  | _ -> loc_fail loc "Clang_to_astRawC.tr_attribute: unsupported attribute"


(** [tr_decl_list dl]: translates a list of declarations *)
and tr_decl_list (dl : decl list) : trms =
  match dl with
  | {decoration = _; desc = RecordDecl {keyword = k; attributes = _;
                                        nested_name_specifier = _; name = rn;
                                        bases = _; fields = fl; final = _;
                                        complete_definition = _; _}} as record_node ::
    {decoration = _; desc = TypedefDecl {name = tn; underlying_type = _q}} ::
    dl' when rn = "" || rn = tn ->
    (* typedef struct rn { int x, y; } tn; construction is encoded weirdly as two consecutive decls. *)
    let loc = loc_of_node record_node in
    begin match k with
      | Struct ->
        (* parse the fields names and types *)
        let prod_list = List.map ( fun (d : decl) ->
          let loc = loc_of_node d in
          match d with
          | {decoration = _; desc = Field {name = fn; qual_type = q;
                                              bitwidth = _; init = _;
                                              attributes = al}} ->
            let ft = tr_qual_type ?loc q in
            let al = List.map (tr_attribute loc) al in
            let ty = {ft with annot = { ft.annot with trm_annot_attributes = al } } in
            (Record_field (fn, ty), Access_unspecified)
          | _ ->
            Tools.debug "Failing from here";
            loc_fail loc "Clang_to_astRawC.tr_decl_list: only fields are allowed in struct declaration"

        ) fl in
        (* Third, add the typedef to the context *)
        let is_rec_type = if rn = "" then false else true in
        let td = {
          typedef_name = name_to_typvar tn;
          typedef_body = Typedef_record prod_list
        } in
        let trm_td = trm_typedef ?loc td in
        let trm_td = if is_rec_type then trm_add_cstyle Rec_struct trm_td else trm_td in
        let tl' = tr_decl_list dl' in
        trm_td :: tl'

      | _ -> loc_fail loc "Clang_to_astRawC.tr_decl_list: only struct records are allowed"
    end
  | d :: dl ->
    begin try
      let td = tr_decl d in
      td :: tr_decl_list dl
    with RedundantTemplateDefinition ->
      tr_decl_list dl
    end
  | [] -> []

(** [tr_member_initialized_list ?loc init_list]: translates class member initializer lists. *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_member_initialized_list ?(loc : location) (init_list :  constructor_initializer list) : trms =
  List.map (fun {kind = k; init = ie} ->
    match k with
    | Member {indirect = b; field = {desc = f}} ->
      let ti = tr_expr ie in
      trm_add_cstyle Member_initializer (trm_set (trm_struct_get ~struct_typ:typ_auto (trm_this ()) f) ti)
    | _ -> loc_fail loc "Clang_to_astRawC.tr_member_initializer_list: only simple member initializers are supported."
  ) init_list

and tr_function_decl ?loc ({linkage = _; function_type = t; nested_name_specifier = nns;
              name = n; body = bo; deleted = _; constexpr = _; _}: function_decl): trm =
  let qpath = tr_nested_name_specifier ?loc nns in
  let s =
    begin match n with
      | IdentifierName s -> s
      | OperatorName op -> string_of_overloaded_op ?loc op
      | _ ->
        loc_fail loc "Clang_to_astRawC.tr_decl: only identifiers and overloaded operators allowed for functions"
    end
  in
  let fn_var = name_to_var ~namespaces:qpath s in

  redundant_template_definition_type := false;
  let tt = tr_type_desc ?loc (FunctionType t) in
  if !redundant_template_definition_type then raise RedundantTemplateDefinition;

  match bo with
  | None -> trm_predecl ?loc (fn_var, tt)
  | Some s ->
    match typ_fun_inv tt with
    | Some (args_t, out_t) ->
      let {calling_conv = _; result = _; parameters = po;
          exception_spec = _; _} = t in
      let params = match po with
        | None -> []
        | Some { non_variadic; _ } -> non_variadic
      in
      let args = List.combine (List.map
        (fun {decoration = _; desc = {qual_type = _; name = n; default = _}} -> if n = "" then new_var "" else name_to_var n)
        params) args_t
      in
      List.iter (fun (y, ty) -> ctx_var_add y ty) args;
      let tb = tr_stmt s in
      trm_let_fun ?loc fn_var out_t args tb
    | None -> loc_fail loc "Clang_to_astRawC.tr_function_decl: should not happen"

(** [tr_decl d]: translates declaration [d] from clang to OptiTrust ast. *)
and tr_decl ?(in_class_decl : bool = false) (d : decl) : trm =
  let loc = loc_of_node d in
  let res = match d.desc with
  | EnumDecl {name = tn; constants; _} ->
    let enum_constant_l =
      List.map
        (fun ({desc = {constant_name; constant_init}; _} : enum_constant) ->
           match constant_init with
           | None -> ((name_to_var constant_name, None) : var * (trm option))
           | Some e ->
             let t_init = tr_expr e in
             (name_to_var constant_name, Some t_init)
        )
        constants
    in
    let td = {
      typedef_name = name_to_typvar tn;
      typedef_body = Typedef_enum enum_constant_l
    } in
    trm_typedef ?loc td

  | Function fun_decl ->
    tr_function_decl ?loc fun_decl
    (* DEPREACTED let res =trm_add_cstyle_clang_cursor (cursor_of_node d) res *)

  | CXXMethod {function_decl; static; const; _} ->
    let res = tr_function_decl ?loc function_decl in
    let res = trm_add_cstyle Method res in
    (* DEPREACTED let res = trm_add_cstyle_clang_cursor (cursor_of_node d) in *)
    if static
      then trm_add_cstyle Static_fun res
      else if const then trm_add_cstyle Const_method res
      else res

  | Constructor { class_name = cn; parameters = {non_variadic = pl; variadic = false}; initializer_list; body; implicit; explicit; defaulted; _} ->
    let class_name = Tools.clean_class_name cn in
    let qpath = if in_class_decl then [] else [cn] in
    let args = List.map (fun {decoration = _;
       desc = {qual_type = q; name = n; default = _}} -> (name_to_var n,tr_qual_type ?loc q)) pl in
    let fun_name = name_to_var ~namespaces:qpath class_name in
    let res = match body with
      | None -> trm_predecl (fun_name, typ_fun (List.map snd args) typ_unit)
      | Some s ->
        let tb = tr_stmt s in
        let tb =
          if List.length initializer_list = 0
            then tb
            else
              let t_il = tr_member_initialized_list ?loc initializer_list in
              insert_at_top_of_seq t_il tb
        in
        trm_let_fun ?loc (name_to_var ~namespaces:qpath class_name) typ_unit args tb
    in
    (* DEPREACTED trm_add_cstyle_clang_cursor (cursor_of_node d) res *)
    (* FIXME: Not all of those flags are necessarily exclusive *)
    if implicit
     then trm_add_cstyle (Class_constructor Constructor_implicit) res
     else if explicit then trm_add_cstyle (Class_constructor Constructor_explicit) res
     else if defaulted then trm_add_cstyle (Class_constructor Constructor_default) res
     else trm_add_cstyle (Class_constructor Constructor_simpl) res

  | Destructor {class_name = cn; body; defaulted; deleted; _} ->
    let class_name = Tools.clean_class_name cn in
    let fun_name = name_to_var class_name in
    let res = match body with
    | None -> trm_predecl ?loc (fun_name, typ_fun [] typ_unit)
    | Some s ->
      let tb = tr_stmt s in
      trm_let_fun ?loc fun_name typ_unit [] tb
    in
    (* DEPREACTED let res =trm_add_cstyle_clang_cursor (cursor_of_node d) in*)
    if defaulted
      then trm_add_cstyle (Class_destructor Destructor_default) res
      else if deleted then trm_add_cstyle (Class_destructor Destructor_delete) res
      else trm_add_cstyle (Class_destructor Destructor_simpl) res

  | Var {linkage = _; storage; var_name; var_type; var_init; constexpr = _; _} ->
    let tt = tr_qual_type ?loc var_type in
    let v = name_to_var var_name in
    ctx_var_add v tt;
    begin match storage, var_init with
    | Extern, None -> trm_predecl ?loc (v, tt)
    | _ ->
      let te = match var_init with
        | None -> trm_uninitialized
        | Some e -> tr_expr e
      in
      trm_let ?loc (v,tt) te
    end
  | TypedefDecl {name = tn; underlying_type = q} ->
    let tq = tr_qual_type ?loc q in
    let td = {
      typedef_name = name_to_typvar tn;
      typedef_body = Typedef_alias tq
    }
    in
    trm_typedef ?loc td
  | TypeAlias {ident_ref = id; qual_type = q} ->
    begin match id.name with
      | IdentifierName tn ->
        let tq = tr_qual_type ?loc q in
        let td = {
          typedef_name = name_to_typvar tn;
          typedef_body = Typedef_alias tq;} in
        trm_typedef ?loc td
      | _ -> loc_fail loc "Clang_to_astRawC.tr_decl: only identifiers allowed for type aliases"
    end
  | LinkageSpec {language = lang;decls = dl} ->
    let dls = tr_decl_list dl in
    let lang = match lang with
    | C -> "C"
    | CXX -> "C++"
    | _ -> "" in
    trm_extern lang dls
  | RecordDecl {keyword = k; attributes = _;
                nested_name_specifier = _; name = rn;
                bases = _; fields = fl; final = _;
                complete_definition = _;_ } ->
    let in_class_decl = true in
    let def_access = match k with
      | Struct -> Access_public
      | _ -> Access_unspecified
    in
    let access_spec = ref def_access in
    let prod_list = List.fold_left (fun acc (d : decl) ->
      let loc = loc_of_node d in
      match d with
      | {decoration = _; desc = Field {name = fn; qual_type = q; attributes = al;_}} ->
        let ft = tr_qual_type ?loc q in
        let al = List.map (tr_attribute loc) al in
        let ty = { ft with annot = { ft.annot with trm_annot_attributes = al } } in
        acc @ [(Record_field (fn, ty), !access_spec)]
      | {decoration = _; desc = CXXMethod _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_method tdl, !access_spec)]
      | {decoration = _; desc = Constructor _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_method tdl, !access_spec)]
      | {decoration = _; desc = Destructor _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_method tdl, !access_spec)]
      | {decoration = _; desc = AccessSpecifier (spec); _} ->
        begin match spec with
        | CXXPublic -> access_spec := Access_public; acc
        | CXXPrivate -> access_spec := Access_private; acc
        | CXXProtected -> access_spec := Access_protected; acc
        | _ -> loc_fail loc "Clang_to_astRawC.tr_decl_list: unkwown access specifier"
        end
      | _ -> Tools.debug "Failing from here";
        loc_fail loc "Clang_to_astRawC.tr_decl_list: only fields are allowed in record declaration"
    ) [] fl in
    let td = {
      typedef_name = name_to_typvar rn;
      typedef_body = Typedef_record prod_list
    } in

    begin match k with
    | Struct -> trm_add_cstyle Struct (trm_typedef ?loc td)
    | Class -> trm_add_cstyle Class (trm_typedef ?loc td)
    | _ -> loc_fail loc "Clang_to_astRawC.tr_decl_list: only classes and structs are supported."
    end

  | Namespace {name = n; declarations = dl; inline = b} ->
    let dls = tr_decl_list dl in
    trm_namespace n (trm_seq_nomarks dls) b

  | TemplateDecl {parameters = {list = pl;_}; decl = d} ->
    let dl = tr_decl d in
    let pl = List.map (fun {decoration = _;desc = {parameter_name = n; parameter_kind = pk; parameter_pack};_} ->
      let n, tpk = match pk with
        | Class {default = opt_q} ->
          (name_to_typvar n, Typename (Option.map (tr_qual_type ?loc) opt_q))
        | NonType {parameter_type = q; default = opt_expr} ->
          (name_to_var n, NonType (tr_qual_type ?loc q, Option.map tr_expr opt_expr))
        | Template _ -> loc_fail loc "Clang_to_astRawC.tr_decl: nested templates are not supported" (* LATER: Add support for nested templates *)
      in
      if parameter_pack then loc_fail loc "Clang_to_astRawC.tr_decl: parameter packs are not supported";
      (n, tpk)
    ) pl in
    trm_template pl dl
  | UsingDirective {nested_name_specifier = _; namespace = ns} ->
    let tr_n = tr_decl ns in
    begin match tr_n.desc with
    | Trm_namespace (nm, _, _) -> trm_using_directive nm
    | _ -> loc_fail loc "Clan_to_astRawC.tr_decl: using direcitves can be used only with namespaces"
    end
  | _ -> loc_fail loc "Clang_to_astRawC.tr_decl: not implemented" in
     res

(* LATER: We should find a way to capture the tree of includes instead and make this structure recursive *)
(* We need to group toplevel decls because of a weird encoding of typedef struct. (see tr_decl_list) *)
type file_decl =
  | ToplevelDecls of decl list
  | IncludedFile of string * decl list

let tr_file_decl (fd: file_decl): trm list =
  match fd with
  | ToplevelDecls d -> tr_decl_list d
  | IncludedFile (file, dl) ->
    [trm_set_include file (trm_seq_nomarks (tr_decl_list dl))]

(** [group_and_filter_include filename dl]: group declarations from whitelisted include directives and filters out the rest *)
(* TODO: Use a blacklist instead would be better but it requires to rebuild the include tree *)
let group_and_filter_include (filename : string) (dl : decl list) : file_decl list =
  List.fold_right (fun d acc ->
      let file = file_of_node d in
      if file = filename then
        match acc with
        | ToplevelDecls dl :: tl ->
          ToplevelDecls (d :: dl) :: tl
        | _ ->
          ToplevelDecls [d] :: acc
      else
        match acc with
        | IncludedFile (last_incl_file, decls) :: tl when file = last_incl_file ->
          IncludedFile (file, d :: decls) :: tl
        | _ ->
          let is_in_current_dir = Filename.dirname file = Filename.dirname filename in
          let basename = Filename.basename file in
          let is_whitelisted = List.exists ((=) basename) ["optitrust.h"; "optitrust_models.h"; "optitrust_common.h"] in
          if is_in_current_dir || is_whitelisted then
            IncludedFile (file, [d]) :: acc
          else
            acc
  ) dl []

(** [tr_ast t]: transalate [t] into OptiTrust AST *)
let tr_ast (t : translation_unit) : trm =
  ctx_reset ();
  (* Initialize id_counter *)
  let {decoration = _; desc = {filename = filename; items = dl}} = t in
  verbose_info "tr_ast: translating %s's AST..." filename;
  let file_decls = group_and_filter_include filename dl in
  begin match !dump_clang_ast with
  | None -> ()
  | Some dump_clang_file ->
      let out_ast = open_out dump_clang_file in
      List.iter (fun fd -> match fd with
        | ToplevelDecls d -> List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d)) dl;
        | IncludedFile (file, dl) ->
          Printf.fprintf out_ast "(* Include %s: *)\n" file;
          List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d)) dl;
          Printf.fprintf out_ast "\n"
        )
        file_decls;
      close_out out_ast;
  end;
  let loc = loc_of_node t in
  trm_set_mainfile (trm_seq_nomarks ?loc (List.concat_map tr_file_decl file_decls))
