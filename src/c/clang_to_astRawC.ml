open Clang.Ast
open Clang.Bindings
open Ast
open Trm
open Typ
open Mark
open Tools

let warn_array_subscript_not_supported (t : typ option) : unit =
  let str = Xoption.to_string AstC_to_c.typ_to_string t in
  if !Flags.report_all_warnings
    && not (String_set.mem str !Flags.warned_array_subscript_not_supported) then begin
    Flags.warned_array_subscript_not_supported := String_set.add str !Flags.warned_array_subscript_not_supported;
    Tools.warn "does not support array subscript base type '%s'" str;
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
let ctx_tconstr : typconstrid Qualified_map.t ref = ref Qualified_map.empty
let ctx_typedef : typedef typmap ref = ref Typ_map.empty
let ctx_label : typconstrid labelmap ref = ref String_map.empty
let ctx_constr : typconstrid constrnamemap ref = ref String_map.empty

let ctx_reset () : unit =
  ctx_var := Qualified_map.empty;
  ctx_tconstr := Qualified_map.empty;
  ctx_typedef := Typ_map.empty;
  ctx_label := String_map.empty;
  ctx_constr := String_map.empty

(** [debug_typedefs]: flag for debugging typedefs *)
let debug_typedefs = false

(** [ctx_var_add tv]: adds variable [v] with type [t] in map [ctx_var] *)
let ctx_var_add (tv : var) (t : typ) : unit =
  ctx_var := Qualified_map.add (tv.namespaces, tv.name) t (!ctx_var)

(** [ctx_tconstr_add tn tid]: adds constructed type [tv] with id [tid] in map [ctx_tconstr] *)
let ctx_tconstr_add (tn : typconstr) (tid : typconstrid) : unit =
  if debug_typedefs then Tools.debug "Type %s has been added into map with typconstrid %d" (Tools.document_to_string (Ast_to_text.print_typconstr tn)) tid;
  ctx_tconstr := Qualified_map.add tn tid (!ctx_tconstr)

(** [ctx_typedef_add tn tid td]: adds typedef [td] with id [tid] in map [ctx_typedef] *)
let ctx_typedef_add (tn : typconstr) (tid : typconstrid) (td : typedef) : unit =
  if debug_typedefs then Tools.debug "Typedef for %s has been registered" (Tools.document_to_string (Ast_to_text.print_typconstr tn));
  ctx_typedef := Typ_map.add tid td (!ctx_typedef)

(** [ctx_label_add lb tid]: adds label [lb] with id [tid] in map [ctx_label] *)
let ctx_label_add (lb : label) (tid : typconstrid) : unit =
  ctx_label := String_map.add lb tid (!ctx_label)

(** [ctx_constr_add c tid]: adds constr [c] with id [tid] in map [ctx_constr_add] *)
let ctx_constr_add (c : constrname) (tid : typconstrid) : unit =
  ctx_constr := String_map.add c tid (!ctx_constr)

(** [get_ctx]: gets the current context *)
let get_ctx () : ctx =
  (* FIXME: #var-id , ids are dummy wich breaks all varmaps. *)
  typing_ctx {
    ctx_var = !ctx_var;
    ctx_tconstr = !ctx_tconstr;
    ctx_typedef = !ctx_typedef;
    ctx_label = !ctx_label;
    ctx_constr = !ctx_constr;
  }

(* CHECK: #type-id *)
let name_to_typconstr ?(namespaces = []) (n : string) : typconstr =
  [], n

(** [get_typid_for_type ty]: gets the type id for type [tv]*)
let get_typid_for_type (namespaces : string list) (name : string) : int  =
   let tid = Qualified_map.find_opt (namespaces, name) !ctx_tconstr in
   begin match tid with
   | Some id -> id
   | None -> -1
   end

(** [string_of_overloaded_op ?loc op]: gets names of overloaded operators (later matched for printing) *)
 let string_of_overloaded_op ?(loc : location)
    (op : clang_ext_overloadedoperatorkind) : string =
  match op with
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

(** [overload_op ?loc ?ctx op]: gets the primitive operation associated with the overloaded operator [op] *)
 let overloaded_op ?(loc : location) ?(ctx : ctx option) (op : clang_ext_overloadedoperatorkind) : trm =
  match op with
  | Plus -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_add))
  | Minus -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_sub))
  | Star -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_mul))
  | Equal -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_set))
  | ExclaimEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_neq))
  | PlusEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_add))
  | MinusEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_sub))
  | StarEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_mul))
  | Slash -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_div))
  | Percent -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_mod))
  | Amp -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_bitwise_and))
  | Pipe -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_bitwise_or))
  | Less -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_lt))
  | Greater -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_gt))
  | SlashEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_div))
  | PercentEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_mod))
  | AmpEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_and))
  | PipeEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_or))
  | LessLess -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_shiftl))
  | GreaterGreater -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_shiftr))
  | LessLessEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_shiftl))
  | GreaterGreaterEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_compound_assgn_op Binop_shiftr))
  | EqualEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_eq))
  | LessEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_le))
  | GreaterEqual -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_ge))
  | AmpAmp -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_and))
  | PipePipe -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop Binop_or))
  | PlusPlus -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_unop (Unop_post_inc)))
  | Subscript -> trm_prim ?loc ?ctx (Prim_overloaded_op (Prim_binop (Binop_array_get)))
  | Call -> trm_var (name_to_var "overloaded_call")
  | _ -> loc_fail loc "Clang_to_astRawC.overloaded_op: non supported operator"

(** [wrap_const ~const t]: wrap type [t] into a const type if [const] is true *)
let wrap_const ?(const : bool = false) (t : typ) : typ =
  if const then typ_const t else t


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
    Pattern.(trm_lt (trm_var (eq loop_index)) !__) (fun n -> (DirUp, n));
    Pattern.(trm_le (trm_var (eq loop_index)) !__) (fun n -> (DirUpEq, n));
    Pattern.(trm_gt (trm_var (eq loop_index)) !__) (fun n -> (DirDown, n));
    Pattern.(trm_ge (trm_var (eq loop_index)) !__) (fun n -> (DirDownEq, n))
  ]

(** [trm_for_c_inv_simple_step step]: checks if the loop step is simple. If that's the case then return that step. *)
let trm_for_c_inv_simple_step (loop_index: var) (loop_dir: loop_dir) (step_expr : trm) : trm option =
  match loop_dir with
  | DirUp | DirUpEq ->
    (* Using eq instead of the more efficient var_eq because var ids are not computed at this step *)
    Pattern.pattern_match_opt step_expr [
      Pattern.(!(trm_unop (eq Unop_post_inc) (trm_var (eq loop_index)))) (fun _ -> trm_step_one_post ());
      Pattern.(!(trm_unop (eq Unop_pre_inc) (trm_var (eq loop_index)))) (fun _ -> trm_step_one_pre ());
      Pattern.(trm_prim_compound Binop_add (trm_var (eq loop_index)) !__) (fun x -> x);
    ]
  | DirDown | DirDownEq ->
    Pattern.pattern_match_opt step_expr [
      Pattern.(!(trm_unop (eq Unop_post_dec) (trm_var (eq loop_index)))) (fun _ -> trm_step_one_post ());
      Pattern.(!(trm_unop (eq Unop_pre_dec) (trm_var (eq loop_index)))) (fun _ -> trm_step_one_pre ());
      Pattern.(trm_prim_compound Binop_sub (trm_var (eq loop_index)) !__) (fun x -> x);
    ]

(** [trm_for_of_trm_for_c t]: checks if loops [t] is a simple loop or not, if yes then return the simple loop else returns [t]. *)
let trm_for_of_trm_for_c (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, cond, step, body, _) ->
    let open Xoption.OptionMonad in
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

(** [tr_type_desc ?loc ~const ~tr_record_types]: translates ClanML C/C++ type decriptions to OptiTrust type descriptions,
    [loc] gives the location of the type in the file that has been translated,
    if [const] is true then it means [d] is a const type, similarly if [const] is false then [d] is not a const type *)
(* FIXME: #odoc why is annotation required on callees? *)
let rec tr_type_desc ?(loc : location) ?(const : bool = false) ?(tr_record_types : bool = true) (d : type_desc) : typ =
  match d with
  | Pointer q ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    let ty = typ_ptr Ptr_kind_mut t in
    wrap_const ~const ty
  | LValueReference  q ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    wrap_const ~const (typ_ptr Ptr_kind_ref t)
  | RValueReference  q ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    wrap_const ~const (typ_ptr Ptr_kind_ref (typ_ptr Ptr_kind_ref t))
  | ConstantArray {element = q; size = n; size_as_expr = eo} ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    begin match eo with
      | None -> wrap_const ~const (typ_array t ~size:(trm_int n))
      | Some e ->
        let size = tr_expr e in
        wrap_const ~const (typ_array t ~size)
    end
  | VariableArray {element = q; size = eo} ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    let size = tr_expr eo in
    wrap_const ~const (typ_array t ~size)
  | IncompleteArray q ->
    let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types q in
    wrap_const ~const (typ_array t)
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
      | LongLong -> wrap_const ~const (typ_int ~annot:[Long; Long] ())
      | ULongLong -> wrap_const ~const (typ_int ~annot:[Unsigned; Long; Long] ())
      | Float -> wrap_const ~const (typ_float ())
      | Double -> wrap_const ~const (typ_double ())
      | LongDouble -> wrap_const ~const (typ_double ~annot:[Long] ())
      | Char_S -> wrap_const ~const (typ_char ())
      | UChar -> wrap_const ~const (typ_char ~annot:[Unsigned] ())
      | Short -> wrap_const ~const (typ_int ~annot:[Short] ())
      | UShort -> wrap_const ~const (typ_int ~annot:[Unsigned; Short] ())
      | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: builtin type not implemented"
    end
  | FunctionType {calling_conv = _; result = qr; parameters = po;
                  exception_spec = _; _} ->
    let tr = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc qr in
    begin match po with
      | None -> typ_fun [] tr
      | Some {non_variadic = pl; variadic = _} ->
        let tl =
          List.map
            (fun (p : parameter) -> (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc p.desc.qual_type)
            pl
        in
        typ_fun tl tr
    end
  | Typedef {nested_name_specifier = nns; name = n; _} ->
    begin match n with
      | IdentifierName n ->
        let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns  in
        let typ_to_add = typ_constr (qpath, n) ~tid:(get_typid_for_type qpath n)  in
        wrap_const ~const typ_to_add
      | _ -> loc_fail loc ("tr_type_desc: only identifiers are allowed in type definitions")
    end
  | Elaborated {keyword = k; nested_name_specifier = nns; named_type = q} ->
      let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
      begin match k with
      | Struct -> if tr_record_types then typ_record Struct ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q) else ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q)

      | Class -> if tr_record_types then typ_record Class ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q) else ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q)
      | NoKeyword -> let tr_ty = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) q in
        begin match tr_ty.typ_desc with
        | Typ_constr (qty, tid, tl) ->
          typ_constr ~annot:tr_ty.typ_annot ~attributes:tr_ty.typ_attributes ~tid ~tl (qpath, (snd qty))
        | _ -> tr_ty
        end
      | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: this elaborated type is not supported."
    end
  | Record {nested_name_specifier = nns; name = n; _} ->
    let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
    begin match n with
      | IdentifierName n ->
         typ_constr ~tid:(get_typid_for_type qpath n) (qpath, n)
      | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: only identifiers are allowed in records"
    end
  | Enum {nested_name_specifier = nns; name = n; _} ->
    let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
    begin match n with
      | IdentifierName n ->
         typ_constr ~tid:(get_typid_for_type qpath n) (qpath, n)
      | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: only identifiers are allowed in enums"
    end
  | TemplateTypeParm name ->
    wrap_const ~const (typ_template_param name)
  | TemplateSpecialization {name = NameTemplate nm; args = tyl } ->
    (* For the moment we don't have a way to create an id for builtin types *)
    let tl = (List.map (fun (targ : template_argument) ->
      match targ with
      | Type q -> (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q
      | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: only simple types are supported as template arguments") tyl) in
    (* temporary hack for printing <> *)
    let tl = if tl = [] then [typ_unit()] else tl in

    wrap_const ~const (typ_constr ([], nm) ~tid:(-1) ~tl)
  | Decltype e ->
    let tr_e = tr_expr e in
    wrap_const ~const (typ_decl tr_e)
  | SubstTemplateTypeParm tys ->
    redundant_template_definition_type := true;
    wrap_const ~const (typ_template_param tys)

  | InjectedClassName {desc = q_d; _} ->
    let tq = (tr_type_desc : ?loc:trm_loc -> ?const:bool -> ?tr_record_types:bool -> type_desc -> typ) ?loc q_d in
    let tq = typ_add_attribute Injected tq in
    wrap_const ~const tq (* @annot_injected *)

  | _ -> loc_fail loc "Clang_to_astRawC.tr_type_desc: not implemented"

(** [is_qual_type_const q]: checks if [q] is a const type or not *)
and is_qual_type_const (q : qual_type) : bool =
  let {const;_} = q in const

(** [tr_qual_type ?loc ~tr_record_types q]: translates  ClanML C/C++ types into OptiTrust types *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_qual_type ?(loc : location) ?(tr_record_types : bool = true) (q : qual_type) : typ =
  let ({desc = d; const = c; _} : qual_type) = q in
  (tr_type_desc : ?loc:trm_loc -> ?const:bool -> ?tr_record_types:bool -> type_desc -> typ) ?loc ~const:c ~tr_record_types d

(** [tr_nested_name_specifier ?loc name_specs]: converts Clang.name_specifier to a string list. *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_nested_name_specifier ?(loc : location) name_specs : string list =
  match name_specs with
  | Some nms ->
      List.map (fun name_spec ->
        match name_spec with
        | NamespaceName nm -> nm
        | TypeSpec q ->
          let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
          AstC_to_c.typ_to_string tq
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
  let ctx = get_ctx () in
  match s.desc with
  | Compound sl ->
    let tl = List.map tr_stmt sl in
    trm_seq_nomarks ?loc ~ctx tl
  | If {init = None; condition_variable = None; cond = c; then_branch = st;
        else_branch = seo} ->
    let tc = tr_expr c in
    let tt = tr_stmt st in
    begin match seo with
      | None -> trm_if ?loc ~ctx tc tt (trm_lit Lit_unit)
      | Some se ->
        let te = tr_stmt se in
        trm_if ?loc ~ctx tc tt te
    end
  | If _ ->
    (* Add support for this feature. *)
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in if conditions"
  | While {condition_variable = _; cond = c; body = s} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_while ?loc ~ctx tc ts
  | Do {body = s; cond = c;} ->
    let tc = tr_expr c in
    let ts = tr_stmt s in
    trm_do_while ?loc ~ctx ts tc
  | For {init = inito; condition_variable = None; cond = condo; inc = stepo;
         body} ->
    let tr_stmt_opt (so : stmt option) : trm =
      match so with
      | None -> trm_lit ?loc ~ctx  Lit_unit
      | Some s -> tr_stmt s
      in
    let init = tr_stmt_opt inito in
    let cond =
      match condo with
      (* no condition is equivalent to true *)
      | None -> trm_add_cstyle Empty_cond (trm_lit ?loc ~ctx (Lit_bool true))
      | Some e -> tr_expr e
    in
    let step = tr_stmt_opt stepo in
    let body = tr_stmt body in
    trm_for_of_trm_for_c (trm_for_c ?loc ~ctx init cond step body)
  | For _ ->
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in for conditions"
  | Return eo ->
    begin match eo with
      | None -> trm_abort ?loc ~ctx (Ret None)
      | Some e ->
        let t = tr_expr e in
        trm_abort ?loc ~ctx (Ret (Some t))
    end
  | Break -> trm_abort ?loc ~ctx (Break None)
  | Continue -> trm_abort ?loc ~ctx (Continue None)
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
       trm_let_mult ?loc ~ctx bindings
    end
  | Expr e ->
    tr_expr e
  | Label {label = l; body = s} ->
    let t = tr_stmt s in
    trm_add_label l t
  | Null -> trm_lit ?loc ~ctx Lit_unit
  | Switch {init = None; condition_variable = None; cond = c; body = s} ->
    begin match s.desc with
      | Compound sl -> tr_switch loc c sl
      | _ -> loc_fail loc "Clang_to_astRawC.tr_stmt: switch cases must be in a compound statement"
    end
  | Switch _ ->
    loc_fail loc "Clang_to_astRawC.tr_stmt: variable declaration forbidden in switch conditions"
  | Goto l -> trm_goto ?loc ~ctx l
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
  trm_switch ?loc ~ctx:(get_ctx ()) t  (aux loc cases)

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
          | tl -> Nobrace.trm_add_style (trm_seq_nomarks ?loc ~ctx:(get_ctx ()) tl), sl
        end
      | _ ->
        let t = tr_stmt s in
        compute_body loc (t :: body_acc) sl
    end


(** [tr_init_list ?loc ~ctx ty el]: translates [el] into a trm, based on type [ty] the initialization list kind
     is determined. *)
(* FIXME: #odoc why is annotation required on callees? *)
and tr_init_list ?(loc : location) ~(ctx : ctx) (ty : typ) (el : expr list) : trm =
  match get_typ_kind (Option.get (get_ctx ()).ctx_types) ty with
  | Typ_kind_array ->
     let tl = List.map tr_expr el in
     trm_array ?loc ~ctx ~typ:ty (Mlist.of_list tl)
  | Typ_kind_record | Typ_kind_undefined ->
     let tl = List.map (fun (e : expr) ->
      match e.desc with
      | DesignatedInit {designators = dl; init = e } ->
        begin match dl with
        | [FieldDesignator f] ->
          (Some f, tr_expr e)
        | _ -> loc_fail loc "Clang_to_astRawC.tr_init_list: struct initialization with multiple designators per field are not supported"
        end
      | _ -> (None, tr_expr e)) el in
        trm_record ?loc ~ctx ~typ:ty (Mlist.of_list tl)
  | _ ->
    let tl = List.map tr_expr el in
    trm_add_cstyle Brace_init (trm_array ?loc ~ctx ~typ:ty (Mlist.of_list tl))
  (* | _ -> loc_fail loc "Clang_to_astRawC.tr_init_list: initialisation lists only allowed for struct and array" *)

(** [tr_expr e]: translates expression [e] into an OptiTrust trm *)
and tr_expr (e : expr) : trm =
  (* let aux = tr_expr *)
  let loc = loc_of_node e in
  let typ : typ option =
    let q = Clang.Type.of_node e in
    try Some ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q) with
    | _ ->
      print_info loc "tr_expr: unable to translate type %s\n"
        (Clang.Type.show q);
      None
  in
  let ctx = get_ctx() in
  match e.desc with
  | ConditionalOperator {cond; then_branch = Some e_then;
                         else_branch = e_else} ->
    let t_cond = tr_expr cond in
    let t_then = tr_expr e_then in
    let t_else = tr_expr e_else in
    trm_apps ?loc ?typ ~ctx (trm_prim ?loc ~ctx Prim_conditional_op)
      [t_cond; t_then; t_else]
  | ConditionalOperator _ ->
    loc_fail loc
      "tr_expr: conditional operators without then branch unsupported"
  | CompoundLiteral {init = init_expr; qual_type = _q} -> tr_expr init_expr
  | IntegerLiteral i ->
    begin match i with
      | Int i -> trm_lit ~typ ?loc ~ctx (Lit_int i)
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: only int literal allowed"
    end
  | BoolLiteral b -> trm_lit ~typ ?loc ~ctx (Lit_bool b)
  | FloatingLiteral f ->
    begin match f with
      | Float f -> trm_lit ~typ ?loc ~ctx (Lit_double f)
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: only float literal allowed"
    end
  | StringLiteral {byte_width = _; bytes = s; string_kind = _} ->
    trm_lit ~typ ?loc ~ctx (Lit_string s)


  | InitList el ->
    (* maybe typ is already the value of tt ---let tt = tr_qual_type ?loc t in *)
    let tt = match typ with
      | None -> loc_fail loc ("unable to obtain type of an initialization list")
      | Some ty -> ty
    in
    (tr_init_list : ?loc:trm_loc -> ctx:ctx -> typ -> Clang.Ast.expr list -> trm) ?loc ~ctx tt el

  | UnaryExpr {kind = k; argument = a} ->
    begin match k with
      | SizeOf ->
        begin match a with
          | ArgumentExpr e ->
            let t = tr_expr e in
            trm_apps ?loc ?typ ~ctx (trm_var ?loc (name_to_var "sizeof")) [t]
          | ArgumentType q ->
            let ty = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) q in
            trm_var ?loc ?typ ~ctx (name_to_var ("sizeof(" ^ AstC_to_c.typ_to_string ty ^ ")"))
        end
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: unsupported unary expr"
    end
  | UnaryOperator {kind = k; operand = e} ->
    let loc = loc_from_to loc (loc_of_node e) in
    let t = tr_expr e in
    begin match k with
      | AddrOf -> (* expectation: e is not a const variable *)
        (* We are translating a term of the form that involves [&p],
           for example, [int p = 3; f(&p)]. In our AST, [p] represents
           the address of the cell at which [3] is stored, thus the
           call is actually [f(p)]. In other words we drop the [&] operator. *)
        trm_apps ?loc ?typ ~ctx (trm_unop Unop_address) [t]
      | _ ->
        let trm_apps1 unop t1 = trm_apps ?loc ?typ ~ctx (trm_unop ?loc unop) [t1] in
        begin match k with
          (* TODO: use the smart constructors instead; like trm_minus *)
          | PostInc -> trm_apps1 Unop_post_inc t
          | PostDec -> trm_apps1 Unop_post_dec t
          | PreInc -> trm_apps1 Unop_pre_inc t
          | PreDec -> trm_apps1 Unop_pre_dec t
          | Deref -> trm_apps1 Unop_get t
          | Minus -> trm_minus ?loc ?typ ~ctx t  (* DEPRECATED trm_apps1 Unop_minus t *)
          | Plus -> trm_apps1 Unop_plus t
          | Not -> trm_apps1 Unop_bitwise_neg t
          | LNot -> trm_apps1 Unop_neg t
          | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: unary operator not implemented"
        end
    end
  | BinaryOperator {lhs = le; kind = k; rhs = re} ->
    let loc = loc_from_to (loc_of_node le) (loc_of_node re) in
    let tr = tr_expr re in
    let tl = tr_expr le in
    let trm_prim_c binop tl tr =
       trm_prim_compound ?loc ~ctx binop tl tr in
    begin match k with
      | Assign ->
        trm_set ?loc ~ctx tl tr
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
          | Mul -> trm_mul ?loc ~ctx ?typ tl tr
          | Div -> trm_div ?loc ~ctx ?typ tl tr
          | Add -> trm_add ?loc ~ctx ?typ tl tr
          | Sub -> trm_sub ?loc ~ctx ?typ tl tr
          | LT ->  trm_lt ?loc ~ctx ?typ tl tr
          | GT ->  trm_gt ?loc ~ctx ?typ tl tr
          | LE ->  trm_le ?loc ~ctx ?typ tl tr
          | GE ->  trm_ge ?loc ~ctx ?typ tl tr
          | EQ ->  trm_eq ?loc ~ctx ?typ tl tr
          | NE ->  trm_neq ?loc ~ctx ?typ tl tr
          | And -> trm_bit_and ?loc ~ctx ?typ tl tr
          | LAnd -> trm_and ?loc ~ctx ?typ tl tr
          | Or ->  trm_bit_or ?loc ~ctx ?typ tl tr
          | LOr -> trm_or ?loc ~ctx ?typ tl tr
          | Shl -> trm_shiftl ?loc ~ctx ?typ tl tr
          | Shr -> trm_shiftr ?loc ~ctx ?typ tl tr
          | Rem -> trm_mod ?loc ~ctx ?typ tl tr
          | Xor -> trm_xor ?loc ~ctx ?typ tl tr
          | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: binary operator not implemented"
        end
    end
  | Call {callee = f; args = el} ->
    let tf = tr_expr f in
    (* DEPREACTED let tf = trm_add_cstyle_clang_cursor (cursor_of_node f) tf in*)
    begin match tf.desc with
    | Trm_var x when var_has_name x "exact_div" ->
      begin match List.map tr_expr el with
      | [n; b] ->
        trm_apps ?loc ~ctx ?typ (trm_binop Binop_exact_div) [n; b]
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: 'exact_div' expects two arguments"
      end
    | Trm_var x when Str.string_match (Str.regexp "overloaded=") x.name 0 ->
        begin match el with
        | [tl;tr] -> trm_set ?loc ~ctx (tr_expr tl) (tr_expr tr)
        | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: overloaded= expects two arguments"
        end
    | Trm_var x when var_has_name x "overloaded_call" ->
      let t_args = List.map tr_expr el in
      let call_name , call_args = Xlist.uncons t_args in
      trm_apps ?loc ~ctx ?typ call_name call_args
    | _->
      trm_apps ?loc ~ctx ?typ tf (List.map tr_expr el)
    end
  | DeclRef {nested_name_specifier = nns; name = n; template_arguments = targs} -> (* Occurrence of a variable *)
    begin match n with
      | IdentifierName s ->
        (*
          particular case for variables:
          we look at the type given in their declaration to take into account
          type aliases
         *)
        let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
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
        let res = trm_var ?loc ~ctx ?typ (name_to_var ~namespaces:qpath s) in
        (* DEPRECATED? get cursor info with locations of variables
        let unified_symbol_resolution1 = Clang.get_cursor_usr (cursor_of_node e) in
        let unified_symbol_resolution2 = Clang.get_cursor_usr (Clang.get_cursor_definition (cursor_of_node e)) in
        printf "%s ---- %s\n" unified_symbol_resolution1 unified_symbol_resolution2;
        *)
        let targs = List.map (fun (targ : template_argument) -> begin match targ with | Type q -> (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: something went wrong." end) targs in
        begin match targs with
        | [] -> res
        | _ -> trm_add_cstyle (Typ_arguments targs) res
        end
      | OperatorName op -> overloaded_op ?loc ~ctx  op
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
            trm_apps ?loc ~ctx ?typ (trm_unop (Unop_struct_get f)) [t_this]
          | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accesses by names"
          end
        else loc_fail loc "Clang_to_astRawC.tr_expr: field accesses should have a base"
    | Some e ->
      let f =
      begin match f with
      | FieldName id -> tr_ident id
      | DependentScopeMember {ident_ref = id; template_arguments = _} ->
        begin match id.name with
        | IdentifierName f -> f
        | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accessed by names"
        end
      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: fields should be accessed by names"
      end in
      let base = tr_expr e in
        if has_arrow then
          trm_apps ?loc ~ctx ?typ (trm_unop (Unop_struct_get f) ) [trm_get base]
        else
          let get_op = trm_unop ?loc (Unop_struct_get f) in
          let get_op = if is_get_operation base then trm_add_cstyle Display_no_arrow get_op else get_op in
          trm_apps ?loc ~ctx ?typ get_op [base]
    end
  | ArraySubscript {base = e; index = i} ->
    let ti = tr_expr i in
    let te = tr_expr e in
    (*
       override typ to account for typedefs:
       if e's type is x*, make sure typ is x (it is not always the case if x is
       declared through a typedef)
      *)
    let typ = Option.bind te.typ typ_of_get in
    if typ = None then begin
      (* happens for, e.g., typedef T = int*; *)
      warn_array_subscript_not_supported te.typ;
    end;
    trm_apps ?loc ~ctx ?typ (trm_binop ?loc ~ctx (Binop_array_get)) [te; ti]

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
        let t = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
        let te' = tr_expr e' in
        trm_apps ?loc ~ctx ?typ (trm_unop ?loc ~ctx (Unop_cast t)) [te']

      | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: only static casts are allowed"
    end
  | New {placement_args = _; qual_type = q; array_size = seo; init = _} ->
    let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
    begin match seo with
      | None -> trm_prim ?loc ~ctx (Prim_new tq)
      | Some se ->
        let size = tr_expr se in
        trm_prim ?loc ~ctx (Prim_new (typ_array tq ~size))
    end
  | Delete {global_delete = _; array_form = b; argument = e} ->
    let te = tr_expr e in
    trm_delete ?loc ~ctx b te
  | Lambda {capture_default = ByRef; captures = _; is_mutable = _; parameters = po; result_type = rt; body = b} ->
    let tt = begin match rt with | Some ty -> Some ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ty ) | None -> None end in
    let tb = tr_stmt b in
    let pl = match po with | Some pl -> pl | None -> [] in
    let args = List.map (fun {decoration = _;
      desc = {qual_type = q; name = n; default = _}} -> (name_to_var n, (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q)) pl in
    List.iter (fun (y, ty) -> ctx_var_add y ty) args;
    trm_fun args tt tb
  | This -> trm_this ?loc ?typ ~ctx ()
  | UnexposedExpr ImplicitValueInitExpr ->
    print_info loc "tr_expr: implicit initial value\n";
    trm_lit ?loc ~ctx Lit_uninitialized
  | UnknownExpr (CompoundLiteralExpr, CompoundLiteralExpr) ->
      Tools.warn "Unknown expressions are parsed as null pointers";
      trm_add_mark "unknown_expr" (trm_null ?loc ~ctx () )
  | ImplicitValueInit _ -> trm_lit ?loc ~ctx Lit_uninitialized

  (* TODO: clean up and use trm_null everywhere;
     TODO: check whether UnknownExpr (GNUNullExpr, GNUNullExpr) is actually used sometimes *)
  | UnknownExpr (GNUNullExpr, GNUNullExpr) -> trm_null ~uppercase:true ?loc ~ctx () (* sometimes Null is translated like this *) (* LATER: in which condition? *)
  | NullPtrLiteral -> trm_null ?loc ~ctx ()
  | UnexposedExpr (GNUNullExpr) -> trm_null ~uppercase:true ?loc ~ctx ()

    (* TODO: use Lit_nullptr, and use annotation to know if should print NULL or nullptr *)

  | UnresolvedConstruct {qual_type = q; args = args} | TemporaryObject {qual_type = q; args = args} ->
    let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) q in
    let args = List.filter (fun (d : expr) -> match d.desc with | TemplateRef _ -> false | _ -> true) args in
    let tr_args = List.map tr_expr args in
    let f_name = AstC_to_c.typ_to_string tq  in
    trm_apps (trm_var (name_to_var f_name)) tr_args
  | ParenList el ->
    begin match el with
    | [e] -> tr_expr e
    | _ -> loc_fail loc "Clang_to_astRawC.tr_expr: inheritance not yet supported."
    end

  | UnexposedExpr RecoveryExpr ->
    loc_fail loc "Clang_to_astRawC.tr_expr: parsing failure"

  | _ ->
    loc_fail loc
      ("Clang_to_astRawC.tr_expr: the following expression is unsupported: " ^
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
        (* First add the constructor name to the context, needed for recursive types *)
        let tc = name_to_typconstr tn in
        let tid = next_typconstrid () in
        ctx_tconstr_add tc tid;

        (* Second, parse the fields names and types *)
        let prod_list = List.map ( fun (d : decl) ->
          let loc = loc_of_node d in
          match d with
          | {decoration = _; desc = Field {name = fn; qual_type = q;
                                              bitwidth = _; init = _;
                                              attributes = al}} ->
            ctx_label_add fn tid;
            let ft = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
            let al = List.map (tr_attribute loc) al in
            let ty = {ft with typ_attributes = al} in
            (Record_field_member (fn, ty), Access_unspecified)
          | _ ->
            Tools.debug "Failing from here";
            loc_fail loc "Clang_to_astRawC.tr_decl_list: only fields are allowed in struct declaration"

        ) fl in
        (* Third, add the typedef to the context *)
        let is_rec_type = if rn = "" then false else true in
        let td = {
          typdef_loc = loc;
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_record prod_list
          } in
        ctx_typedef_add tc tid td;
        let trm_td = trm_typedef ?loc ~ctx:(get_ctx ()) td in
        let trm_td = if is_rec_type then trm_add_cstyle Is_rec_struct trm_td else trm_td in
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
  let ctx = get_ctx () in
  List.map (fun {kind = k; init = ie} ->
    match k with
    | Member {indirect = b; field = {desc = f}} ->
      let ti = tr_expr ie in
      trm_add_cstyle Member_initializer (trm_set (trm_apps ~ctx (trm_unop (Unop_struct_get f)) [trm_this()]) (ti))
    | _ -> loc_fail loc "Clang_to_astRawC.tr_member_initializer_list: only simple member initializers are supported."
  ) init_list

(** [tr_decl d]: translates declaration [d] from clang to OptiTrust ast. *)
and tr_decl ?(in_class_decl : bool = false) (d : decl) : trm =
  let loc = loc_of_node d in
  let ctx = get_ctx () in
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
    let tc = name_to_typconstr tn in
    let tid = next_typconstrid () in
    ctx_tconstr_add tc tid;
    let td = {
      typdef_loc = loc;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_enum enum_constant_l
    } in
    ctx_typedef_add tc tid td;
    trm_typedef ?loc ~ctx td
  | Function {linkage = _; function_type = t; nested_name_specifier = nns;
              name = n; body = bo; deleted = _; constexpr = _; _} ->

    let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
    let s =
      begin match n with
        | IdentifierName s -> s
        | OperatorName op -> string_of_overloaded_op ?loc op
        | _ ->
          loc_fail loc "Clang_to_astRawC.tr_decl: only identifiers and overloaded operators allowed for functions"
      end
    in
    let {calling_conv = _; result = _; parameters = po;
         exception_spec = _; _} = t in

    redundant_template_definition_type := false;
    let tt = (tr_type_desc : ?loc:trm_loc -> ?const:bool -> ?tr_record_types:bool -> type_desc -> typ) ?loc (FunctionType t) in
    if !redundant_template_definition_type then raise RedundantTemplateDefinition;
    let res = begin match tt.typ_desc with
      | Typ_fun (args_t, out_t) ->
        begin match po with
          | None ->
            if List.length args_t != 0 then
              loc_fail loc "Clang_to_astRawC.tr_decl: wrong size of argument list";
            let tb =
              match bo with
              | None -> trm_lit ?loc Lit_uninitialized
              | Some s -> tr_stmt s
            in
            trm_let_fun ?loc (name_to_var ~namespaces:qpath s) out_t [] tb
          | Some {non_variadic = pl; variadic = _} ->
            let args =
              List.combine
                (List.map
                   (fun {decoration = _;
                         desc = {qual_type = _; name = n; default = _}} -> (name_to_var n))
                   pl
                )
                args_t
            in
            List.iter (fun (y, ty) -> ctx_var_add y ty) args;
            let tb =
              match bo with
              | None -> trm_lit ?loc Lit_uninitialized
              | Some s -> tr_stmt s
            in
            trm_let_fun ?loc (name_to_var ~namespaces:qpath s) out_t args tb
        end
      |_ -> loc_fail loc "Clang_to_astRawC.tr_decl: should not happen"
    end in
    (* DEPREACTED let res =trm_add_cstyle_clang_cursor (cursor_of_node d) res *)
    res
  | CXXMethod {function_decl = {linkage = _; function_type = ty; name = n; body = bo; deleted = _; constexpr = _; nested_name_specifier = nns; _};
               static = st; const = c; _} ->
    let qpath = (tr_nested_name_specifier : ?loc:trm_loc -> nested_name_specifier option -> typvar list) ?loc nns in
    let s =
      begin match n with
      | IdentifierName s -> s
      | OperatorName op -> "operator" ^ string_of_overloaded_op ?loc op
      | _ -> loc_fail loc "Clang_to_astRawC.tr_decl: only identifiers and overloaded operators allowed for method declarations"
      end
      in

    let {calling_conv = _; result = _; parameters = po;
         exception_spec = _; _} = ty in
    let tt = (tr_type_desc : ?loc:trm_loc -> ?const:bool -> ?tr_record_types:bool -> type_desc -> typ) ?loc (FunctionType ty) in
    let res =
    begin match tt.typ_desc with
      | Typ_fun (args_t, out_t) ->
        let args = begin match po with
          | None ->
            if List.length args_t != 0 then
              loc_fail loc "Clang_to_astRawC.tr_decl: wrong size of argument list";
            []
          | Some {non_variadic = pl; variadic = _} ->
            List.combine
              (List.map
                  (fun {decoration = _;
                        desc = {qual_type = _; name = n; default = _}} -> (name_to_var n))
                  pl
              )
              args_t
        end in
        List.iter (fun (y, ty) -> ctx_var_add y ty) args;
        let tb =
          match bo with
          | None -> trm_lit ?loc Lit_uninitialized
          | Some s -> tr_stmt s
        in
        trm_add_cstyle Method (trm_let_fun ?loc (name_to_var ~namespaces:qpath s) out_t  args tb)
      |_ -> loc_fail loc "Clang_to_astRawC.tr_decl: should not happen"
    end in
    (* DEPREACTED let res = trm_add_cstyle_clang_cursor (cursor_of_node d) in *)
    if st
      then trm_add_cstyle Static_fun res
      else if c then trm_add_cstyle Const_method res
      else res
  | Constructor { class_name = cn; parameters = {non_variadic = pl; _}; initializer_list = il; body = bd; implicit = ib; explicit = eb;  defaulted = db; _} ->
    let class_name = Tools.clean_class_name cn in
    let qpath = if in_class_decl then [] else [cn] in
    let args = List.map (fun {decoration = _;
       desc = {qual_type = q; name = n; default = _}} -> (name_to_var n,(tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q)) pl in
    let tb = match bd with
    | None -> trm_lit ?loc Lit_uninitialized
    | Some s -> tr_stmt s in
    let tb =
      if List.length il = 0
        then tb
        else
          let t_il = (tr_member_initialized_list : ?loc:trm_loc -> constructor_initializer list -> trms) ?loc il in
          insert_at_top_of_seq t_il tb
      in
    let res = trm_let_fun ?loc (name_to_var ~namespaces:qpath class_name) (typ_unit ()) args tb in
    (* DEPREACTED trm_add_cstyle_clang_cursor (cursor_of_node d) res *)
    if ib
     then trm_add_cstyle (Class_constructor Constructor_implicit) res
     else if eb then trm_add_cstyle (Class_constructor Constructor_explicit) res
     else if db then trm_add_cstyle (Class_constructor Constructor_default) res
     else trm_add_cstyle (Class_constructor Constructor_simpl) res

  | Destructor {class_name = cn; body = bd; defaulted = df; deleted = dl; _} ->
    let tb = match bd with
    | None -> trm_lit ?loc Lit_uninitialized
    | Some s -> tr_stmt s in
    let class_name = Tools.clean_class_name cn in
    let res = trm_let_fun ?loc (name_to_var class_name) (typ_unit ()) [] tb in
    (* DEPREACTED let res =trm_add_cstyle_clang_cursor (cursor_of_node d) in*)
    if df
      then trm_add_cstyle (Class_destructor Destructor_default) res
      else if dl then trm_add_cstyle (Class_destructor Destructor_delete) res
      else trm_add_cstyle (Class_destructor Destructor_simpl) res
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
    let tt = if contains_elaborated_type t then (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc ~tr_record_types:false t else (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc t in
    let te =
      begin match eo with
      | None -> trm_lit ?loc Lit_uninitialized
      | Some e ->
        begin match e.desc with
        | InitList el -> (* {e1,e2,e3} *)(* Array(struct intstantiation) declaration  with initialization *)
          (tr_init_list : ?loc:trm_loc -> ctx:ctx -> typ -> Clang.Ast.expr list -> trm) ?loc ~ctx tt el
        | Construct {args = args; qual_type = q} ->
          let el = List.filter (fun (e : expr) -> match e.desc with DefaultArg -> false | _ -> true) args in
          let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
          let f_name = trm_var (name_to_var (AstC_to_c.typ_to_string tq)) in
          let args = List.map tr_expr el in
          begin match q.desc with
          | Elaborated _ -> trm_add_cstyle Constructed_init (trm_apps f_name args)
          | Record _ ->
            (* DEPREACTED let f_name =  trm_add_cstyle_clang_cursor (cursor_of_node e) f_name in*)
            trm_add_cstyle Constructed_init (trm_apps f_name args)
          | _ -> tr_expr e
          end
        | New { init = ieo; _ } ->
          begin match ieo with
          | Some (ie) -> trm_apps (tr_expr e) [(tr_expr ie)]
          | None -> tr_expr e
          end
        | _ -> tr_expr e
        end
      end
      in
    let v = name_to_var n in
    ctx_var_add v tt;
    trm_let ?loc (v,tt) te
  | TypedefDecl {name = tn; underlying_type = q} ->
    let tc = name_to_typconstr tn in
    let tid = next_typconstrid () in
    ctx_tconstr_add tc tid;
    let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
    let td = {
      typdef_loc = loc;
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_alias tq
    }
    in
    ctx_typedef_add tc tid td;
    trm_typedef ?loc ~ctx td
  | TypeAlias {ident_ref = id; qual_type = q} ->
    begin match id.name with
      | IdentifierName tn ->
        let tc = name_to_typconstr tn in
        let tid = next_typconstrid () in
        ctx_tconstr_add tc tid;
        let tq = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
        let td = {
          typdef_loc = loc;
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_alias tq;} in
        ctx_typedef_add tc tid td;
        trm_typedef ?loc ~ctx td
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
        let ft = (tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q in
        let al = List.map (tr_attribute loc) al in
        let ty = {ft with typ_attributes = al} in
        acc @ [(Record_field_member (fn, ty), !access_spec)]
      | {decoration = _; desc = CXXMethod _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_field_method tdl, !access_spec)]
      | {decoration = _; desc = Constructor _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_field_method tdl, !access_spec)]
      | {decoration = _; desc = Destructor _; _} ->
        let tdl = tr_decl ~in_class_decl d in
        acc @ [(Record_field_method tdl, !access_spec)]
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
      let tc = name_to_typconstr rn in
      let tid = next_typconstrid () in
      ctx_tconstr_add tc tid;
      let td = {
          typdef_loc = loc;
          typdef_typid = tid;
          typdef_tconstr = rn;
          typdef_vars = [];
          typdef_body = Typdef_record prod_list
          } in
        ctx_typedef_add tc tid td;

      begin match k with
      | Struct -> trm_add_cstyle Is_struct (trm_typedef ?loc ~ctx:(get_ctx()) td)
      | Class -> trm_add_cstyle Is_class (trm_typedef ?loc ~ctx:(get_ctx()) td)
      | _ -> loc_fail loc "Clang_to_astRawC.tr_decl_list: only classes and structs are supported."
      end

  | Namespace {name = n; declarations = dl; inline = b} ->
    let dls = tr_decl_list dl in
    trm_namespace n (trm_seq_nomarks dls) b

  | TemplateDecl {parameters = {list = pl;_}; decl = d} ->
    let dl = tr_decl d in
    let pl = List.map (fun {decoration = _;desc = {parameter_name = n; parameter_kind = pk; parameter_pack = b};_} ->
      let tpk =  begin match pk with
        | Class {default = opt_q} ->
         begin match opt_q with
         | Some q -> Type_name (Some ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q))
         | None -> Type_name None
         end
        | NonType {parameter_type = q; default = opt_expr} ->
         begin match opt_expr with
         | Some e -> NonType ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q, Some (tr_expr e ))
         | None -> NonType ((tr_qual_type : ?loc:trm_loc -> ?tr_record_types:bool -> qual_type -> typ) ?loc q, None)
         end
        | _ -> loc_fail loc "Clang_to_astRawC.tr_decl: nested templates are not supported" (* LATER: Add support for nested templates *)
        end in
        (n, tpk, b)
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

(** [Include_map]: module useed for storing inclued files and their AST-s *)
module Include_map = Map.Make(String)
type 'a imap = 'a Include_map.t

(** [filter_out_include filename dl]: filters out all the declarations that are in fact include directives *)
let filter_out_include (filename : string) (dl : decl list) : ((decl list) imap) * (decl list) =
  let rec aux (include_map : (decl list) imap) (dl : decl list) =
    match dl with
    | [] -> (include_map, [])
    | d :: dl ->
      let (include_map, file_decls) = aux include_map dl in
      let file = file_of_node d in
      (* keep only project included files and optitrust.h,
         TODO: use a whitelist instead. *)
      let is_in_current_dir = Filename.dirname file = Filename.dirname filename in
      let is_optitrust_h = Filename.basename file = "optitrust.h" in
      if is_in_current_dir || is_optitrust_h then
        if file <> filename
        then
          (Include_map.update file
             (fun dlo ->
                match dlo with
                | None -> Some [d]
                | Some dl -> Some (d :: dl)
             )
             include_map,
           file_decls

          )
        else (include_map, d :: file_decls)
      else (include_map, file_decls)
  in
  aux (Include_map.empty) dl

(** [tr_ast t]: transalate [t] into OptiTrust AST *)
let tr_ast (t : translation_unit) : trm =
  ctx_reset ();
  (* Initialize id_counter *)
  let {decoration = _; desc = {filename = filename; items = dl}} = t in
  print_info None "tr_ast: translating %s's AST...\n" filename;
  let (include_map, file_decls) = filter_out_include filename dl in
  begin match !Flags.dump_clang_ast with
  | None -> ()
  | Some dump_clang_file ->
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
         trm_set_include h (trm_seq_nomarks (tr_decl_list dl)))
      include_map in
  trm_set_mainfile (trm_seq_nomarks ?loc ((Include_map.fold (fun _ t tl -> t :: tl) tinclude_map []) @ tr_decl_list file_decls))
