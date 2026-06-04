open PPrint
open Ast
open Typ
open Optilambda_style

(** Printer for the OptiLambda textual language.

    This file intentionally depends only on [optitrust.ast], [optitrust.utils], and [pprint]. It must not depend on [optitrust.framework],
    because the framework trace/diff layer should later be able to depend on this module. *)

(** [parens_doc d] wraps [d] in parentheses. *)
let parens_doc d = lparen ^^ d ^^ rparen

(** [brackets_doc d] wraps [d] in square brackets. *)
let brackets_doc d = string "[" ^^ d ^^ string "]"

(** [angles_doc d] wraps [d] in angle brackets. *)
let angles_doc d = string "<" ^^ d ^^ string ">"

(** [parenthesize_if cond d] wraps [d] in parentheses when [cond] is true. *)
let parenthesize_if cond d = if cond then parens_doc d else d

(** [comma_sep docs] prints [docs] separated by commas and one space. *)
let comma_sep docs = separate (comma ^^ blank 1) docs

(** [semi_sep docs] prints [docs] separated by semicolons and hard line breaks. *)
let semi_sep docs = separate (semi ^^ hardline) docs

(** [block_doc docs] prints a sequence of instructions inside braces. *)
let block_doc docs =
  match docs with
  | [] -> lbrace ^^ rbrace
  | _ -> surround 2 1 lbrace (semi_sep docs) rbrace

type block_item = Regular of document | FinalExpr of document
type contract_clause = ContractClause of string * resource_item | ContractRaw of document

(** [code_block_doc items] prints executable block items.

    Regular items always end with a semicolon. The optional final expression is printed without a trailing semicolon, matching
    expression-oriented OptiLambda blocks. *)
let code_block_doc items =
  let item_to_doc = function
    | Regular d -> d ^^ semi
    | FinalExpr d -> d
  in
  match items with
  | [] -> lbrace ^^ rbrace
  | _ -> surround 2 1 lbrace (separate hardline (List.map item_to_doc items)) rbrace

(** [var_to_doc style v] prints a variable, optionally including its internal id. *)
let var_to_doc (style : Optilambda_style.style) (v : var) : document = string (if style.print_var_ids then var_to_string v else var_name v)

let is_internal (style : Optilambda_style.style) : bool =
  match style.representation with
  | Internal -> true
  | Surface
  | FullyTypedInternal -> false

let is_fully_typed_internal (style : Optilambda_style.style) : bool =
  match style.representation with
  | FullyTypedInternal -> true
  | Surface
  | Internal -> false

let is_explicit_internal (style : Optilambda_style.style) : bool = is_internal style || is_fully_typed_internal style

let is_typed_resource_constructor_name = function
  | "cell"
  | "Cell"
  | "CellOf"
  | "UninitCell"
  | "UninitCellOf"
  | "Matrix"
  | "UninitMatrix" ->
      true
  | name -> String.starts_with ~prefix:"Matrix" name || String.starts_with ~prefix:"UninitMatrix" name

(** [is_auto_type ty] checks whether [ty] is the internal [auto] type. *)
let is_auto_type (ty : typ) : bool =
  match ty.desc with
  | Trm_var v -> v.name = "auto" && v.namespaces = []
  | _ -> false

(** [is_type_type ty] checks whether [ty] is the internal [Type] universe. *)
let is_type_type (ty : typ) : bool =
  match ty.desc with
  | Trm_var v -> v.name = "Type" && v.namespaces = []
  | _ -> false

(** [is_ghost_ret_type ty] checks whether [ty] is the internal ghost return type. *)
let is_ghost_ret_type (ty : typ) : bool =
  match ty.desc with
  | Trm_var v -> v.name = "__ghost_ret" && v.namespaces = []
  | _ -> false

(** [typ_to_doc style ty] prints an OptiTrust type using OptiLambda syntax. *)
let rec typ_to_doc (style : Optilambda_style.style) (ty : typ) : document =
  match ty.desc with
  | Trm_var v -> var_to_doc style v
  | Trm_apps ({ desc = Trm_var v; _ }, args, [], []) ->
      begin match (v.name, args) with
      | "ptr", [ inner ] -> string "ptr" ^^ parens_doc (typ_to_doc style inner)
      | "const", [ inner ] -> string "const" ^^ parens_doc (typ_to_doc style inner)
      | "array", [ inner ] -> string "array" ^^ parens_doc (typ_to_doc style inner)
      | "array", [ inner; size ] -> string "array" ^^ parens_doc (comma_sep [ typ_to_doc style inner; trm_to_doc_at style 0 size ])
      | "fun", ret :: args -> string "fun" ^^ parens_doc (comma_sep (typ_to_doc style ret :: List.map (typ_to_doc style) args))
      | _ -> var_to_doc style v ^^ parens_doc (comma_sep (List.map (trm_to_doc_at style 0) args))
      end
  | Trm_arbitrary (Typ code) -> string code
  | _ -> string "type" ^^ parens_doc (trm_to_doc_at style 0 ty)

(** [name_with_optional_type_arg style name ty] prints [name<T>] only in the fully typed internal representation. *)
and name_with_optional_type_arg (style : Optilambda_style.style) (name : string) (ty : typ) : document =
  if is_fully_typed_internal style then string name ^^ angles_doc (typ_to_doc style ty) else string name

(** [typ_of_trm t] returns the known type of [t], or [auto] when unavailable. *)
and typ_of_trm (t : trm) : typ =
  Option.value ~default:typ_auto t.typ

(** [elem_typ_of_access_result ty] turns [ptr(T)] access results into [T] for typed internal access constructors. *)
and elem_typ_of_access_result (ty : typ) : typ =
  Option.value ~default:ty (typ_ptr_inv ty)

(** [typed_var_to_doc style (v, ty)] prints a variable declaration fragment. *)
and typed_var_to_doc (style : Optilambda_style.style) ((v, ty) : typed_var) : document =
  if style.print_types && not (is_auto_type ty) then var_to_doc style v ^^ colon ^^ blank 1 ^^ typ_to_doc style ty else var_to_doc style v

(** [lit_to_doc style lit] prints a literal value. *)
and lit_to_doc (style : Optilambda_style.style) (lit : lit) : document =
  match lit with
  | Lit_unit -> string "unit"
  | Lit_bool b -> string (string_of_bool b)
  | Lit_int (_, n) -> string (string_of_int n)
  | Lit_float (_, f) -> string (string_of_float f)
  | Lit_string s -> dquotes (string (String.escaped s))
  | Lit_null ty -> string "null" ^^ angles_doc (typ_to_doc style ty)

(** [loop_mode_to_doc style mode] prints a loop execution mode. *)
and loop_mode_to_doc (style : Optilambda_style.style) (mode : loop_mode) : document =
  match (style.loop_mode_style, mode) with
  | Short, Sequential -> string "seq"
  | Short, Parallel -> string "par"
  | Short, GpuThread -> string "gpu_thread"
  | Short, MagicThread -> string "magic_thread"
  | Full, Sequential -> string "Sequential"
  | Full, Parallel -> string "Parallel"
  | Full, GpuThread -> string "GpuThread"
  | Full, MagicThread -> string "MagicThread"

(** [loop_dir_to_doc dir] prints a loop bound direction. *)
and loop_dir_to_doc (dir : loop_dir) : document =
  match dir with
  | DirUp -> string "up"
  | DirUpEq -> string "up_eq"
  | DirDown -> string "down"
  | DirDownEq -> string "down_eq"

(** [binop_to_doc op] returns the infix symbol for binary operators that have one. *)
and binop_to_doc (op : binary_op) : document option =
  match op with
  | Binop_set -> Some equals
  | Binop_eq -> Some (string "=")
  | Binop_neq -> Some (string "<>")
  | Binop_sub -> Some (string "-")
  | Binop_add -> Some (string "+")
  | Binop_mul -> Some (string "*")
  | Binop_exact_div
  | Binop_trunc_div ->
      Some (string "/")
  | Binop_trunc_mod -> Some (string "%")
  | Binop_le -> Some (string "<=")
  | Binop_lt -> Some (string "<")
  | Binop_ge -> Some (string ">=")
  | Binop_gt -> Some (string ">")
  | Binop_bitwise_and -> Some (string "&")
  | Binop_bitwise_or -> Some (string "|")
  | Binop_shiftl -> Some (string "<<")
  | Binop_shiftr -> Some (string ">>")
  | Binop_xor -> Some (string "^")
  | Binop_array_access
  | Binop_array_get ->
      None

(** [binop_precedence op] gives larger numbers to more tightly binding operators. *)
and binop_precedence (op : binary_op) : int =
  match op with
  | Binop_set -> 1
  | Binop_eq
  | Binop_neq
  | Binop_le
  | Binop_lt
  | Binop_ge
  | Binop_gt ->
      3
  | Binop_bitwise_or
  | Binop_xor ->
      4
  | Binop_bitwise_and
  | Binop_shiftl
  | Binop_shiftr ->
      5
  | Binop_add
  | Binop_sub ->
      6
  | Binop_mul
  | Binop_exact_div
  | Binop_trunc_div
  | Binop_trunc_mod ->
      7
  | Binop_array_access
  | Binop_array_get ->
      10

(** [trm_precedence t] gives the precedence of [t] when printed as an expression. *)
and trm_precedence (t : trm) : int =
  match t.desc with
  | Trm_var _
  | Trm_lit _
  | Trm_prim _
  | Trm_arbitrary _ ->
      11
  | Trm_apps ({ desc = Trm_prim (_, Prim_binop op); _ }, _, _, _) -> binop_precedence op
  | Trm_apps ({ desc = Trm_prim (_, Prim_unop _); _ }, _, _, _) -> 8
  | Trm_apps _ -> 10
  | _ -> 0

(** [unop_name op] returns the textual fallback name for a unary primitive. *)
and unop_name (op : unary_op) : string option =
  match op with
  | Unop_get -> Some "get"
  | Unop_address -> Some "addr"
  | Unop_neg -> Some "not"
  | Unop_bitwise_neg -> Some "bit_not"
  | Unop_minus -> Some "minus"
  | Unop_plus -> Some "plus"
  | Unop_post_incr -> Some "post_incr"
  | Unop_post_decr -> Some "post_decr"
  | Unop_pre_incr -> Some "pre_incr"
  | Unop_pre_decr -> Some "pre_decr"
  | Unop_struct_access field -> Some ("struct_access." ^ field)
  | Unop_struct_get field -> Some ("struct_get." ^ field)
  | Unop_cast _ -> Some "cast"

(** [prim_to_doc style ty prim] prints a primitive operator or allocation primitive. *)
and prim_to_doc (style : Optilambda_style.style) (ty : typ) (prim : prim) : document =
  match prim with
  | Prim_unop (Unop_cast cast_ty) -> string "cast" ^^ angles_doc (typ_to_doc style cast_ty)
  | Prim_unop op -> string (Option.value ~default:"unop" (unop_name op))
  | Prim_binop op ->
      begin match binop_to_doc op with
      | Some doc -> doc
      | None ->
          begin match op with
          | Binop_array_access -> string "array_access"
          | Binop_array_get -> string "array_get"
          | _ -> string "binop"
          end
      end
  | Prim_compound_assign_op op ->
      begin match binop_to_doc op with
      | Some doc -> parens_doc (doc ^^ equals)
      | None -> string "compound_assign"
      end
  | Prim_ref when is_explicit_internal style -> name_with_optional_type_arg style "ref" ty
  | Prim_ref_uninit when is_explicit_internal style -> name_with_optional_type_arg style "ref_uninit" ty
  | Prim_ref -> string "ref" ^^ angles_doc (typ_to_doc style ty)
  | Prim_ref_uninit -> string "ref_uninit" ^^ angles_doc (typ_to_doc style ty)
  | Prim_new -> string "new" ^^ angles_doc (typ_to_doc style ty)
  | Prim_new_uninit -> string "new_uninit" ^^ angles_doc (typ_to_doc style ty)
  | Prim_delete -> string "delete"
  | Prim_array -> string "array"
  | Prim_record -> string "record"

(** [ghost_args_to_doc style ghost_args] prints call contract arguments, e.g. [[h := g]]. *)
and ghost_args_to_doc (style : Optilambda_style.style) (ghost_args : resource_item list) : document =
  brackets_doc
    (comma_sep
       (List.map
          (fun (hyp, formula) -> var_to_doc style hyp ^^ blank 1 ^^ string ":=" ^^ blank 1 ^^ trm_to_doc_at style 0 formula)
          ghost_args))

(** [ghost_bind_to_doc style ghost_bind] prints returned contract bindings, e.g. [[z : h]]. *)
and ghost_bind_to_doc (style : Optilambda_style.style) (ghost_bind : (var option * var) list) : document =
  brackets_doc
    (comma_sep
       (List.map
          (fun (bound, contract_var) ->
            let bound_doc =
              match bound with
              | None -> string "_"
              | Some v -> var_to_doc style v
            in
            bound_doc ^^ blank 1 ^^ colon ^^ blank 1 ^^ var_to_doc style contract_var)
          ghost_bind))

(** [call_suffix_to_doc style ghost_args ghost_bind] prints optional call contract suffixes. *)
and call_suffix_to_doc (style : Optilambda_style.style) ghost_args ghost_bind : document =
  let ghost_args_doc = if ghost_args = [] then empty else ghost_args_to_doc style ghost_args in
  let ghost_bind_doc = if ghost_bind = [] then empty else ghost_bind_to_doc style ghost_bind in
  ghost_args_doc ^^ ghost_bind_doc

(** [is_int_one t] checks whether [t] is the integer literal [1]. *)
and is_int_one (t : trm) : bool =
  match t.desc with
  | Trm_lit (Lit_int (_, 1)) -> true
  | _ -> false

(** [trm_has_attribute attr t] checks whether [t] carries [attr]. *)
and trm_has_attribute (attr : attribute) (t : trm) : bool = List.exists (( = ) attr) t.annot.trm_annot_attributes

(** [var_has_name name t] recognizes a variable term with the given unqualified name. *)
and var_has_name (name : string) (t : trm) : bool =
  match t.desc with
  | Trm_var v -> v.name = name && v.namespaces = []
  | _ -> false

(** [resource_item_to_doc style item] prints a named logical/resource formula. *)
and resource_item_to_doc (style : Optilambda_style.style) ((hyp, formula) : resource_item) : document =
  var_to_doc style hyp ^^ colon ^^ blank 1 ^^ trm_to_doc_at style 0 formula

(** [contract_clauses keyword items] builds a group of contract clauses. *)
and contract_clauses (keyword : string) (items : resource_item list) : contract_clause list =
  List.map (fun item -> ContractClause (keyword, item)) items

(** [contract_group_to_doc style keyword items] prints consecutive clauses sharing a keyword. *)
and contract_group_to_doc (style : Optilambda_style.style) (keyword : string) (items : resource_item list) : document =
  let align_doc = string (String.make (String.length keyword + 1) ' ') in
  match items with
  | [] -> empty
  | first :: rest ->
      let first_doc = string keyword ^^ blank 1 ^^ resource_item_to_doc style first in
      let rest_docs = List.map (fun item -> comma ^^ hardline ^^ align_doc ^^ resource_item_to_doc style item) rest in
      concat (first_doc :: rest_docs)

(** [contract_clauses_to_docs style clauses] merges consecutive clauses with the same keyword. *)
and contract_clauses_to_docs (style : Optilambda_style.style) (clauses : contract_clause list) : document list =
  let flush_group keyword items acc =
    match (keyword, items) with
    | None, _
    | _, [] ->
        acc
    | Some keyword, items -> contract_group_to_doc style keyword (List.rev items) :: acc
  in
  let rec aux cur_keyword cur_items acc clauses =
    match clauses with
    | [] -> List.rev (flush_group cur_keyword cur_items acc)
    | ContractRaw doc :: rest -> aux None [] (doc :: flush_group cur_keyword cur_items acc) rest
    | ContractClause (keyword, item) :: rest ->
        begin match cur_keyword with
        | Some cur when cur = keyword -> aux cur_keyword (item :: cur_items) acc rest
        | _ -> aux (Some keyword) [ item ] (flush_group cur_keyword cur_items acc) rest
        end
  in
  aux None [] [] clauses

(** [fun_contract_clause_docs style contract] prints the direct internal function contract. *)
and fun_contract_clauses (style : Optilambda_style.style) (contract : fun_contract) : contract_clause list =
  if not style.print_contracts then []
  else
    contract_clauses "requires" contract.pre.pure
    @ contract_clauses "consumes" contract.pre.linear
    @ contract_clauses "ensures" contract.post.pure
    @ contract_clauses "produces" contract.post.linear

(** [fun_spec_clause_docs style spec] prints clauses carried by a function spec. *)
and fun_spec_clauses (style : Optilambda_style.style) (spec : fun_spec) : contract_clause list =
  match spec with
  | FunSpecUnknown -> []
  | FunSpecContract contract -> fun_contract_clauses style contract
  | FunSpecReverts fn -> if style.print_contracts then [ ContractRaw (string "reverts" ^^ blank 1 ^^ var_to_doc style fn) ] else []

(** [loop_contract_clause_docs style contract] prints the direct internal loop contract. *)
and loop_contract_clauses (style : Optilambda_style.style) (contract : loop_contract) : contract_clause list =
  if not style.print_contracts then []
  else
    let strict_doc = if contract.strict then [ ContractRaw (string "strict") ] else [] in
    strict_doc
    @ contract_clauses "requires" contract.loop_ghosts
    @ contract_clauses "requires" contract.invariant.pure
    @ contract_clauses "preserves" contract.invariant.linear
    @ contract_clauses "reads" contract.parallel_reads
    @ contract_clauses "xrequires" contract.iter_contract.pre.pure
    @ contract_clauses "xconsumes" contract.iter_contract.pre.linear
    @ contract_clauses "xensures" contract.iter_contract.post.pure
    @ contract_clauses "xproduces" contract.iter_contract.post.linear

(** [fun_spec_items spec] collects resources mentioned by a function spec. *)
and fun_spec_items (spec : fun_spec) : resource_item list =
  match spec with
  | FunSpecUnknown
  | FunSpecReverts _ ->
      []
  | FunSpecContract contract -> contract.pre.pure @ contract.pre.linear @ contract.post.pure @ contract.post.linear

(** [loop_contract_items contract] collects resources mentioned by a loop contract. *)
and loop_contract_items (contract : loop_contract) : resource_item list =
  contract.loop_ghosts @ contract.invariant.pure @ contract.invariant.linear @ contract.parallel_reads @ contract.iter_contract.pre.pure
  @ contract.iter_contract.pre.linear @ contract.iter_contract.post.pure @ contract.iter_contract.post.linear

(** [contract_summary_to_doc style items] prints the header contract hypothesis list. *)
and contract_summary_to_doc (style : Optilambda_style.style) (items : resource_item list) : document =
  if (not style.print_contracts) || items = [] then empty
  else blank 1 ^^ brackets_doc (comma_sep (List.map (fun (hyp, _) -> var_to_doc style hyp) items))

(** [trm_to_block_doc_with_prefix style prefix t] prints [t] as a block after prefix lines. *)
and trm_to_block_doc_with_prefix (style : Optilambda_style.style) (prefix_docs : document list) (t : trm) : document =
  match t.desc with
  | Trm_seq (instrs, result) ->
      let instr_items = instrs_to_block_items style (Mlist.to_list instrs) in
      let result_items =
        match result with
        | None -> []
        | Some v -> [ FinalExpr (var_to_doc style v) ]
      in
      code_block_doc (List.map (fun doc -> Regular doc) prefix_docs @ instr_items @ result_items)
  | _ -> code_block_doc (List.map (fun doc -> Regular doc) prefix_docs @ [ FinalExpr (trm_to_doc_at style 0 t) ])

(** [type_result_body_to_doc style body] detects [Type { T }] bodies that can be printed as [T]. *)
and type_result_body_to_doc (style : Optilambda_style.style) (body : trm) : document option =
  match body.desc with
  | Trm_seq (instrs, result) ->
      begin match (Mlist.to_list instrs, result) with
      | [], Some v -> Some (var_to_doc style v)
      | [ t ], None -> Some (trm_to_doc_at style 0 t)
      | _ -> None
      end
  | _ -> Some (trm_to_doc_at style 0 body)

(** [fun_def_to_doc style ?type_params name args ret_ty body] prints a function definition. *)
and fun_def_to_doc (style : Optilambda_style.style) ?(type_params = []) (name : var option) (args : typed_vars) (ret_ty : typ)
    (spec : fun_spec) (body : trm) : document =
  let name_doc =
    match name with
    | None -> empty
    | Some v -> blank 1 ^^ var_to_doc style v
  in
  let type_params_doc =
    match type_params with
    | [] -> empty
    | _ -> brackets_doc (comma_sep (List.map (var_to_doc style) type_params))
  in
  let args_doc = parens_doc (comma_sep (List.map (typed_var_to_doc style) args)) in
  let is_ghost = is_ghost_ret_type ret_ty in
  let ret_doc =
    if is_ghost then empty
    else if style.print_types && not (is_auto_type ret_ty) then colon ^^ blank 1 ^^ typ_to_doc style ret_ty
    else empty
  in
  let contract_docs = contract_clauses_to_docs style (fun_spec_clauses style spec) in
  let contract_summary_doc = if is_ghost then empty else contract_summary_to_doc style (fun_spec_items spec) in
  let fun_prefix = if is_ghost then string "ghost fun" else string "fun" in
  let body_doc =
    if style.print_types && is_type_type ret_ty then
      match type_result_body_to_doc style body with
      | Some body_type_doc -> colon ^^ blank 1 ^^ body_type_doc
      | None -> ret_doc ^^ contract_summary_doc ^^ blank 1 ^^ trm_to_block_doc_with_prefix style contract_docs body
    else ret_doc ^^ contract_summary_doc ^^ blank 1 ^^ trm_to_block_doc_with_prefix style contract_docs body
  in
  group (fun_prefix ^^ name_doc ^^ type_params_doc ^^ args_doc ^^ body_doc)

(** [let_to_doc style (v, ty) body] prints a let-binding or function definition. *)
and let_to_doc (style : Optilambda_style.style) ((v, ty) : typed_var) (body : trm) : document =
  match body.desc with
  | Trm_fun (args, ret_ty, fun_body, spec) -> fun_def_to_doc style (Some v) args ret_ty spec fun_body
  | Trm_apps ({ desc = Trm_prim (ref_ty, Prim_ref); _ }, [ init ], [], []) when is_explicit_internal style ->
      string "let" ^^ blank 1 ^^ var_to_doc style v ^^ blank 1 ^^ equals ^^ blank 1 ^^ string "ref"
      ^^ (if is_fully_typed_internal style then angles_doc (typ_to_doc style ref_ty) else empty)
      ^^ parens_doc (trm_to_doc_at style 0 init)
  | Trm_apps ({ desc = Trm_prim (ref_ty, Prim_ref_uninit); _ }, [], [], []) when is_explicit_internal style ->
      string "let" ^^ blank 1 ^^ var_to_doc style v ^^ blank 1 ^^ equals ^^ blank 1 ^^ string "ref_uninit"
      ^^ (if is_fully_typed_internal style then angles_doc (typ_to_doc style ref_ty) else empty)
      ^^ parens_doc empty
  | Trm_apps ({ desc = Trm_prim (_, Prim_ref); _ }, [ init ], [], []) ->
      string "letmut" ^^ blank 1 ^^ var_to_doc style v ^^ blank 1 ^^ equals ^^ blank 1 ^^ trm_to_doc_at style 0 init
  | Trm_apps ({ desc = Trm_prim (_, Prim_ref_uninit); _ }, [], [], []) -> string "letmut" ^^ blank 1 ^^ var_to_doc style v
  | _ ->
      let typed_doc =
        if style.print_types && not (is_auto_type ty) then var_to_doc style v ^^ colon ^^ blank 1 ^^ typ_to_doc style ty
        else var_to_doc style v
      in
      string "let" ^^ blank 1 ^^ typed_doc ^^ blank 1 ^^ equals ^^ blank 1 ^^ trm_to_doc_at style 0 body

(** [app_to_doc style f args ghost_args ghost_bind] prints calls and primitive applications. *)
and app_to_doc (style : Optilambda_style.style) ~(result_typ : typ) (f : trm) (args : trm list) (ghost_args : resource_item list)
    (ghost_bind : (var option * var) list) : document =
  match (f.desc, args) with
  | Trm_prim (_, Prim_binop Binop_array_access), [ base; index ] when is_internal style ->
      trm_to_doc_at style 10 base ^^ blank 1 ^^ string "[+]" ^^ blank 1 ^^ trm_to_doc_at style 0 index
  | Trm_prim (_, Prim_binop Binop_array_get), [ base; index ] when is_internal style ->
      string "get"
      ^^ parens_doc (trm_to_doc_at style 10 base ^^ blank 1 ^^ string "[+]" ^^ blank 1 ^^ trm_to_doc_at style 0 index)
  | Trm_prim (_, Prim_binop Binop_array_access), [ base; index ] when is_fully_typed_internal style ->
      name_with_optional_type_arg style "Array_Access" (elem_typ_of_access_result result_typ)
      ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 base; trm_to_doc_at style 0 index ])
  | Trm_prim (_, Prim_binop Binop_array_get), [ base; index ] when is_fully_typed_internal style ->
      name_with_optional_type_arg style "get" result_typ
      ^^ parens_doc
           (name_with_optional_type_arg style "Array_Access" result_typ
           ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 base; trm_to_doc_at style 0 index ]))
  | Trm_prim (_, Prim_binop (Binop_array_access | Binop_array_get)), [ base; index ] ->
      trm_to_doc_at style 10 base ^^ brackets_doc (trm_to_doc_at style 0 index)
  | Trm_prim (_, Prim_binop Binop_set), [ lhs; rhs ] when is_explicit_internal style ->
      name_with_optional_type_arg style "set" (typ_of_trm rhs)
      ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 lhs; trm_to_doc_at style 0 rhs ])
  | Trm_prim (_, Prim_binop Binop_set), [ lhs; rhs ] ->
      trm_to_doc_at style 2 lhs ^^ blank 1 ^^ equals ^^ blank 1 ^^ trm_to_doc_at style 1 rhs
  | Trm_prim (_, Prim_binop op), [ lhs; rhs ] ->
      begin match binop_to_doc op with
      | Some op_doc ->
          let prec = binop_precedence op in
          trm_to_doc_at style prec lhs ^^ blank 1 ^^ op_doc ^^ blank 1 ^^ trm_to_doc_at style (prec + 1) rhs
      | None -> prim_to_doc style typ_auto (Prim_binop op) ^^ parens_doc (comma_sep (List.map (trm_to_doc_at style 0) args))
      end
  | Trm_prim (_, Prim_unop (Unop_struct_get field)), [ base ] when is_internal style ->
      string "get" ^^ parens_doc (trm_to_doc_at style 10 base ^^ blank 1 ^^ string "[.]" ^^ blank 1 ^^ string field)
  | Trm_prim (_, Prim_unop (Unop_struct_access field)), [ base ] when is_internal style ->
      trm_to_doc_at style 10 base ^^ blank 1 ^^ string "[.]" ^^ blank 1 ^^ string field
  | Trm_prim (_, Prim_unop (Unop_struct_get field)), [ base ] when is_fully_typed_internal style ->
      name_with_optional_type_arg style "get" result_typ
      ^^ parens_doc
           (name_with_optional_type_arg style "Record_Access" result_typ
           ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 base; string field ]))
  | Trm_prim (_, Prim_unop (Unop_struct_access field)), [ base ] when is_fully_typed_internal style ->
      name_with_optional_type_arg style "Record_Access" (elem_typ_of_access_result result_typ)
      ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 base; string field ])
  | Trm_prim (_, Prim_unop (Unop_struct_get field)), [ base ] -> trm_to_doc_at style 10 base ^^ string "." ^^ string field
  | Trm_prim (_, Prim_unop (Unop_struct_access field)), [ base ] -> trm_to_doc_at style 10 base ^^ string "." ^^ string field
  | Trm_prim (_, Prim_unop Unop_get), [ arg ] when is_explicit_internal style ->
      name_with_optional_type_arg style "get" result_typ ^^ parens_doc (trm_to_doc_at style 0 arg)
  | Trm_prim (_, Prim_unop Unop_get), [ arg ] -> trm_to_doc_at style 8 arg
  | Trm_prim (_, Prim_unop Unop_address), [ arg ] -> string "&" ^^ trm_to_doc_at style 8 arg
  | Trm_prim (_, Prim_unop Unop_minus), [ arg ] -> string "-" ^^ trm_to_doc_at style 8 arg
  | Trm_prim (_, Prim_unop Unop_neg), [ arg ] -> string "not" ^^ blank 1 ^^ trm_to_doc_at style 8 arg
  | Trm_prim (_, Prim_unop (Unop_cast cast_ty)), [ arg ] ->
      string "cast" ^^ angles_doc (typ_to_doc style cast_ty) ^^ parens_doc (trm_to_doc_at style 0 arg)
  | Trm_prim (_, Prim_record), _ -> lbrace ^^ comma_sep (List.map (trm_to_doc_at style 0) args) ^^ rbrace
  | Trm_prim (ty, prim), _ -> prim_to_doc style ty prim ^^ parens_doc (comma_sep (List.map (trm_to_doc_at style 0) args))
  | Trm_var v, first_arg :: _ when is_fully_typed_internal style && is_typed_resource_constructor_name v.name ->
      string v.name ^^ angles_doc (typ_to_doc style (typ_of_trm first_arg))
      ^^ parens_doc (comma_sep (List.map (trm_to_doc_at style 0) args))
  | _ ->
      trm_to_doc_at style 10 f
      ^^ parens_doc (comma_sep (List.map (trm_to_doc_at style 0) args))
      ^^ call_suffix_to_doc style ghost_args ghost_bind

(** [ghost_call_to_doc style f args ghost_args ghost_bind] prints the contents of a ghost call. *)
and ghost_call_to_doc (style : Optilambda_style.style) (f : trm) (args : trm list) (ghost_args : resource_item list)
    (ghost_bind : (var option * var) list) : document =
  app_to_doc style ~result_typ:typ_auto f args ghost_args ghost_bind

(** [ghost_to_doc style t] prints ghost instructions in OptiLambda syntax. *)
and ghost_to_doc (style : Optilambda_style.style) (t : trm) : document option =
  if (not style.print_ghosts) || not (trm_has_attribute GhostInstr t) then None
  else
    match t.desc with
    | Trm_let ((pair_var, _), { desc = Trm_apps (ghost_begin, [ { desc = Trm_apps (f, args, ghost_args, ghost_bind); _ } ], [], []); _ })
      when var_has_name "__ghost_begin" ghost_begin ->
        Some
          (string "ghost_begin"
          ^^ parens_doc (comma_sep [ var_to_doc style pair_var; ghost_call_to_doc style f args ghost_args ghost_bind ]))
    | Trm_apps (ghost_end, [ { desc = Trm_var pair_var; _ } ], [], []) when var_has_name "__ghost_end" ghost_end ->
        Some (string "ghost_end" ^^ parens_doc (var_to_doc style pair_var))
    | Trm_apps (f, args, ghost_args, ghost_bind) ->
        Some (string "ghost" ^^ parens_doc (ghost_call_to_doc style f args ghost_args ghost_bind))
    | _ -> None

(** [seq_to_doc style instrs result] prints an OptiLambda block. *)
and seq_to_doc (style : Optilambda_style.style) (instrs : trm mlist) (result : var option) : document =
  let instr_items = instrs_to_block_items style (Mlist.to_list instrs) in
  let result_items =
    match result with
    | None -> []
    | Some v -> [ FinalExpr (var_to_doc style v) ]
  in
  code_block_doc (instr_items @ result_items)

(** [instrs_to_block_items style instrs] prints final [return x] as final expression [x]. *)
and instrs_to_block_items (style : Optilambda_style.style) (instrs : trm list) : block_item list =
  let rec aux acc instrs =
    match instrs with
    | [] -> List.rev acc
    | [ { desc = Trm_abort (Ret (Some ret)); _ } ] -> List.rev (FinalExpr (trm_to_doc_at style 0 ret) :: acc)
    | instr :: rest -> aux (Regular (trm_to_doc_at style 0 instr) :: acc) rest
  in
  aux [] instrs

(** [trm_to_block_doc style t] prints [t] as a block when required by syntax. *)
and trm_to_block_doc (style : Optilambda_style.style) (t : trm) : document =
  match t.desc with
  | Trm_seq _ -> trm_to_doc style t
  | _ -> block_doc [ trm_to_doc_at style 0 t ]

(** [for_to_doc style range mode body] prints a simple OptiTrust for-loop. *)
and for_to_doc (style : Optilambda_style.style) (range : loop_range) (mode : loop_mode) (contract : loop_contract) (body : trm) : document =
  let step_doc =
    match (range.direction, range.step.desc) with
    | (DirDown | DirDownEq), Trm_lit (Lit_int (typ, n)) -> trm_to_doc_at style 0 { range.step with desc = Trm_lit (Lit_int (typ, -n)) }
    | (DirDown | DirDownEq), _ -> string "-" ^^ trm_to_doc_at style 8 range.step
    | _ -> trm_to_doc_at style 0 range.step
  in
  let show_step =
    match range.direction with
    | DirDown
    | DirDownEq ->
        true
    | DirUp
    | DirUpEq ->
        not (style.omit_default_loop_step && is_int_one range.step)
  in
  let range_doc =
    var_to_doc style range.index ^^ blank 1 ^^ string "in" ^^ blank 1 ^^ trm_to_doc_at style 0 range.start ^^ string ".."
    ^^ trm_to_doc_at style 0 range.stop
    ^^ if show_step then colon ^^ step_doc else empty
  in
  string "for"
  ^^ angles_doc (loop_mode_to_doc style mode)
  ^^ blank 1 ^^ range_doc
  ^^ contract_summary_to_doc style (loop_contract_items contract)
  ^^ blank 1
  ^^ trm_to_block_doc_with_prefix style (contract_clauses_to_docs style (loop_contract_clauses style contract)) body

(** [record_member_to_doc style member] prints a typedef record member. *)
and record_member_to_doc (style : Optilambda_style.style) ((member, _) : record_member * record_member_annot) : document =
  match member with
  | Record_field (label, ty) -> string label ^^ colon ^^ blank 1 ^^ typ_to_doc style ty
  | Record_method method_trm -> trm_to_doc_at style 0 method_trm

(** [typedef_to_doc style t] prints a typedef node in a compact OptiLambda form. *)
and typedef_to_doc (style : Optilambda_style.style) (t : trm) : document =
  match t.desc with
  | Trm_typedef td ->
      let name_doc = var_to_doc style td.typedef_name in
      begin match td.typedef_body with
      | Typedef_alias ty -> string "type" ^^ blank 1 ^^ name_doc ^^ blank 1 ^^ equals ^^ blank 1 ^^ typ_to_doc style ty
      | Typedef_record members ->
          string "record" ^^ blank 1 ^^ name_doc ^^ blank 1 ^^ block_doc (List.map (record_member_to_doc style) members)
      | Typedef_enum cases ->
          let case_to_doc (case_var, value) =
            match value with
            | None -> var_to_doc style case_var
            | Some t -> var_to_doc style case_var ^^ blank 1 ^^ equals ^^ blank 1 ^^ trm_to_doc_at style 0 t
          in
          string "enum" ^^ blank 1 ^^ name_doc ^^ blank 1 ^^ block_doc (List.map case_to_doc cases)
      end
  | _ -> string "type" ^^ parens_doc (trm_to_doc_at style 0 t)

(** [switch_to_doc style cond cases] prints a switch statement. *)
and switch_to_doc (style : Optilambda_style.style) (cond : trm) (cases : (trms * trm) list) : document =
  let case_to_doc (patterns, body) =
    let head =
      match patterns with
      | [] -> string "default"
      | _ -> string "case" ^^ blank 1 ^^ comma_sep (List.map (trm_to_doc_at style 0) patterns)
    in
    head ^^ colon ^^ blank 1 ^^ trm_to_block_doc style body
  in
  string "switch" ^^ blank 1 ^^ parens_doc (trm_to_doc_at style 0 cond) ^^ blank 1 ^^ block_doc (List.map case_to_doc cases)

(** [marks_to_doc marks] prints term marks as an OptiLambda attribute prefix. *)
and marks_to_doc (marks : marks) : document = string "@marks" ^^ brackets_doc (comma_sep (List.map string marks))

(** [add_marks_to_doc style t doc] prefixes [doc] with marks carried by [t]. *)
and add_marks_to_doc (style : Optilambda_style.style) (t : trm) (doc : document) : document =
  if style.print_marks && t.annot.trm_annot_marks <> [] then marks_to_doc t.annot.trm_annot_marks ^^ blank 1 ^^ doc else doc

(** [trm_to_doc_at style ctx_prec t] prints [t] in an expression context. *)
and trm_to_doc_at (style : Optilambda_style.style) (ctx_prec : int) (t : trm) : document =
  let doc =
    match ghost_to_doc style t with
    | Some doc -> doc
    | None -> (
        match t.desc with
        | Trm_var v -> var_to_doc style v
        | Trm_lit lit -> lit_to_doc style lit
        | Trm_prim (ty, prim) -> prim_to_doc style ty prim
        | Trm_let (typed_var, body) -> let_to_doc style typed_var body
        | Trm_let_mult bindings -> block_doc (List.map (fun (typed_var, body) -> let_to_doc style typed_var body) bindings)
        | Trm_predecl typed_var -> string "let" ^^ blank 1 ^^ typed_var_to_doc style typed_var
        | Trm_fun (args, ret_ty, body, spec) -> fun_def_to_doc style None args ret_ty spec body
        | Trm_typedef _ -> typedef_to_doc style t
        | Trm_if (cond, then_branch, else_branch) ->
            string "if" ^^ blank 1
            ^^ parens_doc (trm_to_doc_at style 0 cond)
            ^^ blank 1 ^^ trm_to_block_doc style then_branch ^^ blank 1 ^^ string "else" ^^ blank 1 ^^ trm_to_block_doc style else_branch
        | Trm_seq (instrs, result) -> seq_to_doc style instrs result
        | Trm_apps (f, args, ghost_args, ghost_bind) -> app_to_doc style ~result_typ:(typ_of_trm t) f args ghost_args ghost_bind
        | Trm_for (range, mode, body, contract) -> for_to_doc style range mode contract body
        | Trm_for_c (init, cond, step, body, _) ->
            string "for_c"
            ^^ parens_doc (comma_sep [ trm_to_doc_at style 0 init; trm_to_doc_at style 0 cond; trm_to_doc_at style 0 step ])
            ^^ blank 1 ^^ trm_to_block_doc style body
        | Trm_while (cond, body) ->
            string "while" ^^ blank 1 ^^ parens_doc (trm_to_doc_at style 0 cond) ^^ blank 1 ^^ trm_to_block_doc style body
        | Trm_do_while (body, cond) ->
            string "do" ^^ blank 1 ^^ trm_to_block_doc style body ^^ blank 1 ^^ string "while" ^^ blank 1
            ^^ parens_doc (trm_to_doc_at style 0 cond)
        | Trm_switch (cond, cases) -> switch_to_doc style cond cases
        | Trm_abort abort ->
            begin match abort with
            | Ret None -> string "return"
            | Ret (Some ret) -> string "return" ^^ blank 1 ^^ trm_to_doc_at style 0 ret
            | Break None -> string "break"
            | Break (Some label) -> string "break" ^^ blank 1 ^^ string label
            | Continue None -> string "continue"
            | Continue (Some label) -> string "continue" ^^ blank 1 ^^ string label
            end
        | Trm_goto label -> string "goto" ^^ blank 1 ^^ string label
        | Trm_arbitrary code -> string (code_to_str code)
        | Trm_omp_routine _ -> string "omp_routine"
        | Trm_extern (lang, trms) ->
            string "extern" ^^ blank 1 ^^ dquotes (string lang) ^^ blank 1 ^^ block_doc (List.map (trm_to_doc_at style 0) trms)
        | Trm_namespace (name, body, _) -> string "namespace" ^^ blank 1 ^^ string name ^^ blank 1 ^^ trm_to_block_doc style body
        | Trm_template (params, body) ->
            begin match body.desc with
            | Trm_let ((name, _), { desc = Trm_fun (args, ret_ty, fun_body, spec); _ }) ->
                let type_params = List.map fst params in
                fun_def_to_doc style ~type_params (Some name) args ret_ty spec fun_body
            | _ ->
                string "template"
                ^^ brackets_doc (comma_sep (List.map (fun (v, _) -> var_to_doc style v) params))
                ^^ blank 1 ^^ trm_to_doc_at style 0 body
            end
        | Trm_using_directive name -> string "using" ^^ blank 1 ^^ string name)
  in
  parenthesize_if (trm_precedence t < ctx_prec) (add_marks_to_doc style t doc)

(** [trm_to_doc style t] is the main entry point for printing terms. *)
and trm_to_doc (style : Optilambda_style.style) (t : trm) : document = trm_to_doc_at style 0 t

(** [typ_to_string ?style ty] prints a type directly to a string. *)
let typ_to_string ?(style = Optilambda_style.default) (ty : typ) : string = Tools.document_to_string (typ_to_doc style ty)

(** [trm_to_string ?style t] prints a term directly to a string. *)
let trm_to_string ?(style = Optilambda_style.default) (t : trm) : string = Tools.document_to_string (trm_to_doc style t)
