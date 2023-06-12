open Ppxlib

let pat_var_inv (pat : pattern) : (string, extension) result =
  match pat.ppat_desc with
  | Ppat_var loc -> Ok loc.txt
  | _ -> Error (Location.error_extensionf ~loc:pat.ppat_loc "let%%transfo: Only simple binding names are supported")

let module_of_location (l : Location.t) : string =
  let fname = l.loc_start.pos_fname in
  String.capitalize_ascii (Filename.remove_extension (Filename.basename fname))

let (let+) r cont =
  match r with
  | Ok r -> cont r
  | Error err ->
    let (module B) = Ast_builder.make (loc_of_extension err) in
    B.pexp_extension err

let printers_map = Longident.Map.of_seq (List.to_seq [
    (Lident "int", "string_of_int");
    (Lident "bool", "string_of_bool");
    (Lident "string", "Trace_printers.string_arg_printer");
    (Lident "var", "Trace_printers.string_arg_printer");
    (Lident "mark", "Trace_printers.string_arg_printer");
    (Lident "vars", "vars_to_string");
    (Lident "option", "Trace_printers.option_arg_printer");
    (Lident "list", "Trace_printers.list_arg_printer");
    (Lident "trm", "AstC_to_c.ast_to_string");
    (Lident "tile_iteration", "tile_iteration_to_string");
    (Lident "tile_bound", "tile_bound_to_string");
    (Lident "shift_kind", "shift_kind_to_string");
    (Lident "extension_kind", "extension_kind_to_string");
    (Lident "storage_folding_kind", "storage_folding_kind_to_string");
    (* TODO:
      - type rename
      - type local_ops
      - type nd_tile = (trm * trm) list
      - ~simpl args *)
  ])

let rec printer_expr_for_type (typ: core_type) : expression =
  let (module B) = Ast_builder.make typ.ptyp_loc in
  match typ.ptyp_desc with
  | Ptyp_constr (lident, l) when Longident.Map.mem lident.txt printers_map ->
    let printer = Longident.Map.find lident.txt printers_map in
    let sub_printers = List.map (fun t -> (Nolabel, printer_expr_for_type t)) l in
    B.pexp_apply (B.evar printer) sub_printers
  | _ -> B.pexp_fun Nolabel None B.ppat_any (B.estring ("<" ^ string_of_core_type typ ^ ">"))


let deal_with_binding (loc : Location.t) (binding : value_binding): value_binding =
  let (module B) = Ast_builder.make loc in
  let name = pat_var_inv binding.pvb_pat in
  let rec dive_into_args expr args =
    match expr.pexp_desc with
    | Pexp_fun (lbl, exp0, pat, expr) ->
      let label = match lbl with
      | Nolabel -> ""
      | Labelled l -> (* "~" ^ *) l
      | Optional l -> (* "?" ^ *) l
      in
      let+ (name, typ) =
        match pat.ppat_desc with
        | Ppat_constraint (pat, typ) -> Result.map (fun name -> (name, typ)) (pat_var_inv pat)
        | _ -> Error (Location.error_extensionf ~loc:pat.ppat_loc "let%%transfo: Type annotations are required for all arguments")
      in
      let arg_string = B.pexp_apply (printer_expr_for_type typ) [(Nolabel, (B.evar name))] in
      B.pexp_fun lbl exp0 pat (dive_into_args expr (B.pexp_tuple [B.estring label; arg_string] :: args))
    | body ->
      let+ name in
      let args = B.elist (List.rev args) in
      let continuation = B.pexp_fun Nolabel None B.punit expr in
      B.pexp_apply (B.evar ("Trace.transfo_step")) [(Labelled "name", (B.estring (Printf.sprintf "%s.%s" (module_of_location loc) name))); (Labelled "args", args); (Nolabel, continuation)]
  in
  { binding with pvb_expr = dive_into_args binding.pvb_expr [] }

let expand_let_transfo ~ctxt recflag bindings =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module B) = Ast_builder.make loc in
  let bindings = List.map (deal_with_binding loc) bindings in
  B.pstr_value recflag bindings

let let_transfo_extension =
  Extension.V3.declare "transfo"
    Extension.Context.structure_item
    Ast_pattern.(pstr ((pstr_value __ __) ^:: nil))
    expand_let_transfo

let let_transfo_rule = Ppxlib.Context_free.Rule.extension let_transfo_extension
let () = Driver.register_transformation ~rules:[let_transfo_rule] "transfo"
