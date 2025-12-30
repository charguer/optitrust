open Ast
open Trm
open Typ
open Mark

(**
  A [formula] is a [trm] that corresponds to a logical formula. Evaluating inside a formula cannot have side effects and always terminates.
  Formulas may refer to pure variables, bound by a [resource_set] or by the program (program variables are constant due to our AST encoding).
*)

(** An optionally named resource item produced by parser.
    TODO: remove. *)
type contract_resource_item = var option * formula

(** All formulas should have this annotation. *)
let formula_annot = {trm_annot_default with trm_annot_cstyle = [ResourceFormula]}

let is_formula (t: trm): bool =
  trm_has_cstyle ResourceFormula t

let new_hyp = new_var

(** Returns a fresh variable without name. *)
let new_anon_hyp (): var =
  new_hyp ""

(** Primitive function that constructs a read only resource. *)
let var_read_only = toplevel_var "_RO"
let trm_read_only = trm_var var_read_only

(** Primitive type of fractions. *)
let var_frac = toplevel_typvar "_Fraction"
let typ_frac = trm_var var_frac

(** Creates a new fraction variable.
    It ranges on fraction values from \]0; 1\]. *)
let new_frac (): var * resource_item =
  let frac_hyp = new_anon_hyp () in
  (frac_hyp, (frac_hyp, typ_frac))

(** The fraction representing having it all. *)
let full_frac = trm_int ~typ:typ_frac 1

(** Tries to embed a program term within formulas.
    Pure and total terms can be successfully embedded, according to built-in whitelist. *)
let rec formula_of_trm (t: trm): formula option =
  (* DEPRECATED *)
  (* LATER: Extensible list of applications that can be translated into formula.
     OR A term can be embedded in a formula if it is pure (no linear resources).
     Does [t] change or is this just checking whether trm_is_formula?
     I.e. are the trm and formula languages intersecting or separate?
     *)
  let open Option.Monad in
  match t.desc with
  | Trm_var _ | Trm_lit _ | Trm_prim _ -> Some t
  | Trm_apps (fn, args, _, _) ->
    let* f_args = try Some (List.map (fun arg -> Option.get (formula_of_trm arg)) args) with Invalid_argument _ -> None in
    begin match trm_prim_inv fn with
      | Some (_, Prim_binop Binop_add)
      | Some (_, Prim_binop Binop_sub)
      | Some (_, Prim_binop Binop_mul)
      | Some (_, Prim_binop Binop_eq)
      | Some (_, Prim_binop Binop_exact_div) (* TODO: think hard about 'div' totality *)
      | Some (_, Prim_binop Binop_trunc_div)
      | Some (_, Prim_binop Binop_trunc_mod)
      | Some (_, Prim_binop Binop_array_access)
      | Some (_, Prim_unop (Unop_struct_access _))
      | Some (_, Prim_unop (Unop_struct_get _))
      | Some (_, Prim_unop Unop_minus)
      | Some (_, Prim_record)
      | Some (_, Prim_array)
        -> Some (trm_apps fn f_args)
      | Some _ -> None
      | None ->
      begin match trm_var_inv fn with
      | Some fv when Matrix_trm.mindex_var_inv fv <> None ->
        Some (trm_apps fn f_args)
      | Some fv when var_eq fv var_sizeof ->
        Some (trm_apps fn f_args)
      | _ -> None
      end
    end
  | _ -> None

(** The division operator for a fraction *)
let var_frac_div = toplevel_var "__frac_div"
let fun_frac_div = trm_var var_frac_div
let formula_frac_div frac div = trm_apps ~annot:formula_annot ~typ:typ_frac fun_frac_div [frac; div]

(** The subtraction operator for a fraction *)
let var_frac_sub = toplevel_var "__frac_sub"
let fun_frac_sub = trm_var var_frac_sub
let formula_frac_sub base_frac carved_frac = trm_apps ~annot:formula_annot ~typ:typ_frac fun_frac_sub [base_frac; carved_frac]

let formula_prop_unop (op: var) (a: formula): formula =
  trm_apps ~typ:typ_prop ~annot:formula_annot (trm_var op) [a]

let formula_prop_binop (cmp: var) (a: formula) (b: formula): formula =
  trm_apps ~typ:typ_prop ~annot:formula_annot (trm_var cmp) [a; b]

let var_is_true = toplevel_var "__is_true"
let var_is_false = toplevel_var "__is_false"
let var_not = toplevel_var "__not"
let var_and = toplevel_var "__and"
let var_or = toplevel_var "__or"

let formula_is_true = formula_prop_unop var_is_true
let formula_is_false = formula_prop_unop var_is_false
let formula_not = formula_prop_unop var_not
let formula_and = formula_prop_binop var_and
let formula_or = formula_prop_binop var_or

let rec bool_formula_to_prop (t: formula): formula =
  Pattern.pattern_match t [
    Pattern.(trm_neg !__) (fun t () -> neg_bool_formula_to_prop t);
    Pattern.(trm_and !__ !__) (fun t1 t2 () -> formula_and (bool_formula_to_prop t1) (bool_formula_to_prop t2));
    Pattern.(trm_or !__ !__) (fun t1 t2 () -> formula_or (bool_formula_to_prop t1) (bool_formula_to_prop t2));
    Pattern.__ (fun () -> formula_is_true t)
  ]
and neg_bool_formula_to_prop (t: formula): formula =
  Pattern.pattern_match t [
    Pattern.(trm_neg !__) (fun t () -> bool_formula_to_prop t);
    Pattern.(trm_and !__ !__) (fun t1 t2 () -> formula_or (neg_bool_formula_to_prop t1) (neg_bool_formula_to_prop t2));
    Pattern.(trm_or !__ !__) (fun t1 t2 () -> formula_and (neg_bool_formula_to_prop t1) (neg_bool_formula_to_prop t2));
    Pattern.__ (fun () -> formula_is_false t)
  ]

let formula_eq ~typ t1 t2 = formula_is_true (trm_eq ~typ t1 t2)
let formula_neq ~typ t1 t2 = formula_is_true (trm_neq ~typ t1 t2)
let formula_lt ~typ t1 t2 = formula_is_true (trm_lt ~typ t1 t2)
let formula_gt ~typ t1 t2 = formula_is_true (trm_gt ~typ t1 t2)
let formula_leq ~typ t1 t2 = formula_is_true (trm_le ~typ t1 t2)
let formula_geq ~typ t1 t2 = formula_is_true (trm_ge ~typ t1 t2)

(**
  Pure function representing a permission. When it is used in a function contract, the typechecker drops
  the function arguments as provided in the function term, considers the function's pure type as auto,
  and binds _Res to the variable provided as argument, with the provided return type T.
  Used as a hack to implement polymorphic functions that return a polymorphic type.
*)
let var_spec_override_ret = toplevel_var "__spec_override_ret"

(**
  Pure function representing a permission. When it is used in a function contract, the typechecker drops
  the function arguments as provided in the function term, considers the function's pure type as auto,
  and indicates that _Res has type T, but does not bind it to any specific variable.
  Used as a hack to implement polymorphic functions that produce resources referring to _Res directly (like malloc)
*)
let var_spec_override_ret_implicit = toplevel_var "__spec_override_ret_implicit"

(**
  Pure function representing a permission. When it is used in a function contract, the typechecker drops
  the function arguments as provided in the function term, and considers the function's pure type as auto.
  Used as a hack to implement polymorphic functions that have no return.
*)
let var_spec_override_noret = toplevel_var "__spec_override_noret"

let formula_spec_override_inv formula: (trm option * typ) option =
  Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_specific_var var_spec_override_ret) !__ !__) (fun res_typ res_val () ->
      Some (Some res_val, res_typ));
    Pattern.(trm_apps1 (trm_specific_var var_spec_override_ret_implicit) !__) (fun res_typ () ->
      Some (None, res_typ));
    Pattern.(trm_apps0 (trm_specific_var var_spec_override_noret)) (fun () ->
      Some (None, typ_unit));
    Pattern.__ (fun () -> None) ]

(* -------- SMART FORMULA CONSTRUCTORS, INVERTERS and COMBINATORS -------- *)

let formula_fun ?(rettyp = typ_auto) args body =
  trm_fun ~annot:formula_annot args rettyp body

let var_repr = toplevel_var "~>"
let trm_repr = trm_var var_repr
let formula_repr (var: trm) (repr: formula): formula =
  trm_apps ~annot:formula_annot trm_repr [var; repr]

let formula_repr_inv (t: formula): (trm * formula) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var(xf) }, [var; repr]) when var_eq xf var_repr ->
    Some (var, repr)
  | _ -> None

let typ_mem_type_var = toplevel_var "MemType"
let typ_mem_type = trm_var typ_mem_type_var
let mem_typ_any_var = toplevel_var "Any"
let mem_typ_any = trm_var mem_typ_any_var

let var_cell_of = toplevel_var "CellOf"
let trm_cell_of = trm_var var_cell_of

let trm_cell ?(mem_typ=mem_typ_any) () =
    trm_apps ~annot:formula_annot trm_cell_of [mem_typ]

let var_uninit_cell_of = toplevel_var "UninitCellOf"
let trm_uninit_cell_of = trm_var var_uninit_cell_of

let trm_uninit_cell ?(mem_typ=mem_typ_any) () =
    trm_apps ~annot:formula_annot trm_uninit_cell_of [mem_typ]

module PatternPre = struct
  include Pattern

  let trm_cell f_mem_typ = trm_apps1 (trm_specific_var var_cell_of) f_mem_typ
  let trm_cell_noid f_mem_typ = trm_apps1 (trm_var_with_name var_cell_of.name) f_mem_typ

  let trm_uninit_cell f_mem_typ = (trm_apps1 (trm_specific_var var_uninit_cell_of) f_mem_typ)
  let trm_uninit_cell_noid f_mem_typ = (trm_apps1 (trm_var_with_name var_uninit_cell_of.name) f_mem_typ)
end

let formula_cell ~mem_typ (addr: trm): formula =
  if !Flags.use_resources_with_models then
    failwith "formula_cell cannot be used when models are enabled";
  formula_repr addr (trm_cell ~mem_typ ())

let formula_points_to ~mem_typ (addr: trm) (value: formula): formula =
  if !Flags.use_resources_with_models then
    formula_repr addr (trm_apps ~annot:formula_annot (trm_cell ~mem_typ ()) [value])
  else
    formula_cell ~mem_typ addr


let formula_cell_inv (t: formula): (trm * mem_typ) option =
  let open Option.Monad in
  let* addr, repr = formula_repr_inv t in
  Pattern.pattern_match_opt repr [
    PatternPre.(trm_cell !__) (fun mem_typ () ->
      Pattern.when_ (not !Flags.use_resources_with_models);
      (addr,mem_typ)
    );
    PatternPre.(trm_apps1 (trm_cell !__) __) (fun mem_typ () ->
      Pattern.when_ (!Flags.use_resources_with_models);
      (addr,mem_typ)
    );
  ]

let formula_points_to_inv (t: formula): (trm * formula * mem_typ) option =
  let open Option.Monad in
  let* addr, repr = formula_repr_inv t in
  Pattern.pattern_match_opt repr [
    PatternPre.(trm_cell !__) (fun mem_typ () ->
      Pattern.when_ (not !Flags.use_resources_with_models);
      (* We need to return a model that will be ignored anyway. *)
      (addr, trm_cell ~mem_typ (), mem_typ)
    );
    PatternPre.(trm_apps1 (trm_cell !__) !__) (fun mem_typ model () ->
      Pattern.when_ (!Flags.use_resources_with_models);
      (addr, model, mem_typ)
    )
  ]


let formula_uninit_cell ?(mem_typ = mem_typ_any) (addr: trm): formula =
  formula_repr addr (trm_uninit_cell ~mem_typ ())

let formula_uninit_cell_inv (t: formula): (trm * typ) option =
  let open Option.Monad in
  let* addr, repr = formula_repr_inv t in
  Pattern.pattern_match_opt repr [
    PatternPre.(trm_uninit_cell !__) (fun mem_typ () ->
      (addr,mem_typ)
    );
  ]

let formula_read_only ~(frac: formula) (res: formula) =
  trm_apps ~annot:formula_annot trm_read_only [frac; res]

let var_free = toplevel_var "Free"
let formula_free (base_ptr: var) (cells: formula) : formula =
  trm_apps ~annot:formula_annot (trm_var var_free) [trm_var base_ptr; cells]

let var_range = toplevel_var "range"
let trm_range = trm_var var_range
let formula_range (start: trm) (stop: trm) (step: trm) =
  trm_apps ~annot:formula_annot trm_range [start; stop; step]

let var_range_plus = toplevel_var "range_plus"
let trm_range_plus = trm_var var_range_plus
let formula_range_plus (start: trm) (len: trm) =
  trm_apps ~annot:formula_annot trm_range_plus [start; len]

let formula_range_plus_inv (t: trm): (trm * trm) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var v }, [base; len]) when var_eq v var_range_plus -> Some (base,len)
  | _ -> None

let var_group = toplevel_var "Group"
let trm_group = trm_var var_group
let formula_group (index: var) (range: trm) (fi: formula) =
  trm_apps ~annot:formula_annot trm_group [range; formula_fun [index, typ_int] fi]

let formula_group_inv (t: trm): (var * trm * formula) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var v }, [range; items]) when var_eq v var_group ->
    begin match trm_fun_inv items with
    | Some ([index, _], _, body, _) -> Some (index, range, body)
    | _ -> None
    end
  | _ -> None

let var_desyncgroup = toplevel_var "DesyncGroup"
let trm_desyncgroup = trm_var var_desyncgroup
let formula_desyncgroup (index: var) (range: trm) (bound: trm) (fi: formula) =
  trm_apps ~annot:formula_annot trm_desyncgroup [range; bound; formula_fun [index, typ_int] fi]

let formula_desyncgroup_inv (t: trm): (var * trm * trm * formula) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var v }, [range; bound; items]) when var_eq v var_desyncgroup ->
    begin match trm_fun_inv items with
    | Some ([index, _], _, body, _) -> Some (index, range, bound, body)
    | _ -> None
    end
  | _ -> None

let var_threadsctx = toplevel_var "ThreadsCtx"

let trm_threadsctx = trm_var var_threadsctx

let formula_threadsctx (range: trm) =
  trm_apps ~annot:formula_annot trm_threadsctx [range]

let formula_threadsctx_inv (t: trm): (trm) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var v }, [range]) when var_eq v var_threadsctx -> Some range
  | _ -> None

let var_sync = toplevel_var "Sync"

let trm_sync = trm_var var_sync

(* NOTE: RANGE HAS BEEN REMOVED !!!
Not needed with DesyncGroups. See proof in Appendix A of spec document. *)
let formula_sync (mem_fn: trm) (res: formula) =
  trm_apps ~annot:formula_annot trm_sync [mem_fn;res]

let formula_sync_inv (t: trm): (trm * formula) option =
  match trm_apps_inv t with
  | Some ({ desc = Trm_var v }, [mem_fn; res]) when var_eq v var_sync -> Some (mem_fn, res)
  | _ -> None

(*let var_into_uninit = toplevel_var "_Uninit"
let trm_into_uninit = trm_var var_into_uninit
let formula_into_uninit (inner_formula: formula): formula =
  trm_apps ~annot:formula_annot trm_into_uninit [inner_formula]*)

let formula_custom_repr_matrix repr (m: trm) (dims: trm list) : formula =
  let indices = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) dims in
  let inner_trm = formula_repr (Matrix_trm.access m dims (List.map trm_var indices)) (repr indices) in
  List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [formula_range (trm_int 0) dim (trm_int 1); formula_fun [idx, typ_int] formula])
    indices dims inner_trm

let formula_reorder_dims_patch ~mem_typ ?(init=true) (m:trm) (dims : trm list) (order: int list) : formula =

  let indices = List.mapi (fun i _ -> new_var (sprintf "i%d" (i+1))) dims in
  let reordered_indices = List.reorder order indices in
  let reordered_dims = List.reorder order dims in
  let cell = if init then fun _ -> trm_cell ~mem_typ () else fun _  ->  trm_uninit_cell ~mem_typ () in
  let inner_trm = formula_repr (Matrix_trm.access m reordered_dims (List.map trm_var reordered_indices)) (cell reordered_indices) in
  List.fold_right2 (fun idx dim formula ->
    trm_apps ~annot:formula_annot trm_group [formula_range (trm_int 0) dim (trm_int 1); formula_fun [idx, typ_int] formula])
    indices dims inner_trm

let formula_matrix ~mem_typ (m: trm) ?(model: formula option) ?(init=true) (dims: trm list) : formula =
  if !Flags.use_resources_with_models then
    let model = Option.unsome_or_else model (fun () -> failwith "Providing a model to formula_matrix is mandatory when models are enabled") in
    formula_custom_repr_matrix (fun indices -> trm_apps (trm_cell ~mem_typ ()) [trm_apps model (List.map trm_var indices)]) m dims
  else
    let cell = if init then fun _ -> (trm_cell ~mem_typ ()) else fun _  -> (trm_uninit_cell ~mem_typ ()) in
    formula_custom_repr_matrix cell m dims

let formula_uninit_matrix ~mem_typ (m: trm) (dims: trm list) : formula =
  formula_custom_repr_matrix (fun _ -> (trm_uninit_cell ~mem_typ ())) m dims

let formula_cell_var ~mem_typ ?(typ : typ option) (x: var): formula =
  formula_cell ~mem_typ (trm_var ?typ:(Option.map typ_ptr typ) x)

let formula_uninit_cell_var ~mem_typ ?(typ : typ option) (x: var): formula =
  formula_uninit_cell ~mem_typ (trm_var ?typ:(Option.map typ_ptr typ) x)

let formula_cells_var ~mem_typ (typ: typ) (x: var) (model: formula): formula =
  match Matrix_trm.typ_matrix_inv typ with
  | Some (base_ty, dims) -> formula_matrix ~mem_typ (trm_var ~typ:(typ_ptr base_ty) x) dims ~model
  | None -> formula_points_to ~mem_typ (trm_var ~typ x) model

let formula_uninit_cells_var ~mem_typ (typ: typ) (x: var): formula =
  match Matrix_trm.typ_matrix_inv typ with
  | Some (base_ty, dims) -> formula_uninit_matrix ~mem_typ (trm_var ~typ:(typ_ptr base_ty) x) dims
  | None -> formula_uninit_cell_var ~mem_typ ~typ x

let var_in_range = toplevel_var "in_range"
let trm_in_range = trm_var var_in_range
let formula_in_range (i: trm) (range: trm) =
  trm_apps trm_in_range [i; range]

let var_is_subrange = toplevel_var "is_subrange"
let trm_is_subrange = trm_var var_is_subrange
let formula_is_subrange (range1: trm) (range2: trm) =
  trm_apps trm_is_subrange [range1; range2]

let var_range_count = toplevel_var "range_count"
let trm_range_count = trm_var var_range_count
let formula_range_count (range: trm) =
  trm_apps trm_range_count [range]

let var_wand = toplevel_var "Wand"

let formula_wand (f_missing : formula) (f_recoverable : formula) : formula =
  trm_apps (trm_var var_wand) [f_missing; f_recoverable]



module Pattern = struct
  include PatternPre

  let formula_repr f_trm f_repr k t =
    match formula_repr_inv t with
    | Some (var, repr) ->
      let k = f_trm k var in
      let k = f_repr k repr in
      k
    | None -> raise Next

  let formula_points_to f_var f_model f_memtype k t =
    match formula_points_to_inv t with
    | Some (var, model, mem_typ) ->
      let k = f_var k var in
      let k = f_model k model in
      let k = f_memtype k mem_typ in
      k
    | None -> raise Next

  let formula_cell f_var f_memtype k t =
    match formula_cell_inv t with
    | Some (var,mem_typ) ->
        let k = f_var k var in
        let k = f_memtype k mem_typ in
        k
    | None -> raise Next

  let formula_uninit_cell f_var f_memtype k t =
    match formula_uninit_cell_inv t with
    | Some (var,mem_typ) ->
        let k = f_var k var in
        let k = f_memtype k mem_typ in
        k
    | None -> raise Next

  let formula_either_cell f_var f_memtype =
    formula_cell f_var f_memtype ^| formula_uninit_cell f_var f_memtype

  let formula_read_only f_frac f_formula =
    trm_apps2 (trm_specific_var var_read_only) f_frac f_formula

  let formula_group f_index f_range f_group_body k t =
    match formula_group_inv t with
    | Some (index, range, body) ->
      let k = f_index k index in
      let k = f_range k range in
      let k = f_group_body k body in
      k
    | None -> raise Next

  let formula_desyncgroup f_index f_range f_bound f_group_body k t =
    match formula_desyncgroup_inv t with
    | Some (index, range, bound, body) ->
      let k = f_index k index in
      let k = f_range k range in
      let k = f_bound k bound in
      let k = f_group_body k body in
      k
    | None -> raise Next

  let formula_range (f_begin: 'a -> trm -> 'b) (f_end: 'b -> trm -> 'c) (f_step: 'c -> trm -> 'd) =
    trm_apps3 (trm_specific_var var_range) f_begin f_end f_step

  let formula_frac_div f_base f_div =
    trm_apps2 (trm_specific_var var_frac_div) f_base f_div

  let formula_frac_sub f_base f_carved =
    trm_apps2 (trm_specific_var var_frac_sub) f_base f_carved

  let formula_is_true f =
    trm_apps1 (trm_specific_var var_is_true) f
end

type read_only_formula = { frac: formula; formula: formula }
let formula_read_only_inv (formula : formula): read_only_formula option =
  Pattern.pattern_match_opt formula [
    Pattern.(formula_read_only !__ !__) (fun frac formula () -> { frac; formula })
  ]

(** Applies a function below a read only wrapper if there is one,
    otherwise simply applies the function. *)
let formula_map_under_read_only (f_map: formula -> formula) (formula: formula) =
  match formula_read_only_inv formula with
  | Some { frac; formula } ->
    formula_read_only ~frac (f_map formula)
  | None -> f_map formula

let formula_read_only_inv_all (formula: formula): read_only_formula =
  Pattern.pattern_match formula [
    Pattern.(formula_read_only !__ !__) (fun frac formula () -> { frac; formula });
    Pattern.__ (fun () -> {frac = full_frac; formula})
  ]

exception CannotTransformIntoUninit of trm

let () = Printexc.register_printer (function
  | CannotTransformIntoUninit formula -> Some (sprintf "Cannot make an uninitialized version of formula %s" (Ast_to_text.ast_to_string formula))
  | _ -> None)

(** Make an uninitialized version of the formula, trying to convert cells into uninitialized cells *)
let rec formula_uninit (formula: formula): formula =
  Pattern.pattern_match formula [
    Pattern.(formula_either_cell !__ !__) (fun addr mem_typ () -> formula_uninit_cell ~mem_typ addr);
    Pattern.(formula_desyncgroup !__ !__ !__ !__) (fun idx range bound sub () -> formula_desyncgroup idx range bound (formula_uninit sub));
    Pattern.(formula_group !__ !__ !__) (fun idx range sub () -> formula_group idx range (formula_uninit sub));
    Pattern.__ (fun () -> raise (CannotTransformIntoUninit formula))
  ]

let rec is_formula_uninit (formula: formula): bool =
  Pattern.pattern_match formula [
    Pattern.(formula_uninit_cell __ __) (fun () -> true);
    Pattern.(formula_desyncgroup __ __ __ !__) (fun sub () -> is_formula_uninit sub);
    Pattern.(formula_group __ __ !__) (fun sub () -> is_formula_uninit sub);
    Pattern.__ (fun () -> false)
  ]

(** Same as [formula_uninit] but works with raw formulas with syntatic sugar and without var ids *)
let rec raw_formula_uninit (formula: formula): formula =
  Pattern.pattern_match formula [
    Pattern.(trm_apps2 (trm_var_with_name var_repr.name) !__ ((trm_uninit_cell_noid !__) ^| (trm_cell_noid !__) ^| trm_apps1 (trm_cell_noid !__) __)) (fun addr mem_typ () ->
      formula_uninit_cell ~mem_typ addr
    );
    Pattern.(trm_apps2 (trm_var_with_name var_group.name) !__ (trm_fun (pair !__ __ ^:: nil) __ !__ __)) (fun range idx sub () -> formula_group idx range (raw_formula_uninit sub));
    Pattern.(trm_apps3 (trm_var_with_name var_desyncgroup.name) !__ !__ (trm_fun (pair !__ __ ^:: nil) __ !__ __)) (fun range bound idx sub () -> formula_desyncgroup idx range bound (raw_formula_uninit sub));
    Pattern.(trm_apps2 (trm_var_with_name var_repr.name) !__ (trm_apps (trm_var !(check (fun v -> String.starts_with ~prefix:"Matrix" v.name))) !__ __ __)) (fun addr matrix_repr matrix_args () ->
      let size_and_mem =
        if !Flags.use_resources_with_models then
          fst (List.unlast matrix_args)
        else matrix_args
      in
      formula_repr addr (trm_apps (trm_var (toplevel_var ("Uninit" ^ matrix_repr.name))) size_and_mem));
    Pattern.(trm_apps2 (trm_var_with_name var_repr.name) __ (trm_apps (trm_var (check (fun v -> String.starts_with ~prefix:"UninitMatrix" v.name))) __ __ __)) (fun () -> formula);
    Pattern.__ (fun () -> raise (CannotTransformIntoUninit  formula))
  ]

let formula_loop_range (range: loop_range): formula =
  if range.direction <> DirUp then failwith "formula_loop_range only supports DirUp";
  formula_range range.start range.stop range.step

let formula_forall_in (index: var) (range: trm) (fi: formula) : formula =
  typ_pure_fun [(index, typ_int); (new_anon_hyp (), formula_in_range (trm_var index) range)] fi

let formula_forall_range (range: loop_range) (fi: formula): formula =
  let range_var = new_var ~namespaces:range.index.namespaces range.index.name in
  let fi = trm_subst_var range.index (trm_var range_var) fi in
  formula_forall_in range_var (formula_loop_range range) fi

let formula_group_range (range: loop_range) : formula -> formula =
  (* FIXME: Need to generalize models ! *)
  formula_map_under_read_only (fun fi ->
    let range_var = new_var ~namespaces:range.index.namespaces range.index.name in
    let fi = trm_subst_var range.index (trm_var range_var) fi in
    formula_group range_var (formula_loop_range range) fi
  )

let formula_desyncgroup_range (range: loop_range) (r_t: trm) : formula -> formula =
  (* FIXME: Need to generalize models ! *)
  (* FIXME: under read only, OK to or should ask for group instead of desyncgroup? IDK *)
  formula_map_under_read_only (fun fi ->
    let range_var = new_var ~namespaces:range.index.namespaces range.index.name in
    let fi = trm_subst_var range.index (trm_var range_var) fi in
    formula_desyncgroup range_var r_t range.stop fi
  )

let formula_matrix_inv (f: formula): (trm * trm list * (trm*mem_typ) option) option =
  let open Option.Monad in
  let rec nested_group_inv (f: formula): (formula * var list * trm list) =
    Pattern.pattern_match f [
      Pattern.(formula_group !__ (formula_range (trm_int (eq 0)) !__ (trm_int (eq 1))) !__)
        (fun idx dim inner_formula () ->
          let inner_formula, indices, dims = nested_group_inv inner_formula in
          (inner_formula, idx::indices, dim::dims)
        );
      Pattern.__ (fun () -> (f, [], []))
    ]
  in
  let inner_formula, indices, dims = nested_group_inv f in

  let has_matching_indices arg_indices exp_indices = match List.for_all2 (fun arg_idx exp_idx ->
      match trm_var_inv arg_idx with
      | Some arg_idx when var_eq exp_idx arg_idx -> true
      | _ -> false
    ) arg_indices exp_indices with
    | v -> v
    | exception Invalid_argument _ -> false
  in

  let* location, repr = formula_repr_inv inner_formula in
  let* model_memtype = Pattern.pattern_match_opt repr [
    Pattern.(trm_uninit_cell __) (fun () -> None);
    Pattern.(trm_cell !__) (fun mem_typ () ->
      Pattern.when_ (not !Flags.use_resources_with_models);
      Some (trm_cell ~mem_typ (), mem_typ)
    );
    Pattern.(trm_apps1 (trm_cell !__) (trm_apps !__ !__ __ __)) (fun mem_typ model args () ->
      Pattern.when_ (!Flags.use_resources_with_models);
      Pattern.when_ (has_matching_indices args indices);
      Some (model, mem_typ)
    )
  ] in
  let* matrix, mindex_dims, mindex_indices = Matrix_trm.access_inv location in
  let* () = if List.length mindex_dims = List.length dims then Some () else None in
  let* () = if List.for_all2 Trm_unify.are_same_trm mindex_dims dims then Some () else None in
  if has_matching_indices mindex_indices indices
    then Some (matrix, dims, model_memtype)
    else None

let var_arith_checked = toplevel_var "__arith_checked"
let formula_arith_checked = trm_var var_arith_checked

(** [filter_common_resources res1 res2] finds all the resources that are common in [res1] and [res2], and returns [common, res1', res2'] where:
  - [comm] are the common resources,
  - [res1'] are the resources from [res1] that are not in [res2],
  - [res2'] are the resources from [res2] that are not in [res1].

  Resources are matched if they have the same formula, the resource names in [comm] are the names from [res1].

  If [~filter_map_left] is provided, for each resource [r] in [res1]:
  - if [filter_map_left r] returns [Some r'], use [r'] in the comparison with resources in [res2], in case of a match put [r'] in [comm], else leave [r] in [res1].
  - if it returns [None], keep the resource separated from the one in [res2].
*)
let filter_common_resources ?(filter_map_left = fun x -> Some x) ?(compare = fun fl fr -> if Trm_unify.are_same_trm fl fr then Some fl else None) (res1: resource_item list) (res2: resource_item list): resource_item list * resource_item list * resource_item list =
  let res2 = ref res2 in
  let rec try_remove_same_formula formula l =
    match l with
    | [] -> None
    | (_, tr as res) :: l ->
      begin match compare formula tr with
      | Some common_formula -> Some (common_formula, l)
      | None ->
        let open Option.Monad in
        let* common_formula, l = try_remove_same_formula formula l in
        Some (common_formula, res::l)
      end
  in
  let common, res1 = List.partition_map
    (fun (x, formula) ->
      match filter_map_left formula with
      | Some formula' ->
        begin match try_remove_same_formula formula' !res2 with
        | None -> Either.Right (x, formula)
        | Some (common_formula, new_res2) -> res2 := new_res2; Either.Left (x, common_formula)
        end
      | None -> Either.Right (x, formula))
    res1
  in
  common, res1, !res2
