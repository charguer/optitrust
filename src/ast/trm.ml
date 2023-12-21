open Ast
open Typ


(* **************************** Trm constructors *************************** *)

(* [trm_annot_default]: default trm annotation *)
let trm_annot_default = {
  trm_annot_attributes = [];
  trm_annot_marks = [];
  trm_annot_labels = [];
  trm_annot_stringrepr = None;
  trm_annot_pragma = [];
  trm_annot_cstyle = [];
  trm_annot_files = [];
}

(* [is_statement_of_desc t_desc]: checks if t_tesc corresponds to a statement or not  *)
let is_statement_of_desc (ty : typ option) (t_desc : trm_desc) : bool =
  match t_desc with
  | Trm_let _ | Trm_let_mult _ | Trm_let_fun _ | Trm_typedef _ | Trm_if _ | Trm_seq _ | Trm_while _
  | Trm_do_while _ | Trm_for_c _ | Trm_for _ | Trm_switch _ | Trm_abort _ | Trm_goto _  -> true
  | Trm_apps _ ->
    begin match ty with
    | Some {typ_desc = Typ_unit ; _} -> true
    | _ -> false
    end
  | _ -> false

(* [trm_build ~annot ?loc ~is_statement ?typ ?ctx ~desc ()]: builds trm [t] with its fields given as arguments. *)
let trm_build ~(annot : trm_annot) ?(loc : location) ~(is_statement : bool) ?(typ : typ option)
  ?(ctx : ctx = unknown_ctx ()) ~(desc : trm_desc) () : trm =
  let t = {annot; loc; is_statement; typ; desc; ctx} in
  Stats.incr_trm_alloc ();
  t

(* [trm_make ~annot ?loc ~is_statement ?typ ?ctx desc]: builds trm [t] with description [desc] and other fields given
    as default ones. *)
let trm_make ?(annot : trm_annot = trm_annot_default) ?(loc : location) ?(is_statement : bool option)
    ?(typ : typ option) ?(ctx : ctx option) (desc : trm_desc) : trm =
   let is_statement =
     match is_statement with
     | Some b -> b
     | None -> is_statement_of_desc typ desc
     in
   trm_build ~annot ~desc ?loc ~is_statement ?typ ?ctx ()


(* [trm_alter ~annot ?loc ?is_statement ?typ ?ctx ?desc t]: alters any of the fields of [t] that was provided as argument. *)
let trm_alter ?(annot : trm_annot option) ?(loc : location option) ?(is_statement : bool option)
 ?(typ : typ option) ?(ctx : ctx option) ?(desc : trm_desc option) (t : trm) : trm =
    let annot = match annot with Some x -> x | None -> t.annot in
    let loc = match loc with Some x -> x | None -> t.loc in
    let typ = match typ with | None -> t.typ | _ -> typ in
    let is_statement = match is_statement with
      | Some x -> x
      | None -> match desc with
                | Some d -> is_statement_of_desc typ d
                | None -> t.is_statement
      in
    let ctx = Option.value ~default:t.ctx ctx in
    let desc = match desc with | Some x -> x | None -> t.desc in
    trm_build ~annot ~desc ?loc ~is_statement ?typ ~ctx ()

(* [trm_replace desc t]: an alias of [trm_alter] to alter only the descriptiong of [t]. *)
let trm_replace (desc : trm_desc) (t : trm) : trm =
  trm_alter ~desc t

(* [trm_like]: copies the annotations, location and type of the old trm into a new trm *)
let trm_like ~(old:trm) (t:trm): trm =
  trm_alter ~annot:old.annot ~loc:old.loc ?typ:old.typ t

(* **************************** CStyle *************************** *)

(* [trm_get_cstyles t]: returns all cstyle annotations of trm [t]. *)
let trm_get_cstyles (t : trm) : cstyle_annot list =
  t.annot.trm_annot_cstyle

(* [apply_on_cstyles f t]: applies [f] on the cstyme encodings of [t]. *)
let apply_on_cstyles (f : cstyle_annot list -> cstyle_annot list) (t : trm) : trm =
  let t_annot_cstyle = f (trm_get_cstyles t) in
  let t_annot = {t.annot with trm_annot_cstyle=t_annot_cstyle} in
  trm_alter ~annot:t_annot t

(* [trm_add_cstyle cs t]: adds [cs] cstyle annotation to trm [t]. *)
let trm_add_cstyle (cs : cstyle_annot) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> cs :: cstyles) t

(* [trm_filter_cstyle pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
let trm_filter_cstyle (pred : cstyle_annot -> bool) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> List.filter (fun cs -> pred cs) cstyles) t

(* [trm_rem_cstyle cs t]: removes the cstyle_annot annotation [cs] from trm [t]. *)
let trm_rem_cstyle (cs : cstyle_annot) (t : trm) : trm =
  trm_filter_cstyle (fun cs1 -> cs <> cs1) t

(* [trm_has_cstyle cs t]: checks if [t] has the [cs] cstyle annotation. *)
let trm_has_cstyle (cs : cstyle_annot) (t : trm) : bool =
  let cstyles = trm_get_cstyles t in
  List.mem cs cstyles

(* [annot_has_cstyle cs t_ann]: checks if [cs] is constained in [t_ann]. *)
let annot_has_cstyle (cs : cstyle_annot) (t_ann : trm_annot) : bool =
  let cstyles = t_ann.trm_annot_cstyle in
  List.mem cs cstyles


(* **************************** Smart constructors *************************** *)

(* [trm_val ~annot ?loc ?typ ~ctx y]: value *)
let trm_val ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (v : value) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_val v)

(** [trm_var]: create a variable occurence. *)
let trm_var ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
?(kind : varkind = Var_mutable) (v : var) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_var (kind, v))

(** Creates a new variable, using a fresh identifier. *)
let new_var ?(qualifier : string list = []) (name : string) : var =
  let id = next_var_int () in { qualifier; name; id }

(** Refers to a variable by name, letting its identifier be inferred.
    This variable cannot be stored in a [varmap] before its identifier is inferred. *)
let name_to_var ?(qualifier : string list = []) (name : string) : var =
  { qualifier; name; id = inferred_var_id }

let dummy_var = { qualifier = []; name = ""; id = dummy_var_id }

(** Map of toplevel variables. Note that some of them can be declared but never defined. *)
let toplevel_vars: var_id Qualified_map.t ref = ref Qualified_map.empty

(* TODO: Decide if we want to change the pattern to fail if the toplevel_var already exists. *)
(** [toplevel_var]: return the toplevel variable with the given name
  A new variable identifier is predeclared if the variable did not exist.
    *)
let toplevel_var ?(qualifier : string list = []) (name : string) : var =
  match Qualified_map.find_opt (qualifier, name) !toplevel_vars with
  | None ->
    let v = new_var ~qualifier name in
    toplevel_vars := Qualified_map.add (qualifier, name) v.id !toplevel_vars;
    v
  | Some id -> { qualifier; name; id }

let trm_toplevel_var ?(qualifier : string list = []) (name : string) : trm =
  trm_var (toplevel_var ~qualifier name)

(* [trm_array ~annot ?loc ?typ ?ctx tl]: array initialization list *)
let trm_array ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tl : trm mlist) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_array tl)

(* [trm_record ~annot ?loc ?typ ?ctx tl]: struct initialization list *)
let trm_record ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tl : (label option * trm) mlist) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_record tl)

(* [trm_let ~annot ?loc ?ctx kind typed_var init]: variable declaration *)
let trm_let ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (kind : varkind) (typed_var : typed_var) (init : trm): trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let (kind, typed_var, init))

(* [trm_let ~annot ?loc ?ctx kind ty vl tl]: multiple variable declarations *)
let trm_let_mult ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (kind : varkind)
   (tvl : typed_vars) (tl : trms) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let_mult (kind, tvl, tl))

(* [trm_let ~annot ?loc ?ctx name ret_typ args body]: function definition *)
let trm_let_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: fun_spec = FunSpecUnknown)
  (v : var) (ret_typ : typ) (args : typed_vars) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let_fun (v, ret_typ, args, body, contract))

(* [trm_fun ~annot ?loc args ret_typ body]: anonymous function.  *)
let trm_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: fun_spec = FunSpecUnknown)
  (args : typed_vars) (ret_typ : typ option) (body : trm) =
  trm_make ~annot ?loc ?ctx (Trm_fun (args, ret_typ, body, contract))

let trm_fun_inv (t: trm) : (typed_vars * typ option * trm * fun_spec) option =
    match t.desc with
    | Trm_fun (args, typ, body, contract) -> Some (args, typ, body, contract)
    | _ -> None

(* [trm_typedef ~annot ?loc ?ctx def_typ]: type definition *)
let trm_typedef ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (def_typ : typedef): trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_typedef def_typ)

(* [trm_if ~annot ?loc ?ctx cond tb eb]: if statement *)
let trm_if ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm)
  (tb : trm) (eb : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_if (cond, tb, eb))

(* [trm_seq ~annot ?loc ?ctx tl]: block statement *)
let trm_seq ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (tl : trm mlist) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_seq tl)

(* [trm_seq_nomarks ~annot ?loc ?ctx tl]: like [trm_seq] but takes
   a list as arguments --LATER: use it everywhere it should instead of
  [trm_seq (Mlist_of_list tl)] *)
let trm_seq_nomarks ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (tl : trms) : trm =
  trm_seq ~annot ?loc ?ctx (Mlist.of_list tl)

(* [trm_apps ~annot ?loc ?typ ?ctx f args]: function call *)
let trm_apps ?(annot = trm_annot_default) ?(loc) ?(typ) ?(attributes = [])
  ?(ctx : ctx option) ?(ghost_args = []) (f : trm) (args : trms) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_apps (f, args, ghost_args))

(* [trm_while ~annot ?loc ?ctx cond body]: while loop *)
let trm_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_while (cond, body))

(* [trm_do_while ~annot ?loc ?ctx cond body]: do while loop *)
let trm_do_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)  (body : trm) (cond : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_do_while (body, cond))

(* [trm_for_c ?annot ?loc ?ctx init cond step body]: for loop *)
let trm_for_c ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(invariant: resource_set option) (init : trm) (cond : trm)
  (step : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_for_c (init, cond, step, body, invariant))

(* [trm_switch ~annot ?loc ?ctx cond cases]: switch statement *)
let trm_switch ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm)
  (cases : (trms * trm) list) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_switch (cond, cases))

(* [trm_abort ~annot ?loc ?ctx a]: abort instruction *)
let trm_abort ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (a : abort) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_abort a)

(* [trm_goto ~annot ?loc ?ctx l]: goto statement *)
let trm_goto ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (l : label) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_goto l)

(* [trm_uninitialized ~annot ?loc ?ctx ()]: used for variable declarations without initialization
    and function declarations *)
let trm_uninitialized ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) () : trm =
  trm_make ~annot ?loc ?ctx (Trm_val (Val_lit (Lit_uninitialized)))

(* [trm_for ~annot ?loc ?ctx index start direction stop step body]: simple for loop *)
let trm_for ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: loop_spec)
  (loop_range : loop_range) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_for (loop_range, body, contract))

let trm_for_instrs ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: loop_spec)
(loop_range : loop_range) (body_instrs : trm mlist) : trm =
  trm_for ~annot ?loc ?ctx ?contract loop_range (trm_seq body_instrs)

(* [code code_str ]: arbitrary code entered by the user *)
let code (code_str : code_kind) : trm =
  trm_make (Trm_arbitrary code_str)

(* [trm_omp_routine ?loc omp_routine] OpenMP routine *)
let trm_omp_routine ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (omp_routine : omp_routine) : trm =
  trm_make ~annot ?loc ?ctx (Trm_omp_routine omp_routine)

(* [extern ?loc ~lang tl]: extern *)
let trm_extern ?(annot = trm_annot_default) ?(loc) (lang : string) (tl : trms) : trm =
  trm_make ~annot ?loc  (Trm_extern (lang, tl))

(* [trm_namespace ?loc ?ctx name t inline ]: namespace *)
let trm_namespace ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (name : string) (t : trm ) (inline : bool) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_namespace (name, t, inline))

(* [trm_template ?loc ?typ ?ctx tpl t]: template statemented *)
let trm_template ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tpl : template_parameter_list) (t : trm ) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_template (tpl, t))

(* [trm_using_directive ~annot ?loc ?typ ?ctx namespace]: creates a using namespace directive. *)
let trm_using_directive ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (namespace : string) =
  trm_make ~annot ?loc ?typ ?ctx (Trm_using_directive namespace)

(* NOTE: var id = inferred_var_id *)
let var_this = name_to_var "this"

(* [trm_this ~annot ?loc ?typ ?ctx ()]: this pointer.
   NOTE: var id = inferred_var_id *)
let trm_this ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) () =
  trm_make ~annot ?loc ?typ ?ctx (Trm_var (Var_immutable, var_this))

(* [trm_delete ~annot ?loc ?typ ?ctx is_array_form t]: delete operator  *)
let trm_delete ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (is_array_form : bool) (t : trm) =
  trm_make ~annot ?loc ?typ ?ctx (Trm_delete (is_array_form, t))

(* ********************************** Auxiliary functions ************************************ *)

(* [trm_unop ~annot ?loc ?ctx p]: unary operator *)
let trm_unop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : unary_op) : trm =
  trm_val ~annot ?loc ?ctx (Val_prim (Prim_unop p))

(* [trm_biop ~annot ?loc ?ctx p]: binary operator *)
let trm_binop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : binary_op) : trm =
  trm_val ~annot:annot ?loc ?ctx (Val_prim (Prim_binop p))

(* [trm_cast ty t]: type cast *)
let trm_cast ?(annot : trm_annot = trm_annot_default) (ty : typ) (t : trm) : trm =
  trm_apps ~annot (trm_unop (Unop_cast ty)) [t]

(* [typ_of_lit l]: get the type of a literal *)
let typ_of_lit (l : lit) : typ option =
  match l with
  | Lit_unit -> Some (typ_unit ())
  | Lit_uninitialized -> None
  | Lit_bool _ -> Some (typ_bool ())
  | Lit_int _ -> Some (typ_int ())
  | Lit_double _ -> Some (typ_double ())
  | Lit_string _ -> Some (typ_string ())
  | Lit_nullptr -> Some (typ_unit ())

(* [trm_lit ~annot ?loc ?ctx l]: literal *)
let trm_lit ?(typ : typ option = None) ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (l : lit) : trm =
  let typ = Xoption.or_ typ (typ_of_lit l) in
  trm_val ~annot:annot ?loc ?ctx ?typ (Val_lit l)

let trm_unit ?(loc) () : trm =
  trm_lit ?loc (Lit_unit)
let trm_bool ?(loc) (b : bool) =
  trm_lit ?loc (Lit_bool b)
(* LATER: allow arbitrary sized integer types/values *)
let trm_int ?(loc) (i : int) =
  trm_lit ?loc (Lit_int i)
(* LATER: may need arbitrary sized float values *)
let trm_float ?(typ : typ = typ_float ()) ?(loc) (d : float) =
  trm_lit ~typ:(Some typ) ?loc (Lit_double d)
let trm_double ?(loc) (d : float) =
  trm_lit ?loc (Lit_double d)
let trm_string ?(loc) (s : string) =
  trm_lit ?loc (Lit_string s)

(* [trm_null ~annot ?loc ?ctx ()]: build the term [nullptr], or [NULL] if [~uppercase:true]
   (also used for [void* 0] by Menhir, but decoded in cMenhir_to_ast)  *)
let trm_null ?(uppercase : bool = false) ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (_ : unit) : trm =
  let t = trm_lit ?loc ?ctx Lit_nullptr in
  if uppercase then trm_add_cstyle Display_null_uppercase t else t

let var_free = toplevel_var "free"

(* [trm_free]: build a term calling the 'free' function. *)
let trm_free ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (memory : trm) : trm =
  trm_apps ~annot ?loc ?ctx (trm_var var_free) [memory]

(* [trm_prim ~annot ?loc ?ctx p]: primitives *)
let trm_prim ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : prim) : trm =
  trm_val ~annot:annot ?loc ?ctx (Val_prim p)

(* [trm_set ~annot ?loc ?ctx t1 t2] *)
let trm_set ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  ?(typ : typ option = Some (typ_unit ()))  (lhs : trm) (rhs : trm) : trm =
  trm_apps ~annot:annot ?loc ?ctx ?typ (trm_binop Binop_set) [lhs; rhs]

(* [trm_neq ~annot ?loc ?ctx t1 t2] *)
let trm_neq ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ?loc ?ctx ~typ:(typ_unit ()) (trm_binop Binop_neq) [t1; t2]

type ghost_call = {
  ghost_fn: trm;
  ghost_args: (var * formula) list;
}

let trm_ghost ({ ghost_fn; ghost_args } : ghost_call): trm =
  trm_add_cstyle GhostCall (trm_apps ghost_fn [] ~ghost_args)

let ghost_call (ghost_var: var) (ghost_args: (string * formula) list): ghost_call =
  { ghost_fn = trm_var ghost_var; ghost_args = List.map (fun (g, t) -> (name_to_var g, t)) ghost_args }

let ghost_closure_call contract body =
  { ghost_fn = trm_fun [] None ~contract:(FunSpecContract contract) body; ghost_args = [] }

(*****************************************************************************)

(* [trm_get_attr t]: returns all the attributes of trm [t]. *)
let trm_get_attr (t : trm) : attribute list =
  t.annot.trm_annot_attributes

(* [trm_attr_add att t]: adds attribute [att] to trm [t] *)
let trm_attr_add (att : attribute) (t : trm) : trm =
  let t_annot_attributes = t.annot.trm_annot_attributes in
  let t_annot = {t.annot with trm_annot_attributes = att :: t_annot_attributes} in
  trm_alter ~annot:t_annot t


(*****************************************************************************)


(* ********************************** Annotation manipulation ************************************ *)
(**** Attributes  ****)



(**** Labels  ****)

(* [trm_get_labels t]: gets all the labels of trm [t]. *)
let trm_get_labels (t : trm) =
  t.annot.trm_annot_labels

 (* [apply_on_labels f t]: applies [f] on the labels of [t]. *)
 let apply_on_labels (f : marks -> marks) (t : trm) : trm =
   let t_labels = trm_get_labels t in
   let t_annot_labels = f t_labels in
   let t_annot = {t.annot with trm_annot_labels = t_annot_labels} in
   trm_alter ~annot:t_annot t

 (* [trm_add_label l] adds label [l] to trm [t].
    Returns [t] unchanged if [l = ""]. *)
let trm_add_label (l : label) (t : trm) : trm =
  if l = "" then t else apply_on_labels (fun labels -> l :: labels) t

 (* [trm_filter_label pred t]: filters all labels that satisfy predicate [pred]. *)
 let trm_filter_label (pred : label -> bool) (t : trm) : trm =
   apply_on_labels (fun labels -> List.filter (fun l -> pred l) labels) t

 (* [trm_rem_label l t]: removes label [l] from trm [t]. *)
 let trm_rem_label (l : label) (t : trm) : trm =
   trm_filter_label (fun l1 -> l <> l1) t

 (* [trm_rem_labels t]: removes all the labels from trm [t]. *)
 let trm_rem_labels (t : trm) : trm =
   apply_on_labels (fun _ -> []) t

 (* [trm_has_label l t]: checks if trm [t] has label [l]. *)
 let trm_has_label (l : label) (t : trm) : bool =
   let t_labels = trm_get_labels t in
   List.mem l t_labels

 (* [trm_pass_labels t1 t2]: passes the labels of trm [t1] to trm [t2]. *)
 let trm_pass_labels (t1 : trm) (t2 : trm) : trm =
   let t1_labels = trm_get_labels t1 in
   let t2_labels = trm_get_labels t2 in
   let t2_annot = {t2.annot with trm_annot_labels = t2_labels @ t1_labels} in
   {t2 with annot = t2_annot}

 (**** Stringrepr  ****)

 (* [trm_set_stringreprid id t]: sets the string representation id [t] to [id]. *)
 let trm_set_stringreprid (id : stringreprid) (t : trm) : trm =
   let annot = {t.annot with trm_annot_stringrepr = Some id} in
   trm_alter ~annot t

 (* [trm_get_stringreprid t]: gets the string representation of trm [t]. *)
 let trm_get_stringreprid (t : trm) : stringreprid option =
   t.annot.trm_annot_stringrepr


 (**** CPragmas  ****)

 (* [apply_on_pragmas f t]: applies [f] on the pragma directives associated with [t]. *)
 let apply_on_pragmas (f : cpragma list -> cpragma list) (t : trm) : trm =
   let t_annot_pragmas = f (t.annot.trm_annot_pragma) in
   let annot = {t.annot with trm_annot_pragma = t_annot_pragmas} in
   trm_alter ~annot t

 (* [trm_add_pragma p t]: adds the pragma [p] into [t]. *)
 let trm_add_pragma (p : cpragma) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> p :: pragmas) t

 let trm_add_pragmas (p : cpragma list) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> p @ pragmas) t

 (* [trm_filter_pragma pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
 let trm_filter_pragma (pred : cpragma -> bool) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> List.filter (fun p -> pred p) pragmas) t

 (* [trm_rem_pragma p t]: removes the pragma [p] from [t]. *)
 let trm_rem_pragma (p : cpragma) (t : trm) : trm =
   trm_filter_pragma (fun p1 -> p <> p1) t

 (* [trm_get_pragmas t]: returns all the pragmas annotated to [t]. *)
 let trm_get_pragmas (t : trm) : cpragma list =
   t.annot.trm_annot_pragma

 (* [trm_has_pragma pred t]: check if [t] has pragmas that satisfy [pred]. *)
 let trm_has_pragma (pred : cpragma -> bool) (t : trm) : bool =
   let t_pragmas = trm_get_pragmas t in
   List.exists pred t_pragmas

 (* [trm_pass_pragmas t1 t2]: pass pragmas of trm [t1] to trm [t2]. *)
 let trm_pass_pragmas (t1 : trm) (t2 : trm) : trm =
   let t1_pragmas = trm_get_pragmas t1 in
   let t2_pragmas = trm_get_pragmas t2 in
   let t2_annot = {t2.annot with trm_annot_pragma = t1_pragmas @ t2_pragmas} in
   {t2 with annot = t2_annot}

 (**** Files  ****)

 (* [trm_get_files_annot t]: returns all file annotations of trm [t]. *)
 let trm_get_files_annot (t : trm) : files_annot list =
   t.annot.trm_annot_files

 (* [trm_set_mainfile]: adds [Main_file] annotation to trm [t]. *)
 let trm_set_mainfile (t : trm) : trm =
    let t_files = trm_get_files_annot t in
    let t_annot_files = Main_file :: t_files in
    let annot = {t.annot with trm_annot_files=t_annot_files} in
    trm_alter ~annot t

 (* [trm_set_include filename t]: add [Include filename] annotation to trm [t]. *)
 let trm_set_include (filename : string) (t : trm) : trm =
   let t_files = trm_get_files_annot t in
   let t_annot_files = Include filename :: t_files in
   let annot = {t.annot with trm_annot_files = t_annot_files} in
   trm_alter ~annot t

 (* [trm_is_mainfile t]: checks if [t] contains the [Main_file] annotation. *)
 let trm_is_mainfile (t : trm) : bool =
   let t_files = trm_get_files_annot t in
   List.mem Main_file t_files

 (* [trm_is_include]: checks if [t] contains the [Include f] annotation. *)
 let trm_is_include (t : trm) : bool =
   let t_files = trm_get_files_annot t in
   List.exists (function |Include _ -> true | _ -> false) t_files

 (* [trm_is_nobrace_seq t]: checks if [t] is a visible sequence or not *)
 let trm_is_nobrace_seq (t : trm) : bool =
   List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle

(* [trm_vardef_get_trm_varse]: gets the singleton declaration variable in the case when [t] is a variable declaration
    or a list of variable in the case when we have multiple variable declarations in one line *)
let rec trm_vardef_get_vars (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x, _), _) -> [x]
  | Trm_seq tl when trm_has_cstyle Multi_decl t -> List.flatten (List.map trm_vardef_get_vars (Mlist.to_list tl))
  | _ -> []

(* [trm_ret ~annot a]; special trm_abort case, used for return statements *)
let trm_ret ?(annot = trm_annot_default) ?loc ?ctx (a : trm option) : trm =
  trm_abort ~annot ?loc ?ctx (Ret a)

(* [trm_prim_inv t]: gets the primitive operation *)
let trm_prim_inv (t : trm) : prim option =
  match t.desc with
  | Trm_val (Val_prim p) -> Some p
  | _ -> None

(* [trm_lit_inv t]: gets the literal from a literal trm *)
let trm_lit_inv (t : trm) : lit option =
  match t.desc with
  | Trm_val (Val_lit v) -> Some v
  | _ -> None

(* [trm_int_inv t] gets an int literal from a trm *)
let trm_int_inv (t : trm) : int option =
  match trm_lit_inv t with
  | Some (Lit_int n) -> Some n
  | _ -> None


(* [trm_inv ~error k t]: returns the results of applying [k] on t, if the result is [None]
     then function fails with error [error]. *)
let trm_inv ?(error : string = "") (k : trm -> 'a option) (t : trm) : 'a =
  match k t with
  | None -> trm_fail t (if error = "" then "failed inversion" else error)
  | Some r -> r



(* [trm_let_inv t]: returns the components of a [trm_let] constructor if [t] is a let declaration.
     Otherwise it returns [None]. *)
let trm_let_inv (t : trm) : (varkind * var * typ * trm) option =
  match t.desc with
  | Trm_let (vk, (x, tx), init) -> Some (vk, x, tx, init)
  | _ -> None

(* [trm_let_mult_inv t]: returns the components of a [trm_let_mult] constructor if [t] is a multiple let declaration.
     Otherwise it returns [None]. *)
let trm_let_mult_inv (t : trm) : (varkind * typed_vars * trm list) option =
  match t.desc with
  | Trm_let_mult (vk, tvs, inits) -> Some (vk, tvs, inits)
  | _ -> None

(* [trm_let_fun_inv t]: returns the componnets of a [trm_let_fun] constructor if [t] is a function declaration.
     Otherwise it returns a [None]. *)
let trm_let_fun_inv (t : trm) : (var * typ * typed_vars * trm) option =
  match t.desc with
  | Trm_let_fun (f, ret_ty, args, body, _) -> Some (f, ret_ty, args, body)
  | _ -> None


(* [trm_apps_inv t]: returns the components of a [trm_apps] constructor in case [t] is function application.
    Otherwise it returns [None]. *)
let trm_apps_inv (t : trm) : (trm * trm list) option =
  match t.desc with
  | Trm_apps (f, tl, _) -> Some (f, tl)
  | _ -> None

(* [trm_apps_inv_ghosts t]: returns the components of a [trm_apps] constructor in case [t] is function application.
    Otherwise it returns [None]. *)
let trm_apps_inv (t : trm) : (trm * trm list) option =
  match t.desc with
  | Trm_apps (f, tl, _) -> Some (f, tl)
  | _ -> None

(* [trm_seq_inv t]: returns the components of a [trm_seq] constructor when [t] is a sequence.
    Otherwise it returns [None]. *)
let trm_seq_inv (t : trm) : (trm mlist) option =
  match t.desc with
  | Trm_seq tl ->  Some tl
  | _ -> None

let trm_seq_nth_inv (i : int) (t : trm) : trm option =
  Option.bind (trm_seq_inv t) (fun instrs ->
    if i < Mlist.length instrs
    then Some (Mlist.nth instrs i)
    else None
  )

(* [trm_val_inv t]: returns the components of a [trm_val] constructor when [t] is a value.
    Otherwise it returns [None]. *)
let trm_val_inv (t: trm): value option =
  match t.desc with
  | Trm_val v -> Some v
  | _ -> None

(* [trm_var_inv t]: returns the components of a [trm_var] constructor when [t] is a variable occurrence.
    Otherwise it returns [None]. *)
let trm_var_inv (t : trm) : var option =
  match t.desc with
  | Trm_var (_, x) -> Some x
  | _ -> None

(* [trm_free_inv]: deconstructs a 'free(x)' call. *)
let trm_free_inv (t : trm) : trm option =
  match trm_apps_inv t with
  | Some (f, [x]) ->
    begin match trm_var_inv f with
    | Some f_var when var_eq f_var var_free -> Some x
    | _ -> None
    end
  | _ -> None

(* [trm_if_inv t]: returns the components of a [trm_if] constructor when [t] is an if statement.
    Otherwise it returns [None]. *)
let trm_if_inv (t : trm) : (trm * trm * trm) option =
  match t.desc with
  | Trm_if (cond, then_, else_) -> Some (cond, then_, else_)
  | _ -> None

(* [trm_typedef_inv t]: returns the components of a [trm_typedef] constructor when [t] is a type definition. *)
let trm_typedef_inv (t : trm) : typedef option =
  match t.desc with
  | Trm_typedef td -> Some td
  | _ -> None

(* [trm_unop_inv t]: deconstructs t = op t1 *)
let trm_unop_inv (t : trm) : (unary_op * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (Prim_unop op), [a] -> Some (op, a)
    | _ -> None
    end
  | _ -> None

(* [trm_binop_inv t]: deconstructs t = t1 op t2 *)
let trm_binop_inv (op : binary_op) (t : trm) : (trm * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (Prim_binop op'), [a; b] when op = op' -> Some (a, b)
    | _ -> None
    end
  | _ -> None

let trm_cast_inv (t : trm) : (typ * trm) option =
  match trm_unop_inv t with
  | Some (Unop_cast ty, t2) -> Some (ty, t2)
  | _ -> None

let trm_get_inv (t : trm) : trm option =
  match trm_unop_inv t with
  | Some (Unop_get, t2) -> Some t2
  | _ -> None

let trm_var_get_inv (t : trm) : var option =
  match trm_get_inv t with
  | Some t2 -> trm_var_inv t2
  | None -> None

(* [trm_prod_inv t]: gets a the list of factors involved in a multiplication*)
let trm_prod_inv (t : trm) : trm list =
  let rec aux (indepth : bool) (acc : trm list) (t : trm) : trm list =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_mul))); _}, [l; r], _) -> (aux true acc l) @ (aux true acc r)
    | _ -> if indepth then acc @ [t] else acc
  in aux false [] t

(* [trm_mlist_inv_marks t] gets the description of marks in a term that
   contains a MList, for example [Trm_seq], [Trm_array], or [Trm_record]. *)
let trm_mlist_inv (t : trm) : mark list list option =
  match t.desc with
  | Trm_seq tl | Trm_array tl -> Some tl.marks
  | Trm_record tl -> Some tl.marks
  | _ -> None



(* [decl_name t]: returns the name of the declared variable/function. *)
let decl_name (t : trm) : var option =
  match t.desc with
  | Trm_let (_,(x,_),_) -> Some x
  | Trm_let_fun (f, _, _, _, _) -> Some f
  | _ -> None

(* [vars_bound_in_trm_init t]: gets the list of variables that are bound inside the initialization trm of the for_c loop*)
let vars_bound_in_trm_init (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x,_), _) -> [x]
  | Trm_let_mult (_, tvl, _) -> fst (List.split tvl)
  | _ -> []

(* [is_null_pointer ty t]: check if t == (void * ) 0 *)
let is_null_pointer (ty : typ) (t : trm) : bool =
  match ty.typ_desc, t.desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = {typ_desc = Typ_unit;_}}, Trm_val (Val_lit (Lit_int 0)) -> true
  | _ -> false



(* [get_init_val t]: gets the value of a variable initialization *)
let rec get_init_val (t : trm) : trm option =
  match t.desc with
  | Trm_let (vk, (_, _), init) ->
      begin match vk with
      | Var_immutable -> Some init
      | _ -> get_init_val init
      end
  | Trm_apps(f,[base], _) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_new _)) -> Some base
        | _ -> Some t
        end
  | Trm_val (Val_prim (Prim_new _))  -> None
  | _ ->
      Some t

(* [for_loop_index t]: returns the index of the loop [t] *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for (l_range,  _, _) ->
     let (index, _, _, _, _, _) = l_range in
     index
  | Trm_for_c (init, _, _, _, _) ->
     (* covered cases:
        - for (i = …; …)
        - for (int i = …; …) *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [{desc = Trm_var (_, x); _}; _], _) -> x
     | _ -> begin match trm_var_inv init with
            | Some x -> x
            | None -> trm_fail init "Ast.for_loop_index: could't get the loop index"
            end
     end
  | _ -> trm_fail t "Ast.for_loop_index: expected for loop"

(* [for_loop_init t]: returns the initial value of the loop index *)
let for_loop_init (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, _, _, _, _) ->
     (* covered cases:
        - for (i = n; …)
        - for (int i = n; …) *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; n], _) -> n
     | Trm_let (_,(_, _), init) ->
        begin match get_init_val init with
        | Some v  -> v
        | None -> trm_fail init "Ast.for_loop_init: bad for loop initialization"
        end
     | _ -> trm_fail init "Ast.for_loop_init: bad for loop initialisation"
     end
  | _ -> trm_fail t "Ast.for_loop_init: expected for loop"

(* [for_loop_bound t]: returns the bound of the for loop *)
let for_loop_bound (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, cond, _, _, _) ->
     (* covered cases:
       - for (…; i < n; …)
       - for (…; i <= n; …)
       - for (…; i > n; …)
       - for (…; i >= n; …) *)
     begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                 [_; n], _) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
                 [_; n], _) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
                 [_; n], _) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _},
                 [_; n], _) -> n
     | _ -> trm_fail cond "Ast.for_loop_bound: bad for loop condition"
     end
  | _ -> trm_fail t "Ast.for_loop_bound: expected for loop"

(* [for_loop_step t]: returns the step increment of the for loop *)
let for_loop_step (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, _, step, _, _) ->
     (* covered cases:
        - for (…; …; i++)
        - for (…; …; ++i)
        - for (…; …; i--)
        - for (…; …; --i)
        - for (…; …; i += n) for n > 0
        - for (…; …; i -= n) for n > 0 *)
     begin match step.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_inc)); _}, _, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_inc)); _}, _, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_dec)); _}, _, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_dec)); _}, _, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; t'], _) ->
        begin match t'.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_add)); _},
                    [_; n], _) ->
           n
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_sub)); _},
                    [_; n], _) ->
           trm_apps (trm_unop Unop_minus) [n]
        | _ -> trm_fail step "Ast.for_loop_step: bad for loop step"
        end
     | _ -> trm_fail step "Ast.for_loop_step: bad for loop step"
     end
  | _ -> trm_fail t "Ast.for_loop_step: expected for loop"

(* [for_loop_nb_iter t]: gets the number of iterations of a for loop *)
let for_loop_nb_iter (t : trm) : trm =
  let init = for_loop_init t in
  let bound = for_loop_bound t in
  let step = for_loop_step t in
  (* reorder to use positive step *)
  let (init, bound, step) =
    match step.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_minus)); _}, [step'], _) ->
       (bound, init, step')
    | _ -> (init, bound, step)
  in
  match init.desc, bound.desc, step.desc with
  (* if all vars are integers, perform the computation to simplify *)
  | Trm_val (Val_lit (Lit_int init)), Trm_val (Val_lit (Lit_int bound)),
    Trm_val (Val_lit (Lit_int step)) ->
     trm_lit (Lit_int ((bound - init + step - 1) / step))
  (* otherwise, use the same formula with terms *)
  | _ ->
     trm_apps (trm_binop Binop_div)
       [
         trm_apps (trm_binop Binop_sub)
           [
             trm_apps (trm_binop Binop_add)
               [
                 trm_apps (trm_binop Binop_sub)
                   [
                     bound;
                     init
                   ];
                 step
               ];
             trm_lit (Lit_int 1)
           ];
         step
       ]

(* [for_loop_body_trms t]: gets the list of trms from the body of the loop *)
let for_loop_body_trms (t : trm) : trm mlist =
  match t.desc with
  | Trm_for (_, body, _) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> trm_fail body "Ast.for_loop_body_trms: body of a simple loop should be a sequence"
    end
  | Trm_for_c (_, _, _, body, _) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> trm_fail body "Ast.for_loop_body_trms: body of a generic loop should be a sequence"
    end
  | _ -> trm_fail t "Ast.for_loop_body_trms: expected a loop"

(*****************************************************************************)

(* [trm_main_inv_toplevel_defs ast]: returns a list of all toplevel declarations *)
let trm_main_inv_toplevel_defs (ast : trm) : trm list =
match ast.desc with
| Trm_seq tl when trm_is_mainfile ast -> Mlist.to_list tl
| _ -> trm_fail ast "Ast.trm_main_inv_toplevel_defs: expected the ast of the main file"

(* [trm_seq_add_last t_insert t]: appends [t_insert] at the end of the sequence [t] *)
let trm_seq_add_last (t_insert : trm) (t : trm) : trm =
match t.desc with
| Trm_seq tl ->
 let new_tl = Mlist.insert_at (Mlist.length tl) t_insert tl in
 trm_seq ~annot:t.annot new_tl
| _ -> trm_fail t "Ast.trm_seq_add_last: expected a sequence"

(* [get_lit_from_trm_lit t]: gets the literal value from a trm_lit *)
let get_lit_from_trm_lit (t : trm) : lit =
  match t.desc with
  | Trm_val (Val_lit l) -> l
  | _ -> trm_fail t "Ast.get_lit_from_trm: this type of literal is not supported"


(* [is_get_operation t]: checks if [t] is a get operation(read operation) *)
  let is_get_operation (t : trm) : bool =
    match t.desc with
    | Trm_apps ({desc = Trm_val(Val_prim (Prim_unop Unop_get))}, _, _) -> true
    | _ -> false

  (* [is_new_operation t] checks if [t] is new operation *)
  let is_new_operation (t : trm) : bool =
    match t.desc with
    | Trm_apps (f, _, _) ->
      begin match trm_prim_inv f with
      | Some (Prim_new _) -> true
      | _ -> false
      end
    | _ -> false

  let trm_set_inv (t : trm) : (trm * trm) option =
    trm_binop_inv Binop_set t

  (* [is_set_operation t]: checks if [t] is a set operation(write operation) *)
  let is_set_operation (t : trm) : bool =
    match t.desc with
    | Trm_apps (f, _, _) ->
      begin match trm_prim_inv f with
      | Some (Prim_binop Binop_set) | Some(Prim_compound_assgn_op _)
      (* FIXME: not supported by [trm_set_inv] *)
       | Some (Prim_overloaded_op (Prim_binop Binop_set)) -> true
      | _ -> false
      end
    | _ -> false


  (* [is_compound_assignment]: checks if [t] is a compound assignment *)
  let is_compound_assignment (t : trm) : bool =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_compound_assgn_op _))}, _, _) -> true
    | _ -> false

  (* [is_access t]: check if t is a struct or array access *)
  let is_access (t : trm) : bool = match t.desc with
    | Trm_apps (f, _, _) ->
      begin match trm_prim_inv f with
      | Some p ->
        begin match p with
        | Prim_unop (Unop_struct_access _) | Prim_unop (Unop_struct_get _) | Prim_binop (Binop_array_access)
          | Prim_binop (Binop_array_get) -> true
        | _ -> false
        end
      | None -> false
      end
    | _ -> false

  (* [get_operation_arg t]: gets the arg of a get operation. *)
  let get_operation_arg (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1], _) -> t1
    | _ -> t

  (* [new_operation_arg t]: get the argument of the encoded new operation. *)
  let new_operation_arg (t : trm) : trm =
    match t.desc with
    | Trm_apps (_, [arg], _) when is_new_operation t -> arg
    | _ -> t


  (* [new_operation_inv t]: returns the type and the argument of the new operation [t]. *)
  let new_operation_inv (t : trm) : (typ * trm) option =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_new ty))}, [arg], _) -> Some (ty, arg)
    | _ -> None


  (* [trm_let_mut ~annot ?ctx typed_var init]: an extension of trm_let for
      creating mutable variable declarations *)
  let trm_let_mut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
    (typed_var : typed_var) (init : trm): trm =
    let var_name, var_type = typed_var in
    let var_type_ptr = typ_ptr_generated var_type in
    let t_let = trm_let ?loc ?ctx Var_mutable (var_name, var_type_ptr) (trm_apps (trm_prim (Prim_new var_type)) [init]) in
    trm_add_cstyle Stackvar t_let

  (* [trm_let_ref ~annot ?ctx typed_var init]: an extension of trm_let for creating references *)
  let trm_let_ref ?(annot = trm_annot_default) ?(loc)  ?(ctx : ctx option)
    (typed_var : typed_var) (init : trm): trm =
    let var_name, var_type = typed_var in
    let var_type_ptr = typ_ptr_generated var_type in
    let t_let = trm_let ?loc ?ctx Var_mutable (var_name, var_type_ptr) init in
    trm_add_cstyle Reference t_let


  (* [trm_let_IMmut ~annot ?ctx typed_var init]: an extension of trm_let for creating immutable variable declarations. *)
  let trm_let_immut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
    (typed_var : typed_var) (init : trm): trm =
    let var_name, var_type = typed_var in
    let var_type = typ_const var_type in
    trm_let ~annot ?loc ?ctx Var_immutable (var_name, var_type) (init)

  (* [trm_let_array ~annot ?ctx typed_var sz init]: an extension of trm_let for creating array variable declarations *)
  let trm_let_array ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (kind : varkind )
    (typed_var : typed_var) (sz : size)(init : trm): trm =
    let var_name, var_type = typed_var in
    let var_type = if kind = Var_immutable then typ_const (typ_array var_type sz) else typ_ptr_generated (typ_array var_type sz) in
    let var_init = if kind = Var_immutable then init else trm_apps (trm_prim (Prim_new var_type)) [init]  in
    let res = trm_let ~annot ?loc ?ctx kind (var_name, var_type) var_init in
    if kind = Var_mutable then trm_add_cstyle Stackvar res else res


  (* [trm_for_c_inv_simple_init init]: checks if the init loop component is simple. If that's the case then return
     initial value of the loop index.
    Ex.:
      int x = a -> Some (x, a, true)
      x = a -> Some (x, a, false) *)
  let trm_for_c_inv_simple_init (init : trm) : (var * trm) option =
    match init.desc with
    | Trm_let (_, (x, _), init_val) ->
      begin match get_init_val init_val with
      | Some init1 -> Some (x, init1)
      | _ -> None
      end
    | _ -> None


  (* [trm_for_c_inv_simple_stop stop]: checks if the loop bound is simple.  If that's the case return that bound. *)
  let trm_for_c_inv_simple_stop (stop : trm) : (loop_dir * trm) option =
    match stop.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                   [_; n], _) -> Some (DirUp, n)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
                [_; n], _) -> Some (DirUpEq, n)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
                [_; n], _) -> Some (DirDown, n)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _},
                [_; n], _) -> Some (DirDownEq, n)
    | _ -> None

  (* [trm_for_c_inv_simple_step step]: checks if the loop step is simple. If that's the case then return that step. *)
  let trm_for_c_inv_simple_step (step : trm) : loop_step option =
    match step.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_inc)); _}, _, _) ->
        Some Post_inc
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_inc)); _}, _, _) ->
       Some Pre_inc
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_dec)); _}, _, _) ->
       Some Post_dec
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_dec)); _}, _, _) ->
       Some Pre_dec
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op _)); _},
                   [_; n], _) -> Some (Step n)
    | _ -> None


  (* [trm_for_of_trm_for_c t]: checks if loops [t] is a simple loop or not, if yes then return the simple loop else returns [t]. *)
  let trm_for_of_trm_for_c (t : trm) : trm =
    match t.desc with
    | Trm_for_c (init, cond , step, body, _) ->
      let init_ops = trm_for_c_inv_simple_init init in
      let bound_ops = trm_for_c_inv_simple_stop cond in
      let step_ops = trm_for_c_inv_simple_step step in
      let t_pragmas = trm_get_pragmas t in
      let is_parallel = List.exists (function | Parallel_for _ -> true | _ -> false) t_pragmas in
      begin match init_ops, bound_ops, step_ops with
      | Some (index, start), Some (direction, stop), Some step ->
        trm_for (index, start, direction, stop, step, is_parallel ) body
      | _ -> t
      end
    | _ -> trm_fail t "Ast.trm_for_of_trm_for_c: expected a for loop"




(* [get_include_filename t]: returns the included filename if [t] is an include directive. *)
  let get_include_filename (t : trm) : string  =
let f_name = List.fold_left (fun acc x ->
  match x with
  | Include s -> Some s
  | _ -> acc
) None t.annot.trm_annot_files in
match f_name with
| Some s -> s
| _ -> trm_fail t "Ast.get_include_filename: couldn't get the requested filename"

(* [compute_app_unop_value p v1]: simplifies unary operations on literals. *)
let compute_app_unop_value (p : unary_op) (v1:lit) : trm =
match p, v1 with
| Unop_neg, Lit_bool b -> trm_bool (not b)
| Unop_post_inc, Lit_int n -> trm_int (n + 1)
| Unop_pre_inc, Lit_int n -> trm_int (n + 1)
| Unop_post_dec, Lit_int n -> trm_int (n - 1)
| Unop_pre_dec, Lit_int n -> trm_int (n - 1)
| _ -> failwith "Ast.compute_app_unop_value: only negation can be applied here"

(* [compute_app_binop_value]: simplifies binary operations on literals. *)
let compute_app_binop_value (p : binary_op) (typ1 : typ option) (typ2 : typ option) (v1 : lit) (v2 : lit) : trm =
match p,v1, v2 with
| Binop_eq , Lit_int n1, Lit_int n2 -> trm_bool (n1 == n2)
| Binop_eq, Lit_double d1, Lit_double d2 -> trm_bool (d1 == d2)
| Binop_neq , Lit_int n1, Lit_int n2 -> trm_bool (n1 <> n2)
| Binop_neq, Lit_double d1, Lit_double d2 -> trm_bool (d1 <> d2)
| Binop_sub, Lit_int n1, Lit_int n2 -> trm_int (n1 - n2)
| Binop_sub, Lit_double d1, Lit_double d2 ->
  let typ = Xoption.or_ typ1 typ2 in
  trm_float ?typ (d1 -. d2)
| Binop_add, Lit_int n1, Lit_int n2 -> trm_int (n1 + n2)
| Binop_add, Lit_double d1, Lit_double d2 ->
  let typ = Xoption.or_ typ1 typ2 in
  trm_float ?typ (d1 +. d2)
| Binop_mul, Lit_int n1, Lit_int n2 -> trm_int (n1 * n2)
| Binop_mul, Lit_double n1, Lit_double n2 ->
  let typ = Xoption.or_ typ1 typ2 in
  trm_float ?typ (n1 *. n2)
| Binop_mod, Lit_int n1, Lit_int n2 -> trm_int (n1 mod n2)
| Binop_div, Lit_int n1, Lit_int n2 -> trm_int (n1 / n2)
| Binop_div, Lit_double d1, Lit_double d2 ->
  let typ = Xoption.or_ typ1 typ2 in
  trm_float ?typ (d1 /. d2)
| Binop_le, Lit_int n1, Lit_int n2 -> trm_bool (n1 <= n2)
| Binop_le, Lit_double d1, Lit_double d2 -> trm_bool (d1 <= d2)
| Binop_lt, Lit_int n1, Lit_int n2 -> trm_bool (n1 < n2)
| Binop_lt, Lit_double d1, Lit_double d2 -> trm_bool (d1 < d2)
| Binop_ge, Lit_int n1, Lit_int n2 -> trm_bool (n1 >= n2)
| Binop_ge, Lit_double d1, Lit_double d2 -> trm_bool (d1 >= d2)
| Binop_gt, Lit_int n1, Lit_int n2 -> trm_bool (n1 > n2)
| Binop_gt, Lit_double d1, Lit_double d2 -> trm_bool (d1 > d2)
| _ -> failwith "Ast.compute_app_binop_value: operator not supporeted"

(* [decl_list_to_typed_vars tl]: converts a list of variable declarations to a list of paris where each pair
  consists of a variable and its type *)
let decl_list_to_typed_vars (tl : trms) : typed_vars =
List.map (fun t ->
  match t.desc with
  | Trm_let (_, (x, tx),_) -> (x, get_inner_ptr_type tx)
  | _ -> trm_fail t "Ast.decl_list_to_typed_vars: expected a list of declarations"
) tl

(* [trm_is_var t]: checks if [t] is a variable occurrence. *)
let trm_is_var (t : trm) : bool =
  match t.desc with
  | Trm_var _ -> true
  | _ -> false

(* [trm_is_val_or_var t]: checks if [t] is a variable occurrence or a value *)
let rec trm_is_val_or_var (t : trm) : bool =
match t.desc with
| Trm_val _ | Trm_var _ -> true
| Trm_apps (_, [var_occ], _) when is_get_operation t -> trm_is_val_or_var var_occ
| _ -> false

(* [is_prefix_unary unop]: checks if [unop] is a prefix unary operator *)
let is_prefix_unary (unop : unary_op) : bool =
  match unop with
  | Unop_pre_inc | Unop_pre_dec -> true
  | _ -> false

(* [is_postfix_unary unop]: checks if [unop] is a postfix unary operator *)
let is_postfix_unary (unop : unary_op) : bool =
  match unop with
  | Unop_post_inc | Unop_post_dec -> true
  | _ -> false

let is_unary_compound_assign (unop : unary_op) : bool =
  (is_prefix_unary unop) || (is_postfix_unary unop)

(* [trm_is_unary_compound_assign t] checks whether [t] represents a unary  compound assignment: e.g., increment or decrement operation. *)
let trm_is_unary_compound_assign (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, _, _) when is_unary_compound_assign op -> true
  | _ -> false

(* [trm_for_inv t]: gets the loop range from loop [t] *)
let trm_for_inv (t : trm) : (loop_range * trm * loop_spec)  option =
match t.desc with
| Trm_for (l_range, body, contract) -> Some (l_range, body, contract)
| _ -> None

(* [trm_for_inv_instrs t]: gets the loop range and body instructions from loop [t]. *)
let trm_for_inv_instrs (t : trm) : (loop_range * trm mlist * loop_spec) option =
Option.bind (trm_for_inv t) (fun (r, b, c) ->
  Option.map (fun instrs -> (r, instrs, c)) (trm_seq_inv b))

(* [is_trm_seq t]: checks if [t] is a sequence. *)
let is_trm_seq (t : trm) : bool =
match t.desc with
| Trm_seq _ -> true  | _ -> false

(* [trm_var_def_inv t] gets the name type and the initialization value *)
let trm_var_def_inv (t : trm) : (varkind * var * typ * trm option) option =
match t.desc with
| Trm_let (vk, (x,tx), init) ->
  let init1 = match get_init_val init with
  | Some init1 -> Some init1
  | _ -> None in Some (vk, x, get_inner_ptr_type tx, init1)
| _ -> None

(* [trm_fors rgs tbody] creates nested loops with the main body [tbody] each
  nested loop takes its components from [rgs]. *)
let trm_fors (rgs : loop_range list) (tbody : trm) : trm =
  List.fold_right (fun l_range acc ->
    trm_for l_range (if (is_trm_seq acc) then acc else trm_seq_nomarks [acc])
  ) rgs tbody

(* [trm_fors_inv nb t]: gets a list of loop ranges up to the loop at depth [nb] from nested loops [t] *)
let trm_fors_inv (nb : int) (t : trm) : (loop_range list * trm) option =
let nb_loops = ref 0 in
let body_to_return  = ref (trm_int 0) in
let rec aux (t : trm) : loop_range list =
  match t.desc with
  | Trm_for (l_range, body, _) ->
    incr nb_loops;
    begin match body.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      if !nb_loops = nb
        then begin
          body_to_return := body;
          l_range :: []
          end
        else l_range :: aux (Mlist.nth tl 0)
    | _ ->  l_range :: []
    end

  | _ -> []
  in

let loop_range_list = aux t in
if List.length loop_range_list <> nb then None else Some (loop_range_list, !body_to_return)

let trm_new_inv (t : trm) : (typ * trm) option =
match trm_apps_inv t with
| Some (f, [v]) ->
  begin match trm_prim_inv f with
  | Some (Prim_new ty) -> Some (ty, v)
  | _ -> None
  end
| _ -> None

(* [is_trm_uninitialized t]: checks if [t] is the body of an uninitialized function or variable *)
let is_trm_uninitialized (t:trm) : bool =
match t.desc with
| Trm_val (Val_lit Lit_uninitialized) -> true
| _ -> false

let is_trm_new_uninitialized (t : trm) : bool =
match trm_new_inv t with
| Some (_, v) -> is_trm_uninitialized v
| None -> false


(* [is_trm_arbit t]: checks if [t] is a proper trm *)
  let is_trm_arbit (t : trm) : bool =
    match t.desc with
    | Trm_arbitrary _ -> true
    | _ -> false


(* [is_infix_prim_fun p]: checks if the primitive function [p] is one of those that supports app and set operations or not *)
  let is_infix_prim_fun (p : prim) : bool =
    match p with
    | Prim_compound_assgn_op __ -> true
    | Prim_binop op ->
      begin match op with
      | Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_mod | Binop_shiftl | Binop_shiftr | Binop_and | Binop_or -> true
      | _ -> false
      end
    | _ -> false

  (* [get_binop_from_prim p]: if [p] is a binop operation then return its underlying binary operation *)
  let get_binop_from_prim (p : prim) : binary_op option =
    match p with
    | Prim_compound_assgn_op binop -> Some binop
    | Prim_binop binop -> Some binop
    | _ -> None

  (* [is_arith_fun p]: checks if the primitive function [p] is an arithmetic operation or not *)
  let is_arith_fun (p : prim) : bool =
    match p with
    | Prim_binop bin_op ->
      begin match bin_op with
      | Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_mod -> true
      | _ -> false
      end
    | _ -> false

  (* [is_prim_arith p]: checks if [p] is a primitive arithmetic operation *)
  let is_prim_arith (p : prim) : bool =
    match p with
    | Prim_binop (Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_exact_div)
    | Prim_unop Unop_neg ->
        true
    | _ -> false

  (* [is_prim_arith_call t]: checks if [t] is a function call to a primitive arithmetic operation *)
  let is_prim_arith_call (t : trm) : bool =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim p);_}, args, _) when is_prim_arith p -> true
    | _ -> false

  (* [is_struct_init t]: checks if [t] is struct_init *)
  let is_struct_init (t : trm) : bool =
    match t.desc with
    | Trm_record _ -> true | _ -> false

  (* [is_same_binop op1 op2 ]: checks if two primitive operations are the same *)
  let is_same_binop (op1 : binary_op) (op2 : binary_op) : bool =
    match op1, op2 with
    | Binop_set, Binop_set -> true
    | Binop_array_access, Binop_array_access -> true
    | Binop_array_get, Binop_array_get -> true
    | Binop_eq, Binop_eq -> true
    | Binop_neq, Binop_neq -> true
    | Binop_sub, Binop_sub -> true
    | Binop_add, Binop_add -> true
    | Binop_mul, Binop_mul -> true
    | Binop_mod, Binop_mod -> true
    | Binop_div, Binop_div -> true
    | Binop_le, Binop_le -> true
    | Binop_lt, Binop_lt -> true
    | Binop_ge, Binop_ge -> true
    | Binop_gt, Binop_gt -> true
    | Binop_and, Binop_and -> true
    | Binop_bitwise_and, Binop_bitwise_and -> true
    | Binop_or, Binop_or -> true
    | Binop_bitwise_or, Binop_bitwise_or -> true
    | Binop_shiftl, Binop_shiftl -> true
    | Binop_shiftr, Binop_shiftr -> true
    | Binop_xor, Binop_xor -> true
    | _, _ -> false


  (* [trm_struct_access ~annot ?typ base field]: creates a struct_access encoding *)
  let trm_struct_access ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (field : field) : trm =
    trm_apps ?typ (trm_unop ~annot (Unop_struct_access field)) [base]

  (* [trm_struct_get ~annot ?typ base field]: creates a struct_get encoding *)
  let trm_struct_get ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (field : field) : trm =
    trm_apps ?typ (trm_unop ~annot (Unop_struct_get field)) [base]

  (* [trm_array_access~annot ?typ base index]: creates array_access(base, index) encoding *)
  let trm_array_access ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (index : trm) : trm =
    trm_apps ?typ (trm_binop ~annot Binop_array_access) [base; index]

  (* [trm_array_get ~annot ?typ base index]: creates array_get (base, index) encoding *)
  let trm_array_get ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (index : trm) : trm =
    trm_apps ?typ (trm_binop ~annot Binop_array_get) [base; index]

  (* [trm_get ~annot ?typ t]: embeds [t] into a get operation *)
  let trm_get ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
    let typ = Xoption.or_ typ (Option.bind t.typ typ_of_get) in
    trm_apps ~annot ?typ (trm_unop Unop_get) [t]

  (* [trm_address_of ~anot ?typ t]: creates an address operation in [t] *)
  let trm_address_of ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
    trm_apps ?typ:t.typ (trm_unop Unop_address) [t]

  (* [trm_var_get ?typ x]: generates *x *)
  let trm_var_get ?(typ : typ option) (x : var) : trm =
    trm_get ?typ (trm_var ?typ x)

  (* [trm_var_addr ?typ x]: generates &x*)
  let trm_var_addr ?(typ : typ option) (x : var) : trm =
    trm_address_of ?typ (trm_var ?typ x)

  (* [trm_var_possibly_mut ~const ?typ x]: reads the value of x, it can be x or *x  *)
  let trm_var_possibly_mut ?(const : bool = false) ?(typ : typ option) (x : var) : trm =
    if const then trm_var ?typ ~kind:Var_immutable x else trm_var_get ?typ x

  (* [trm_new ty t]: generates "new ty" *)
  let trm_new (ty : typ) (t : trm) : trm =
    trm_apps (trm_prim (Prim_new ty)) [t]

let var_any_bool = new_var "ANY_BOOL"

  (* [trm_any_bool]: generates ANY_BOOL () *)
  let trm_any_bool : trm =
    trm_apps (trm_var var_any_bool) []

  (* [trm_minus ?loc ?ctx ?typ t]: generates -t *)
  let trm_minus ?(loc) ?(ctx : ctx option) ?(typ) (t : trm) : trm =
    let typ = Xoption.or_ typ t.typ in
    trm_apps ?loc ?ctx ?typ (trm_unop ?loc ?ctx Unop_minus) [t]

  (* [trm_eq ?loc ?ctx ?typ t1 t2]: generates t1 = t2 *)
  let trm_eq ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx  Binop_eq) [t1; t2]

  (* [trm_neq t1 t2]: generates t1 != t2 *)
  let trm_neq ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_neq) [t1; t2]

  (* [trm_sub t1 t2]: generates t1 - t2 *)
  let trm_sub ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_sub) [t1; t2]

  (* [trm_add t1 t2]: generates t1 + t2 *)
  let trm_add ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_add) [t1; t2]

  (* [trm_mod t1 t2]: generates t1 % t2 *)
  let trm_mod ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_mod) [t1; t2]

  (* [trm_add_inv t1 t2]: deconstructs t = t1 + t2 *)
  let trm_add_inv (t : trm) : (trm * trm) option  =
    trm_binop_inv Binop_add t

  (* [trm_mul t1 t2]: generates t1 * t2 *)
  let trm_mul ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_mul) [t1; t2]

  (* [trm_div t1 t2]: generates t1 / t2 *)
  let trm_div ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_div) [t1; t2]

  (* [trm_exact_div t1 t2]: generates exact_div(t1, t2) *)
  let trm_exact_div ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_exact_div) [t1; t2]

  (* [trm_le t1 t2]: generates t1 <= t2 *)
  let trm_le ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_le) [t1; t2]

  (* [trm_lt t1 t2]: generates t1 < t2 *)
  let trm_lt ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_lt) [t1; t2]

  (* [trm_ge t1 t2]: generates t1 >= t2 *)
  let trm_ge ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_ge) [t1; t2]

  (* [trm_gt t1 t2]: generates t1 > t2 *)
  let trm_gt ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_gt) [t1; t2]

  (* [trm_ineq ineq_sgn t1 t2]: generates an inequality t1 # t2 where # is one of the following operators <, <=, >, >=.
      The operator is provided implicitly through the [ineq_sng] arg *)
  let trm_ineq (ineq_sgn : loop_dir) (t1 : trm) (t2 : trm) : trm =
    match ineq_sgn with
    | DirUp -> trm_lt t1 t2
    | DirUpEq -> trm_le t1 t2
    | DirDown ->  trm_gt t1 t2
    | DirDownEq -> trm_ge t1 t2


  (* [trm_and t1 t2]: generates t1 && t2 *)
  let trm_and ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_and) [t1;t2]

  (* [trm_bit_and t1 t2]: generates t1 & t2 *)
  let trm_bit_and ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_bitwise_and) [t1;t2]

  (* [trm_or t1 t2]: generates t1 || t2 *)
  let trm_or ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.or_ typ (Some (typ_bool ())) in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_or) [t1;t2]

  (* [trm_bit_or t1 t2]: generates t1 | t2 *)
  let trm_bit_or ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_bitwise_or) [t1;t2]

  (* [trm_shiftl t1 t2]: generates t1 << t2*)
  let trm_shiftl ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_shiftl) [t1; t2]

  (* [trm_shiftr t1 t2]: generates t1 >> t2*)
  let trm_shiftr ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    let typ = Xoption.ors [typ; t1.typ; t2.typ] in
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_shiftr) [t1; t2]
  (* LATER [trm_fmod t1 t2]: generates fmod(t1, t2)
  let trm_fmod ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_fmod) [t1;t2]
  *)

  (* [trm_xor t1 t2]: generates t1 ^ t2 *)
  let trm_xor ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
    trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_xor) [t1; t2]

  (* [trm_ands ts] generalized version of trm_and *)
  let trm_ands (ts : trm list) : trm =
    Xlist.fold_lefti (fun i acc t1 ->
      if i = 0 then t1 else trm_and acc t1
    ) (trm_bool true) ts

  (* [trm_prim_compound ~annot ?ctx ?loc ?typ binop t1 t2]: generates a compound operation, ex t1+=t2*)
  let trm_prim_compound ?(annot : trm_annot = trm_annot_default) ?(ctx : ctx option) ?(loc) ?(typ)
    (binop : binary_op) (t1 : trm) (t2 : trm) : trm =
    trm_apps ?loc ~annot ?typ (trm_prim ?loc ?ctx (Prim_compound_assgn_op binop)) [t1; t2]





(* [empty_ast]: generates {} *)
let empty_ast : trm =
  trm_set_mainfile (trm_seq_nomarks [])



(* [array_access base index]: generates array_access (base, index) *)
let array_access (base : trm) (index : trm) : trm =
  trm_apps (trm_binop Binop_array_access) [base; index]

let array_access_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access));_},
              [base;index], _) -> Some (base, index)
  | _ -> None

let array_inv (t : trm) : trm mlist option =
  match t.desc with
  | Trm_array els -> Some els
  | _ -> None

let array_get_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get));_},
              [base;index], _) -> Some (base, index)
  | _ -> None

(* [get_array_access base index]: generates get(array_access (base, index)) *)
let get_array_access (base : trm) (index : trm) : trm =
  trm_get (array_access base index)

(* [get_array_access_inv t] returns the Some(base, index) of an array_access if [t]
     is of the form get(array_access(base, index) otherwise None *)
let get_array_access_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg], _) ->
    array_access_inv arg
  | _ -> None

(* [struct_access base index]: generates struct_access (base, index) *)
let struct_access (f : field) (base : trm) : trm =
  trm_apps (trm_unop (Unop_struct_access f)) [base]

(* [get_struct_access base index]: generates get(struct_access (base, index)) *)
let get_struct_access (f : field) (base : trm) : trm =
  trm_get (struct_access f base)

(* [struct_access_inv t]: if [t] is  a struct access then return its base and the accessed field; else Npone *)
let struct_access_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)));_}, [base], _) -> Some (base, f)
  | _ -> None

(* [struct_access_inv_some t]: if [t] is  a struct access then return its base and the accessed field *)
let struct_access_inv_some (t : trm) : (trm * field) =
  match struct_access_inv t with
  | None -> assert false
  | Some r -> r

(* [struct_get_inv t]: if [t] is a struct get then return its base and the accesses field; else none *)
let struct_get_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_}, [base], _) -> Some (base, f)
  | _ -> None

(* [struct_get_inv_some t]: if [t] is a struct get then return its base and the accesses field *)
let struct_get_inv_some (t : trm) : (trm * field) =
  match struct_get_inv t with
  | None -> assert false
  | Some r -> r

(* [get_struct_access_inv t]: if [t] is of the form get(struct_access (f, base)) returns Some (f,base); else None *)
let get_struct_access_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg], _) -> struct_access_inv arg
  | _ -> None

(* [get_struct_access_inv_some t]: if [t] is of the form get(struct_access (f, base)) returns (f,base) *)
let get_struct_access_inv_some (t : trm) : (trm * field) =
  match get_struct_access_inv t with
  | None -> assert false
  | Some r -> r


(* [set_struct_access_inv t]: if [t] is a write on a struct access, then return the base, the field of that access
    and the value that has been assigned to; else None *)
let set_struct_access_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
   begin match struct_access_inv lhs with
   | Some (base, f) -> Some (base, f, rhs)
   | _ -> None
   end
  | _ -> None

(* [set_struct_access_inv t]: if [t] is a write operation on a struct field, then it will return the base, the field and the
     value that has been assigned to.  *)
let set_struct_access_inv_some (t : trm) : (trm * field * trm) =
  match set_struct_access_inv t with
  | None -> assert false
  | Some r -> r


(* [set_struct_get_inv t]: if [t] is a write operation on a struct field, then it will return the base, the field and the
     value that has been assigned to.  *)
let set_struct_get_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
    begin match struct_get_inv lhs with
    | Some (base, f) -> Some (base, f, rhs)
    | _ -> None
    end
  | _ -> None


(* [set_struct_get_inv_some t]: similar to [set_struct_get_inv] but this one fails if [t] is not a write on a struct_get operation.  *)
let set_struct_get_inv_some (t : trm) : (trm * field * trm) =
  match set_struct_get_inv t with
  | None -> assert false
  | Some r -> r

(* [struct_init_inv t]: if is t is a struct initialization, get the list of terms; else None *)
let struct_init_inv (t : trm) : (label option * trm) mlist option =
  match t.desc with
  | Trm_record sl -> Some sl
  | _ -> None

(* [struct_init_inv_some t]: gets struct initialization list trms *)
let struct_init_inv_some (t : trm) : (label option * trm) mlist =
 match struct_init_inv t with
  | None -> assert false
  | Some r -> r

(* [set_inv t]: gets the lhs and the rhs of a set(write) operation *)
let set_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation  t-> Some (lhs, rhs)
  | _ -> None

(* [insert_at_top_of_seq tl t]: insert the list of trms [tl] at the top of sequence [t]. *)
let insert_at_top_of_seq (tl : trm list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 ->
    let new_tl = Mlist.insert_sublist_at 0 tl tl1 in
    trm_alter ~desc:(Trm_seq new_tl) t
  | _ -> t


(* [filter_out_from_seq f t]: extracts all the trms that satisfy the predicate [f] from sequence [t].
      The final result is a pair consisting of the final sequence and the filtered out trms.*)
      let filter_out_from_seq (f : trm -> bool) (t : trm) : (trm * trms)  =
      match t.desc with
      | Trm_seq tl ->
        let tl_to_remove, tl_to_keep = Mlist.partition f tl in
        (trm_alter ~desc:(Trm_seq tl_to_keep) t , Mlist.to_list tl_to_remove)
      | _  -> (t, [])

    (* [is_class_constructor t] checks if [t] is a class constructor declaration or definition. *)
    let is_class_constructor (t : trm) : bool =
      List.exists (function  | Class_constructor _ -> true | _ -> false) (trm_get_cstyles t)

(* [get_typ_arguments t]: returns the list of types used during a template specialization. *)
let get_typ_arguments (t : trm) : typ list =
  let c_annot = trm_get_cstyles t in
  List.fold_left (fun acc c_ann ->
    match c_ann with
    | Typ_arguments tyl -> tyl
    | _ -> acc
  ) [] c_annot

let var_has_name (v : var) (n : string) : bool =
  v.qualifier = [] && v.name = n

(* [is_trm_record t]: checks if [t] has [Trm_record] or [Trm_array] description or not. *)
  let is_trm_record (t : trm) : bool =
    match t.desc with
    | Trm_record _ | Trm_array _ -> true | _ -> false


  (* [is_return t]: checks if [t] is a return statement. *)
  let is_return (t : trm) : bool =
    match t.desc with
    | Trm_abort (Ret _) -> true | _ -> false

  (* [is_trm_abort t]: checks if [t] has [Trm_abort abort] description. *)
  let is_trm_abort (t: trm) : bool =
    match t.desc with
    | Trm_abort _ -> true | _ -> false

  (* [is_trm_initialization_list] *)
  let is_trm_initialization_list (t : trm) : bool =
    match t.desc with
    | Trm_array _ | Trm_record _ -> true
    | _ -> false

  let is_trm_unit (t : trm) : bool =
    match trm_lit_inv t with
    | Some Lit_unit -> true
    | _ -> false

  let is_trm_int (cst : int) (t : trm) : bool =
    match trm_lit_inv t with
    | Some (Lit_int c) when c = cst -> true
    | _ -> false

(* [is_fun_with_empty_body t]: checks if the function [t] has an empty body or not. *)
let is_fun_with_empty_body (t : trm) : bool =
  match t.desc with
  | Trm_let_fun (_, _, _, body, _) when is_trm_uninitialized body -> true
  | _ -> false

let fun_with_empty_body (t : trm) : trm =
  match t.desc with
  | Trm_let_fun (v, vt, args, _body, contract) ->
    trm_alter ~desc:(Trm_let_fun (v, vt, args, trm_uninitialized (), contract)) t
  | _ -> failwith "Trm.fun_with_empty_body expected let fun"

(* ********************************************************************************************** *)

let trm_combinators_unsupported_case (f_name : string) (t : trm) : trm =
  if !Flags.report_all_warnings && !Flags.trm_combinators_warn_unsupported_case then begin
    Tools.warn (sprintf "don't know how to '%s' on '%s'" f_name (trm_desc_to_string t.desc));
    Printf.printf "<suppressing similar warnings henceforth>\n";
    Flags.trm_combinators_warn_unsupported_case := false;
  end;
  t

(* [trm_map_with_terminal]: applies function [f] over ast nodes,
   where [f] may treat terminal nodes differently.
   Here, terminal means that the end of the term would be the end of the function.
   - [share_if_no_change]: enables sharing trm nodes if they are unchanged by [f].
  *)
let trm_map_with_terminal ?(share_if_no_change = true) ?(keep_ctx = false) (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let typ = t.typ in
  let ctx = if keep_ctx then t.ctx else unknown_ctx () in

  let fun_spec_map f tf old_spec =
    match old_spec with
    | FunSpecContract old_contract ->
      let new_contract = f old_contract in
      if share_if_no_change && tf old_contract new_contract
        then old_spec
        else FunSpecContract new_contract
    | _ -> old_spec
  in
  let opt_map f tf old_opt =
    let new_opt = Option.map f old_opt in
    if share_if_no_change && Option.equal tf old_opt new_opt
      then old_opt
      else new_opt in
  let list_map f tf old_list =
    let new_list = List.map f old_list in
    if share_if_no_change && List.for_all2 tf old_list new_list
      then old_list
      else new_list in
  let mlist_map f tf old_list =
    let new_list = Mlist.map f old_list in
    if share_if_no_change && Mlist.for_all2 tf old_list new_list
      then old_list
      else new_list in

  let resource_items_map resources: resource_item list =
    list_map
      (fun (name, formula) -> (name, f false formula))
      (fun (_, fa) (_, fb) -> fa == fb)
      resources
  in
  let resource_set_map resources: resource_set =
    let pure = resource_items_map resources.pure in
    let linear = resource_items_map resources.linear in
    if share_if_no_change && pure == resources.pure && linear == resources.linear
      then resources
      else { resources with pure; linear }
  in
  let fun_contract_map contract: fun_contract =
    let pre = resource_set_map contract.pre in
    let post = resource_set_map contract.post in
    if share_if_no_change && pre == contract.pre && post == contract.post
      then contract
    else { pre; post }
  in
  let loop_contract_map contract: loop_contract =
    let loop_ghosts = resource_items_map contract.loop_ghosts in
    let invariant = resource_set_map contract.invariant in
    let iter_contract = fun_contract_map contract.iter_contract in
    if share_if_no_change && loop_ghosts == contract.loop_ghosts && invariant == contract.invariant && iter_contract == contract.iter_contract
      then contract
      else { loop_ghosts; invariant; iter_contract }
  in

  match t.desc with
  | Trm_val _ | Trm_var _ -> t
  | Trm_array tl ->
    let tl' = mlist_map (f false) (==) tl in
    if (share_if_no_change(*redundant*) && tl' == tl)
      then t
      else (trm_array ~annot ?loc ?typ ~ctx tl')
  | Trm_record tl ->
    let tl' = mlist_map
      (fun (lb, t) -> (lb, f false t))
      (fun (_, ta) (_, tb) -> ta == tb) tl
    in
    if (share_if_no_change(*redundant*) && tl' == tl)
      then t
      else (trm_record ~annot ?loc ?typ ~ctx tl')
  | Trm_let (vk, tv, init) ->
    let init' = f false init in
    if (share_if_no_change && init' == init)
      then t
      else (trm_let ~annot ?loc ~ctx vk tv init')
  | Trm_let_mult (vk, tvs, tis) ->
    let tis' = list_map (f false) (==) tis in
    if (tis' == tis) then t else
      (trm_let_mult ~annot ?loc ~ctx vk tvs tis')
  | Trm_let_fun (f', res, args, body, contract) ->
    let body' = f false body in
    let contract' = fun_spec_map fun_contract_map (==) contract in
    if (share_if_no_change && body' == body && contract' == contract)
      then t
      else (trm_let_fun ~annot ?loc ~contract:contract' ~ctx f' res args body')
  | Trm_fun (args, ret, body, contract) ->
    let body' = f false body in
    let contract' = fun_spec_map fun_contract_map (==) contract in
    if (share_if_no_change && body' == body && contract' == contract)
      then t
      else (trm_fun ~annot ?loc ~contract:contract' ~ctx args ret body')
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = f is_terminal then_ in
    let else_' = f is_terminal else_ in
    if (share_if_no_change && cond' == cond && then_' == then_ && else_' == else_)
      then t
      else (trm_if ~annot ?loc ~ctx cond' then_' else_')
  | Trm_seq tl ->
    let n = ref (Mlist.length tl) in
    let tl' = mlist_map
      (fun tsub ->
        n := !n - 1;
        let sub_is_terminal = (is_terminal && !n == 0) in
        f sub_is_terminal tsub)
      (==) tl
    in
    if (share_if_no_change(*redundant*) && tl == tl')
      then t
      else (trm_seq ~annot ?loc ~ctx tl')
  | Trm_apps (func, args, ghost_args) ->
    let func' = f false func in
    let args' = list_map (f false) (==) args in
    let ghost_args' = resource_items_map ghost_args in
    if (share_if_no_change(*redundant*) && func' == func && args' == args && ghost_args' == ghost_args)
      then t
      (* warning: may have different type *)
      else (trm_apps ~annot ?loc ?typ ~ctx ~ghost_args:ghost_args' func' args')
  | Trm_while (cond, body) ->
    let cond' = f false cond in
    let body' = f false body in
    if (share_if_no_change && cond' == cond && body' == body)
      then t
      else (trm_while ~annot ?loc ~ctx cond' body')
  | Trm_for_c (init, cond, step, body, invariant) ->
      let init' = f false init in
      let cond' = f false cond in
      let step' = f false step in
      let body' = f is_terminal body in
      let invariant' = opt_map resource_set_map (==) invariant in
      if (share_if_no_change && init' == init && cond' == cond && step' == step && body' == body && invariant' == invariant)
        then t
        else (trm_for_c ~annot ?loc ?invariant:invariant' ~ctx init' cond' step' body')
  | Trm_for (l_range, body, contract) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let start' = f false start in
    let stop' = f false stop in
    let step' = match step with
      | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
      | Step sp -> Step (f is_terminal sp)
      in
    let body' = f is_terminal body in
    let contract' = opt_map loop_contract_map (==) contract in
    if (share_if_no_change && step' == step && start' == start && stop' == stop && body' == body && contract' == contract)
      then t
      else (trm_for ~annot ?loc ?contract:contract' ~ctx (index, start', direction, stop', step', is_parallel) body')
  | Trm_switch (cond, cases) ->
      let cond' = f false cond in
      let cases' = list_map
        (fun (tl, body) -> (tl, f is_terminal body))
        (fun (_, body1) (_, body2) -> body1 == body2) cases in
      if (share_if_no_change(*redundant*) && cond' == cond && cases' == cases)
        then t
        else (trm_switch ~annot ?loc ~ctx cond' cases')
  | Trm_abort a ->
    begin match a with
    | Ret (Some r) ->
        let r' = f false r in
        if (share_if_no_change && r' == r)
          then t
          else (trm_ret ~annot ?loc ~ctx (Some r'))
    | _ -> t
    end
  | Trm_namespace (name, body, inline) ->
    let body' = f false body in
    if (share_if_no_change && body == body')
      then t
      else (trm_namespace ~annot ?loc ~ctx name body' inline)
  | Trm_typedef td ->
    let body' = begin match td.typdef_body with
    | Typdef_alias _ -> td.typdef_body
    | Typdef_record rfl ->
      let rfl' = list_map
        (fun (rf, rf_ann) ->
          let rf' = begin match rf with
          | Record_field_method rft ->
            let rft' = f false rft in
            if share_if_no_change && rft == rft'
              then rf
              else Record_field_method rft'
          | Record_field_member _ -> rf
          end in
          rf', rf_ann
        )
        (fun (rfa, _) (rfb, _) -> rfa == rfb)
        rfl in
      if share_if_no_change(*redundant*) && rfl == rfl'
        then td.typdef_body
        else Typdef_record rfl'
    | _ -> failwith "trm_map_with_terminal: unexpected typdef_body"
    end in
    if (share_if_no_change(*redundant*) && body' == td.typdef_body)
      then t
      else trm_typedef ~annot ?loc ~ctx { td with typdef_body = body' }
  | Trm_template (typ_params, templated) ->
    let templated' = f false templated in
    if (share_if_no_change && templated == templated')
      then t
      else trm_template ~annot ?loc ~ctx typ_params templated'
  | _ ->
    trm_combinators_unsupported_case "trm_map_with_terminal"  t


(* [trm_map f]: applies f on t recursively *)
let trm_map ?(keep_ctx = false) (f : trm -> trm) (t : trm) : trm =
  trm_map_with_terminal ~keep_ctx false (fun _is_terminal t -> f t) t

(* [trm_bottom_up]: applies f on t recursively from bottom to top. *)
let rec trm_bottom_up (f : trm -> trm) (t : trm) : trm =
  let t2 = trm_map (trm_bottom_up f) t in
  f t2

(* [trm_iter f t]: similar to [trm_map] but this one doesn't return a trm at the end. *)
let trm_iter (f : trm -> unit) (t : trm) : unit =
  match t.desc with
  | Trm_array tl ->
    Mlist.iter f tl
  | Trm_record tl ->
    Mlist.iter (function (_, t) -> f t) tl
  | Trm_let (vk, tv, init) ->
    f init
  | Trm_let_mult (vk, tvl, tl) ->
    List.iter f tl
  | Trm_let_fun (f', res, args, body, _) ->
    f body
  | Trm_if (cond, then_, else_) ->
    f cond;  f then_ ; f else_
  | Trm_seq tl ->
    Mlist.iter f tl
  | Trm_apps (func, args, ghost_args) ->
    f func; List.iter f args; List.iter (fun (_, t) -> f t) ghost_args
  | Trm_while (cond, body) ->
    f cond; f body
  | Trm_for_c (init, cond, step, body, _) ->
    f init; f cond; f step; f body
  | Trm_for (l_range, body, _) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    f start; f stop;
    begin match step with
     | Post_inc | Post_dec | Pre_inc | Pre_dec -> ()
     | Step sp -> f sp
    end;
    f body
  | Trm_do_while (body, cond) ->
    f body; f cond
  | Trm_switch (cond, cases) ->
     f cond;
     List.iter (fun (tl, body) -> f body) cases
  | Trm_abort a ->
    begin match a with
    | Ret (Some t') -> f t'
    | _ -> ()
    end
  | Trm_namespace (name, t, b) ->
    f t
  | Trm_delete (is_array, t) ->
    f t
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rfl ->
      List.iter (fun (rf, rf_ann) ->
        match rf with
        | Record_field_method t1 -> f t1
        | _ -> ()
      ) rfl
    | _ -> ()
    end
  | Trm_fun (_, _, body, _) -> f body
  | Trm_arbitrary _ ->
    ignore (trm_combinators_unsupported_case "trm_iter" t)
  | Trm_val _ | Trm_var _ | Trm_goto _  | Trm_extern _ | Trm_omp_routine _  | Trm_template _ | Trm_using_directive _ -> ()


(* TODO: this is different from trm_free_vars because bound variables are not treated. delete ? *)
let trm_used_vars (t: trm): Var_set.t =
  let vars = ref Var_set.empty in
  let rec aux t = match trm_var_inv t with
  | Some x -> vars := Var_set.add x !vars
  | _ -> trm_iter aux t
  in
  aux t;
  !vars

type var_metadata = trm_annot * location * typ option * ctx * varkind

let trm_map_vars
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  ?(enter_beta_redex: ('ctx -> (var * trm) list -> 'ctx) option)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> (ctx, bind))
  (map_var: 'ctx -> var_metadata -> var -> trm)
  (ctx: 'ctx) (t: trm): trm =
  let rec f_map (ctx:'ctx) (t:trm): 'ctx * trm =
    let annot = t.annot in
    let loc = t.loc in
    let typ = t.typ in
    let t_ctx = if keep_ctx then t.ctx else unknown_ctx () in

    let ctx, trm = match t.desc with
    | Trm_var (kind, x) ->
      (ctx, map_var ctx (annot, loc, typ, t_ctx, kind) x)

    | Trm_let (var_kind, (var, typ), body) ->
      let _, body' = f_map ctx body in
      (* FIXME: in C, extern int x; is a predeclaration, however, it is not stored in the AST *)
      let cont_ctx, var' = map_binder ctx var false in
      let t' = if (body == body' && var == var')
        then t
        else (trm_let ~annot ?loc ~ctx:t_ctx var_kind (var', typ) body')
      in
      (cont_ctx, t')

    | Trm_let_mult (vk, tvs, ts) ->
      let cont_ctx = ref ctx in
      let tvsts' = List.map2 (fun tv t ->
        let var, typ = tv in
        let (_, t') = f_map !cont_ctx t in
        let cont_ctx', var' = map_binder !cont_ctx var false in
        cont_ctx := cont_ctx';
        let tv' = if var == var' then tv else (var', typ) in
        (tv', t')
      ) tvs ts in
      let (tvs', ts') = List.split tvsts' in
      let t' = if ((List.for_all2 (==) ts ts') && List.for_all2 (==) tvs tvs')
        then t
        else (trm_let_mult ~annot ?loc ~ctx:t_ctx vk tvs' ts')
      in
      (!cont_ctx, t')

    | Trm_let_fun (fn, rettyp, args, body, contract) ->
      let body_ctx, args' = List.fold_left_map (fun ctx (arg, typ) ->
        let ctx, arg' = map_binder ctx arg false in
        (ctx, (arg', typ))
      ) (enter_scope ctx t) args in
      let body_ctx, contract' = fun_spec_map body_ctx contract in
      let body_ctx, body' = f_map body_ctx body in
      let cont_ctx, fn' = map_binder ctx fn (is_fun_with_empty_body t) in
      (* TODO: (List.for_all2 (==) args args') ? *)
      let t' = if (body' == body && args == args' && fn == fn' && contract == contract')
        then t
        else (trm_let_fun ~annot ?loc ~ctx:t_ctx ~contract:contract' fn' rettyp args' body')
      in
      (* TODO: Proper function type here *)
      let ctx = exit_scope cont_ctx body_ctx t' in
      (ctx, t')

    | Trm_for ((index, start, dir, stop, step, is_par), body, contract) ->
      let loop_ctx, index' = map_binder (enter_scope ctx t) index false in
      let loop_ctx, contract' = match contract with
      | None -> (loop_ctx, None)
      | Some c ->
        let loop_ctx, c' = loop_contract_map loop_ctx c in
        let c' = if (c == c') then contract else Some c' in
        (loop_ctx, c')
      in
      let step' = match step with
      | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
      | Step sp -> Step (snd (f_map loop_ctx sp))
      in
      let _, start' = f_map loop_ctx start in
      let _, stop' = f_map loop_ctx stop in
      let loop_ctx, body' = f_map loop_ctx body in
      let t' = if (index' == index && step' == step && start' == start && stop' == stop && body' == body && contract == contract')
        then t
        else (trm_for ~annot ?loc ~ctx:t_ctx ?contract:contract' (index', start', dir, stop', step', is_par) body')
      in
      let ctx = exit_scope ctx loop_ctx t' in
      (ctx, t')

    | Trm_for_c (init, cond, step, body, invariant) ->
      let loop_ctx, init' = f_map (enter_scope ctx t) init in
      let loop_ctx, invariant' = match invariant with
      | None -> (loop_ctx, None)
      | Some inv ->
        let loop_ctx, inv' = resource_set_map loop_ctx inv in
        let inv' = if inv == inv' then invariant else Some inv' in
        (loop_ctx, inv')
      in
      let _, cond' = f_map loop_ctx cond in
      let _, step' = f_map loop_ctx step in
      let loop_ctx, body' = f_map loop_ctx body in
      let t' = if (init' == init && cond' == cond && step' == step && body' == body && invariant == invariant')
        then t
        else (trm_for_c ~annot ?loc ~ctx:t_ctx ?invariant:invariant' init' cond' step' body')
      in
      let ctx = exit_scope ctx loop_ctx t' in
      (ctx, t')

    | Trm_fun (args, ret, body, contract) ->
      let body_ctx, args' = List.fold_left_map (fun ctx (arg, typ) ->
        let ctx, arg' = map_binder ctx arg false in
        (ctx, (arg', typ))) (enter_scope ctx t) args in
      let body_ctx, contract' = fun_spec_map body_ctx contract in
      let body_ctx, body' = f_map body_ctx body in
      (* TODO: (List.for_all2 (==) args args') ? *)
      let t' = if (args == args' && body == body' && contract == contract')
        then t
        else trm_fun ~annot ?loc ~ctx:t_ctx ~contract:contract' args' ret body' in
      (* TODO: Proper function type here *)
      let ctx = exit_scope ctx body_ctx t' in
      (ctx, t')

    | Trm_seq instrs ->
      (* LATER: add bool for no scope seq? *)
      let no_scope = trm_is_include t || trm_is_mainfile t in
      let cont_ctx = ref (if no_scope then ctx else enter_scope ctx t) in
      let instrs' = Mlist.map (fun t ->
        let cont_ctx', t' = f_map !cont_ctx t in
        cont_ctx := cont_ctx';
        t'
      ) instrs in
      let t' = if Mlist.for_all2 (==) instrs instrs'
        then t
        else trm_seq ~annot ?loc ~ctx:t_ctx instrs'
      in
      let ctx = if no_scope then !cont_ctx else exit_scope ctx !cont_ctx t' in
      (ctx, t')

    | Trm_apps (func, args, ghost_args) ->
      let _, func' = f_map ctx func in
      let args' = List.map (fun arg -> snd (f_map ctx arg)) args in
      let ghost_args' = List.map (fun (g, t) -> (g, snd (f_map ctx t))) ghost_args in
      begin match func'.desc, enter_beta_redex with
      | Trm_fun (params, _, body, FunSpecUnknown), Some enter_beta_redex ->
        (* LATER: deal with ghost_args and spec *)
        let args_inst = List.map2 (fun (param, _) arg -> (param, arg)) params args' in
        let inner_ctx = enter_beta_redex ctx args_inst in
        (ctx, snd (f_map inner_ctx body))
      | _ ->
        let args' = if List.for_all2 (==) args args' then args else args' in
        let ghost_args' = if List.for_all2 (==) ghost_args ghost_args' then ghost_args else ghost_args' in
        let t' = if (func' == func && args' == args && ghost_args' == ghost_args)
          then t
          else (trm_apps ~annot ?loc ?typ ~ctx:t_ctx ~ghost_args:ghost_args' func' args')
        in
        (ctx, t')
      end

    | Trm_typedef td ->
      (* Class namespace *)
      let class_ctx = ref (enter_scope ctx t) in
      let body' = begin match td.typdef_body with
      | Typdef_alias _ -> td.typdef_body
      | Typdef_record rfl ->
        let rfl' = List.map (fun (rf, rf_ann) ->
          let rf' = begin match rf with
          | Record_field_method rft ->
            let (class_ctx', rft') = f_map !class_ctx rft in
            class_ctx := class_ctx';
            if rft == rft' then rf else
              Record_field_method rft'
          | Record_field_member _ -> rf
          end in
          rf', rf_ann
        ) rfl in
        if List.for_all2 (==) rfl rfl' then td.typdef_body else Typdef_record rfl'
      | _ -> failwith "unexpected typdef_body"
      end in
      let td' = if (body' == td.typdef_body)
        then td
        else { td with typdef_body = body' }
      in
      let t' = if (td == td')
        then t
        else (trm_typedef ~annot ?loc ~ctx:t_ctx td')
      in
      let cont_ctx = exit_scope ctx !class_ctx t' in
      (cont_ctx, t')

    | Trm_namespace (name, body, inline) ->
      let body_ctx = ref (enter_scope ctx t) in
      let body' = begin match body.desc with
      | Trm_seq instrs ->
        (* FIXME: duplicated code with Trm_seq case *)
        let instrs' = Mlist.map (fun t ->
          let body_ctx', t' = f_map !body_ctx t in
          body_ctx := body_ctx';
          t'
        ) instrs in
        if Mlist.for_all2 (==) instrs instrs'
          then body
          else trm_seq ~annot:body.annot ?loc:body.loc ~ctx:body.ctx instrs'
      | _ ->
        failwith "unexpected namespace body"
      end in
      let t' = if (body == body')
        then t
        else (trm_namespace ~annot ?loc ~ctx:t_ctx name body' inline)
      in
      let cont_ctx = exit_scope ctx !body_ctx t' in
      (cont_ctx, t')

    | _ ->
      (ctx, trm_map ~keep_ctx (fun ti -> snd (f_map ctx ti)) t)

    in
    post_process ctx trm

  and resource_items_map ctx resources: 'ctx * resource_item list =
    List.fold_left_map (fun ctx resources ->
      let (name, formula) = resources in
      let _, formula' = f_map ctx formula in
      let ctx, name' = map_binder ctx name false in
      let resources' = if (name == name' && formula == formula')
        then resources else (name', formula')
      in
      (ctx, resources')
    ) (enter_scope ctx (trm_seq_nomarks [])) resources

  and resource_set_map ctx resources: 'ctx * resource_set =
    let ctx, pure = resource_items_map ctx resources.pure in
    let _, linear = resource_items_map ctx resources.linear in
    let resources' = if (List.for_all2 (==) pure resources.pure && List.for_all2 (==) linear resources.linear)
      then resources
      else { resources with pure; linear }
    in
    (ctx, resources')

  and fun_spec_map ctx fun_spec: 'ctx * fun_spec =
    match fun_spec with
    | FunSpecContract contract ->
      let ctx, contract' = fun_contract_map ctx contract in
      if contract == contract' then (ctx, fun_spec) else (ctx, FunSpecContract contract')
    | _ -> (ctx, fun_spec)

  and fun_contract_map ctx contract: 'ctx * fun_contract =
    let ctx, pre = resource_set_map ctx contract.pre in
    let _, post = resource_set_map ctx contract.post in
    let contract = if (pre == contract.pre && post == contract.post) then contract else { pre; post } in
    (ctx, contract)

  and loop_contract_map ctx contract: 'ctx * loop_contract =
    let ctx, loop_ghosts = resource_items_map ctx contract.loop_ghosts in
    let ctx, invariant = resource_set_map ctx contract.invariant in
    let ctx, iter_contract = fun_contract_map ctx contract.iter_contract in
    let contract =
      if (loop_ghosts == contract.loop_ghosts && invariant == contract.invariant && iter_contract == contract.iter_contract)
      then contract
      else { loop_ghosts; invariant; iter_contract }
    in
    (ctx, contract)

  in
  snd (f_map ctx t)


let trm_rename_vars
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  (map_var: 'ctx -> var -> var)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> (ctx, map_var ctx bind))
  ?(map_ghost_arg_name: 'ctx -> trm -> hyp -> hyp = fun ctx _ g -> map_var ctx g)
  (ctx: 'ctx) (t: trm): trm =
  trm_map_vars ~keep_ctx ~enter_scope ~exit_scope ~post_process:(fun ctx t ->
    match t.desc with
    | Trm_apps (fn, args, ghost_args) when ghost_args <> [] ->
      let map_ghost_arg_name = map_ghost_arg_name ctx fn in
      let ghost_args = List.map (fun (g, gt) -> (map_ghost_arg_name g, gt)) ghost_args in
      post_process ctx (trm_alter ~desc:(Trm_apps (fn, args, ghost_args)) t)
    | Trm_let_fun (fn, rettyp, args, body, FunSpecReverts other_fn) ->
      let other_fn' = map_var ctx other_fn in
      post_process ctx (trm_alter ~desc:(Trm_let_fun (fn, rettyp, args, body, FunSpecReverts other_fn')) t)
    | Trm_fun (args, rettyp, body, FunSpecReverts other_fn) ->
      let other_fn' = map_var ctx other_fn in
      post_process ctx (trm_alter ~desc:(Trm_fun (args, rettyp, body, FunSpecReverts other_fn')) t)
    | _ -> post_process ctx t
    ) ~map_binder (fun ctx (annot, loc, typ, vctx, kind) var -> trm_var ~annot ?loc ?typ ~ctx:vctx ~kind (map_var ctx var)) ctx t

let trm_iter_vars
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx = fun ctx _ -> ctx)
  (iter_var: 'ctx -> var -> unit)
  ?(iter_binder: 'ctx -> var -> bool -> 'ctx = fun ctx bind is_predecl -> iter_var ctx bind; ctx)
  ?(iter_ghost_arg_name: 'ctx -> trm -> hyp -> unit = fun ctx _ g -> iter_var ctx g)
  (ctx: 'ctx) (t: trm): unit =
  ignore (trm_rename_vars ~keep_ctx:true ~enter_scope ~exit_scope
    ~post_process:(fun ctx trm -> (post_process ctx trm, trm))
    (fun ctx x -> iter_var ctx x; x)
    ~map_binder:(fun ctx bind is_predecl -> (iter_binder ctx bind is_predecl, bind))
    ~map_ghost_arg_name:(fun ctx fn g -> iter_ghost_arg_name ctx fn g; g)
    ctx t)

(** Erase variable identifiers, useful for e.g. embedding a subexpression in a new context. *)
let trm_erase_var_ids (t : trm) : trm =
  let rec aux t =
    match trm_var_inv t with
    | Some x -> trm_var ~annot:t.annot ?loc:t.loc ?typ:t.typ ~ctx:t.ctx (name_to_var ~qualifier:x.qualifier x.name)
    | _ -> trm_map aux t
  in
  aux t

(** Uses a fresh variable identifier for every variable declation, useful for e.g. copying a term while keeping unique ids. *)
let trm_copy (t : trm) : trm =
  let map_binder var_map v _ =
    (* ???
    if v.id = inferred_var_id then (var_map, v)
    else *) begin
      assert (not (Var_map.mem v var_map));
      let new_v = new_var ~qualifier:v.qualifier v.name in
      (Var_map.add v new_v var_map, new_v)
    end
  in
  let map_var var_map v =
    if v.id = inferred_var_id then v
    else match Var_map.find_opt v var_map with
    | Some nv -> nv
    | None -> v
  in
  trm_rename_vars ~map_binder map_var Var_map.empty t

(* Assumes var-id's are unique, can locally break scope rules and might require a binder renaming. *)
let trm_subst
  ?(on_subst : trm -> trm -> trm = fun old_t new_t -> trm_copy new_t)
  (subst_map: trm varmap) (t: trm) =
  let subst_var (subst_map: trm varmap) ((annot, loc, typ, _ctx, kind): var_metadata) (var: var) =
    let var_t = trm_var ~annot ?loc ?typ ~kind var in
    match Var_map.find_opt var subst_map with
    | Some subst_t -> on_subst var_t subst_t
    | None -> var_t
  in
  let enter_beta_redex =
      List.fold_left (fun subst_map (param, effective_arg) -> Var_map.add param effective_arg subst_map)
  in
  trm_map_vars ~enter_beta_redex subst_var subst_map t

(** [subst x u t]: replace all the occurences of x with u in t *)
let trm_subst_var (x : var) (u : trm) (t : trm) =
  trm_subst (Var_map.singleton x u) t

(* The value associated with an existential variable (evar).
   None if the value is unknown, Some if the value is known (it was unified). *)
type eval = trm option

(* The unification context is a map from variables to evals.
   If a variable is not in the map, then it is not an evar, and should not be substituted/unified.
   If a variable is in the map, then it is an evar, and should be substituted/unified (i.e. its eval should eventually become Some). *)
type unification_ctx = eval varmap

(** [unfold_if_resolved_evar t evar_ctx] tries to unfold resolved evars inside [evar_ctx] occuring
    at the top of [t] (but not in depth).
    Since we can do it almost for free, it also performs simplifications in the [evar_ctx]
    when resolved evars are pointing to other resolved evars.

    It returns the possibly unfolded [trm], along with a possibly simplified [evar_ctx].

    On application nodes, this function tries to unfold the function and performs immediate
    beta reduction if possible. *)
let rec unfold_if_resolved_evar (t: trm) (evar_ctx: unification_ctx): trm * unification_ctx =
  match t.desc with
  | Trm_var (_, x) ->
    begin match Var_map.find_opt x evar_ctx with
    | Some (Some t) ->
      let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
      (* Path compression in case of cascading evars *)
      t, Var_map.add x (Some t) evar_ctx
    | _ -> t, evar_ctx
    end
  (* Immediate beta redex replacement *)
  | Trm_apps (apps_fn, args, ghost_args) ->
    let fn, evar_ctx = unfold_if_resolved_evar apps_fn evar_ctx in
    if fn == apps_fn then t, evar_ctx else
    begin match trm_fun_inv fn with
    | Some (params, ret, body, spec) ->
      let t = trm_subst (List.fold_left2 (fun subst_ctx arge (param, _) -> Var_map.add param arge subst_ctx) Var_map.empty args params) body in
      t, evar_ctx
    | None -> t, evar_ctx
    end
  | _ -> t, evar_ctx

(** [unify_trm t1 t2 evar_ctx] tries to unify [t1] with [t2], possibly instantiating and substituting evars
    that occur in [evar_ctx].
    If the unification succeeds, returns an updated unification context, with the newly resolved evars.
    If it fails, returns None. *)
let rec unify_trm (t_left: trm) (t_right: trm) (evar_ctx: unification_ctx) : unification_ctx option =
  let open Xoption.OptionMonad in
  (* Pattern match on one component to get a warning if there is a missing one *)
  let check cond = if cond then Some evar_ctx else None in
  (* Unfold first to avoid problems on f(?x, ?y) = f(?y, ?y) *)
  let t_left, evar_ctx = unfold_if_resolved_evar t_left evar_ctx in
  let t_right, evar_ctx = unfold_if_resolved_evar t_right evar_ctx in
  match trm_var_inv t_left, trm_var_inv t_right with
  | Some x_left, Some x_right when var_eq x_left x_right ->
    Some evar_ctx
  | Some x_left, _ when Var_map.mem x_left evar_ctx ->
    assert (Var_map.find x_left evar_ctx = None);
    Some (Var_map.add x_left (Some t_right) evar_ctx)
  | _, Some x_right when Var_map.mem x_right evar_ctx ->
    assert (Var_map.find x_right evar_ctx = None);
    Some (Var_map.add x_right (Some t_left) evar_ctx)
  | _ ->
    match t_right.desc with
    | Trm_var _ ->
      (* t_right is a variable but it is not the same as t_left, and none is an unresolved evar *)
      None

    | Trm_val ve ->
      let* v = trm_val_inv t_left in
      check (ve = v)

    | Trm_apps (fe, argse, ghost_args) ->
      let* f, args = trm_apps_inv t_left in
      let* evar_ctx = unify_trm f fe evar_ctx in
      begin try
        List.fold_left2 (fun evar_ctx arg arge -> let* evar_ctx in unify_trm arg arge evar_ctx) (Some evar_ctx) args argse
      with Invalid_argument _ -> None end

    | Trm_fun (argse, _, bodye, _) ->
      let* args, _, body, _ = trm_fun_inv t_left in
      (* Remember the masked context in case of shadowing, this is needed in case of recursive
         or higher order functions with evars. *)
      let* evar_ctx, masked_ctx =
        try
          Some (List.fold_left2 (fun (evar_ctx, masked) (arge, _) (arg, _) ->
              if var_eq arge arg then
                (* This case is required to handle comparison of a trm with itself *)
                (evar_ctx, masked)
              else
                let masked_entry = Var_map.find_opt arge evar_ctx in
                let evar_ctx = Var_map.add arge (Some (trm_var arg)) evar_ctx in
                (evar_ctx, (arge, masked_entry) :: masked)
            ) (evar_ctx, []) argse args)
        with Invalid_argument _ -> None
      in
      let* evar_ctx = unify_trm body bodye evar_ctx in
      Some (List.fold_left (fun evar_ctx (arge, masked_entry) ->
          match masked_entry with
          | Some entry -> Var_map.add arge entry evar_ctx
          | None -> Var_map.remove arge evar_ctx
        ) evar_ctx masked_ctx)

    | _ -> failwith (sprintf "unify_trm: unhandled constructor") (* TODO: Implement the rest of constructors *)

(** [are_same_trm t1 t2] checks that [t1] and [t2] are alpha-equivalent (same modulo name of the binders). *)
let are_same_trm (t1: trm) (t2: trm): bool =
  (* they are the same if they can be unified without allowing substitutions. *)
  Option.is_some (unify_trm t1 t2 Var_map.empty)

(* TODO: Use a real trm_fold later to avoid reconstructing trm *)
let trm_free_vars ?(bound_vars = Var_set.empty) (t: trm): Var_set.t =
  let fv = ref Var_set.empty in
  let _ = trm_map_vars ~map_binder:(fun bound_set binder _ -> (Var_set.add binder bound_set, binder))
    (fun bound_set _ var ->
      (if Var_set.mem var bound_set then () else fv := Var_set.add var !fv); trm_var var)
    bound_vars t
  in
  !fv

(*****************************************************************************)

exception Unknown_key

(* [tmap_to_list keys map]: gets the list of values for all keys [keys] in [map] *)
let tmap_to_list (keys : vars) (map : tmap) : trms =
  List.map (fun x -> match Var_map.find_opt x map with
    | Some v -> v
    | None -> raise Unknown_key
  ) keys

(* [tmap_filter keys tmap]: removes all the bindings with [keys] in [map] and return [map] *)
let tmap_filter (keys : vars) (map : tmap) : tmap =
  Var_map.filter (fun k _ -> not (List.mem k keys)) map

(* [hide_function_bodies f_pred tl]: all the toplevel function with their names satisfying [f_pred] will have hidden bodies.
    Others will be kept unchanged. The new ast is called the chopped_ast. This function wlll return the choped_ast and
    a map with keys the names of the functions whose body has been removed and values their removed body. *)
let hide_function_bodies (f_pred : var -> bool) (t : trm) : trm * tmap =
  let t_map = ref Var_map.empty in
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_let_fun (f, ty, tv, _, _) ->
        if f_pred f then begin
          t_map := Var_map.add f t !t_map;
         trm_let_fun ~annot:t.annot ~ctx:t.ctx f ty tv (trm_lit Lit_uninitialized) end
        else t
      | _ -> trm_map aux t
      in
  let res = aux t in
  res, !t_map

(* [update_chopped_ast chopped_ast chopped_fun_map]: for all the functions whose bodies were removed during the creation
    of the chopped_ast restore their bodies by using [chopped_fun_map], which is map with keys beingthe the names
    of the functions that were chopped and values being their actual declaration *)
let update_chopped_ast (chopped_ast : trm) (chopped_fun_map : tmap): trm =
  match chopped_ast.desc with
  | Trm_seq tl ->
      let new_tl =
      Mlist.map (fun def -> match def.desc with
      | Trm_let_fun (f, _, _, _, _) ->
        begin match Var_map.find_opt f chopped_fun_map with
        | Some tdef ->  tdef
        | _ -> def
        end
      | Trm_let (_, (x, _), _) ->
        (* There is a slight difference between clang and menhir how they handle function declarations, that's why we
         need to check if there are variables inside the function map *)
        begin match Var_map.find_opt x chopped_fun_map with
        | Some tdef -> tdef
        | _ -> def
        end
      |_ ->  def
    ) tl in trm_seq ~annot:chopped_ast.annot new_tl
  | _ -> trm_fail chopped_ast "Ast.update_ast_with_chopped_ast: ast of the main file should start with a top level sequence"




(* [label_subterms_with_fresh_stringreprids f t]: annotates all the subterms of [t]
   that satisfy the boolean predicate [f] with a fresh string representation identifier.
   This operation should be performed to enable the term to doc function to memoize
   its results, and possibly export a table mapping subterms to their string representation. *)
   let rec label_subterms_with_fresh_stringreprids (f : trm -> bool) (t : trm) : trm =
    let t2 =
      if not (f t) then t else begin
        let id = next_stringreprid () in
        trm_set_stringreprid id t
      end in
    trm_map (label_subterms_with_fresh_stringreprids f) t2



(*****************************************************************************)
(* [build_nested_accesses base access_list]: from a list of accesses build the original trm *)
let build_nested_accesses (base : trm) (access_list : trm_access list) : trm =
  List.fold_left (fun acc access ->
    match access with
    | Struct_access_addr f ->
      trm_apps (trm_unop (Unop_struct_access f)) [acc]
    | Struct_access_get f ->
      trm_apps (trm_unop (Unop_struct_get f)) [acc]
    | Array_access_addr i ->
      trm_apps (trm_binop (Binop_array_access)) [acc;i]
    | Array_access_get i ->
      trm_apps (trm_binop (Binop_array_get)) [acc;i]
  ) base access_list

let trm_def_or_used_vars (t : trm) : Var_set.t =
  let vars = ref Var_set.empty in
  let rec aux t =
    match trm_var_inv t with
    | Some x -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_let_inv t with
    | Some (_, x, _, _) -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_let_fun_inv t with
    | Some (x, _, _, _) -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_for_inv t with
    | Some ((x, _, _, _, _, _), _, _) -> vars := Var_set.add x !vars
    | _ -> ()
    end end end;
    trm_iter aux t
  in
  aux t;
  !vars


(* [trm_simplify_addressof_and_get t]: simplifies [&*t] and [*&t] to [t] *)
let trm_simplify_addressof_and_get (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1], _) }
    ], _)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [t1], _) }
    ], _) -> t1
  | _ -> t

(* [simpl_struct_get_get t]: transform struct_get (get(t1), f) to get(struct_access (t1, f)) *)
let simpl_struct_get_get (t : trm) : trm = (* DEPRECATED? *)
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as op, [t1], _) ->
    begin match t1.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [t2], _) ->
      {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [t2], []))}], []))}
    | _ -> t
    end
  | _ -> t

(* [simpl_array_get t]: tranform array_get (get(t1), index) to get(array_access (t1), index) *)
let rec simpl_array_get_get (t : trm) : trm = (* DEPRECATED? *)
  let aux = simpl_array_get_get in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)));_} as op, [base; index], _) ->
    begin match base.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [base1], _) ->
       {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [base1; index], []))}], []))}
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t
