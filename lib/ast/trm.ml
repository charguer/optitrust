open Ast
open Typ
open Contextualized_error

(* **************************** Referent *************************** *)

(** [trm_find_referent t]: returns the referent of trm [t], transitively.
   (referents should not be cyclic!) *)
let rec trm_find_referent (t : trm) : trm =
  match t.annot.trm_annot_referent with
  | None -> t
  | Some tref -> trm_find_referent tref

(** [trm_annot_set_referent annot t] updates an annotation to specify
   a referent. *)
let trm_annot_set_referent (annot : trm_annot) (target_trm : trm) : trm_annot =
  { annot with trm_annot_referent = Some target_trm }

(* **************************** Errors *************************** *)

(** [trm_error_merge ~from t] returns a copy of [t] with [t.errors] updated
   by appending the errors of [from]. *)
let trm_error_merge ~(from:trm) (t:trm) : trm =
  { t with errors = t.errors @ from.errors }

(* **************************** Attributes *************************** *)
let trm_add_attribute (attr: attribute) (t: trm): trm =
  let annot = { t.annot with trm_annot_attributes = attr :: t.annot.trm_annot_attributes } in
  trm_alter ~annot t

let trm_has_attribute (attr: attribute) (t: trm): bool =
  List.exists (fun a -> a = attr) t.annot.trm_annot_attributes

(* **************************** CStyle *************************** *)

(** [trm_get_cstyles t]: returns all cstyle annotations of trm [t]. *)
let trm_get_cstyles (t : trm) : cstyle_annot list =
  t.annot.trm_annot_cstyle

(** [apply_on_cstyles f t]: applies [f] on the cstyme encodings of [t]. *)
let apply_on_cstyles (f : cstyle_annot list -> cstyle_annot list) (t : trm) : trm =
  let t_annot_cstyle = f (trm_get_cstyles t) in
  let t_annot = { t.annot with trm_annot_cstyle = t_annot_cstyle } in
  trm_alter ~annot:t_annot t

(** [trm_add_cstyle cs t]: adds [cs] cstyle annotation to trm [t]. *)
let trm_add_cstyle (cs : cstyle_annot) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> cs :: cstyles) t

(** [trm_filter_cstyle pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
let trm_filter_cstyle (pred : cstyle_annot -> bool) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> List.filter (fun cs -> pred cs) cstyles) t

(** [trm_rem_cstyle cs t]: removes the cstyle_annot annotation [cs] from trm [t]. *)
let trm_rem_cstyle (cs : cstyle_annot) (t : trm) : trm =
  trm_filter_cstyle (fun cs1 -> cs <> cs1) t

(** [trm_has_cstyle cs t]: checks if [t] has the [cs] cstyle annotation. *)
let trm_has_cstyle (cs : cstyle_annot) (t : trm) : bool =
  let cstyles = trm_get_cstyles t in
  List.mem cs cstyles

(** [annot_has_cstyle cs t_ann]: checks if [cs] is constained in [t_ann]. *)
let annot_has_cstyle (cs : cstyle_annot) (t_ann : trm_annot) : bool =
  let cstyles = t_ann.trm_annot_cstyle in
  List.mem cs cstyles


(* **************************** Smart constructors *************************** *)

(** [trm_var]: create a variable occurence. *)
let trm_var ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (v : var) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_var v)

let trm_toplevel_var ?(annot = trm_annot_default) ?(typ) ?(namespaces: string list = []) (name : string) : trm =
  trm_var ~annot ?typ (toplevel_var ~namespaces name)

(** [trm_lit ~annot ?loc ?ctx l]: literal *)
let trm_lit ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (l : lit) : trm =
  let typ = typ_of_lit l in
  trm_make ~annot:annot ?loc ?ctx ~typ (Trm_lit l)

let trm_unit ?(loc) () : trm =
  trm_lit ?loc Lit_unit
let trm_bool ?(loc) (b : bool) =
  trm_lit ?loc (Lit_bool b)
(* LATER: allow arbitrary sized integer types/values *)
let trm_int ?annot ?(typ: typ = typ_int) ?(loc) ?ctx (i : int) =
  trm_lit ?annot ?loc ?ctx (Lit_int (typ, i))
(* LATER: may need arbitrary sized float values *)
let trm_float ?annot ?loc ?ctx ?(typ : typ = typ_f64) (v : float) =
  trm_lit ?annot ?loc ?ctx (Lit_float (typ, v))
let trm_string ?(loc) (s : string) =
  trm_lit ?loc (Lit_string s)

(** [trm_null ~annot ?loc ?ctx ()]: build the term [nullptr], or [NULL] if [~uppercase:true] *)
let trm_null ?(uppercase : bool = false) ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typ: typ) : trm =
  let t = trm_lit ~annot ?loc ?ctx (Lit_null typ) in
  if uppercase then trm_add_cstyle Display_null_uppercase t else t

(** [trm_let ~annot ?loc ?ctx typed_var init]: variable declaration *)
let trm_let ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_let (typed_var, init))

(** [trm_let_mult ~annot ?loc ?ctx ty tl]: multiple variable declarations *)
let trm_let_mult ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
   (tl : (typed_var * trm) list) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_let_mult tl)

(** [trm_predecl ~annot ?loc ?ctx typed_var]: predeclaration *)
let trm_predecl ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typed_var : typed_var): trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_predecl typed_var)

(** [trm_fun ~annot ?loc args ret_typ body]: anonymous function.  *)
let trm_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: fun_spec = FunSpecUnknown)
  (args : typed_vars) (ret_typ : typ) (body : trm) =
  trm_make ~annot ?loc ?ctx (Trm_fun (args, ret_typ, body, contract))

(** [trm_let_fun ~annot ?loc ?ctx name ret_typ args body]: function definition
  FIXME: Swap argument order between ret_typ and args
*)
let trm_let_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: fun_spec = FunSpecUnknown)
  (v : var) (ret_typ : typ) (args : typed_vars) (body : trm) : trm =
  trm_let ~annot ?loc ?ctx (v, typ_fun ?loc (List.map snd args) ret_typ) (trm_fun ?loc ~contract args ret_typ body)

let trm_fun_inv (t: trm) : (typed_vars * typ * trm * fun_spec) option =
    match t.desc with
    | Trm_fun (args, typ, body, contract) -> Some (args, typ, body, contract)
    | _ -> None

(** [trm_typedef ~annot ?loc ?ctx def_typ]: type definition *)
let trm_typedef ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (def_typ : typedef): trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_typedef def_typ)

(** [trm_if ~annot ?loc ?ctx cond tb eb]: if statement *)
let trm_if ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(typ : typ option) (cond : trm)
  (tb : trm) (eb : trm) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_if (cond, tb, eb))

(** [trm_seq ~annot ?loc ?ctx tl]: block statement *)
let trm_seq ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(result: var option) ?(typ: typ option)
  (tl : trm mlist) : trm =
  trm_make ~annot ?loc ?typ:(if Option.is_none result then Some typ_unit else typ) ?ctx (Trm_seq (tl, result))

(** [trm_seq_nomarks ~annot ?loc ?ctx tl]: like [trm_seq] but takes
   a list as arguments --LATER: use it everywhere it should instead of
  [trm_seq (Mlist_of_list tl)] *)
let trm_seq_nomarks ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (tl : trms) : trm =
  trm_seq ~annot ?loc ?ctx (Mlist.of_list tl)

(** [trm_apps ~annot ?loc ?typ ?ctx f args]: function call *)
let trm_apps ?(annot = trm_annot_default) ?(loc) ?(typ)
  ?(ctx : ctx option) ?(ghost_args = []) (f : trm) (args : trms) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_apps (f, args, ghost_args))

(** [trm_while ~annot ?loc ?ctx cond body]: while loop *)
let trm_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_while (cond, body))

(** [trm_do_while ~annot ?loc ?ctx cond body]: do while loop *)
let trm_do_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)  (body : trm) (cond : trm) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_do_while (body, cond))

(** [trm_for_c ?annot ?loc ?ctx init cond step body]: for loop *)
let trm_for_c ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(invariant: resource_set option) (init : trm) (cond : trm)
  (step : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_for_c (init, cond, step, body, invariant))

(** [trm_switch ~annot ?loc ?ctx cond cases]: switch statement *)
let trm_switch ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm)
  (cases : (trms * trm) list) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_switch (cond, cases))

(** [trm_my_switch ~annot ?loc ?ctx cases]: switch-case statement *)
let trm_my_switch ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cases : (bbtrm * trm) list) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_my_switch cases)

(** [trm_abort ~annot ?loc ?ctx a]: abort instruction *)
let trm_abort ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (a : abort) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_abort a)

(** [trm_goto ~annot ?loc ?ctx l]: goto statement *)
let trm_goto ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (l : label) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_goto l)

(** [trm_for ~annot ?loc ?ctx index start direction stop step body]: simple for loop *)
let trm_for ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: loop_contract = empty_loop_contract)
  (loop_range : loop_range) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:typ_unit ?ctx (Trm_for (loop_range, body, contract))

let trm_for_instrs ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(contract: loop_contract option)
(loop_range : loop_range) (body_instrs : trm mlist) : trm =
  trm_for ~annot ?loc ?ctx ?contract loop_range (trm_seq body_instrs)

(** [code code_str ]: arbitrary code entered by the user *)
let code (code_str : code_kind) : trm =
  trm_make (Trm_arbitrary code_str)

(** [trm_omp_routine ?loc omp_routine] OpenMP routine *)
let trm_omp_routine ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (omp_routine : omp_routine) : trm =
  trm_make ~annot ?loc ?ctx (Trm_omp_routine omp_routine)

(** [extern ?loc ~lang tl]: extern *)
let trm_extern ?(annot = trm_annot_default) ?(loc) (lang : string) (tl : trms) : trm =
  trm_make ~annot ?loc  (Trm_extern (lang, tl))

(** [trm_namespace ?loc ?ctx name t inline ]: namespace *)
let trm_namespace ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (name : string) (t : trm ) (inline : bool) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_namespace (name, t, inline))

(** [trm_template ?loc ?typ ?ctx tpl t]: template statemented *)
let trm_template ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tpl : template_parameter_list) (t : trm ) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_template (tpl, t))

(** [trm_using_directive ~annot ?loc ?typ ?ctx namespace]: creates a using namespace directive. *)
let trm_using_directive ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (namespace : string) =
  trm_make ~annot ?loc ?typ ?ctx (Trm_using_directive namespace)

(* NOTE: var id = inferred_var_id *)
let var_this = name_to_var "this"

(** [trm_this ~annot ?loc ?typ ?ctx ()]: this pointer.
   NOTE: var id = inferred_var_id *)
let trm_this ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) () =
  trm_make ~annot ?loc ?typ ?ctx (Trm_var var_this)




(* ********************************** Auxiliary functions ************************************ *)

(** [trm_unop ~annot ?loc ?ctx p]: unary operator *)
let trm_unop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typ: typ) (p : unary_op) : trm =
  trm_make ~annot ?loc ?ctx (Trm_prim (typ, Prim_unop p))

(** [trm_biop ~annot ?loc ?ctx p]: binary operator *)
let trm_binop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typ: typ) (p : binary_op) : trm =
  trm_make ~annot:annot ?loc ?ctx (Trm_prim (typ, Prim_binop p))


let var_ignore = toplevel_var "__ignore"

let trm_ignore ?(annot = trm_annot_default) ?loc ?ctx t: trm =
  trm_apps ~annot ?loc ?ctx ~typ:typ_unit (trm_var var_ignore) [t]

let var_sizeof = toplevel_var "sizeof"

(** [trm_sizeof]: build a term evaluating to the size of type [ty]. *)
let trm_sizeof ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (ty: typ) : trm =
  trm_apps ~annot ?loc ?ctx ~typ:typ_usize (trm_var var_sizeof) [ty]

(** [trm_prim ~annot ?loc ?ctx p]: primitives *)
let trm_prim ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typ: typ) (p : prim) : trm =
  trm_make ~annot:annot ?loc ?ctx (Trm_prim (typ, p))

(** [trm_array ~annot ?loc ?typ ?ctx tl]: array initialization list *)
let trm_array ?(annot = trm_annot_default) ?(loc) ~(elem_typ) ?(ctx : ctx option)
  (tl : trm list) : trm =
  let typ = typ_array elem_typ ~size:(trm_int (List.length tl)) in
  trm_apps ~annot ?loc ~typ ?ctx (trm_prim typ Prim_array) tl

(** [trm_record ~annot ?loc ?typ ?ctx tl]: struct initialization list *)
let trm_record ?(annot = trm_annot_default) ?(loc) ~(typ) ?(ctx : ctx option)
  (tl : trm list) : trm =
  trm_apps ~annot ?loc ~typ ?ctx (trm_prim typ Prim_record) tl

let trm_step_one_pre () = trm_add_cstyle Prefix_step (trm_int 1)
let trm_step_one_post () = trm_add_cstyle Postfix_step (trm_int 1)
let trm_step_one = trm_step_one_post

type ghost_call = {
  ghost_fn: trm;
  ghost_args: (var * formula) list;
}

let trm_ghost_force ({ ghost_fn; ghost_args } : ghost_call): trm =
  trm_add_attribute GhostCall (trm_apps ghost_fn [] ~ghost_args)

let ghost_call (ghost_var: var) (ghost_args: (string * formula) list): ghost_call =
  { ghost_fn = trm_var ghost_var; ghost_args = List.map (fun (g, t) -> (name_to_var g, t)) ghost_args }

let ghost_closure_call contract body =
  { ghost_fn = trm_fun [] typ_auto ~contract:(FunSpecContract contract) body; ghost_args = [] }

(*****************************************************************************)

(** [trm_get_attr t]: returns all the attributes of trm [t]. *)
let trm_get_attr (t : trm) : attribute list =
  t.annot.trm_annot_attributes

(** [trm_attr_add att t]: adds attribute [att] to trm [t] *)
let trm_attr_add (att : attribute) (t : trm) : trm =
  let t_annot_attributes = t.annot.trm_annot_attributes in
  let t_annot = {t.annot with trm_annot_attributes = att :: t_annot_attributes} in
  trm_alter ~annot:t_annot t


(*****************************************************************************)

(** [trm_is_statement t]: checks if [t] corresponds to a statement or not *)
let trm_is_statement (t : trm) : bool =
  match t.desc with
  | Trm_let _ | Trm_typedef _ | Trm_if _ | Trm_seq _ | Trm_while _
  | Trm_do_while _ | Trm_for_c _ | Trm_for _ | Trm_switch _ | Trm_abort _ | Trm_goto _  -> true
  | Trm_apps _ ->
    begin match t.typ with
    | Some ty when is_typ_unit ty -> true
    | _ -> false
    end
  | _ -> false

(* ********************************** Annotation manipulation ************************************ *)
(**** Attributes  ****)



(**** Labels  ****)

(** [trm_get_labels t]: gets all the labels of trm [t]. *)
let trm_get_labels (t : trm) =
  t.annot.trm_annot_labels

 (** [apply_on_labels f t]: applies [f] on the labels of [t]. *)
 let apply_on_labels (f : marks -> marks) (t : trm) : trm =
   let t_labels = trm_get_labels t in
   let t_annot_labels = f t_labels in
   let t_annot = {t.annot with trm_annot_labels = t_annot_labels} in
   trm_alter ~annot:t_annot t

 (** [trm_add_label l] adds label [l] to trm [t].
    Returns [t] unchanged if [l = ""]. *)
let trm_add_label (l : label) (t : trm) : trm =
  if l = "" then t else apply_on_labels (fun labels -> l :: labels) t

 (** [trm_filter_label pred t]: filters all labels that satisfy predicate [pred]. *)
 let trm_filter_label (pred : label -> bool) (t : trm) : trm =
   apply_on_labels (fun labels -> List.filter (fun l -> pred l) labels) t

 (** [trm_rem_label l t]: removes label [l] from trm [t]. *)
 let trm_rem_label (l : label) (t : trm) : trm =
   trm_filter_label (fun l1 -> l <> l1) t

 (** [trm_rem_labels t]: removes all the labels from trm [t]. *)
 let trm_rem_labels (t : trm) : trm =
   apply_on_labels (fun _ -> []) t

 (** [trm_has_label l t]: checks if trm [t] has label [l]. *)
 let trm_has_label (l : label) (t : trm) : bool =
   let t_labels = trm_get_labels t in
   List.mem l t_labels

 (** [trm_pass_labels t1 t2]: passes the labels of trm [t1] to trm [t2]. *)
 let trm_pass_labels (t1 : trm) (t2 : trm) : trm =
   let t1_labels = trm_get_labels t1 in
   let t2_labels = trm_get_labels t2 in
   let t2_annot = {t2.annot with trm_annot_labels = t2_labels @ t1_labels} in
   {t2 with annot = t2_annot}

 (**** Stringrepr  ****)

 (** [trm_set_stringreprid id t]: sets the string representation id [t] to [id]. *)
 let trm_set_stringreprid (id : stringreprid) (t : trm) : trm =
   let annot = {t.annot with trm_annot_stringrepr = Some id} in
   trm_alter ~annot t

 (** [trm_get_stringreprid t]: gets the string representation of trm [t]. *)
 let trm_get_stringreprid (t : trm) : stringreprid option =
   t.annot.trm_annot_stringrepr


 (**** CPragmas  ****)

 (** [apply_on_pragmas f t]: applies [f] on the pragma directives associated with [t]. *)
 let apply_on_pragmas (f : cpragma list -> cpragma list) (t : trm) : trm =
   let t_annot_pragmas = f (t.annot.trm_annot_pragma) in
   let annot = {t.annot with trm_annot_pragma = t_annot_pragmas} in
   trm_alter ~annot t

 (** [trm_add_pragma p t]: adds the pragma [p] into [t]. *)
 let trm_add_pragma (p : cpragma) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> p :: pragmas) t

 let trm_add_pragmas (p : cpragma list) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> p @ pragmas) t

 (** [trm_filter_pragma pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
 let trm_filter_pragma (pred : cpragma -> bool) (t : trm) : trm =
   apply_on_pragmas (fun pragmas -> List.filter (fun p -> pred p) pragmas) t

 (** [trm_rem_pragma p t]: removes the pragma [p] from [t]. *)
 let trm_rem_pragma (p : cpragma) (t : trm) : trm =
   trm_filter_pragma (fun p1 -> p <> p1) t

 (** [trm_get_pragmas t]: returns all the pragmas annotated to [t]. *)
 let trm_get_pragmas (t : trm) : cpragma list =
   t.annot.trm_annot_pragma

 (** [trm_has_pragma pred t]: check if [t] has pragmas that satisfy [pred]. *)
 let trm_has_pragma (pred : cpragma -> bool) (t : trm) : bool =
   let t_pragmas = trm_get_pragmas t in
   List.exists pred t_pragmas

 (** [trm_pass_pragmas t1 t2]: pass pragmas of trm [t1] to trm [t2]. *)
 let trm_pass_pragmas (t1 : trm) (t2 : trm) : trm =
   let t1_pragmas = trm_get_pragmas t1 in
   let t2_pragmas = trm_get_pragmas t2 in
   let t2_annot = {t2.annot with trm_annot_pragma = t1_pragmas @ t2_pragmas} in
   {t2 with annot = t2_annot}

 (**** Files  ****)

 (** [trm_set_mainfile]: adds [Main_file] annotation to trm [t]. *)
 let trm_set_mainfile (t : trm) : trm =
    let annot = {t.annot with trm_annot_file = Main_file} in
    trm_alter ~annot t

 (** [trm_set_include filename t]: add [Include filename] annotation to trm [t]. *)
 let trm_set_include (filename : string) (t : trm) : trm =
   let annot = {t.annot with trm_annot_file = Included_file filename} in
   trm_alter ~annot t

 (** [trm_is_mainfile t]: checks if [t] contains the [Main_file] annotation. *)
 let trm_is_mainfile (t : trm) : bool =
   t.annot.trm_annot_file = Main_file

 (** [trm_is_include]: checks if [t] contains the [Include f] annotation. *)
 let trm_is_include (t : trm) : bool =
   match t.annot.trm_annot_file with
   | Included_file _ -> true
   | _ -> false

(** [trm_include_inv]: return the included file corresponding to t *)
 let trm_include_inv (t : trm) : string option=
   match t.annot.trm_annot_file with
   | Included_file f -> Some f
   | _ -> None

 (** [trm_is_nobrace_seq t]: checks if [t] is a visible sequence or not *)
 let trm_is_nobrace_seq (t : trm) : bool =
   List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle

(** [trm_vardef_get_trm_varse]: gets the singleton declaration variable in the case when [t] is a variable declaration
    or a list of variable in the case when we have multiple variable declarations in one line *)
let trm_vardef_get_vars (t : trm) : var list =
  match t.desc with
  | Trm_let ((x, _), _) -> [x]
  | Trm_let_mult tl -> List.map (fun ((x, _), _) -> x) tl
  | _ -> []

(** [trm_ret ~annot a]; special trm_abort case, used for return statements *)
let trm_ret ?(annot = trm_annot_default) ?loc ?ctx (a : trm option) : trm =
  trm_abort ~annot ?loc ?ctx (Ret a)

(** [trm_prim_inv t]: gets the primitive operation *)
let trm_prim_inv (t : trm) : (typ * prim) option =
  match t.desc with
  | Trm_prim (typ, p) -> Some (typ, p)
  | _ -> None

(** [trm_lit_inv t]: gets the literal from a literal trm *)
let trm_lit_inv (t : trm) : lit option =
  match t.desc with
  | Trm_lit v -> Some v
  | _ -> None

(** [trm_int_inv t] gets an int literal from a trm *)
let trm_int_inv (t : trm) : int option =
  match trm_lit_inv t with
  | Some (Lit_int (_, n)) -> Some n
  | _ -> None

(** [trm_float_inv t] gets a float literal from a trm *)
let trm_float_inv (t : trm) : float option =
  match trm_lit_inv t with
  | Some (Lit_float (_, n)) -> Some n
  | _ -> None

(** [trm_bool_inv t] gets a bool literal from a trm *)
let trm_bool_inv (t: trm) : bool option =
  match trm_lit_inv t with
  | Some (Lit_bool b) -> Some b
  | _ -> None

(** [trm_is_one step]: checks if the step of the loop is one or not *)
let trm_is_one (step : trm) : bool =
  match trm_int_inv step with
  | Some 1 -> true
  | _ -> false

(** [trm_inv ~error k t]: returns the results of applying [k] on t, if the result is [None]
     then function fails with error [error]. *)
let trm_inv ?(error : string = "") (k : trm -> 'a option) (t : trm) : 'a =
  match k t with
  | None -> trm_fail t (if error = "" then "failed inversion" else error)
  | Some r -> r



(** [trm_let_inv t]: returns the components of a [trm_let] constructor if [t] is a let declaration.
     Otherwise it returns [None]. *)
let trm_let_inv (t : trm) : (var * typ * trm) option =
  match t.desc with
  | Trm_let ((x, tx), init) -> Some (x, tx, init)
  | _ -> None

(** [trm_let_mult_inv t]: returns the components of a [trm_let_mult] constructor if [t] is a multiple let declaration.
     Otherwise it returns [None]. *)
let trm_let_mult_inv (t : trm) : (typed_var * trm) list option =
  match t.desc with
  | Trm_let_mult ts -> Some ts
  | _ -> None

(** [trm_predecl_inv t]: returns the components of a [trm_predecl] constructor if [t] is a predeclaration.
     Otherwise it returns [None]. *)
let trm_predecl_inv (t : trm) : typed_var option =
  match t.desc with
  | Trm_predecl tv -> Some tv
  | _ -> None

(** [trm_let_fun_inv t]: returns the componnets of a [trm_let_fun] constructor if [t] is a function declaration.
     Otherwise it returns a [None].
    TODO: Refactor this function : ret_typ should go after arguments
*)
let trm_let_fun_inv (t : trm) : (var * typ * typed_vars * trm * fun_spec) option =
  let open Option.Monad in
  let* xf, _, tf = trm_let_inv t in
  let* args, ret_ty, body, fun_spec = trm_fun_inv tf in
  Some (xf, ret_ty, args, body, fun_spec)

(** [trm_apps_inv t]: returns the components of a [trm_apps] constructor in case [t] is function application.
    Otherwise it returns [None]. *)
let trm_apps_inv (t : trm) : (trm * trm list) option =
  match t.desc with
  | Trm_apps (f, tl, _) -> Some (f, tl)
  | _ -> None

(** [trm_seq_inv t]: returns the components of a [trm_seq] constructor when [t] is a sequence.
    Otherwise it returns [None]. *)
let trm_seq_inv (t : trm) : (trm mlist * var option) option =
  match t.desc with
  | Trm_seq (tl, result) ->  Some (tl, result)
  | _ -> None

let trm_seq_nth_inv (i : int) (t : trm) : trm option =
  Option.bind (trm_seq_inv t) (fun (instrs, _) ->
    if i < Mlist.length instrs
    then Some (Mlist.nth instrs i)
    else None
  )

(** [trm_var_inv t]: returns the components of a [trm_var] constructor when [t] is a variable occurrence.
    Otherwise it returns [None]. *)
let trm_var_inv (t : trm) : var option =
  match t.desc with
  | Trm_var x -> Some x
  | _ -> None

let trm_array_inv (t: trm) : (typ * trm list) option =
  match trm_apps_inv t with
  | Some (f, args) ->
    begin match trm_prim_inv f with
    | Some (typ, Prim_array) -> Some (typ, args)
    | _ -> None
    end
  | _ -> None

let trm_record_inv (t: trm) : (typ * trm list) option =
  match trm_apps_inv t with
  | Some (f, args) ->
    begin match trm_prim_inv f with
    | Some (typ, Prim_record) -> Some (typ, args)
    | _ -> None
    end
  | _ -> None

(** [trm_ignore_inv]: deconstructs a 'ignore(x)' call. *)
let trm_ignore_inv (t: trm): trm option =
  match trm_apps_inv t with
  | Some (f, [x]) ->
    begin match trm_var_inv f with
    | Some f_var when var_eq f_var var_ignore -> Some x
    | _ -> None
    end
  | _ -> None

let trm_sizeof_inv (t: trm): typ option =
  match trm_apps_inv t with
  | Some (f, [ty]) ->
    begin match trm_var_inv f with
    | Some f_var when var_eq f_var var_sizeof -> Some ty
    | _ -> None
    end
  | _ -> None

(** [trm_if_inv t]: returns the components of a [trm_if] constructor when [t] is an if statement.
    Otherwise it returns [None]. *)
let trm_if_inv (t : trm) : (trm * trm * trm) option =
  match t.desc with
  | Trm_if (cond, then_, else_) -> Some (cond, then_, else_)
  | _ -> None

(** [trm_typedef_inv t]: returns the components of a [trm_typedef] constructor when [t] is a type definition. *)
let trm_typedef_inv (t : trm) : typedef option =
  match t.desc with
  | Trm_typedef td -> Some td
  | _ -> None

(** [trm_unop_inv t]: deconstructs t = op t1 *)
let trm_unop_inv (op: unary_op) (t : trm) : trm option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (typ, Prim_unop op'), [a] when op = op' -> Some a
    | _ -> None
    end
  | _ -> None

(** [trm_binop_inv t]: deconstructs t = t1 op t2 *)
let trm_binop_inv (op : binary_op) (t : trm) : (trm * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (typ, Prim_binop op'), [a; b] when op = op' -> Some (a, b)
    | _ -> None
    end
  | _ -> None

let trm_cast_inv (t : trm) : (typ * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (ty_from, Prim_unop (Unop_cast ty_to)), [a] -> Some (ty_to, a)
    | _ -> None
    end
  | _ -> None

(** [trm_compound_assign_inv t]: deconstructs t1 =# t2 *)
let trm_compound_assign_inv (op : binary_op) (t : trm) : (trm * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (typ, Prim_compound_assign_op op'), [a; b] when op = op' -> Some (a, b)
    | _ -> None
    end
  | _ -> None

let trm_get_inv (t : trm) : trm option =
  trm_unop_inv Unop_get t

(* LATER: rename to trm_get_var_inv *)
let trm_var_get_inv (t : trm) : var option =
  match trm_get_inv t with
  | Some t2 -> trm_var_inv t2
  | None -> None

(** [trm_prod_inv t]: gets a the list of factors involved in a multiplication*)
let trm_prod_inv (t : trm) : trm list =
  let rec aux (indepth : bool) (acc : trm list) (t : trm) : trm list =
    match t.desc with
    | Trm_apps ({desc = Trm_prim (_, Prim_binop (Binop_mul)); _}, [l; r], _) -> (aux true acc l) @ (aux true acc r)
    | _ -> if indepth then acc @ [t] else acc
  in aux false [] t

(** [trm_mlist_inv_marks t] gets the description of marks in a term that
   contains a Mlist *)
let trm_mlist_inv_marks (t : trm) : mark list list option =
  match t.desc with
  | Trm_seq (tl, _) -> Some (Mlist.get_marks tl)
  | _ -> None



(** [decl_name t]: returns the name of the declared variable/function. *)
let decl_name (t : trm) : var option =
  match t.desc with
  | Trm_let ((x,_),_) -> Some x
  | _ -> None

(** [vars_bound_in_trm_init t]: gets the list of variables that are bound inside the initialization trm of the for_c loop*)
let vars_bound_in_trm_init (t : trm) : var list =
  match t.desc with
  | Trm_let ((x,_), _) -> [x]
  | Trm_let_mult ts -> (List.map (fun ((x, _), _) -> x)) ts
  | _ -> []

(** [for_loop_index t]: returns the index of the loop [t] *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for (l_range,  _, _) ->
     l_range.index
  | Trm_for_c (init, _, _, _, _) ->
     (* covered cases:
        - for (i = …; …)
        - for (int i = …; …) *)
     begin match init.desc with
    | Trm_apps ({desc = Trm_prim (_, Prim_binop Binop_set); _},
                 [{desc = Trm_var x; _}; _], _) -> x
     | _ -> begin match trm_var_inv init with
            | Some x -> x
            | None -> trm_fail init "Ast.for_loop_index: could't get the loop index"
            end
     end
  | _ -> trm_fail t "Ast.for_loop_index: expected for loop"

(** [for_loop_body_trms t]: gets the list of trms from the body of the loop *)
let for_loop_body_trms (t : trm) : trm mlist =
  match t.desc with
  | Trm_for (_, body, _) ->
    begin match body.desc with
    | Trm_seq (tl, _) -> tl
    | _ -> trm_fail body "Ast.for_loop_body_trms: body of a simple loop should be a sequence"
    end
  | Trm_for_c (_, _, _, body, _) ->
    begin match body.desc with
    | Trm_seq (tl, _) -> tl
    | _ -> trm_fail body "Ast.for_loop_body_trms: body of a generic loop should be a sequence"
    end
  | _ -> trm_fail t "Ast.for_loop_body_trms: expected a loop"

(*****************************************************************************)

(** [trm_main_inv_toplevel_defs ast]: returns a list of all toplevel declarations *)
let trm_main_inv_toplevel_defs (ast : trm) : trm list =
  match ast.desc with
  | Trm_seq (tl, None) when trm_is_mainfile ast -> Mlist.to_list tl
  | _ -> trm_fail ast "Ast.trm_main_inv_toplevel_defs: expected the ast of the main file"

(** [trm_seq_add_last t_insert t]: appends [t_insert] at the end of the sequence [t] *)
let trm_seq_add_last (t_insert : trm) (t : trm) : trm =
  match t.desc with
  | Trm_seq (tl, result) ->
    let new_tl = Mlist.push_back t_insert tl in
    trm_seq ~annot:t.annot ?result new_tl
  | _ -> trm_fail t "Ast.trm_seq_add_last: expected a sequence"


(** [is_get_operation t]: checks if [t] is a get operation(read operation) *)
let is_get_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get)}, _, _) -> true
  | _ -> false

(** [is_ref_operation t] checks if [t] is new operation *)
let is_ref_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _, _) ->
    begin match trm_prim_inv f with
    | Some (_, Prim_ref) -> true
    | _ -> false
    end
  | _ -> false

let trm_set_inv (t : trm) : (trm * trm) option =
  trm_binop_inv Binop_set t

(** [is_set_operation t]: checks if [t] is a set operation(write operation) *)
let is_set_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _, _) ->
    begin match trm_prim_inv f with
    | Some (_, Prim_binop Binop_set) | Some (_, Prim_compound_assign_op _) -> true
    | _ -> false
    end
  | _ -> false


(** [is_compound_assignment]: checks if [t] is a compound assignment *)
let is_compound_assignment (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_prim (_, Prim_compound_assign_op _)}, _, _) -> true
  | _ -> false

(** [is_access t]: check if t is a struct or array access *)
let is_access (t : trm) : bool = match t.desc with
  | Trm_apps (f, _, _) ->
    begin match trm_prim_inv f with
    | Some (_, p) ->
      begin match p with
      | Prim_unop (Unop_struct_access _) | Prim_unop (Unop_struct_get _) | Prim_binop (Binop_array_access)
      | Prim_binop (Binop_array_get) -> true
      | _ -> false
      end
    | None -> false
    end
  | _ -> false

(** [get_operation_arg t]: gets the arg of a get operation. *)
let get_operation_arg (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get); _}, [t1], _) -> t1
  | _ -> t

(** [ref_operation_arg t]: get the argument of the encoded new operation. *)
let ref_operation_arg (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, [arg], _) when is_ref_operation t -> arg
  | _ -> t


(** [compute_app_unop_value p v1]: simplifies unary operations on literals. *)
let compute_app_unop_value (p : unary_op) (v1:lit) : lit option =
  match p, v1 with
  | Unop_neg, Lit_bool b -> Some (Lit_bool (not b))
  | Unop_minus, Lit_int (ty, n) -> Some (Lit_int (ty, -n))
  | Unop_plus, Lit_int (ty, n) -> Some (Lit_int (ty, +n))
  | Unop_minus, Lit_float (ty, x) -> Some (Lit_float (ty, -.x))
  | Unop_plus, Lit_float (ty, x) -> Some (Lit_float (ty, +.x))
  | _ -> None

(** [compute_app_binop_value]: simplifies binary operations on literals. *)
let compute_app_binop_value (p : binary_op) (v1 : lit) (v2 : lit) : lit option =
  match p,v1, v2 with
  | Binop_eq , Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 == n2))
  | Binop_eq, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 == d2))
  | Binop_neq , Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 <> n2))
  | Binop_neq, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 <> d2))
  | Binop_add, Lit_int (typ, n1), Lit_int (_, n2) -> Some (Lit_int (typ, n1 + n2))
  | Binop_add, Lit_float (typ, d1), Lit_float (_, d2) -> Some (Lit_float (typ, d1 +. d2))
  | Binop_sub, Lit_int (typ , n1), Lit_int (_, n2) -> Some (Lit_int (typ, n1 - n2))
  | Binop_sub, Lit_float (typ, d1), Lit_float (_, d2) -> Some (Lit_float (typ, d1 -. d2))
  | Binop_mul, Lit_int (typ, n1), Lit_int (_, n2) -> Some (Lit_int (typ, n1 * n2))
  | Binop_mul, Lit_float (typ, n1), Lit_float (_, n2) -> Some (Lit_float (typ, n1 *. n2))
  | Binop_exact_div, Lit_float (typ, d1), Lit_float (_, d2) -> Some (Lit_float (typ, d1 /. d2))
  | Binop_trunc_div, Lit_int (typ, n1), Lit_int (_, n2) -> Some (Lit_int (typ, n1 / n2))
  | Binop_trunc_mod, Lit_int (typ, n1), Lit_int (_, n2) -> Some (Lit_int (typ, n1 mod n2))
  | Binop_le, Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 <= n2))
  | Binop_le, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 <= d2))
  | Binop_lt, Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 < n2))
  | Binop_lt, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 < d2))
  | Binop_ge, Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 >= n2))
  | Binop_ge, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 >= d2))
  | Binop_gt, Lit_int (_, n1), Lit_int (_, n2) -> Some (Lit_bool (n1 > n2))
  | Binop_gt, Lit_float (_, d1), Lit_float (_, d2) -> Some (Lit_bool (d1 > d2))
  | _ -> None

(** [decl_list_to_typed_vars tl]: converts a list of variable declarations to a list of paris where each pair
  consists of a variable and its type *)
let decl_list_to_typed_vars (tl : trms) : typed_vars =
  List.map (fun t ->
    match t.desc with
    | Trm_let ((x, tx),_) -> (x, get_inner_ptr_type tx)
    | _ -> trm_fail t "Ast.decl_list_to_typed_vars: expected a list of declarations"
  ) tl

(** [trm_is_var t]: checks if [t] is a variable occurrence. *)
let trm_is_var (t : trm) : bool =
  match t.desc with
  | Trm_var _ -> true
  | _ -> false

(** [trm_is_val_or_var t]: checks if [t] is a variable occurrence or a value *)
let rec trm_is_val_or_var (t : trm) : bool =
match t.desc with
| Trm_var _ | Trm_lit _ | Trm_prim _ -> true
| Trm_apps (_, [var_occ], _) when is_get_operation t -> trm_is_val_or_var var_occ
| _ -> false

(** [is_prefix_unary unop]: checks if [unop] is a prefix unary operator *)
let is_prefix_unary (unop : unary_op) : bool =
  match unop with
  | Unop_pre_incr | Unop_pre_decr -> true
  | _ -> false

(** [is_postfix_unary unop]: checks if [unop] is a postfix unary operator *)
let is_postfix_unary (unop : unary_op) : bool =
  match unop with
  | Unop_post_incr | Unop_post_decr -> true
  | _ -> false

let is_unary_compound_assign (unop : unary_op) : bool =
  (is_prefix_unary unop) || (is_postfix_unary unop)

(** [trm_is_unary_compound_assign t] checks whether [t] represents a unary  compound assignment: e.g., increment or decrement operation. *)
let trm_is_unary_compound_assign (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_prim (_, Prim_unop op); _}, _, _) when is_unary_compound_assign op -> true
  | _ -> false

(** [trm_for_inv t]: gets the loop range from loop [t] *)
let trm_for_inv (t : trm) : (loop_range * trm * loop_contract)  option =
match t.desc with
| Trm_for (l_range, body, contract) -> Some (l_range, body, contract)
| _ -> None

(** [trm_for_inv_instrs t]: gets the loop range and body instructions from loop [t]. *)
let trm_for_inv_instrs (t : trm) : (loop_range * trm mlist * loop_contract) option =
  let open Option.Monad in
  let* r, b, c = trm_for_inv t in
  let* instrs, _ = trm_seq_inv b in
  Some (r, instrs, c)

(** [is_trm_seq t]: checks if [t] is a sequence. *)
let is_trm_seq (t : trm) : bool =
match t.desc with
| Trm_seq _ -> true  | _ -> false

(** [trm_fors rgs tbody] creates nested loops with the main body [tbody] each
  nested loop takes its components from [rgs]. *)
let trm_fors (rgs : loop_range list) (tbody : trm) : trm =
  List.fold_right (fun l_range acc ->
    trm_for l_range (if (is_trm_seq acc) then acc else trm_seq_nomarks [acc])
  ) rgs tbody

(** [trm_fors_inv nb t]: gets a list of loop ranges up to the loop at depth [nb] from nested loops [t] *)
let trm_fors_inv (nb : int) (t : trm) : ((loop_range * loop_contract) list * trm) option =
  let rec aux (nb : int) (ranges_rev: (loop_range * loop_contract) list) (t : trm)
    : ((loop_range * loop_contract) list * trm) option =
    if nb = 0 then Some (List.rev ranges_rev, t) (* arrived at requested body *)
    else
      let t = if ranges_rev = []
        (* at first, 't' should be the for loop *)
        then Some t
        (* then, 't' is the sequence of a surrounding loop *)
        else Option.bind (trm_seq_inv t) (fun (instrs, _) -> Mlist.nth_opt instrs 0)
      in
      let open Option.Monad in
      let* t in
      let* range, body, contract = trm_for_inv t in
      aux (nb - 1) ((range, contract) :: ranges_rev) body
  in
  aux nb [] t

let trm_ref_inv (t : trm) : (typ * trm) option =
  match trm_apps_inv t with
  | Some (f, [v]) ->
    begin match trm_prim_inv f with
    | Some (ty, Prim_ref) -> Some (ty, v)
    | _ -> None
    end
  | _ -> None

(** [trm_ref_inv_init t]: gets the value of a variable initialization. *)
let trm_ref_inv_init (t : trm) : trm option =
  Option.map snd (trm_ref_inv t)

let trm_ref_uninit_inv (t: trm) : typ option =
  match trm_apps_inv t with
  | Some (f, []) ->
    begin match trm_prim_inv f with
    | Some (ty, Prim_ref_uninit) -> Some ty
    | _ -> None
    end
  | _ -> None

let trm_ref_any_inv (t : trm) : typ option =
  match trm_apps_inv t with
  | Some (f, _) ->
    begin match trm_prim_inv f with
    | Some (ty, (Prim_ref | Prim_ref_uninit)) -> Some ty
    | _ -> None
    end
  | _ -> None

let trm_ref_maybe_init_inv (t : trm) : (typ * trm option) option =
  match trm_ref_inv t with
  | Some (ty, init) -> Some (ty, Some init)
  | None ->
    match trm_ref_uninit_inv t with
    | Some ty -> Some (ty, None)
    | None -> None

let trm_new_inv (t: trm): (typ * trm) option =
  match trm_apps_inv t with
  | Some (f, [v]) ->
    begin match trm_prim_inv f with
    | Some (ty, Prim_new) -> Some (ty, v)
    | _ -> None
    end
  | _ -> None

let trm_new_uninit_inv (t: trm): typ option =
  match trm_apps_inv t with
  | Some (f, []) ->
    begin match trm_prim_inv f with
    | Some (ty, Prim_new_uninit) -> Some ty
    | _ -> None
    end
  | _ -> None

let trm_delete_inv (t: trm) : trm option =
  match trm_apps_inv t with
  | Some (f, [p]) ->
    begin match trm_prim_inv f with
    | Some (ty, Prim_delete) -> Some p
    | _ -> None
    end
  | _ -> None

let is_trm_ref_uninit (t : trm) : bool =
  match trm_ref_uninit_inv t with
  | Some _ -> true
  | None -> false

(** [is_infix_prim_fun p]: checks if the primitive function [p] is one of those that supports app and set operations or not *)
let is_infix_prim_fun (p : prim) : bool =
  match p with
  | Prim_compound_assign_op _ -> true
  | Prim_binop op ->
    begin match op with
    | Binop_add | Binop_sub | Binop_mul | Binop_exact_div | Binop_trunc_div | Binop_trunc_mod | Binop_shiftl | Binop_shiftr | Binop_bitwise_and | Binop_bitwise_or -> true
    | _ -> false
    end
  | _ -> false

(** [get_binop_from_prim p]: if [p] is a binop operation then return its underlying binary operation *)
let get_binop_from_prim (p : prim) : binary_op option =
  match p with
  | Prim_compound_assign_op binop -> Some binop
  | Prim_binop binop -> Some binop
  | _ -> None

(** [is_arith_fun p]: checks if the primitive function [p] is an arithmetic operation or not *)
let is_arith_fun (p : prim) : bool =
  match p with
  | Prim_binop bin_op ->
    begin match bin_op with
    | Binop_add | Binop_sub | Binop_mul | Binop_exact_div | Binop_trunc_div | Binop_trunc_mod -> true
    | _ -> false
    end
  | _ -> false

(** [is_prim_arith p]: checks if [p] is a primitive arithmetic operation *)
let is_prim_arith (p : prim) : bool =
  match p with
  | Prim_binop (Binop_add | Binop_sub | Binop_mul | Binop_exact_div | Binop_trunc_div | Binop_trunc_mod)
  | Prim_unop Unop_neg ->
      true
  | _ -> false

(** [is_prim_arith_call t]: checks if [t] is a function call to a primitive arithmetic operation *)
let is_prim_arith_call (t : trm) : bool =
  match t.desc with
  | Trm_apps ({desc = Trm_prim (_, p)}, args, _) when is_prim_arith p -> true
  | _ -> false

(** [trm_struct_access ~annot ?typ base field]: creates a struct_access encoding *)
let trm_struct_access ?(annot = trm_annot_default) ?(loc: location) ?(field_typ : typ option) ~(struct_typ: typ) (base : trm) (field : field) : trm =
  trm_apps ~annot ?loc ?typ:(Option.map typ_ptr field_typ) (trm_unop struct_typ (Unop_struct_access field)) [base]

(** [trm_struct_access_inv t]: if [t] is  a struct access then return its base and the accessed field; else None *)
let trm_struct_access_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_prim (typ, Prim_unop (Unop_struct_access f));_}, [base], _) -> Some ({ base with typ = Some (typ_ptr typ) }, f)
  | _ -> None

(** [trm_struct_get ~annot ?typ base field]: creates a struct_get encoding *)
let trm_struct_get ?(annot = trm_annot_default) ?(loc: location) ?(field_typ : typ option) ~(struct_typ: typ) (base : trm) (field : field) : trm =
  trm_apps ~annot ?loc ?typ:field_typ (trm_unop struct_typ (Unop_struct_get field)) [base]

(** [struct_get_inv t]: if [t] is a struct get then return its base and the accesses field; else none *)
let trm_struct_get_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_prim (typ, Prim_unop (Unop_struct_get f));_}, [base], _) -> Some ({ base with typ = Some typ }, f)
  | _ -> None

(** [trm_array_access ~annot ?typ base index]: creates array_access(base, index) encoding *)
let trm_array_access ?(annot = trm_annot_default) ?(loc: location) ?(elem_typ : typ option) (base : trm) (index : trm) : trm =
  let elem_typ = Option.or_ elem_typ (Option.bind base.typ typ_ptr_inv) in
  trm_apps ~annot ?loc ?typ:(Option.map typ_ptr elem_typ) (trm_binop typ_auto Binop_array_access) [base; index]

let trm_array_access_inv (t : trm) : (trm * trm) option =
  trm_binop_inv Binop_array_access t

(** [trm_array_get ~annot ?typ base index]: creates array_get (base, index) encoding *)
let trm_array_get ?(annot = trm_annot_default) ?(loc: location) ?(typ : typ option) (base : trm) (index : trm) : trm =
  trm_apps ~annot ?loc ?typ (trm_binop typ_auto Binop_array_get) [base; index]

let trm_array_get_inv (t : trm) : (trm * trm) option =
  trm_binop_inv Binop_array_get t

(** [trm_get ~annot ?typ t]: embeds [t] into a get operation *)
let trm_get ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  let typ = Option.or_ typ (Option.bind t.typ typ_ptr_inv) in
  trm_apps ~annot ?typ (trm_unop typ_auto Unop_get) [t]

(** [trm_address_of ~anot ?typ t]: creates an address operation in [t] *)
let trm_address_of ?(annot = trm_annot_default) ?(arg_typ : typ option) (t : trm) : trm =
  let typ = Option.or_ arg_typ t.typ in
  trm_apps ~annot ?typ:(Option.map typ_ptr typ) (trm_unop typ_auto Unop_address) [t]

(** [trm_var_get ?typ x]: generates *x *)
let trm_var_get ?(typ : typ option) (x : var) : trm =
  trm_get ?typ (trm_var ?typ:(Option.map typ_ptr typ) x)

(** [trm_var_possibly_get ~const ?typ x]: reads the value of x, it can be x or *x  *)
let trm_var_possibly_get ~(const : bool) ?(typ : typ option) (x : var) : trm =
  if const then trm_var ?typ x else trm_var_get ?typ x

(** [trm_get_array_access base index]: generates get(array_access (base, index)) *)
let trm_get_array_access (base : trm) (index : trm) : trm =
  trm_get (trm_array_access base index)

(** [trm_get_array_access_inv t] returns the Some(base, index) of an array_access if [t]
     is of the form get(array_access(base, index) otherwise None *)
let trm_get_array_access_inv (t : trm) : (trm * trm) option =
  Option.bind (trm_get_inv t) trm_array_access_inv

(** [trm_get_struct_access_inv t]: if [t] is of the form get(struct_access (f, base)) returns Some (f,base); else None *)
let trm_get_struct_access_inv (t : trm) : (trm * field) option =
  Option.bind (trm_get_inv t) trm_struct_access_inv

(** [trm_post_incr ~annot ?typ t]: returns [t++] *)
let trm_post_incr ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ~annot ?typ (trm_unop (typ_or_auto typ) Unop_post_incr) [t]

(** [trm_post_decr ~annot ?typ t]: returns [t--] *)
let trm_post_decr ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ~annot ?typ (trm_unop (typ_or_auto typ) Unop_post_decr) [t]

(** [trm_pre_incr ~annot ?typ t]: returns [++t] *)
let trm_pre_incr ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ~annot ?typ (trm_unop (typ_or_auto typ) Unop_pre_incr) [t]

(** [trm_pre_decr ~annot ?typ t]: returns [--t] *)
let trm_pre_decr ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ~annot ?typ (trm_unop (typ_or_auto typ) Unop_pre_decr) [t]


(*****************************************************************************)

(** [trm_pat_var]: similarly to [trm_var], creates a variable occurence. To be used when creating switch clauses.*)
let trm_pat_var ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (v : var) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_pat_var v)

(** [trm_pat_as]: creates an alias in a bbtrm. To be used when creating switch clauses.*)
let trm_pat_as ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (t : trm) (v : var) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_pat_as (t, v))

(** [trm_pat_any]: represents a non-binded variable in a bbtrm. To be used when creating switch clauses.*)
let trm_pat_any ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) () : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_pat_any)

(** [trm_pat_is]: represents constructor inversion in a bbtrm. To be used when creating switch clauses.*)
let trm_pat_is ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (t : trm) (p : pat) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_pat_is (t, p))

let var_any_bool = new_var "ANY_BOOL"

(** [trm_any_bool]: generates ANY_BOOL () *)
let trm_any_bool : trm =
  trm_apps (trm_var var_any_bool) []



(** [empty_ast]: generates {} *)
let empty_ast : trm =
  trm_set_mainfile (trm_seq_nomarks [])

(** [set_struct_access_inv t]: if [t] is a write on a struct access, then return the base, the field of that access
    and the value that has been assigned to; else None *)
let set_struct_access_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
   begin match trm_struct_access_inv lhs with
   | Some (base, f) -> Some (base, f, rhs)
   | _ -> None
   end
  | _ -> None

(** [set_struct_get_inv t]: if [t] is a write operation on a struct field, then it will return the base, the field and the
     value that has been assigned to.  *)
let set_struct_get_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs], _) when is_set_operation t ->
    begin match trm_struct_get_inv lhs with
    | Some (base, f) -> Some (base, f, rhs)
    | _ -> None
    end
  | _ -> None


(** [insert_at_top_of_seq tl t]: insert the list of trms [tl] at the top of sequence [t]. *)
let insert_at_top_of_seq (tl : trm list) (t : trm) : trm =
  match t.desc with
  | Trm_seq (tl1, result) ->
    let new_tl = Mlist.insert_sublist_at 0 tl tl1 in
    trm_alter ~desc:(Trm_seq (new_tl, result)) t
  | _ -> t


(** [filter_out_from_seq f t]: extracts all the trms that satisfy the predicate [f] from sequence [t].
    The final result is a pair consisting of the final sequence and the filtered out trms.*)
let filter_out_from_seq (f : trm -> bool) (t : trm) : (trm * trms)  =
  match t.desc with
  | Trm_seq (tl, result) ->
    (* FIXME: Preserve marks *)
    let tl_to_remove, tl_to_keep = List.partition f (Mlist.to_list tl) in
    (trm_alter ~desc:(Trm_seq (Mlist.of_list tl_to_keep, result)) t, tl_to_remove)
  | _  -> (t, [])

(** [is_class_constructor t] checks if [t] is a class constructor declaration or definition. *)
let is_class_constructor (t : trm) : bool =
  List.exists (function | Class_constructor _ -> true | _ -> false) (trm_get_cstyles t)

(** [get_typ_arguments t]: returns the list of types used during a template specialization. *)
let get_typ_arguments (t : trm) : typ list =
  let c_annot = trm_get_cstyles t in
  List.fold_left (fun acc c_ann ->
    match c_ann with
    | Typ_arguments tyl -> tyl
    | _ -> acc
  ) [] c_annot

  (** [is_return t]: checks if [t] is a return statement. *)
  let is_return (t : trm) : bool =
    match t.desc with
    | Trm_abort (Ret _) -> true | _ -> false

  (** [is_trm_abort t]: checks if [t] has [Trm_abort abort] description. *)
  let is_trm_abort (t: trm) : bool =
    match t.desc with
    | Trm_abort _ -> true | _ -> false


  let is_trm_unit (t : trm) : bool =
    match trm_lit_inv t with
    | Some Lit_unit -> true
    | _ -> false

  let is_trm_null (t: trm) : bool =
    match trm_lit_inv t with
    | Some (Lit_null _) -> true
    | _ -> false

  let is_trm_int (cst : int) (t : trm) : bool =
    match trm_lit_inv t with
    | Some (Lit_int (_, c)) when c = cst -> true
    | _ -> false

(** [trm_seq_enforce t]: if [t] is not already a sequence, wrap it in one. *)
let trm_seq_enforce (t : trm) : trm =
  if is_trm_seq t then t else trm_seq_nomarks [t]

(* ********************************************************************************************** *)

let trm_combinators_warn_unsupported_case = Tools.resetable_ref true

let trm_combinators_unsupported_case (f_name : string) (t : trm) : trm =
  if !trm_combinators_warn_unsupported_case then begin
    Tools.warn "don't know how to '%s' on '%s'\n\
      <suppressing similar warnings henceforth>"
      f_name (trm_desc_to_string t.desc);
    trm_combinators_warn_unsupported_case := false;
  end;
  t

(** [trm_map]: applies function [f] over ast nodes.
   - [share_if_no_change]: enables sharing trm nodes if they are unchanged by [f].
  *)
let trm_map ?(share_if_no_change = true) ?(keep_ctx = false) (f: trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let typ = t.typ in
  let ctx = if keep_ctx then t.ctx else unknown_ctx () in
  let errors = t.errors in

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
      (fun (name, formula) -> (name, f formula))
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
    let parallel_reads = resource_items_map contract.parallel_reads in
    let iter_contract = fun_contract_map contract.iter_contract in
    if share_if_no_change && loop_ghosts == contract.loop_ghosts && invariant == contract.invariant && parallel_reads == contract.parallel_reads && iter_contract == contract.iter_contract
      then contract
      else { loop_ghosts; invariant; parallel_reads; iter_contract; strict = contract.strict }
  in

  let t' = match t.desc with
  | Trm_var _ -> t
  | Trm_lit (Lit_bool _ | Lit_string _ | Lit_unit) -> t
  | Trm_pat_var _ | Trm_pat_any -> t
  | Trm_pat_as (p, v) ->
    let p' = f p in
    if share_if_no_change && p == p'
      then t
      else trm_pat_as ~annot ?loc ~ctx p' v
  | Trm_pat_is (t1, t2) ->
    let t1' = f t1 in
    let t2' = f t2 in
    if share_if_no_change && t1 == t1' && t2 == t2'
      then t
      else trm_pat_is ~annot ?loc ~ctx t1' t2'

  (*TODO : add a case with the Trm_pat constructors, probably for most of them just return the t. (In particular Trm_pat_var and Trm_pat_any)*)
  | Trm_lit (Lit_int (ty, n)) ->
    let ty' = f ty in
    if share_if_no_change && ty == ty'
      then t
      else trm_int ~annot ?loc ~ctx ~typ:ty' n
  | Trm_lit (Lit_float (ty, x)) ->
    let ty' = f ty in
    if share_if_no_change && ty == ty'
      then t
      else trm_float ~annot ?loc ~ctx ~typ:ty' x
  | Trm_lit (Lit_null ty) ->
    let ty' = f ty in
    if share_if_no_change && ty == ty'
      then t
      else trm_null ~annot ?loc ~ctx ty'
  | Trm_prim (ty, prim) ->
    let ty' = f ty in
    let prim' = match prim with
      | Prim_unop (Unop_cast ty_to) ->
        let ty_to' = f ty_to in
        if ty_to == ty_to' then prim else Prim_unop (Unop_cast ty_to')
      | _ -> prim
    in
    if share_if_no_change && ty == ty' && prim == prim'
      then t
      else trm_prim ~annot ?loc ~ctx ty' prim'
  | Trm_let ((var, ty), init) ->
    let ty' = f ty in
    let init' = f init in
    if (share_if_no_change && ty' == ty && init' == init)
      then t
      else (trm_let ~annot ?loc ~ctx (var, ty') init')
  | Trm_let_mult ts ->
    let ts' = list_map (fun ((x, ty), init) -> ((x, f ty), f init)) (fun ((_, ty1), init1) ((_, ty2), init2) -> ty1 == ty2 && init1 == init2) ts in
    if (share_if_no_change && ts' == ts)
      then t
      else (trm_let_mult ~annot ?loc ~ctx ts')
  | Trm_predecl (var, ty) ->
    let ty' = f ty in
    if (share_if_no_change && ty' == ty)
      then t
      else (trm_predecl ~annot ?loc ~ctx (var, ty'))
  | Trm_fun (args, res, body, contract) ->
    let res' = f res in
    let args' = list_map (fun (x, ty) -> (x, f ty)) (fun (_, ty1) (_, ty2) -> ty1 == ty2) args in
    let body' = f body in
    let contract' = fun_spec_map fun_contract_map (==) contract in
    if (share_if_no_change && res' == res && args' == args && body' == body && contract' == contract)
      then t
      else (trm_fun ~annot ?loc ~contract:contract' ~ctx args' res' body')
  | Trm_if (cond, then_, else_) ->
    let cond' = f cond in
    let then_' = f then_ in
    let else_' = f else_ in
    if (share_if_no_change && cond' == cond && then_' == then_ && else_' == else_)
      then t
      else (trm_if ~annot ?loc ~ctx cond' then_' else_')
  | Trm_seq (tl, result) ->
    let tl' = mlist_map f (==) tl in
    if (share_if_no_change(*redundant*) && tl == tl')
      then t
      else (trm_seq ~annot ?loc ~ctx ?typ ?result tl')
  | Trm_apps (func, args, ghost_args) ->
    let func' = f func in
    let args' = list_map f (==) args in
    let ghost_args' = resource_items_map ghost_args in
    if (share_if_no_change(*redundant*) && func' == func && args' == args && ghost_args' == ghost_args)
      then t
      (* warning: may have different type *)
      else (trm_apps ~annot ?loc ?typ ~ctx ~ghost_args:ghost_args' func' args')
  | Trm_while (cond, body) ->
    let cond' = f cond in
    let body' = f body in
    if (share_if_no_change && cond' == cond && body' == body)
      then t
      else (trm_while ~annot ?loc ~ctx cond' body')
  | Trm_for_c (init, cond, step, body, invariant) ->
      let init' = f init in
      let cond' = f cond in
      let step' = f step in
      let body' = f body in
      let invariant' = opt_map resource_set_map (==) invariant in
      if (share_if_no_change && init' == init && cond' == cond && step' == step && body' == body && invariant' == invariant)
        then t
        else (trm_for_c ~annot ?loc ?invariant:invariant' ~ctx init' cond' step' body')
  | Trm_for (range, body, contract) ->
    let start = f range.start in
    let stop = f range.stop in
    let step = f range.step in
    let range' = if share_if_no_change && range.step == step && range.start == start && range.stop == stop
      then range
      else { range with start; stop; step }
    in
    let body' = f body in
    let contract' = loop_contract_map contract in
    if (share_if_no_change && range' == range && body' == body && contract' == contract)
      then t
      else (trm_for ~annot ?loc ~contract:contract' ~ctx range' body')
  | Trm_switch (cond, cases) ->
      let cond' = f cond in
      let cases' = list_map
        (fun (tl, body) -> (tl, f body))
        (fun (_, body1) (_, body2) -> body1 == body2) cases in
      if (share_if_no_change(*redundant*) && cond' == cond && cases' == cases)
        then t
        else (trm_switch ~annot ?loc ~ctx cond' cases')
  | Trm_my_switch cases ->
      let cases' =
        list_map
        (fun (cond, body) ->
          let cond' = f cond in
          let body' = f body in
          (cond',body'))
        (fun (cond1, body1) (cond2, body2) -> cond1 == cond2 && body1 == body2)
        cases
      in
      if (share_if_no_change && cases == cases')
        then t
        else (trm_my_switch ~annot ?loc ~ctx cases')
      (*TODO: add actual code for this after understanding what the function does, (probably applying f to everything inside?)*)
  | Trm_abort a ->
    begin match a with
    | Ret (Some r) ->
        let r' = f r in
        if (share_if_no_change && r' == r)
          then t
          else (trm_ret ~annot ?loc ~ctx (Some r'))
    | _ -> t
    end
  | Trm_namespace (name, body, inline) ->
    let body' = f body in
    if (share_if_no_change && body == body')
      then t
      else (trm_namespace ~annot ?loc ~ctx name body' inline)
  | Trm_typedef td ->
    let body' = begin match td.typedef_body with
    | Typedef_alias ty ->
      let ty' = f ty in
      if share_if_no_change && ty' == ty
        then td.typedef_body
        else Typedef_alias ty'
    | Typedef_record rfl ->
      let rfl' = list_map
        (fun (rf, rf_ann) ->
          let rf' =
            begin match rf with
            | Record_field (name, ty) ->
                let ty' = f ty in
                if share_if_no_change && ty == ty'
                  then rf
                  else Record_field (name, ty')
            | Record_method rft ->
                let rft' = f rft in
                if share_if_no_change && rft == rft'
                  then rf
                  else Record_method rft'
            end in
          rf', rf_ann
        )
        (fun (rfa, _) (rfb, _) -> rfa == rfb)
        rfl in
      if share_if_no_change && rfl == rfl'
        then td.typedef_body
        else Typedef_record rfl'
    | Typedef_union ucl ->
      let ucl' =
        list_map
        (fun uc ->
          let args_types' = list_map f (==) uc.union_constructor_args_type in
          { uc with union_constructor_args_type = args_types' })
        (fun uc1 uc2 -> uc1.union_constructor_args_type == uc2.union_constructor_args_type)
        ucl
      in
      if share_if_no_change && ucl == ucl'
        then td.typedef_body
        else Typedef_union ucl'
    | _ -> failwith "trm_map: unexpected typedef_body"
    end in
    if (share_if_no_change && body' == td.typedef_body)
      then t
      else trm_typedef ~annot ?loc ~ctx { td with typedef_body = body' }
  | Trm_template (typ_params, templated) ->
    let templated' = f templated in
    if (share_if_no_change && templated == templated')
      then t
      else trm_template ~annot ?loc ~ctx typ_params templated'
  | Trm_arbitrary (Comment _) -> t
  | _ ->
    trm_combinators_unsupported_case "trm_map"  t
  in
  t'.errors <- errors;(* LATER: errors is not treated like other arguments *)

  (* TODO: #typfield: Remove this inefficient code after refactoring *)
  match typ with
  | None -> t'
  | Some typ ->
    let typ' = f typ in
    if typ == typ' then t' else trm_alter ~typ:typ' t'

(** [trm_bottom_up]: applies f on t recursively from bottom to top. *)
let rec trm_bottom_up (f : trm -> trm) (t : trm) : trm =
  let t2 = trm_map (trm_bottom_up f) t in
  f t2

(** [trm_iter f t]: similar to [trm_map] but this one doesn't return a trm at the end. *)
let trm_iter (f : trm -> unit) (t : trm) : unit =
  ignore (trm_map (fun t -> f t; t) t)


(* TODO: this is different from trm_free_vars because bound variables are not treated. delete ? *)
let trm_used_vars (t: trm): Var_set.t =
  let vars = ref Var_set.empty in
  let rec aux t = match trm_var_inv t with
  | Some x -> vars := Var_set.add x !vars
  | _ -> trm_iter aux t
  in
  aux t;
  !vars

type var_metadata = trm_annot * location * typ option * ctx

let empty_var_metadata () =
  trm_annot_default, None, None, unknown_ctx ()

(** Traverse recursively an AST, keeping track of a ctx for resolving names;
    this context evolves when encountering binders or scope enter/exit points;
    returns an extended context for the continuation (e.g. after a let in a sequence)

    - if [keep_ctx] is [false], the field [t.ctx] is discarded (replaced with [unknown_ctx]);
      keeping ctx is used as an optimization so that typing et al need not be done again.
    - [enter_scope ctx t] is a function that builds the extended scope, e.g.
      when entering a loop or function body, or a sequence body, and [t] is the
      term which introduces the scope.
    - [exit_scope old_ctx ctx t] is a function to be called when leaving the scope,
      giving it the outer_ctx (before entering the scope), and the current ctx
      (e.g. what is obtained at the end of the body of the scope), and applied on
      the term [t] that introduces the scope. (TODO: should it be the input [t]
      or the output [t]? See the problem in handling Typedef_record in trm_map_vars_ret_ctx.)

    - [post_process ctx t] is called at the end of the recursion on subterms,
      for computing additional metadata, e.g. we currently use it for gathering
      all the specifications of all the functions being bound in the AST.
    - [enter_beta_redex] is TODO (for computing effective arguments in certain function applications)
    - [map_binder ctx var is_predecl] is called for every name being bound,
      e.g. a let-binding on a variable, or a type definition. The variable is
      the one being bound. [is_predecl] is true for function prototypes that
      are declared but not defined---the only case in which a same name could
      be bound twice without producing a name conflict; there could be zero or
      several predeclaration before the proper declaration.
    - [map_var ctx meta var] is called on every variable occurrence. [ctx] is
      again the current context. [var] is the current description of the variable.
      The [meta] data is of the form [(annot, loc, typ, t_ctx)] and describes
      the information associated with the [trm_var] (if it is another kind of
      term, the [empty_meta_data] is used, e.g. for handling the "res" of a sequence).
      *)
let trm_map_vars_ret_ctx
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  ?(enter_beta_redex: ('ctx -> (var * trm) list -> 'ctx) option)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> (ctx, bind))
  (map_var: 'ctx -> var_metadata -> var -> trm)
  (ctx: 'ctx) (t: trm): 'ctx * trm =
  let rec f_map (ctx:'ctx) (t:trm): 'ctx * trm =
    let annot = t.annot in
    let errors = t.errors in
    let loc = t.loc in
    let typ = t.typ in
    let t_ctx = if keep_ctx then t.ctx else unknown_ctx () in

    (* TODO: #typfield: Remove this inefficient code *)
    let typ' = match typ with
      | None -> typ
      | Some ty ->
        let _, ty' = f_map ctx ty in
        if ty == ty' then typ else Some ty'
    in

    let ctx, trm = match t.desc with
    | Trm_var x ->
      (ctx, map_var ctx (annot, loc, typ, t_ctx) x)

    (*TODO : Add a case for the Trm_pat constructors, this will be needed when I'll have a working Union and Switch*)

    | Trm_let ((var, typ), body) ->
      let _, typ' = f_map ctx typ in
      let _, body' = f_map ctx body in
      let cont_ctx, var' = map_binder ctx var false in
      let t' = if (body == body' && var == var' && typ == typ')
        then t
        else (trm_let ~annot ?loc ~ctx:t_ctx (var', typ') body')
      in
      (cont_ctx, t')

    | Trm_let_mult ts ->
      let cont_ctx = ref ctx in
      let ts' = List.map (fun binding ->
        let (var, typ), t = binding in
        let _, t' = f_map !cont_ctx t in
        let _, typ' = f_map !cont_ctx typ in
        let cont_ctx', var' = map_binder !cont_ctx var false in
        cont_ctx := cont_ctx';
        if var == var' && typ == typ' && t == t' then binding else ((var', typ'), t')
      ) ts in
      let t' = if ((List.for_all2 (==) ts ts'))
        then t
        else (trm_let_mult ~annot ?loc ~ctx:t_ctx ts')
      in
      (!cont_ctx, t')

    | Trm_predecl (var, typ) ->
      let _, typ' = f_map ctx typ in
      let cont_ctx, var' = map_binder ctx var true in
      let t' = if (var == var' && typ == typ')
        then t
        else (trm_predecl ~annot ?loc ~ctx:t_ctx (var', typ'))
      in
      (cont_ctx, t')

    | Trm_for (range, body, contract) ->
      let loop_ctx, index = map_binder (enter_scope ctx t) range.index false in
      let _, start = f_map loop_ctx range.start in
      let _, stop = f_map loop_ctx range.stop in
      let _, step = f_map loop_ctx range.step in
      let range' = if (range.index == index && range.step == step && range.start == start && range.stop == stop)
        then range
        else { range with index; start; stop; step }
      in
      let loop_ctx, contract' = loop_contract_map loop_ctx contract in
      let loop_ctx, body' = f_map loop_ctx body in
      let t' = if (range' == range && body' == body && contract == contract')
        then t
        else (trm_for ~annot ?loc ~ctx:t_ctx ~contract:contract' range' body')
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
      let _, ret' = f_map ctx ret in
      let body_ctx, args' = List.fold_left_map (fun ctx typ_arg ->
        let arg, typ = typ_arg in
        let _, typ' = f_map ctx typ in
        let ctx, arg' = map_binder ctx arg false in
        let typ_arg' = if arg' == arg && typ' == typ then typ_arg else (arg', typ') in
        (ctx, typ_arg')) (enter_scope ctx t) args in
      let body_ctx, contract' = fun_spec_map body_ctx contract in
      let body_ctx, body' = f_map body_ctx body in
      let t' = if (List.for_all2 (==) args args' && ret == ret' && body == body' && contract == contract')
        then t
        else trm_fun ~annot ?loc ~ctx:t_ctx ~contract:contract' args' ret' body' in
      (* TODO: Proper function type here *)
      let ctx = exit_scope ctx body_ctx t' in
      (ctx, t')

    | Trm_seq (instrs, result) ->
      (* LATER: add bool for no scope seq? *)
      let no_scope = trm_is_include t || trm_is_mainfile t in
      let cont_ctx = ref (if no_scope then ctx else enter_scope ctx t) in
      let instrs' = Mlist.map (fun t ->
        let cont_ctx', t' = f_map !cont_ctx t in
        cont_ctx := cont_ctx';
        t'
      ) instrs in
      let result' = match result with
        | None -> result
        | Some resvar ->
          begin match trm_var_inv (map_var !cont_ctx (empty_var_metadata ()) resvar) with
          | Some new_resvar ->
            if resvar == new_resvar then result else Some new_resvar
          | None -> failwith "trm_map_vars: Cannot map variable '%s' to a term that is not a variable because it is used as the result of a sequence." (var_to_string resvar)
          end
      in
      let t' = if result' == result && Mlist.for_all2 (==) instrs instrs'
        then t
        else trm_seq ~annot ?loc ~ctx:t_ctx ?result:result' instrs'
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
      let ctx, typename' = map_binder ctx td.typedef_name false in
      let body', cont_ctx =
        begin match td.typedef_body with

        | Typedef_alias ty ->
          let _, ty' = f_map ctx ty in
          let body' = if ty' == ty then td.typedef_body else Typedef_alias ty' in
          (body', ctx)

        | Typedef_union ucl ->
            let cont_ctx = ref ctx in
            let ucl' =
              List.map (fun uc ->
                let cont_ctx', union_constructor_constructor' = map_binder !cont_ctx uc.union_constructor_constructor false in
                cont_ctx := cont_ctx';
                let cont_ctx', union_constructor_inversor' = map_binder !cont_ctx uc.union_constructor_inversor false in
                cont_ctx := cont_ctx';
                let union_constructor_args_type' = List.map (fun t -> let (_, t') = f_map !cont_ctx t in t') uc.union_constructor_args_type in
                { union_constructor_constructor = union_constructor_constructor';
                  union_constructor_inversor = union_constructor_inversor';
                  union_constructor_args_type = union_constructor_args_type' }
             ) ucl in

          let cmp_ucl uc uc' : bool =
               uc.union_constructor_constructor == uc'.union_constructor_constructor
            && uc.union_constructor_inversor    == uc'.union_constructor_inversor
            && uc.union_constructor_args_type   == uc'.union_constructor_args_type
            in
          let body' = if List.for_all2 cmp_ucl ucl ucl' then td.typedef_body else Typedef_union ucl' in
          (body', !cont_ctx)

        | Typedef_record rfl ->
          if !Ast.behavior_ocaml then begin
          (* Ocaml record behavior *)
            let cont_ctx = ref ctx in
            let rfl' = List.map (fun (rf, rf_ann) ->
              assert (rf_ann = Access_unspecified); (* no scope modifier on ocaml style records *)
              let rf' =
                begin match rf with
                | Record_field (field, ty) ->
                  (* Note: could call map_binder on field if we wanted to view field names as projection functions; if so, cont_ctx would be updated for each field. *)
                  let _, ty' = f_map !cont_ctx ty in
                  if ty == ty' then rf else Record_field (field, ty')
                | Record_method rft -> assert false (* should not have method in ocaml style records *)
                end in
              (rf', rf_ann)
            ) rfl in
             let body' = if List.for_all2 (==) rfl rfl' then td.typedef_body else Typedef_record rfl' in
             (body', !cont_ctx)
          end else begin
          (* C++ class behavior *)
              let class_ctx = ref (enter_scope ctx t) in (* introduce a namespace for the class *)
              let rfl' = List.map (fun (rf, rf_ann) ->
              let rf' = begin match rf with
              | Record_method rft ->
                let class_ctx', rft' = f_map !class_ctx rft in
                class_ctx := class_ctx';
                if rft == rft' then rf else Record_method rft'
              | Record_field (field, ty) ->
                let _, ty' = f_map !class_ctx ty in
                if ty == ty' then rf else Record_field (field, ty')
              end in
              rf', rf_ann
            ) rfl in
            (* WARNING: exit_scope is called on a t' that is not phyiscally the same as the t' returned later *)
            let body' = if List.for_all2 (==) rfl rfl' then td.typedef_body else Typedef_record rfl' in
            let t'_copy = trm_typedef ~annot ?loc ~ctx:t_ctx { typedef_name = typename'; typedef_body = body' } in
            let cont_ctx = exit_scope ctx !class_ctx t'_copy in
            (body', cont_ctx)
          end
        | _ -> failwith "unexpected typedef_body"
        end in

      let td' =
        if (body' == td.typedef_body && typename' == td.typedef_name)
          then td
          else { typedef_name = typename'; typedef_body = body' }
        in
      let t' =
        if (td == td')
          then t
          else (trm_typedef ~annot ?loc ~ctx:t_ctx td')
      in
      (cont_ctx, t')

    | Trm_namespace (name, body, inline) ->
      let body_ctx = ref (enter_scope ctx t) in
      let body' = begin match body.desc with
      | Trm_seq (instrs, None) ->
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

    | Trm_template (params, body) ->
      (* HACK: There should be some scope manipulation around templates parameters to give them local names that scope over the template body. However, it is not easy: the body can introduce binders that should be kept. For now, we simply declare template arguments as toplevel binders *)
      let body_ctx, params' = List.fold_left_map (fun ctx param ->
        let param_name, param_kind = param in
        let param_kind' = match param_kind with
          | Typename None -> param_kind
          | Typename (Some default) ->
            let _, default' = f_map ctx default in
            if default == default' then param_kind else Typename (Some default')
          | NonType (param_ty, default_opt) ->
            let _, param_ty' = f_map ctx param_ty in
            let default_opt' = match default_opt with
              | None -> default_opt
              | Some default ->
                let _, default' = f_map ctx default in
                if default == default' then default_opt else (Some default')
            in
            if param_ty == param_ty' && default_opt == default_opt' then param_kind else NonType (param_ty', default_opt')
        in
        let ctx, param_name' = map_binder ctx param_name true in
        let param' = if param_name == param_name' && param_kind == param_kind' then param else (param_name', param_kind') in
        (ctx, param')) ctx params
      in
      let body_ctx, body' = f_map body_ctx body in
      let t' = if body == body' && List.equal (==) params params' then t else (trm_template ~annot ?loc ~ctx:t_ctx params' body') in
      (body_ctx, t')

    | _ ->
      (ctx, trm_map ~keep_ctx (fun ti -> snd (f_map ctx ti)) t) (*TODO : add cases for the trm_pat *)

    in
    trm.errors <- errors;
    (* TODO: #typfield: Remove next line *)
    let trm = if typ == typ' then trm else trm_alter ?typ:typ' trm in
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
    let ctx, parallel_reads = resource_items_map ctx contract.parallel_reads in
    let ctx, iter_contract = fun_contract_map ctx contract.iter_contract in
    let contract =
      if (loop_ghosts == contract.loop_ghosts && invariant == contract.invariant && parallel_reads == contract.parallel_reads && iter_contract == contract.iter_contract)
      then contract
      else { loop_ghosts; invariant; parallel_reads; iter_contract; strict = contract.strict }
    in
    (ctx, contract)

  in
  f_map ctx t

let trm_map_vars
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  ?(enter_beta_redex: ('ctx -> (var * trm) list -> 'ctx) option)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> (ctx, bind))
  (map_var: 'ctx -> var_metadata -> var -> trm)
  (ctx: 'ctx) (t: trm): trm =
  snd (trm_map_vars_ret_ctx ~keep_ctx ~enter_scope ~exit_scope ~post_process ?enter_beta_redex ~map_binder map_var ctx t)

let trm_rename_vars_ret_ctx
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  (map_var: 'ctx -> var -> var)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> ctx, map_var ctx bind)
  ?(map_ghost_arg_name: 'ctx -> trm -> var -> var = fun ctx _ g -> map_var ctx g)
  (ctx: 'ctx) (t: trm): 'ctx * trm =
  trm_map_vars_ret_ctx ~keep_ctx ~enter_scope ~exit_scope ~post_process:(fun ctx t ->
    match t.desc with
    | Trm_apps (fn, args, ghost_args) when ghost_args <> [] ->
      let map_ghost_arg_name = map_ghost_arg_name ctx fn in
      let ghost_args = List.map (fun (g, gt) -> (map_ghost_arg_name g, gt)) ghost_args in
      post_process ctx (trm_alter ~desc:(Trm_apps (fn, args, ghost_args)) t)
    | Trm_fun (args, rettyp, body, FunSpecReverts other_fn) ->
      let other_fn' = map_var ctx other_fn in
      post_process ctx (trm_alter ~desc:(Trm_fun (args, rettyp, body, FunSpecReverts other_fn')) t)
    | _ -> post_process ctx t
    ) ~map_binder (fun ctx (annot, loc, typ, vctx) var ->
      let var = map_var ctx var in
      trm_var ~annot ?loc ?typ ~ctx:vctx var
    ) ctx t

let trm_rename_vars
  ?(keep_ctx = false)
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx * trm = fun ctx t -> (ctx, t))
  (map_var: 'ctx -> var -> var)
  ?(map_binder: 'ctx -> var -> bool -> 'ctx * var = fun ctx bind is_predecl -> (ctx, map_var ctx bind))
  ?(map_ghost_arg_name: 'ctx -> trm -> var -> var = fun ctx _ g -> map_var ctx g)
  (ctx: 'ctx) (t: trm): trm =
  snd (trm_rename_vars_ret_ctx ~keep_ctx ~enter_scope ~exit_scope ~post_process map_var ~map_binder ~map_ghost_arg_name ctx t)

let trm_iter_vars
  ?(enter_scope: 'ctx -> trm -> 'ctx = fun ctx t -> ctx)
  ?(exit_scope: 'ctx -> 'ctx -> trm -> 'ctx = fun outer_ctx inner_ctx t -> outer_ctx)
  ?(post_process: 'ctx -> trm -> 'ctx = fun ctx _ -> ctx)
  (iter_var: 'ctx -> var -> unit)
  ?(iter_binder: 'ctx -> var -> bool -> 'ctx = fun ctx bind is_predecl -> iter_var ctx bind; ctx)
  ?(iter_ghost_arg_name: 'ctx -> trm -> var -> unit = fun ctx _ g -> iter_var ctx g)
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
    | Some x -> trm_var ~annot:t.annot ?loc:t.loc ?typ:t.typ ~ctx:t.ctx (name_to_var ~namespaces:x.namespaces x.name)
    | _ -> trm_map aux t
  in
  aux t


(** [prepare_for_serialize ?remove_ctx t] should be called before serializing an ast.
   This function is a no-op if [remove_ctx=false]. *)
let prepare_for_serialize ?(remove_ctx : bool = false) (t:trm) : trm =
  if not remove_ctx then t else
  begin
    let rec aux t =
      let t = if remove_ctx then { t with ctx = unknown_ctx() } else t in
      trm_map aux t
      in
    aux t
  end


(** Uses a fresh variable identifier for every variable declation, useful for e.g. copying a term while keeping unique ids. *)
let trm_copy (t : trm) : trm =
  let map_binder var_map v _ =
    if has_unset_id v then begin
      Tools.warn "A binder should always be introduced with a fresh id, not with an inferred id.";
      (var_map, v)
    end else begin
      if (Var_map.mem v var_map) then failwith "trm_copy found a second binder for %s" (var_to_string v);
      let new_v = new_var ~namespaces:v.namespaces v.name in
      (Var_map.add v new_v var_map, new_v)
    end
  in
  let map_var var_map v =
    if has_unset_id v then v
    else match Var_map.find_opt v var_map with
    | Some nv -> nv
    | None -> v
  in
  trm_rename_vars ~map_binder map_var Var_map.empty t

(* Assumes var-id's are unique, can locally break scope rules and might require a binder renaming. *)
let trm_subst
  ?(on_subst : trm -> trm -> trm = fun old_t new_t -> trm_copy new_t)
  (subst_map: trm varmap) (t: trm) =
  let subst_var (subst_map: trm varmap) ((annot, loc, typ, _ctx): var_metadata) (var: var) =
    let var_t = trm_var ~annot ?loc ?typ var in
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
   It is generic over the type of information stored when the value is unknown. *)
type 'a evar_resolution =
  | Resolved of trm
  | Unknown of 'a

(* The unification context is a map from variables to evals.
   If a variable is not in the map, then it is not an evar, and should not be substituted/unified.
   If a variable is in the map, then it is an evar, and should be substituted/unified (i.e. it should eventually become Resolved). *)
type 'a unification_ctx = 'a evar_resolution varmap

(** [unfold_if_resolved_evar t evar_ctx] tries to unfold resolved evars inside [evar_ctx] occuring
    at the top of [t] (but not in depth).
    Since we can do it almost for free, it also performs simplifications in the [evar_ctx]
    when resolved evars are pointing to other resolved evars.

    It returns the possibly unfolded [trm], along with a possibly simplified [evar_ctx].

    On application nodes, this function tries to unfold the function and performs immediate
    beta reduction if possible. *)
let rec unfold_if_resolved_evar (t: trm) (evar_ctx: 'a unification_ctx): trm * 'a unification_ctx =
  match t.desc with
  | Trm_var x ->
    begin match Var_map.find_opt x evar_ctx with
    | Some (Resolved t) ->
      (* Avoid cycles in this function, this can help debugging *)
      let evar_ctx = Var_map.remove x evar_ctx in
      let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
      (* Path compression in case of cascading evars *)
      t, Var_map.add x (Resolved t) evar_ctx
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

(** [unify_trm t1 t2 evar_ctx validate_inst] tries to unify [t1] with [t2],
    possibly instantiating and substituting evars that occur in [evar_ctx].
    If the unification succeeds, returns an updated unification context, with the newly resolved evars.
    If it fails, returns None.
    For each potential unification, this function calls [validate_inst t info evar_ctx] that can itself
    perform chained unifications.
    *)
let rec unify_trm ?(on_failure = fun a b -> ()) (t_left: trm) (t_right: trm) (evar_ctx: 'a unification_ctx) (validate_inst: trm -> 'a -> 'a unification_ctx -> 'a unification_ctx) : 'a unification_ctx option =
  let open Option.Monad in
  (* Pattern match on one component to get a warning if there is a missing one *)
  let check cond = if cond then Some evar_ctx else None in
  (* Unfold first to avoid problems on f(?x, ?y) = f(?y, ?y) *)
  let t_left, evar_ctx = unfold_if_resolved_evar t_left evar_ctx in
  let t_right, evar_ctx = unfold_if_resolved_evar t_right evar_ctx in
  let validate_and_subst evar t_subst =
    match Var_map.find evar evar_ctx with
    | Unknown info ->
      let evar_ctx = validate_inst t_subst info evar_ctx in
      Some (Var_map.add evar (Resolved t_subst) evar_ctx)
    | Resolved _ -> failwith "Resolved evars should have been substituted before"
  in
  let res = match trm_var_inv t_left, trm_var_inv t_right with
  | Some x_left, Some x_right when var_eq x_left x_right ->
    Some evar_ctx
  | Some x_left, _ when Var_map.mem x_left evar_ctx ->
    validate_and_subst x_left t_right
  | _, Some x_right when Var_map.mem x_right evar_ctx ->
    validate_and_subst x_right t_left
  | _ ->
    match t_right.desc with
    | Trm_var _ ->
      (* t_right is a variable but it is not the same as t_left, and none is an unresolved evar *)
      None

    | Trm_lit le ->
      let* l = trm_lit_inv t_left in
      begin match le with
      | Lit_unit | Lit_bool _ | Lit_string _ -> check (l = le)
      | Lit_int (te, ve) ->
        let* t, v = match l with | Lit_int (t, v) -> Some (t, v) | _ -> None in
        let* _ = check (v = ve) in
        unify_trm ~on_failure t te evar_ctx validate_inst
      | Lit_float (te, ve) ->
        let* t, v = match l with | Lit_float (t, v) -> Some (t, v) | _ -> None in
        let* _ = check (v = ve) in
        unify_trm ~on_failure t te evar_ctx validate_inst
      | Lit_null te ->
        let* t = match l with | Lit_null t -> Some t | _ -> None in
        unify_trm ~on_failure t te evar_ctx validate_inst
      end

    | Trm_prim (tye, pe) ->
      let* ty, p = trm_prim_inv t_left in
      (* FIXME: This can fail because primitives may recursively contain types and terms *)
      if pe = p then unify_trm ~on_failure ty tye evar_ctx validate_inst else None

    | Trm_apps (fe, argse, ghost_args) ->
      let* f, args = trm_apps_inv t_left in
      let* evar_ctx = unify_trm ~on_failure f fe evar_ctx validate_inst in
      begin try
        List.fold_left2 (fun evar_ctx arg arge -> let* evar_ctx in unify_trm ~on_failure arg arge evar_ctx validate_inst) (Some evar_ctx) args argse
      with Invalid_argument _ -> None end

    | Trm_fun (argse, _, bodye, _) ->
      let* args, _, body, _ = trm_fun_inv t_left in
      (* Remember the masked context in case of shadowing, this is needed in case of recursive
         or higher order functions with evars. *)
      let* evar_ctx, masked_ctx =
        try
          Some (List.fold_left2 (fun (evar_ctx, masked) (arge, _) (arg, argty) ->
              if var_eq arge arg then
                (* This case is required to handle comparison of a trm with itself *)
                (evar_ctx, masked)
              else
                let masked_entry = Var_map.find_opt arge evar_ctx in
                let evar_ctx = Var_map.add arge (Resolved (trm_var ~typ:argty arg)) evar_ctx in
                (evar_ctx, (arge, masked_entry) :: masked)
            ) (evar_ctx, []) argse args)
        with Invalid_argument _ -> None
      in
      let* evar_ctx = unify_trm ~on_failure body bodye evar_ctx validate_inst in
      Some (List.fold_left (fun evar_ctx (arge, masked_entry) ->
          match masked_entry with
          | Some entry -> Var_map.add arge entry evar_ctx
          | None -> Var_map.remove arge evar_ctx
        ) evar_ctx masked_ctx)

    | Trm_arbitrary _ -> failwith "unify_trm: found Trm_arbitrary during unification (a reparse is missing)"
    | _ -> failwith "unify_trm: unhandled constructor (%s)" (Ast_to_text.ast_to_string t_right) (* TODO: Implement the rest of constructors *)
    in
    if Option.is_none res then on_failure t_left t_right;
    res

(** [are_same_trm t1 t2] checks that [t1] and [t2] are alpha-equivalent (same modulo name of the binders). *)
let are_same_trm (t1: trm) (t2: trm): bool =
  (* they are the same if they can be unified without allowing substitutions. *)
  Option.is_some (unify_trm t1 t2 Var_map.empty (fun _ _ ctx -> ctx))

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

let trm_val_unop (unop: unary_op)  ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typ: typ) (t : trm) : trm =
  trm_apps ?loc ?ctx ~annot ~typ (trm_unop typ unop) [t]

(** [trm_cast ty t]: type cast *)
let trm_cast ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ty_from: typ option) (ty_to : typ) (t : trm) : trm =
  let ty_from = Option.or_ ty_from t.typ in
  trm_apps ?loc ~typ:ty_to ~annot (trm_unop (typ_or_auto ty_from) (Unop_cast ty_to)) [t]

(** [trm_minus ?loc ?ctx ?typ t]: generates -t *)
let trm_minus ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(typ : typ = typ_int) (t: trm) =
  trm_val_unop Unop_minus ~annot ?loc ?ctx typ t

(** [trm_plus ?loc ?ctx ?typ t]: generates +t *)
let trm_plus ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(typ : typ = typ_int) (t: trm) =
  trm_val_unop Unop_plus ~annot ?loc ?ctx typ t

  (** [trm_bitwise_neg ?loc ?ctx ?typ t]: generates ~t *)
let trm_bitwise_neg ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(typ : typ = typ_int) (t: trm) =
  trm_val_unop Unop_bitwise_neg ~annot ?loc ?ctx typ t

  (** [trm_neg ?loc ?ctx t]: generates !t *)
let trm_neg ?(annot : trm_annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (t: trm) =
  trm_val_unop Unop_neg ~annot ?loc ?ctx typ_bool t

let trm_cmp_binop (binop: binary_op) ?(loc) ?(ctx : ctx option) ?(typ: typ = typ_int) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ?ctx ~typ:typ_bool (trm_binop typ binop) [t1; t2]

(** [trm_eq ?loc ?ctx ?typ t1 t2]: generates t1 == t2 *)
let trm_eq = trm_cmp_binop Binop_eq

(** [trm_eq_inv t1 t2]: deconstructs t = t1 == t2 *)
let trm_eq_inv (t : trm) : (trm * trm) option  =
  trm_binop_inv Binop_eq t

(** [trm_neq t1 t2]: generates t1 != t2 *)
let trm_neq = trm_cmp_binop Binop_neq

(** [trm_le t1 t2]: generates t1 <= t2 *)
let trm_le = trm_cmp_binop Binop_le

(** [trm_lt t1 t2]: generates t1 < t2 *)
let trm_lt = trm_cmp_binop Binop_lt

(** [trm_ge t1 t2]: generates t1 >= t2 *)
let trm_ge = trm_cmp_binop Binop_ge

(** [trm_gt t1 t2]: generates t1 > t2 *)
let trm_gt = trm_cmp_binop Binop_gt

(** [trm_ineq ineq_sgn t1 t2]: generates an inequality t1 # t2 where # is one of the following operators <, <=, >, >=.
    The operator is provided implicitly through the [ineq_sng] arg *)
let trm_ineq (ineq_sgn : loop_dir) (t1 : trm) (t2 : trm) : trm =
  match ineq_sgn with
  | DirUp -> trm_lt t1 t2
  | DirUpEq -> trm_le t1 t2
  | DirDown ->  trm_gt t1 t2
  | DirDownEq -> trm_ge t1 t2

let trm_arith_binop (binop: binary_op) ?(loc) ?(ctx : ctx option) ?(typ: typ = typ_int) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ?ctx ~typ (trm_binop typ binop) [t1; t2]

(** [trm_sub t1 t2]: generates t1 - t2 *)
let trm_sub = trm_arith_binop Binop_sub

(** [trm_add t1 t2]: generates t1 + t2 *)
let trm_add = trm_arith_binop Binop_add

(** [trm_add_inv t1 t2]: deconstructs t = t1 + t2 *)
let trm_add_inv (t : trm) : (trm * trm) option  =
  trm_binop_inv Binop_add t

(** [trm_mul t1 t2]: generates t1 * t2 *)
let trm_mul = trm_arith_binop Binop_mul

(** [trm_exact_div t1 t2]: generates exact_div(t1, t2) *)
let trm_exact_div = trm_arith_binop Binop_exact_div

(** [trm_trunc_div t1 t2]: generates t1 / t2 *)
let trm_trunc_div = trm_arith_binop Binop_trunc_div

(** [trm_trunc_mod t1 t2]: generates t1 % t2 *)
let trm_trunc_mod = trm_arith_binop Binop_trunc_mod

(** [trm_bit_and t1 t2]: generates t1 & t2 *)
let trm_bit_and = trm_arith_binop Binop_bitwise_and

(** [trm_bit_or t1 t2]: generates t1 | t2 *)
let trm_bit_or = trm_arith_binop Binop_bitwise_or

(** [trm_shiftl t1 t2]: generates t1 << t2*)
let trm_shiftl = trm_arith_binop Binop_shiftl

(** [trm_shiftr t1 t2]: generates t1 >> t2*)
let trm_shiftr = trm_arith_binop Binop_shiftr

(** [trm_xor t1 t2]: generates t1 ^ t2 *)
let trm_xor = trm_arith_binop Binop_xor

(** [trm_compound_assign ~annot ?ctx ?loc ?typ binop t1 t2]: generates a compound operation, ex t1+=t2*)
let trm_compound_assign ?(annot : trm_annot = trm_annot_default) ?(ctx : ctx option) ?(loc) ?(typ = typ_auto)
  (binop : binary_op) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ~annot ?ctx ~typ:typ_unit (trm_prim typ (Prim_compound_assign_op binop)) [t1; t2]

(** [trm_ref ty t]: generates "ref ty(t)" *)
let trm_ref ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (ty : typ) (t : trm) : trm =
  trm_apps ?loc ~annot ?ctx ~typ:(typ_of_alloc ty) (trm_prim ty Prim_ref) [t]

let trm_ref_uninit ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (ty : typ) : trm =
  trm_apps ?loc ~annot ?ctx ~typ:(typ_of_alloc ty) (trm_prim ty Prim_ref_uninit) []

(** [trm_set ~annot ?loc ?ctx t1 t2] *)
let trm_set ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (lhs : trm) (rhs : trm) : trm =
  trm_apps ~annot:annot ?loc ?ctx ~typ:typ_unit (trm_binop typ_auto Binop_set) [lhs; rhs]

(** [trm_new ~annot ?loc ?ctx ty t]: heap allocation operator *)
let trm_new ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (ty : typ) (t : trm): trm =
  trm_apps ?loc ~annot ?ctx ~typ:(typ_of_alloc ty) (trm_prim ty Prim_new) [t]

(** [trm_new_uninit ~annot ?loc ?ctx ty t]: heap allocation operator *)
let trm_new_uninit ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (ty : typ) : trm =
  trm_apps ?loc ~annot ?ctx ~typ:(typ_of_alloc ty) (trm_prim ty Prim_new_uninit) []

(** [trm_delete ~annot ?loc ?ctx is_array_form t]: heap deallocation operator *)
let trm_delete ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (t : trm) =
  trm_apps ?loc ~annot ?ctx ~typ:typ_unit (trm_prim typ_auto Prim_delete) [t]

(** [trm_let_mut ~annot ?ctx typed_var init]: an extension of trm_let for
    creating mutable variable declarations *)
let trm_let_mut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_of_alloc var_type in
  trm_let ~annot ?loc ?ctx (var_name, var_type_ptr) (trm_ref var_type init)

let trm_let_mut_uninit ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (typed_var : typed_var) =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_of_alloc var_type in
  trm_let ~annot ?loc ?ctx (var_name, var_type_ptr) (trm_ref_uninit var_type)

(*****************************************************************************)
(** [trm_and t1 t2]: generates t1 && t2, which is encoded as [if t1 then t2 else false] with an annotation *)
let trm_and ?(loc) ?(ctx : ctx option) (t1 : trm) (t2 : trm) : trm =
  trm_add_cstyle Shortcircuit_and (trm_if ?loc ?ctx ~typ:typ_bool t1 t2 (trm_bool false))

(** [trm_or t1 t2]: generates t1 || t2 *)
let trm_or ?(loc) ?(ctx : ctx option) (t1 : trm) (t2 : trm) : trm =
  trm_add_cstyle Shortcircuit_or (trm_if ?loc ?ctx ~typ:typ_bool t1 (trm_bool true) t2)

(** [trm_ands ts]: generalized version of trm_and *)
let trm_ands (ts : trm list) : trm =
  List.fold_lefti (fun i acc t1 ->
    if i = 0 then t1 else trm_and acc t1
  ) (trm_bool true) ts

(*****************************************************************************)
(** Pattern operations *)

(** [trm_pat_and]: alias of [trm_and], represents binding [&&] clauses in a bbtrm. To be used when creating switch clauses.*)
let trm_pat_and = trm_and

(** [trm_pat_or]: alias of [trm_or], represents non-binding [||] clauses in a bbtrm. To be used when creating switch clauses.*)
let trm_pat_or = trm_or

(**[trm_pat_neg]: returns [not] clause of a bbtrm. Is equivalent to [trm_apps (trm_prim Unop_neg) [t1]].*)
let trm_pat_neg ?(loc) ?(ctx : ctx option) (t : trm) : trm =
  trm_add_cstyle Shortcircuit_neg (trm_if ?loc ?ctx ~typ:typ_bool t (trm_bool false) (trm_bool true))


(*****************************************************************************)

exception Unknown_key

(** [tmap_to_list keys map]: gets the list of values for all keys [keys] in [map] *)
let tmap_to_list (keys : vars) (map : tmap) : trms =
  List.map (fun x -> match Var_map.find_opt x map with
    | Some v -> v
    | None -> raise Unknown_key
  ) keys

(** [tmap_filter keys tmap]: removes all the bindings with [keys] in [map] and return [map] *)
let tmap_filter (keys : vars) (map : tmap) : tmap =
  Var_map.filter (fun k _ -> not (List.mem k keys)) map

(** [hide_function_bodies f_pred tl]: all the toplevel function with their names satisfying [f_pred] will have hidden bodies.
    Others will be kept unchanged. The new ast is called the chopped_ast. This function wlll return the choped_ast and
    a map with keys the names of the functions whose body has been removed and values their removed body. *)
let hide_function_bodies (f_pred : var -> bool) (t : trm) : trm * tmap =
  let t_map = ref Var_map.empty in
    let rec aux (t : trm) : trm =
      match trm_let_fun_inv t with
      | Some (f, ty, tv, _, _) ->
        if f_pred f then begin
          t_map := Var_map.add f t !t_map;
          (* replace the body with an empty body with an annotation *)
          let t2 = trm_let_fun ~annot:t.annot ~ctx:t.ctx f ty tv (trm_unit ()) in
          trm_add_cstyle BodyHiddenForLightDiff t2
        end else
          t
      | None -> trm_map aux t
      in
  let res = aux t in
  res, !t_map

(** [update_chopped_ast chopped_ast chopped_fun_map]: for all the functions whose bodies were removed during the creation
    of the chopped_ast restore their bodies by using [chopped_fun_map], which is map with keys beingthe the names
    of the functions that were chopped and values being their actual declaration *)
let update_chopped_ast (chopped_ast : trm) (chopped_fun_map : tmap): trm =
  match chopped_ast.desc with
  | Trm_seq (tl, None) ->
      let new_tl =
      Mlist.map (fun def -> match def.desc with
      | Trm_let ((x, _), _) ->
        begin match Var_map.find_opt x chopped_fun_map with
        | Some tdef -> tdef
        | _ -> def
        end
      |_ ->  def
    ) tl in trm_seq ~annot:chopped_ast.annot new_tl
  | _ -> trm_fail chopped_ast "Ast.update_ast_with_chopped_ast: ast of the main file should start with a top level sequence"




(** [label_subterms_with_fresh_stringreprids f t]: annotates all the subterms of [t]
   that satisfy the boolean predicate [f] with a fresh string representation identifier.
   This operation should be performed to enable the term to doc function to memoize
   its results, and possibly export a table mapping subterms to their string representation. *)
let rec label_subterms_with_fresh_stringreprids (f : trm -> bool) (t : trm) : trm =
  let t2 =
    if not (f t) then t else begin
      let id = next_stringreprid () in
      trm_set_stringreprid id t
    end in
  trm_map ~keep_ctx:true (label_subterms_with_fresh_stringreprids f) t2



(*****************************************************************************)

let trm_def_or_used_vars (t : trm) : Var_set.t =
  (* FIXME: duplicate code with find_var_filter_on *)
  let vars = ref Var_set.empty in
  let rec aux t =
    match trm_var_inv t with
    | Some x -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_let_inv t with
    | Some (x, _, _) -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_let_fun_inv t with
    | Some (x, _, _, _, _) -> vars := Var_set.add x !vars
    | _ ->
    begin match trm_for_inv t with
    | Some (range, _, _) -> vars := Var_set.add range.index !vars
    | _ -> ()
    end end end;
    trm_iter aux t
  in
  aux t;
  !vars


(** [trm_simplify_addressof_and_get t]: simplifies [&*t] and [*&t] to [t] *)
let trm_simplify_addressof_and_get (t : trm) : trm =
  match Option.bind (trm_get_inv t) (trm_unop_inv Unop_address)  with
  | Some t' -> t'
  | None ->
    match Option.bind (trm_unop_inv Unop_address t) trm_get_inv with
    | Some t' -> t'
    | None -> t
