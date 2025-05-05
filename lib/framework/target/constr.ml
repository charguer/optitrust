open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Str
open Tools
open Path

(** [Resolve_target_failure]: exception raised when a target couldn't be resolved *)
exception Resolve_target_failure of string

(* TODO deprecate this after Target.iter is used everywhere *)
let old_resolution = ref false

(******************************************************************************)
(*                        Data structure for targets                          *)
(******************************************************************************)

(** [trm_kind]: type to classify trms into four main classes
    1) Structuring statements
    2) Instructions
    3) Expression
    4) others  *)
type trm_kind =
  | TrmKind_Typedef (* type definition that appears in the AST *)
  | TrmKind_Instr
  | TrmKind_Expr
  | TrmKind_Any
[@@deriving show { with_path = false }]


(** [rexp]: data structure for storing regular expressions *)
type rexp = {
  rexp_desc : string; (* printable version of regexp *)
  rexp_exp : regexp;
  rexp_substr : bool;
  rexp_trm_kind : trm_kind }
let pp_rexp fmt rexp = Format.pp_print_string fmt rexp.rexp_desc

(** [target_relative]: target kind *)
type target_relative =
    | TargetAt
    | TargetFirst
    | TargetLast
    | TargetBetweenAll
    | TargetBefore
    | TargetAfter
[@@deriving show { with_path = false }]

(** [target_occurrences]: the number of targets to match *)
type target_occurrences =
    | ExpectNb of int  (* exactly n occurrences *)
    | ExpectMulti  (* > 0 number of occurrences *)
    | ExpectAnyNb  (* any number of occurrences *)
    | SelectOcc of int option * int list (* filter the occurences to keep only those in the list, negative index are taken from the back, if the first argument is given, check that the number of occurences is exactly that *)
[@@deriving show { with_path = false }]

(* A [target] is a list of constraints to identify nodes of the AST
   that we require the result path to go through. *)

(** [target]: is a list of constraints to identify nodes of the AST that we require the result path to go through. *)
type target = constr list

(** [targets]: a list of targets *)
and targets = target list

(** [depth]: targets by default resolve at any depth, but they can also be resolved only on a specific depth *)
and depth =
  | DepthAny
  | DepthAt of int

[@@deriving show { with_path = false }]

(** [constr]: are the unit constrainst that are checked when target is being resolved, a constraint can be one of the following ones
     - direction constraints
     - include directives constraints
     - depth constraints
     - regexp constraints
     - trm constraints
     - type constraints
     - logic constraints
     - occurrence constraints
     - relative constraints
     - OpenMP pragam constraints *)

(* type constr_qname = constr_name * constr_qpath


   and constr_qpath = var list -> bool


   f and M :: f

   let check_qname (qx : qvar ) (cq : constr_qname) : bool =
    ((snd cq) qx.qvar_path ) && (check_name (fst cq) (qx.qvar_var)) ||
    (check_name (fst cq) (qx.qvar_str )


*)

and constr =
  | Constr_incontracts
  | Constr_depth of depth
  | Constr_dir of dir
  (* already resolved paths: *)
  | Constr_paths of paths
  | Constr_include of string
  | Constr_regexp of rexp
  (* for: init, cond, step, body *)
  | Constr_for_c of target * target * target * target
  (* for index, start, stop, step, body *)
  | Constr_for of constr_name * target * loop_dir option * target * target * target
  (* while: cond, body *)
  | Constr_while of target * target
  (* do while: body, cond *)
  | Constr_do_while of target * target
  (* if: cond, then, else *)
  | Constr_if of target * target * target
  (* decl_var: name, body *)
  | Constr_decl_var of typ_constraint * constr_name * target
  (* decl_vars *)
  | Constr_decl_vars of typ_constraint * constr_name * target
  (* decl_type: name *)
  | Constr_decl_type of constr_name
  (* decl_enum: name, constants *)
  | Constr_decl_enum of constr_name * constr_enum_const
  (* seq *)
  | Constr_seq of target_list_pred
  (* var *)
  | Constr_var of constr_name
  (* lit *)
  | Constr_lit of (lit -> bool)
  (* decl_fun: args, rettyp, body *)
  | Constr_fun of target_list_pred * typ_constraint * target
  (* app: function, arguments *)
  | Constr_app of target * target_list_pred * bool
  (* label *)
  | Constr_label of constr_name * target
  (* goto *)
  | Constr_goto of constr_name
  (* return *)
  | Constr_return of target
  (* abort *)
  | Constr_abort of abort_kind
  (* accesses: base, accesses *)
  | Constr_access of target * constr_accesses * bool
  (* switch: cond, cases *)
  | Constr_switch of target * constr_cases
  (* Target relative to another trm *)
  | Constr_relative of target_relative
  (* Target matching a range of contiguous instructions inside a sequence *)
  | Constr_span of target * target
  (* Number of  occurrences expected  *)
  | Constr_occurrences of target_occurrences
  (* List of constraints *)
  | Constr_target of constr list
  (* Constraint used for argument match *)
  | Constr_bool of bool
  (* Constraint that matches only the root of the AST *)
  | Constr_root
  (* Constraint that matches primitive operations *)
  | Constr_prim of (typ -> bool) * (prim -> bool)
  (* Constraint that matches ast nodes whose marks satisfy the predicate *)
  | Constr_mark of (mark -> bool) * string
  (* Constraint that matches a union of targets *)
  | Constr_or of target list
  (* Constraint that matches an intersection of targets *)
  | Constr_and of target list
  (* Constrain that match the diff between two targets *)
  | Constr_diff of target list * target list
  (* Constraint to match arguments  that sastify both the name predicated and the type predicate *)
  | Constr_arg of var_constraint * typ_constraint
  (* Constraint to match ast nodes of types that satisfy the type predicate *)
  | Constr_hastype of typ_constraint
  (* Constraint to match variable initialization value *)
  | Constr_var_init
  (* Constraint to match an array initialization list *)
  | Constr_omp of (directive->bool) * string
  (* Constraint to match a namespace *)
  | Constr_namespace of constr_name
  (* Constraint to match a term when a predicate is true *)
  | Constr_pred of (trm -> bool)

(* LATER: optimize constr_of_path; should be recognized by resolution,
   and processed more efficiently; checking that the start of the path
   is the root, then reaching directly the position, assuming the path
   is valid (option: raise an exception if the path is invalid). *)

(** [typ_constraint]: predicate on types *)
and typ_constraint = typ -> bool

(** [arg_constraint]: constraint on an argument, represented as a target with a single item
    of the form [cArg ..] or [cTrue]  *)
and arg_constraint = target

(** [var_constraint]: constraint over variable names *)
and var_constraint = string -> bool

(** [constr_name]: names involved in constraints, e.g. for goto labels *)
and constr_name = rexp option

(** [constr_enum_const]: constraint on const enums *)
and constr_enum_const = ((constr_name * target) list) option

(** [constr_accesses]: constraint of a list of struct accesses or array accesses *)
and constr_accesses = (constr_access list) option

(* Predicate for expressing constraints over a list of subterms. It consists of:
   - a function that produces the constraints for the i-th subterm
   - a function that takes a list of booleans indicating which of the subterms
     matched their respective constraint, and returns a boolean indicating whether
     the full list should be considered as matching or not.
   - a string that explains what was the user intention *)

(** [target_list_pred]: predicate for expressing constraints over a list of stubterms. It consists of
    - a function that produces the constraints for the i-th subterm
    - a function that takes a list of booleans indicating which of the subterms match their respective constraint,
        and returns a boolean indicatin whether the full list should be considered as matching or not
    - a string that explains what was the user intention *)
and target_list_pred =
  { target_list_pred_ith_target : int -> target;
    target_list_pred_validate : bool list -> bool;
    target_list_pred_to_string : unit -> string; }

(** [constr_access]: constraint over a single struct or array access  *)
and constr_access =
  (* array indices may be arbitrary terms *)
  | Array_access of target
  (* struct fields are strings *)
  | Struct_access of constr_name
  | Any_access

(** [constr_cases]: for each case, its kind and a constraint on its body *)
and constr_cases = ((case_kind * target) list) option

(** [case_kind]: switch case kind used on [constr_cases] *)
and case_kind =
  (* case: value *)
  | Case_val of target
  | Case_default
  | Case_any

(** [abort_kind]: abort instruction kind used for abort insitruction constraints  *)
and abort_kind =
  | Any
  | Return
  | Break
  | Continue

[@@deriving show { with_path = false }]

let target_to_string = show_target
let constr_to_string = show_constr
let trm_kind_to_string = show_trm_kind

(** [target_simple]: is a [target] without any of the following relative constraints
   Constr_relative, Constr_occurrences, Constr_target.
   Note: It can include [cStrict] *)

type target_simple = target



(** [target_struct]: structured representation of a [target] that decomposes the special constructors
   such as Constr_relative, Constr_occurrences, Constr_target from the [target_simple]. *)
type target_struct = {
   target_path : target_simple; (* this path contains no nbMulti/nbEx/tBefore/etc.., only cStrict can be there *)
   target_relative : target_relative;
   target_occurrences : target_occurrences;
   target_incontracts : bool }


(** [make_target_list_pred ith_target validate to_string]: creates a simple target_pred object with with its components
   being [ith_target], [validate] and [to_string] *)
let make_target_list_pred (ith_target : int -> target) (validate : bool list -> bool) (to_string : unit -> string) : target_list_pred =
  { target_list_pred_ith_target = ith_target;
    target_list_pred_validate = validate;
    target_list_pred_to_string = to_string; }

(** [depth_pred d]: predicate on the depth *)
let depth_pred (d : depth) : depth =
  match d with
  | DepthAny -> DepthAny
  | DepthAt n ->
      if n <= 0 then failwith "Constr.depth_pred: argument of DepthAt is not positive";
      DepthAt (n-1)

(** [constr_map f c]: applies [f] recursively on constraint [c] *)
let constr_map (f : constr -> constr) (c : constr) : constr =
  (* LATER: optimize using identity reconstruction, like trm_map with optim *)
  let aux (cs:target) : target =
    List.map f cs in
  let auxs (tgs:targets) : targets =
    List.map aux tgs in
  match c with
  | Constr_incontracts -> c
  | Constr_depth _depth -> c
  | Constr_dir _d -> c
  | Constr_paths _ps -> c
  | Constr_include _s -> c
  | Constr_regexp _r -> c
  | Constr_for_c (p_init, p_cond, p_step, p_body) ->
     let s_init = aux p_init in
     let s_cond = aux p_cond in
     let s_step = aux p_step in
     let s_body = aux p_body in
     Constr_for_c (s_init, s_cond, s_step, s_body)
  | Constr_for (p_index, p_start, p_direction, p_stop, p_step, p_body) ->
      let s_start = aux p_start in
      let s_stop = aux p_stop in
      let s_step = aux p_step in
      let s_body = aux p_body in
      Constr_for (p_index, s_start, p_direction, s_stop, s_step, s_body)
  | Constr_while (p_cond, p_body) ->
     let s_cond = aux p_cond in
     let s_body = aux p_body in
     Constr_while (s_cond, s_body)
  | Constr_do_while (p_body, p_cond) ->
     let s_body = aux p_body in
     let s_cond = aux p_cond in
     Constr_do_while (s_body, s_cond)
  | Constr_if (p_cond, p_then, p_else) ->
     let s_cond = aux p_cond in
     let s_then = aux p_then in
     let s_else = aux p_else in
     Constr_if (s_cond, s_then, s_else)
  | Constr_decl_var (ty_pred, name, p_body) ->
     let s_body = aux p_body in
     Constr_decl_var (ty_pred, name, s_body)
  | Constr_decl_vars (ty_pred, name, p_body) ->
    let s_body = aux p_body in
    Constr_decl_vars (ty_pred, name, s_body)
  | Constr_decl_type name -> c
  | Constr_decl_enum (name, c_const) ->
     let s_const = Option.map (List.map (fun (n,tg) -> (n,aux tg))) c_const in
     Constr_decl_enum (name, s_const)
  | Constr_seq _tgt_list_pred -> c
  | Constr_var _name -> c
  | Constr_lit _-> c
  | Constr_fun (tgt_list_pred, ty_pred, p_body) ->
     Constr_fun (tgt_list_pred, ty_pred, aux p_body)
  | Constr_app (p_fun, tgt_list_pred, accept_encoded) ->
     let s_fun = aux p_fun in
     Constr_app (s_fun, tgt_list_pred, accept_encoded)
  | Constr_label (so, p_body) ->
     let s_body = aux p_body in
     Constr_label (so, s_body)
  | Constr_goto so -> c
  | Constr_return p_res ->
      let s_res = aux p_res in
      Constr_return s_res
  | Constr_abort kind -> c
  | Constr_access (p_base, ca, inner) ->
     let s_base = aux p_base in
     Constr_access (s_base, ca, inner)
  | Constr_switch (p_cond, cc) ->
     let s_cond = aux p_cond in
     let s_cc = Option.map (List.map (fun (k,tg) -> (k,aux tg))) cc in
     Constr_switch (s_cond, s_cc)
  | Constr_relative tr -> c
  | Constr_span (p_begin, p_end) ->
    let s_begin = aux p_begin in
    let s_end = aux p_end in
    Constr_span (s_begin, s_end)
  | Constr_occurrences oc -> c
  | Constr_target cl ->
      Constr_target (aux cl)
  | Constr_bool b -> c
  | Constr_root -> c
  | Constr_prim _ -> c
  | Constr_mark (_, str) -> c
  | Constr_or tl ->
      Constr_or (auxs tl)
  | Constr_and tl ->
      Constr_and (auxs tl)
  | Constr_diff (tl1, tl2) ->
      let s_tl1 = auxs tl1 in
      let s_tl2 = auxs tl2 in
      Constr_diff (s_tl1, s_tl2)
  | Constr_arg _ -> c
  | Constr_hastype _ -> c
  | Constr_var_init -> c
  | Constr_omp _ -> c
  | Constr_namespace _ -> c
  | Constr_pred _ -> c

(** [get_target_regexp_kinds tgs]: gets the list of trm_kinds of the terms
   for which we would potentially need to use the string representation,
   for resolving the targets [tgs]. The result is either a list of kinds
   without [TrmKind_Any], or a singleton list made of [TrmKind_Any]. *)
let get_target_regexp_kinds (tgs : target list) : trm_kind list =

  (* LATER: could be optimize by avoiding the construction of a copy of c;
     but this should be done automatically when constr_map is optimized *)
  (* Note: we use lists, but this is fine because the lists are very short *)
  let res = ref [] in
  let add (k : trm_kind) : unit =
    match k with
    | TrmKind_Any -> res := [TrmKind_Any]
    | _ -> if not (List.mem k !res) then res := k :: !res
    in
  let rec explore (c : constr) : constr = (* LATER: optimize using a constr_iter instead of constr_map *)
    begin match c with
    | Constr_regexp r -> add r.rexp_trm_kind
    | _ -> ()
    end;
    ignore (constr_map explore c);
    c
    in
  let iter_in_target tg =
    List.iter (fun c -> ignore (explore c)) tg in
  List.iter iter_in_target tgs;
  !res

(******************************************************************************)
(*                        Preprocessing of targets before resolution          *)
(******************************************************************************)

(** [target_flatten tg]: flattens all the constraints of type Constr_target *)
let target_flatten (tg : target) : target =
    let rec aux (cs : target) : target =
      match cs with
      | [] -> []
      | c::cs2 ->
          let r = match c with
            | Constr_target cs1 -> (aux cs1)
            | _ -> [c]
            in
          r @ (aux cs2)
      in
    aux tg

(** [target_to_target_struct]: converts a target into a target struct  *)
let target_to_target_struct ?(default_occ = ExpectNb 1) (tr : target) : target_struct =
  let tr = target_flatten tr in
  let relative = ref None in
  let occurences = ref None in
  let incontracts = ref true in (* TODO: think about it *)
  let process_constr (c : constr) : unit =
    match c with
    | Constr_relative re ->
      begin match !relative with
      | None -> relative := Some re;
      | Some _ -> failwith "Constr_relative provided twice in path"
      end
    | Constr_occurrences oc ->
      begin match !occurences with
      | None -> occurences := Some oc;
      | Some poc when poc = oc -> ();
      | _ -> failwith "Constr.Constr_occurrences provided twice in path"
      end
    | Constr_incontracts ->
        incontracts := true
    | _ -> ()
    in
  List.iter process_constr tr;
  let tgs = {
    target_path = List.filter (function | Constr_relative _ | Constr_occurrences _ -> false | _ -> true) tr;
    target_relative = begin match !relative with | None -> TargetAt | Some re -> re end;
    target_occurrences = begin match !occurences with | None -> default_occ | Some oc -> oc end;
    target_incontracts = !incontracts;
    } in
  tgs

(** [is_target_between tr]: checks if [tr] contains a relative constraint different from TargetAt *)
let is_target_between (tr : target) : bool =
   let tgs = target_to_target_struct tr in
   tgs.target_relative <> TargetAt

(******************************************************************************)
(*                              Target resolution                             *)
(******************************************************************************)

(*
  Particular case for target resolution: heap allocated variables
  Patterns:
    - declaration:A declaration is heap allocated if it is mutable
    - usage: particular use of get/access because variables are pointers
      in practice, only dereferencing has to be taken into account: array and
      struct get/access are better matched with regexp
  The user expresses targets as if the variables were not heap allocated but target
  resolution computes the appropriate target
 *)

(*
  Other particular case: accesses
  Pattern:
    when a Rvalue is expected, there is a star operator at the root of the
    subterm
  The user should ignore the existence of this star operator but target resolution
  should compute the appropriate target
 *)

(*
  translate t into the trm the user sees by removing heap allocation
  should be locally applied to the patterns described above
 *)

(** [add_dir d dll]: extends current explicit paths with a direction *)
let add_dir (d : dir) (dll : paths) : paths =
  List.map (fun dl -> d :: dl) dll

(** [is_equal_lit l l']: compares literals l and l' based on their values *)
(* LATER: Decide if similar lit with different types should be different or not *)
let is_equal_lit (l : lit) (l' : lit) =
  match l, l' with
  | Lit_unit, Lit_unit -> true
  | Lit_bool b, Lit_bool b' when b = b' -> true
  | Lit_int (_, n), Lit_int (_, n') when n = n' -> true
  | Lit_float (_, d), Lit_float (_, d') when d = d' -> true
  | Lit_string s, Lit_string s' when s = s' -> true
  | Lit_null _, Lit_null _ -> true
  | _ -> false

(** [get_trm_kind t]: gets the kind of trm [t] *)
let get_trm_kind (t : trm) : trm_kind =
  let instr_if_unit = begin match t.typ with
    | Some ty when is_typ_unit ty -> TrmKind_Instr
    | _ -> TrmKind_Expr
    end
    in
  match t.desc with
  | Trm_var _ -> TrmKind_Expr
  | Trm_lit _ -> instr_if_unit
  | Trm_prim _ -> TrmKind_Expr
  | Trm_let _ | Trm_let_mult _ | Trm_predecl _ -> TrmKind_Instr
  | Trm_typedef _ -> TrmKind_Typedef
  | Trm_if _-> instr_if_unit
  | Trm_fun _ -> TrmKind_Expr
  | Trm_seq _ -> instr_if_unit
  | Trm_apps _ -> instr_if_unit
  | Trm_while _ | Trm_do_while _ | Trm_for_c _ | Trm_for _| Trm_switch _ | Trm_my_switch _ | Trm_abort _ | Trm_goto _ -> TrmKind_Instr
  | Trm_omp_routine _ | Trm_extern _  | Trm_namespace _ | Trm_template _ | Trm_arbitrary _ | Trm_using_directive _ -> TrmKind_Any


(** [match_regexp_str r s]: checks if [s] can be matched with [r] *)
let match_regexp_str (r : rexp) (s : string) : bool =
  (*if s = "x" then incr Debug.counter;
  if !Debug.counter = 2 then raise Debug.Breakpoint; *)
  if r.rexp_substr then begin
    try let _ = Str.search_forward r.rexp_exp s 0 in true
    with Not_found -> false
  end else begin
    (* Here we assume that [r.rexp_exp] ends with [^], to ensure that
       the pattern matches all of [s] and not a strict substring of [s]. *)
    Str.string_match r.rexp_exp s 0
  end


(* LATER: this could be passed as argument throughout the function calls *)
(* This variable should only be modified by [Target.with_stringreprs_available] *)
(* Keep in mind that the stringrepr is not available for AST nodes that are removed
   during the computation of [cfeatures_intro], that is, during the translations
   from OptiTrust AST to the C AST. *)
let stringreprs : (stringreprid, string) Hashtbl.t option ref = ref None

(** [print_stringreprs ()]: for debugging purpose *)
let print_stringreprs () : unit =
  match !stringreprs with
  | None -> failwith "Constr.print_stringreprs: no table registered"
  | Some m ->
      let pr id s =
        Tools.debug "stringreprs[%d] = %s\n----" id s in
      Tools.debug "====<constr.stringreprs>====";
      Hashtbl.iter pr m;
      Tools.debug "====</constr.stringreprs>===="

(** [get_stringrepr t]: returns the string representation saved in table [stringreprs],
   or an empty string otherwise *)
let get_stringrepr (t : trm) : string =
    let print (t : trm) : unit =
      let s = Ast_to_c.default_style () in
      let style = { s with print_string_repr = true } in
      Tools.debug "==\n%s\n==" (Ast_to_c.ast_to_string ~style t)
      in
    match !stringreprs with
    | None -> trm_fail t (Printf.sprintf "Constr.get_stringrepr: stringreprs must be computed and registered before resolving constraints, %s" (Ast_to_text.ast_to_string t))
    | Some m ->
        match Trm.trm_get_stringreprid t with
        | Some id ->
          begin match Hashtbl.find_opt m id with
          | None ->
              (* This term must correspond to a node that was removed during
                 [cfeatures_intro], hence not printed *)
              if !Flags.debug_stringreprs then begin
                Tools.warn "missing stringrepr for id %i" id;
                print t;
              end;
              ""
          | Some s -> s
          end
        | None ->
          if !Flags.debug_stringreprs then begin
            Tools.warn "missing stringrepr id";
            print t;
          end;
          ""

(** [match_regexp_trm_kind k t]: checks if [t] is of kind [k] *)
let match_regexp_trm_kind (k : trm_kind) (t : trm) : bool =
  (k = TrmKind_Any) || (get_trm_kind t = k)

(** [match_regexp_trm_kinds ks t]: checks if [t] is of one of the kinds in [ks] *)
let match_regexp_trm_kinds (ks : trm_kind list) (t : trm) : bool =
  List.mem (get_trm_kind t) ks

(** [match_regexp_trm r t]: checks if the string representation of [t] can be matched with [r] *)
let match_regexp_trm (r : rexp) (t : trm) : bool =
  if not (match_regexp_trm_kind r.rexp_trm_kind t) then false else begin
    let s = get_stringrepr t in
    if !Flags.debug_stringreprs then
      Tools.debug "Considered: %s" s;
    s <> "" && match_regexp_str r s
    (* If the stringrepr is not available, we return false *)
  end

(** [is_constr_regexp c]: checks if [c] is a regexp constraint *)
let is_constr_regexp (c : constr) : bool =
  match c with | Constr_regexp _ -> true | _ -> false


(* MIGHT DISAPPEAR? *)
(** [trm_access]: concrete accesses in a trm *)
type trm_access =
  | Array_access_get of trm (* operator -> [i] *)
  | Array_access_addr of trm (* operator [i] *)
  | Struct_access_get of field (* operator->f *)
  | Struct_access_addr of field (* operator.f *)

(** [get_nested_accesses t]: for a given trm [t], if it's an access trm return the list of accesses,
    the list starts with the base, and ends with the last access *)
let rec get_nested_accesses (t : trm) : trm * (trm_access list) =
  Pattern.pattern_match t [
    Pattern.(trm_struct_access !__ !__) (fun t' f () ->
      let (base, al) = get_nested_accesses t' in
      (base, Struct_access_addr f :: al)
    );
    Pattern.(trm_struct_get !__ !__) (fun t' f () ->
      let (base, al) = get_nested_accesses t' in
      (base, Struct_access_get f :: al)
    );
    Pattern.(trm_array_access !__ !__) (fun t' i () ->
      let (base, al) = get_nested_accesses t' in
      (base, Array_access_addr i :: al)
    );
    Pattern.(trm_array_get !__ !__) (fun t' i () ->
      let (base, al) = get_nested_accesses t' in
      (base, Array_access_get i :: al)
    );
    Pattern.__ (fun () -> (t, []))
  ]


(** [extract_last_path_item p]: extracts the last direction from a nonempty path *)
let extract_last_path_item (p : path) : dir * path =
  match List.rev p with
  | [] -> raise Not_found
  | d :: p' -> (d, List.rev p')

  (** [get_sequence_length t]: gets the number of instructions on a sequence *)
let get_sequence_length (t : trm) : int =
  begin match t.desc with
  | Trm_seq (tl, _) -> Mlist.length tl
  | _ -> trm_fail t "Constr.get_sequence_length: expected a sequence"
  end

(** [get_arity_of_seq_at]: gets the arity of a sequence at path [p] *)
(* TODO: move higher in file*)
let get_arity_of_seq_at (p : path) (t : trm) : int =
  let seq_trm = Path.resolve_path p t in
  get_sequence_length seq_trm


(** [check_hastype pred t]: tests whether [t] carries a type
   that satisfies [pred]. If [t] does not carry a type information,
   then the return value is [false]. For constraints to work properly,
   one may want to check that types are up to date (e.g. by reparsing). *)
let check_hastype (pred : typ->bool) (t : trm) : bool =
    match t.typ with
    | Some ty -> pred ty
    | None -> false

(** [check_name name s]: checks if constraint [name] matches string [s] *)
let check_name (name : constr_name) (s : string) : bool =
  match name with
  | None -> true
  | Some r ->
     match_regexp_str r s


(** [explore_list tl d cont]: calls [cont] on each element of the list and gathers the results
    used for seq, array, struct, and fun arguments
   [d] is the function that gives the direction to add depending on the index *)
let explore_list (tl : 'a list) (d : int -> dir)
  (cont : 'a -> paths) : paths =
  List.fold_lefti (fun i epl t -> epl @ add_dir (d i) (cont t)) [] tl

(*
  call cont on each element of the list whose index is in the domain and
  gather the results
  d: function that gives the direction to add depending on the index
 *)

(** [explore_list_ind tl d dom cont]: calls [cont] on each element of the list,
   [index] is in the domain and gather results
   [d] is a fucntion that gives direction to add depending on the [index] *)
let explore_list_ind (tl : 'a list) (d : int -> dir) (dom : int list)
  (cont : 'a -> paths) : paths =
  List.fold_lefti
    (fun i epl t ->
      if List.mem i dom then epl @ add_dir (d i) (cont t) else epl)
    []
    tl

(** [check_constraint ~incontracts c]: checks if constraint c is satisfied by trm t *)
let rec check_constraint ~(incontracts:bool) (c : constr) (t : trm) : bool =
  let check_target = check_target ~incontracts in
  match c, t.desc with
  (*
    target constraints never hold since they are checked against nodes before
    calling check_constraint in resolve_target
  *)
  | Constr_depth _,_
    | Constr_dir _, _
    | Constr_include _, _ ->
    false
  | Constr_regexp r, _ -> match_regexp_trm r t
  | Constr_for_c (p_init, p_cond, p_step, p_body),
    Trm_for_c (init, cond, step, body, _) ->
    check_target p_init init &&
    check_target p_cond cond &&
    check_target p_step step &&
    check_target p_body body
  | Constr_for (p_index, p_start, p_direction, p_stop, p_step, p_body), Trm_for(range, body, _) ->
    let direction_match = match p_direction with
    | None -> true
    | Some d -> d = range.direction in
    check_name p_index range.index.name &&
    direction_match &&
    check_target p_start range.start &&
    check_target p_stop range.stop &&
    check_target p_step range.step &&
    check_target p_body body
  | Constr_while (p_cond, p_body), Trm_while (cond, body) ->
    check_target p_cond cond &&
    check_target p_body body
  | Constr_do_while (p_body, p_cond), Trm_do_while (body, cond) ->
    check_target p_body body &&
    check_target p_cond cond
  | Constr_if (p_cond, p_then, p_else), Trm_if (cond, then_t, else_t) ->
    check_target p_cond cond &&
    check_target p_then then_t &&
    check_target p_else else_t
  | Constr_decl_var (ty_pred, name, p_body) , Trm_let ((x,tx), body) ->
    ty_pred (get_inner_ptr_type tx) &&
    check_name name x.name &&
    check_target p_body body
  | Constr_decl_vars (ty_pred, name, p_body), Trm_let_mult bs ->
    List.fold_left (fun acc ((x, tx), body) ->
      let b = ty_pred (get_inner_ptr_type tx) &&
        check_name name x.name &&
        check_target p_body body in
      acc || b
    ) false bs
  | Constr_decl_type name, Trm_typedef td ->
    (* TODO: Why do we check this ? *)
    let is_new_typ = begin match td.typedef_body with
    | Typedef_alias _ -> true
    | Typedef_record _ -> true
    | _ -> false
    end in
    let x = td.typedef_name in
    is_new_typ && check_name name x.name
  | Constr_decl_enum (name, cec), Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_enum xto_l -> check_name name td.typedef_name.name && check_enum_const ~incontracts cec xto_l
    | _ -> false
    end
  | Constr_seq cl, Trm_seq (tl, _) when not (trm_is_nobrace_seq t || trm_is_mainfile t) ->
    check_list ~incontracts ~depth:(DepthAt 0) cl (Mlist.to_list tl) (* LATER: check why depth 0 here and not
    in Constr_app *)
  | Constr_var name, Trm_var x ->
    check_name name x.name
  | Constr_lit pred_l, Trm_lit l ->
    pred_l l
  | Constr_fun (cl_args, ty_pred, p_body), Trm_fun (args, tx, body, _) ->
    ty_pred tx &&
    check_args cl_args args &&
    check_target p_body body
  | Constr_app (p_fun, cl_args, accept_encoded), Trm_apps (f, args, _) ->
    if not accept_encoded then
      begin match f.desc with
      | Trm_prim (_, Prim_ref)
      | Trm_prim (_, Prim_unop Unop_get) -> false
      |  _ -> check_target p_fun f &&
              check_list ~incontracts ~depth:(DepthAny) cl_args args
      end
    else
      check_target p_fun f &&
      check_list ~incontracts ~depth:(DepthAny) cl_args args
  | Constr_label (so, p_body), _ ->
    let t_labels = trm_get_labels t in
    List.fold_left (fun acc (l : label) -> check_name so l || acc) false t_labels &&
    check_target p_body t
  | Constr_goto so, Trm_goto l ->
    check_name so l
  | Constr_return p_res, Trm_abort (Ret (Some res)) ->
    check_target p_res res
  | Constr_abort Any, Trm_abort _ -> true
  | Constr_abort Return, Trm_abort (Ret _) -> true
  | Constr_abort Break, Trm_abort (Break _) -> true
  | Constr_abort Continue, Trm_abort (Continue _) -> true
  | Constr_access (p_base, ca,ia), _ ->
    let (base, al) = get_nested_accesses t in
    check_target p_base base &&
    check_accesses ~incontracts ~inner_accesses:ia ca al
  | Constr_switch (p_cond, cc), Trm_switch (cond, cases) ->
    check_target p_cond cond &&
    check_cases ~incontracts cc cases
  | Constr_bool b, _ -> b
  | Constr_root, _ -> trm_is_mainfile t

  | Constr_prim (pred_ty, pred_prim), Trm_prim (ty1, p1) ->
    pred_ty ty1 && pred_prim p1
  | Constr_mark (pred, _m), _ ->
    if !old_resolution then begin
      let t_marks = trm_get_marks t in
      begin match t.desc with
      | Trm_seq (tl, _) ->
        (List.exists pred t_marks) || (List.fold_left (fun acc x -> (List.exists pred x) || acc) false (Mlist.get_marks tl))
      | _ -> List.exists pred t_marks
      end
    end else begin
      List.exists pred (trm_get_marks t)
    end
  | Constr_hastype pred , _ ->
    check_hastype pred t
  | Constr_var_init, Trm_apps ({desc = Trm_prim (_, (Prim_ref | Prim_ref_uninit)); _}, _, _) -> false
  | Constr_var_init, _ -> true
  | Constr_omp (pred, _), _ -> trm_has_pragma pred t
  | Constr_namespace cn, Trm_namespace (name, _, _) -> check_name cn name
  | Constr_pred pred, _ -> pred t
  | _ -> false

(** [check_list ~incontracts ~depth lpred tl]: checks if [tl] satisfy the predicate [lpred] *)
and check_list ~(incontracts:bool) ?(depth : depth = DepthAny) (lpred : target_list_pred) (tl : trms) : bool =
  let ith_target = lpred.target_list_pred_ith_target in
  let validate = lpred.target_list_pred_validate in
  validate (List.mapi (fun i t -> check_target ~incontracts ~depth (ith_target i) t) tl)

(** [check_args lpred tx]: checks if [txl] satisfy the predicate [lpred] *)
and check_args (lpred : target_list_pred) (txl : typed_vars) : bool =
  let ith_target = lpred.target_list_pred_ith_target in
  let validate = lpred.target_list_pred_validate in
  validate (List.mapi (fun i tx -> check_arg (ith_target i) tx) txl)

(** [check_arg tg tx]: checks if the typed-variable [tx] satisfies the argument-constraint [tg].
   An argument constraint is a singleton constraint, of the form [cArg ..] or [cTrue]. *)
and check_arg (tg:arg_constraint) ((var, var_typ) : typed_var) : bool =
  match tg with
  | [] -> true
  | [c] -> begin match c with
           | Constr_bool true -> true
           | Constr_arg (var_constraint, typ_constraint) ->
              var_constraint var.name && typ_constraint var_typ
           | _ -> failwith "Constr.check_arg: target expressing constraints on arguments must be of
                             the form [cArg...] or [cTrue]."
          end
  | _ -> failwith "Constr.check_arg: target expressing constraints on arguments must be list with at most one item."


(** [check_accesses ~inner_accesses ca al]: checks if [al] is matched by [ca] *)
and check_accesses ~(incontracts:bool) ?(inner_accesses : bool = true) (ca : constr_accesses) (al : trm_access list) : bool =
  let rec aux (cal : constr_access list) (al : trm_access list) : bool =
    match cal, al with
    | [], a -> if not inner_accesses then a = [] else true
    | Array_access p_index :: cal, Array_access_get index :: al ->
       check_target ~incontracts p_index index &&
       aux cal al
    | Array_access p_index :: cal, Array_access_addr index :: al ->
       check_target ~incontracts p_index index &&
       aux cal al
    | Struct_access so :: cal, Struct_access_get f :: al ->
       check_name so f &&
       aux cal al
    | Struct_access so :: cal, Struct_access_addr f :: al ->
       check_name so f &&
       aux cal al
    | Any_access :: cal, _ :: al -> aux cal al
    | _ -> false
  in
  match ca with
  | None -> true
  | Some cal -> aux cal al

(** [check_cases ~incontract cc cases]: checks if [cases] are matched by [cc] *)
and check_cases ~(incontracts:bool) (cc : constr_cases) (cases : (trms * trm) list) : bool =
  let rec aux (cl : (case_kind * target) list)
    (cases : (trms * trm) list) : bool =
    match cl, cases with
    | [], [] -> true
    | (k, p_body) :: cl, (tl, body) :: cases ->
       check_kind ~incontracts k tl &&
       check_target ~incontracts p_body body &&
       aux cl cases
    | _ -> false
  in
  match cc with
  | None -> true
  | Some cl -> aux cl cases

(** [check_kind ~incontracts k tl]: checks if [tl] are of kind [k] *)
and check_kind ~(incontracts:bool) (k : case_kind) (tl : trms) : bool =
  match k, tl with
  | Case_any, _ -> true
  | Case_default, [] -> true
  | Case_val p_val, _ ->
     (* check that one of the cases corresponds to the given target *)
     List.mem true (List.map (check_target ~incontracts p_val) tl)
  | _ -> false

(** [check_enum_const ~incontracts cec xto_l]: checks if [xto_l] are matched by constraint [cec] *)
and check_enum_const ~(incontracts:bool) (cec : constr_enum_const)
  (xto_l : (var * (trm option)) list) : bool =
  match cec with
  | None -> true
  | Some cnp_l ->
     List.for_all2
       (fun (cn, p) (n, t_o) ->
         check_name cn n.name &&
         match p, t_o with
         | [], None -> true
         | _, None -> false
         | _, Some t -> check_target ~incontracts p t
       )
       cnp_l
       xto_l

(** [check_target ~incontracts ~depth tr t]: checks if target tr leads to at least one subterm of t *)
and check_target ~(incontracts:bool) ?(depth : depth = DepthAny) (tr : target) (t : trm) : bool =
  match resolve_target_simple ~incontracts ~depth tr t with
  | [] -> false
  | _ -> true

(*
  resolve_target computes the directions to matching subterms
  expected invariants: no duplicate target and no target which is prefix of
  another target that appears after it in the list. Guaranteed by the call to
  sort_unique *)

(** [debug_resolution]: only for debugging *)
and debug_resolution = false

(** [resolve_target_simple ~incontracts ~depth trs t]: the main function that resolves a target
     It returns a list of paths that resolve to the given target
     [depth] is the depth level where the targetd should be resolved
     [trs]: a clean target t
     [t]: ast  *)
and resolve_target_simple ~(incontracts:bool) ?(depth : depth = DepthAny) (trs : target_simple) (t : trm) : paths =
  Stats.incr_target_resolution_steps ();
  let epl =
    match trs with
    | [] -> [[]]
    | Constr_target tl :: trest -> (* LATER: see if we can flatten recursively in targets before we start *)
       resolve_target_simple ~incontracts ~depth (tl @ trest) t

    | Constr_or tl :: trest -> (* LATER: maybe we'll add an option to enforce that each target from the list tl resolves to at least one solution *)
        let all_targets_must_resolve = false in
        let res = List.fold_left (fun acc tr ->
          let potential_targets = resolve_target_simple ~incontracts ~depth (tr @ trest) t in
          begin match potential_targets with
          | ([] | [[]]) when all_targets_must_resolve -> trm_fail t "Constr.resolve_target_simple: for Constr_and all targets should match a trm"
          | _ ->
            Path.union acc potential_targets
          end ) [] tl in
       if debug_resolution then begin
          Tools.debug "resolve_target_simple[Constr_or]\n  ~target:%s\n  ~term:%s\n  ~res:%s"
            (target_to_string trs)
            (Ast_to_c.ast_to_string t)
            (paths_to_string ~sep:"\n   " res)
        end;
        res
    | Constr_diff (tl1 , tl2) :: [] ->
      let all_targets_must_resolve = false in
      let targets_to_keep =
      List.fold_left (fun acc tr ->
          let potential_targets = resolve_target_simple ~incontracts ~depth tr t in
          begin match potential_targets with
          | ([] | [[]]) when all_targets_must_resolve -> trm_fail t "Constr.resolve_target_simple: for Constr_and all targets should match a trm"
          | _ ->
            Path.union acc potential_targets
          end ) [] tl1 in
      let targets_to_remove =
      List.fold_left (fun acc tr ->
          let potential_targets = resolve_target_simple ~incontracts tr t in
          begin match potential_targets with
          | ([] | [[]]) -> acc
          | _ ->
            Path.union acc potential_targets
          end ) [] tl2 in
      Path.diff targets_to_keep targets_to_remove

    | Constr_and tl :: trest ->
        (* LATER: ARTHUR : optimize resolution by resolving the targets only by exploring
          through the paths that are candidates; using e.g. path_satisfies_target *)
        let all_targets_must_resolve = false in
        List.fold_lefti (fun i acc tr ->
          let targetsi = resolve_target_simple ~incontracts (tr @ trest) t in
          begin match targetsi with
          | ([] | [[]]) when all_targets_must_resolve -> trm_fail t "Constr.resolve_target_simple: for Constr_and all targets should match a trm"
          | _ ->
            if i = 0
              (* First step, initalize the acc *)
              then targetsi
            (* Compute the intersection of all resolved targets *)
              else Path.intersect acc targetsi
          end) [] tl

    | Constr_diff (_ , _) :: _ ->
        trm_fail t "Constr.cDiff can only appear at last item in a target"

    | Constr_depth new_depth :: tr ->
        (* Force the depth argument for the rest of the target, override the current [depth] *)
        resolve_target_simple ~incontracts ~depth:new_depth tr t
    (* TODO: refactor follow_dir to avoid special case? *)
    | Constr_dir (Dir_before i) :: [] ->
        [[Dir_before i]]
    | Constr_dir (Dir_span { start; stop }) :: [] ->
        [[Dir_span { start; stop }]]
    | Constr_dir d :: tr ->
        follow_dir (resolve_target_simple ~incontracts tr) d t
    | Constr_paths ps :: tr ->
      resolve_target_simple ~incontracts ~depth (
        Constr_or (
          List.map (List.map (fun d -> Constr_dir d)) ps
        ) :: tr) t
    | c :: p ->
      let strict = match depth with
        | DepthAt 0 -> true
        | _ when c = Constr_root -> true (* useless to search for the root in depth *)
        | _ -> false in
      let skip_here = match depth with DepthAt n when n > 0 -> true | _ -> false in
      let res_deep =
        if strict
           then [] (* in strict mode, must match c here *)
           else (explore_in_depth ~incontracts ~depth (c :: p) t) in
      let res_here =
         if skip_here || (is_constr_regexp c && res_deep <> [])
           then [] (* if a regexp matches in depth, don't test it here *)
           else (resolve_constraint ~incontracts c p t) in

      (* DEBUG *)
      if debug_resolution then begin
         Tools.debug "resolve_target_simple\n  ~strict:%s\n  ~target:%s\n  ~term:%s\n ~deep:%s\n  ~here:%s"
          (if strict then "true" else "false")
          (target_to_string trs)
          (Ast_to_c.ast_to_string t)
          (paths_to_string ~sep:"\n   " res_deep)
          (paths_to_string ~sep:"\n   " res_here);
      end;

      res_deep@res_here  (* put deeper nodes first *)
  in
  List.sort_uniq compare_path epl

(** [resolve_target_struct tgs t]: resolves the structured target [tgs] over trm [t]
    This function gets the result from [resolve_target_simple] and checks if it matches
    the given requirements. If one of the requirements is not fulfilled the target resolution will fail. *)
and resolve_target_struct ?(depth = DepthAny) (tgs : target_struct) (t : trm) : paths =
  (* DEBUG: printf "tgs: %s\n" (target_struct_to_string tgs); *)
  let res = resolve_target_simple ~incontracts:tgs.target_incontracts ~depth tgs.target_path t in
  (* DEBUG: if res <> [] then
    printf "resolve_target_struct res: %s\n" (paths_to_string res); *)
  let nb = List.length res in
  (* Check if nb is equal to the specification of tgs.target_occurrences, if not then something went wrong *)
  let error s =
    raise (Resolve_target_failure s) in
  begin match tgs.target_occurrences with
  | ExpectNb 1 -> if nb <> 1 then error (Printf.sprintf "resolve_target_struct: expected exactly one match, got %d." nb); res
  | ExpectNb n -> if nb <> n then error (Printf.sprintf "resolve_target_struct: expected %d matches, got %d." n nb); res
  | ExpectMulti -> if nb = 0 then error (Printf.sprintf "resolve_target_struct: expected at least one occurrence, got %d." nb); res
  | ExpectAnyNb -> res
  | SelectOcc (n_opt, i_selected) ->
    begin match n_opt with
    | Some n ->
      if n <> nb then error (Printf.sprintf "resolve_target_struct: expected %d matches, got %d" n nb)
        else
          List.filter_selected i_selected res;
    | None ->
      let min_nb = List.fold_left (fun acc i -> if i > 0 then max (i+1) acc else max (-i) acc) 0 i_selected in
      if nb < min_nb
        then error (Printf.sprintf "resolve_target_struct: expected at least %d matches, got %d" min_nb nb)
        else List.filter_selected i_selected res;
    end
  end

(** [fix_target_between rel t p]:
   - if rel is [TargetBefore] or [TargetAfter], takes a path to a node in a sequence,
     and convert the last [Dir_seq_nth n] in the path into a [Dir_before n] or [Dir_before (n+1)]
   - if rel is [Target_First] or [TargetLast], takes a path to a sequence with [n] items,
     and extend the path with [Dir_before 0] or [Dir_before n]. *)
and fix_target_between (rel : target_relative) (t : trm) (p : path) : paths =
  match rel with
  | TargetAt -> failwith "Constr.fix_target_between: Didn't expect a TargetAt"
  | TargetFirst ->
      [p @ [Dir_before 0]]
  | TargetLast ->
      let n = get_arity_of_seq_at p t in
      [p @ [Dir_before n]]
  | TargetBetweenAll ->
      let n = get_arity_of_seq_at p t in
      (* printf "%d / %s\n" n (Tools.list_to_string (List.map (sprintf "%d") (List.init (n - 1) (fun i -> i + 1)))); *)
      List.init (n - 1) (fun i -> p @ [Dir_before (i + 1)])
  | TargetBefore | TargetAfter ->
      let shift =
         match rel with
         | TargetBefore -> 0
         | TargetAfter -> 1
         | _ -> assert false
         in
      let (d,p') =
        try extract_last_path_item p
        with Not_found -> failwith "Constr.fix_target_between: expected a nonempty path"
        in
      match d with
      | Dir_seq_nth i -> [p' @ [Dir_before (i + shift)]]
      | _ -> failwith "Constr.fix_target_between: expected a Dir_seq_nth as last direction"

(** [resolve_target tg t]: resolves the target [tg];
   Marks are set in the ast_after inside the trace only if -dump-trace is provided *)
and resolve_target ?(prefix : Path.path = []) (tg : target) (t : trm) : paths =
  match tg with
  (* shortcut: paths are already resolved *)
  | [Constr_paths ps] -> ps
  | _ -> Trace.target_resolve_step ~prefix (fun () ->
    Trace.step_arg (target_to_string tg);
    let tgs = target_to_target_struct tg in
    let res = resolve_target_struct tgs t in
    (* Patch the path if it is a target_between *)
    let ps =
      if tgs.target_relative <> TargetAt then begin
        let res2 = List.concat_map (fix_target_between tgs.target_relative t) res in
        res2
      end else res
      in
    ps)

  (* LATER FOR THE OPTIMIZATION
      let marks = List.map (fun _ -> Mark.next()) ps in
      (* LATER: could use a system to set all the marks in a single pass over the ast,
          able to hand the Dir_before *)
      let t = List.fold_left2 (fun t p m ->
        match last_dir_before_inv p with
        | None -> apply_on_path (trm_add_mark m) t p
        | Some (p_to_seq,i) -> apply_on_path (trm_add_mark_between i m) t p_to_seq)
        t ps marks in
      Trace.set_ast t;
      match marks_out with
      | Some mso -> mso := marks
      | None -> begin
        (* Here we don't call [Marks.remove] to avoid a circular dependency issue. *)
        (* FIXME: duplicated code with Target.iteri *)
        let t =
          match last_dir_before_inv p with
          | None -> apply_on_path (trm_rem_mark m) t p
          | Some (p_to_seq,i) -> apply_on_path (trm_rem_mark_between m) t p_to_seq
          in
        Trace.set_ast t
      end;
      ps
  *)


(** [resolve_target_exactly_one tg t]: similar to [resolve_target] but this one fails if the target resolves to more than one path *)
and resolve_target_exactly_one (tg : target) (t : trm) : path =
  match resolve_target tg t with
  | [p] -> p
  | _ -> trm_fail t "Constr.resolve_target_exactly_one: obtained several targets."

(** [resolve_target_between_children]: resolves target [tg] restricted to spaces between children items of [t] *)
and resolve_target_between_children (tg: target) (t: trm) : int list =
  let tgs = target_to_target_struct ~default_occ:ExpectAnyNb tg in
  let depth = match tgs.target_relative with
    | TargetBefore | TargetAfter -> 1
    | _ -> 0
  in
  let res = resolve_target_struct ~depth:(DepthAt depth) tgs t in
  let ps =
    if tgs.target_relative <> TargetAt then
      List.concat_map (fix_target_between tgs.target_relative t) res
    else res
  in
  let exception InvalidBetweenChildrenPath of path in
  try
    List.map (function [Dir_before i] -> i | path -> raise (InvalidBetweenChildrenPath path)) ps
  with InvalidBetweenChildrenPath path ->
    Flags.verbose_warn t.loc "Constr.resolve_target_between_children: found path %s that does not correspond to a space between children, did you forget to add tBefore or tAfter ?" (path_to_string path);
    []

(** [resolve_constraint ~incontracts c p t]: checks [c] against [t] and in case of success continue with [p].
   With a special case for handling a [Constr_mark] constrained that reaches a
  mark found in aa MList. *)
and resolve_constraint ~(incontracts:bool) (c : constr) (p : target_simple) (t : trm) : paths =
  let loc = t.loc in
  match c with
  | Constr_target _ -> trm_fail t "Constr.resolve_constraint should not reach a Constr_target"
  (*
    do not resolve in included files, except if the constraint is Constr_include
   *)
  | Constr_include h when trm_is_include t ->
     (* remove the include annotation for target resolution to proceed in the
       included file *)
     resolve_target_simple ~incontracts p (trm_alter ~annot:trm_annot_default t)
  | _ when trm_is_include t ->
     Flags.verbose_warn loc "Constr.resolve_constraint: not an include constraint";
     []
  (* target constraints first *)
  (* following directions *)
  | Constr_dir _ -> trm_fail t "Constr.resolve_constraint should not reach a Constr_dir"

  | Constr_span (tbegin, tend) ->
    if p <> [] then begin
      Flags.verbose_warn loc "Constr.resolve_constraint: tSpan should be the last element of a target";
      []
    end else begin
      match trm_seq_inv t with
      | None -> []
      | Some _ ->
        let exception DifferentNumberOfStartAndStopPaths of int * int in
        let exception NegativeSpan of int * int in
        let exception OverlappingSpans of int * int in
        try
          let starts = resolve_target_between_children tbegin t in
          let stops = resolve_target_between_children tend t in
          (* Target paths are sorted by default *)

          let len_starts = List.length starts in
          let len_stop = List.length stops in
          if len_starts <> len_stop then raise (DifferentNumberOfStartAndStopPaths (len_starts, len_stop));

          let last_stop = ref 0 in
          let spans = List.map (fun (start, stop) ->
              if start > stop then raise (NegativeSpan (start, stop));
              if !last_stop > start then raise (OverlappingSpans (!last_stop, start));
              last_stop := stop;
              [Dir_span { start; stop }]
            ) (List.combine starts stops) in
            spans

        with
        | Resolve_target_failure str ->
          Flags.verbose_warn loc "Constr.resolve_constraint: tSpan sub-target failed: %s" str;
          []
        | DifferentNumberOfStartAndStopPaths (len_start, len_stop) ->
          Flags.verbose_warn loc "Constr.resolve_constraint: tSpan found different numbers of start (%d) and stop (%d) paths" len_start len_stop;
          []
        | NegativeSpan (b, e) ->
          Flags.verbose_warn loc "Constr.resolve_constraint: negative tSpan: found a stop path (at position %d) before the matching start path (at position %d)" e b;
          []
        | OverlappingSpans (e, b) ->
          Flags.verbose_warn loc "Constr.resolve_constraint: overlapping tSpan: found a start path (at position %d) before the next end path (at position %d)" b e;
          []

    end

  (*
    if the constraint is a target constraint that does not match the node or
    if it is another kind of constraint, then we check if it holds
   *)
  | _ ->
    let paths_on_this_node =
      if check_constraint ~incontracts c t
        then resolve_target_simple ~incontracts p t
        else [] in
    let paths_on_the_mlist =
      if !old_resolution then [] else
      (* find paths towards mark-between in a MList, in which case we generate a Dir_before *)
      match c, trm_mlist_inv_marks t with
      | (Constr_mark (pred,_)), (Some marks) ->
          List.concat (List.mapi (fun i ms -> if List.exists pred ms then [[Dir_before i]] else []) marks)
      | _ -> []
      in
    let res = paths_on_this_node @ paths_on_the_mlist in
    (* DEBUG:
    if paths_on_the_mlist <> [] then
      printf "paths_on_the_mlist: %s\n" (paths_to_string paths_on_the_mlist); *)
    if res = [] then
      Flags.verbose_warn loc "Constr.resolve_constraint: constraint %s does not hold" (constr_to_string c);
    res

(** [explore_in_depth ~incontracts ~depth p t]: calls resolve_target_simple on subterms of t if possible *)
and explore_in_depth ~(incontracts:bool) ?(depth : depth = DepthAny) (p : target_simple) (t : trm) : paths =
  (* By default, traversing a node decreases the depth *)
  let aux = resolve_target_simple ~incontracts ~depth:(depth_pred depth) p in
  (* For bodies of functions/loops/conditials, however, we do decrease the depth
     because traversing the [Trm_seq] just below will already decrease the depth *)
  let aux_body = resolve_target_simple ~incontracts ~depth p in

  let aux_resource_set_dir contract_dir resource_set_dir res_list =
    explore_list res_list (fun n -> Dir_contract (contract_dir, resource_set_dir, n)) (fun (h, f) -> aux f)
  in

  let aux_contract_dir contract_dir res =
    aux_resource_set_dir contract_dir Resource_set_pure res.pure @
    aux_resource_set_dir contract_dir Resource_set_linear res.linear
  in

  let loc = t.loc in
  (* no exploration in depth in included files *)
  if trm_is_include t then begin
    Flags.verbose_warn loc "Constr.explore_in_depth: no exploration in included files";
    []
  end
  else
    begin match t.desc with
    | Trm_let ((_, ty), body) ->
      (if incontracts then add_dir Dir_type (aux ty) else []) @ add_dir Dir_let_body (aux body)
    | Trm_fun (_, _, body, contract) ->
      add_dir Dir_body (aux_body body) @
      begin match contract with
      | FunSpecContract contract ->
        if incontracts then
          (aux_contract_dir Contract_pre contract.pre) @
          (aux_contract_dir Contract_post contract.post)
        else []
      | _ -> []
      end
    | Trm_typedef td ->
      begin match td.typedef_body with
      | Typedef_record rfl ->
        let res = List.fold_lefti (fun i acc (rf, _) ->
          begin match rf with
          | Record_method t1 ->
            (add_dir (Dir_record_member i) (aux_body t1)) @ acc
          | _ -> acc
          end
        ) [] rfl in
        res
      | Typedef_enum xto_l ->
        let (il, tl) =
          List.fold_lefti
            (fun n (il, tl) (_, t_o) ->
              match t_o with
              | None -> (il, tl)
              | Some t -> (il@[n], tl@[t])
            )
           ([], [])
           xto_l
        in
        add_dir Dir_name (aux (trm_var ?loc td.typedef_name)) @
        (explore_list (List.map (fun (y, _) -> trm_var ?loc y) xto_l)
           (fun n -> Dir_enum_const (n, Enum_const_name))
           (aux)) @
        (explore_list tl
           (fun n -> Dir_enum_const (List.nth il n, Enum_const_val))
           (aux))
      | _ -> []
      end
    | Trm_abort (Ret (Some body)) ->
      add_dir Dir_body (aux body)
    | Trm_for (range, body, contract) ->
      (add_dir Dir_for_start (aux range.start)) @
      (add_dir Dir_for_stop (aux range.stop)) @
      (add_dir Dir_for_step (aux range.step)) @
      (add_dir Dir_body (aux_body body)) @
      if incontracts then
        (aux_contract_dir Contract_pre contract.iter_contract.pre) @
        (aux_contract_dir Contract_post contract.iter_contract.post) @
        (aux_resource_set_dir Contract_loop_ghosts Resource_set_pure contract.loop_ghosts) @
        (aux_resource_set_dir Contract_parallel_reads Resource_set_linear contract.parallel_reads) @
        (aux_contract_dir Contract_invariant contract.invariant)
      else []

    | Trm_for_c (init, cond, step, body, invariant) ->
      (* init *)
      (add_dir Dir_for_c_init (aux init)) @
      (* cond *)
      (add_dir Dir_cond (aux cond)) @
      (* step *)
      (add_dir Dir_for_c_step (aux step)) @
      (* body *)
      (add_dir Dir_body (aux_body body)) @
      if incontracts then
        Option.flat_map (fun invariant -> aux_contract_dir Contract_invariant invariant) invariant
      else []
    | Trm_while (cond, body) ->
      (* cond *)
      (add_dir Dir_cond (aux cond)) @
      (* body *)
      (add_dir Dir_body (aux_body body))
    | Trm_do_while (body, cond) ->
      (* body *)
      (add_dir Dir_body (aux_body body)) @
      (* cond *)
      (add_dir Dir_cond (aux cond))
    | Trm_if (cond, then_t, else_t) ->
      (* cond *)
      (add_dir Dir_cond (aux cond)) @
      (* then *)
      (add_dir Dir_then (aux_body then_t)) @
      (* else *)
      (add_dir Dir_else (aux_body else_t))
    | Trm_apps (f, args, ghost_args) ->
      (* fun *)
      (add_dir Dir_app_fun (aux f)) @
      (* args *)
      (explore_list args (fun n -> Dir_arg_nth n) (aux)) @
      (* ghost args *)
      (explore_list ghost_args (fun n -> Dir_ghost_arg_nth n) (fun (g, t) -> aux t))
    | Trm_seq (tl, _) ->
      explore_list (Mlist.to_list tl) (fun n -> Dir_seq_nth n) (aux)
    | Trm_switch (cond, cases) ->
      (add_dir Dir_cond (aux cond)) @
      (List.fold_lefti (fun i epl case -> epl@explore_case ~incontracts depth i case p) [] cases)
    | Trm_namespace (name, body, inline) ->
      add_dir Dir_namespace (aux body)
    | Trm_prim (ty, _) ->
      if incontracts then add_dir Dir_type (aux ty) else []
    | _ ->
      Flags.verbose_warn loc "explore_in_depth: cannot find a subterm to explore";
      []
    end

(*
  call resolve_target_simple on given case name and body
  i is the index of the case in its switch
 *)
(** [explore_case ~incontracts depth i case p]: calls resolve_target_simple on given case name and body,
    [i] is the index of the case in its switch satement *)
and explore_case ~(incontracts:bool) (depth : depth) (i : int) (case : trms * trm) (p : target_simple) : paths =
  let aux = resolve_target_simple ~incontracts ~depth p in
  let (tl, body) = case in
  match tl with
  (* default case *)
  | [] ->
     add_dir (Dir_case (i, Case_body)) (aux body)
  | _ ->
     (List.fold_lefti
        (fun j epl t ->
          epl @
          (add_dir (Dir_case (i, Case_name j)) (aux t))
        )
        []
        tl
     ) @
     add_dir (Dir_case (i, Case_body)) (aux body)

(** [follow_dir aux d t]: follows the direction [d] in [t] and call [aux] *)
and follow_dir (aux:trm->paths) (d : dir) (t : trm) : paths =
  let loc = t.loc in
  match d, t.desc with
  | Dir_before _, _ ->
      loc_fail loc "follow_dir: Dir_before should not remain at this stage"
  | Dir_span _, _ ->
      loc_fail loc "follow_dir: Dir_span should not remain at this stage"
  | Dir_seq_nth n, Trm_seq (tl, _) ->
    app_to_nth_dflt (Mlist.to_list tl) n
       (fun nth_t -> add_dir (Dir_seq_nth n) (aux nth_t))
  | Dir_cond, Trm_if (cond, _, _)
    | Dir_cond, Trm_while (cond, _)
    | Dir_cond, Trm_do_while (_, cond)
    | Dir_cond, Trm_for_c (_, cond, _, _, _)
    | Dir_cond, Trm_switch (cond, _) ->
     add_dir Dir_cond (aux cond)
  | Dir_then, Trm_if (_, then_t, _) ->
     add_dir Dir_then (aux then_t)
  | Dir_else, Trm_if (_, _, else_t) ->
     add_dir Dir_else (aux else_t)
  | Dir_var_body, Trm_let (_, body) ->
     let ref_op_arg = ref_operation_arg body in
     add_dir Dir_var_body (aux ref_op_arg)
  | Dir_let_body, Trm_let (_, body) ->
    add_dir Dir_let_body (aux body)
  | Dir_body, Trm_fun (_, _, body, _)
  | Dir_body, Trm_for_c (_, _, _, body, _)
  | Dir_body, Trm_for (_, body, _)
  | Dir_body, Trm_while (_, body)
  | Dir_body, Trm_do_while (body, _)
  | Dir_body, Trm_abort (Ret (Some body)) ->
     add_dir Dir_body (aux body)
  | Dir_for_c_init, Trm_for_c (init, _, _, _, _) ->
     add_dir Dir_for_c_init (aux init)
  | Dir_for_c_step, Trm_for_c (_, _, step, _, _) ->
     add_dir Dir_for_c_step (aux step)
  | Dir_for_start, Trm_for (range, _, _) ->
     add_dir Dir_for_start (aux range.start)
  | Dir_for_stop, Trm_for (range, _, _) ->
     add_dir Dir_for_stop (aux range.stop)
  | Dir_app_fun, Trm_apps (f, _, _) -> add_dir Dir_app_fun (aux f)
  | Dir_arg_nth n, Trm_apps (_, tl, _) ->
     app_to_nth_dflt tl n (fun nth_t ->
         add_dir (Dir_arg_nth n) (aux nth_t))
  | Dir_arg_nth n, Trm_fun (arg, _, _, _) ->
     let tl = List.map (fun (x, _) -> trm_var ?loc x) arg in
     app_to_nth_dflt tl n (fun nth_t ->
         add_dir (Dir_arg_nth n) (aux nth_t))
  | Dir_name, Trm_typedef td ->
    add_dir Dir_name (aux (trm_var ?loc td.typedef_name))
  | Dir_name, Trm_let ((x,_),_) ->
    add_dir Dir_name (aux (trm_var ?loc x))
  | Dir_name, Trm_goto x ->
    (* CHECK: #var-id-dir-name , is this correct? *)
    add_dir Dir_name (aux (trm_var ?loc (name_to_var x)))
  | Dir_type, Trm_let ((_,t), _) ->
    add_dir Dir_type (aux t)
  | Dir_type, Trm_prim (t, _) ->
    add_dir Dir_type (aux t)
  | Dir_case (n, cd), Trm_switch (_, cases) ->
     app_to_nth_dflt cases n
       (fun (tl, body) ->
         match cd with
         | Case_body ->
            add_dir (Dir_case (n, cd)) (aux body)
         | Case_name i ->
            app_to_nth_dflt tl i (fun ith_t ->
                add_dir (Dir_case (n, cd)) (aux ith_t))
       )
  | Dir_enum_const (n, ecd), Trm_typedef td ->
     begin match td.typedef_body with
     | Typedef_enum xto_l ->
          app_to_nth_dflt xto_l n
          (fun (x, t_o) ->
            match ecd with
            | Enum_const_name ->
               add_dir (Dir_enum_const (n, ecd)) (aux (trm_var ?loc x))
            | Enum_const_val ->
               begin match t_o with
               | None ->
                  Flags.verbose_warn loc "follow_dir: no value for constant of index %d"
                    n;
                  []
               | Some t ->
                  add_dir (Dir_enum_const (n, ecd)) (aux t)
               end
          )
      | _ -> []
      end
  | Dir_record_member i, Trm_typedef td ->
    begin match td.typedef_body with
    | Typedef_record rfl ->
      app_to_nth_dflt rfl i (fun (rf, rf_ann) ->
        begin match rf with
        | Record_method t1 ->
          add_dir (Dir_record_member i) (aux t1)
        | _ -> loc_fail loc "follow_dir: wrong field index"
        end
      )
    | _ -> []
    end
  | Dir_namespace, Trm_namespace (name, body, inline) ->
    add_dir Dir_namespace (aux body)
  | _, _ ->
     Flags.verbose_warn loc "follow_dir: direction %s does not match"
       (dir_to_string d);
     []



(******************************************************************************)
(*                          Target-between resolution                         *)
(******************************************************************************)

(** [resolve_target_between tg t]: resolves a target that points before or after an instruction *)
let resolve_target_between (tg : target) (t : trm) : (path * int) list =
  List.map extract_last_dir_before (resolve_target tg t)

(** [resolve_target_between_exactly_one tg t]: similar to [resolve_target_between] but this one fails if the target matches
    multiple paths *)
let resolve_target_between_exactly_one (tg : target) (t : trm) : path * int =
  match resolve_target_between tg t with
  | [(p,i)] -> (p,i)
  | _ -> trm_fail t "Constr.resolve_target_between_exactly_one: target resolved to multiple paths"

(** [resolve_target_span tg t]: resolves a target that points to a span of instruction *)
let resolve_target_span (tg: target) (t: trm) : (path * span) list =
  List.map extract_last_dir_span (resolve_target tg t)

(** [resolve_target_span_exactly_one tg t]: similar to [resolve_target_span] but this one fails if the target matches
    multiple paths *)
let resolve_target_span_exactly_one (tg: target) (t: trm) : path * span =
  match resolve_target_span tg t with
  | [(p,s)] -> (p,s)
  | _ -> trm_fail t "Constr.resolve_target_span_exactly_one: target resolved to multiple paths"
