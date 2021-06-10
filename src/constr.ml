open Ast
open Ast_to_c
open Str
open Tools
open Path


(******************************************************************************)
(*                        Data structure for targets                          *)
(******************************************************************************)

(* Type to classify trms into four main classes: 1)Structuring statements, 2) Instructions 3) Expression and others*)
type trm_kind =
  | TrmKind_Struct
  | TrmKind_Instr
  | TrmKind_Expr
  | TrmKind_Any

type rexp = {
  rexp_desc : string; (* printable version of regexp *)
  rexp_exp : regexp;
  rexp_substr : bool;
  rexp_trm_kind : trm_kind }

(* Relative target to term *)
type target_relative =
    | TargetAt
    | TargetFirst
    | TargetLast
    | TargetBefore
    | TargetAfter

(* Number of targets to match *)
type target_occurences =
    | ExpectedOne  (* = 1 occurence, the default value *)
    | ExpectedNb of int  (* exactly n occurrences *)
    | ExpectedMulti  (* > 0 number of occurrences *)
    | ExpectedAnyNb  (* any number of occurrences *)

(* A [target] is a list of constraints to identify nodes of the AST
   that we require the result path to go through. *)
type target = constr list

and constr =
  (*
    target constraints:
    - directions to follow
    - list constraint: a constraint is matched against all elements of the list
      used for seq elements and function arguments
      the user provides a function that, given the results of this constraint
      matching, returns the ranks of the elements to explore next
    - include file to explore
   *)
  | Constr_strict
  | Constr_dir of dir
  | Constr_include of string
  (*
    matching constraint: match against regexp
    the user may also match against a string through smart constructors
   *)
  (* todo: beware of multiline matching *)
  | Constr_regexp of rexp
  (*
    node related constraints (constraints are expressed using a target):
    - for loop: constraints on the initialisation, the condition, the step
    instruction and the body
    - while loop: constraints on the condition and the body
    - if statement: constraints on the condition and each branch
    - decl_var: constraints on the name and on the initialisation
    - decl_fun: constraints on the name, the arguments and the body
    - decl_type: constraint on the name
    - decl_enum: constraints on the name and the constants
    - seq: constraint matched against each all elements of a seq
      depending on the result, a function decides whether the constraint holds
    - var: constraint on the name
    - lit: literal
    - app: constraints on the function and on the list of arguments
    - label: constraints on the label and on the term
    - goto: constraint on the label
    - return: constraint on the returned value
    - abort: matches abort nodes (return, break and continue)
    - accesses: constraints on a succession of accesses + on the base
    - switch: constraints on the condition and on the cases
   *)
  (* for: init, cond, step, body *)
  | Constr_for of target * target * target * target
  (* while: cond, body *)
  | Constr_while of target * target
  (* if: cond, then, else *)
  | Constr_if of target * target * target
  (* decl_var: name, body *)
  | Constr_decl_var of constr_name * target
  (* decl_fun: name, args, body *)
  | Constr_decl_fun of constr_name * target_list_pred * target
  (* decl_type: name *)
  | Constr_decl_type of constr_name
  (* decl_enum: name, constants *)
  | Constr_decl_enum of constr_name * constr_enum_const
  (* seq *)
  | Constr_seq of target_list_pred
  (* var *)
  | Constr_var of constr_name
  (* lit *)
  | Constr_lit of lit
  (* app: function, arguments *)
  | Constr_app of target * target_list_pred
  (* label *)
  | Constr_label of constr_name * target
  (* goto *)
  | Constr_goto of constr_name
  (* return *)
  | Constr_return of target
  (* abort *)
  | Constr_abort of abort_kind
  (* accesses: base, accesses *)
  | Constr_access of target * constr_accesses
  (* switch: cond, cases *)
  | Constr_switch of target * constr_cases
  (* TODO: Constraint for types? *)
  (* Target relative to another trm *)
  | Constr_relative of target_relative
  (* Number of  occurrences expected  *)
  | Constr_occurences of target_occurences
  (* List of constraints *)
  | Constr_chain of constr list
  (* Constraint used for argument match *)
  | Constr_bool of bool
  (* Constraint that matches only the root of the AST *)
  | Constr_root
  (* LATER: add Constr_or, Constr_and, Constr_not *)

(* Names involved in constraints, e.g. for goto labels *)
and constr_name = rexp option

and constr_enum_const = ((constr_name * target) list) option

and constr_accesses = (constr_access list) option

(* Predicate for expressing constraints over a list of subterms. It consists of:
   - a function that produces the constraints for the i-th subterm
   - a function that takes a list of booleans indicating which of the subterms
     matched their respective constraint, and returns a boolean indicating whether
     the full list should be considered as matching or not.
   - a string that explains what was the user intention *)
and target_list_pred =
  { target_list_pred_ith_constr : int -> constr;
    target_list_pred_validate : bool list -> bool;
    target_list_pred_to_string : unit -> string; }

and constr_access =
  (* array indices may be arbitrary terms *)
  | Array_access of target
  (* struct fields are strings *)
  | Struct_access of constr_name
  | Any_access

(* for each case, its kind and a constraint on its body *)
and constr_cases = ((case_kind * target) list) option

and case_kind =
  (* case: value *)
  | Case_val of target
  | Case_default
  | Case_any

and abort_kind =
  | Any
  | Return
  | Break
  | Continue

(* [target_simple] is a [target] without Constr_relative, Constr_occurences, Constr_chain;
   It can however, include [cStrict]. *)
type target_simple = target

(* [target_struct] is the structured representation of a [target] that decomposes the
   special constructors such as Constr_relative, Constr_occurences, Constr_chain from the
   [target_simple]. *)
type target_struct = {
   target_path : target_simple; (* this path contains no cMulti/cNb/cBefore/etc.., only cStrict can be there *)
   target_relative : target_relative;
   target_occurences : target_occurences; }

let make_target_list_pred ith_constr validate to_string =
  { target_list_pred_ith_constr = ith_constr;
    target_list_pred_validate = validate;
    target_list_pred_to_string = to_string; }


(******************************************************************************)
(*                        Pretty-printing of targets                          *)
(******************************************************************************)

let trm_kind_to_string (k : trm_kind) : string =
  match k with
  | TrmKind_Instr -> "Instr"
  | TrmKind_Struct -> "Struct"
  | TrmKind_Expr -> "Expr"
  | TrmKind_Any -> "Any"

let regexp_to_string (r : rexp) : string =
  (if r.rexp_substr then "Sub" else "Exact") ^ "-" ^
  (trm_kind_to_string r.rexp_trm_kind) ^
  "(" ^ r.rexp_desc ^ ")"

let rec constr_to_string (c : constr) : string =
  match c with
  | Constr_strict -> "Strict"
  | Constr_dir d -> dir_to_string d
  (* | Constr_list (p_elt, _) -> "List (" ^ target_to_string p_elt ^ ")" *)
  | Constr_include s -> "Include " ^ s
  | Constr_regexp r -> "Regexp " ^ regexp_to_string r
  | Constr_for (p_init, p_cond, p_step, p_body) ->
     let s_init = target_to_string p_init in
     let s_cond = target_to_string p_cond in
     let s_step = target_to_string p_step in
     let s_body = target_to_string p_body in
     "For (" ^ s_init ^ ", " ^ s_cond ^ ", " ^ s_step ^ ", " ^ s_body ^ ")"
  | Constr_while (p_cond, p_body) ->
     let s_cond = target_to_string p_cond in
     let s_body = target_to_string p_body in
     "While (" ^ s_cond ^ ", " ^ s_body ^ ")"
  | Constr_if (p_cond, p_then, p_else) ->
     let s_cond = target_to_string p_cond in
     let s_then = target_to_string p_then in
     let s_else = target_to_string p_else in
     "If (" ^ s_cond ^ ", " ^ s_then ^ ", " ^ s_else ^ ")"
  | Constr_decl_var (name, p_body) ->
     let s_name =
       match name with | None -> "_" | Some r -> regexp_to_string r
     in
     let s_body = target_to_string p_body in
     "Decl_var (" ^ s_name ^ ", " ^ s_body ^ ")"
  | Constr_decl_fun (name, _tgt_list_pred, p_body) ->
    let s_name =
       match name with | None -> "_" | Some r -> regexp_to_string r
     in
     let spred = _tgt_list_pred.target_list_pred_to_string() in
     let s_body = target_to_string p_body in
     "Decl_fun (" ^ s_name ^ spred ^ s_body ^ ")"

  | Constr_decl_type name ->
     let s_name =
       match name with | None -> "_" | Some r -> regexp_to_string r
     in
     "Decl_type " ^ s_name
  | Constr_decl_enum (name, c_const) ->
     let s_name =
       match name with | None -> "_" | Some r -> regexp_to_string r
     in
     let s_const =
       match c_const with
       | None -> "_"
       | Some np_l ->
          let sl =
            List.map
              (fun (n, p) ->
                let s_n =
                  match n with | None -> "_" | Some r -> regexp_to_string r
                in
                let s_p = target_to_string p in
                "(" ^ s_n ^ ", " ^ s_p ^ ")"
              )
              np_l
          in
          list_to_string sl
     in
     "Decl_enum (" ^ s_name ^ ", " ^ s_const ^ ")"
  | Constr_seq tgt_list_pred ->
     let spred = tgt_list_pred.target_list_pred_to_string() in
     sprintf "Seq (%s)" spred
  | Constr_var name ->
     "Var " ^ (match name with | None -> "_" | Some r -> regexp_to_string r)
  | Constr_lit l ->
     let s =
       begin match l with
       | Lit_unit ->  "()"
       | Lit_uninitialized -> "?"
       | Lit_bool b -> string_of_bool b
       | Lit_int n -> string_of_int n
       | Lit_double d -> string_of_float d
       | Lit_string s -> s
       end
     in "Lit " ^ s
  | Constr_app (p_fun,tgt_list_pred) ->
    let spred = tgt_list_pred.target_list_pred_to_string() in
    let s_fun = target_to_string p_fun in
    "App (" ^ s_fun ^ ", " ^ spred ^ ")"
  | Constr_label (so, p_body) ->
     let s_label =
       match so with | None -> "_" | Some r -> regexp_to_string r
     in
     let s_body = target_to_string p_body in
     "Label (" ^ s_label ^ ", " ^ s_body ^ ")"
  | Constr_goto so ->
     let s_label =
       match so with | None -> "_" | Some r -> regexp_to_string r
     in
     "Goto " ^ s_label
  | Constr_return p_res ->
      let s_res = target_to_string p_res in
      "Return " ^ s_res
  | Constr_abort kind ->
     let s_kind =
       match kind with
       | Any -> "Any"
       | Return -> "Return"
       | Break -> "Break"
       | Continue -> "Continue"
     in
     "Abort_" ^ s_kind
  | Constr_access (p_base, ca) ->
     let s_accesses =
       match ca with
       | None -> "_"
       | Some cal -> list_to_string (List.map access_to_string cal)
     in
     let s_base = target_to_string p_base in
     (* let s = target_to_string p_elt in *)
     "Access (" ^ s_accesses ^ ", " ^ s_base ^ ")"
  | Constr_switch (p_cond, cc) ->
     let s_cond = target_to_string p_cond in
     let s_cases =
       match cc with
       | None -> "_"
       | Some cl ->
          let string_of_kind = function
            | Case_val p_val -> "Case_val " ^ target_to_string p_val
            | Case_default -> "Default"
            | Case_any -> "Any"
          in
          let sl =
            List.map
              (fun (k, p_body) ->
                let s_body = target_to_string p_body in
                "(" ^ string_of_kind k ^ ", " ^ s_body ^ ")"
              )
              cl
          in
          list_to_string sl
     in
     "Switch (" ^ s_cond ^ ", " ^ s_cases ^ ")"
  | Constr_relative tr -> target_relative_to_string tr
  | Constr_occurences oc -> target_occurences_to_string oc
  | Constr_chain cl ->
    let string_cl = List.map constr_to_string cl in
    list_to_string string_cl
  | Constr_bool b -> if b then "True" else "False"
  | Constr_root -> "Root"

and target_to_string (tg : target) : string =
  list_to_string (List.map constr_to_string tg)

(* TODO: later rename the fileds of target_struct *)

and target_struct_to_string (tgs : target_struct) : string =
  "TargetStruct(" ^
    target_relative_to_string tgs.target_relative ^ ", " ^
    target_occurences_to_string tgs.target_occurences ^ ", " ^
    target_to_string tgs.target_path ^ ")"

and target_occurences_to_string (occ : target_occurences) =
  match occ with
  | ExpectedOne -> "ExpectedOne"
  | ExpectedNb n -> sprintf "ExpectedNb(%d)" n
  | ExpectedMulti -> "ExpectedMulti"
  | ExpectedAnyNb -> "ExpectedAnyNb"

and target_relative_to_string (rel : target_relative) =
  match rel with
  | TargetAt -> "TargetAt"
  | TargetFirst -> "TargetFirst"
  | TargetLast -> "TargetLast"
  | TargetBefore -> "TargetBefore"
  | TargetAfter -> "TargetAfter"

and access_to_string (ca : constr_access) : string =
  match ca with
  | Array_access p_index ->
     let s_index = target_to_string p_index in
     "Array_access " ^ s_index
  | Struct_access so ->
     let s_field =
       match so with | None -> "_" | Some r -> regexp_to_string r
     in
     "Struct_access " ^ s_field
  | Any_access -> "Any_access"



(******************************************************************************)
(*                        Preprocessing of targets before resolution          *)
(******************************************************************************)

(* Flatten all the constrainst of type Constr_chain *)
let target_flatten (tg : target) : target =
    let rec aux (cs : target) : target =
      match cs with
      | [] -> []
      | c::cs2 ->
          let r = match c with
            | Constr_chain cs1 -> (aux cs1)
            | _ -> [c]
            in
          r @ (aux cs2)
      in
    aux tg

(* Convert a target into a target struct  *)
let target_to_target_struct (tr : target) : target_struct =
  let tr = target_flatten tr in
  let relative = ref None in
  let occurences = ref None in
  let process_constr (c : constr) : unit =
    match c with
    | Constr_relative re ->
      begin match !relative with
      | None -> relative := Some re;
      | Some _ -> fail None  "Constr_relative provided twice in path"
      end
    | Constr_occurences oc ->
      begin match !occurences with
      | None -> occurences := Some oc;
      | _ -> fail None "Constr_occurences provided twice in path"
      end
    | _ -> ()
    in
  (* Check if relative constraint are applied once and the number of occurences is unique *)
  List.iter process_constr tr;
  (* Return a target_struct *)
  let tgs = {
    target_path = List.filter (function | Constr_relative _ | Constr_occurences _ -> false | _ -> true) tr;
    target_relative = begin match !relative with | None -> TargetAt | Some re -> re end;
    target_occurences = begin match !occurences with | None -> ExpectedOne | Some oc -> oc end; } in
  (* TODO *)
  (* printf "%s\n" (target_struct_to_string tgs); *)
  tgs


(******************************************************************************)
(*                              Target resolution                             *)
(******************************************************************************)

(*
  Particular case for target resolution: heap allocated variables
  Patterns:
    - declaration: seq annotated with Heap_allocated containing decl +
      optional initialisation annotated with Initialisation_instruction
    - usage: particular use of get/access because variables are pointers
      in practice, only dereferencing has to be taken into account: array and
      struct get/access are better matched with regexp
    - elimination:
      + in return instruction: seq annotated with Delete_instructions containing
        delete instructions annotated with Heap_allocated + abort instruction
      + at the end of scopes: seq annotated with Delete_instructions containing
        last instruction of the scope + annotated delete instructions
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


(* compare literals *)
let is_equal_lit (l : lit) (l' : lit) =
  match l, l' with
  | Lit_unit, Lit_unit -> true
  | Lit_uninitialized, Lit_uninitialized -> true
  | Lit_bool b, Lit_bool b' when b = b' -> true
  | Lit_int n, Lit_int n' when n = n' -> true
  | Lit_double d, Lit_double d' when d = d' -> true
  | Lit_string s, Lit_string s' when s = s' -> true
  | _ -> false

let get_trm_kind (t : trm) : trm_kind =
  if t.is_statement then
    match t.desc with
    | Trm_struct _ | Trm_array _ | Trm_let _ | Trm_let_fun _ | Trm_typedef _  | Trm_if (_,_,_) | Trm_seq _ | Trm_while (_,_)
    | Trm_for (_,_,_,_) | Trm_switch (_,_) -> TrmKind_Struct
    | _ -> TrmKind_Instr
  else
    TrmKind_Expr
(* Not used anywhere?? *)
let is_structuring_statement (t : trm) : bool =
  get_trm_kind t = TrmKind_Struct


let match_regexp_str (r : rexp) (s : string) : bool =
  (*if s = "x" then incr Debug.counter;
  if !Debug.counter = 2 then raise Debug.Breakpoint; *)
  if r.rexp_substr then begin
    try let _ = Str.search_forward r.rexp_exp s 0 in true
    with Not_found -> false
  end else begin
    Str.string_match r.rexp_exp s 0
  end

let match_regexp_trm (r : rexp) (t : trm) : bool =
  (* DEBUG: *) (* printf "match_regexp_trm(%s, %s)\n" (regexp_to_string r) (Ast_to_c.ast_to_string t); *)
  (* DEBUG: *) (* printf "%s vs %s\n" (trm_kind_to_string r.rexp_trm_kind) (trm_kind_to_string (get_trm_kind t)); *)
  if r.rexp_trm_kind <> get_trm_kind t && r.rexp_trm_kind <> TrmKind_Any
    then false
    else match_regexp_str r (ast_to_string t)

let is_constr_regexp (c : constr) : bool =
  match c with | Constr_regexp _ -> true | _ -> false

(* check if constraint c is satisfied by trm t *)
let rec check_constraint (c : constr) (t : trm) : bool =
  (* LATER: find if it is find to deactivate these encodings *)
  match t.annot with
  (* | Some Heap_allocated | Some Delete_instructions -> *)
     (* if t is one of the heap allocation patterns, we simplify it before *)
     (* check_constraint c (forget_heap_alloc t) *)
  | Some Access ->
     (* forget the star operator at the root before checking the constraint *)
     begin match t.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t']) ->
        check_constraint c t'
     | _ -> fail t.loc "check_constraint: bad access annotation"
     end
  | Some Multi_decl ->
     (*
       check the constraint on each element of the seq and return true if one
       is true
      *)
     begin match t.desc with
     | Trm_seq tl -> List.mem true (List.map (check_constraint c) tl)
     | _ -> fail t.loc "check_constraint: bad multi_decl annotation"
     end
  | _ ->

     let loc = t.loc in
     begin match c, t.desc with
     (*
       target constraints never hold since they are checked against nodes before
       calling check_constraint in resolve_target
      *)
      | Constr_strict,_
       | Constr_dir _, _
       (* | Constr_list _, _ *)
       | Constr_include _, _ ->
        false
     | Constr_regexp r, _ -> match_regexp_trm r t
     | Constr_for (p_init, p_cond, p_step, p_body),
       Trm_for (init, cond, step, body) ->
        check_target p_init init &&
        check_target p_cond cond &&
        check_target p_step step &&
        check_target p_body body
     | Constr_while (p_cond, p_body), Trm_while (cond, body) ->
        check_target p_cond cond &&
        check_target p_body body
     | Constr_if (p_cond, p_then, p_else), Trm_if (cond, then_t, else_t) ->
        check_target p_cond cond &&
        check_target p_then then_t &&
        check_target p_else else_t
      | Constr_decl_var (name, p_body) , Trm_let (_,(x,_), body) ->
        check_name name x &&
        check_target p_body body
     | Constr_decl_fun (name, cl_args, p_body),
       Trm_let_fun (x, _, args, body) ->
        let tl = List.map (fun (x, _) -> trm_var ~loc x) args in
        check_name name x &&
        check_list cl_args tl &&
        check_target p_body body
     | Constr_decl_type name, Trm_typedef (Typedef_abbrev (x, _)) ->
        check_name name x
     | Constr_decl_enum (name, cec), Trm_typedef (Typedef_enum (n, xto_l)) ->
        check_name name n &&
        check_enum_const cec xto_l
     | Constr_seq cl, Trm_seq tl ->
        check_list cl tl
     | Constr_var name, Trm_var x ->
        check_name name x
     | Constr_lit l, Trm_val (Val_lit l') ->
        is_equal_lit l l'
     | Constr_app ((*accepted_encoded*) p_fun, cl_args), Trm_apps (f, args) ->
        (*(accepted_encoded || not (is_encoded_fun f)) && ... *)
        (*  where [is_encoded_fun f] returns true when [f] is [unop_get] or [unop_new] or similar *)
        check_target p_fun f &&
        check_list cl_args args
     | Constr_label (so, p_body), Trm_labelled (l, body) ->
        check_name so l &&
        check_target p_body body
     | Constr_goto so, Trm_goto l ->
        check_name so l
     | Constr_return p_res, Trm_abort (Ret (Some res)) ->
        check_target p_res res
     | Constr_abort Any, Trm_abort _ -> true
     | Constr_abort Return, Trm_abort (Ret _) -> true
     | Constr_abort Break, Trm_abort Break -> true
     | Constr_abort Continue, Trm_abort Continue -> true
     | Constr_access (p_base, ca), _ ->
        let (base, al) = compute_accesses t in
        check_target p_base base &&
        check_accesses ca al
     | Constr_switch (p_cond, cc), Trm_switch (cond, cases) ->
        check_target p_cond cond &&
        check_cases cc cases
     | Constr_bool b, _ -> b
     | Constr_root, _ ->
        begin match t.annot with Some Main_file -> true | _ -> false end
     | _ -> false
     end

and check_name (name : constr_name) (s : string) : bool =
  match name with
  | None -> true
  | Some r -> match_regexp_str r s

and check_list (lpred : target_list_pred) (tl : trm list) : bool =
  (* DEBUG: printf "%s\n" (lpred.target_list_pred_to_string()); *)
  let cstr = lpred.target_list_pred_ith_constr in
  let validate = lpred.target_list_pred_validate in
  validate (List.mapi (fun i t -> check_target ([cstr i]) t) tl)
  (* DEBUG: printf "%s\n" (if res then "true" else "false"); *)

(* and check_list (cl : constr_list) (tl : trm list) : bool =
  let (p, validate) = cl in
  validate (List.map (check_target p) tl) *)

and check_accesses (ca : constr_accesses) (al : trm_access list) : bool =
  let rec aux (cal : constr_access list) (al : trm_access list) : bool =
    match cal, al with
    | [], [] -> true
    | Array_access p_index :: cal, Array_access index :: al ->
       check_target p_index index &&
       aux cal al
    | Struct_access so :: cal, Struct_access f :: al ->
       check_name so f &&
       aux cal al
    | Any_access :: cal, _ :: al -> aux cal al
    | _ -> false
  in
  match ca with
  | None -> true
  | Some cal -> aux cal al

and check_cases (cc : constr_cases) (cases : (trm list * trm) list) : bool =
  let rec aux (cl : (case_kind * target) list)
    (cases : (trm list * trm) list) : bool =
    match cl, cases with
    | [], [] -> true
    | (k, p_body) :: cl, (tl, body) :: cases ->
       check_kind k tl &&
       check_target p_body body &&
       aux cl cases
    | _ -> false
  in
  match cc with
  | None -> true
  | Some cl -> aux cl cases

and check_kind (k : case_kind) (tl : trm list) : bool =
  match k, tl with
  | Case_any, _ -> true
  | Case_default, [] -> true
  | Case_val p_val, _ ->
     (* check that one of the cases corresponds to the given target *)
     List.mem true (List.map (check_target p_val) tl)
  | _ -> false

and check_enum_const (cec : constr_enum_const)
  (xto_l : (string * (trm option)) list) : bool =
  match cec with
  | None -> true
  | Some cnp_l ->
     List.for_all2
       (fun (cn, p) (n, t_o) ->
         check_name cn n &&
         match p, t_o with
         | [], None -> true
         | _, None -> false
         | _, Some t -> check_target p t
       )
       cnp_l
       xto_l

(* check if target tr leads to at least one subterm of t *)
and check_target (tr : target) (t : trm) : bool =
  match resolve_target_simple tr t with
  | [] -> false
  | _ -> true

(*
  resolve_target computes the directions to matching subterms
  expected invariants: no duplicate target and no target which is prefix of
  another target that appears after it in the list. Guaranteed by the call to
  sort_unique
 *)
(* Problem is comming from this function *)
and resolve_target_simple ?(strict : bool = false) (trs : target_simple) (t : trm) : paths =
  let epl =
    match trs with
    | [] -> [[]]
    | Constr_strict :: tr -> resolve_target_simple ~strict:true tr t
    | c :: p ->
      let res_deep =
        if strict
           then [] (* in strict mode, must match c here *)
           else (explore_in_depth (c :: p) t) in
      let res_here =
         if is_constr_regexp c && res_deep <> []
           then [] (* if a regexp matches in depth, don't test it here *)
           else (resolve_constraint c p t) in

      (* DEBUG *)
        (* printf "resolve_target_simple\n  ~strict:%s\n  ~target:%s\n  ~term:%s\n ~deep:%s\n  ~here:%s\n"
          (if strict then "true" else "false")
          (target_to_string trs)
          (Ast_to_c.ast_to_string ~ast_decode:false t)
          (paths_to_string ~sep:"\n   " res_deep)
          (paths_to_string ~sep:"\n   " res_here); *)


      res_deep ++ res_here  (* put deeper nodes first *) in
  List.sort_uniq compare_path epl

and resolve_target_struct (tgs : target_struct) (t : trm) : paths =
  let res = resolve_target_simple tgs.target_path t in
  let nb = List.length res in
  (* Check if nb is equal to the specification of tgs.target_occurences, if not then something went wrong *)
  (* TODO: one day, report the location from the OCaml file where the target is coming from;
     the idea would be to track the OCaml line of code form which users write the target *)
  (* TODO: insert to the head of a line
     (ocamlpos:=__LOC__); Tr.transfo [path] *)
  begin match tgs.target_occurences with (* TODO: use sprintf everywhere *)
  | ExpectedOne -> if nb <> 1 then fail None (sprintf "resolve_target_struct: expected exactly one match, got %d." nb)
  | ExpectedNb n -> if nb <> n then fail None (sprintf "resolve_target_struct: expected %d matches, got %d." n nb)
  | ExpectedMulti -> if nb = 0 then fail None (sprintf "resolve_target_struct: expected at least one occurrence, got %d." nb)
  | ExpectedAnyNb -> ();
  end;
  res

and resolve_target (tg : target) (t : trm) : paths =
  let tgs = target_to_target_struct tg in
  if tgs.target_relative <> TargetAt
    then fail None "resolve_target: this target should not contain a cBefore/cAfter/cFirst/cLast";
  resolve_target_struct tgs t

and resolve_target_exactly_one (tg : target) (t : trm) : path =
  match resolve_target tg t with
  | [p] -> p
  | _ -> fail None (* TODO: loc? *) "resolve_target_exactly_one: obtained several targets."

(* check c against t and in case of success continue with p *)
and resolve_constraint (c : constr) (p : target_simple) (t : trm) : paths =
  let loc = t.loc in
  match c with
  (*
    do not resolve in included files, except if the constraint is Constr_include
   *)
  | Constr_include h when t.annot = Some (Include h) ->
     (*
       remove the include annotation for target resolution to proceed in the
       included file
      *)
     resolve_target_simple p {t with annot = None}
  | _ when is_included t ->
     print_info loc "resolve_constraint: not an include constraint\n";
     []
  (* target constraints first *)
  (* following directions *)
  | Constr_dir d -> follow_dir d p t
  (*
    if the constraint is a target constraint that does not match the node or
    if it is another kind of constraint, then we check if it holds
   *)
  | c when check_constraint c t -> resolve_target_simple p t
  | _ ->
     print_info loc "resolve_constraint: constraint %s does not hold\n"
       (constr_to_string c);
     []

(* call resolve_target_simple on subterms of t if possible *)
and explore_in_depth (p : target_simple) (t : trm) : paths =
  (* let p = target_to_target_simple p in ---TODO: used for getting rid of Constr_chain that appear in depth *)
  let loc = t.loc in
  match t.annot with
  (* no exploration in depth in included files *)
  | Some (Include _) ->
     print_info loc "explore_in_depth: no exploration in included files\n";
     []

  | Some Access ->
     begin match t.desc with
       (*
         the wildcard is a star operator the user doesn't know about
         t' is an access under which want to explore
        *)
     | Trm_apps (_, [t']) -> add_dir (Dir_arg 0) (explore_in_depth p t')
     | _ -> fail loc "explore_in_depth: bad access annotation"
     end
  | Some Multi_decl ->
     (* explore each declaration in the seq *)
     begin match t.desc with
     | Trm_seq tl ->
        explore_list tl (fun i -> Dir_nth i) (explore_in_depth p)
     | _ -> fail loc "explore_in_depth: bad multi_decl annotation"
     end
  | _ ->
     begin match t.desc with
     | Trm_let (_ ,(_, _), body) ->
       add_dir Dir_body (resolve_target_simple p body)
     | Trm_let_fun (_, _ ,_ ,body) ->
        (* DEPRECATED: the name of the function should not be considered an occurence;
            add_dir Dir_name (resolve_target_simple p (trm_var ~loc x)) ++ *)
        add_dir Dir_body (resolve_target_simple p body)
     |Trm_typedef (Typedef_enum (x, xto_l)) ->
        let (il, tl) =
          foldi
            (fun n (il, tl) (_, t_o) ->
              match t_o with
              | None -> (il, tl)
              | Some t -> (il ++ [n], tl ++ [t])
            )
           ([], [])
           xto_l
        in
        add_dir Dir_name (resolve_target_simple p (trm_var ~loc x)) ++
        (explore_list (List.map (fun (y, _) -> trm_var ~loc y) xto_l)
           (fun n -> Dir_enum_const (n, Enum_const_name))
           (resolve_target_simple p)) ++
        (explore_list tl
           (fun n -> Dir_enum_const (List.nth il n, Enum_const_val))
           (resolve_target_simple p))
     | Trm_abort (Ret (Some body)) ->
        add_dir Dir_body (resolve_target_simple p body)
     | Trm_for (init, cond, step, body) ->
        (* init *)
        (add_dir Dir_for_init (resolve_target_simple p init)) ++
        (* cond *)
        (add_dir Dir_cond (resolve_target_simple p cond)) ++
        (* step *)
        (add_dir Dir_for_step (resolve_target_simple p step)) ++
        (* body *)
        (add_dir Dir_body (resolve_target_simple p body))
     | Trm_while (cond, body) ->
        (* cond *)
        (add_dir Dir_cond (resolve_target_simple p cond)) ++
        (* body *)
        (add_dir Dir_body (resolve_target_simple p body))
     | Trm_if (cond, then_t, else_t) ->
        (* cond *)
        (add_dir Dir_cond (resolve_target_simple p cond)) ++
        (* then *)
        (add_dir Dir_then (resolve_target_simple p then_t)) ++
        (* else *)
        (add_dir Dir_else (resolve_target_simple p else_t))
     | Trm_apps (f, args) ->
        (* fun *)
        (add_dir Dir_app_fun (resolve_target_simple p f)) ++
        (* args *)
        (explore_list args (fun n -> Dir_arg n) (resolve_target_simple p))
     | Trm_seq tl
       | Trm_array tl
       | Trm_struct tl ->
        explore_list tl (fun n -> Dir_nth n) (resolve_target_simple p)
     | Trm_val (Val_array vl)
       | Trm_val (Val_struct vl) ->
        explore_list (List.map (trm_val ~loc) vl) (fun n -> Dir_nth n)
          (resolve_target_simple p)
     | Trm_labelled (l, body) ->
        add_dir Dir_name (resolve_target_simple p (trm_var ~loc l)) ++
        add_dir Dir_body (resolve_target_simple p body)
     | Trm_switch (cond, cases) ->
        (add_dir Dir_cond (resolve_target_simple p cond)) ++
        (foldi (fun i epl case -> epl ++ explore_case i case p) [] cases)
     | _ ->
        print_info loc "explore_in_depth: cannot find a subterm to explore\n";
        []
     end

(*
  call resolve_target_simple on given case name and body
  i is the index of the case in its switch
 *)
and explore_case (i : int) (case : trm list * trm) (p : target_simple) : paths =
  let (tl, body) = case in
  match tl with
  (* default case *)
  | [] ->
     add_dir (Dir_case (i, Case_body)) (resolve_target_simple p body)
  | _ ->
     (foldi
        (fun j epl t ->
          epl ++
          (add_dir (Dir_case (i, Case_name j)) (resolve_target_simple p t))
        )
        []
        tl
     ) ++
     add_dir (Dir_case (i, Case_body)) (resolve_target_simple p body)

(* follow the direction d in t and call resolve_target_simple on p *)
and follow_dir (d : dir) (p : target_simple) (t : trm) : paths =
  let loc = t.loc in
  match d, t.desc with
  | Dir_nth n, Trm_seq tl
    | Dir_nth n, Trm_array tl
    | Dir_nth n, Trm_struct tl ->
     app_to_nth_dflt loc tl n
       (fun nth_t -> add_dir (Dir_nth n) (resolve_target_simple p nth_t))
  | Dir_nth n, Trm_val (Val_array vl)
    | Dir_nth n, Trm_val (Val_struct vl) ->
     app_to_nth_dflt loc vl n (fun nth_v ->
         add_dir (Dir_nth n) (resolve_target_simple p (trm_val ~loc nth_v)))
  | Dir_cond, Trm_if (cond, _, _)
    | Dir_cond, Trm_while (cond, _)
    | Dir_cond, Trm_for (_, cond, _, _)
    | Dir_cond, Trm_switch (cond, _) ->
     add_dir Dir_cond (resolve_target_simple p cond)
  | Dir_then, Trm_if (_, then_t, _) ->
     add_dir Dir_then (resolve_target_simple p then_t)
  | Dir_else, Trm_if (_, _, else_t) ->
     add_dir Dir_else (resolve_target_simple p else_t)
  | Dir_body, Trm_let (_,(_,_),body)
    | Dir_body, Trm_let_fun (_, _, _, body)
    | Dir_body, Trm_for (_, _, _, body)
    | Dir_body, Trm_while (_, body)
    | Dir_body, Trm_abort (Ret (Some body))
    | Dir_body, Trm_labelled (_, body) ->
     add_dir Dir_body (resolve_target_simple p body)
  | Dir_for_init, Trm_for (init, _, _, _) ->
     add_dir Dir_for_init (resolve_target_simple p init)
  | Dir_for_step, Trm_for (_, _, step, _) ->
     add_dir Dir_for_step (resolve_target_simple p step)
  | Dir_app_fun, Trm_apps (f, _) -> add_dir Dir_app_fun (resolve_target_simple p f)
  | Dir_arg n, Trm_apps (_, tl) ->
     app_to_nth_dflt loc tl n (fun nth_t ->
         add_dir (Dir_arg n) (resolve_target_simple p nth_t))
  | Dir_arg n, Trm_let_fun (_, _, arg, _) ->
     let tl = List.map (fun (x, _) -> trm_var ~loc x) arg in
     app_to_nth_dflt loc tl n (fun nth_t ->
         add_dir (Dir_arg n) (resolve_target_simple p nth_t))
  | Dir_name, Trm_let (_,(x,_),_)
    | Dir_name, Trm_let_fun (x, _, _, _)
    | Dir_name, Trm_typedef (Typedef_abbrev (x, _))
    | Dir_name, Trm_labelled (x, _)
    | Dir_name, Trm_goto x ->
     add_dir Dir_name (resolve_target_simple p (trm_var ~loc x))
  | Dir_case (n, cd), Trm_switch (_, cases) ->
     app_to_nth_dflt loc cases n
       (fun (tl, body) ->
         match cd with
         | Case_body ->
            add_dir (Dir_case (n, cd)) (resolve_target_simple p body)
         | Case_name i ->
            app_to_nth_dflt loc tl i (fun ith_t ->
                add_dir (Dir_case (n, cd)) (resolve_target_simple p ith_t))
       )
  | Dir_enum_const (n, ecd), Trm_typedef (Typedef_enum (_, xto_l)) ->
     app_to_nth_dflt loc xto_l n
       (fun (x, t_o) ->
         match ecd with
         | Enum_const_name ->
            add_dir (Dir_enum_const (n, ecd)) (resolve_target_simple p (trm_var ~loc x))
         | Enum_const_val ->
            begin match t_o with
            | None ->
               print_info loc "follow_dir: no value for constant of index %d\n"
                 n;
               []
            | Some t ->
               add_dir (Dir_enum_const (n, ecd)) (resolve_target_simple p t)
            end
       )
  | _, _ ->
     print_info loc "follow_dir: direction %s does not match"
       (dir_to_string d);
     []

(*
  call cont on each element of the list and gathers the results
  used for seq, array, struct, and fun arguments
  d: function that gives the direction to add depending on the index
 *)
and explore_list (tl : trm list) (d : int -> dir)
  (cont : trm -> paths) : paths =
  foldi (fun i epl t -> epl ++ add_dir (d i) (cont t)) [] tl

(*
  call cont on each element of the list whose index is in the domain and
  gather the results
  d: function that gives the direction to add depending on the index
 *)
and explore_list_ind (tl : trm list) (d : int -> dir) (dom : int list)
  (cont : trm -> paths) : paths =
  foldi
    (fun i epl t ->
      if List.mem i dom then epl ++ add_dir (d i) (cont t) else epl)
    []
    tl


(******************************************************************************)
(*                          Target-between resolution                         *)
(******************************************************************************)

(* Extracts the last direction from a nonempty path *)
let extract_last_path_item (p : path) : dir * path =
  match List.rev p with
  | [] -> raise Not_found
  | d :: p' -> (d, List.rev p')

(* Get the number of instructions a sequence contains *)
let get_arity_of_seq_at (p : path) (t : trm) : int =
  let (d,p') =
    try extract_last_path_item p
    with Not_found -> fail None "get_arity_of_seq_at: expected a nonempty path"
    in
  match d with
  | Dir_nth _ ->
      let (seq_trm,_context) = Path.resolve_path p' t in
      begin match seq_trm.desc with
      | Trm_seq tl -> List.length tl
      | _ -> fail None "get_arity_of_seq_at: expected a sequence"
      end
  | _ -> fail None "get_arity_of_seq_at: expected a Dir_nth as last direction"

let compute_relative_index (rel : target_relative) (t : trm) (p : path) : path * int =
  match rel with
  | TargetAt -> fail None "compute_relative_index: Didn't expect a TargetAt"
  | TargetFirst -> (p, 0)
  | TargetLast -> (p, get_arity_of_seq_at p t)
  | TargetBefore | TargetAfter ->
      let shift =
         match rel with
         | TargetBefore -> 0
         | TargetAfter -> 1
         | _ -> assert false
         in
      let (d,p') =
        try extract_last_path_item p
        with Not_found -> fail None "compute_relative_index: expected a nonempty path"
        in
      match d with
      | Dir_nth i -> (p', i + shift)
      | _ -> fail None "compute_relative_index: expected a Dir_nth as last direction"

(* TODO: use this function to implement seq_insert , etc. *)
(* TODO: include a test case for seq_insert that says [cAfter, cStr "x ="] where the index of
   the instruction "x =" is not the same in different sequences. *)
let resolve_target_between (tg : target) (t : trm) : (path * int) list =
  let tgs = target_to_target_struct tg in
  if tgs.target_relative = TargetAt
    then fail None "resolve_target_between:this target should contain a cBefore, cAfter, cFirst, or cLast";
  let res = resolve_target_struct tgs t in
  List.map (compute_relative_index tgs.target_relative t) res

