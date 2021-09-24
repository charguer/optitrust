open Ast
open Ast_to_c
open Str
open Tools
open Path


(******************************************************************************)
(*                        Data structure for targets                          *)
(******************************************************************************)

(* Type to classify trms into four main classes: 1)Structuring statements, 2) Instructions 3) Expression and 4) others*)
type trm_kind =
  | TrmKind_Typedef (* type definition that appears in the AST *)
  | TrmKind_Ctrl    (* control statement, for loops, while loops and unit-type if statements  *)
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
type target_occurrences =
    | ExpectedOne  (* = 1 occurence, the default value *)
    | ExpectedNb of int  (* exactly n occurrences *)
    | ExpectedMulti  (* > 0 number of occurrences *)
    | ExpectedAnyNb  (* any number of occurrences *)
    | ExpectedSelected of int option * int list

(* A [target] is a list of constraints to identify nodes of the AST
   that we require the result path to go through. *)
type target = constr list

and targets = target list
and depth =
  | DepthAny
  | DepthAt of int

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
  | Constr_depth of depth
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
  | Constr_for_c of target * target * target * target
  (* for index, start, stop, step, body *)
  | Constr_for of constr_name * loop_dir * target * target * target * target
  (* while: cond, body *)
  | Constr_while of target * target
  (* do while: body, cond *)
  | Constr_do_while of target * target
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
  | Constr_lit of lit option
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
  | Constr_access of target * constr_accesses
  (* switch: cond, cases *)
  | Constr_switch of target * constr_cases
  (* Target relative to another trm *)
  | Constr_relative of target_relative
  (* Number of  occurrences expected  *)
  | Constr_occurrences of target_occurrences
  (* List of constraints *)
  | Constr_target of constr list
  (* Constraint used for argument match *)
  | Constr_bool of bool
  (* Constraint that matches only the root of the AST *)
  | Constr_root
  | Constr_prim of prim
  | Constr_mark of (mark -> bool) * string
  | Constr_or of target list
  | Constr_and of target list


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
  { target_list_pred_ith_target : int -> target;
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

(* [target_simple] is a [target] without Constr_relative, Constr_occurrences, Constr_target;
   It can however, include [cStrict]. *)
type target_simple = target

(* [target_struct] is the structured representation of a [target] that decomposes the
   special constructors such as Constr_relative, Constr_occurrences, Constr_target from the
   [target_simple]. *)
type target_struct = {
   target_path : target_simple; (* this path contains no nbMulti/nbEx/tBefore/etc.., only cStrict can be there *)
   target_relative : target_relative;
   target_occurrences : target_occurrences; }

let make_target_list_pred (ith_target : int -> target) (validate : bool list -> bool) (to_string : unit -> string) : target_list_pred =
  { target_list_pred_ith_target = ith_target;
    target_list_pred_validate = validate;
    target_list_pred_to_string = to_string; }

let depth_pred (d : depth) : depth =
  match d with
  | DepthAny -> DepthAny
  | DepthAt n ->
      if n <= 0 then fail None "depth_pred: argument of DepthAt is not positive";
      DepthAt (n-1)

(******************************************************************************)
(*                        Pretty-printing of targets                          *)
(******************************************************************************)

let trm_kind_to_string (k : trm_kind) : string =
  match k with
  | TrmKind_Typedef -> "Typedef"
  | TrmKind_Ctrl -> "Ctrl"
  | TrmKind_Instr -> "Instr"
  | TrmKind_Expr -> "Expr"
  | TrmKind_Any -> "Any"

let rexp_to_string (r : rexp) : string =
  (trm_kind_to_string r.rexp_trm_kind) ^ "-" ^
  (if r.rexp_substr then "Sub" else "Exact") ^ "-" ^
  r.rexp_desc

let depth_to_string (depth : depth) : string =
  match depth with
  | DepthAny -> "DepthAny"
  | DepthAt n -> "DepthAt " ^ string_of_int n

let rec constr_to_string (c : constr) : string =
  match c with
  | Constr_depth depth -> "Depth " ^ (depth_to_string depth)
  | Constr_dir d -> dir_to_string d
  | Constr_include s -> "Include " ^ s
  | Constr_regexp r -> "Regexp " ^ rexp_to_string r
  | Constr_for_c (p_init, p_cond, p_step, p_body) ->
     let s_init = target_to_string p_init in
     let s_cond = target_to_string p_cond in
     let s_step = target_to_string p_step in
     let s_body = target_to_string p_body in
     "For (" ^ s_init ^ ", " ^ s_cond ^ ", " ^ s_step ^ ", " ^ s_body ^ ")"
  | Constr_for (p_index, p_direction, p_start, p_stop, p_step, p_body) ->
    let s_index =
      match p_index with | None -> "_" | Some r -> rexp_to_string r
    in
    let s_direction = match p_direction with
    | DirUp -> "Up"
    | DirDown -> "Down"
    in
    let s_start = target_to_string p_start in
    let s_stop = target_to_string p_stop in
    let s_step = target_to_string p_step in
    let s_body = target_to_string p_body in
    "For ("^s_index ^ " ," ^ s_direction ^ " , " ^ s_start ^ ", " ^ s_stop ^ ", " ^ s_step ^ ", " ^ s_body ^ ")"
  | Constr_while (p_cond, p_body) ->
     let s_cond = target_to_string p_cond in
     let s_body = target_to_string p_body in
     "While (" ^ s_cond ^ ", " ^ s_body ^ ")"
  | Constr_do_while (p_body, p_cond) ->
     let s_body = target_to_string p_body in
     let s_cond = target_to_string p_cond in
     "Do_While (" ^ s_body ^ ", " ^ s_cond ^ ")"
  | Constr_if (p_cond, p_then, p_else) ->
     let s_cond = target_to_string p_cond in
     let s_then = target_to_string p_then in
     let s_else = target_to_string p_else in
     "If (" ^ s_cond ^ ", " ^ s_then ^ ", " ^ s_else ^ ")"
  | Constr_decl_var (name, p_body) ->
     let s_name =
       match name with | None -> "_" | Some r -> rexp_to_string r
     in
     let s_body = target_to_string p_body in
     "Decl_var (" ^ s_name ^ ", " ^ s_body ^ ")"
  | Constr_decl_fun (name, _tgt_list_pred, p_body) ->
    let s_name =
       match name with | None -> "_" | Some r -> rexp_to_string r
     in
     let spred = _tgt_list_pred.target_list_pred_to_string() in
     let s_body = target_to_string p_body in
     "Decl_fun (" ^ s_name ^ spred ^ s_body ^ ")"

  | Constr_decl_type name ->
     let s_name =
       match name with | None -> "_" | Some r -> rexp_to_string r
     in
     "Decl_type " ^ s_name
  | Constr_decl_enum (name, c_const) ->
     let s_name =
       match name with | None -> "_" | Some r -> rexp_to_string r
     in
     let s_const =
       match c_const with
       | None -> "_"
       | Some np_l ->
          let sl =
            List.map
              (fun (n, p) ->
                let s_n =
                  match n with | None -> "_" | Some r -> rexp_to_string r
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
     "Var " ^ (match name with | None -> "_" | Some r -> rexp_to_string r)
  | Constr_lit l ->
     let s =
       begin match l with
       | Some l1 -> begin match l1 with
            | Lit_unit ->  "()"
            | Lit_uninitialized -> "?"
            | Lit_bool b -> string_of_bool b
            | Lit_int n -> string_of_int n
            | Lit_double d -> string_of_float d
            | Lit_string s -> s
            end
       | None -> "Any"
       end
     in "Lit " ^ s
  | Constr_app (p_fun, tgt_list_pred, accept_encoded) ->
    let spred = tgt_list_pred.target_list_pred_to_string() in
    let s_fun = target_to_string p_fun in
    "App (" ^ s_fun ^ ", " ^ spred ^ string_of_bool(accept_encoded) ^ ")"
  | Constr_label (so, p_body) ->
     let s_label =
       match so with | None -> "_" | Some r -> rexp_to_string r
     in
     let s_body = target_to_string p_body in
     "Label (" ^ s_label ^ ", " ^ s_body ^ ")"
  | Constr_goto so ->
     let s_label =
       match so with | None -> "_" | Some r -> rexp_to_string r
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
  | Constr_occurrences oc -> target_occurrences_to_string oc
  | Constr_target cl ->
    let string_cl = List.map constr_to_string cl in
    list_to_string string_cl
  | Constr_bool b -> if b then "True" else "False"
  | Constr_root -> "Root"
  | Constr_prim _ -> "Prim"
  | Constr_mark (_, str) -> "Mark (" ^ str ^ ")"
  | Constr_or tl -> "Or (" ^ Tools.list_to_string (List.map target_to_string tl) ^ ")"
  | Constr_and tl -> " (" ^ Tools.list_to_string (List.map target_to_string tl) ^ ")"
and target_to_string (tg : target) : string =
  list_to_string (List.map constr_to_string tg)


and target_struct_to_string (tgs : target_struct) : string =
  "TargetStruct(" ^
    target_relative_to_string tgs.target_relative ^ ", " ^
    target_occurrences_to_string tgs.target_occurrences ^ ", " ^
    target_to_string tgs.target_path ^ ")"

and target_occurrences_to_string (occ : target_occurrences) =
  match occ with
  | ExpectedOne -> "ExpectedOne"
  | ExpectedNb n -> sprintf "ExpectedNb(%d)" n
  | ExpectedMulti -> "ExpectedMulti"
  | ExpectedAnyNb -> "ExpectedAnyNb"
  | ExpectedSelected (ex_opt, il) ->
    let exact_nb_s = match ex_opt with
    | None -> "None"
    | Some i -> "Some " ^ (string_of_int i) in
    sprintf "ExpectedSelect(%s, %s)" exact_nb_s (Tools.list_to_string (List.map (string_of_int) il))

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
       match so with | None -> "_" | Some r -> rexp_to_string r
     in
     "Struct_access " ^ s_field
  | Any_access -> "Any_access"



(******************************************************************************)
(*                        Preprocessing of targets before resolution          *)
(******************************************************************************)

(* Flatten all the constrainst of type Constr_target *)
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
    | Constr_occurrences oc ->
      begin match !occurences with
      | None -> occurences := Some oc;
      | _ -> fail None "Constr_occurrences provided twice in path"
      end
    | _ -> ()
    in
  List.iter process_constr tr;
  let tgs = {
    target_path = List.filter (function | Constr_relative _ | Constr_occurrences _ -> false | _ -> true) tr;
    target_relative = begin match !relative with | None -> TargetAt | Some re -> re end;
    target_occurrences = begin match !occurences with | None -> ExpectedOne | Some oc -> oc end; } in
  tgs

(* Computes whether a [target] is contains a [Constr_relative] that is not [TargetAt]. *)
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

(* extend current explicit paths with a direction *)
let add_dir (d : dir) (dll : paths) : paths =
  List.map (fun dl -> d :: dl) dll




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

let rec get_trm_kind (t : trm) : trm_kind =
   let is_unit = begin match t.typ with
                 | Some ty ->
                    begin match ty.typ_desc with
                    | Typ_unit -> true
                    | _ -> false
                    end
                 | None -> false
                 end
    in
   match t.desc with
   | Trm_val _ -> if is_unit then TrmKind_Instr else TrmKind_Expr
   | Trm_var _ -> TrmKind_Expr
   | Trm_struct _ | Trm_array _ -> TrmKind_Expr
   | Trm_let_fun _ -> TrmKind_Ctrl (* purposely not an instruction *)
   | Trm_let _ -> TrmKind_Instr
   | Trm_typedef _ | Trm_let_record _-> TrmKind_Typedef
   | Trm_if _-> if is_unit then TrmKind_Ctrl else TrmKind_Expr
   | Trm_seq _ -> TrmKind_Ctrl
   (* | Trm_apps _ -> if is_unit then TrmKind_Instr else TrmKind_Expr *)
   | Trm_apps (f,_) ->
     begin match f.desc with
      | Trm_var _ -> TrmKind_Instr
      | Trm_val (Val_prim (Prim_unop Unop_post_inc)) | Trm_val (Val_prim (Prim_unop Unop_post_dec))
      | Trm_val (Val_prim (Prim_unop Unop_pre_inc)) | Trm_val (Val_prim (Prim_unop Unop_pre_dec)) -> TrmKind_Instr
      | Trm_val (Val_prim (Prim_binop Binop_set)) -> TrmKind_Instr
      | _ -> TrmKind_Expr
      end
   | Trm_while _ | Trm_do_while _ | Trm_for_c _ | Trm_for _| Trm_switch _ | Trm_abort _ | Trm_goto _ -> TrmKind_Ctrl
   | Trm_labelled (_, t) -> get_trm_kind t
   | Trm_arbitrary _ -> fail t.loc "get_trm_kind: Trm_arbitrary is removed during parsing"
   | Trm_omp_directive _ | Trm_omp_routine _ | Trm_extern _  | Trm_namespace _ | Trm_template _->TrmKind_Any

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
  if r.rexp_trm_kind <> get_trm_kind t
    then false
    else match_regexp_str r (ast_to_string t)

let is_constr_regexp (c : constr) : bool =
  match c with | Constr_regexp _ -> true | _ -> false

(* check if constraint c is satisfied by trm t *)
let rec check_constraint (c : constr) (t : trm) : bool =
  if List.mem Access t.annot  then
     (* forget the star operator at the root before checking the constraint *)
     begin match t.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t']) ->
        check_constraint c t'
     | _ -> false
        (* Ast_to_text.print_ast ~only_desc:true stdout t;
        fail t.loc "check_constraint: bad access annotation" *)
     end
  else if List.mem Multi_decl t.annot then
     (*
       check the constraint on each element of the seq and return true if one
       is true
      *)
     begin match t.desc with
     | Trm_seq tl -> List.mem true (List.map (check_constraint c) (Mlist.to_list tl))
     | _ -> fail t.loc "check_constraint: bad multi_decl annotation"
     end
  else

     let _loc = t.loc in
     begin match c, t.desc with
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
       Trm_for_c (init, cond, step, body) ->
        check_target p_init init &&
        check_target p_cond cond &&
        check_target p_step step &&
        check_target p_body body
     | Constr_for (p_index, p_direction, p_start, p_stop, p_step, p_body),
        Trm_for(index, direction, start, stop, step, body) ->
        check_name p_index index &&
        (p_direction = direction) &&
        check_target p_start start &&
        check_target p_stop stop &&
        check_target p_step step &&
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
      | Constr_decl_var (name, p_body) , Trm_let (_,(x,_), body) ->
        check_name name x &&
        check_target p_body body
     | Constr_decl_fun (name, cl_args, p_body),
       Trm_let_fun (x, _, args, body) ->
        (* DEPRECATED let tl = List.map (fun (x, _) -> trm_var ~loc x) args in
          check_list ~depth:(DepthAt 0) cl_args tl && *)
        check_name name x &&
        check_args cl_args args &&
        check_target p_body body
     | Constr_decl_type name, Trm_typedef td ->
        let is_new_typ = begin match td.typdef_body with
        | Typdef_alias _ -> true
        | Typdef_prod _ -> true
        | _ -> false
        end in
        let x = td.typdef_tconstr in
        is_new_typ && check_name name x
     | Constr_decl_enum (name, cec), Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_enum xto_l -> check_name name td.typdef_tconstr && check_enum_const cec xto_l
        | _ -> false
        end
     | Constr_seq cl, Trm_seq tl when
        not ((List.mem (No_braces (Nobrace.current())) t.annot) || List.mem Main_file t.annot)->
        check_list  ~depth:(DepthAt 0) cl (Mlist.to_list tl)
     | Constr_var name, Trm_var x ->
        check_name name x
     | Constr_lit l, Trm_val (Val_lit l') ->
        begin match l with
        | Some l1 -> is_equal_lit l1 l'
        | None -> true
        end
     | Constr_app (p_fun, cl_args, accept_encoded), Trm_apps (f, args) ->
        if not accept_encoded then
          begin match f.desc with
          | Trm_val (Val_prim (Prim_new _))
          | Trm_val (Val_prim (Prim_unop Unop_get)) -> false
          |  _ -> check_target p_fun f &&
                  check_list ~depth:(DepthAny) cl_args args
          end
        else
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
     | Constr_abort Break, Trm_abort (Break _) -> true
     | Constr_abort Continue, Trm_abort (Continue _) -> true
     | Constr_access (p_base, ca), _ ->
        let (base, al) =get_nested_accesses t in
        check_target p_base base &&
        check_accesses ca al
     | Constr_switch (p_cond, cc), Trm_switch (cond, cases) ->
        check_target p_cond cond &&
        check_cases cc cases
     | Constr_bool b, _ -> b
     | Constr_root, _ ->
        List.mem Main_file t.annot
     | Constr_prim p, Trm_val (Val_prim p1) ->
        p = p1
     | Constr_mark (pred, _), _ ->
        begin match t.desc with
        | Trm_seq tl | Trm_array tl | Trm_struct tl->
          (List.exists pred t.marks) || (List.fold_left (fun acc x -> (List.exists pred x) || acc) false tl.marks)
        | _ -> List.exists pred t.marks
        end

     | _ -> false
     end

and check_name (name : constr_name) (s : string) : bool =
  match name with
  | None -> true
  | Some r ->
     match_regexp_str r  s

and check_list ?(depth : depth = DepthAt 1) (lpred : target_list_pred) (tl : trm list) : bool =
  let ith_target = lpred.target_list_pred_ith_target in
  let validate = lpred.target_list_pred_validate in
  validate (List.mapi (fun i t -> check_target ~depth (ith_target i) t) tl)

  (* TODO typed_vars = typed_var list *)
and check_args (lpred : target_list_pred) (txl : typed_var list) : bool =
  let ith_target = lpred.target_list_pred_ith_target in
  let validate = lpred.target_list_pred_validate in
  validate (List.mapi (fun i tx -> check_arg (ith_target i) tx) txl)


  (* TODO  cVar ?typ:string ?typ_ast:typ "x"   ->  
      let tg = ... current code ... in
      add_type_constraint ?typ ?typ_ast tg

      where let add_type_constraint ?typ:string ?typ_ast:typ tg =
          if typ = None && typ_ast = None 
              then tg
              else cAnd [c; [Constr_hastyp (make_typ_constraint ?typ ?typ_ast ())]]
           
            type typ_constraint = typ -> bool 

            let make_typ_constraint ?typ:string ?typ_ast:typ () : typ_constraint
              match typ, typ_ast with
              | None, None -> (fun (ty:typ) -> true)
              | Some ty_str, None -> (fun (ty:typ) -> str = (typ_to_string ty.typ))
              | None, Some ty_ast -> (fun (ty:typ) -> compare_type ty ty_ast)
              | Some _, Some _ -> error 

            new constructor, need to be added to check_constraint
                     | Constr_hastyp of typ_constraint 
          
            type var_constraint = (string -> bool)

             new constructor, handled by check_arg only (should raise error in check_constraint)
             | Constr_arg of  var_constraint * typ_constraint

            cHasTypePred (pred:typ->bool) : constr =
                Constr_hastype pred

            cHasTypeAst (ty:typ) : constr =
                make_typ_constraint ?typ_ast=ty ()

            cHasType (tystr:string) : constr =
               make_typ_constraint ?typ=tystr ()
              
            cArgPred ?typ:string ?typ_ast:typ (pred:string->bool) : constr =
               Constr_arg (pred, make_typ_constraint ?typ ?typ_ast ())

            cArg ?typ:string ?typ_ast:typ (name:string) : constr =
              cArgPred ?typ ?typ_ast (fun x -> x = name)
       *)

  (* [check_arg] understands [cHasType] and [cArg], expect target to be singleton constraints *)
and check_arg (_tg:target) ((_var_name,_var_typ) : typed_var) : bool =
(* TODO *) true (*
  match tg with
  | [] -> true
  | [Constr_arg (var_constraint, typ_constraint)] -> 
       var_constraint var_name
    && typ_constraint var_typ
  | _ -> fail None "check_arg expects just one constraint in the target "
  *)


and check_accesses (ca : constr_accesses) (al : trm_access list) : bool =
  let rec aux (cal : constr_access list) (al : trm_access list) : bool =
    match cal, al with
    | [], [] -> true
    | Array_access p_index :: cal, Array_access_get index :: al ->
       check_target p_index index &&
       aux cal al
    | Array_access p_index :: cal, Array_access_addr index :: al ->
       check_target p_index index &&
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
and check_target ?(depth : depth = DepthAny) (tr : target) (t : trm) : bool =
  match resolve_target_simple ~depth tr t with
  | [] -> false
  | _ -> true

(*
  resolve_target computes the directions to matching subterms
  expected invariants: no duplicate target and no target which is prefix of
  another target that appears after it in the list. Guaranteed by the call to
  sort_unique
 *)

and resolve_target_simple ?(depth : depth = DepthAny) (trs : target_simple) (t : trm) : paths =
  let epl =
    match trs with
    | [] -> [[]]
    | Constr_or tl :: [] -> (* LATER: maybe we'll add an option to enforce that each target from the list tl resolves to at list one solution *)
        let all_target_must_resolve = false in
        List.fold_left (fun acc tr ->
          let potential_targets = resolve_target_simple tr t in
          begin match potential_targets with
          | [[]] when all_target_must_resolve -> fail t.loc "resolve_target_simple: for Constr_and all targets should match a trm"
          | _ -> acc @ potential_targets  (* LATER: make code more complex to avoid quadratic operation here -- TODO: call list_union acc potential_targets? *)
          end ) [] tl
    | Constr_and tl :: [] ->
        Tools.foldi (fun i acc tr ->
        let potential_target = resolve_target_simple tr t in
        begin match potential_target with
        | [[]] -> fail t.loc "resolve_target_simple: for Constr_and all targets should match a trm"
        | _ ->
          if i = 0
            (* First step, initalize the acc *)
            then potential_target
          (* Compute the intersection of all resolved targets *)
            else Tools.list_intersect acc potential_target
        end ) [] tl
    | Constr_depth new_depth :: tr ->
        (* Force the depth argument for the rest of the target, override the current [depth] *)
        resolve_target_simple ~depth:new_depth tr t
    | Constr_dir d :: tr ->
        follow_dir d tr t
    | c :: p ->
      let strict = match depth with DepthAt 0 -> true | _ -> false in
      let skip_here = match depth with DepthAt n when n > 0 -> true | _ -> false in
      let res_deep =
        if strict
           then [] (* in strict mode, must match c here *)
           else (explore_in_depth ~depth (c :: p) t) in
      let res_here =
         if skip_here || (is_constr_regexp c && res_deep <> [])
           then [] (* if a regexp matches in depth, don't test it here *)
           else (resolve_constraint c p t) in

      (* DEBUG *)
        (* printf "resolve_target_simple\n  ~strict:%s\n  ~target:%s\n  ~term:%s\n ~deep:%s\n  ~here:%s\n"
          (if strict then "true" else "false")
          (target_to_string trs)
          (Ast_to_c.ast_to_string ~ast_decode:false t)
          (paths_to_string ~sep:"\n   " res_deep)
          (paths_to_string ~sep:"\n   " res_here); *)

        (* printf " ~deep:%s\n  ~here:%s\n"
          (paths_to_string ~sep:"\n   " res_deep)
          (paths_to_string ~sep:"\n   " res_here); *)


      res_deep@res_here  (* put deeper nodes first *) in
  List.sort_uniq compare_path epl

and resolve_target_struct (tgs : target_struct) (t : trm) : paths =
  let res = resolve_target_simple tgs.target_path t in
  let nb = List.length res in
  (* Check if nb is equal to the specification of tgs.target_occurrences, if not then something went wrong *)
  let error s =
    raise (Resolve_target_failure (None, s)) in
  begin match tgs.target_occurrences with
  | ExpectedOne -> if nb <> 1 then error (sprintf "resolve_target_struct: expected exactly one match, got %d." nb); res
  | ExpectedNb n -> if nb <> n then error (sprintf "resolve_target_struct: expected %d matches, got %d." n nb); res
  | ExpectedMulti -> if nb = 0 then error (sprintf "resolve_target_struct: expected at least one occurrence, got %d." nb); res
  | ExpectedAnyNb -> res
  | ExpectedSelected (n_opt, i_selected) ->
    begin match n_opt with
    | Some n ->
      if n <> nb then error (sprintf "resolve_target_struct: expected %d matches, got %d" n nb)
        else
          (* List.iter (fun i -> if not (0 <= i && i < nb) then error (sprintf "resolve_target_struct: the requested indices are out of range") else ()); *)
          Tools.filter_not_selected i_selected res;
    | None -> if nb = 0 then error (sprintf "resolve_target_struct: expected %d matches, got %d" (List.length i_selected) nb)
                else Tools.filter_not_selected i_selected res;
    end
  end

and resolve_target (tg : target) (t : trm) : paths =
  let tgs = target_to_target_struct tg in
  if tgs.target_relative <> TargetAt
    then fail None "resolve_target: this target should not contain a tBefore/tAfter/tFirst/tLast";
  try resolve_target_struct tgs t
  with Resolve_target_failure (_loc_opt,str) ->
    fail None (str ^ "\n" ^ (target_to_string tg))

and resolve_target_exactly_one (tg : target) (t : trm) : path =
  match resolve_target tg t with
  | [p] -> p
  | _ -> fail t.loc "resolve_target_exactly_one: obtained several targets."

(* check c against t and in case of success continue with p *)
and resolve_constraint (c : constr) (p : target_simple) (t : trm) : paths =
  let loc = t.loc in
  match c with
  (*
    do not resolve in included files, except if the constraint is Constr_include
   *)
  | Constr_include h when List.mem (Include h) t.annot ->
     (*
       remove the include annotation for target resolution to proceed in the
       included file
      *)
     resolve_target_simple p {t with annot = []}
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
and explore_in_depth ?(depth : depth = DepthAny) (p : target_simple) (t : trm) : paths =
  (* By default, traversing a node decreases the depth *)
  let aux = resolve_target_simple ~depth:(depth_pred depth) p in
  (* For bodies of functions/loops/conditials, however, we do decrease the depth
     because traversing the [Trm_seq] just below will already decrease the depth *)
  let aux_body = resolve_target_simple ~depth p in

  let loc = t.loc in
  (* no exploration in depth in included files *)
  if  (List.exists (function Include _ -> true | _ -> false) t.annot) then begin
     print_info loc "explore_in_depth: no exploration in included files\n";
     []
     end

  else if List.mem Access t.annot then
     begin match t.desc with
       (*
         the wildcard is a star operator the user doesn't know about
         t' is an access under which want to explore
        *)
     | Trm_apps (_, [t']) -> add_dir (Dir_arg 0) (explore_in_depth p t')
     | _ -> fail loc "explore_in_depth: bad access annotation"
     end
  else if List.mem Multi_decl t.annot then
     (* explore each declaration in the seq *)
     begin match t.desc with
     | Trm_seq tl ->
        explore_list (Mlist.to_list tl) (fun i -> Dir_seq_nth i) (explore_in_depth p)
     | _ -> fail loc "explore_in_depth: bad multi_decl annotation"
     end
  else
     begin match t.desc with
     | Trm_let (_ ,(_, _), body) ->
       add_dir Dir_body (aux body)
     | Trm_let_fun (_, _ , _,body) ->
        (* DEPRECATED: the name of the function should not be considered an occurence;
            add_dir Dir_name (aux (trm_var ~loc x))@*)
        add_dir Dir_body (aux_body body)
     | Trm_typedef td  ->
      begin match td.typdef_body with
      | Typdef_enum xto_l ->
        let (il, tl) =
          foldi
            (fun n (il, tl) (_, t_o) ->
              match t_o with
              | None -> (il, tl)
              | Some t -> (il@[n], tl@[t])
            )
           ([], [])
           xto_l
        in
        add_dir Dir_name (aux (trm_var ~loc td.typdef_tconstr)) @
        (explore_list (List.map (fun (y, _) -> trm_var ~loc y) xto_l)
           (fun n -> Dir_enum_const (n, Enum_const_name))
           (aux)) @
        (explore_list tl
           (fun n -> Dir_enum_const (List.nth il n, Enum_const_val))
           (aux))
      | _ -> []
      end
     | Trm_abort (Ret (Some body)) ->
        add_dir Dir_body (aux body)
     | Trm_for ( _, _, start, stop, step, body) ->
        (add_dir Dir_for_start (aux start)) @
        (add_dir Dir_for_stop (aux stop)) @
        (add_dir Dir_for_step (aux step)) @
        (add_dir Dir_body (aux_body body))
     | Trm_for_c (init, cond, step, body) ->
        (* init *)
        (add_dir Dir_for_c_init (aux init)) @
        (* cond *)
        (add_dir Dir_cond (aux cond)) @
        (* step *)
        (add_dir Dir_for_c_step (aux step)) @
        (* body *)
        (add_dir Dir_body (aux_body body))
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
     | Trm_apps (f, args) ->
        (* fun *)
        (add_dir Dir_app_fun (aux f)) @
        (* args *)
        (explore_list args (fun n -> Dir_arg n) (aux))
     | Trm_seq tl ->
        explore_list (Mlist.to_list tl) (fun n -> Dir_seq_nth n) (aux)
     | Trm_array tl
     | Trm_struct tl ->
        explore_list (Mlist.to_list tl) (fun n -> Dir_nth n) (aux)
     | Trm_labelled (l, body) ->
        add_dir Dir_name (aux (trm_var ~loc l)) @
        add_dir Dir_body (aux body)
     | Trm_switch (cond, cases) ->
        (add_dir Dir_cond (aux cond)) @
        (foldi (fun i epl case -> epl@explore_case depth i case p) [] cases)
     | _ ->
        print_info loc "explore_in_depth: cannot find a subterm to explore\n";
        []
     end

(*
  call resolve_target_simple on given case name and body
  i is the index of the case in its switch
 *)
and explore_case (depth : depth) (i : int) (case : trm list * trm) (p : target_simple) : paths =
  let aux = resolve_target_simple ~depth p in
  let (tl, body) = case in
  match tl with
  (* default case *)
  | [] ->
     add_dir (Dir_case (i, Case_body)) (aux body)
  | _ ->
     (foldi
        (fun j epl t ->
          epl @
          (add_dir (Dir_case (i, Case_name j)) (aux t))
        )
        []
        tl
     ) @
     add_dir (Dir_case (i, Case_body)) (aux body)

(* follow the direction d in t and call resolve_target_simple on p *)
and follow_dir (d : dir) (p : target_simple) (t : trm) : paths =
  let aux = resolve_target_simple p in
  let loc = t.loc in
  match d, t.desc with
  | Dir_seq_nth n, Trm_seq tl ->
    app_to_nth_dflt loc (Mlist.to_list tl) n
       (fun nth_t -> add_dir (Dir_seq_nth n) (aux nth_t))
  | Dir_nth n, Trm_array tl
    | Dir_nth n, Trm_struct tl ->
     app_to_nth_dflt loc (Mlist.to_list tl) n
       (fun nth_t -> add_dir (Dir_nth n) (aux nth_t))
  | Dir_cond, Trm_if (cond, _, _)
    | Dir_cond, Trm_while (cond, _)
    | Dir_cond, Trm_do_while (_, cond)
    | Dir_cond, Trm_for_c (_, cond, _, _)
    | Dir_cond, Trm_switch (cond, _) ->
     add_dir Dir_cond (aux cond)
  | Dir_then, Trm_if (_, then_t, _) ->
     add_dir Dir_then (aux then_t)
  | Dir_else, Trm_if (_, _, else_t) ->
     add_dir Dir_else (aux else_t)
  | Dir_body, Trm_let (_,(_,_),body)
    | Dir_body, Trm_let_fun (_, _, _, body)
    | Dir_body, Trm_for_c (_, _, _, body)
    | Dir_body, Trm_for (_, _, _, _, _, body)
    | Dir_body, Trm_while (_, body)
    | Dir_body, Trm_do_while (body, _)
    | Dir_body, Trm_abort (Ret (Some body))
    | Dir_body, Trm_labelled (_, body) ->
     add_dir Dir_body (aux body)
  | Dir_for_c_init, Trm_for_c (init, _, _, _) ->
     add_dir Dir_for_c_step (aux init)
  | Dir_for_c_step, Trm_for_c (_, _, step, _) ->
     add_dir Dir_for_c_step (aux step)
  | Dir_app_fun, Trm_apps (f, _) -> add_dir Dir_app_fun (aux f)
  | Dir_arg n, Trm_apps (_, tl) ->
     app_to_nth_dflt loc tl n (fun nth_t ->
         add_dir (Dir_arg n) (aux nth_t))
  | Dir_arg n, Trm_let_fun (_, _, arg, _) ->
     let tl = List.map (fun (x, _) -> trm_var ~loc x) arg in
     app_to_nth_dflt loc tl n (fun nth_t ->
         add_dir (Dir_arg n) (aux nth_t))
  | Dir_name, Trm_typedef td ->
     add_dir Dir_name (aux (trm_var ~loc td.typdef_tconstr))
  | Dir_name, Trm_let (_,(x,_),_)
    | Dir_name, Trm_let_fun (x, _, _, _)
    | Dir_name, Trm_labelled (x, _)
    | Dir_name, Trm_goto x ->
     add_dir Dir_name (aux (trm_var ~loc x))
  | Dir_case (n, cd), Trm_switch (_, cases) ->
     app_to_nth_dflt loc cases n
       (fun (tl, body) ->
         match cd with
         | Case_body ->
            add_dir (Dir_case (n, cd)) (aux body)
         | Case_name i ->
            app_to_nth_dflt loc tl i (fun ith_t ->
                add_dir (Dir_case (n, cd)) (aux ith_t))
       )
  | Dir_enum_const (n, ecd), Trm_typedef td ->
     begin match td.typdef_body with
     | Typdef_enum xto_l ->
          app_to_nth_dflt loc xto_l n
          (fun (x, t_o) ->
            match ecd with
            | Enum_const_name ->
               add_dir (Dir_enum_const (n, ecd)) (aux (trm_var ~loc x))
            | Enum_const_val ->
               begin match t_o with
               | None ->
                  print_info loc "follow_dir: no value for constant of index %d\n"
                    n;
                  []
               | Some t ->
                  add_dir (Dir_enum_const (n, ecd)) (aux t)
               end
          )
      | _ -> []
      end
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
  foldi (fun i epl t -> epl@add_dir (d i) (cont t)) [] tl

(*
  call cont on each element of the list whose index is in the domain and
  gather the results
  d: function that gives the direction to add depending on the index
 *)
and explore_list_ind (tl : trm list) (d : int -> dir) (dom : int list)
  (cont : trm -> paths) : paths =
  foldi
    (fun i epl t ->
      if List.mem i dom then epl@add_dir (d i) (cont t) else epl)
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

let extract_last_dir (p : path) : path * int =
  match List.rev p with
  | [] -> raise Not_found
  | d :: p' ->
    begin match d with
    | Dir_seq_nth i | Dir_nth i -> (List.rev p', i)
    | _ -> fail None "extract_last_dir: expected a directory in a sequence"
    end

let get_sequence_length (t : trm) : int =
  begin match t.desc with
  | Trm_seq tl -> Mlist.length tl
  | _ -> fail t.loc "get_sequence_lenth: expected a sequence"
  end
(* Get the number of instructions a sequence contains *)
let get_arity_of_seq_at (p : path) (t : trm) : int =
  let (d,p') =
    try extract_last_path_item p
    with Not_found -> fail None "get_arity_of_seq_at: expected a nonempty path"
    in
  match d with
  | Dir_seq_nth _ ->
      let (seq_trm,_context) = Path.resolve_path p' t in
      get_sequence_length seq_trm
  | Dir_then | Dir_else | Dir_body  ->
      let (seq_trm, _) = resolve_path p t in
      get_sequence_length seq_trm
  | _ -> fail None "get_arity_of_seq_at: expected a Dir_seq_nth, Dir_then, Dir_else or Dir_body as last direction"

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
      | Dir_seq_nth i -> (p', i + shift)
      | Dir_nth i -> (p', i + shift)
      | _ -> fail None "compute_relative_index: expected a Dir_nth as last direction"

let resolve_target_between (tg : target) (t : trm) : (path * int) list =
  let tgs = target_to_target_struct tg in
  if tgs.target_relative = TargetAt
    then fail None "resolve_target_between:this target should contain a tBefore, tAfter, tFirst, or tLast";
  let res = resolve_target_struct tgs t in
  List.map (compute_relative_index tgs.target_relative t) res


let resolve_target_between_exactly_one (tg : target) (t : trm) : (path * int) =
  match resolve_target_between tg t with
  | [(p,i)] -> (p,i)
  | _ -> fail t.loc "resolve_target_between_exactly_one: obtainer several targets"
