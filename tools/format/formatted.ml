open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Str
open Tools
open Path

exception Resolve_target_failure of string
(** [Resolve_target_failure]: exception raised when a target couldn't be resolved *)

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

type rexp = {
  rexp_desc : string; (* printable version of regexp *)
  rexp_exp : regexp;
  rexp_substr : bool;
  rexp_trm_kind : trm_kind;
}
(** [rexp]: data structure for storing regular expressions *)

let pp_rexp fmt rexp = Format.pp_print_string fmt rexp.rexp_desc

(** [target_relative]: target kind *)
type target_relative = TargetAt | TargetFirst | TargetLast | TargetBetweenAll | TargetBefore | TargetAfter
[@@deriving show { with_path = false }]

(** [target_occurrences]: the number of targets to match *)
type target_occurrences =
  | ExpectNb of int (* exactly n occurrences *)
  | ExpectMulti (* > 0 number of occurrences *)
  | ExpectAnyNb (* any number of occurrences *)
  | SelectOcc of int option * int list
    (* filter the occurences to keep only those in the list, negative index are taken from the back, if the first argument is given, check that the number of occurences is exactly that *)
[@@deriving show { with_path = false }]

(* A [target] is a list of constraints to identify nodes of the AST
   that we require the result path to go through. *)

type target = constr list
(** [target]: is a list of constraints to identify nodes of the AST that we require the result path to go through. *)

and targets = target list
(** [targets]: a list of targets *)

(** [depth]: targets by default resolve at any depth, but they can also be resolved only on a specific depth *)
and depth = DepthAny | DepthAt of int [@@deriving show { with_path = false }]
(* type constr_qname = constr_name * constr_qpath


   and constr_qpath = var list -> bool


   f and M :: f

   let check_qname (qx : qvar ) (cq : constr_qname) : bool =
    ((snd cq) qx.qvar_path ) && (check_name (fst cq) (qx.qvar_var)) ||
    (check_name (fst cq) (qx.qvar_str )


*)

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
  | Constr_omp of (directive -> bool) * string
  (* Constraint to match a namespace *)
  | Constr_namespace of constr_name
  (* Constraint to match a term when a predicate is true *)
  | Constr_pred of (trm -> bool)
(* LATER: optimize constr_of_path; should be recognized by resolution,
   and processed more efficiently; checking that the start of the path
   is the root, then reaching directly the position, assuming the path
   is valid (option: raise an exception if the path is invalid). *)

and typ_constraint = typ -> bool
(** [typ_constraint]: predicate on types *)

and arg_constraint = target
(** [arg_constraint]: constraint on an argument, represented as a target with a single item
    of the form [cArg ..] or [cTrue]  *)

and var_constraint = string -> bool
(** [var_constraint]: constraint over variable names *)

and constr_name = rexp option
(** [constr_name]: names involved in constraints, e.g. for goto labels *)

and constr_enum_const = (constr_name * target) list option
(** [constr_enum_const]: constraint on const enums *)

and constr_accesses = constr_access list option
(** [constr_accesses]: constraint of a list of struct accesses or array accesses *)
(* Predicate for expressing constraints over a list of subterms. It consists of:
   - a function that produces the constraints for the i-th subterm
   - a function that takes a list of booleans indicating which of the subterms
     matched their respective constraint, and returns a boolean indicating whether
     the full list should be considered as matching or not.
   - a string that explains what was the user intention *)

and target_list_pred = {
  target_list_pred_ith_target : int -> target;
  target_list_pred_validate : bool list -> bool;
  target_list_pred_to_string : unit -> string;
}
(** [target_list_pred]: predicate for expressing constraints over a list of stubterms. It consists of
    - a function that produces the constraints for the i-th subterm
    - a function that takes a list of booleans indicating which of the subterms match their respective constraint,
        and returns a boolean indicatin whether the full list should be considered as matching or not
    - a string that explains what was the user intention *)

(** [constr_access]: constraint over a single struct or array access  *)
and constr_access =
  (* array indices may be arbitrary terms *)
  | Array_access of target
  (* struct fields are strings *)
  | Struct_access of constr_name
  | Any_access

and constr_cases = (case_kind * target) list option
(** [constr_cases]: for each case, its kind and a constraint on its body *)

(** [case_kind]: switch case kind used on [constr_cases] *)
and case_kind =
  (* case: value *)
  | Case_val of target
  | Case_default
  | Case_any

(** [abort_kind]: abort instruction kind used for abort insitruction constraints  *)
and abort_kind = Any | Return | Break | Continue [@@deriving show { with_path = false }]

let target_to_string = show_target
let constr_to_string = show_constr
let trm_kind_to_string = show_trm_kind

(** [target_simple]: is a [target] without any of the following relative constraints
   Constr_relative, Constr_occurrences, Constr_target.
   Note: It can include [cStrict] *)

type target_simple = target

type target_struct = {
  target_path : target_simple; (* this path contains no nbMulti/nbEx/tBefore/etc.., only cStrict can be there *)
  target_relative : target_relative;
  target_occurrences : target_occurrences;
  target_incontracts : bool;
}
(** [target_struct]: structured representation of a [target] that decomposes the special constructors
   such as Constr_relative, Constr_occurrences, Constr_target from the [target_simple]. *)

(** [make_target_list_pred ith_target validate to_string]: creates a simple target_pred object with with its components
   being [ith_target], [validate] and [to_string] *)
let make_target_list_pred (ith_target : int -> target) (validate : bool list -> bool) (to_string : unit -> string) :
    target_list_pred =
  {
    target_list_pred_ith_target = ith_target;
    target_list_pred_validate = validate;
    target_list_pred_to_string = to_string;
  }

let%transfo local_name
  ?(my_mark : mark = no_mark)
  ?(indices : (string list) = [])
  ?(alloc_instr : target option) (* TODO: this should be supported at non-basic level *)
  ?(type_and_dims : (typ * trms) option)
  (v : var) ~(into : string)
  ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
  ?(local_ops : local_ops = Local_arith (Lit_int (typ_int, 0), Binop_add))
  (tg : target) : unit =
  let remove = (my_mark = no_mark) in
  let get_type_and_dims (t : trm) (tg1 : target) : typ * trms =
    let var_type = begin match t.desc with
      | Trm_let ((_, ty), _) -> get_inner_ptr_type ty
      | Trm_apps (_, [lhs; _rhs], _, _) when is_set_operation t ->
        begin match lhs.typ with
        | Some ty -> ty
        | None -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s\n'" (var_to_string v))
        end
      | _ -> trm_fail t (Printf.sprintf "Matrix_basic.get_alloc_type_and_trms: couldn't findd the type of variable %s, alloc_instr
          target doesn't point to a write operation or a variable declaration \n'" (var_to_string v))
      end in
      let dims = begin match Target.get_trm_at (tg1 @ [Target.cNew ()]) with
        | Some at ->
          begin match Matrix_trm.alloc_inv at with
          | Some (ty, dims, _) -> dims
          | _ -> trm_fail t "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
          end
        | None -> failwith "Matrix_basic.get_alloc_type_and_trms: couldn't get the dimensions and the size of the matrix"
        end in (var_type, dims)
    in
  Nobrace_transfo.remove_after ~remove (fun _ ->
    Target.iter (fun p ->
      let seq_p, _ = Internal.isolate_last_dir_in_seq p in
      let seq_tg = target_of_path seq_p in
      let (elem_type, dims) =
        match type_and_dims with
        | Some stuff -> stuff
        | None -> begin match alloc_instr with
          | Some tg1 ->
            begin match get_trm_at tg1 with
            | Some t1 -> get_type_and_dims t1 tg1
            | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
            end
          | None ->
            let var_target = cOr [[cVarDef v.name]; [cWriteVar v.name]] in
            begin match get_trm_at (seq_tg @ [var_target]) with
            | Some t1 ->
              let tg1 = (seq_tg @ [var_target]) in
              get_type_and_dims t1 tg1
            | None -> failwith "Matrix_basical_name: alloc_instr target does not match to any ast node"
            end
        end
      in
      if not remove then Nobrace.enter();
      Target.apply_at_path (Matrix_core.local_name_aux my_mark v into dims elem_type indices local_ops) p
    ) tg
  )
