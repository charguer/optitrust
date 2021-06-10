open Ast
open Path

type path = Path.path
type paths = path list

type constr = Constr.constr
type target = constr list

type case_dir = Path.case_dir

type case_kind =
  | Case_val of target
  | Case_default
  | Case_any

type abort_kind =
  | Any
  | Return
  | Break
  | Continue

type constr_name

type constr_access =
  (* array indices may be arbitrary terms *)
  | Array_access of target
  (* struct fields are strings *)
  | Struct_access of constr_name
  | Any_access

type target_list_pred


val cTrue : constr

val cFalse : constr

val cBefore : constr

val cAfter : constr

val cFirst : constr

val cLast : constr

val cMulti : constr

val cAnyNb : constr

val cNb : int -> constr

val cNth : int -> constr

val cCond : constr

val cThen : constr

val cElse : constr

val cBody : constr

val cInit : constr

val cStep : constr

val cCallFun : constr

val cArg : int -> constr

val cName : constr

val cStrict : constr

val cDirCase : int -> case_dir -> constr

val cCaseName : int -> case_dir

val cCaseBody : case_dir

val cEnumConst : int -> enum_const_dir -> constr

val cEnumConstName : enum_const_dir

val cEnumConstVal : enum_const_dir

val cInclude : string -> constr

val cInstr : string -> constr

val cExpr : string -> constr

val cInstrRegexp :?substr:bool -> string -> constr

val cExprRegexp : ?substr:bool -> string -> constr

val cFor : ?init:target -> ?cond:target ->
           ?step:target -> ?body:target -> string -> constr

val cWhile : ?cond:target -> ?body:target -> unit ->
             constr

val cIf : ?cond:target -> ?then_:target ->
          ?else_:target -> unit -> constr
val cDef : string -> constr

val cVarDef : ?regexp:bool -> ?substr:bool -> ?body:target -> string -> constr

val cFunDef : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> ?regexp:bool ->string -> constr

val cTopFun : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> string -> constr

val cTypDef : ?substr:bool -> ?regexp:bool -> string -> constr

val cEnum : ?name:string -> ?substr:bool -> ?constants:((string * target) list) -> ?regexp:bool -> unit -> constr

val cSeq : ?args:target -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?substr:bool -> ?regexp:bool -> string -> constr

val cBool : bool -> constr

val cInt : int -> constr

val cDouble : float -> constr

val cString : string -> constr

val cCall : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cFun : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cLabel : ?substr:bool -> ?body:target -> ?regexp:bool -> string -> constr

val cGoto : ?label:string -> ?substr:bool -> ?regexp:bool -> unit -> constr

val cReturn_target : ?res:target -> unit -> constr

val cReturn : constr

val cAbort : ?kind:abort_kind -> unit -> constr

val cAbrtAny : abort_kind

val cAbrtRet : abort_kind

val cAbrtBrk : abort_kind

val cAbrtCtn : abort_kind

val cBreak : constr

val cContinue : constr

val cAccesses : ?base:target ->
                ?accesses:(constr_access list) -> unit -> constr

val cIndex : ?index:target -> unit -> constr_access

val cField : ?field:string -> ?substr:bool -> ?regexp:bool -> unit -> constr_access

val cAccess : constr_access

val cSwitch : ?cond:target ->
              ?cases:((case_kind * target) list) -> unit -> constr

val cCase : ?value:target -> unit -> case_kind

val cDefault : case_kind

val cSet : ?lhs:target -> ?rhs:target -> unit -> target

val make_target_list_pred : (int -> constr) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

val target_list_simpl : constr list -> target_list_pred

val target_list_one_st : constr -> target_list_pred

val target_list_pred_always_true : target_list_pred



(* Target resolution *)

val resolve_target : target -> trm -> paths
val resolve_target_between : target -> trm -> (path * int) list

val target_to_decl : var -> trm -> path option

val apply_on_path : (trm -> trm) -> trm -> path -> trm
val applyi_on_target : (int -> trm -> path -> trm) -> target -> unit
val apply_on_target : (trm -> path -> trm) -> target -> unit
val apply_on_target_between : ((path*int) -> trm-> trm) -> target -> unit
val apply_on_transformed_targets : (path -> 'a) -> ('a -> trm -> trm) -> target -> unit

(* Target debugging *)

val target_to_string : target -> string

module Transfo : sig
  type t = target -> unit
  type local = trm -> path -> trm
end

