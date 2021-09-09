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

type trm_kind = Constr.trm_kind

val bTrue : constr

val bFalse : constr

val tBefore : constr

val tAfter : constr

val tFirst : constr

val tLast : constr

val tIndices  : ?nb:int -> int list -> constr

val tIndex : ?nb:int -> int -> constr

val nbMulti : constr

val nbAny : constr

val nbExact : int -> constr

val target_of_path : path -> target

val dRoot : constr

val dNth : int -> constr

val dCond : constr

val dThen : constr

val dElse : constr

val dBody : constr

val dInit : constr

val dStep : constr

val dArg : int -> constr

val dName : constr

val dDirCase : int -> case_dir -> constr

val dCaseName : int -> case_dir

val dCaseBody : case_dir

val dEnumConst : int -> enum_const_dir -> constr

val dEnumConstName : enum_const_dir

val dEnumConstVal : enum_const_dir

val cStrict : constr

val cChain : constr list -> constr

val cInclude : string -> constr

val cSetVar : string -> constr

val cArg : int -> constr

val sInstr : ?substr:bool -> string -> constr

val sExpr : ?substr:bool -> string -> constr

val sInstrRegexp : ?substr:bool -> string -> constr

val sExprRegexp : ?substr:bool -> string -> constr

val cFor_c: ?init:target -> ?cond:target ->
           ?step:target -> ?body:target -> string -> constr

val cFor : ?direction:loop_dir -> ?start:target -> ?stop:target -> ?step:target -> ?body:target -> string -> constr

val cWhile : ?cond:target -> ?body:target -> unit ->
             constr
val cDoWhile : ?body:target -> ?cond:target -> unit -> constr

val cIf : ?cond:target -> ?then_:target ->
          ?else_:target -> unit -> constr

val cThen : constr

val cOr : target list -> constr

val cAnd : target list -> constr

val cVarDef : ?regexp:bool -> ?substr:bool -> ?body:target -> string -> constr

val cFunDef : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> ?regexp:bool ->string -> constr

val cTopFun : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> string -> constr

val cTypDef : ?substr:bool -> ?regexp:bool -> string -> constr

val cDef : string -> constr

val cEnum : ?name:string -> ?substr:bool -> ?constants:((string * target) list) -> ?regexp:bool -> unit -> constr

val cSeq : ?args:target -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?regexp:bool -> ?trmkind:trm_kind -> string -> constr

val cBool : bool -> constr

val cInt : int -> constr

val cDouble : float -> constr

val cString : string -> constr

val cLit : constr

val cCall : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> ?accept_encoded:bool -> string -> constr

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

val dRHS : constr 

val dLHS : constr 

val cTargetInDepth : target -> constr

val make_target_list_pred : (int -> constr) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

val target_list_simpl : constr list -> target_list_pred

val target_list_one_st : constr -> target_list_pred

val target_list_all_st : constr -> target_list_pred

val target_list_pred_always_true : target_list_pred


(* Target resolution *)

val resolve_target : target -> trm -> paths

val resolve_target_exactly_one : target -> trm -> path

val resolve_target_between : target -> trm -> (path * int) list

val apply_on_path : (trm -> trm) -> trm -> path -> trm

val applyi_on_target : (int -> trm -> path -> trm) -> target -> unit

val apply_on_target : (trm -> path -> trm) -> target -> unit

val apply_on_target_between : (trm -> (path*int) -> trm) -> target -> unit

val applyi_on_transformed_targets : ?rev:bool -> (path -> 'a) -> (int -> 'a -> trm -> trm) -> target -> unit

val apply_on_transformed_targets : ?rev:bool -> (path -> 'a) -> ('a -> trm -> trm) -> target -> unit

val apply_on_transformed_target_between : (path -> 'a) -> ('a -> trm -> trm) -> target -> unit

val show : ?line:int -> ?reparse:bool -> target -> unit

(* Target debugging *)

val target_to_string : target -> string

module Transfo : sig
  type t = target -> unit
  type local = trm -> path -> trm
end

val string_to_rexp : bool -> bool -> string -> Constr.trm_kind -> Constr.rexp

(* Shorthand *)

val (!!) : 'a -> 'a
val (!!!) : 'a -> 'a

val force_reparse_after : Transfo.t -> Transfo.t
