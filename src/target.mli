open Ast
open Path

type path = Path.path
type paths = path list

type constr = Constr.constr
type typ_constraint = (typ->bool)
type target = constr list
type targets = target list

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

val cTrue : constr

val cFalse : constr

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

val cAny : constr 

val cChoose : constr 

val sInstr : ?substr:bool -> string -> constr

val sExpr : ?substr:bool -> string -> constr

val sInstrRegexp : ?substr:bool -> string -> constr

val sExprRegexp : ?substr:bool -> string -> constr

val cFor_c: ?init:target -> ?cond:target ->
           ?step:target -> ?body:target -> string -> constr

val cFor : ?direction:loop_dir -> ?start:target -> ?stop:target -> ?step:target -> ?body:target -> string -> constr

val cForNestedAtDepth : int -> constr

val cWhile : ?cond:target -> ?body:target -> unit ->
             constr
val cDoWhile : ?body:target -> ?cond:target -> unit -> constr

val cIf : ?cond:target -> ?then_:target ->
          ?else_:target -> unit -> constr

val cThen : constr

val cOr : target list -> constr

val cAnd : target list -> constr

val cHasTypePred : (typ -> bool) -> constr

val cHasTypeAst : typ -> constr

val cHasType : string -> constr

val with_type : ?typ:string -> ?typ_pred:typ_constraint -> target -> target

val cArgPred : ?typ:string -> ?typ_pred:typ_constraint -> (string -> bool) -> constr

val cArg : ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cVarDef : ?regexp:bool -> ?substr:bool -> ?body:target -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cFunDef : ?args:targets -> ?args_pred:target_list_pred -> ?body:target -> ?ret_typ:string -> ?ret_typ_pred:typ_constraint -> ?regexp:bool ->string -> constr

val cTopFunDef : ?args:targets -> ?args_pred:target_list_pred -> ?body:target -> ?ret_typ:string -> ?ret_typ_pred:typ_constraint -> string -> constr

val cTypDef : ?substr:bool -> ?regexp:bool -> string -> constr

val cDef : string -> constr

val cEnum : ?name:string -> ?substr:bool -> ?constants:((string * target) list) -> ?regexp:bool -> unit -> constr

val cSeq : ?args:targets -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?regexp:bool -> ?trmkind:trm_kind -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cBool : bool -> constr

val cInt : int -> constr

val cDouble : float -> constr

val cString : string -> constr

val cLit : constr

val cCall : ?fun_:target -> ?args:targets -> ?args_pred:target_list_pred -> ?accept_encoded:bool -> string -> constr

val cFun : ?fun_:target -> ?args:targets -> ?args_pred:target_list_pred -> string -> constr

val cPrimPred : (prim -> bool) -> constr

val cPrim : prim -> constr

val cPrimPredFun : ?args:targets -> ?args_pred:target_list_pred -> (prim -> bool) -> constr

val cPrimFun : ?args:targets -> ?args_pred:target_list_pred -> prim -> constr

val cMark : mark -> constr

val cMarks : mark list -> constr

val cMarkSt : (mark -> bool) -> constr

val cMarkAny : constr

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
                ?accesses:(constr_access list) -> ?inner_accesses:bool -> unit -> constr

val cIndex : ?index:target -> unit -> constr_access

val cField : ?field:string -> ?substr:bool -> ?regexp:bool -> unit -> constr_access

val cAccess : constr_access

val cFieldGet : ?base:target -> ?substr:bool -> ?regexp:bool-> string -> constr 

val cFieldAccess : ?base:target -> ?substr:bool -> ?regexp:bool -> string -> constr

val cFieldSet : ?base:target -> ?substr:bool -> ?regexp:bool -> string -> constr 

val cIndexGet : ?base:target -> target -> constr 

val cIndexAccess : ?base:target -> target -> constr

val cIndexSet : ?base:target -> target -> constr 

val cCell : int -> constr

val cSwitch : ?cond:target ->
              ?cases:((case_kind * target) list) -> unit -> constr

val cCase : ?value:target -> unit -> case_kind

val cDefault : case_kind

val cSet : ?lhs:target -> ?rhs:target -> ?typ:string -> ?typ_pred:typ_constraint -> unit -> constr

val cGet : ?arg:target -> unit -> constr

val dRHS : constr

val dLHS : constr

val cTargetInDepth : target -> constr

val make_target_list_pred : (int -> target) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

val target_list_simpl : targets -> target_list_pred

val target_list_one_st : target -> target_list_pred

val target_list_all_st : target -> target_list_pred

val target_list_pred_default : target_list_pred

(* Target resolution *)

val resolve_target : target -> trm -> paths

val resolve_target_exactly_one : target -> trm -> path

val resolve_target_between : target -> trm -> (path * int) list

val apply_on_path : (trm -> trm) -> trm -> path -> trm


val applyi_on_targets : (int -> trm -> path -> trm) -> target -> unit

val apply_on_targets : (trm -> path -> trm) -> target -> unit

val applyi_on_transformed_targets : (path -> 'a) -> (int -> trm -> 'a -> trm) -> target -> unit

val apply_on_transformed_targets : (path -> 'a) -> ('a -> trm -> trm) -> target -> unit


val iteri_on_targets : (int -> trm -> path -> unit) -> target -> unit

val iter_on_targets : (trm -> path -> unit) -> target -> unit

val iteri_on_transformed_targets : (path -> 'a) -> (int -> trm -> 'a -> unit) -> target -> unit

val iter_on_transformed_targets : (path -> 'a) -> ('a -> trm -> unit) -> target -> unit


val applyi_on_targets_between : (int -> trm -> path * int -> trm) -> target -> unit

val apply_on_targets_between : (trm -> path*int -> trm) -> target -> unit

val applyi_on_transformed_targets_between : (path * int -> 'a) -> (int -> trm -> 'a -> trm) -> target -> unit

val apply_on_transformed_targets_between : (path * int -> 'a) -> (trm -> 'a -> trm) -> target -> unit

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

val reparse_after : ?reparse:bool -> Transfo.t -> Transfo.t

val get_trm_at : target -> trm 
