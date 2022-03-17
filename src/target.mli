open Ast
open Path

type path = Path.path
type paths = path list

type constr = Constr.constr
type typ_constraint = (typ->bool)
type target = constr list
type target_relative = Constr.target_relative
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

val occIndices  : ?nb:int -> int list -> constr

val occIndex : ?nb:int -> int -> constr

val occFirst : constr

val occLast : constr

val nbMulti : constr

val nbAny : constr

val nbExact : int -> constr

val target_of_path : path -> target

val dRoot : constr

val dArrayNth : int -> constr

val dSeqNth : int -> constr

val dStructNth : int -> constr

val dCond : constr

val dThen : constr

val dElse : constr

val dBody : constr

val dForInit : constr

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

val cInDepth : constr

val cStrictNew : constr

val cChain : constr list -> constr

val cInclude : string -> constr

val cWriteVar : ?regexp:bool -> ?substr:bool -> ?trmkind:trm_kind -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cReadVar : string -> constr

val cAny : constr

val cChoose : constr

val cAlloc : int option -> constr

val cMalloc : int option -> constr

val cCalloc : int option -> constr

val sInstr : ?substr:bool -> string -> constr

val sExpr : ?substr:bool -> string -> constr

val sInstrRegexp : ?substr:bool -> string -> constr

val sExprRegexp : ?substr:bool -> string -> constr

val cFor_c: ?init:target -> ?cond:target ->
           ?step:target -> ?body:target -> string -> constr

val cFor : ?start:target -> ?direction: loop_dir -> ?stop:target -> ?step:target -> ?body:target -> string -> constr

val cForNestedAtDepth : int -> constr

val cWhile : ?cond:target -> ?body:target -> unit ->
             constr
val cDoWhile : ?body:target -> ?cond:target -> unit -> constr

val cIf : ?cond:target -> ?then_:target ->
          ?else_:target -> unit -> constr

val cThen : constr

val cOr : target list -> constr

val cAnd : target list -> constr

val cDiff : target list -> target list -> constr

val cHasTypePred : (typ -> bool) -> constr

val cHasTypeAst : typ -> constr

val cHasType : string -> constr

val with_type : ?typ:string -> ?typ_pred:typ_constraint -> target -> target

val cArgPred : ?typ:string -> ?typ_pred:typ_constraint -> (string -> bool) -> constr

val cArg : ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cVarDef : ?regexp:bool -> ?substr:bool -> ?body:target -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cFunDef : ?args:targets -> ?args_pred:target_list_pred -> ?body:target -> ?ret_typ:string -> ?ret_typ_pred:typ_constraint -> ?regexp:bool ->string -> constr

val cTopFunDef : ?args:targets -> ?args_pred:target_list_pred -> ?body:target -> ?ret_typ:string -> ?ret_typ_pred:typ_constraint -> ?regexp:bool -> string -> constr

val cTop : ?regexp:bool -> string -> constr

val cTypDef : ?substr:bool -> ?regexp:bool -> string -> constr

val cDef : string -> constr

val cEnum : ?name:string -> ?substr:bool -> ?constants:((string * target) list) -> ?regexp:bool -> unit -> constr

val cSeq : ?args:targets -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?regexp:bool -> ?substr:bool -> ?trmkind:trm_kind -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr

val cLitPred : (lit -> bool) -> constr

val cLit : constr

val cIntPred : (int -> bool) -> constr

val cInt : int -> constr

val cDoublePred : (float -> bool) -> constr

val cDouble : float -> constr

val cBoolPred : (bool -> bool) -> constr

val cBool : bool -> constr

val cStringPred : (string -> bool) -> constr

val cString : string -> constr

val cCall : ?fun_:target -> ?args:targets -> ?args_pred:target_list_pred -> ?accept_encoded:bool -> ?regexp:bool -> string -> constr

val cFun : ?fun_:target -> ?args:targets -> ?args_pred:target_list_pred -> ?regexp:bool -> string -> constr

val cPrimPred : (prim -> bool) -> constr

val cPrim : prim -> constr

val cPrimPredFun : ?args:targets -> ?args_pred:target_list_pred -> (prim -> bool) -> constr

val cPrimFun : ?args:targets -> ?args_pred:target_list_pred -> prim -> constr

val cPrimFunArith : ?args:targets -> ?args_pred:target_list_pred -> unit -> constr

val cPrimNew : ?arg:target -> unit -> constr

val cInit : ?arg:target -> unit -> constr

val dInit : constr

val cMark : mark -> constr

val cMarks : mark list -> constr

val cMarkSt : (mark -> bool) -> constr

val cMarkAny : constr

val cLabel : ?substr:bool -> ?body:target -> ?regexp:bool -> string -> constr

val cLabelBody : ?substr:bool -> ?body:target -> ?regexp:bool -> string -> constr

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

val cFieldRead : ?field:string -> ?base:target -> ?substr:bool -> ?regexp:bool-> unit -> constr

val cFieldWrite : ?base:target -> ?substr:bool -> ?regexp:bool -> ?rhs:target -> ?field:string -> unit -> constr

val cFieldAccess : ?base:target -> ?substr:bool -> ?regexp:bool -> ?field:string -> unit -> constr

val cFieldReadOrWrite : ?base:target -> ?substr:bool -> ?regexp:bool -> ?field:string -> unit -> constr

val cCellRead : ?base:target -> ?index:target -> unit -> constr

val cCellWrite : ?base:target -> ?rhs:target -> ?index:target -> unit -> constr

val cCellReadOrWrite : ?base:target -> ?index:target -> unit -> constr

val cCellAccess : ?base:target -> ?index:target -> unit -> constr

val cArrayInit : constr

val cStructInit : constr

val cCell : ?cell_index: int option -> unit -> constr

val cSwitch : ?cond:target ->
              ?cases:((case_kind * target) list) -> unit -> constr

val cCase : ?value:target -> unit -> case_kind

val cDefault : case_kind

val cWrite : ?lhs:target -> ?rhs:target -> ?typ:string -> ?typ_pred:typ_constraint -> unit -> constr

val cRead : ?addr:target -> unit -> constr

val cReadOrWrite : ?addr:target -> unit -> constr

val dRHS : constr

val dLHS : constr

val cTargetInDepth : target -> constr

val make_target_list_pred : (int -> target) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

val target_list_simpl : targets -> target_list_pred

val target_list_one_st : target -> target_list_pred

val target_list_all_st : target -> target_list_pred

val target_list_pred_default : target_list_pred

val filter_constr_occurrence : target -> target

val enable_multi_targets : target -> target

val compute_stringreprs_and_update_ast : ?optitrust_syntax:bool -> (trm->bool) -> AstC_to_c.stringreprs

(* Target resolution *)

val resolve_target : target -> trm -> paths

val resolve_target_exactly_one : target -> trm -> path

val resolve_target_between : target -> trm -> (path * int) list

val resolve_target_between_exactly_one : target -> trm -> (path * int)

val with_stringreprs_available_for : target list -> trm -> (trm -> 'a) -> 'a

val resolve_target_with_stringreprs_available : target -> trm -> paths

val resolve_target_exactly_one_with_stringreprs_available : target -> trm -> path

val resolve_path_with_stringreprs_available : path -> trm -> trm

val apply_on_path : (trm -> trm) -> trm -> path -> trm

val applyp_on_path : (path -> trm -> trm) -> trm -> path -> trm

val applyi_on_targets : (int -> trm -> path -> trm) -> target -> unit

val apply_on_targets : (trm -> path -> trm) -> target -> unit

val applyi_on_transformed_targets : ?rev:bool -> (path -> 'a) -> (int -> trm -> 'a -> trm) -> target -> unit

val apply_on_transformed_targets : ?rev:bool -> (path -> 'a) -> (trm -> 'a -> trm) -> target -> unit

val iteri_on_targets : ?rev:bool -> (int -> trm -> path -> unit) -> target -> unit

val iter_on_targets : ?rev:bool -> (trm -> path -> unit) -> target -> unit

val iteri_on_transformed_targets : ?rev:bool -> (path -> 'a) -> (int -> trm -> 'a -> unit) -> target -> unit

val iter_on_transformed_targets : ?rev:bool -> (path -> 'a) -> (trm -> 'a -> unit) -> target -> unit


val applyi_on_targets_between : (int -> trm -> path * int -> trm) -> target -> unit

val apply_on_targets_between : (trm -> path*int -> trm) -> target -> unit

val applyi_on_transformed_targets_between : (path * int -> 'a) -> (int -> trm -> 'a -> trm) -> target -> unit

val apply_on_transformed_targets_between : (path * int -> 'a) -> (trm -> 'a -> trm) -> target -> unit

val show : ?line:int -> ?reparse:bool -> ?types:bool -> target -> unit

val bigstep : string -> unit

(* Target debugging *)

val target_to_string : target -> string

module Transfo : sig
  type t = target -> unit
  type local = trm -> path -> trm
end

val string_to_rexp : bool -> bool -> string -> Constr.trm_kind -> Constr.rexp

(* Shorthand *)

val (!!) : 'a -> 'a
val (!^) : 'a -> 'a
val (!!!) : 'a -> 'a
val (!!^) : 'a -> 'a


val reparse_after : ?reparse:bool -> Transfo.t -> Transfo.t

val get_trm_at : target -> trm option
val get_ast : unit -> trm

val var : ?annot:trm_annot list -> ?loc:location -> ?add:special_operator list -> ?typ:Ast.typ option -> ?attributes:attribute list -> ?ctx:ctx option -> ?marks:string list -> ?kind:varkind-> string -> trm

val lit : string -> trm

val atyp : string -> typ

val expr : ?vars:string list -> string -> trm

val atypexpr : string -> typ

val stmt : string -> trm

val get_relative_type : target -> target_relative option

(* Other *)

val show_next_id_reset : unit -> unit
