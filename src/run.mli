(** Context management *)

val run : (unit -> unit) -> unit

val run_unit_test : ?out_prefix:string -> ?ast_decode:bool -> (unit -> unit) -> unit

(* val reset : unit -> unit *)

val set_exn_backtrace : bool -> unit

val exit_script : unit -> unit

val dump : ?out_prefix:string -> unit -> unit

val switch : ?only_branch:int -> (unit -> unit) list -> unit

val set_init_source : string -> unit

val reset : unit -> unit

(** Path constructors *)
type constr = Target.constr
type target = constr list
type case_dir
type case_kind
type abort_kind
type constr_access
type enum_const_dir
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

val cInstr : ?substr:bool -> string -> constr

val cExpr : ?substr:bool -> string -> constr

val cInstrRegexp : ?substr:bool -> string -> constr

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

val make_target_list_pred : (int -> constr) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

(* val cSet : ?lhs:target -> ?rhs:target -> unit -> target *)

(** Transformations *)

(* val inline_record_access : ?replace_top:bool -> ?field:string -> ?var:string -> unit -> unit  *)

(* val move_loop_before : ?replace_top:bool -> target -> string -> unit

val move_loop_after : ?replace_top:bool -> target -> string -> unit

val move_loop : ?replace_top:bool -> ?move_before:string -> ?move_after:string -> string-> unit *)
module type DebugSig = sig

  exception Breakpoint

  val counter : int ref

  val backtrace : (unit -> unit) -> unit

end

module Debug : DebugSig