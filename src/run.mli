(** Context management *)

val run : (unit -> unit) -> unit

val run_unit_test : ?out_prefix:string -> ?ast_decode:bool -> (unit -> unit) -> unit

(* val reset : unit -> unit *)

val exit_script : unit -> unit

val dump : ?out_prefix:string -> unit -> unit

val switch : ?only_branch:int -> (unit -> unit) list -> unit

val set_init_source : string -> unit

val reset : unit -> unit

val set_repeat_io : bool -> unit

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

val cVarDef : ?regex:bool -> ?substr:bool -> ?body:target -> string -> constr

val cFunDef : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> ?regex:bool ->string -> constr

val cTopFun : ?args:target -> ?args_pred:target_list_pred -> ?body:target -> string -> constr

val cTypDef : ?substr:bool -> ?regex:bool -> string -> constr

val cEnum : ?name:string -> ?substr:bool -> ?constants:((string * target) list) -> ?regex:bool -> unit -> constr

val cSeq : ?args:target -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?substr:bool -> ?regex:bool -> string -> constr

val cBool : bool -> constr

val cInt : int -> constr

val cDouble : float -> constr

val cString : string -> constr

val cCall : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cFun : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cLabel : ?substr:bool -> ?body:target -> ?regex:bool -> string -> constr

val cGoto : ?label:string -> ?substr:bool -> ?regex:bool -> unit -> constr

val cReturn_target : ?res:target -> unit -> constr

val cReturn : constr

val cAbort : ?kind:abort_kind -> unit -> constr

val cBreak : constr

val cContinue : constr

val cAccesses : ?base:target ->
                ?accesses:(constr_access list) -> unit -> constr

val cIndex : ?index:target -> unit -> constr_access

val cField : ?field:string -> ?substr:bool -> ?regex:bool -> unit -> constr_access

val cAccess : constr_access

val cSwitch : ?cond:target ->
              ?cases:((case_kind * target) list) -> unit -> constr

val cCase : ?value:target -> unit -> case_kind

val cDefault : case_kind

val make_target_list_pred : (int -> constr) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

(* val cSet : ?lhs:target -> ?rhs:target -> unit -> target *)

(** Transformations *)

val show_target : ?debug_ast:bool -> ?replace_top:bool -> ?keep_previous:bool -> target -> unit

val show_ast : ?replace_top:bool -> ?file:string -> ?to_stdout:bool -> target -> unit

val clean_target_decorators : unit -> unit

val extract_loop_var : ?replace_top:bool -> ?keep_label:bool -> ?label:string ->
                       target -> unit

val extract_loop_vars : ?replace_top:bool -> ?keep_label:bool ->
                        ?label:string -> target -> unit


val fold_decl : ?replace_top:bool -> ?as_reference:bool ->
                ?fold_at:(target list) -> decl_target:target -> unit -> unit

val insert_decl : ?replace_top:bool -> ?insert_before:target ->
                  ?insert_after:target -> ?const:bool ->
                  ?as_reference:bool -> name:string -> value:string -> unit ->
                  unit

val insert_const : ?replace_top:bool -> ?insert_before:target ->
                   ?insert_after:target -> name:string -> value:string ->
                   unit -> unit

val insert_and_fold : ?replace_top:bool -> ?insert_before:target ->
                      ?insert_after:target -> ?const:bool ->
                      ?as_reference:bool -> ?fold_at:(target list) ->
                      name:string -> value:string -> unit -> unit

val insert_typedef : ?replace_top:bool -> ?insert_before:target ->
                     ?insert_after:target -> name:string -> value:string ->
                     unit -> unit

val insert_and_fold_typedef : ?replace_top:bool -> ?insert_before:target ->
                              ?insert_after:target ->
                              ?fold_at:(target list) -> name:string ->
                              value:string -> unit -> unit

val remove_decl : ?replace_top:bool -> decl_target:target -> unit -> unit

val inline_decl : ?replace_top:bool -> ?delete_decl:bool ->
                  ?inline_at:(target list) -> ?fun_result:string -> ?fun_args:(string list) ->
                  ?fun_return_label:string -> decl_target:target -> unit ->unit

val inline_struct : ?replace_top:bool -> ?struct_name:string -> ?struct_fields:string list -> unit -> unit

(* val inline_record_access : ?replace_top:bool -> ?field:string -> ?var:string -> unit -> unit  *)

val delocalize : ?replace_top:bool -> ?section_of_interest:string -> ?array_size:string -> ?neutral_element:int -> ?fold_operation:string -> unit -> unit

(* val move_loop_before : ?replace_top:bool -> target -> string -> unit

val move_loop_after : ?replace_top:bool -> target -> string -> unit

val move_loop : ?replace_top:bool -> ?move_before:string -> ?move_after:string -> string-> unit *)

val aos_to_soa : ?replace_top:bool -> ?name:(string -> string) -> string -> unit

val eliminate_goto_next : ?replace_top:bool -> unit -> unit

val group_decl_init : ?replace_top:bool -> unit -> unit

val inline_seq : ?replace_top:bool -> seq_target:target -> unit -> unit

val add_attribute : ?replace_top:bool -> string -> target -> unit



module type DebugSig = sig

  exception Breakpoint

  val counter : int ref

  val backtrace : (unit -> unit) -> unit

end

module Debug : DebugSig