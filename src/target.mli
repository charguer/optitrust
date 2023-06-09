val old_resolution : bool ref
type trm_kind =
  Constr.trm_kind =
    TrmKind_Typedef
  | TrmKind_Ctrl
  | TrmKind_Instr
  | TrmKind_Expr
  | TrmKind_Any
type rexp =
  Constr.rexp = {
  rexp_desc : string;
  rexp_exp : Str.regexp;
  rexp_substr : bool;
  rexp_trm_kind : trm_kind;
}
type target_relative =
  Constr.target_relative =
    TargetAt
  | TargetFirst
  | TargetLast
  | TargetBefore
  | TargetAfter
type target_occurrences =
  Constr.target_occurrences =
    ExpectedOne
  | ExpectedNb of int
  | ExpectedMulti
  | ExpectedAnyNb
  | ExpectedSelected of int option * int list
  | FirstOcc
  | LastOcc
type target = constr list
and targets = target list
and depth = Constr.depth = DepthAny | DepthAt of int
and constr =
  Constr.constr =
    Constr_depth of depth
  | Constr_dir of Path.dir
  | Constr_paths of Path.paths
  | Constr_include of string
  | Constr_regexp of rexp
  | Constr_for_c of target * target * target * target
  | Constr_for of constr_name * target * Ast.loop_dir option * target *
      target * target
  | Constr_while of target * target
  | Constr_do_while of target * target
  | Constr_if of target * target * target
  | Constr_decl_var of typ_constraint * constr_name * target
  | Constr_decl_vars of typ_constraint * constr_name * target
  | Constr_decl_fun of typ_constraint * constr_name * target_list_pred *
      target * bool * Clang.cxcursor option
  | Constr_decl_type of constr_name
  | Constr_decl_enum of constr_name * constr_enum_const
  | Constr_seq of target_list_pred
  | Constr_var of constr_name
  | Constr_lit of (Ast.lit -> bool)
  | Constr_app of target * target_list_pred * bool
  | Constr_label of constr_name * target
  | Constr_goto of constr_name
  | Constr_return of target
  | Constr_abort of abort_kind
  | Constr_access of target * constr_accesses * bool
  | Constr_switch of target * constr_cases
  | Constr_relative of target_relative
  | Constr_occurrences of target_occurrences
  | Constr_target of constr list
  | Constr_bool of bool
  | Constr_root
  | Constr_prim of (Ast.prim -> bool)
  | Constr_mark of (string -> bool) * string
  | Constr_or of target list
  | Constr_and of target list
  | Constr_diff of target list * target list
  | Constr_arg of var_constraint * typ_constraint
  | Constr_hastype of typ_constraint
  | Constr_var_init
  | Constr_array_init
  | Constr_struct_init
  | Constr_omp of (Ast.directive -> bool) * string
  | Constr_namespace of constr_name
  | Constr_pred of (Ast.trm -> bool)
and typ_constraint = Ast.typ -> bool
and arg_constraint = target
and var_constraint = string -> bool
and constr_name = rexp option
and constr_enum_const = (constr_name * target) list option
and constr_accesses = constr_access list option
and target_list_pred =
  Constr.target_list_pred = {
  target_list_pred_ith_target : int -> target;
  target_list_pred_validate : bool list -> bool;
  target_list_pred_to_string : unit -> string;
}
and constr_access =
  Constr.constr_access =
    Array_access of target
  | Struct_access of constr_name
  | Any_access
and constr_cases = (case_kind * target) list option
and case_kind =
  Constr.case_kind =
    Case_val of target
  | Case_default
  | Case_any
and abort_kind = Constr.abort_kind = Any | Return | Break | Continue
type target_simple = arg_constraint
type target_struct =
  Constr.target_struct = {
  target_path : target_simple;
  target_relative : target_relative;
  target_occurrences : target_occurrences;
}
val make_target_list_pred :
  (int -> target) ->
  (bool list -> bool) -> (unit -> string) -> target_list_pred
val depth_pred : depth -> depth
val trm_kind_to_string : trm_kind -> string
val rexp_to_string : rexp -> string
val depth_to_string : depth -> string
val constr_to_string : constr -> string
val target_to_string : target -> string
val target_struct_to_string : target_struct -> string
val target_occurrences_to_string : target_occurrences -> string
val target_relative_to_string : target_relative -> string
val access_to_string : constr_access -> string
val constr_map : (constr -> constr) -> constr -> constr
val get_target_regexp_kinds : target list -> trm_kind list
exception Topfuns_cannot_filter
val get_target_regexp_topfuns_opt : target list -> constr_name list option
val target_flatten : target -> target
val target_to_target_struct : target -> target_struct
val is_target_between : target -> bool
val add_dir : Path.dir -> Path.paths -> Path.paths
val is_equal_lit : Ast.lit -> Ast.lit -> bool
val get_trm_kind : Ast.trm -> trm_kind
val match_regexp_str : rexp -> string -> bool
val stringreprs : (int, string) Hashtbl.t option ref
val print_stringreprs : unit -> unit
val get_stringrepr : Ast.trm -> string
val match_regexp_trm_kind : trm_kind -> Ast.trm -> bool
val match_regexp_trm_kinds : trm_kind list -> Ast.trm -> bool
val match_regexp_trm : rexp -> Ast.trm -> bool
val is_constr_regexp : constr -> bool
val extract_last_path_item : Path.path -> Path.dir * Path.path
val extract_last_dir : Path.path -> Path.path * int
val get_sequence_length : Ast.trm -> int
val get_arity_of_seq_at : Path.path -> Ast.trm -> int
val check_hastype : (Ast.typ -> bool) -> Ast.trm -> bool
val check_constraint : constr -> Ast.trm -> bool
val check_name : constr_name -> string -> bool
val check_list : ?depth:depth -> target_list_pred -> Ast.trms -> bool
val check_args : target_list_pred -> Ast.typed_vars -> bool
val check_arg : target -> Ast.typed_var -> bool
val check_accesses :
  ?inner_accesses:bool -> constr_accesses -> Ast.trm_access list -> bool
val check_cases : constr_cases -> (Ast.trms * Ast.trm) list -> bool
val check_kind : case_kind -> Ast.trms -> bool
val check_enum_const :
  constr_enum_const -> (string * Ast.trm option) list -> bool
val check_target : ?depth:depth -> target -> Ast.trm -> bool
val debug_resolution : bool
val resolve_target_simple : ?depth:depth -> target -> Ast.trm -> Path.paths
val resolve_target_struct : target_struct -> Ast.trm -> Path.paths
val old_resolve_target : target -> Ast.trm -> Path.paths
val fix_target_between : target_relative -> Ast.trm -> Path.path -> Path.path
val resolve_target : target -> Ast.trm -> Path.paths
val resolve_target_internal : target -> Ast.trm -> Path.paths
val resolve_target_exactly_one : target -> Ast.trm -> Path.path
val resolve_constraint : constr -> target_simple -> Ast.trm -> Path.paths
val explore_in_depth : ?depth:depth -> target_simple -> Ast.trm -> Path.paths
val explore_case :
  depth -> int -> Ast.trms * Ast.trm -> target_simple -> Path.paths
val follow_dir : Path.dir -> target_simple -> Ast.trm -> Path.paths
val explore_list :
  Ast.trms -> (int -> Path.dir) -> (Ast.trm -> Path.paths) -> Path.paths
val explore_list_ind :
  Ast.trms ->
  (int -> Path.dir) -> int list -> (Ast.trm -> Path.paths) -> Path.paths
val compute_relative_index :
  target_relative -> Ast.trm -> Path.path -> Path.path * int
val resolve_target_between : target -> Ast.trm -> (Path.path * int) list
val resolve_target_between_exactly_one : target -> Ast.trm -> Path.path * int
type path = Path.path
type paths = path list
type case_dir = Path.case_dir
val debug : bool
module Int_map = Trace.Int_map
val ml_file_excerpts : string Int_map.t ref
val debug_compute_ml_file_excerpts : bool
val compute_ml_file_excerpts : string list -> string Int_map.t
val now : unit -> float
val timing_log_handle : out_channel option ref
val stats_log_handle : out_channel option ref
val logs : out_channel list ref
val close_logs : unit -> unit
val init_logs : string -> out_channel
val write_log : out_channel -> string -> unit
val trm_to_log : out_channel -> string -> Ast.trm -> unit
type parser = string -> string * Ast.trm
val parse : parser:parser -> string -> string * Ast.trm
type context =
  Trace.context = {
  parser : parser;
  mutable prefix : string;
  extension : string;
  header : string;
  clog : out_channel;
}
val context_dummy : context
type stepdescr =
  Trace.stepdescr = {
  mutable isbigstep : string option;
  mutable script : string;
  mutable exectime : int;
}
type step_kind =
  Trace.step_kind =
    Step_root
  | Step_big
  | Step_small
  | Step_transfo
  | Step_target_resolve
  | Step_io
  | Step_scoped
  | Step_aborted
  | Step_interactive
  | Step_error
val step_kind_to_string : step_kind -> string
type step_infos =
  Trace.step_infos = {
  mutable step_script : string;
  mutable step_script_line : int option;
  mutable step_time_start : float;
  mutable step_exectime : float;
  mutable step_name : string;
  mutable step_args : (string * string) list;
  mutable step_valid : bool;
  mutable step_justif : string list;
  mutable step_tags : string list;
}
type step_tree =
  Trace.step_tree = {
  mutable step_kind : step_kind;
  mutable step_ast_before : Ast.trm;
  mutable step_ast_after : Ast.trm;
  mutable step_sub : step_tree list;
  mutable step_infos : step_infos;
}
type step_stack = step_tree list
type trace =
  Trace.trace = {
  mutable context : context;
  mutable cur_ast : Ast.trm;
  mutable step_stack : step_stack;
}
val trm_dummy : Ast.trm
val trace_dummy : trace
val the_trace : trace
val is_trace_dummy : unit -> bool
val get_decorated_history :
  ?prefix:string -> unit -> string * context * step_tree
val dummy_exectime : float
val get_cur_step : ?error:string -> unit -> step_tree
val open_root_step : ?source:string -> unit -> unit
val step_set_validity : step_tree -> unit
val finalize_step : step_tree -> unit
val get_root_step : unit -> step_tree
val get_excerpt : int -> string
val open_step :
  ?valid:bool ->
  ?line:int ->
  ?step_script:string -> kind:step_kind -> name:string -> unit -> step_tree
val step_justif : string -> unit
val step_justif_always_correct : unit -> unit
val step_arg : name:string -> value:string -> unit
val step_tag : string -> unit
val step_trivial : unit -> unit
val step_atomic : unit -> unit
val step_valid_by_composition : unit -> unit
val step_simpl_arith : unit -> unit
val close_step : ?check:step_tree -> unit -> unit
val close_step_kind_if_needed : step_kind -> unit
val close_smallstep_if_needed : unit -> unit
val close_bigstep_if_needed : unit -> unit
val close_root_step : unit -> unit
val step :
  ?valid:bool ->
  ?line:int -> kind:step_kind -> name:string -> (unit -> 'a) -> 'a
val scoped_step : kind:step_kind -> (unit -> unit) -> unit
type backtrack_result = Trace.backtrack_result = Success | Failure of exn
val backtrack_on_failure : (unit -> unit) -> backtrack_result
val parsing_step : (unit -> unit) -> unit
val dumping_step : (unit -> unit) -> unit
val error_step : string -> unit
val open_target_resolve_step : unit -> unit
val close_target_resolve_step : Path.path list -> Ast.trm -> unit
val invalidate : unit -> unit
val get_initial_ast :
  parser:parser ->
  Flags.serialization_mode -> string -> string -> string * Ast.trm
val init : ?prefix:string -> parser:parser -> string -> unit
val finalize : unit -> unit
val finalize_on_error : error:string -> unit
val get_last_substep : unit -> step_tree
val get_original_ast : unit -> Ast.trm
val alternative : (unit -> unit) -> unit
exception Failure_expected_did_not_fail
val failure_expected : (unit -> unit) -> unit
val apply : (Ast.trm -> Ast.trm) -> unit
val reset : unit -> unit
val call : (Ast.trm -> unit) -> unit
val cleanup_cpp_file_using_clang_format :
  ?uncomment_pragma:bool -> string -> unit
val get_header : unit -> string
val ensure_header : string -> unit
val output_prog :
  ?beautify:bool -> ?ast_and_enc:bool -> context -> string -> Ast.trm -> unit
val reparse_trm :
  ?info:string -> ?parser:parser -> context -> Ast.trm -> Ast.trm
val reparse :
  ?update_cur_ast:bool -> ?info:string -> ?parser:parser -> unit -> unit
val reparse_alias :
  ?update_cur_ast:bool -> ?info:string -> ?parser:parser -> unit -> unit
val dump_steps : ?onlybig:bool -> ?prefix:string -> string -> unit
val cmd : string -> unit
val trace_custom_postprocessing : (Ast.trm -> Ast.trm) ref
val dump_step_tree_to_js :
  is_substep_of_targeted_line:bool ->
  ?beautify:bool ->
  (unit -> int) -> (string -> unit) -> int -> step_tree -> unit
val dump_trace_to_js : ?beautify:bool -> ?prefix:string -> unit -> unit
val step_tree_to_doc : step_tree -> PPrint.document
val step_tree_to_file : string -> step_tree -> unit
val dump_trace_to_textfile : ?prefix:string -> unit -> unit
val output_prog_check_empty :
  ?ast_and_enc:bool -> context -> string -> Ast.trm -> unit
val light_diff : Ast.trm -> Ast.trm -> Ast.trm * Ast.trm
val dump_diff_and_exit : unit -> unit
val check_exit : line:int -> unit
val check_exit_at_end : unit -> unit
val open_bigstep : line:int -> string -> unit
val open_smallstep : line:int -> ?reparse:bool -> unit -> unit
val interactive_step :
  line:int ->
  ast_before:(unit -> Ast.trm) -> ast_after:(unit -> Ast.trm) -> unit
val show_step :
  line:int ->
  interactive_action:(unit -> unit) ->
  ?action_otherwise:(unit -> unit) -> unit -> unit
val transfo_step :
  name:string -> args:(string * string) list -> (unit -> unit) -> unit
val check_recover_original : unit -> unit
val ( !! ) : 'a -> 'a
val ( !!! ) : 'a -> 'a
val bigstep : string -> unit
val dump : ?prefix:string -> unit -> unit
val ast : unit -> Ast.trm
val set_ast : Ast.trm -> unit
val get_context : unit -> context
val var : string -> Ast.trm
val var_mut : string -> Ast.trm
val lit : string -> Ast.trm
val ty : string -> Ast.typ
val subst_dollar_number : string list -> string -> string
val expr : ?vars:string list -> string -> Ast.trm
val stmt : string -> Ast.trm
val instr : string -> Ast.trm
val cTrue : constr
val cFalse : constr
val cStrictNew : constr
val cStrict : constr
val cInDepth : constr
val cTarget : constr list -> constr
val tBefore : constr
val tAfter : constr
val tFirst : constr
val tLast : constr
val nbMulti : constr
val nbAny : constr
val nbExact : int -> constr
val occIndices : ?nb:int -> int list -> constr
val occIndex : ?nb:int -> int -> constr
val occFirst : constr
val occLast : constr
val target_of_path : path -> target
val target_of_paths : paths -> target
val dRoot : constr
val dArrayNth : int -> constr
val dSeqNth : int -> constr
val dStructNth : int -> constr
val dCond : constr
val dThen : constr
val dElse : constr
val dBody : constr
val dVarBody : constr
val dForStart : constr
val dForStop : constr
val dForStep : constr
val dForCInit : constr
val dForCStep : constr
val dName : constr
val dDirCase : int -> case_dir -> constr
val dCaseName : int -> case_dir
val dCaseBody : case_dir
val dEnumConst : int -> Path.enum_const_dir -> constr
val dEnumConstName : Path.enum_const_dir
val dEnumConstVal : Path.enum_const_dir
val dArg : int -> constr
val string_to_rexp : bool -> bool -> string -> trm_kind -> rexp
val string_to_rexp_opt : bool -> bool -> string -> trm_kind -> rexp option
val sInstrOrExpr : ?substr:bool -> trm_kind -> string -> constr
val sInstr : ?substr:bool -> string -> constr
val sExpr : ?substr:bool -> string -> constr
val sInstrOrExprRegexp : trm_kind -> bool -> string -> constr
val sInstrRegexp : ?substr:bool -> string -> constr
val sExprRegexp : ?substr:bool -> string -> constr
val cPred : (Ast.trm -> bool) -> constr
val cInclude : string -> constr
val cOr : target list -> constr
val any : ('a -> constr) -> 'a list -> constr
val multi : ('a -> constr) -> 'a list -> constr
val cAnd : target list -> constr
val cDiff : target list -> target list -> constr
val typ_constraint_default : typ_constraint
val make_typ_constraint :
  ?typ:string -> ?typ_pred:typ_constraint -> unit -> typ_constraint
val cHasTypePred : (Ast.typ -> bool) -> constr
val cHasTypeAst : Ast.typ -> constr
val cHasType : string -> constr
val with_type : ?typ:string -> ?typ_pred:typ_constraint -> target -> target
val cArgPred :
  ?typ:string -> ?typ_pred:typ_constraint -> (string -> bool) -> constr
val cArg : ?typ:string -> ?typ_pred:typ_constraint -> string -> constr
val cVarDef :
  ?regexp:bool ->
  ?substr:bool ->
  ?body:target -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr
val cVarDefs : Ast.vars -> constr
val cVarDefReg : string -> constr
val cVarInit : string -> constr
val cVarsDef :
  ?regexp:bool ->
  ?substr:bool ->
  ?body:target -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr
val cFor :
  ?start:target ->
  ?direction:Ast.loop_dir ->
  ?stop:target -> ?step:target -> ?body:target -> string -> constr
val cForNestedAtDepth : int -> constr
val cFor_c :
  ?init:target ->
  ?cond:target -> ?step:target -> ?body:target -> string -> constr
val cWhile : ?cond:target -> ?body:target -> unit -> constr
val cDoWhile : ?body:target -> ?cond:target -> unit -> constr
val cIf : ?cond:target -> ?then_:target -> ?else_:target -> unit -> constr
val cThen : constr
val target_list_simpl : targets -> target_list_pred
val target_list_one_st : target -> target_list_pred
val target_list_all_st : target -> target_list_pred
val target_list_pred_default : target_list_pred
val combine_args : targets -> target_list_pred -> target_list_pred
val cFunDef :
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?body:target ->
  ?ret_typ:string ->
  ?ret_typ_pred:typ_constraint ->
  ?regexp:bool ->
  ?is_def:bool -> ?clang_id:Clang.cxcursor -> string -> constr
val cFunBody :
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?body:target ->
  ?ret_typ:string ->
  ?ret_typ_pred:typ_constraint ->
  ?regexp:bool ->
  ?is_def:bool -> ?clang_id:Clang.cxcursor -> string -> constr
val cFunDefAndDecl :
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?body:target ->
  ?ret_typ:string ->
  ?ret_typ_pred:typ_constraint ->
  ?regexp:bool -> ?clang_id:Clang.cxcursor -> string -> constr
val cTopFunDef :
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?body:target ->
  ?ret_typ:string ->
  ?ret_typ_pred:typ_constraint ->
  ?regexp:bool ->
  ?is_def:bool -> ?clang_id:Clang.cxcursor -> string -> constr
val cTopFunDefAndDecl :
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?body:target ->
  ?ret_typ:string ->
  ?ret_typ_pred:typ_constraint ->
  ?regexp:bool -> ?clang_id:Clang.cxcursor -> string -> constr
val cTopFunDefAndDeclReg : string -> constr
val cTopFunDefs : string list -> constr
val cTopFunDefReg : string -> constr
val cTop : ?regexp:bool -> string -> constr
val cTypDef : ?substr:bool -> ?regexp:bool -> string -> constr
val cDef : string -> constr
val cEnum :
  ?name:string ->
  ?substr:bool ->
  ?constants:(string * target) list -> ?regexp:bool -> unit -> constr
val cSeq : ?args:targets -> ?args_pred:target_list_pred -> unit -> constr
val cVar :
  ?regexp:bool ->
  ?substr:bool -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr
val cVarReg : string -> constr
val cLitPred : (Ast.lit -> bool) -> constr
val cLit : constr
val cIntPred : (int -> bool) -> constr
val cInt : int -> constr
val cDoublePred : (float -> bool) -> constr
val cDouble : float -> constr
val cBoolPred : (bool -> bool) -> constr
val cBool : bool -> constr
val cStringPred : (string -> bool) -> constr
val cString : string -> constr
val cCall :
  ?fun_:target ->
  ?args:targets ->
  ?args_pred:target_list_pred ->
  ?accept_encoded:bool -> ?regexp:bool -> string -> constr
val cFun :
  ?fun_:target ->
  ?args:targets ->
  ?args_pred:target_list_pred -> ?regexp:bool -> string -> constr
val cFuns : string list -> constr
val cPrimPred : (Ast.prim -> bool) -> constr
val cPrim : Ast.prim -> constr
val cPrimPredFun :
  ?args:targets ->
  ?args_pred:target_list_pred -> (Ast.prim -> bool) -> constr
val cPrimFun :
  ?args:targets -> ?args_pred:target_list_pred -> Ast.prim -> constr
val cPrimFunArith :
  ?args:targets -> ?args_pred:target_list_pred -> unit -> constr
val cBinop : ?lhs:target -> ?rhs:target -> Ast.binary_op -> constr
val cPrimNew : ?arg:target -> unit -> constr
val dVarInit : constr
val dInit : constr
val cWrite :
  ?lhs:target ->
  ?rhs:target -> ?typ:string -> ?typ_pred:typ_constraint -> unit -> constr
val cRead : ?addr:target -> unit -> constr
val cReadOrWrite : ?addr:target -> unit -> constr
val cWriteVar :
  ?regexp:bool ->
  ?substr:bool -> ?typ:string -> ?typ_pred:typ_constraint -> string -> constr
val cReadVar : string -> constr
val cMark : string -> constr
val cMarks : string list -> constr
val cMarkSt : (string -> bool) -> constr
val cMarkAny : constr
val cLabel : ?substr:bool -> ?body:target -> ?regexp:bool -> string -> constr
val cGoto : ?label:string -> ?substr:bool -> ?regexp:bool -> unit -> constr
val cReturn_tg : ?res:target -> unit -> constr
val cAbrtAny : abort_kind
val cAbrtRet : abort_kind
val cAbrtBrk : abort_kind
val cAbrtCtn : abort_kind
val cAbort : ?kind:abort_kind -> unit -> constr
val cReturn : constr
val cBreak : constr
val cContinue : constr
val cAny : constr
val cChoose : constr
val cAlloc : int option -> constr
val cMalloc : ?d:int -> unit -> constr
val cMindex : ?d:int -> ?args:targets -> unit -> constr
val cCalloc : ?d:int -> unit -> constr
val cSwitch :
  ?cond:target -> ?cases:(case_kind * target) list -> unit -> constr
val cCase : ?value:target -> unit -> case_kind
val cDefault : case_kind
val dLHS : constr
val dRHS : constr
val cTargetInDepth : target -> constr
val cAccesses :
  ?base:target ->
  ?accesses:constr_access list -> ?inner_accesses:bool -> unit -> constr
val cIndex : ?index:target -> unit -> constr_access
val cField :
  ?field:string -> ?substr:bool -> ?regexp:bool -> unit -> constr_access
val cAccess : constr_access
val cFieldAccess :
  ?base:target ->
  ?substr:bool -> ?regexp:bool -> ?field:string -> unit -> constr
val cFieldRead :
  ?field:string ->
  ?base:target -> ?substr:bool -> ?regexp:bool -> unit -> constr
val cFieldWrite :
  ?base:target ->
  ?substr:bool ->
  ?regexp:bool -> ?rhs:target -> ?field:string -> unit -> constr
val cFieldReadOrWrite :
  ?base:target ->
  ?substr:bool -> ?regexp:bool -> ?field:string -> unit -> constr
val cCellAccess : ?base:target -> ?index:target -> unit -> constr
val cCellRead : ?base:target -> ?index:target -> unit -> constr
val cCellWrite :
  ?base:target -> ?rhs:target -> ?index:target -> unit -> constr
val cCellReadOrWrite : ?base:target -> ?index:target -> unit -> constr
val cArrayInit : constr
val cStructInit : constr
val cCell : ?cell_index:int -> unit -> constr
val cArrayWrite : string -> constr
val cArrayWriteAccess : string -> constr
val cArrayRead : ?index:target -> string -> constr
val cPlusEq : target -> constr
val cOmp_match_all : Ast.directive -> bool
val cOmp : ?pred:(Ast.directive -> bool) -> unit -> constr
val cNamespace : ?substr:bool -> ?regexp:bool -> string -> constr
val check : target -> unit
val filter_constr_occurrence : target -> target
val enable_multi_targets : target -> target
val relative_target : target -> target
module Transfo :
  sig
    type t = target_simple -> unit
    type local = Ast.trm -> path -> Ast.trm
    type local_between = int -> local
  end
val apply_on_path : (Ast.trm -> Ast.trm) -> Ast.trm -> Path.path -> Ast.trm
val convert_stringreprs_from_documentation_to_string :
  AstC_to_c.stringreprs -> (int, string) Hashtbl.t
val compute_stringreprs :
  ?optitrust_syntax:bool ->
  ?topfuns:Constr.constr_name list ->
  (Ast.trm -> bool) -> Ast.trm -> Ast.trm * AstC_to_c.stringreprs
val compute_stringreprs_and_update_ast :
  ?optitrust_syntax:bool -> (Ast.trm -> bool) -> AstC_to_c.stringreprs
val debug_disappearing_mark : bool
exception Interrupted_applyi_on_transformed_targets of Ast.trm
val fix_target : target -> target
val with_stringreprs_available_for :
  target list -> Ast.trm -> (Ast.trm -> 'a) -> 'a
val resolve_target_with_stringreprs_available : target -> Ast.trm -> paths
val resolve_target_exactly_one_with_stringreprs_available :
  target -> Ast.trm -> path
val resolve_path_with_stringreprs_available : path -> Ast.trm -> Ast.trm
val path_of_target_mark_one : string -> Ast.trm -> path
val resolve_target_mark_one_else_any : string -> Ast.trm -> paths
val resolve_target_between_mark_one_else_any :
  string -> Ast.trm -> (path * int) list
val applyi_on_transformed_targets :
  ?rev:bool ->
  (path -> 'a) -> (int -> Ast.trm -> 'a -> Ast.trm) -> target -> unit
val apply_on_transformed_targets :
  ?rev:bool -> (path -> 'a) -> (Ast.trm -> 'a -> Ast.trm) -> target -> unit
val applyi_on_targets : (int -> Ast.trm -> path -> Ast.trm) -> target -> unit
val apply_on_targets : (Ast.trm -> path -> Ast.trm) -> target -> unit
val transfo_on_targets : (Ast.trm -> Ast.trm) -> target -> unit
val iteri_on_transformed_targets :
  ?rev:bool ->
  (path -> 'a) -> (int -> Ast.trm -> 'a -> unit) -> target -> unit
val iter_on_transformed_targets :
  ?rev:bool -> (path -> 'a) -> (Ast.trm -> 'a -> unit) -> target -> unit
val iteri_on_targets :
  ?rev:bool -> (int -> Ast.trm -> path -> unit) -> target -> unit
val iter_on_targets :
  ?rev:bool -> (Ast.trm -> path -> unit) -> target -> unit
val applyi_on_transformed_targets_between :
  (path * int -> 'a) -> (int -> Ast.trm -> 'a -> Ast.trm) -> target -> unit
val apply_on_transformed_targets_between :
  (path * int -> 'a) -> (Ast.trm -> 'a -> Ast.trm) -> target -> unit
val applyi_on_targets_between :
  (int -> Ast.trm -> path * int -> Ast.trm) -> target -> unit
val apply_on_targets_between :
  (Ast.trm -> path * int -> Ast.trm) -> target -> unit
val trm_add_marks_at_paths : string list -> paths -> Ast.trm -> Ast.trm
val trm_add_mark_at_paths :
  (path -> Ast.trm -> string) -> paths -> Ast.trm -> Ast.trm
val iteri : ?rev:bool -> (int -> Ast.trm -> path -> unit) -> target -> unit
val iter : (Ast.trm -> path -> unit) -> target -> unit
val applyi : (int -> Ast.trm -> path -> Ast.trm) -> target -> unit
val apply : (Ast.trm -> path -> Ast.trm) -> target -> unit
val apply_at_target_paths : (Ast.trm -> Ast.trm) -> target -> unit
val apply_at_target_paths_before :
  (Ast.trm -> int -> Ast.trm) -> target -> unit
val target_show_aux : ?types:bool -> string -> Ast.trm -> Ast.trm
val target_show_transfo : ?types:bool -> string -> Transfo.local
val target_between_show_aux : string -> int -> Ast.trm -> Ast.trm
val target_between_show_transfo : string -> Transfo.local_between
val show_next_id : unit -> int
val show_next_id_reset : unit -> unit
val show : ?line:int -> ?types:bool -> target -> unit
val show_ast : ?line:int -> unit -> unit
val show_res : ?line:int -> unit -> unit
val show_type : ?line:int -> target -> unit
val get_trm_at_exn : target -> Ast.trm
val get_trm_at : target -> Ast.trm option
val get_ast : unit -> Ast.trm
val get_function_name_at : path -> string option
val get_toplevel_function_name_containing : path -> string option
val reparse_only : ?update_cur_ast:bool -> string list -> unit
val get_relative_type : target -> target_relative option
val reparse_after :
  ?update_cur_ast:bool -> ?reparse:bool -> Transfo.t -> target -> unit
val resolve_target_current_ast : target -> paths
val resolve_path_current_ast : path -> Ast.trm
val path_of_target_mark_one_current_ast : string -> path
val ( ~~ ) : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
