val failwith : ('a, unit, string, 'b) format4 -> 'a
val sprintf : ('a, unit, string) format -> 'a
type pos = { pos_line : int; pos_col : int; }
type trm_loc = { loc_file : string; loc_start : pos; loc_end : pos; }
type location = trm_loc option
val loc_to_string : location -> string
type loc = int
type mark = string
type marks = mark list
val no_mark : string
type 'a mlist = 'a Mlist.t
type var_id = loc
val unset_var_id : int
type var = { id : var_id; namespaces : mark list; name : mark; }
val has_unset_id : var -> bool
val is_toplevel_var : var -> bool
val is_anon_var : var -> bool
val var_has_name : string -> var -> bool
val qualified_name_to_string : string list -> string -> string
val var_name : var -> string
val var_to_string : var -> string
exception UnsetVarId of var
val assert_var_id_set : var -> unit
val var_eq : var -> var -> bool
module Var :
  sig
    type t = var
    val compare : var -> var -> var_id
    val equal : var -> var -> bool
    val hash : var -> var_id
  end
type vars = var list
module Var_set :
  sig
    type elt = var
    type t = Set.Make(Var).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> var_id
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> var_id
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
module Var_map :
  sig
    type key = var
    type 'a t = 'a Map.Make(Var).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> loc) -> 'a t -> 'a t -> loc
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> loc
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
module Var_Hashtbl :
  sig
    type key = var
    type 'a t = 'a Hashtbl.Make(Var).t
    val create : var_id -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> var_id
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type 'a varmap = 'a Var_map.t
val var_map_of_list : (Var_map.key * 'a) list -> 'a Var_map.t
val vars_to_string : var list -> string
val next_var_id : unit -> int
val fresh_var_name : ?prefix:string -> unit -> string
module Qualified_name :
  sig
    type t = mark list * mark
    val compare : t -> t -> var_id
    val equal : t -> t -> bool
  end
module Qualified_set :
  sig
    type elt = Qualified_name.t
    type t = Set.Make(Qualified_name).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> var_id
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> var_id
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
module Qualified_map :
  sig
    type key = Qualified_name.t
    type 'a t = 'a Map.Make(Qualified_name).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare : ('a -> 'a -> var_id) -> 'a t -> 'a t -> var_id
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> var_id
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
type stringreprid = var_id
val next_stringreprid : unit -> stringreprid
type field = mark
type fields = field list
type label = field
type 'a labelmap = 'a Tools.String_map.t
type labels = label list
val no_label : string
type loop_dir = DirUp | DirUpEq | DirDown | DirDownEq
val pp_loop_dir : Format.formatter -> loop_dir -> unit
val show_loop_dir : loop_dir -> string
type code_kind =
    Lit of label
  | Typ of label
  | Expr of label
  | Stmt of label
  | Instr of label
  | Comment of label
val pp_code_kind : Format.formatter -> code_kind -> unit
val show_code_kind : code_kind -> string
val code_to_str : code_kind -> string
type trm = {
  desc : trm_desc;
  annot : trm_annot;
  loc : location;
  mutable typ : typ option;
  mutable ctx : ctx;
  mutable errors : label list;
}
and trms = trm list
and trm_desc =
    Trm_var of var
  | Trm_lit of lit
  | Trm_prim of typ * prim
  | Trm_let of typed_var * trm
  | Trm_let_mult of (typed_var * trm) list
  | Trm_predecl of typed_var
  | Trm_fun of typed_vars * typ * trm * fun_spec
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm
  | Trm_seq of trm mlist * var option
  | Trm_apps of trm * trm list * resource_item list * (var option * var) list
  | Trm_for of loop_range * trm * loop_contract
  | Trm_for_c of trm * trm * trm * trm * resource_set option
  | Trm_while of trm * trm
  | Trm_do_while of trm * trm
  | Trm_switch of trm * (trms * trm) list
  | Trm_abort of abort
  | Trm_goto of label
  | Trm_arbitrary of code_kind
  | Trm_omp_routine of omp_routine
  | Trm_extern of label * trms
  | Trm_namespace of label * trm * bool
  | Trm_template of template_parameter_list * trm
  | Trm_using_directive of label
and typ = trm
and typvar = var
and typed_var = var * typ
and typed_vars = typed_var list
and typedef = { typedef_name : typvar; typedef_body : typedef_body; }
and record_members = (record_member * record_member_annot) list
and record_member = Record_field of (label * typ) | Record_method of trm
and record_member_annot = access_control
and access_control =
    Access_public
  | Access_private
  | Access_protected
  | Access_unspecified
and typedef_body =
    Typedef_alias of typ
  | Typedef_record of record_members
  | Typedef_enum of (var * trm option) list
and cstyle_annot =
    No_struct_get_arrow
  | Empty_cond
  | Fun_inline
  | Ternary_cond
  | Shortcircuit_and
  | Shortcircuit_or
  | No_braces of stringreprid
  | Prefix_step
  | Postfix_step
  | Reference
  | Struct
  | Rec_struct
  | Class
  | Static_fun
  | Method_call
  | Implicit_this
  | Typ_arguments of typ list
  | Closure
  | Implicit_constructor
  | Explicit_constructor
  | Default_constructor
  | Const_method
  | Method
  | Constructed_init
  | Class_constructor of constructor_kind
  | Class_destructor of destructor_kind
  | Member_initializer
  | Brace_init
  | Display_null_uppercase
  | ResourceFormula
  | Type
  | InjectedClassName
  | BodyHiddenForLightDiff
and constructor_kind =
    Constructor_implicit
  | Constructor_explicit
  | Constructor_default
  | Constructor_simpl
and destructor_kind =
    Destructor_default
  | Destructor_delete
  | Destructor_simpl
and file_annot = Inside_file | Main_file | Included_file of label
and cpragma = directive
and attribute = Alignas of trm | GhostInstr
and trm_annot = {
  trm_annot_attributes : attribute list;
  trm_annot_marks : marks;
  trm_annot_labels : labels;
  trm_annot_stringrepr : stringreprid option;
  trm_annot_pragma : cpragma list;
  trm_annot_cstyle : cstyle_annot list;
  trm_annot_file : file_annot;
  trm_annot_referent : trm option;
}
and unary_op =
    Unop_get
  | Unop_address
  | Unop_bitwise_neg
  | Unop_neg
  | Unop_minus
  | Unop_plus
  | Unop_post_incr
  | Unop_post_decr
  | Unop_pre_incr
  | Unop_pre_decr
  | Unop_struct_access of label
  | Unop_struct_get of label
  | Unop_cast of typ
and binary_op =
    Binop_set
  | Binop_array_access
  | Binop_array_get
  | Binop_eq
  | Binop_neq
  | Binop_add
  | Binop_sub
  | Binop_mul
  | Binop_exact_div
  | Binop_trunc_div
  | Binop_trunc_mod
  | Binop_le
  | Binop_lt
  | Binop_ge
  | Binop_gt
  | Binop_bitwise_and
  | Binop_bitwise_or
  | Binop_shiftl
  | Binop_shiftr
  | Binop_xor
  | Binop_faa
and consistency_mode = Sequentially_consistent | Release | Acquire
and prim =
    Prim_unop of unary_op
  | Prim_binop of binary_op
  | Prim_compound_assign_op of binary_op
  | Prim_ref
  | Prim_ref_uninit
  | Prim_new
  | Prim_new_uninit
  | Prim_delete
  | Prim_array
  | Prim_record
and lit =
    Lit_unit
  | Lit_bool of bool
  | Lit_int of typ * stringreprid
  | Lit_float of typ * float
  | Lit_string of label
  | Lit_null of typ
and loop_range = {
  index : var;
  start : trm;
  direction : loop_dir;
  stop : trm;
  step : trm;
}
and ctx = {
  mutable ctx_resources_before : resource_set option;
  mutable ctx_resources_usage : resource_usage_map option;
  mutable ctx_resources_contract_invoc : contract_invoc option;
  mutable ctx_resources_after : resource_set option;
  mutable ctx_resources_post_inst : used_resource_set option;
  mutable ctx_resources_exectime : float;
}
and formula = trm
and resource_item = var * formula
and resource_set = {
  pure : resource_item list;
  linear : resource_item list;
  fun_specs : fun_spec_resource varmap;
  aliases : trm varmap;
  struct_fields : (label * typ) list varmap;
}
and fun_spec_resource = {
  args : var list;
  contract : fun_contract;
  inverse : var option;
}
and fun_contract = { pre : resource_set; post : resource_set; }
and fun_spec =
    FunSpecUnknown
  | FunSpecContract of fun_contract
  | FunSpecReverts of var
and loop_contract = {
  loop_ghosts : resource_item list;
  invariant : resource_set;
  parallel_reads : resource_item list;
  parallel_atomic : resource_item list;
  iter_contract : fun_contract;
  strict : bool;
}
and used_resource_item = {
  hyp : var;
  inst_by : formula;
  used_formula : formula;
}
and used_resource_set = {
  used_pure : used_resource_item list;
  used_linear : used_resource_item list;
}
and produced_resource_set = {
  produced_res : resource_set;
  contract_hyp_names : var varmap;
}
and resource_usage =
    Required
  | Ensured
  | ArbitrarilyChosen
  | Cleared
  | ConsumedFull
  | ConsumedUninit
  | SplittedFrac
  | JoinedFrac
  | Produced
and resource_usage_map = resource_usage varmap
and contract_invoc = {
  contract_frame : resource_item list;
  contract_inst : used_resource_set;
  contract_produced : produced_resource_set;
  contract_joined_resources : (var * var) list;
}
and template_param_kind =
    Typename of typ option
  | NonType of typ * trm option
and template_parameter_list = (var * template_param_kind) list
and abort =
    Ret of trm option
  | Break of label option
  | Continue of label option
and mode = Shared_m | None_
and expression = label
and sched_type = Static | Dynamic | Guided | Runtime
and reduction_identifier =
    Plus
  | Minus
  | Prod
  | And
  | Or
  | Power
  | BitAnd
  | BitOr
  | Min
  | Max
and map_type = Alloc | To | From | ToFrom | No_map
and proc_bind = Master_pb | Close | Spread
and dep = Dep_var of var | Dep_ptr of dep
and deps = dep list
and dependence_type =
    In of deps
  | Out of deps
  | Inout of deps
  | Outin of deps
  | Sink of deps
  | Source
and clause =
    Default of mode
  | Shared of vars
  | Private of vars
  | FirstPrivate of vars
  | LastPrivate of vars
  | Linear of vars * stringreprid
  | Reduction of reduction_identifier * vars
  | Copyin of vars
  | CopyPrivate of vars
  | Map_c of map_type * vars
  | Defaultmap of map_type * vars
  | Safelen of stringreprid
  | Collapse of stringreprid
  | Simdlen of stringreprid
  | Aligned of vars * stringreprid
  | Uniform of vars
  | Inbranch
  | NotInbranch
  | Nowait
  | Ordered_c of stringreprid
  | If of expression
  | Device of var
  | Num_threads of var
  | Schedule of sched_type * var
  | Dist_schedule of sched_type * var
  | Parallel_c
  | Section_c
  | For_c
  | Taskgroup_c
  | Proc_bind of proc_bind
  | Priority of var
  | Depend of dependence_type list
  | Grainsize of stringreprid
  | Mergeable
  | Nogroup
  | Num_tasks of stringreprid
  | Untied
  | Final of expression
  | To_c of vars
  | From_c of vars
  | Link of vars
  | Num_teams of var
  | Thread_limit of var
and atomic_operation = Read | Write | Update | Capture
and directive =
    Atomic of atomic_operation option
  | Atomic_capture
  | Barrier
  | Cancel of clause * clause list
  | Cancellation_point of clause * clause list
  | Critical of var * label
  | Declare_simd of clause list
  | Declare_reduction of reduction_identifier * label list * expression *
      clause
  | Declare_target of clause list
  | Distribute of clause list
  | Distribute_parallel_for of clause list
  | Distribute_parallel_for_simd of clause list
  | Distribute_simd
  | End_declare_target
  | Flush of vars
  | For of clause list
  | For_simd of clause list
  | Master
  | Ordered of clause list
  | Parallel of clause list
  | Parallel_for of clause list
  | Parallel_for_simd of clause list
  | Parallel_sections of clause list
  | Section
  | Sections of clause list
  | Simd of clause list
  | Single of clause list
  | Target of clause list
  | Target_data of clause list
  | Target_enter_data of clause list
  | Target_exit_data of clause list
  | Target_teams of clause list
  | Target_teams_distribute of clause list
  | Target_teams_distribute_parallel_for of clause list
  | Target_teams_distribute_parallel_for_simd of clause list
  | Target_teams_distribute_simd of clause list
  | Target_update of clause list
  | Task of clause list
  | Taskgroup
  | Taskloop of clause list
  | Taskloop_simd of clause list
  | Taskwait of clause list
  | Taskyield
  | Teams of clause list
  | Teams_distribute of clause list
  | Teams_distribute_end of clause list
  | Teams_distribute_parallel_for of clause list
  | Teams_distribute_parallel_for_simd of clause list
  | Threadprivate of vars
and omp_routine =
    Set_num_threads of stringreprid
  | Get_num_threads
  | Get_max_threads
  | Get_thread_num
  | Get_num_procs
  | In_parallel
  | Set_dynamic of stringreprid
  | Get_dynamic
  | Get_cancellation
  | Set_nested of stringreprid
  | Get_nested
  | Set_schedule of sched_type * stringreprid
  | Get_schedule of sched_type * stringreprid
  | Get_thread_limit
  | Set_max_active_levels of stringreprid
  | Get_max_active_levels
  | Get_level
  | Get_ancestor_thread_num
  | Get_team_size of stringreprid
  | Get_active_level
  | In_final
  | Get_proc_bind
  | Set_default_device of var
  | Get_default_device
  | Get_num_devices
  | Get_num_teams
  | Get_team_num
  | Is_initial_device
  | Init_lock of var
  | Init_nest_lock of var
  | Destroy_lock of var
  | Destroy_nest_lock of var
  | Set_lock of var
  | Set_nest_lock of var
  | Unset_lock of var
  | Unset_nest_lock of var
  | Test_lock of var
  | Test_nest_lock of var
  | Get_wtime
  | Get_wtick
val new_var : ?namespaces:string list -> string -> var
val name_to_var : ?namespaces:string list -> string -> var
module Toplevel_id :
  sig
    type t = stringreprid
    val compare : stringreprid -> stringreprid -> stringreprid
    val equal : stringreprid -> stringreprid -> bool
    val hash : 'a -> 'a
    val from_qualified_name :
      namespaces:expression list -> expression -> stringreprid
  end
module Toplevel_hashtbl :
  sig
    type key = stringreprid
    type 'a t = 'a Hashtbl.Make(Toplevel_id).t
    val create : stringreprid -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_opt : 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> stringreprid
    val stats : 'a t -> Hashtbl.statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : 'a t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end
val toplevel_vars : var Toplevel_hashtbl.t
val toplevel_var : ?namespaces:string list -> string -> var
val dummy_var : var
val trm_desc_to_string : trm_desc -> string
val resource_usage_opt_to_string : resource_usage option -> string
type tmap = formula varmap
val unknown_ctx : unit -> ctx
val empty_resource_set : resource_set
val empty_fun_contract : fun_contract
val empty_loop_contract : loop_contract
val empty_strict_loop_contract : loop_contract
val trm_annot_default : trm_annot
val incr_trm_alloc : (unit -> unit) ref
val trm_make :
  ?annot:trm_annot ->
  ?loc:trm_loc ->
  ?typ:typ -> ?ctx:ctx -> ?errors:string list -> trm_desc -> trm
val trm_alter :
  ?annot:trm_annot ->
  ?loc:location ->
  ?typ:typ -> ?ctx:ctx -> ?errors:string list -> ?desc:trm_desc -> trm -> trm
val trm_replace : trm_desc -> trm -> trm
val trm_like : old:trm -> trm -> trm
type local_ops =
    Local_arith of lit * binary_op
  | Local_obj of typvar * typvar * typvar
