(* for debugging and message printing *)
let printf = Printf.printf
let sprintf = Printf.sprintf

(* [pos]: record used to represent a specific location inside the code *)
type pos = {
    pos_line : int;
    pos_col : int; }

(* [trm_loc]: record used to keep track of the trm information like the
file it belongs, and the start and the end positions inside the code. *)
type trm_loc = {
  loc_file : string;
  loc_start : pos;
  loc_end : pos;}

(* [location]: an optional type representing the location of a trm *)
type location = trm_loc option

(* [loc]: memory location *)
type loc = int

(* [mark]: annotation used for resolving targets, see module mark.ml *)
type mark = Mark.t

(* [mlists]: generalized lists, see module mlist.ml *)
type 'a mlist = 'a Mlist.t

(* [strm]: string representation of a term, as provided by the user *)
type strm = string

(* [styp]: string representation of a type, as provided by the user *)
type styp = string

(* [var]: variable *)
type var = string

(* [Var_set]: a set module used for storing variables *)
module Var_set = Set.Make(String)

(* [vars]: variables, a list of elements of type variable *)
type vars = var list

(* let vars_to_string vs = Tools.list_to_string vs *)
let vars_to_string vs = Trace_printers.(list_arg_printer string_arg_printer vs)

(* [qvar]: qualiefied variables, Ex M :: N :: x
    qx = {qvar_var = "x"; qvar_path = ["M"; "N"]; qvar_str = "M :: N :: x"}. *)
type qvar = {qvar_var : var; qvar_path : var list; qvar_str : string}

(* [typconstr]: name of type constructors (e.g. [list] in Ocaml's type [int list];
   or [vect] in C type [struct { int x,y }; *)
type typconstr = string

(* [typvar]: name of type variables (e.g. ['a] in type ['a list] *)
type typvar = var

(* [qtypvar]: qname of the type variables *)
type qtypvar = qvar

(* [typvars]: a list of typvar *)
type typvars = typvar list

(* [typconstrid]: unique identifier for typ constructors*)
type typconstrid = int

(* [next_typconstrid ()] generates and return a new id *)
let next_typconstrid : (unit -> typconstrid) =
  Tools.fresh_generator ()

(* [stringreprid]: unique identifier used as keys for memoization of string representation of subterms *)
type stringreprid = int

(* [next_stringreprid ()] generates and return a new string representation id *)
let next_stringreprid : (unit -> stringreprid) =
  Tools.fresh_generator ()

(* ['a typmap] is a map from [typeid] to ['a] *)
module Typ_map = Map.Make(Int)

(* [typmap]: instantiation of Typ_map *)
type 'a typmap = 'a Typ_map.t

(* [field]: struct field defined as a string *)
type field = string

(* [fields]: struct fields as a list of fields *)
type fields = field list

(* ['a varmap] is a map from string to ['a] *)
module String_map = Map.Make(String)

(* [varmap]: instantiation of String_map *)
type 'a varmap = 'a String_map.t

(* [label]: labels (for records) *)
type label = string

(* [labels]: a list of labels. *)
type labels = label list

(* [string_trm]: description of a term as a string (convenient for the user) *)
type string_trm = string

(* [constrname]: constructor name (for enum and algebraic datatypes) *)
type constrname = string

(* [size]: array sizes *)
type size =
  | Undefined    (* t[] *)
  | Const of int (* t[3] *)
  | Trm of trm   (* t[2*nb] *)

(* [loop_step]: loop step kinds *)
and loop_step =
  | Pre_inc     (* ++i   *)
  | Post_inc    (* i++   *)
  | Pre_dec     (* --i   *)
  | Post_dec    (* i--   *)
  | Step of trm (* i += 2*)

(* [loop_dir]: loop bound inequalities *)
and loop_dir =
  | DirUp      (* i < 2  *)
  | DirUpEq    (* i <= 2 *)
  | DirDown    (* i > 0  *)
  | DirDownEq  (* i >= 0 *)

(* [code_kind]; code kind entered by the user *)
and code_kind =
  | Lit of string   (* 1, "hello", 1.0, (), true, false *)
  | Atyp of string  (* int, double float, vect, particle *)
  | Expr of string  (* expression of the form a * (b + 1) *)
  | Stmt of string  (* functions, for loops, while loops etc *)
  | Instr of string (* a = b, a += b *)

(* [typ_desc]: type description *)
and typ_desc =
  | Typ_const of typ   (* e.g. [const int *] is a pointer on a [const int] type. *)
  | Typ_var of typvar * typconstrid  (* e.g. ['a] in the type ['a -> 'a] -- *)
  | Typ_constr of qtypvar * typconstrid * typ list (* e.g. [int list] or
                                                  [(int,string) map] or [vect] *)
  | Typ_auto                                (* auto *)
  | Typ_unit                                (* void *)
  | Typ_int                                 (* int *)
  | Typ_float                               (* float *)
  | Typ_double                              (* double *)
  | Typ_bool                                (* bool *)
  | Typ_char                                (* char *)
  | Typ_string                              (* string a *)
  | Typ_ptr of
    {ptr_kind : ptr_kind; inner_typ: typ }  (* "int*" *)
  | Typ_array of typ * size                 (* int[3], or int[], or int[2*n] *)
  | Typ_fun of (typ list) * typ             (* int f(int x, int y) *)
  | Typ_record of record_type * typ         (* class, struct, union *)
  | Typ_template_param of string            (* template(Soon..) *)
  | Typ_arbitrary of code_kind              (* types entered as string  *)
  | Typ_decl of trm                        (* Since C++11, decltype (nullptr), create a type out of an expression *)

(* [ptr_kind]: type used for distinguishing pointers from references, note that
    both pointers and references are considered by OptiTrust as pointers.*)
and ptr_kind =
  | Ptr_kind_mut   (* int* *)
  | Ptr_kind_ref   (* int& *)

(* [typ_annot]: annotation for types that can be build from the main ones *)
and typ_annot =
  | Unsigned  (* unsigned int *)
  | Long      (* long int *)
  | Short     (* short int *)

(* [typ]: is a record containing the description, annotation and some attributes*)
and typ = {
  typ_desc : typ_desc;
  typ_annot : typ_annot list;
  typ_attributes : attribute list;
  }

(* [typedef]: is a record containing the id of the type, the name of the new defined
    type, for sum types there can be also more then one variable. And finally the
     body of the type *)
and typedef = {
  typdef_loc : location;      (* the location of the typedef *)
  typdef_typid : typconstrid; (* the unique id associated with the type [t] *)
  typdef_tconstr : typconstr; (* the name [t] *)
  typdef_vars : typvars;      (* the list containing the names ['a] and ['b];
         [typedef_vars] is always the empty list in C code without templates *)
  typdef_body : typdef_body;(* the body of the definition,
                            i.e. the description of [...] *)
}


(* [record_fields]: fields representation for classes, structs and unions. *)
and record_fields = (record_field * record_field_annot) list

and record_field =
  | Record_field_member of (label * typ)
  | Record_field_method of trm

and record_field_annot = access_control

and access_control =
  | Access_public
  | Access_private
  | Access_protected
  | Access_unspecified


(* [typedef_body]: typedef kinds *)
and typdef_body =
  | Typdef_alias of typ   (* for abbreviations, e.g. [type 'a t = ('a * 'a)
                          list] or [typdef vect t] *)
  | Typdef_record of record_fields
    (* for records / struct, e.g. [type 'a t = { f : 'a; g : int } *)
  | Typdef_sum of (constrname * typ) list (* for algebraic definitions / enum,
                                             e.g. [type 'a t = A | B of 'a] *)
  | Typdef_enum of (var * (trm option)) list (* for C/C++ enums *)

(* [typed_var]: used for function arguments *)
and typed_var = var * typ

(* [typed_vars]: a list of typed_var *)
and typed_vars = typed_var list


(* [loop_parallel]: for parallel loops this flag is set to true *)
and loop_parallel = bool

(* [loop_range]: a type for representing  for loops *)
and loop_range = var * trm * loop_dir * trm * loop_step * loop_parallel

(* [unary_op]: unary operators *)
and unary_op =
  | Unop_get                     (* the "*" operator as in *p  *)
  | Unop_address                 (* the "&" operator as in &p *)
  | Unop_bitwise_neg             (* ~ *)
  | Unop_neg                     (* !true *)
  | Unop_minus                   (* -a *)
  | Unop_plus                    (* +a *)
  | Unop_post_inc                (* x++ *)
  | Unop_post_dec                (* x-- *)
  | Unop_pre_inc                 (* ++x *)
  | Unop_pre_dec                 (* --x *)
  | Unop_struct_access of field  (* struct access encoding*)
  | Unop_struct_get of field     (* struct access *)
  | Unop_cast of typ             (* (int)x *)

(* LATER: numeric operation takes a type argument *)
(* [binary_op]: binary operators *)
and binary_op =
  | Binop_set           (* lvalue = rvalue *)
  | Binop_array_access  (* array acces encoding *)
  | Binop_array_get     (* array access *)
  | Binop_eq            (* a = b *)
  | Binop_neq           (* a != b *)
  | Binop_sub           (* a - b *)
  | Binop_add           (* a + b *)
  | Binop_mul           (* a * b *)
  | Binop_mod           (* a % b *)
  | Binop_div           (* a / b *)
  | Binop_exact_div     (* a / b with a % b = 0 *)
  | Binop_le            (* a <= b *)
  | Binop_lt            (* a < b *)
  | Binop_ge            (* a >= b *)
  | Binop_gt            (* a > b *)
  | Binop_and           (* a && b *)
  | Binop_bitwise_and   (* a & b *)
  | Binop_or            (* a || b *)
  | Binop_bitwise_or    (* a | b *)
  | Binop_shiftl        (* a >> k*)
  | Binop_shiftr        (* a << k *)
  | Binop_xor           (* a ^ b *)
  (* LATER: add types to operations wherever relevant *)
  (* TODO: not coherent to use a grammar of binary_op for certain ops, and use conventional functions for others, eg. fmod *)
  (* | Binop_fmod          (* floatting point modulo, LATER: merge with mod when annotated with type *) *)


(* [consistency_mode]: C++ memory model consistency *)
and consistency_mode =
  | Sequentially_consistent
  | Release
  | Acquire

(* [prim]: primitives  *)
and prim =
  | Prim_unop of unary_op (* e.g. "!b" *)
  | Prim_binop of binary_op (* e.g. "n + m" *)
  | Prim_compound_assgn_op of binary_op (* e.g. "a += b" *)
  | Prim_overloaded_op of prim (* used for overloaded operators *)
  | Prim_new of typ (* "new T" *)
  | Prim_conditional_op (* "(foo) ? x : y" *)

(* [lit]: literals *)
and lit =
  | Lit_unit              (* void, e.g. "return;" is represented as "Lit_unit" *)
  | Lit_uninitialized     (* e.g. "int x;" is "int x = Lit_uninitalized" *)
  | Lit_bool of bool      (* true, false *)
  | Lit_int of int        (* 1, 10, 100 *)
  | Lit_double of float   (* 1.0, 2.0, 0.5 *)
  | Lit_string of string  (* "hello"  *)
  | Lit_nullptr               (* nullptr *)

(* [value]: values *)
and value =
  | Val_lit of lit   (* literal values *)
  | Val_prim of prim (* primitive values *)
  | Val_ptr of loc   (* pointer values, ex NULL *)
  (* These are values that can only be constructed during the program execution,
     and thus useful only for carrying out proofs about the program Generic *)
  (* LATER: add functions, which are also values that can be created at execution time *)

(* [trm_annot]: annotations are used to decorate this AST when it is built from
    the Clang AST in such a way to be able to print back the AST like
    the original C code.*)

(* [cstyle_annot]: annotations used for encodings and decodings. *)
and cstyle_annot =

  (* distinguish [p->f] vs [( *p ).f], represented as [get(access(p,f)],
     with an annotation carried by the [get] operation *)
  | Display_no_arrow

  (* [ for (int i = 0; ; i++) ]  vs [for (int i = 0; true; i++)],
     the latter form is used in the encoding. *)
  | Empty_cond      (* used for loops with empty conditions *)

  (* [inline] meta-information on a C-function *)
  | Fun_inline

  (* describe a sequence that does not impose a scope;
     LATER: maybe refine this notion *)
  | No_braces of int (* LATER: Add another category *)

  (* [int x, y]  encoded as [{ int x; int y}] with an annotation
     on this special kind of no-scope block *)
  | Multi_decl      (* annotation for encoding mutiple one line declarations *)

  (* DEPRECATED *)
  | Postfix_set     (* annotates all x++ and x-- unary operations aswrite operations *)

  (* [int& x = 3]  encoded as  [let x : ( int* ) = ref 3] in the internal AST *)
  | Reference

  (* annotation to distinguish [int x = 3]  vs [int* const x = ref 3]
     because the two have the same encoding in the internal AST.
     annotation is carried by the Trm_let. *) (* LATER: is the type also annotation? *)
  | Stackvar

  (* distinguish between class vs struct *)
  | Is_struct

  (*  [typedef struct node { int item; struct node* p } node; ]
      this flag [Is_rec_struct] indicates whether to reprint the type at the front. *)
  | Is_rec_struct

  (* distinguish between class vs struct -- seems redundant with is_struct *)
  | Is_class


  (* [static] meta-information on a C-function *)
  | Static_fun

  (* syntax [x.f(a)] vs [f(x,a)] because we encode "this" object as first argument *)
  (* LATER: beware when refering to the "nth" argument *)
  | Method_call

  (* syntax [x] where [x] is a class field instead of [this->x],
     which is the encoding in OptiTrust *)
  | Implicit_this (* Direct access to a class member. *)

  (* call to a polymorphic function   [f<int>(x)]
      where definition is [template<typename T> f(T x) { return x }] *)
  (* LATER: keep track of whether the user has written it explicitly;
     ---would be needed in particular when the return type is generic *)
  | Typ_arguments of typ list  (* <int, float> , type arguments used for template specializations. *)

  (* Automatically-synthesized constructors *)
  | Implicit_constructor
  | Explicit_constructor
  | Default_constructor

  (* [const] meta-information on a C++ method, to indicate that the object is not modified *)
  | Const_method

  (*  [class foo {  int x;  foo() : x(3) { bla} }] is encoded as
     [class foo {  int x; foo() { x = 3; bla  }], where [x=3] is tagged as Constructed_init
     LATER: verify.  *)
  | Constructed_init (* objects initialized with a constructor. *)

  (* LATER: document *)
  | Class_constructor of constructor_kind
  | Class_destructor of destructor_kind
  | Member_initializer

  (* LATER: what use? *)
  | Redundant_decl

  (* used for int[2] = { 3, 4 }, the trm_array is annotated with [Brace_init] *)
  | Brace_init

  | Clang_cursor of Clang.cxcursor

  (* tag for printing [NULL] instead of [nullptr] *)
  | Display_null_uppercase

(* [constructor_kind]: special annotation for constructors *)
and constructor_kind =
  | Constructor_implicit
  | Constructor_explicit
  | Constructor_default
  | Constructor_simpl

and destructor_kind =
  | Destructor_default
  | Destructor_delete
  | Destructor_simpl

(* [files_annot]: file annotation *)
and files_annot =
  | Include of string
  | Main_file

(* [cpragma]: type alias for directives *)
and cpragma = directive

(* [marks]: a list of marks *)
and marks = mark list

(* [trm_annot]: a record containing all kinds of annotations used on the AST of OptiTrust. *)
and trm_annot = {
    trm_annot_attributes : attribute list;
    trm_annot_marks : marks;
    trm_annot_labels : labels;
    trm_annot_stringrepr : stringreprid option;
    trm_annot_pragma : cpragma list;
    trm_annot_cstyle : cstyle_annot list;
    trm_annot_files : files_annot list;
  }
  (* LATER: use a smartconstruct for trm_annot with optional arguments *)

(* [attribute]: trm attributes *)
and attribute =
  | Alignas of trm (* alignas(64) double* deposit; *)
  | GeneratedTyp   (* pointers used only for encoding stack variables*)
  | Injected       (* injected type *)
  | Others         (* TO BE CONTINUED ... *)

(* [record_type]: C++ record types *)
and record_type =
  | Struct  (* struct *)
  | Union   (* union *)
  | Class   (* class *)

(* [trm] is a record representing an ast node *)
and trm =
 { annot : trm_annot;
   desc : trm_desc;
   loc : location;
   is_statement : bool;
   typ : typ option;
   ctx : ctx option}

(* [trms]: a list of trms *)
and trms = trm list

(* [ctx]: stores all the information about types, labels, constructors, etc. *)
and ctx = {
  ctx_var : typ varmap;             (* from [var] to [typ], i.e. giving the type
                                       of program variables *)
  ctx_tconstr : typconstrid varmap; (* from [typconstr] to [typconstrid] *)
  ctx_typedef : typedef typmap;     (* from [typconstrid] to [typedef] *)
  ctx_label : typconstrid varmap;   (* from [label] to [typconstrid] *)
  ctx_constr : typconstrid varmap;  (* from [constr] to [typconstrid] *)
  }

(* [trm_desc]: description of an ast node *)
and trm_desc =
  | Trm_val of value
  | Trm_var of varkind * qvar
  | Trm_array of trm mlist (* { 0, 3, 5} as an array *)
  | Trm_record of (label option * trm) mlist (* { 4, 5.3 } as a record *)
  | Trm_let of varkind * typed_var * trm (* int x = 3 *)
  | Trm_let_mult of varkind * typed_vars * trm list
  | Trm_let_fun of qvar * typ * typed_vars * trm
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm (* if (x > 0) {x += 1} else{x -= 1} *)
  | Trm_seq of trm mlist      (* { st1; st2; st3 } *)
  | Trm_apps of trm * (trms)  (* f(t1, t2) *)
  | Trm_while of trm * trm    (* while (t1) { t2 } *)
  | Trm_for of loop_range  * trm
  | Trm_for_c of trm * trm * trm * trm
  | Trm_do_while of trm * trm
  (* Remark: in the AST, arguments of cases that are enum labels
     appear as variables, hence the use of terms as opposed to
     closed values to represent case arguments.
    Trm_switch (cond, [([t1; t2], b1); ([t3], b2); ([], b3)]) =
    switch (cond) {
      case t1:
      case t2:
        b1;
        break;
      case t3:
        b2;
        break;
      default:
        b3;
        break;
    }
   *)
  | Trm_switch of trm * ((trms * trm) list)
  | Trm_abort of abort                            (* return or break or continue *)
  | Trm_goto of label                             (* goto foo *)
  | Trm_arbitrary of code_kind                    (* "int x = 10" *)
  | Trm_omp_routine of omp_routine                (* get_thread_id *)
  | Trm_extern of string * trms                   (* extern keyword *)
  | Trm_namespace of string * trm * bool          (* namespaces *)
  | Trm_template of template_parameter_list * trm (* templates *)
  | Trm_using_directive of string                 (* using namespace std *)
  | Trm_fun of typed_vars * typ option * trm      (* anonymous functions, [&](int const& x) -> void ({r += x;}) *)
  | Trm_delete of bool * trm                      (* delete t, delete[] t *)

(* [template_param_kind]: parameters kind, typename , empty or another template *)
and template_param_kind =
  | Type_name of typ option               (* <T> *)
  | NonType of typ * trm option           (* <> *)
  | Template of template_parameter_list   (* vect<int, int> (i,j) *)

(* [template_parameter_list]: template parameter list *)
and template_parameter_list = (string * template_param_kind * bool) list

(* [varkind]: type for the mutability of the variable *)
and varkind =
  | Var_immutable (* const variables *)
  | Var_mutable   (* non-const stack-allocated variable. *)

(* [abort]: ways of aborting *)
and abort =
  | Ret of trm option        (* return;  or return 3; *)
  | Break of label option    (* break; *)
  | Continue of label option (* continue; *)


(* [mode]: mode used for default OpenMP clause *)
and mode =
  | Shared_m
  | None_

(* [expression]: representing the code inside an If OpenMP clause *)
and expression = string

(* [sched_type]: scheduling type for OpenMP *)
and sched_type =
  | Static
  | Dynamic
  | Guided
  | Runtime

(* [reduction_identifier]: reduction operation for OpenMP reduction clause *)
and reduction_identifier =
  | Plus
  | Minus
  | Prod
  | And
  | Or
  | Power
  | BitAnd
  | BitOr
  | Min
  | Max

(* [map_type] map type for map OpenMP clause *)
and map_type =
  | Alloc
  | To
  | From
  | ToFrom
  | No_map

(* [proc_bind]: process binding *)
and proc_bind =
  | Master_pb
  | Close
  | Spread

(* [dep]:  *)
and dep =
  | Dep_var of var
  | Dep_ptr of dep

(* [deps]: *)
and deps = dep list

(* [dependecy_type]: dependency kind *)
and dependence_type =
  | In of deps
  | Out of deps
  | Inout of deps
  | Outin of deps
  | Sink of deps
  | Source

(* [clause]: OpenMP clauses *)
and clause =
  (* Data sharing clauses *)
  | Default of mode
  | Shared of vars
  | Private of vars
  | FirstPrivate of vars
  | LastPrivate of vars
  | Linear of vars * int
  | Reduction of reduction_identifier * (vars)
  (* Data copying clasuses *)
  | Copyin of vars
  | CopyPrivate of vars
  | Map_c of map_type * vars
  | Defaultmap of map_type * vars
  (* SIMD clauses *)
  | Safelen of int
  | Collapse of int
  | Simdlen of int
  | Aligned of vars * int
  | Uniform of vars
  | Inbranch
  | NotInbranch
  (* General clauses *)
  | Nowait
  | Ordered_c of int
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
  | Grainsize of int
  | Mergeable
  | Nogroup
  | Num_tasks of int
  | Untied
  | Final of expression
  | To_c of vars
  | From_c of vars
  | Link of vars
  | Num_teams of var
  | Thread_limit of var
(* [atomic_operation]: atomic operations for atomic OpenMP directives *)
and atomic_operation =
  | Read
  | Write
  | Update
  | Capture

(* [directive]: OpenMP directives *)
and directive =
  | Atomic of atomic_operation option
  | Atomic_capture
  | Barrier
  | Cancel of clause * clause list
  | Cancellation_point of clause * clause list
  | Critical of var * var
  | Declare_simd of clause list
  | Declare_reduction of reduction_identifier * typvars * expression * clause
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

(* [omp_routine]: OpenMP Routines *)
and omp_routine =
  | Set_num_threads of int
  | Get_num_threads
  | Get_max_threads
  | Get_thread_num
  | Get_num_procs
  | In_parallel
  | Set_dynamic of int
  | Get_dynamic
  | Get_cancellation
  | Set_nested of int
  | Get_nested
  | Set_schedule of sched_type * int
  | Get_schedule of sched_type * int
  | Get_thread_limit
  | Set_max_active_levels of int
  | Get_max_active_levels
  | Get_level
  | Get_ancestor_thread_num
  | Get_team_size of int
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

(* **************************** Rewrite rules ****************************** *)
(* [pat]: patterns *)
type pat = trm

(* [rewrite_rule]: a type for defining rewrite rules *)
type rewrite_rule = {
  rule_vars : typed_vars;
  rule_aux_vars : typed_vars;
  rule_from : pat;
  rule_to : pat }

(* basic rewrite rules *)
type base = rewrite_rule list

(* pattern instantiation *)
module Trm_map = Map.Make(String)

(* trm map used for rewrite rules and pattern matching *)
type tmap = trm Trm_map.t

(* [fields_order]: the order should be provided as argument to the transformation [reorder_fields]. *)
type fields_order =
  | Move_before of (string * string list)
  | Move_after of (string * string list)
  | Reorder_all of var list

(* **************************** Typ constructors *************************** *)

(* [typ_build ~annot ~attributes ~desc ()]: builds typ [ty] with its fields given as arguments. *)
let typ_build ~(annot : typ_annot list) ?(attributes : attribute list = []) ~(desc : typ_desc) () : typ =
  let ty = {typ_annot = annot; typ_attributes = attributes; typ_desc = desc} in
  (* Stats for types? *)
  ty

(* [typ_make ~annot ~attributes desc]: builds typ [ty] with the type description [desc] and other fields given. *)
let typ_make ?(annot : typ_annot list = []) ?(attributes = []) (desc : typ_desc) : typ =
  typ_build ~annot ~attributes ~desc ()

(* [typ_alter ~annot ~attributes ~desc ty]: alters any of the fields of [ty] that was provided as argument. *)
let typ_alter ?(annot : typ_annot list = []) ?(attributes = []) ?(desc : typ_desc option) (ty : typ) : typ =
  let annot = match annot with [] -> ty.typ_annot |_ -> annot in
  let attributes = match attributes with | [] -> ty.typ_attributes | _ -> attributes in
  let desc = match desc with | Some td -> td | None -> ty.typ_desc in
  typ_build ~annot ~attributes ~desc ()


(* [typ_repplace desc]: an alias of [typ_alter] to alter only thet description of the trm [ty]. *)
let typ_replace (desc : typ_desc) (ty : typ) : typ =
  typ_alter ~desc ty

(* [typ_const ~annot ~attributes t]: const type constructor *)
let typ_const ?(annot : typ_annot list = []) ?(attributes = [])
  (t : typ) : typ =
  (* DEPRECATED {typ_annot = annot; typ_desc = Typ_const t; typ_attributes} *)
  typ_make ~annot ~attributes (Typ_const t)

(* [qvar_build qv qp qs]: builds a qvar record with fields qvar_var = [qvar], qvar_path = [qpath] and qvar_str = [qs]. TODO FIX COMMENT *)
let qvar_build ?(qpath : var list = []) (qvar : var) : qvar =
  let qstr =
    if qpath = []
      then qvar
      else  (Xlist.fold_lefti (fun i acc p -> if i = 0 then p else acc ^ "::  " ^ p ) "" qpath ) ^ "::" ^ qvar
    in
 {qvar_var = qvar; qvar_path = qpath; qvar_str = qstr}

(* [qvar_update ~qpath var qv]: updates [qv] by updating the fields [qvar_var] and [qvar_path] respectively.*)
let qvar_update ?(qpath : var list = []) ?(var : var = "") (qv : qvar) : qvar =
  let qpath = if qpath = [] then qv.qvar_path else qpath in
  let qvar = if var = "" then qv.qvar_var else var in
  qvar_build ~qpath qvar

(* [empty_qvar]: empty qvar is just a qvar with the string representation being the empty string. *)
let empty_qvar : qvar =
  {qvar_var = ""; qvar_path = []; qvar_str = ""}

(* [typ_constr ~annot ~attributes ~tid ~tl x]: constructed type constructor *)
let typ_constr ?(annot : typ_annot list = []) ?(attributes = [])
  ?(tid : typconstrid = next_typconstrid ()) ?(tl : typ list = []) ?(qpath : var list = [])
  ?(qtypvar : qvar = empty_qvar) (x : typvar) : typ =
  let qtx = qvar_build x ~qpath in
  let qtx = if qtypvar = empty_qvar then qtx else qtypvar in
  (* DEPRECATED {typ_annot = annot; typ_desc = Typ_constr (qtx, tid, tl); typ_attributes} *)
  typ_make ~annot ~attributes (Typ_constr (qtx, tid, tl))

(* [typ_auto ~annot ~attributes ()]: auto type constructor *)
let typ_auto ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_auto

(* [typ_unit ~annot ~attributes ()]: void type constructor *)
let typ_unit ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_unit


(* [typ_int ~annot ~attributes ()]: int type constructor *)
let typ_int ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_int

(* [typ_float ~annot ~attributes ()]: float type constructor *)
let typ_float ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_float

(* [typ_double ~annot ~attributes ()]: double type constructor *)
let typ_double ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_double

(* [typ_bool ~annot ~attributes ()]: bool type constructor *)
let typ_bool ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_bool

(* [typ_char ~annot ~attributes ()]: char type constructor *)
let typ_char ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_char


(* [typ_string ~annot ~attributes ()]: char type constructor *)
let typ_string ?(annot : typ_annot list = []) ?(attributes = []) () : typ =
  typ_make ~annot ~attributes Typ_string

(* [typ_ptr ~annot ~attributes kind t]: pointer type constructor,
   Note: references are considered as pointer types in OptiTrust *)
let typ_ptr ?(annot : typ_annot list = []) ?(attributes = [])
  (kind : ptr_kind) (t : typ) : typ =
  typ_make ~annot ~attributes (Typ_ptr {ptr_kind = kind; inner_typ = t} )

(* [typ_array ~annot ~attributes t s]: array type constructor *)
let typ_array ?(annot : typ_annot list = []) ?(attributes = []) (t : typ) (s : size) : typ =
  typ_make ~annot ~attributes (Typ_array (t, s))

(* [typ_fun ~annot ~attributes args res]: function type constructor *)
let typ_fun ?(annot : typ_annot list = []) ?(attributes = [])
  (args : typ list) (res : typ) : typ =
  typ_make ~annot ~attributes (Typ_fun (args, res) )

(* [typ_record ~annot ~attributes rt name]: record type constructor *)
let typ_record ?(annot : typ_annot list = []) ?(attributes = [])
  (rt : record_type) (name : typ) : typ =
  typ_make ~annot ~attributes (Typ_record (rt, name) )

(* [typ_template_param ~annot ~attributes name]: template type constructor *)
let typ_template_param ?(annot : typ_annot list = []) ?(attributes = [])
  (name : string) : typ =
  typ_make ~annot ~attributes (Typ_template_param name )

(* [typ_ptr_generated ty]: generated pointer type constructor *)
let typ_ptr_generated (ty : typ) : typ =
  typ_ptr ~attributes:[GeneratedTyp] Ptr_kind_mut ty

(* [typedef_prod ~recursive field_list]: typedef kind constructor *)
let typdef_record (fields : record_fields) : typdef_body =
  Typdef_record fields

(* [typ_str ~annot ~attributes s] *)
let typ_str ?(annot : typ_annot list = []) ?(attributes = [])
  (s : code_kind) : typ =
  typ_make ~annot ~attributes (Typ_arbitrary s )

(* [typ_decl ~annot ~attributes expr]: type declaration based on an expression. *)
let typ_decl ?(annot : typ_annot list = []) ?(attributes = []) (expr : trm) =
  typ_make ~annot ~attributes (Typ_decl expr )

(* [typ_ref ~annot ~attributes ty]: alias to typ_ptr Ptr_kind_ref ty *)
let typ_ref ?(annot : typ_annot list = []) ?(attributes = [])
  (ty : typ) : typ =
  typ_ptr ~annot ~attributes Ptr_kind_ref ty

(* [typ_lref ~annot ~attributes ty]: alias to typ_ref (typ_ref ty). *)
let typ_lref ?(annot : typ_annot list = []) ?(attributes = [])
  (ty : typ) : typ =
    typ_ref ~annot ~attributes (typ_ref ty)

(* **************************** Trm constructors *************************** *)

(* [trm_annot_default]: default trm annotation *)
let trm_annot_default = {
  trm_annot_attributes = [];
  trm_annot_marks = [];
  trm_annot_labels = [];
  trm_annot_stringrepr = None;
  trm_annot_pragma = [];
  trm_annot_cstyle = [];
  trm_annot_files = [];
}

(* [is_statement_of_desc t_desc]: checks if t_tesc corresponds to a statement or not  *)
let is_statement_of_desc (ty : typ option) (t_desc : trm_desc) : bool =
  match t_desc with
  | Trm_let _ | Trm_let_mult _ | Trm_let_fun _ | Trm_typedef _ | Trm_if _ | Trm_seq _ | Trm_while _
  | Trm_do_while _ | Trm_for_c _ | Trm_for _ | Trm_switch _ | Trm_abort _ | Trm_goto _  -> true
  | Trm_apps _ ->
    begin match ty with
    | Some {typ_desc = Typ_unit ; _} -> true
    | _ -> false
    end
  | _ -> false

(* [trm_build ~annot ?loc ~is_statement ?typ ~ctx ~desc ()]: builds trm [t] with its fields given as arguments. *)
let trm_build ~(annot : trm_annot) ?(loc : location) ~(is_statement : bool) ?(typ : typ option)
  ~(ctx : ctx option) ~(desc : trm_desc) () : trm =
  let t = {annot; loc; is_statement; typ; desc; ctx} in
  Stats.incr_trm_alloc ();
  t

(* [trm_make ~annot ?loc ~is_statement ?typ ~ctx desc]: builds trm [t] with description [desc] and other fields given
    as default ones. *)
let trm_make ?(annot : trm_annot = trm_annot_default) ?(loc : location) ?(is_statement : bool option)
    ?(typ : typ option) ?(ctx : ctx option) (desc : trm_desc) : trm =
   let is_statement =
     match is_statement with
     | Some b -> b
     | None -> is_statement_of_desc typ desc
     in
   trm_build ~annot ~desc ?loc ~is_statement ?typ ~ctx ()


(* [trm_alter ~annot ?loc ~is_statement ?typ ~ctx ~desc t]: alters any of the fields of [t] that was provided as argument. *)
let trm_alter ?(annot : trm_annot option) ?(loc : location option) ?(is_statement : bool option)
 ?(typ : typ option) ?(ctx : ctx option) ?(desc : trm_desc option) (t : trm) : trm =
    let annot = match annot with Some x -> x | None -> t.annot in
    let loc = match loc with Some x -> x | None -> t.loc in
    let is_statement = match is_statement with
      | Some x -> x
      | None -> match desc with
                | Some d -> is_statement_of_desc typ d
                | None -> t.is_statement
      in
    let typ = match typ with | None -> t.typ | _ -> typ in
    let ctx = match ctx with | None -> t.ctx | _ -> ctx in
    let desc = match desc with | Some x -> x | None -> t.desc in
    trm_build ~annot ~desc ?loc ~is_statement ?typ ~ctx ()

(* [trm_replace desc t]: an alias of [trm_alter] to alter only the descriptiong of [t]. *)
let trm_replace (desc : trm_desc) (t : trm) : trm =
  trm_alter ~desc t


(* **************************** CStyle *************************** *)

(* [trm_get_cstyles t]: returns all cstyle annotations of trm [t]. *)
let trm_get_cstyles (t : trm) : cstyle_annot list =
  t.annot.trm_annot_cstyle

(* [apply_on_cstyles f t]: applies [f] on the cstyme encodings of [t]. *)
let apply_on_cstyles (f : cstyle_annot list -> cstyle_annot list) (t : trm) : trm =
  let t_annot_cstyle = f (trm_get_cstyles t) in
  let t_annot = {t.annot with trm_annot_cstyle=t_annot_cstyle} in
  trm_alter ~annot:t_annot t

(* [trm_add_cstyle cs t]: adds [cs] cstyle annotation to trm [t]. *)
let trm_add_cstyle (cs : cstyle_annot) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> cs :: cstyles) t

(* [trm_filter_cstyle pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
let trm_filter_cstyle (pred : cstyle_annot -> bool) (t : trm) : trm =
  apply_on_cstyles (fun cstyles -> List.filter (fun cs -> pred cs) cstyles) t

(* [trm_rem_cstyle cs t]: removes the cstyle_annot annotation [cs] from trm [t]. *)
let trm_rem_cstyle (cs : cstyle_annot) (t : trm) : trm =
  trm_filter_cstyle (fun cs1 -> cs <> cs1) t

(* [trm_has_cstyle cs t]: checks if [t] has the [cs] cstyle annotation. *)
let trm_has_cstyle (cs : cstyle_annot) (t : trm) : bool =
  let cstyles = trm_get_cstyles t in
  List.mem cs cstyles

(* [annot_has_cstyle cs t_ann]: checks if [cs] is constained in [t_ann]. *)
let annot_has_cstyle (cs : cstyle_annot) (t_ann : trm_annot) : bool =
  let cstyles = t_ann.trm_annot_cstyle in
  List.mem cs cstyles


(* **************************** Smart constructors *************************** *)

(* [trm_val ~annot ?loc ?typ ~ctx y]: value *)
let trm_val ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (v : value) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_val v)

(* [trm_var ~annot ?loc ?typ ?ctx ~kind x]: variable occurrence *)
let trm_var ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  ?(kind : varkind = Var_mutable) ?(qpath : var list = []) ?(qvar : qvar = empty_qvar) (x : var) : trm =
  let qx = qvar_build x ~qpath in
  let qx = if qvar = empty_qvar then qx else qvar in
  trm_make ~annot ?loc ?typ ?ctx (Trm_var (kind, qx))

(* [trm_array ~annot ?loc ?typ ?ctx tl]: array initialization list *)
let trm_array ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tl : trm mlist) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_array tl)

(* [trm_record ~annot ?loc ?typ ?ctx tl]: struct initialization list *)
let trm_record ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tl : (label option * trm) mlist) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_record tl)

(* [trm_let ~annot ?loc ?ctx kind typed_var init]: variable declaration *)
let trm_let ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (kind : varkind) (typed_var : typed_var) (init : trm): trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let (kind, typed_var, init))

(* [trm_let ~annot ?loc ?ctx kind ty vl tl]: multiple variable declarations *)
let trm_let_mult ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (kind : varkind)
   (tvl : typed_vars) (tl : trms) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let_mult (kind, tvl, tl))

(* [trm_let ~annot ?loc ?ctx name ret_typ args body]: function definition *)
let trm_let_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) ?(qpath : var list = [])
  ?(qvar : qvar = empty_qvar) (name : var) (ret_typ : typ) (args : typed_vars) (body : trm) : trm =
  let qname = qvar_build name ~qpath in
  let qname = if qvar = empty_qvar then qname else qvar in
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_let_fun (qname, ret_typ, args, body))

(* [trm_fun ~annot ?loc args ret_typ body]: anonymous function.  *)
let trm_fun ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (args : typed_vars)
  (ret_typ : typ option) (body : trm) =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_fun (args, ret_typ, body))

(* [trm_typedef ~annot ?loc ?ctx def_typ]: type definition *)
let trm_typedef ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (def_typ : typedef): trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_typedef def_typ)

(* [trm_if ~annot ?loc ?ctx cond tb eb]: if statement *)
let trm_if ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm)
  (tb : trm) (eb : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_if (cond, tb, eb))

(* [trm_seq ~annot ?loc ?ctx tl]: block statement *)
let trm_seq ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (tl : trm mlist) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_seq tl)

(* [trm_apps ~annot ?loc ?typ ?ctx f args]: function call *)
let trm_apps ?(annot = trm_annot_default) ?(loc) ?(typ) ?(attributes = [])
  ?(ctx : ctx option) (f : trm) (args : trms) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_apps (f, args))

(* [trm_while ~annot ?loc ?ctx cond body]: while loop *)
let trm_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_while (cond, body))

(* [trm_do_while ~annot ?loc ?ctx cond body]: do while loop *)
let trm_do_while ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)  (body : trm) (cond : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_do_while (body, cond))

(* [trm_for_c ~annot ?loc ?ctx init cond step body]: for loop *)
let trm_for_c ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)  (init : trm) (cond : trm)
  (step : trm) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_for_c (init, cond, step, body))

(* [trm_switch ~annot ?loc ?ctx cond cases]: switch statement *)
let trm_switch ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (cond : trm)
  (cases : (trms * trm) list) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_switch (cond, cases))

(* [trm_abort ~annot ?loc ?ctx a]: abort instruction *)
let trm_abort ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (a : abort) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_abort a)

(* [trm_goto ~annot ?loc ?ctx l]: goto statement *)
let trm_goto ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (l : label) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit()) ?ctx (Trm_goto l)

(* [trm_uninitialized ~annot ?loc ?ctx ()]: used for variable declarations without initialization
    and function declarations *)
let trm_uninitialized ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) () : trm =
  trm_make ~annot ?loc ?ctx (Trm_val (Val_lit (Lit_uninitialized)))

(* [trm_for ~annot ?loc ?ctx index start direction stop step body]: simple for loop *)
let trm_for ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (loop_range : loop_range) (body : trm) : trm =
  trm_make ~annot ?loc ~typ:(typ_unit ()) ?ctx (Trm_for (loop_range, body))

let trm_for_instrs ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
(loop_range : loop_range) (body_instrs : trm mlist) : trm =
  trm_for ~annot ?loc ?ctx loop_range (trm_seq body_instrs)

(* [code code_str ]: arbitrary code entered by the user *)
let code (code_str : code_kind) : trm =
  trm_make (Trm_arbitrary code_str)

(* [trm_omp_routine ?loc omp_routine] OpenMP routine *)
let trm_omp_routine ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (omp_routine : omp_routine) : trm =
  trm_make ~annot ?loc ?ctx (Trm_omp_routine omp_routine)

(* [extern ?loc ~lang tl]: extern *)
let trm_extern ?(annot = trm_annot_default) ?(loc) (lang : string) (tl : trms) : trm =
  trm_make ~annot ?loc  (Trm_extern (lang, tl))

(* [trm_namespace ?loc ?ctx name t inline ]: namespace *)
let trm_namespace ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (name : string) (t : trm ) (inline : bool) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_namespace (name, t, inline))

(* [trm_template ?loc ?typ ?ctx tpl t]: template statemented *)
let trm_template ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (tpl : template_parameter_list) (t : trm ) : trm =
  trm_make ~annot ?loc ?typ ?ctx (Trm_template (tpl, t))

(* [trm_using_directive ~annot ?loc ?typ ?ctx namespace]: creates a using namespace directive. *)
let trm_using_directive ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option)
  (namespace : string) =
  trm_make ~annot ?loc ?typ ?ctx (Trm_using_directive namespace)

(* [trm_this ~annot ?loc ?typ ?ctx ()]: this pointer. *)
let trm_this ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) () =
  trm_make ~annot ?loc ?typ ?ctx (Trm_var (Var_immutable, qvar_build "this" ))

(* [trm_delete ~annot ?loc ?typ ?ctx is_array_form t]: delete operator  *)
let trm_delete ?(annot = trm_annot_default) ?(loc) ?(typ) ?(ctx : ctx option) (is_array_form : bool) (t : trm) =
  trm_make ~annot ?loc ?typ ?ctx (Trm_delete (is_array_form, t))

(* ********************************** Auxiliary functions ************************************ *)

(* [trm_unop ~annot ?loc ?ctx p]: unary operator *)
let trm_unop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : unary_op) : trm =
  trm_val ~annot ?loc ?ctx (Val_prim (Prim_unop p))

(* [trm_biop ~annot ?loc ?ctx p]: binary operator *)
let trm_binop ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : binary_op) : trm =
  trm_val ~annot:annot ?loc ?ctx (Val_prim (Prim_binop p))

(* [trm_cast ty t]: type cast *)
let trm_cast ?(annot : trm_annot = trm_annot_default) (ty : typ) (t : trm) : trm =
  trm_apps ~annot (trm_unop (Unop_cast ty)) [t]

(* [typ_of_lit l]: get the type of a literal *)
let typ_of_lit (l : lit) : typ option =
  match l with
  | Lit_unit -> Some (typ_unit ())
  | Lit_uninitialized -> None
  | Lit_bool _ -> Some (typ_bool ())
  | Lit_int _ -> Some (typ_int ())
  | Lit_double _ -> Some (typ_double ())
  | Lit_string _ -> Some (typ_string ())
  | Lit_nullptr -> Some (typ_unit ())

(* [trm_lit ~annot ?loc ?ctx l]: literal *)
let trm_lit ?(typ : typ option = None) ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (l : lit) : trm =
  let typ = Tools.option_or typ (typ_of_lit l) in
  trm_val ~annot:annot ?loc ?ctx ?typ (Val_lit l)

let trm_unit ?(loc) () : trm =
  trm_lit ?loc (Lit_unit)
let trm_bool ?(loc) (b : bool) =
  trm_lit ?loc (Lit_bool b)
(* LATER: allow arbitrary sized integer types/values *)
let trm_int ?(loc) (i : int) =
  trm_lit ?loc (Lit_int i)
(* LATER: may need arbitrary sized float values *)
let trm_float ?(typ : typ = typ_float ()) ?(loc) (d : float) =
  trm_lit ~typ:(Some typ) ?loc (Lit_double d)
let trm_double ?(loc) (d : float) =
  trm_float ~typ:(typ_double ()) ?loc d
let trm_sring ?(loc) (s : string) =
  trm_lit ?loc (Lit_string s)

(* [trm_null ~annot ?loc ?ctx ()]: build the term [nullptr], or [NULL] if [~uppercase:true]
   (also used for [void* 0] by Menhir, but decoded in cMenhir_to_ast)  *)
let trm_null ?(uppercase : bool = false) ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (_ : unit) : trm =
  (* DEPRECATED trm_val ~annot ?loc ?ctx  (Val_ptr 0) *)
  let t = trm_lit ?loc ?ctx Lit_nullptr in
  if uppercase then trm_add_cstyle Display_null_uppercase t else t

(* [trm_free]: build a term calling the 'free' function. *)
let trm_free ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (memory : trm) : trm =
  trm_apps (trm_var "free") [memory]

(* [trm_prim ~annot ?loc ?ctx p]: primitives *)
let trm_prim ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (p : prim) : trm =
  trm_val ~annot:annot ?loc ?ctx (Val_prim p)

(* [trm_set ~annot ?loc ?ctx t1 t2] *)
let trm_set ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  ?(typ : typ option = Some (typ_unit ()))  (lhs : trm) (rhs : trm) : trm =
  trm_apps ~annot:annot ?loc ?ctx ?typ (trm_binop Binop_set) [lhs; rhs]

(* [trm_set ~annot ?loc ?ctx t1 t2] *)
let trm_neq ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ?loc ?ctx ~typ:(typ_unit ()) (trm_binop Binop_neq) [t1; t2]

(* [trm_seq_nomarks ~annot ?loc ?ctx tl]: hidden block statement *)
let trm_seq_nomarks ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (tl : trms) : trm =
  trm_seq ~annot ?loc ?ctx (Mlist.of_list tl)

(* [typ_ref_inv ty]: get the inner type of a reference *)
let typ_ref_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref; inner_typ = ty1} -> Some ty1
  | _ -> None

(* [typ_ptr_inv ty]: get the inner type of a pointer *)
let typ_ptr_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty1} -> Some ty1
  | _ -> None

let typ_array_inv (ty : typ) : (typ * size) option =
  match ty.typ_desc with
  | Typ_array (typ, size) -> Some (typ, size)
  | _ -> None

let typ_const_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_const typ -> Some typ
  | _ -> None

(* [typ_const_ptr_inv ty]: get the inner type of a constant pointer *)
let typ_const_ptr_inv (ty : typ) : typ option =
  Option.bind (typ_const_inv ty) typ_ptr_inv

let typ_const_array_inv (ty : typ) : (typ * size) option =
  Option.bind (typ_array_inv ty) (fun (ty2, size) ->
    Option.map (fun ty3 -> (ty3, size)) (typ_const_inv ty2))

(* [typ_add_attribute att ty]: adds the attribute [att] to the type [ty] *)
let typ_add_attribute (att : attribute)(ty : typ) : typ =
  {ty with typ_attributes = att :: ty.typ_attributes}

(* [typ_has_attribute att ty]: checks if [ty] has attribute [att]. *)
let typ_has_attribute (att : attribute) (ty : typ) : bool =
  List.mem att ty.typ_attributes

(* [TransfoError]: exception raised in case a transformation fails *)
exception TransfoError of string

(* [Resolve_target_failure]: exception raised when a target couldn't be resolved *)
exception Resolve_target_failure of location option * string

(* [loc_to_string loc]: pretty print a trm location *)
let loc_to_string (loc : location) : string =
  match loc with
  | None -> ""
  | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column};
          loc_end = {pos_line = end_row; pos_col = end_column}} -> (filename ^ " start_location [" ^
                                (string_of_int start_row) ^": " ^ (string_of_int start_column) ^" ]" ^
                                " end_location [" ^ (string_of_int end_row) ^": " ^
                                (string_of_int end_column) ^" ]")

(* [fail loc err]: fails with error [err] raised at location [loc] *)
let fail (loc : location) (err : string) : 'a =
  match loc with
  | None -> raise (TransfoError err)
  | Some _ -> raise (TransfoError (loc_to_string loc ^ " : " ^ err))

let assert_transfo_error (msg : string) (f : unit -> unit) : unit =
  try f () with
  | TransfoError msg2 -> assert (msg = msg2)

(* ********************************** Annotation manipulation ************************************ *)
(**** Attributes  ****)

(* [trm_get_attr t]: returns all the attributes of trm [t]. *)
let trm_get_attr (t : trm) : attribute list =
  t.annot.trm_annot_attributes

(* [trm_attr_add att t]: adds attribute [att] to trm [t] *)
let trm_attr_add (att : attribute) (t : trm) : trm =
  let t_annot_attributes = t.annot.trm_annot_attributes in
  let t_annot = {t.annot with trm_annot_attributes = att :: t_annot_attributes} in
  trm_alter ~annot:t_annot t

(**** Marks  ****)

(* [apply_on_marks f t]: applies [f] on the marks of [t]. *)
let apply_on_marks (f : marks -> marks) (t : trm) : trm =
  let t_annot_marks = f (t.annot.trm_annot_marks) in
  let t_annot = {t.annot with trm_annot_marks=t_annot_marks} in
  trm_alter ~annot:t_annot t

(* [trm_add_mark m]: adds mark [m] to the trm [t] *)
let trm_add_mark (m : mark) (t : trm) : trm =
  if m = "" then t else apply_on_marks (fun marks -> m :: marks) t

let trm_may_add_mark (mo : mark option) (t : trm) : trm =
  match mo with
  | Some m -> trm_add_mark m t
  | None -> t

(* [trm_filter_mark m t]: filters all marks that satisfy the predicate [pred]. *)
let trm_filter_mark (pred : mark -> bool) (t : trm): trm =
  apply_on_marks (fun marks -> List.filter (fun m -> pred m) marks) t

(* [trm_rem_mark m t]: removes mark [m] from trm [t]. *)
let trm_rem_mark (m : mark) (t : trm) : trm =
  trm_filter_mark (fun m1 -> m <> m1) t

(* [trm_add_mark_between index m t]: adds mark [m] at [index] in the mlist of [t], where [t] should be a sequence. *)
let trm_add_mark_between (index : int) (m : mark) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_mark_at index m tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Ast.trm_add_mark_between: expected a sequence"

(* [trm_remove_marks t]: removes all the marks from trm [t] *)
let trm_remove_marks (t : trm) : trm =
  let res =
  match t.desc with
  (* In the case of sequences, special treatment is needed for in between marks*)
  | Trm_seq tl -> trm_replace (Trm_seq {items = tl.items; marks = []}) t
  | _ -> t in
  trm_filter_mark (fun _ -> false) res

(* [trm_rem_mark_between m t]: removes the between mark [m] from trm [t] *)
let trm_rem_mark_between (m : mark) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.remove_mark m tl in
    trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Ast.trm_rem_mark_between: expected a sequence"

(* [trm_has_mark m t]: checks if trm [t] has mark [m]. *)
let trm_has_mark (m : mark) (t : trm) : bool =
  List.mem m t.annot.trm_annot_marks

(* [trm_get_marks t]: returns all the marks of [t]. *)
let trm_get_marks (t : trm) : marks =
  t.annot.trm_annot_marks

(* [trm_pass_marks t1 t2]: passes the marks of trm [t1] to trm [t2]. *)
let trm_pass_marks (t1 : trm) (t2 : trm) : trm =
  let t1_marks = trm_get_marks t1 in
  let t2_marks = trm_get_marks t2 in
  let t2_annot = {t2.annot with trm_annot_marks = t2_marks @ t1_marks} in
  trm_alter ~annot:t2_annot t2

(**** Labels  ****)

(* [trm_get_labels t]: gets all the labels of trm [t]. *)
let trm_get_labels (t : trm) =
 t.annot.trm_annot_labels

(* [apply_on_labels f t]: applies [f] on the labels of [t]. *)
let apply_on_labels (f : marks -> marks) (t : trm) : trm =
  let t_labels = trm_get_labels t in
  let t_annot_labels = f t_labels in
  let t_annot = {t.annot with trm_annot_labels = t_annot_labels} in
  trm_alter ~annot:t_annot t

(* [trm_add_label l]: adds label [l] to trm [t]. *)
let trm_add_label (l : label) (t : trm) : trm =
  apply_on_labels (fun labels -> l :: labels) t

(* [trm_filter_label pred t]: filters all labels that satisfy predicate [pred]. *)
let trm_filter_label (pred : label -> bool) (t : trm) : trm =
  apply_on_labels (fun labels -> List.filter (fun l -> pred l) labels) t

(* [trm_rem_label l t]: removes label [l] from trm [t]. *)
let trm_rem_label (l : label) (t : trm) : trm =
  trm_filter_label (fun l1 -> l <> l1) t

(* [trm_rem_labels t]: removes all the labels from trm [t]. *)
let trm_rem_labels (t : trm) : trm =
  apply_on_labels (fun _ -> []) t

(* [trm_has_label l t]: checks if trm [t] has label [l]. *)
let trm_has_label (l : label) (t : trm) : bool =
  let t_labels = trm_get_labels t in
  List.mem l t_labels

(* [trm_pass_labels t1 t2]: passes the labels of trm [t1] to trm [t2]. *)
let trm_pass_labels (t1 : trm) (t2 : trm) : trm =
  let t1_labels = trm_get_labels t1 in
  let t2_labels = trm_get_labels t2 in
  let t2_annot = {t2.annot with trm_annot_labels = t2_labels @ t1_labels} in
  {t2 with annot = t2_annot}

(**** Stringrepr  ****)

(* [trm_set_stringreprid id t]: sets the string representation id [t] to [id]. *)
let trm_set_stringreprid (id : stringreprid) (t : trm) : trm =
  let annot = {t.annot with trm_annot_stringrepr = Some id} in
  trm_alter ~annot t

(* [trm_get_stringreprid t]: gets the string representation of trm [t]. *)
let trm_get_stringreprid (t : trm) : stringreprid option =
  t.annot.trm_annot_stringrepr


(**** CPragmas  ****)

(* [apply_on_pragmas f t]: applies [f] on the pragma directives associated with [t]. *)
let apply_on_pragmas (f : cpragma list -> cpragma list) (t : trm) : trm =
  let t_annot_pragmas = f (t.annot.trm_annot_pragma) in
  let annot = {t.annot with trm_annot_pragma = t_annot_pragmas} in
  trm_alter ~annot t

(* [trm_add_pragma p t]: adds the pragma [p] into [t]. *)
let trm_add_pragma (p : cpragma) (t : trm) : trm =
  apply_on_pragmas (fun pragmas -> p :: pragmas) t

let trm_add_pragmas (p : cpragma list) (t : trm) : trm =
  apply_on_pragmas (fun pragmas -> p @ pragmas) t

(* [trm_filter_pragma pred t]: filters all the pragmas that satisfy the predicate [pred]. *)
let trm_filter_pragma (pred : cpragma -> bool) (t : trm) : trm =
  apply_on_pragmas (fun pragmas -> List.filter (fun p -> pred p) pragmas) t

(* [trm_rem_pragma p t]: removes the pragma [p] from [t]. *)
let trm_rem_pragma (p : cpragma) (t : trm) : trm =
  trm_filter_pragma (fun p1 -> p <> p1) t

(* [trm_get_pragmas t]: returns all the pragmas annotated to [t]. *)
let trm_get_pragmas (t : trm) : cpragma list =
  t.annot.trm_annot_pragma

(* [trm_has_pragma pred t]: check if [t] has pragmas that satisfy [pred]. *)
let trm_has_pragma (pred : cpragma -> bool) (t : trm) : bool =
  let t_pragmas = trm_get_pragmas t in
  List.exists pred t_pragmas

(* [trm_pass_pragmas t1 t2]: pass pragmas of trm [t1] to trm [t2]. *)
let trm_pass_pragmas (t1 : trm) (t2 : trm) : trm =
  let t1_pragmas = trm_get_pragmas t1 in
  let t2_pragmas = trm_get_pragmas t2 in
  let t2_annot = {t2.annot with trm_annot_pragma = t1_pragmas @ t2_pragmas} in
  {t2 with annot = t2_annot}

(**** Files  ****)

(* [trm_get_files_annot t]: returns all file annotations of trm [t]. *)
let trm_get_files_annot (t : trm) : files_annot list =
  t.annot.trm_annot_files

(* [trm_set_mainfile]: adds [Main_file] annotation to trm [t]. *)
let trm_set_mainfile (t : trm) : trm =
   let t_files = trm_get_files_annot t in
   let t_annot_files = Main_file :: t_files in
   let annot = {t.annot with trm_annot_files=t_annot_files} in
   trm_alter ~annot t

(* [trm_set_include filename t]: add [Include filename] annotation to trm [t]. *)
let trm_set_include (filename : string) (t : trm) : trm =
  let t_files = trm_get_files_annot t in
  let t_annot_files = Include filename :: t_files in
  let annot = {t.annot with trm_annot_files = t_annot_files} in
  trm_alter ~annot t

(* [trm_is_mainfile t]: checks if [t] contains the [Main_file] annotation. *)
let trm_is_mainfile (t : trm) : bool =
  let t_files = trm_get_files_annot t in
  List.mem Main_file t_files

(* [trm_is_include]: checks if [t] contains the [Include f] annotation. *)
let trm_is_include (t : trm) : bool =
  let t_files = trm_get_files_annot t in
  List.exists (function |Include _ -> true | _ -> false) t_files

(* [trm_is_nobrace_seq t]: checks if [t] is a visible sequence or not *)
let trm_is_nobrace_seq (t : trm) : bool =
  List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle

(* ********************************************************************************************** *)

(* [trm_vardef_get_trm_varse]: gets the singleton declaration variable in the case when [t] is a variable declaration
    or a list of variable in the case when we have multiple variable declarations in one line *)
let rec trm_vardef_get_vars (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x, _), _) -> [x]
  | Trm_seq tl when trm_has_cstyle Multi_decl t -> List.flatten (List.map trm_vardef_get_vars (Mlist.to_list tl))
  | _ -> []

(* [trm_ret ~annot a]; special trm_abort case, used for return statements *)
let trm_ret ?(annot = trm_annot_default) ?(loc) (a : trm option) : trm =
  trm_abort ~annot ?loc (Ret a)

(* [trm_prim_inv t]: gets the primitive operation *)
let trm_prim_inv (t : trm) : prim option =
  match t.desc with
  | Trm_val (Val_prim p) -> Some p
  | _ -> None

(* [trm_lit_inv t]: gets the literal from a literal trm *)
let trm_lit_inv (t : trm) : lit option =
  match t.desc with
  | Trm_val (Val_lit v) -> Some v
  | _ -> None


(* [trm_inv ~arror k t]: returns the results of applying [k] on t, if the result is [None] thne
     then function fails with error [error]. *)
let trm_inv ?(error : string = "") ?(loc : location) (k : trm -> 'a option) (t : trm) : 'a =
  let loc = if loc = None then t.loc else loc in
  match k t with
  | None -> if error = "" then assert false else fail loc error
  | Some r -> r

let typ_inv ?(error : string = "") (loc : location) (k : typ -> 'a option) (t : typ) : 'a =
  match k t with
  | None -> if error = "" then assert false else fail loc error
  | Some r -> r

(* [trm_let_inv t]: returns the components of a [trm_let] constructor if [t] is a let declaration.
     Otherwise it returns [None]. *)
let trm_let_inv (t : trm) : (varkind * var * typ * trm) option =
  match t.desc with
  | Trm_let (vk, (x, tx), init) -> Some (vk, x, tx, init)
  | _ -> None

(* [trm_let_fun_inv t]: returns the componnets of a [trm_let_fun] constructor if [t] is a function declaration.
     Otherwise it returns a [Noen]. *)
let trm_let_fun_inv (t : trm) : (qvar * typ * typed_vars * trm) option =
  match t.desc with
  | Trm_let_fun (f, ret_ty, args, body) -> Some (f, ret_ty, args, body)
  | _ -> None


(* [trm_apps_inv t]: returns the components of a [trm_apps] constructor in case [t] is function application.
    Otherwise it returns [None]. *)
let trm_apps_inv (t : trm) : (trm * trm list) option =
  match t.desc with
  | Trm_apps (f, tl) -> Some (f, tl)
  | _ -> None

(* [trm_seq_inv t]: returns the components of a [trm_seq] constructor when [t] is a sequence.
    Otherwise it returns [None]. *)
let trm_seq_inv (t : trm) : (trm mlist) option =
  match t.desc with
  | Trm_seq tl ->  Some tl
  | _ -> None

(* [trm_var_inv t]: returns the components of a [trm_var] constructor when [t] is a variable occurrence.
    Otherwise it returns [None]. *)
let trm_var_inv (t : trm) : (varkind * var) option =
  match t.desc with
  | Trm_var (vk, x) -> Some (vk, x.qvar_var)
  | _ -> None

(* [trm_free_inv]: deconstructs a 'free(x)' call. *)
let trm_free_inv (t : trm) : trm option =
  match trm_apps_inv t with
  | Some (f, [x]) ->
    begin match trm_var_inv f with
    | Some (_, f_name) when f_name = "free" -> Some x
    | _ -> None
    end
  | _ -> None

(* [trm_if_inv t]: returns the components of a [trm_if] constructor when [t] is an if statement.
    Otherwise it returns [None]. *)
let trm_if_inv (t : trm) : (trm * trm * trm) option =
  match t.desc with
  | Trm_if (cond, then_, else_) -> Some (cond, then_, else_)
  | _ -> None

(* [trm_typedef_inv t]: returns the components of a [trm_typedef] constructor when [t] is a type definition. *)
let trm_typedef_inv (t : trm) : typedef option =
  match t.desc with
  | Trm_typedef td -> Some td
  | _ -> None

(* [trm_unop_inv t]: deconstructs t = op t1 *)
let trm_unop_inv (t : trm) : (unary_op * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (Prim_unop op), [a] -> Some (op, a)
    | _ -> None
    end
  | _ -> None

(* [trm_binop_inv t]: deconstructs t = t1 op t2 *)
let trm_binop_inv (op : binary_op) (t : trm) : (trm * trm) option =
  match trm_apps_inv t with
  | Some (f, args) -> begin
    match (trm_prim_inv f, args) with
    | Some (Prim_binop op'), [a; b] when op = op' -> Some (a, b)
    | _ -> None
    end
  | _ -> None

let trm_cast_inv (t : trm) : (typ * trm) option =
  match trm_unop_inv t with
  | Some (Unop_cast ty, t2) -> Some (ty, t2)
  | _ -> None

let trm_get_inv (t : trm) : trm option =
  match trm_unop_inv t with
  | Some (Unop_get, t2) -> Some t2
  | _ -> None

let trm_var_get_inv (t : trm) : (varkind * var) option =
  match trm_get_inv t with
  | Some t2 -> trm_var_inv t2
  | None -> None

(* [trm_prod_inv t]: gets a the list of factors involved in a multiplication*)
let trm_prod_inv (t : trm) : trm list =
  let rec aux (indepth : bool) (acc : trm list) (t : trm) : trm list =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_mul))); _}, [l; r]) -> (aux true acc l) @ (aux true acc r)
    | _ -> if indepth then acc @ [t] else acc
  in aux false [] t

(* [trm_mlist_inv_marks t] gets the description of marks in a term that
   contains a MList, for example [Trm_seq], [Trm_array], or [Trm_record]. *)
let trm_mlist_inv (t : trm) : mark list list option =
  match t.desc with
  | Trm_seq tl | Trm_array tl -> Some tl.marks
  | Trm_record tl -> Some tl.marks
  | _ -> None

(* [get_mark_index m t]: for relative targets marks are stored on the parent sequence, this function give the index
   that mark m targets to *)
let get_mark_index (m : mark) (t : trm) : int option =
  match t.desc with
  | Trm_seq tl ->
    Xlist.fold_lefti (fun i acc ml ->
      match acc with
      | Some _ -> acc
      | None ->
        if List.mem m ml then Some i else None
    ) None tl.marks
  | _ -> fail t.loc "Ast.get_mark_index: expected a sequence trm"

(* [print_info loc]: computes a function that prints information related to some location in file only if the verbose
   flag is activated *)
let print_info (loc : location) : ('a, out_channel, unit) format -> 'a =
  if !Flags.verbose then
    match loc with
    | None -> Printf.printf
    | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column};
                                  loc_end = {pos_line = end_row; pos_col = end_column}} ->
       Printf.kfprintf Printf.fprintf stdout ("<%s> from <%d>,<%d> to   <%d>,<%d>")
       filename start_row start_column end_row end_column
  else
    Printf.ifprintf stdout

(* [trm_access]: concrete accesses in a trm *)
type trm_access =
  | Array_access_get of trm (* operator -> [i] *)
  | Array_access_addr of trm (* operator [i] *)
  | Struct_access_get of field (* operator->f *)
  | Struct_access_addr of field (* operator.f *)

(* [get_nested_accesses t]: for a given trm [t], if it's an access trm return the list of accesses,
    the list starts with the base, and ends with the last access *)
let rec get_nested_accesses (t : trm) : trm * (trm_access list) =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f))); _},
              [t']) ->
     let (base, al) = get_nested_accesses t' in
     (base, Struct_access_addr f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f))); _},
              [t']) ->
     let (base, al) = get_nested_accesses t' in
     (base, Struct_access_get f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access)); _},
              [t'; i]) ->
     let (base, al) = get_nested_accesses t' in
     (base, Array_access_addr i :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get)); _},
              [t'; i]) ->
     let (base, al) = get_nested_accesses t' in
     (base, Array_access_get i :: al)
  | _ -> (t, [])

(* [build_nested_accesses base access_list]: from a list of accesses build the original trm *)
let build_nested_accesses (base : trm) (access_list : trm_access list) : trm =
  List.fold_left (fun acc access ->
    match access with
    | Struct_access_addr f ->
      trm_apps (trm_unop (Unop_struct_access f)) [acc]
    | Struct_access_get f ->
      trm_apps (trm_unop (Unop_struct_get f)) [acc]
    | Array_access_addr i ->
      trm_apps (trm_binop (Binop_array_access)) [acc;i]
    | Array_access_get i ->
      trm_apps (trm_binop (Binop_array_get)) [acc;i]
  ) base access_list

(* [loop_step_to_trm l_step]: returns the loop step as trm *)
let loop_step_to_trm (l_step: loop_step) : trm =
  match l_step with
  | Post_inc | Post_dec | Pre_inc | Pre_dec -> trm_lit (Lit_int 1)
  | Step s -> s

(* [is_step_one step]: checks if the step of the loop is one or not *)
let is_step_one (step : loop_step) : bool =
  match step with
  | Post_inc | Post_dec | Pre_inc | Pre_dec -> true
  | _ -> false

(* [apply_on_loop_step]: applies [f] on the step [l_step] *)
let apply_on_loop_step (f : trm -> trm) (l_step : loop_step) : loop_step =
  match l_step with
  | Step st -> Step (f st)
  | _ -> l_step

(* [trm_map_with_terminal_unop is_terminal f t]: applies function [f] over ast nodes, if nodes are terminal nodes than
    specific treatment is considered depending on the definition of function f
    Note: This is an unoptimized version of trm_map_with_terminal *)
let trm_map_with_terminal_unopt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let typ = t.typ in
  match t.desc with
  | Trm_array tl ->
    let tl' = Mlist.map (f false) tl in
    trm_array ~annot ?loc ?typ tl'
  | Trm_record tl ->
    let tl' = Mlist.map (fun (lb, t) -> (lb, f false t)) tl in
    trm_record ~annot ?loc ?typ tl'
  | Trm_let (vk, tv, init) ->
    let init' = f false init in
    trm_let ~annot ?loc vk tv init'
  | Trm_let_fun (f', res, args, body) ->
    let body' = f false body in
    trm_let_fun ~annot ?loc ~qvar:f' "" res args body'
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = f is_terminal then_ in
    let else_' = f is_terminal else_ in
    trm_if ~annot ?loc cond' then_' else_'
  | Trm_seq tl ->
    let n = Mlist.length tl in
    let tl' = Mlist.mapi(fun i tsub ->
      let sub_is_terminal = is_terminal && i == n-1 in
      f sub_is_terminal tsub
    ) tl in
    trm_seq ~annot ?loc tl'
  | Trm_apps (f', args) ->
    let f'' = f false f' in
    let args' = List.map (f false) args in
     (*
       warning: f'' may have different type
       -> print and reparse to have the right type
      *)
    trm_apps ~annot ?loc ?typ f'' args'
  | Trm_while (cond, body) ->
     let cond' = f false cond in
     let body' = f false body in
     trm_while ~annot ?loc cond' body'
  | Trm_for_c (init, cond, step, body) ->
     let init' = f false init in
     let cond' = f false cond in
     let step' = f false step in
     let body' = f is_terminal body in
     trm_for_c ~annot ?loc init' cond' step' body'
  | Trm_for (l_range, body) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let m_step = match step with
    | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
    | Step sp -> Step (f is_terminal sp)
    in
    let start' = f false start in
    let stop' = f false stop in
    let body' = f is_terminal body in
    trm_for ~annot ?loc (index, start', direction, stop', m_step, is_parallel) body'
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, f is_terminal body)) cases in
     trm_switch ~annot ?loc cond' cases'
  | Trm_abort a ->
     begin match a with
     | Ret (Some t') -> trm_ret ~annot ?loc (Some (f false t'))
     (* return without value, continue, break *)
     | _ -> t
     end
  | Trm_namespace (name, t, inline) ->
    trm_namespace ~annot ?loc name (f false t) inline
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rfl ->
      let rfl = List.map (fun (rf, rf_ann) ->
        match rf with
        | Record_field_method t1 ->  (Record_field_method (f false t1), rf_ann)
        | _ -> (rf, rf_ann)
      ) rfl in
      let td = {td with typdef_body = Typdef_record rfl} in
      trm_typedef ~annot ?loc td
    | _ -> t
    end

  | _ -> t

(* TODO ARTHUR: think about how to factorize this.

  trm_map (f: bool -> trm -> trm) (t : trm) : trm =

  trm_map_with_terminal_opt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
    using trm_map, and only duplicating 5 cases *)

(* [trm_map_with_terminal_opt is_terminal f]: trm_map_with_terminal derived from trm_map *)
let trm_map_with_terminal_opt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let typ = t.typ in
  let aux = f is_terminal in

  (* [flist tl]: applies [f] to all the elements of a list [tl] *)
  let flist tl =
    let tl' = List.map (f false) tl in
    if List.for_all2 (==) tl tl' then tl else tl'
  in
  (* [fmlist]: is like [flist] but for marked lists *)
  let fmlist is_terminal tl =
    let tl' = Mlist.map (f is_terminal) tl in
    if Mlist.for_all2 (==) tl tl' then tl else tl' in

  match t.desc with
  | Trm_array tl ->
    let tl' = fmlist false tl in
    if (tl' == tl) then t else
        (trm_array ~annot ?loc ?typ tl')
  | Trm_record tl ->
    let tl' = Mlist.map (fun (lb, t) -> (lb, f false t)) tl in
    let tl' = if Mlist.for_all2 (==) tl tl' then tl else tl' in
    if (tl' == tl) then t else
        (trm_record ~annot ?loc ?typ tl')
  | Trm_let (vk, tv, init) ->
    let init' = f false init in
    if (init' == init) then t else
        (trm_let ~annot ?loc vk tv init')
  | Trm_let_fun (f', res, args, body) ->
    let body' = f false body in
    if (body' == body) then t else
        (trm_let_fun ~annot ?loc ~qvar:f' "" res args body' )
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = aux then_ in
    let else_' = aux else_ in
    if (cond' == cond && then_' == then_ && else_' == else_) then t else
        (trm_if ~annot ?loc cond' then_' else_')
  | Trm_seq tl ->
    let n = Mlist.length tl in
    let tl' = Mlist.mapi (fun i tsub ->
        let sub_is_terminal = (is_terminal && i == n-1) in
        f sub_is_terminal tsub
      ) tl in
    if (Mlist.for_all2 (==) tl tl') then t else
        (trm_seq ~annot ?loc tl')
  | Trm_apps (func, args) ->
    let func' = f false func in
    let args' = flist args in
    if (func' == func && args' == args) then t else
      (trm_apps ~annot ?loc ?typ func' args')
  | Trm_while (cond, body) ->
    let cond' = f false cond in
    let body' = f false body in
    if (cond' == cond && body' == body) then t else
        (trm_while ~annot ?loc cond' body')
  | Trm_for_c (init, cond, step, body) ->
     let init' = f false init in
     let cond' = f false cond in
     let step' = f false step in
     let body' = aux body in
     if (init' == init && cond' == cond && step' == step && body' == body) then t else
         (trm_for_c ~annot ?loc init' cond' step' body')
  | Trm_for (l_range, body) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let start' = f false start in
    let stop' = f false stop in
    let step' = match step with
      | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
      | Step sp -> Step (aux sp)
      in
    let body' = aux body in
    if (step' == step && start' == start && stop' == stop && body' == body) then t else
        (trm_for ~annot ?loc (index, start', direction, stop', step', is_parallel) body')
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, aux body)) cases in
     if (cond' == cond && List.for_all2 (fun (_tl1,body1) (_tl2,body2) -> body1 == body2) cases' cases) then t else
         (trm_switch ~annot ?loc cond' cases')
  | Trm_abort a ->
    begin match a with
    | Ret (Some t') ->
        let t'2 = f false t' in
        if (t'2 == t') then t else
            (trm_ret ~annot ?loc (Some t'2))
    | _ -> t
    end
  | _ -> t


(* [trm_map_with_terminal is_terminal f t] *)
let trm_map_with_terminal (is_terminal : bool)  (f : bool -> trm -> trm) (t : trm) : trm =
  (* TODO FIXME: trm_map_with_terminal_opt is_terminal f t*)
  trm_map_with_terminal_unopt is_terminal f t

(* [trm_map f]: applies f on t recursively *)
let trm_map (f : trm -> trm) (t : trm) : trm =
  trm_map_with_terminal false (fun _is_terminal t -> f t) t


(* [trm_iter f t]: similar to [trm_map] but this one doesn't return a trm at the end. *)
let trm_iter (f : trm -> unit) (t : trm) : unit =
  match t.desc with
  | Trm_array tl ->
    Mlist.iter f tl
  | Trm_record tl ->
    Mlist.iter (function (_, t) -> f t) tl
  | Trm_let (vk, tv, init) ->
    f init
  | Trm_let_mult (vk, tvl, tl) ->
    List.iter f tl
  | Trm_let_fun (f', res, args, body) ->
    f body
  | Trm_if (cond, then_, else_) ->
    f cond;  f then_ ; f else_
  | Trm_seq tl ->
    Mlist.iter f tl
  | Trm_apps (func, args) ->
    f func; List.iter f args
  | Trm_while (cond, body) ->
    f cond; f body
  | Trm_for_c (init, cond, step, body) ->
    f init; f cond; f step; f body
  | Trm_for (l_range, body) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    f start; f stop;
    begin match step with
     | Post_inc | Post_dec | Pre_inc | Pre_dec -> ()
     | Step sp -> f sp
    end;
    f body
  | Trm_do_while (body, cond) ->
    f body; f cond
  | Trm_switch (cond, cases) ->
     f cond;
     List.iter (fun (tl, body) -> f body) cases
  | Trm_abort a ->
    begin match a with
    | Ret (Some t') -> f t'
    | _ -> ()
    end
  | Trm_namespace (name, t, b) ->
    f t
  | Trm_delete (is_array, t) ->
    f t
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rfl ->
      List.iter (fun (rf, rf_ann) ->
        match rf with
        | Record_field_method t1 -> f t1
        | _ -> ()
      ) rfl
    | _ -> ()
    end
  | _ -> ()


(* [is_generated_typ ty]: checks ia a typ is a type used only for optitrust encoding *)
let is_generated_typ (ty : typ) : bool =
  List.mem GeneratedTyp ty.typ_attributes

(* [typ_map f ty]: applies f on type ty recursively *)
let typ_map (f : typ -> typ) (ty : typ) : typ =
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr {ptr_kind= pk; inner_typ = ty} -> typ_ptr ~annot ~attributes pk (f ty)
  | Typ_array (ty, n) -> typ_array ~annot ~attributes (f ty) n
  | Typ_fun (tyl, ty) ->
     typ_fun ~annot ~attributes (List.map f tyl) (f ty)
  (* var, unit, int, float, double, bool, char *)
  | _ -> ty


(* [label_subterms_with_fresh_stringreprids f t]: annotates all the subterms of [t]
   that satisfy the boolean predicate [f] with a fresh string representation identifier.
   This operation should be performed to enable the term to doc function to memoize
   its results, and possibly export a table mapping subterms to their string representation. *)
let rec label_subterms_with_fresh_stringreprids (f : trm -> bool) (t : trm) : trm =
  let t2 =
    if not (f t) then t else begin
      let id = next_stringreprid () in
      trm_set_stringreprid id t
    end in
  trm_map (label_subterms_with_fresh_stringreprids f) t2


(* [contains_decl x t]: checks if t constains a sub-trm that is a redeclaration of a variable x *)
let contains_decl (x : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_let (_, (y, _), _) when y = x -> true
    | Trm_seq tl -> Mlist.fold_left (fun acc t -> acc || aux t) false tl
    | Trm_for (l_range, body) ->
        let (y, _, _, _, _, _) = l_range in
        y = x || aux body
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_for_c (init, _, _, body) -> aux init || aux body
    | _ -> false
  in aux t

(* [is_qvar_var]: checks is [qv.qvar_var] is equal to [v]. *)
let is_qvar_var (qv : qvar) (v : var) : bool =
  qv.qvar_var = v


(* [contains_occurrence x t]: checks if [t] contains any occurrence of the variable [x]*)
let contains_occurrence (x : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_var (_, y) -> is_qvar_var y x
    | Trm_apps (_, tl) -> List.fold_left (fun acc t1 -> acc || aux t1) false tl
    | _ -> false
  in aux t

(* [contains_field_access f t]: checks if [t] contains an access on field [f] *)
let contains_field_access (f : field) (t : trm) : bool =
  let rec aux (t : trm) : bool =
   match t.desc with
   | Trm_apps (f', tl) ->
      begin match f'.desc with
      | Trm_val (Val_prim (Prim_unop (Unop_struct_access f1))) -> f = f1
      | Trm_val (Val_prim (Prim_unop (Unop_struct_get f1))) -> f = f1
      | _ -> List.fold_left (fun acc t1 -> acc || aux t1) false tl
      end
   | _ -> false
  in aux t

(* [decl_name t]: returns the name of the declared object *)
let decl_name (t : trm) : var option =
  match t.desc with
  | Trm_let (_,(x,_),_) -> Some x
  | Trm_let_fun (f, _, _, _) -> Some f.qvar_var
  | Trm_typedef td -> Some td.typdef_tconstr
  | _ -> None

(* [vars_bound_in_trm_init t]: gets the list of variables that are bound inside the initialization trm of the for_c loop*)
let vars_bound_in_trm_init (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x,_), _) -> [x]
  | Trm_let_mult (_, tvl, _) -> fst (List.split tvl)
  | _ -> []

(* [is_null_pointer ty t]: check if t == (void * ) 0 *)
let is_null_pointer (ty : typ) (t : trm) : bool =
  match ty.typ_desc, t.desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = {typ_desc = Typ_unit;_}}, Trm_val (Val_lit (Lit_int 0)) -> true
  | _ -> false

(* [is_qvar_eq qv1 qv2]: checks is qv1 = qv2. *)
let is_qvar_eq (qv1 : qvar) (qv2 : qvar) : bool =
  (qv1.qvar_var = qv2.qvar_var) && (qv1.qvar_path = qv2.qvar_path) && (qv1.qvar_str = qv2.qvar_str)


(* [same_sizes sz1 sz2]: checks if two arrays are of the same size *)
let same_sizes (sz1 : size) (sz2 : size) : bool =
 match sz1, sz2 with
 | Undefined, Undefined -> true
 | Const i1, Const i2 -> i1 = i2
 | Trm t1, Trm t2->  t1 = t2
 | _, _ -> false

(* [same_types ~match_generated_start typ_1 typ_2]: checks if two types are the same *)
let rec same_types ?(match_generated_star : bool = false) (typ_1 : typ) (typ_2 : typ) : bool =
  let aux = same_types ~match_generated_star in
  (typ_1.typ_annot = typ_2.typ_annot) && (
    match typ_1.typ_desc, typ_2.typ_desc with
    | Typ_const typ_a1, Typ_const typ_a2 ->
      (aux typ_a1 typ_a2)
    | Typ_var (a1, _), Typ_var (a2, _) ->
      a1 = a2
    | Typ_constr (typ_var1, typ_id1, typ_list1), Typ_constr (typ_var2, typ_id2, typ_list2) ->
      (is_qvar_eq typ_var1 typ_var2) && (typ_id1 = typ_id2) && (typ_list1 = typ_list2)
    | Typ_unit, Typ_unit -> true
    | Typ_int, Typ_int -> true
    | Typ_float, Typ_float -> true
    | Typ_double, Typ_double -> true
    | Typ_bool, Typ_bool -> true
    | Typ_char, Typ_char -> true
    | Typ_string, Typ_string -> true
    | Typ_ptr {ptr_kind = pk1; inner_typ = typ_a1}, Typ_ptr {ptr_kind = pk2; inner_typ = typ_a2} ->
     if match_generated_star then (pk1 = pk2) && (is_generated_typ typ_1 && is_generated_typ typ_2) && (aux typ_a1 typ_a2)
      else (not (is_generated_typ typ_1 || is_generated_typ typ_2)) && (pk1 = pk2) && (aux typ_a1 typ_a2)
    | Typ_array (typa1, size1), Typ_array (typa2, size2) ->
        (same_types typa1 typa2) && (same_sizes size1 size2)
    | _, _ -> false)

(* [get_init_val t]: gets the value of a variable initialization *)
let rec get_init_val (t : trm) : trm option =
  match t.desc with
  | Trm_let (vk, (_, _), init) ->
      begin match vk with
      | Var_immutable -> Some init
      | _ -> get_init_val init
      end
  | Trm_apps(f,[base]) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_new _)) -> Some base
        | _ -> Some t
        end
  | Trm_val (Val_prim (Prim_new _))  -> None
  | _ ->
      Some t

(* [for_loop_index t]: returns the index of the loop [t] *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for (l_range,  _) ->
     let (index, _, _, _, _, _) = l_range in
     index
  | Trm_for_c (init, _, _, _) ->
     (* covered cases:
        - for (i = …; …)
        - for (int i = …; …) *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [{desc = Trm_var (_, x); _}; _]) -> x.qvar_var
     | _ -> begin match decl_name init with
            | Some x -> x
            | None -> fail init.loc "Ast.for_loop_index: could't get the loop index"
            end
     end
  | _ -> fail t.loc "Ast.for_loop_index: expected for loop"

(* [for_loop_init t]: returns the initial value of the loop index *)
let for_loop_init (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, _, _, _) ->
     (* covered cases:
        - for (i = n; …)
        - for (int i = n; …) *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; n]) -> n
     | Trm_let (_,(_, _), init) ->
        begin match get_init_val init with
        | Some v  -> v
        | None -> fail init.loc "Ast.for_loop_init: bad for loop initialization"
        end
     | _ -> fail init.loc "Ast.for_loop_init: bad for loop initialisation"
     end
  | _ -> fail t.loc "Ast.for_loop_init: expected for loop"

(* [for_loop_bound t]: returns the bound of the for loop *)
let for_loop_bound (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, cond, _, _) ->
     (* covered cases:
       - for (…; i < n; …)
       - for (…; i <= n; …)
       - for (…; i > n; …)
       - for (…; i >= n; …) *)
     begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _},
                 [_; n]) -> n
     | _ -> fail cond.loc "Ast.for_loop_bound: bad for loop condition"
     end
  | _ -> fail t.loc "Ast.for_loop_bound: expected for loop"

(* [for_loop_step t]: returns the step increment of the for loop *)
let for_loop_step (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, _, step, _) ->
     (* covered cases:
        - for (…; …; i++)
        - for (…; …; ++i)
        - for (…; …; i--)
        - for (…; …; --i)
        - for (…; …; i += n) for n > 0
        - for (…; …; i -= n) for n > 0 *)
     begin match step.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_inc)); _}, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_inc)); _}, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_dec)); _}, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_dec)); _}, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; t']) ->
        begin match t'.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_add)); _},
                    [_; n]) ->
           n
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_sub)); _},
                    [_; n]) ->
           trm_apps (trm_unop Unop_minus) [n]
        | _ -> fail step.loc "Ast.for_loop_step: bad for loop step"
        end
     | _ -> fail step.loc "Ast.for_loop_step: bad for loop step"
     end
  | _ -> fail t.loc "Ast.for_loop_step: expected for loop"

(* [for_loop_nb_iter t]: gets the number of iterations of a for loop *)
let for_loop_nb_iter (t : trm) : trm =
  let init = for_loop_init t in
  let bound = for_loop_bound t in
  let step = for_loop_step t in
  (* reorder to use positive step *)
  let (init, bound, step) =
    match step.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_minus)); _}, [step']) ->
       (bound, init, step')
    | _ -> (init, bound, step)
  in
  match init.desc, bound.desc, step.desc with
  (* if all vars are integers, perform the computation to simplify *)
  | Trm_val (Val_lit (Lit_int init)), Trm_val (Val_lit (Lit_int bound)),
    Trm_val (Val_lit (Lit_int step)) ->
     trm_lit (Lit_int ((bound - init + step - 1) / step))
  (* otherwise, use the same formula with terms *)
  | _ ->
     trm_apps (trm_binop Binop_div)
       [
         trm_apps (trm_binop Binop_sub)
           [
             trm_apps (trm_binop Binop_add)
               [
                 trm_apps (trm_binop Binop_sub)
                   [
                     bound;
                     init
                   ];
                 step
               ];
             trm_lit (Lit_int 1)
           ];
         step
       ]

(* [for_loop_body_trms t]: gets the list of trms from the body of the loop *)
let for_loop_body_trms (t : trm) : trm mlist =
  match t.desc with
  | Trm_for (_, body) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> fail body.loc "Ast.for_loop_body_trms: body of a simple loop should be a sequence"
    end
  | Trm_for_c (_, _, _,  body) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> fail body.loc "Ast.for_loop_body_trms: body of a generic loop should be a sequence"
    end
  | _ -> fail t.loc "Ast.for_loop_body_trms: expected a loop"

(* [typ_kind]: initialization type kind *)
type typ_kind =
  | Typ_kind_undefined
  | Typ_kind_reference
  | Typ_kind_array
  | Typ_kind_sum
  | Typ_kind_record
  | Typ_kind_basic of typ_desc
  | Typ_kind_fun
  | Typ_kind_var

(* [typ_kind_to_string tpk]: converts a type kind to a string *)
let typ_kind_to_string (tpk : typ_kind) : string =
  begin match tpk with
  | Typ_kind_undefined -> "undefined"
  | Typ_kind_reference -> "reference"
  | Typ_kind_array -> "array"
  | Typ_kind_sum -> "sum"
  | Typ_kind_record -> "prod"
  | Typ_kind_basic _ -> "basic"
  | Typ_kind_fun -> "fun"
  | Typ_kind_var -> "var"
  end

(* [is_atomic_typ ty]: checks if [ty] is an atomic type *)
let is_atomic_typ (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_int | Typ_unit | Typ_float | Typ_double | Typ_bool | Typ_char |Typ_string -> true
  | _ -> false

(* [get_typ_kind ctx ty]: based on the context [ctx], get the kind of type [ty] *)
let rec get_typ_kind (ctx : ctx) (ty : typ) : typ_kind =
  if is_atomic_typ ty then Typ_kind_basic ty.typ_desc
    else
  match ty.typ_desc with
  | Typ_const ty1 -> get_typ_kind ctx ty1
  | Typ_ptr rf when rf.ptr_kind = Ptr_kind_ref -> Typ_kind_reference
  | (Typ_ptr _| Typ_array _) -> Typ_kind_array
  | Typ_fun _ -> Typ_kind_fun
  | Typ_var _ -> Typ_kind_var
  | Typ_constr (_, tyid, _) ->
     let td_opt = Typ_map.find_opt tyid ctx.ctx_typedef in
     begin match td_opt with
     | None -> Typ_kind_undefined
     | Some td ->
         begin match td.typdef_body with
        | Typdef_alias ty1 -> get_typ_kind ctx ty1
        | Typdef_record _ -> Typ_kind_record
        | Typdef_sum _| Typdef_enum _ -> Typ_kind_sum
        end
     end
  | _ -> Typ_kind_basic ty.typ_desc

(* [get_inner_ptr_type ty]: gets the underlying type of [ty] when [ty] is a generated pointer type *)
let get_inner_ptr_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_ptr {inner_typ = ty1;_} when is_generated_typ ty -> ty1
  | _ -> ty

(* [get_inner_array_type ty]: returns the underlying type of [ty] when [ty] is an array type. *)
let get_inner_array_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_array (ty, _) -> ty
  | _ -> ty


(* [get_inner_const_type ty]: gets the underlying type of [ty] when [ty] is a const type *)
let get_inner_const_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_const ty -> ty
  | _ -> ty

(* [get_inner_type ty]: returns the inner type of [ty] when [ty] is a pointer type, const type or an array type. *)
let get_inner_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_const ty -> ty
  | Typ_ptr {inner_typ = ty; _} -> ty
  | Typ_array (ty, _) -> ty
  | _ -> ty


(* [decl_type t]: returns the type of declaration [t]. *)
let decl_type (t : trm) : typ option =
  match t.desc with
  | Trm_let (_, (_, tx), _) -> Some (get_inner_ptr_type tx)
  | Trm_let_fun (_, ty, _, _) -> Some ty
  | _ -> None


(* [is_reference]: checks if the type is a reference type or not *)
let is_reference (ty : typ) : bool =
  let ty = get_inner_ptr_type ty in
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref;_} -> true
  | _ -> false

(* [is_typ_const ty]: checks if [ty] is a const type *)
let is_typ_const (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_const _ -> true
  (* | Typ_array (ty, s) -> is_typ_const ty *)
  | _ -> false

(* [tile_bound]: used for loop tiling transformation *)
type tile_bound = TileBoundMin | TileBoundAnd | TileDivides

let tile_bound_to_string = function
  | TileBoundMin -> "TileBoundMin"
  | TileBoundAnd -> "TileBoundAnd"
  | TileDivides -> "TileDivides"

(* [Nobrace]: module for managing nobrace sequences(hidden sequences), these sequence are visible only at the AST level *)
module Nobrace = struct

  let ids = ref []

  let current_id = ref 0

  let init () =
    ids := !current_id :: !ids

  let enter () =
    current_id := !current_id + 1;
    ids := !current_id :: !ids

  let current () =
    match !ids with
    | [] ->  failwith "current:empty list"
    | id :: _rest -> id

  let exit () =
    match !ids with
    | [] -> failwith "exit: empty list"
    | id :: rest ->
        ids := rest;
        id
end

(* [trm_seq_no_brace tl]: genereates a no_brace sequence with a fresh id *)
let trm_seq_no_brace (tl : trms) : trm=
    trm_add_cstyle (No_braces (Nobrace.current())) (trm_seq (Mlist.of_list tl))

(* [trm_main_inv_toplevel_defs ast]: returns a list of all toplevel declarations *)
let trm_main_inv_toplevel_defs (ast : trm) : trm list =
  match ast.desc with
  | Trm_seq tl when trm_is_mainfile ast -> Mlist.to_list tl
  | _ -> fail ast.loc "Ast.trm_main_inv_toplevel_defs: expected the ast of the main file"

(* [trm_seq_add_last t_insert t]: appends [t_insert] at the end of the sequence [t] *)
let trm_seq_add_last (t_insert : trm) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
     let new_tl = Mlist.insert_at (Mlist.length tl) t_insert tl in
     trm_seq ~annot:t.annot new_tl
  | _ -> fail t.loc "Ast.trm_seq_add_last: expected a sequence"

(* [get_nobrace_id t]: gets the id of the sequence annotated as No_braces *)
let get_nobrace_id (t : trm) : int option =
  let rec aux l = match l with
  | [] -> None
  | hd :: tl ->
    begin match hd with
    | No_braces i -> Some i
    | _ -> aux tl
    end in
  aux t.annot.trm_annot_cstyle

(* [rename]: variable renaming based on the suffix or by using a predefined list of pairs, where each pair gives the
    current variable and the one that is going to replace it *)
type rename = | Suffix of string | Rename_list of (var * var) list

(* [get_lit_from_trm_lit t]: gets the literal value from a trm_lit *)
let get_lit_from_trm_lit (t : trm) : lit =
  match t.desc with
  | Trm_val (Val_lit l) -> l
  | _ -> fail t.loc "Ast.get_lit_from_trm: this type of literal is not supported"

(* [is_type_unit t]: checks if the [t] has type void *)
let is_type_unit (t : typ) : bool =
  match t.typ_desc with
  | Typ_unit -> true
  | _ -> false

(* [is_lit t]: checks if [t] is a literal or not *)
let is_lit (t : trm) : bool =
  match t.desc with
  | Trm_val (Val_lit _) -> true
  | _ -> false

(* [is_typ_ptr ty]: checks if [ty] is a pointer type *)
let is_typ_ptr (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut;_} -> true
  | _ -> false

(* [is_typ_fun ty]: checks if [ty] is a function type *)
let is_typ_fun (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_fun _ -> true | _ -> false

(* [is_typ_struct struct_name ty]: checks if [ty] is a constructed struct type *)
let is_typ_struct (struct_name : var) (ty_opt : typ option) : bool =
  match ty_opt with
  | Some ty ->
    begin match ty.typ_desc with
    | Typ_constr (sn, _, _) -> is_qvar_var sn struct_name
    | _ -> false
    end
  | None -> false

(* [is_get_operation t]: checks if [t] is a get operation(read operation) *)
let is_get_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({desc = Trm_val(Val_prim (Prim_unop Unop_get))}, _) -> true
  | _ -> false

(* [is_new_operation t] checks if [t] is new operation *)
let is_new_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some (Prim_new _) -> true
    | _ -> false
    end
  | _ -> false

let trm_set_inv (t : trm) : (trm * trm) option =
  trm_binop_inv Binop_set t

(* [is_set_operation t]: checks if [t] is a set operation(write operation) *)
let is_set_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_set) | Some(Prim_compound_assgn_op _)
    (* FIXME: not supported by [trm_set_inv] *)
     | Some (Prim_overloaded_op (Prim_binop Binop_set)) -> true
    | _ -> false
    end
  | _ -> false


(* [is_compound_assignment]: checks if [t] is a compound assignment *)
let is_compound_assignment (t : trm) : bool =
  match t.desc with
  | Trm_apps ({ desc = Trm_val (Val_prim (Prim_compound_assgn_op _))}, _) -> true
  | _ -> false

(* [is_access t]: check if t is a struct or array access *)
let is_access (t : trm) : bool = match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some p ->
      begin match p with
      | Prim_unop (Unop_struct_access _) | Prim_unop (Unop_struct_get _) | Prim_binop (Binop_array_access)
        | Prim_binop (Binop_array_get) -> true
      | _ -> false
      end
    | None -> false
    end
  | _ -> false

(* [get_operation_arg t]: gets the arg of a get operation. *)
let get_operation_arg (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) -> t1
  | _ -> t

(* [new_operation_arg t]: get the argument of the encoded new operation. *)
let new_operation_arg (t : trm) : trm =
  match t.desc with
  | Trm_apps (_, [arg]) when is_new_operation t -> arg
  | _ -> t


(* [new_operation_inv t]: returns the type and the argument of the new operation [t]. *)
let new_operation_inv (t : trm) : (typ * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_new ty))}, [arg]) -> Some (ty, arg)
  | _ -> None


(* [trm_let_mut ~annot ?ctx typed_var init]: an extension of trm_let for
    creating mutable variable declarations *)
let trm_let_mut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr_generated var_type in
  let t_let = trm_let ?loc ?ctx Var_mutable (var_name, var_type_ptr) (trm_apps (trm_prim (Prim_new var_type)) [init]) in
  trm_add_cstyle Stackvar t_let

(* [trm_let_ref ~annot ?ctx typed_var init]: an extension of trm_let for creating references *)
let trm_let_ref ?(annot = trm_annot_default) ?(loc)  ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr_generated var_type in
  let t_let = trm_let ?loc ?ctx Var_mutable (var_name, var_type_ptr) init in
  trm_add_cstyle Reference t_let


(* [trm_let_IMmut ~annot ?ctx typed_var init]: an extension of trm_let for creating immutable variable declarations. *)
let trm_let_immut ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option)
  (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type = typ_const var_type in
  trm_let ~annot ?loc ?ctx Var_immutable (var_name, var_type) (init)

(* [trm_let_array ~annot ?ctx typed_var sz init]: an extension of trm_let for creating array variable declarations *)
let trm_let_array ?(annot = trm_annot_default) ?(loc) ?(ctx : ctx option) (kind : varkind )
  (typed_var : typed_var) (sz : size)(init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type = if kind = Var_immutable then typ_const (typ_array var_type sz) else typ_ptr_generated (typ_array var_type sz) in
  let var_init = if kind = Var_immutable then init else trm_apps (trm_prim (Prim_new var_type)) [init]  in
  let res = trm_let ~annot ?loc ?ctx kind (var_name, var_type) var_init in
  if kind = Var_mutable then trm_add_cstyle Stackvar res else res


(* [trm_for_c_inv_simple_init init]: checks if the init loop component is simple. If that's the case then return
   initial value of the loop index.
  Ex.:
    int x = a -> Some (x, a, true)
    x = a -> Some (x, a, false) *)
let trm_for_c_inv_simple_init (init : trm) : (var * trm) option =
  match init.desc with
  | Trm_let (_, (x, _), init_val) ->
    begin match get_init_val init_val with
    | Some init1 -> Some (x, init1)
    | _ -> None
    end
  | _ -> None


(* [trm_for_c_inv_simple_stop stop]: checks if the loop bound is simple.  If that's the case return that bound. *)
let trm_for_c_inv_simple_stop (stop : trm) : (loop_dir * trm) option =
  match stop.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                 [_; n]) -> Some (DirUp, n)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
              [_; n]) -> Some (DirUpEq, n)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
              [_; n]) -> Some (DirDown, n)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _},
              [_; n]) -> Some (DirDownEq, n)
  | _ -> None

(* [trm_for_c_inv_simple_step step]: checks if the loop step is simple. If that's the case then return that step. *)
let trm_for_c_inv_simple_step (step : trm) : loop_step option =
  match step.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_inc)); _}, _) ->
      Some Post_inc
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_inc)); _}, _) ->
     Some Pre_inc
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_post_dec)); _}, _) ->
     Some Post_dec
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_pre_dec)); _}, _) ->
     Some Pre_dec
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op _)); _},
                 [_; n]) -> Some (Step n)
  | _ -> None


(* [trm_for_of_trm_for_c t]: checks if loops [t] is a simple loop or not, if yes then return the simple loop else returns [t]. *)
let trm_for_of_trm_for_c (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, cond , step, body) ->
    let init_ops = trm_for_c_inv_simple_init init in
    let bound_ops = trm_for_c_inv_simple_stop cond in
    let step_ops = trm_for_c_inv_simple_step step in
    let t_pragmas = trm_get_pragmas t in
    let is_parallel = List.exists (function | Parallel_for _ -> true | _ -> false) t_pragmas in
    begin match init_ops, bound_ops, step_ops with
    | Some (index, start), Some (direction, stop), Some step ->
      trm_for (index, start, direction, stop, step, is_parallel ) body
    | _ -> t
    end
  | _ -> fail t.loc "Ast.trm_for_of_trm_for_c: expected a for loop"

(* TODO: rename to monoid *)
(* [local_ops]: type used for the local_name transformation. *)
type local_ops =
(* | Functional_monoid of trm * trm (* 0 and +; what about += ? *)
   | Imperative_monoid of trm * trm (* create 0 and mutating += (e.g. bag extend) and others (e.g. bag push) *)
   Maybe should only take += even for functional
      *)
  | Local_arith of lit * binary_op
  | Local_obj of string * string * string

(* [get_include_filename t]: returns the included filename if [t] is an include directive. *)
let get_include_filename (t : trm) : string  =
  let f_name = List.fold_left (fun acc x ->
    match x with
    | Include s -> Some s
    | _ -> acc
  ) None t.annot.trm_annot_files in
  match f_name with
  | Some s -> s
  | _ -> fail t.loc "Ast.get_include_filename: couldn't get the requested filename"

(* [compute_app_unop_value p v1]: simplifies unary operations on literals. *)
let compute_app_unop_value (p : unary_op) (v1:lit) : trm =
  match p, v1 with
  | Unop_neg, Lit_bool b -> trm_bool (not b)
  | Unop_post_inc, Lit_int n -> trm_int (n + 1)
  | Unop_pre_inc, Lit_int n -> trm_int (n + 1)
  | Unop_post_dec, Lit_int n -> trm_int (n - 1)
  | Unop_pre_dec, Lit_int n -> trm_int (n - 1)
  | _ -> fail None "Ast.compute_app_unop_value: only negation can be applied here"

(* [compute_app_binop_value]: simplifies binary operations on literals. *)
let compute_app_binop_value (p : binary_op) (typ1 : typ option) (typ2 : typ option) (v1 : lit) (v2 : lit) : trm =
  match p,v1, v2 with
  | Binop_eq , Lit_int n1, Lit_int n2 -> trm_bool (n1 == n2)
  | Binop_eq, Lit_double d1, Lit_double d2 -> trm_bool (d1 == d2)
  | Binop_neq , Lit_int n1, Lit_int n2 -> trm_bool (n1 <> n2)
  | Binop_neq, Lit_double d1, Lit_double d2 -> trm_bool (d1 <> d2)
  | Binop_sub, Lit_int n1, Lit_int n2 -> trm_int (n1 - n2)
  | Binop_sub, Lit_double d1, Lit_double d2 ->
    let typ = Tools.option_or typ1 typ2 in
    trm_float ?typ (d1 -. d2)
  | Binop_add, Lit_int n1, Lit_int n2 -> trm_int (n1 + n2)
  | Binop_add, Lit_double d1, Lit_double d2 ->
    let typ = Tools.option_or typ1 typ2 in
    trm_float ?typ (d1 +. d2)
  | Binop_mul, Lit_int n1, Lit_int n2 -> trm_int (n1 * n2)
  | Binop_mul, Lit_double n1, Lit_double n2 ->
    let typ = Tools.option_or typ1 typ2 in
    trm_float ?typ (n1 *. n2)
  | Binop_mod, Lit_int n1, Lit_int n2 -> trm_int (n1 mod n2)
  | Binop_div, Lit_int n1, Lit_int n2 -> trm_int (n1 / n2)
  | Binop_div, Lit_double d1, Lit_double d2 ->
    let typ = Tools.option_or typ1 typ2 in
    trm_float ?typ (d1 /. d2)
  | Binop_le, Lit_int n1, Lit_int n2 -> trm_bool (n1 <= n2)
  | Binop_le, Lit_double d1, Lit_double d2 -> trm_bool (d1 <= d2)
  | Binop_lt, Lit_int n1, Lit_int n2 -> trm_bool (n1 < n2)
  | Binop_lt, Lit_double d1, Lit_double d2 -> trm_bool (d1 < d2)
  | Binop_ge, Lit_int n1, Lit_int n2 -> trm_bool (n1 >= n2)
  | Binop_ge, Lit_double d1, Lit_double d2 -> trm_bool (d1 >= d2)
  | Binop_gt, Lit_int n1, Lit_int n2 -> trm_bool (n1 > n2)
  | Binop_gt, Lit_double d1, Lit_double d2 -> trm_bool (d1 > d2)
  | _ -> fail None "Ast.compute_app_binop_value: operator not supporeted"

(* [decl_list_to_typed_vars tl]: converts a list of variable declarations to a list of paris where each pair
    consists of a variable and its type *)
let decl_list_to_typed_vars (tl : trms) : typed_vars =
  List.map (fun t ->
    match t.desc with
    | Trm_let (_, (x, tx),_) -> (x, get_inner_ptr_type tx)
    | _ -> fail t.loc "Ast.decl_list_to_typed_vars: expected a list of declarations"
  ) tl

(* [trm_is_val_or_var t]: checks if [t] is a variable occurrence or a value *)
let rec trm_is_val_or_var (t : trm) : bool =
  match t.desc with
  | Trm_val _ | Trm_var _ -> true
  | Trm_apps (_, [var_occ]) when is_get_operation t -> trm_is_val_or_var var_occ
  | _ -> false

(* [trm_for_inv t]: gets the loop range from loop [t] *)
let trm_for_inv (t : trm) : (loop_range * trm)  option =
  match t.desc with
  | Trm_for (l_range, body) -> Some (l_range, body)
  | _ -> None

(* [trm_for_inv_instrs t]: gets the loop range and body instructions from loop [t]. *)
let trm_for_inv_instrs (t : trm) : (loop_range * trm mlist) option =
  Option.bind (trm_for_inv t) (fun (r, b) ->
    Option.map (fun instrs -> (r, instrs)) (trm_seq_inv b))

(* [is_trm_seq t]: checks if [t] is a sequence. *)
let is_trm_seq (t : trm) : bool =
  match t.desc with
  | Trm_seq _ -> true  | _ -> false

(* [trm_fors rgs tbody]: creates nested loops with the main body [tbody] each nested loop
   takes its components from [rgs] *)
let trm_fors (rgs : loop_range list) (tbody : trm) : trm =
  List.fold_right (fun l_range acc ->
    trm_for l_range (if (is_trm_seq acc) then acc else trm_seq_nomarks [acc])
  ) rgs tbody


(* [trm_var_def_inv t] gets the name type and the initialization value *)
let trm_var_def_inv (t : trm) : (varkind * var * typ * trm option) option =
  match t.desc with
  | Trm_let (vk, (x,tx), init) ->
    let init1 = match get_init_val init with
    | Some init1 -> Some init1
    | _ -> None in Some (vk, x, get_inner_ptr_type tx, init1)
  | _ -> None

(* [trm_fors_inv nb t]: gets a list of loop ranges up to the loop at depth [nb] from nested loops [t] *)
let trm_fors_inv (nb : int) (t : trm) : (loop_range list * trm) option =
  let nb_loops = ref 0 in
  let body_to_return  = ref (trm_int 0) in
  let rec aux (t : trm) : loop_range list =
    match t.desc with
    | Trm_for (l_range, body) ->
      incr nb_loops;
      begin match body.desc with
      | Trm_seq tl when Mlist.length tl = 1 ->
        if !nb_loops = nb
          then begin
            body_to_return := body;
            l_range :: []
            end
          else l_range :: aux (Mlist.nth tl 0)
      | _ ->  l_range :: []
      end

    | _ -> []
    in

  let loop_range_list = aux t in
  if List.length loop_range_list <> nb then None else Some (loop_range_list, !body_to_return)

let trm_new_inv (t : trm) : (typ * trm) option =
  match trm_apps_inv t with
  | Some (f, [v]) ->
    begin match trm_prim_inv f with
    | Some (Prim_new ty) -> Some (ty, v)
    | _ -> None
    end
  | _ -> None

(* [is_trm_uninitialized t]: checks if [t] is the body of an uninitialized function or variable *)
let is_trm_uninitialized (t:trm) : bool =
  match t.desc with
  | Trm_val (Val_lit Lit_uninitialized) -> true
  | _ -> false

let is_trm_new_uninitialized (t : trm) : bool =
  match trm_new_inv t with
  | Some (_, v) -> is_trm_uninitialized v
  | None -> false

exception Unknown_key

(* [tmap_to_list keys map]: gets the list of values for all keys [keys] in [map] *)
let tmap_to_list (keys : vars) (map : tmap) : trms =
  List.map (fun x -> match Trm_map.find_opt x map with
    | Some v -> v
    | None -> raise Unknown_key
  ) keys

(* [tmap_filter keys tmap]: removes all the bindings with [keys] in [map] and return [map] *)
let tmap_filter (keys : vars) (map : tmap) : tmap =
  Trm_map.filter (fun k _ -> not (List.mem k keys)) map


(* [is_trm_arbit t]: checks if [t] is a proper trm *)
let is_trm_arbit (t : trm) : bool =
  match t.desc with
  | Trm_arbitrary _ -> true
  | _ -> false

(* [is_typ ty]: checks if [ty] is a proper type *)
let is_typ (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_arbitrary _ -> false
  | _ -> true

(* [hide_function_bodies f_pred tl]: all the toplevel function with their names satisfying [f_pred] will have hidden bodies.
    Others will be kept unchanged. The new ast is called the chopped_ast. This function wlll return the choped_ast and
    a map with keys the names of the functions whose body has been removed and values their removed body. *)
let hide_function_bodies (f_pred : var -> bool) (t : trm) : trm * tmap =
  let t_map = ref Trm_map.empty in
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_let_fun (f, ty, tv, _) ->
        if f_pred f.qvar_var then begin
          t_map := Trm_map.add f.qvar_var t !t_map;
         trm_let_fun ~annot:t.annot ~qvar:f "" ty tv (trm_lit  Lit_uninitialized) end
        else t
      | _ -> trm_map aux t
      in
  let res = aux t in
  res, !t_map

(* [update_chopped_ast chopped_ast chopped_fun_map]: for all the functions whose bodies were removed during the creation
    of the chopped_ast restore their bodies by using [chopped_fun_map], which is map with keys beingthe the names
    of the functions that were chopped and values being their actual declaration *)
let update_chopped_ast (chopped_ast : trm) (chopped_fun_map : tmap): trm =
  match chopped_ast.desc with
  | Trm_seq tl ->
      let new_tl =
      Mlist.map (fun def -> match def.desc with
      | Trm_let_fun (f, _, _, _) ->
        begin match Trm_map.find_opt f.qvar_var chopped_fun_map with
        | Some tdef ->  tdef
        | _ -> def
        end
      | Trm_let (_, (x, _), _) ->
        (* There is a slight difference between clang and menhir how they handle function declarations, that's why we
         need to check if there are variables inside the function map *)
        begin match Trm_map.find_opt x chopped_fun_map with
        | Some tdef -> tdef
        | _ -> def
        end
      |_ ->  def
    ) tl in trm_seq ~annot:chopped_ast.annot new_tl
  | _ -> fail chopped_ast.loc "Ast.update_ast_with_chopped_ast: ast of the main file should start with a top level sequence"


(* [is_infix_prim_fun p]: checks if the primitive function [p] is one of those that supports app and set operations or not *)
let is_infix_prim_fun (p : prim) : bool =
  match p with
  | Prim_compound_assgn_op __ -> true
  | Prim_binop op ->
    begin match op with
    | Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_mod | Binop_shiftl | Binop_shiftr | Binop_and | Binop_or -> true
    | _ -> false
    end
  | _ -> false

(* [get_binop_from_prim p]: if [p] is a binop operation then return its underlying binary operation *)
let get_binop_from_prim (p : prim) : binary_op option =
  match p with
  | Prim_compound_assgn_op binop -> Some binop
  | Prim_binop binop -> Some binop
  | _ -> None

(* [is_postfix_unary unop]: checks if [unop] is a postfix unary operator *)
let is_postfix_unary (unop : unary_op) : bool =
  match unop with
  | Unop_post_inc | Unop_post_dec -> true
  | _ -> false

(* [is_arith_fun p]: checks if the primitive function [p] is an arithmetic operation or not *)
let is_arith_fun (p : prim) : bool =
  match p with
  | Prim_binop bin_op ->
    begin match bin_op with
    | Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_mod -> true
    | _ -> false
    end
  | _ -> false

(* [is_prim_arith p]: checks if [p] is a primitive arithmetic operation *)
let is_prim_arith (p : prim) : bool =
  match p with
  | Prim_binop (Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_exact_div)
  | Prim_unop Unop_neg ->
      true
  | _ -> false

(* [is_prim_arith_call t]: checks if [t] is a function call to a primitive arithmetic operation *)
let is_prim_arith_call (t : trm) : bool =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim p);_}, args) when is_prim_arith p -> true
  | _ -> false

(* [is_struct_init t]: checks if [t] is struct_init *)
let is_struct_init (t : trm) : bool =
  match t.desc with
  | Trm_record _ -> true | _ -> false

(* [is_trm_seq t]: checks if [t] has [Trm_seq tl] description. *)
let is_trm_seq (t : trm) : bool =
  match t.desc with | Trm_seq _ -> true | _ -> false

(* [is_same_binop op1 op2 ]: checks if two primitive operations are the same *)
let is_same_binop (op1 : binary_op) (op2 : binary_op) : bool =
  match op1, op2 with
  | Binop_set, Binop_set -> true
  | Binop_array_access, Binop_array_access -> true
  | Binop_array_get, Binop_array_get -> true
  | Binop_eq, Binop_eq -> true
  | Binop_neq, Binop_neq -> true
  | Binop_sub, Binop_sub -> true
  | Binop_add, Binop_add -> true
  | Binop_mul, Binop_mul -> true
  | Binop_mod, Binop_mod -> true
  | Binop_div, Binop_div -> true
  | Binop_le, Binop_le -> true
  | Binop_lt, Binop_lt -> true
  | Binop_ge, Binop_ge -> true
  | Binop_gt, Binop_gt -> true
  | Binop_and, Binop_and -> true
  | Binop_bitwise_and, Binop_bitwise_and -> true
  | Binop_or, Binop_or -> true
  | Binop_bitwise_or, Binop_bitwise_or -> true
  | Binop_shiftl, Binop_shiftl -> true
  | Binop_shiftr, Binop_shiftr -> true
  | Binop_xor, Binop_xor -> true
  | _, _ -> false


(* [trm_struct_access ~annot ?typ base field]: creates a struct_access encoding *)
let trm_struct_access ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (field : var) : trm =
  trm_apps ?typ (trm_unop ~annot (Unop_struct_access field)) [base]

(* [trm_struct_get ~annot ?typ base field]: creates a struct_get encoding *)
let trm_struct_get ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (field : var) : trm =
  trm_apps ?typ (trm_unop ~annot (Unop_struct_get field)) [base]

(* [trm_array_access~annot ?typ base index]: creates array_access(base, index) encoding *)
let trm_array_access ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (index : trm) : trm =
  trm_apps ?typ (trm_binop ~annot Binop_array_access) [base; index]

(* [trm_array_get ~annot ?typ base index]: creates array_get (base, index) encoding *)
let trm_array_get ?(annot = trm_annot_default) ?(typ : typ option) (base : trm) (index : trm) : trm =
  trm_apps ?typ (trm_binop ~annot Binop_array_get) [base; index]

(* [trm_get ~annot ?typ t]: embeds [t] into a get operation *)
let trm_get ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ~annot (trm_unop Unop_get) [t]

(* [trm_address_of ~anot ?typ t]: creates an address operation in [t] *)
let trm_address_of ?(annot = trm_annot_default) ?(typ : typ option) (t : trm) : trm =
  trm_apps ?typ:t.typ (trm_unop Unop_address) [t]

(* [trm_var_get ?typ x]: generates *x *)
let trm_var_get ?(typ : typ option) (x : var) : trm =
  trm_get ?typ (trm_var ?typ x)

(* [trm_var_addr ?typ x]: generates &x*)
let trm_var_addr ?(typ : typ option) (x : var) : trm =
  trm_address_of ?typ (trm_var ?typ x)

(* [trm_var_possibly_mut ~const ?typ x]: reads the value of x, it can be x or *x  *)
let trm_var_possibly_mut ?(const : bool = false) ?(typ : typ option) (x : var) : trm =
  if const then trm_var ?typ ~kind:Var_immutable x else trm_var_get ?typ x

(* [trm_new ty t]: generates "new ty" *)
let trm_new (ty : typ) (t : trm) : trm =
  trm_apps (trm_prim (Prim_new ty)) [t]

(* [trm_any_bool]: generates ANY_BOOL () *)
let trm_any_bool : trm =
  trm_apps (trm_var "ANY_BOOL") []

(* [trm_minus ?loc ?ctx ?typ t]: generates -t *)
let trm_minus ?(loc) ?(ctx : ctx option) ?(typ) (t : trm) : trm =
  let typ = Tools.option_or typ t.typ in
  trm_apps ?loc ?ctx ?typ (trm_unop ?loc ?ctx Unop_minus) [t]

(* [trm_eq ?loc ?ctx ?typ t1 t2]: generates t1 = t2 *)
let trm_eq ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx  Binop_eq) [t1; t2]

(* [trm_neq t1 t2]: generates t1 != t2 *)
let trm_neq ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_neq) [t1; t2]

(* [trm_sub t1 t2]: generates t1 - t2 *)
let trm_sub ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_sub) [t1; t2]

(* [trm_add t1 t2]: generates t1 + t2 *)
let trm_add ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_add) [t1; t2]

(* [trm_mod t1 t2]: generates t1 % t2 *)
let trm_mod ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_mod) [t1; t2]

(* [trm_add_inv t1 t2]: deconstructs t = t1 + t2 *)
let trm_add_inv (t : trm) : (trm * trm) option  =
  trm_binop_inv Binop_add t

(* [trm_mul t1 t2]: generates t1 * t2 *)
let trm_mul ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_mul) [t1; t2]

(* [trm_div t1 t2]: generates t1 / t2 *)
let trm_div ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_div) [t1; t2]

(* [trm_exact_div t1 t2]: generates exact_div(t1, t2) *)
let trm_exact_div ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_exact_div) [t1; t2]

(* [trm_le t1 t2]: generates t1 <= t2 *)
let trm_le ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_le) [t1; t2]

(* [trm_lt t1 t2]: generates t1 < t2 *)
let trm_lt ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_lt) [t1; t2]

(* [trm_ge t1 t2]: generates t1 >= t2 *)
let trm_ge ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_ge) [t1; t2]

(* [trm_gt t1 t2]: generates t1 > t2 *)
let trm_gt ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_gt) [t1; t2]

(* [trm_ineq ineq_sgn t1 t2]: generates an inequality t1 # t2 where # is one of the following operators <, <=, >, >=.
    The operator is provided implicitly through the [ineq_sng] arg *)
let trm_ineq (ineq_sgn : loop_dir) (t1 : trm) (t2 : trm) : trm =
  match ineq_sgn with
  | DirUp -> trm_lt t1 t2
  | DirUpEq -> trm_le t1 t2
  | DirDown ->  trm_gt t1 t2
  | DirDownEq -> trm_ge t1 t2


(* [trm_and t1 t2]: generates t1 && t2 *)
let trm_and ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_and) [t1;t2]

(* [trm_bit_and t1 t2]: generates t1 & t2 *)
let trm_bit_and ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_bitwise_and) [t1;t2]

(* [trm_or t1 t2]: generates t1 || t2 *)
let trm_or ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_or typ (Some (typ_bool ())) in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_or) [t1;t2]

(* [trm_bit_or t1 t2]: generates t1 | t2 *)
let trm_bit_or ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_bitwise_or) [t1;t2]

(* [trm_shiftl t1 t2]: generates t1 << t2*)
let trm_shiftl ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_shiftl) [t1; t2]

(* [trm_shiftr t1 t2]: generates t1 >> t2*)
let trm_shiftr ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  let typ = Tools.option_ors [typ; t1.typ; t2.typ] in
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_shiftr) [t1; t2]
(* LATER [trm_fmod t1 t2]: generates fmod(t1, t2)
let trm_fmod ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_fmod) [t1;t2]
*)

(* [trm_xor t1 t2]: generates t1 ^ t2 *)
let trm_xor ?(loc) ?(ctx : ctx option) ?(typ) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ?ctx ?typ (trm_binop ?loc ?ctx Binop_xor) [t1; t2]

(* [trm_ands ts] generalized version of trm_and *)
let trm_ands (ts : trm list) : trm =
  Xlist.fold_lefti (fun i acc t1 ->
    if i = 0 then t1 else trm_and acc t1
  ) (trm_bool true) ts

(* [trm_prim_compound ~annot ?ctx ?loc ?typ binop t1 t2]: generates a compound operation, ex t1+=t2*)
let trm_prim_compound ?(annot : trm_annot = trm_annot_default) ?(ctx : ctx option) ?(loc) ?(typ)
  (binop : binary_op) (t1 : trm) (t2 : trm) : trm =
  trm_apps ?loc ~annot ?typ (trm_prim ?loc ?ctx (Prim_compound_assgn_op binop)) [t1; t2]

(* [code_to_str]: extracts the code from the trms that contain the arbitrary code. *)
let code_to_str (code : code_kind) : string =
  match code with
  | Lit l -> l
  | Atyp ty -> ty
  | Expr e -> e
  | Stmt s -> s
  | Instr s -> s

(* [AstParser]: module for integrating pieces of code given as input by the user. *)
module AstParser = struct
  let var v = trm_var v

  let var_mut v = trm_var_get v

  let lit l = code (Lit l)

  let ty ty = typ_str (Atyp ty)

  let subst_dollar_number (inst : string list) (s : string) : string =
  Xlist.fold_lefti (fun i acc insti ->
    Tools.string_subst ("${"^(string_of_int i) ^ "}") insti acc
  ) s inst

  let expr ?(vars : var list = []) (e : string)  =
    let e = if vars = [] then e else subst_dollar_number vars e in
    code (Expr e)

  let stmt s = code (Stmt s)

  let instr s = code (Instr s)

end

(* [var_mutability_unkown]: dummy value used for variable mutability *)
let var_mutability_unknown = Var_mutable

(* [top_level_fun_bindings t]: returns a map with keys the names of toplevel function names and values being their bodies *)
let top_level_fun_bindings (t : trm) : tmap =
  let tmap = ref Trm_map.empty in
    let aux (t : trm) : unit =
      match t.desc with
      | Trm_seq tl ->
        Mlist.iter (fun t1 ->
          match t1.desc with
          | Trm_let_fun (f, _, _, body) -> tmap := Trm_map.add f.qvar_var body !tmap
          | _ -> ()
        ) tl
      | _ -> fail t.loc "Ast.top_level_fun_bindings: expected the global sequence that contains all the toplevel declarations"
   in
  aux t;
  !tmap

(* [get_common_top_fun tm1 tm2]: takes two maps, binding function names to terms describing the function bodies,
    and returns the list of function names that ard bound to the same terms in the two maps. *)
let get_common_top_fun (tm1 : tmap) (tm2 : tmap) : vars =
  let common = ref [] in
  Trm_map.iter (fun f1 b1 ->
    match Trm_map.find_opt f1 tm2 with
    | Some b2 when b1 == b2 -> common := f1 :: !common
    | _ -> ()
  ) tm1;
  !common

(* [get_mutability t]: if [t] is a variable declaration or a variable occurrence then return its occurrences
    otherwise return nothing *)
let get_mutability (t : trm) : varkind option =
  match t.desc with
  | Trm_let (vk, _, _) -> Some vk
  | Trm_var (vk, _) -> Some vk
  | _ -> None


(* [empty_ast]: generates {} *)
let empty_ast : trm =
  trm_set_mainfile (trm_seq_nomarks [])

(* [trm_simplify_addressof_and_get t]: simplifies [&*t] and [*&t] to [t] *)
let trm_simplify_addressof_and_get (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) }
    ])
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [t1]) }
    ]) -> t1
  | _ -> t

(* [simpl_struct_get_get t]: transform struct_get (get(t1), f) to get(struct_access (t1, f)) *)
let simpl_struct_get_get (t : trm) : trm = (* DEPRECATED? *)
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as op, [t1]) ->
    begin match t1.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [t2]) ->
      {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [t2]))}]))}
    | _ -> t
    end
  | _ -> t

(* [simpl_array_get t]: tranform array_get (get(t1), index) to get(array_access (t1), index) *)
let rec simpl_array_get_get (t : trm) : trm = (* DEPRECATED? *)
  let aux = simpl_array_get_get in
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)));_} as op, [base; index]) ->
    begin match base.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [base1]) ->
       {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [base1; index]))}]))}
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t


(* [array_access base index]: generates array_access (base, index) *)
let array_access (base : trm) (index : trm) : trm =
  trm_apps (trm_binop Binop_array_access) [base; index]

let array_access_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access));_},
              [base;index]) -> Some (base, index)
  | _ -> None

let array_inv (t : trm) : trm mlist option =
  match t.desc with
  | Trm_array els -> Some els
  | _ -> None

let array_get_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get));_},
              [base;index]) -> Some (base, index)
  | _ -> None

(* [get_array_access base index]: generates get(array_access (base, index)) *)
let get_array_access (base : trm) (index : trm) : trm =
  trm_get (array_access base index)

(* [get_array_access_inv t] returns the Some(base, index) of an array_access if [t]
     is of the form get(array_access(base, index) otherwise None *)
let get_array_access_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg]) ->
    array_access_inv arg
  | _ -> None

(* [struct_access base index]: generates struct_access (base, index) *)
let struct_access (f : field) (base : trm) : trm =
  trm_apps (trm_unop (Unop_struct_access f)) [base]

(* [get_struct_access base index]: generates get(struct_access (base, index)) *)
let get_struct_access (f : field) (base : trm) : trm =
  trm_get (struct_access f base)

(* [struct_access_inv t]: if [t] is  a struct access then return its base and the accessed field; else Npone *)
let struct_access_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)));_}, [base]) -> Some (base, f)
  | _ -> None

(* [struct_access_inv_some t]: if [t] is  a struct access then return its base and the accessed field *)
let struct_access_inv_some (t : trm) : (trm * field) =
  match struct_access_inv t with
  | None -> assert false
  | Some r -> r

(* [struct_get_inv t]: if [t] is a struct get then return its base and the accesses field; else none *)
let struct_get_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_}, [base]) -> Some (base, f)
  | _ -> None

(* [struct_get_inv_some t]: if [t] is a struct get then return its base and the accesses field *)
let struct_get_inv_some (t : trm) : (trm * field) =
  match struct_get_inv t with
  | None -> assert false
  | Some r -> r

(* [get_struct_access_inv t]: if [t] is of the form get(struct_access (f, base)) returns Some (f,base); else None *)
let get_struct_access_inv (t : trm) : (trm * field) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg]) -> struct_access_inv arg
  | _ -> None

(* [get_struct_access_inv_some t]: if [t] is of the form get(struct_access (f, base)) returns (f,base) *)
let get_struct_access_inv_some (t : trm) : (trm * field) =
  match get_struct_access_inv t with
  | None -> assert false
  | Some r -> r


(* [set_struct_access_inv t]: if [t] is a write on a struct access, then return the base, the field of that access
    and the value that has been assigned to; else None *)
let set_struct_access_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs]) when is_set_operation t ->
   begin match struct_access_inv lhs with
   | Some (base, f) -> Some (base, f, rhs)
   | _ -> None
   end
  | _ -> None

(* [set_struct_access_inv t]: if [t] is a write operation on a struct field, then it will return the base, the field and the
     value that has been assigned to.  *)
let set_struct_access_inv_some (t : trm) : (trm * field * trm) =
  match set_struct_access_inv t with
  | None -> assert false
  | Some r -> r


(* [set_struct_get_inv t]: if [t] is a write operation on a struct field, then it will return the base, the field and the
     value that has been assigned to.  *)
let set_struct_get_inv (t : trm) : (trm * field * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs]) when is_set_operation t ->
    begin match struct_get_inv lhs with
    | Some (base, f) -> Some (base, f, rhs)
    | _ -> None
    end
  | _ -> None


(* [set_struct_get_inv_some t]: similar to [set_struct_get_inv] but this one fails if [t] is not a write on a struct_get operation.  *)
let set_struct_get_inv_some (t : trm) : (trm * field * trm) =
  match set_struct_get_inv t with
  | None -> assert false
  | Some r -> r

(* [struct_init_inv t]: if is t is a struct initialization, get the list of terms; else None *)
let struct_init_inv (t : trm) : (label option * trm) mlist option =
  match t.desc with
  | Trm_record sl -> Some sl
  | _ -> None

(* [struct_init_inv_some t]: gets struct initialization list trms *)
let struct_init_inv_some (t : trm) : (label option * trm) mlist =
 match struct_init_inv t with
  | None -> assert false
  | Some r -> r

(* [set_inv t]: gets the lhs and the rhs of a set(write) operation *)
let set_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps (_, [lhs; rhs]) when is_set_operation  t-> Some (lhs, rhs)
  | _ -> None

(* [trm_var_assoc_list to_map al]: creats a map from an association list wher keys are string and values are trms *)
let map_from_trm_var_assoc_list (al : (string * trm) list) : tmap =
  let tm = Trm_map.empty in
  List.fold_left (fun acc (k, v) -> Trm_map.add k v acc) tm al

(* [typ_align align ty]: adds the alignas attribute to type ty *)
let typ_align (align : trm) (ty : typ) =
  typ_add_attribute (Alignas align) ty

(* [typedef_get_members ~access t]: returns all the memebers of typedef [t]. If [access] is provided as an argument
     then only members with the specified access_control are returned. *)
let typedef_get_members ?(access : access_control option) (t : trm) : (label * typ) list =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rf ->
      List.fold_left (fun acc (rf, rf_ann) ->
        match rf with
        | Record_field_member (lb, ty) ->
          begin match access with
          | Some accs -> if accs = rf_ann then (lb, ty) :: acc else acc
          | None -> (lb, ty) :: acc
          end
        | Record_field_method _ -> acc
      ) [] (List.rev rf)
    | _ -> fail t.loc "Ast.typdef_get_members: this function should be called only for typedef structs and classes"
    end
  | _ -> fail t.loc "Ast.typedef_get_members: can't get members of a trm that's not a type definition."


(* [typedef_get_methods ~access t]: returns all the methods of typedef [t]. If [access] is provided as an argument
      then only methods with the specified access_control are returned. *)
let typedef_get_methods ?(access : access_control option) (t : trm) : trm list =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rf ->
      List.fold_left (fun acc (rf, rf_ann) ->
        match rf with
        | Record_field_member _fm ->  acc
        | Record_field_method trm ->
          begin match access with
          | Some accss -> if accss = rf_ann then trm :: acc else acc
          | None -> trm :: acc
          end
      ) [] (List.rev rf)
    | _ -> fail t.loc "Ast.typdef_get_methods: this function should be called only for typedef structs and classes."
    end
  | _ -> fail t.loc "Ast.typedef_get_methods: can't get methods of a trm that's not a type definition. "

(* [typedef_get_all_fields t]: returns all the fields of [t]. *)
let typedef_get_all_fields (t : trm) : record_fields =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_record rf -> rf
    | _ -> fail t.loc "Ast.typdef_get_all_fields: this function should be called only for structs and classes."
    end
  | _ -> fail t.loc "Ast.get_all_fields: only structs and classes have fields"


(* [get_member_type rf]: returns the type of the member [rf]. *)
let get_member_type (rf : record_field) : typ =
  match rf with
  | Record_field_member (_, ty) -> ty
  | Record_field_method t1 ->
    begin match t1.desc with
    | Trm_let_fun (_, ty, _, _) -> ty
    | _ -> fail None "Ast.get_member_type: can't get the type of the member [rf]."
    end

(* [get_typ_arguments t]: returns the list of types used during a template specialization. *)
let get_typ_arguments (t : trm) : typ list =
  let c_annot = trm_get_cstyles t in
  List.fold_left (fun acc c_ann ->
    match c_ann with
    | Typ_arguments tyl -> tyl
    | _ -> acc
  ) [] c_annot

(* [insert_at_top_of_seq tl t]: insert the list of trms [tl] at the top of sequence [t]. *)
let insert_at_top_of_seq (tl : trm list) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 ->
    let new_tl = Mlist.insert_sublist_at 0 tl tl1 in
    trm_alter ~desc:(Trm_seq new_tl) t
  | _ -> t


(* [filter_out_from_seq f t]: extracts all the trms that satisfy the predicate [f] from sequence [t].
      The final result is a pair consisting of the final sequence and the filtered out trms.*)
let filter_out_from_seq (f : trm -> bool) (t : trm) : (trm * trms)  =
  match t.desc with
  | Trm_seq tl ->
    let tl_to_remove, tl_to_keep = Mlist.partition f tl in
    (trm_alter ~desc:(Trm_seq tl_to_keep) t , Mlist.to_list tl_to_remove)
  | _  -> (t, [])

(* [is_class_constructor t] checks if [t] is a class constructor declaration or definition. *)
let is_class_constructor (t : trm) : bool =
  List.exists (function  | Class_constructor _ -> true | _ -> false) (trm_get_cstyles t)

(* [is_typ_array ty]: checks if [ty] is of type array. *)
let is_typ_array (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_array _ -> true
  | _ -> false

(* [is_trm_record t]: checks if [t] has [Trm_record] or [Trm_array] description or not. *)
let is_trm_record (t : trm) : bool =
  match t.desc with
  | Trm_record _ | Trm_array _ -> true | _ -> false


(* [is_return t]: checks if [t] is a return statement. *)
let is_return (t : trm) : bool =
  match t.desc with
  | Trm_abort (Ret _) -> true | _ -> false

(* [is_trm_abort t]: checks if [t] has [Trm_abort abort] description. *)
let is_trm_abort (t: trm) : bool =
  match t.desc with
  | Trm_abort _ -> true | _ -> false

(* [is_trm_initialization_list] *)
let is_trm_initialization_list (t : trm) : bool =
  match t.desc with
  | Trm_array _ | Trm_record _ -> true
  | _ -> false

let is_trm_unit (t : trm) : bool =
  match trm_lit_inv t with
  | Some Lit_unit -> true
  | _ -> false

(* [has_empty_body t]: checks if the function [t] has an empty body or not. *)
let has_empty_body (t : trm) : bool =
  match t.desc with
  | Trm_let_fun (_, _, _, body) when is_trm_uninitialized body -> true
  | _ -> false

(* ========== matrix helpers =========== *)

(* [mindex dims indices]: builds a call to the macro MINDEX(dims, indices)
    [dims] - dimensions of the matrix access,
    [indices ] - indices of the matrix access.

     Example:
     MINDEXN(N1,N2,N3,i1,i2,i3) = i1 * N2 * N3 + i2 * N3 + i3
     Here, dims = [N1, N2, N3] and indices = [i1, i2, i3]. *)
let mindex (dims : trms) (indices : trms) : trm =
  if List.length dims <> List.length indices then fail None "Matrix_core.mindex: the number of
      dimension should correspond to the number of indices";
  let n = List.length dims in
  let mindex = "MINDEX" ^ (string_of_int n) in
  trm_apps (trm_var mindex) (dims @ indices)

(* [mindex_inv t]: returns the list of dimensions and indices from the call to MINDEX [t]/ *)
let mindex_inv (t : trm) : (trms * trms) option =
  match t.desc with
  | Trm_apps (f, dims_and_indices) ->
    begin match f.desc with
    | Trm_var (_, f_name) when (Tools.pattern_matches "MINDEX" f_name.qvar_var) ->
      let n = List.length dims_and_indices in
      if (n mod 2 = 0) then
        Some (Xlist.split_at (n/2) dims_and_indices)
      else None
    | _ -> None
    end
  | _ -> None
