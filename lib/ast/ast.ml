(** Abstract Syntax Tree *)
(**

{1 Variables}

The type [var] is used to represent program variables for local variables, function names, function arguments and loop indices.
A [var] carries a name as a string, a unique identifier and optionally a namespace path.
A program variable has a binding point (let, let fun, for), and a number of occurences.

{2 When the AST is in a stable state}

{b Identification Invariant}: In a stable AST, every variable carries a non-null identifier ([<> 0]).

{b Uniqueness Invariant}: In a given AST, two different binding points must introduce variables using different ids, regardless of their names.
To ensure uniqueness, transformations must call {!Trm.trm_copy} when duplicating subterms.

{b Scoping Invariant}: If an occurence of a variable [x] refers to a binding point of 'x' according to the scoping rules of the programming language, then the two variables must have the same identifier.

Global toplevel variables are assigned an id based on their fully qualified name. These ids are negative. Call {!Trm.toplevel_var name} to get a global toplevel variable with the given name.

TODO: Check that this is still correct:
We provide an easy way to preregister global toplevel variables using {!Trm.toplevel_var}. The variable returned by this call will be considered predeclared and the id will be shared by the toplevel binder of the same name.
Calls to Trm.toplevel_var must be done before the scope resolution, otherwise the id will not correspond to the one already set in the AST.

{2 Transition states}

After parsing, identifiers are set to the dummy value [0].
The function {!Scope_computation.infer_var_ids} produces an ast with correct identifiers according to the scoping rules.
This function is called during the encoding/decoding phases.

If a transformation introduces dummy identifiers, it should call [Scope.infer_var_ids] to compute missing ids at the end of the transformation to restore the invariants.

{2 Checking invariants}

{!Scope.check_unique_var_ids} checks the Uniqueness Invariant. It fails if the invariant is broken.
It has an option to also check the Identification Invariant.

{2 Display mechanism}

In order to pretty print ASTs with identifiers in a deterministic and human-readable fashion, we provide a renaming function {!Scope.unique_alpha_rename}.
This function renames program variables to give each binder a unique name.
For example 'x' with id #18 could be printed as 'x__0' and 'x' with id #29 as 'x__1'.

{1 AST design choices}

Some AST nodes such as Trm_for must respect the invariant that their body is always a Trm_seq.
This might seem inefficient and insufficiently typed but it allows to target the body sequence like any other sequence,
and still distinguish the body from the whole loop (or any other construction that respects the Trm_seq invariant) after
target resolution.
*)

(* raise exception with format string *)
let failwith msg = Printf.ksprintf failwith msg
let sprintf = Printf.sprintf

(*****************************************************************************)

(* TODO: move to loc.ml *)

(** [pos]: record used to represent a specific location inside the code *)
type pos = {
    pos_line : int;
    pos_col : int; }

(** [trm_loc]: record used to keep track of the trm information like the
file it belongs, and the start and the end positions inside the code. *)
type trm_loc = {
  loc_file : string;
  loc_start : pos;
  loc_end : pos;}

(** [location]: an optional type representing the location of a trm *)
type location = trm_loc option

(** [loc_to_string loc]: pretty print a trm location *)
let loc_to_string (loc : location) : string =
  match loc with
  | None -> "Unknown location"
  | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column};
          loc_end = {pos_line = end_row; pos_col = end_column}} ->
    if start_row <> end_row then
      Printf.sprintf "File %s, lines %d-%d, columns %d-%d" filename start_row end_row start_column end_column
    else if start_column <> end_column then
      Printf.sprintf "File %s, line %d, columns %d-%d" filename start_row start_column end_column
    else
      Printf.sprintf "File %s, line %d, column %d" filename start_row start_column

(*****************************************************************************)

(** [loc]: memory location *)
type loc = int (* TODO: remove this? and rename location to loc for conciseness *)

(** [mark]: annotation used for resolving targets, see module mark.ml *)
type mark = string

(** [marks]: a list of marks *)
type marks = mark list

let no_mark = ""

(** [mlists]: generalized lists, see module mlist.ml *)
type 'a mlist = 'a Mlist.t

type var_id = int
let unset_var_id = 0

(** [var]: variables are uniquely identified with [id], but are printed using a qualified name. *)
type var = { namespaces: string list; name: string; id: var_id }

let has_unset_id var = var.id = unset_var_id
let is_toplevel_var var = var.id < 0

let qualified_name_to_string namespaces name =
  String.concat "" (List.map (fun q -> q ^ "::") namespaces) ^ name

let var_to_string (v : var) : string =
  let qualified_name = qualified_name_to_string v.namespaces v.name in
  if is_toplevel_var v then
    qualified_name
  else if has_unset_id v then
    qualified_name ^ "?"
  else
    qualified_name ^ "#" ^ string_of_int v.id

let assert_var_id_set ~error_loc v =
  if has_unset_id v then failwith "%s: Variable %s has an id that is not set (maybe forgot to call Scope.infer_var_ids)" error_loc (var_to_string v)

let var_eq (v1 : var) (v2 : var) : bool =
  assert_var_id_set ~error_loc:"var_eq" v1;
  assert_var_id_set ~error_loc:"var_eq" v2;
  v1.id = v2.id

module Var = struct
  type t = var
  let compare v1 v2 =
    assert_var_id_set ~error_loc:"Var.compare" v1;
    assert_var_id_set ~error_loc:"Var.compare" v2;
    Int.compare v1.id v2.id
  let equal v1 v2 = var_eq v1 v2
  let hash v = Hashtbl.hash v.id
end

(** [vars]: variables, a list of elements of type variable *)
type vars = var list

(** [Var_set]: a set module used for storing variables *)
module Var_set = Set.Make(Var)

(** [Var_map]: a map module used for mapping variables to values *)
module Var_map = Map.Make(Var)

(** [Var_Hashtbl]: a hash table module used for variables *)
module Var_Hashtbl = Hashtbl.Make(Var)

(** [varmap]: instantiation of Var_map *)
type 'a varmap = 'a Var_map.t

let var_map_of_list l = Var_map.of_seq (List.to_seq l)

(* let vars_to_string vs = Tools.list_to_string vs *)
let vars_to_string vs = Tools.(list_to_ocaml_code var_to_string vs)

(** [next_var_id]: generates an integer for variable names *)
let next_var_id : unit -> int = Tools.fresh_generator ()

(** [fresh_var_name]: creates a fresh variable name *)
let fresh_var_name : ?prefix:string -> unit -> string =
  let next_number = Tools.fresh_generator () in
  fun ?(prefix = "_v") () -> prefix ^ string_of_int (next_number ())

module Qualified_name = struct
  type t = string list * string
  [@@deriving ord, eq]
end

module Qualified_set = Set.Make(Qualified_name)
module Qualified_map = Map.Make(Qualified_name)

(** [stringreprid]: unique identifier used as keys for memoization of string representation of subterms *)
type stringreprid = int

(** [next_stringreprid ()] generates and return a new string representation id *)
let next_stringreprid : (unit -> stringreprid) =
  Tools.fresh_generator ()

(** [field]: struct field defined as a string *)
type field = string

(** [fields]: struct fields as a list of fields *)
type fields = field list

(** [label]: labels (for records) *)
type label = string
type 'a labelmap = 'a Tools.String_map.t

(** [labels]: a list of labels. *)
type labels = label list

let no_label = ""

(** [loop_dir]: loop bound inequalities *)
type loop_dir =
  | DirUp      (* i < 2  *)
  | DirUpEq    (* i <= 2 *)
  | DirDown    (* i > 0  *)
  | DirDownEq  (* i >= 0 *)
[@@deriving show]

(** [code_kind]; code kind entered by the user *)
type code_kind =
  | Lit of string   (* 1, "hello", 1.0, (), true, false *)
  | Typ of string  (* int, double float, vect, particle *)
  | Expr of string  (* expression of the form a * (b + 1) *)
  | Stmt of string  (* functions, for loops, while loops etc *)
  | Instr of string (* a = b, a += b *)
  | Comment of string (* "// txt", or "/*txt*/" *)
[@@deriving show]

(** [code_to_str]: extracts the code from the trms that contain the arbitrary code. *)
let code_to_str (code : code_kind) : string =
  match code with
  | Lit l -> l
  | Typ ty -> ty
  | Expr e -> e
  | Stmt s -> s
  | Instr s -> s
  | Comment s -> s

(*****************************************************************************)

(** [trm] is a record representing an ast node *)
type trm =
 { annot : trm_annot; (* Annotations cannot be recomputed. Check if it should not be inlined here. *)
   desc : trm_desc;
   loc : location; (* TODO: move into annot ? *)
   typ : typ option; (* TODO: #typfield: Remove this badly designed field and split between authoritative types for some trm_desc and computed types in ctx *)
   mutable ctx : ctx; (* Context contains information that can be recomputed over the AST. Transformations can safely discard it. *)
   mutable errors : string list; (* Maybe split into two and merge with annot and ctx ? *)
   (* LATER: mutable typing_aux should be the only mutable flag *)
}

(** [trms]: a list of trms *)
and trms = trm list

(** [trm_desc]: description of an ast node *)
and trm_desc =
  | Trm_var of var
  | Trm_lit of lit   (* literal values *)
  | Trm_prim of prim (* primitive values *) (* TODO: Remove and replace with overloaded builtin functions *)
  | Trm_array of trm mlist (* { 0, 3, 5 } as an array *)
  | Trm_record of (label option * trm) mlist (* { 4, 5.3 } as a record *)
  | Trm_let of typed_var * trm (* int x = 3 *)
  | Trm_let_mult of (typed_var * trm) list (* TODO: replace with non-scoping seq *)
  | Trm_let_fun of var * typ * typed_vars * trm * fun_spec (* TODO: Remove and replace with Trm_let (Trm_fun _) *)
  | Trm_fun of typed_vars * typ option * trm * fun_spec (* anonymous functions, [&](int const& x) -> void {r += x;} *) (* TODO: Remove option on return type *)
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm  (* if (x > 0) {x += 1} else{x -= 1} *)
  | Trm_seq of trm mlist       (* { st1; st2; st3 } *)
  | Trm_apps of trm * trm list * resource_item list (* f(t1, t2) / __with_ghosts(f(t1, t2), "g1 := e1, g2 := e2")*)
  | Trm_for of loop_range * trm * loop_contract
  | Trm_for_c of trm * trm * trm * trm * resource_set option (* TODO: Remove from here and desugar into while *)
  | Trm_while of trm * trm     (* while (t1) { t2 } *)
  | Trm_do_while of trm * trm (* TODO: Remove from here and desugar into while *)
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
  (* TODO: new root for multi-files
    | Trm_files of trms *)
  | Trm_omp_routine of omp_routine                (* get_thread_id *)
  | Trm_extern of string * trms                   (* extern keyword *)
  | Trm_namespace of string * trm * bool          (* namespaces *)
  | Trm_template of template_parameter_list * trm (* templates *) (* TODO: Replace by annotated arguments on definitions and calls? *)
  | Trm_using_directive of string                 (* using namespace std *)

(*****************************************************************************)

(** [typ] is an alias for [trm] used wherever a type is expected *)
and typ = trm

(** [typvar] is an alias for [var] used wherever a type varaible (i.e. type constructor or base type) is expected *)
and typvar = var

(** [typed_var]: used for function arguments *)
and typed_var = var * typ

(** [typed_vars]: a list of typed_var *)
and typed_vars = typed_var list

(*****************************************************************************)
(** [typedef]: is a record containing the id of the type, the name of the new defined
    type, for sum types there can be also more then one variable. And finally the
     body of the type *)
and typedef = {
  typedef_name : var; (* the defined type [t] *)
  typedef_body : typedef_body;(* the body of the definition,
                            i.e. the description of [...] *)
}

(** [record_fields]: fields representation for classes, structs and unions. *)
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


(** [typedef_body]: typedef kinds *)
and typedef_body =
  | Typedef_alias of typ   (* for abbreviations, e.g. [type 'a t = ('a * 'a)
                          list] or [typedef vect t] *)
  | Typedef_record of record_fields
    (* for records / struct, e.g. [type 'a t = { f : 'a; g : int } *)
  | Typedef_enum of (var * (trm option)) list (* for C/C++ enums *)


(*****************************************************************************)

(** [cstyle_annot]: annotations are used to decorate this AST when it is built from
    the Clang AST in such a way to be able to print back the AST like
    the original C code.*)
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

  | Prefix_step (* on a loop step, writes ++i / --i instead of i += 1 / i -= 1 *)
  | Postfix_step (* on a loop step, writes i++ / i-- instead of i += 1 / i -= 1 *)

  (* [int& x = 3]  encoded as  [let x : ( int* ) = ref 3] in the internal AST *)
  | Reference

  (* On typedefs, distinguish between class vs struct.
     On types, determines if the struct keyword was used *)
  | Struct

  (*  [typedef struct node { int item; struct node* p } node; ]
      this flag [Rec_struct] indicates whether to reprint the type at the front. *)
  | Rec_struct

  (* On typedefs, distinguish between class vs struct.
     On types, determines if the class keyword was used *)
  | Class

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
  (* meta-information identifying a C++ method *)
  | Method

  (*  [class foo {  int x;  foo() : x(3) { bla} }] is encoded as
     [class foo {  int x; foo() { x = 3; bla  }], where [x=3] is tagged as Constructed_init
     LATER: verify.  *)
  | Constructed_init (* objects initialized with a constructor. *)

  (* LATER: document *)
  | Class_constructor of constructor_kind
  | Class_destructor of destructor_kind
  | Member_initializer

  (* used for int[2] = { 3, 4 }, the trm_array is annotated with [Brace_init] *)
  | Brace_init

  (* tag for printing [NULL] instead of [nullptr] *)
  | Display_null_uppercase

  (* tag for printing using resource or type syntax
     LATER: Use different printers for different languages *)
  | ResourceFormula
  | Type

  | InjectedClassName

  (* tag used by light diff *)
  | BodyHiddenForLightDiff

(** [constructor_kind]: special annotation for constructors *)
and constructor_kind =
  | Constructor_implicit
  | Constructor_explicit
  | Constructor_default
  | Constructor_simpl

and destructor_kind =
  | Destructor_default
  | Destructor_delete
  | Destructor_simpl

(** [file_annot]: file annotation *)
and file_annot =
  | Inside_file
  | Main_file
  | Included_file of string

(** [cpragma]: type alias for directives *)
and cpragma = directive

(** [attribute]: trm attributes *)
and attribute =
  | Alignas of trm (* Placed on types like in: alignas(64) double* deposit; *)
  | GhostCall (* Used for ghost annotations (__ghost syntax in C) *)

(** [trm_annot]: a record containing all kinds of annotations used on the AST of OptiTrust. *)
and trm_annot = {
    trm_annot_attributes : attribute list;
    trm_annot_marks : marks;
    trm_annot_labels : labels; (* TODO: Replace with noop instructions in sequences *)
    trm_annot_stringrepr : stringreprid option; (* TODO: move to context *)
    trm_annot_pragma : cpragma list; (* TODO: Maybe merge with attribute ? What is the difference anyway ? *)
    trm_annot_cstyle : cstyle_annot list;
    trm_annot_file : file_annot;
    trm_annot_referent : trm option; (* used for typing errors *)
  }
  (* LATER: use a smartconstructor for trm_annot with optional arguments *)

(*****************************************************************************)

(** [unary_op]: unary operators *)
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
(** [binary_op]: binary operators *)
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
  | Binop_and           (* a && b *) (* FIXME: binop must be call by value *)
  | Binop_bitwise_and   (* a & b *)
  | Binop_or            (* a || b *) (* FIXME: binop must be call by value *)
  | Binop_bitwise_or    (* a | b *)
  | Binop_shiftl        (* a >> k*)
  | Binop_shiftr        (* a << k *)
  | Binop_xor           (* a ^ b *)
  (* LATER: add types to operations wherever relevant *)
  (* TODO: not coherent to use a grammar of binary_op for certain ops, and use conventional functions for others, eg. fmod *)
  (* | Binop_fmod          (* floatting point modulo, LATER: merge with mod when annotated with type *) *)


(** [consistency_mode]: C++ memory model consistency *)
and consistency_mode =
  | Sequentially_consistent
  | Release
  | Acquire

(** [prim]: primitives  *)
and prim =
  | Prim_unop of unary_op (* e.g. "!b" *)
  | Prim_binop of binary_op (* e.g. "n + m" *)
  | Prim_compound_assgn_op of binary_op (* e.g. "a += b" *)
  | Prim_overloaded_op of prim (* used for overloaded operators *)
  | Prim_ref of typ (* "ref T", used to wrap mutable variables *)
  | Prim_ref_array of typ * trm list (* "ref[m,n] T", used to wrap mutable arrays *)
  | Prim_new of typ (* C++ "new T" *)
  | Prim_delete (* C++ "delete t" *)
  | Prim_delete_array (* C++ "delete[] t" *)
  | Prim_conditional_op (* "(foo) ? x : y" *)

(** [lit]: literals *)
and lit =
  | Lit_unit                 (* void, e.g. "return;" is represented as "Lit_unit" *)
  | Lit_uninitialized of typ (* e.g. "int x;" is "int x = Lit_uninitalized" *)
  | Lit_bool of bool         (* true, false *)
  | Lit_int of typ * int     (* 1, 10, 100 *)
  | Lit_float of typ * float (* 1.0, 2.0, 0.5 *)
  | Lit_string of string     (* "hello" *)
  | Lit_nullptr of typ       (* nullptr *)


(** [loop_range]: a type for representing for loops range *)
and loop_range = {
  index: var;
  start: trm;
  direction: loop_dir;
  stop: trm;
  step: trm;
}

(*****************************************************************************)

(** [ctx]: stores context information that can be recomputed and must be updated
   when changes occur (reset the field to unknown_ctx for invalidation). *)
and ctx = {
  (* The set of accessible resources before this term. *)
  mutable ctx_resources_before: resource_set option;
  (* The map of used variables inside the trm (recursively) *)
  mutable ctx_resources_usage: resource_usage_map option;
  (* The resources framed, used and produced during a contract invocation *)
  mutable ctx_resources_contract_invoc: contract_invoc option;
  (* The set of accessible resources after this term. *)
  mutable ctx_resources_after: resource_set option;
  (* The instantiation of the requested post condition *)
  mutable ctx_resources_post_inst: used_resource_set option;
}

and formula = trm
and resource_item = var * formula

and resource_set = {
  pure: resource_item list;
  linear: resource_item list;
  fun_specs: fun_spec_resource varmap; (** Pure facts that give specification to functions are stored here instead of pure to allow easier lookup. *)
  aliases: trm varmap; (** Map of variables to their definition, variables may come from the program or pure facts *)
}

(* Represents the knowledge of the specification of a function *)
and fun_spec_resource = {
  args: var list; (** List of program arguments to the function *)
  contract: fun_contract;
  inverse: var option;
}

and fun_contract = {
  pre: resource_set;
  post: resource_set;
}

and fun_spec =
  | FunSpecUnknown
  | FunSpecContract of fun_contract
  | FunSpecReverts of var
  (** [FunSpecReverts f] is the reverse of the spec of [f]. *)

(* forall ghosts,
    { invariant(0) * RO(parallel_reads) * for i -> iter_contract.pre(i) }
      loop
    { invariant(n) * RO(parallel_reads) * for i -> iter_contract.post(i) } *)
(* forall ghosts,
    { invariant(i) * RO(parallel_reads) * iter_contract.pre(i) }
      loop body
    { invariant(i) * RO(parallel_reads) * iter_contract.post(i) } *)
and loop_contract = {
  loop_ghosts: resource_item list;
  invariant: resource_set;
  parallel_reads: resource_item list; (* all the resources should be of the form RO(_, _) *)
  iter_contract: fun_contract;
  strict: bool; (* Non strict loop contracts take all the resources in the frame after instantiation as additional invariants *)
}

and used_resource_item = {
  hyp: var;
  inst_by: formula;
  used_formula: formula;
}
and used_resource_set = {
  used_pure: used_resource_item list;
  used_linear: used_resource_item list
}

and produced_resource_item = {
  post_hyp: var;
  produced_hyp: var;
  produced_formula: formula;
}
and produced_resource_set = {
  produced_pure: produced_resource_item list;
  produced_linear: produced_resource_item list;
}

and resource_usage =
  | Required
  | Ensured
  | ArbitrarilyChosen
  | ConsumedFull
  | ConsumedUninit
  | SplittedFrac
  | JoinedFrac
  | Produced

and resource_usage_map = resource_usage Var_map.t

and contract_invoc = {
  contract_frame: resource_item list;
  contract_inst: used_resource_set;
  contract_produced: produced_resource_set;
  contract_joined_resources: (var * var) list;
}

(** [template_param_kind]: parameters kind: typename or variable and an optional default value *)
and template_param_kind =
  | Typename of typ option               (* <typename T> *)
  | NonType of typ * trm option           (* <int a> *)

(** [template_parameter_list]: template parameter list *)
and template_parameter_list = (var * template_param_kind) list

(** [abort]: ways of aborting *)
and abort =
  | Ret of trm option        (* return;  or return 3; *)
  | Break of label option    (* break; *)
  | Continue of label option (* continue; *)


(*****************************************************************************)

(** [mode]: mode used for default OpenMP clause *)
and mode =
  | Shared_m
  | None_

(** [expression]: representing the code inside an If OpenMP clause *)
and expression = string

(** [sched_type]: scheduling type for OpenMP *)
and sched_type =
  | Static
  | Dynamic
  | Guided
  | Runtime

(** [reduction_identifier]: reduction operation for OpenMP reduction clause *)
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

(** [map_type] map type for map OpenMP clause *)
and map_type =
  | Alloc
  | To
  | From
  | ToFrom
  | No_map

(** [proc_bind]: process binding *)
and proc_bind =
  | Master_pb
  | Close
  | Spread

(** [dep]:  *)
and dep =
  | Dep_var of var
  | Dep_ptr of dep

(** [deps]: *)
and deps = dep list

(** [dependecy_type]: dependency kind *)
and dependence_type =
  | In of deps
  | Out of deps
  | Inout of deps
  | Outin of deps
  | Sink of deps
  | Source

(** [clause]: OpenMP clauses *)
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

(** [atomic_operation]: atomic operations for atomic OpenMP directives *)
and atomic_operation =
  | Read
  | Write
  | Update
  | Capture

(** [directive]: OpenMP directives *)
and directive =
  | Atomic of atomic_operation option
  | Atomic_capture
  | Barrier
  | Cancel of clause * clause list
  | Cancellation_point of clause * clause list
  | Critical of var * string
  | Declare_simd of clause list
  | Declare_reduction of reduction_identifier * string list * expression * clause
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

(** [omp_routine]: OpenMP Routines *)
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

(*************************** Variable constructors ***************************)

(** Creates a new variable, using a fresh identifier. *)
let new_var ?(namespaces: string list = []) (name : string) : var =
  let id = next_var_id () in
  { namespaces; name; id }

(** Refers to a variable by name, letting its identifier be inferred.
    This variable cannot be stored in a [varmap] before its identifier is inferred. *)
let name_to_var ?(namespaces: string list = []) (name : string) : var =
  { namespaces; name; id = unset_var_id }

module Toplevel_id = struct
  type t = var_id
  let compare = Int.compare
  let equal = Int.equal
  let hash id = id
  let from_qualified_name ~namespaces name =
    let qualified_name = qualified_name_to_string namespaces name in
    let hash = Hashtbl.hash qualified_name in
    - hash - 1
end

module Toplevel_hashtbl = Hashtbl.Make(Toplevel_id)

(** Set of toplevel variables already attributed.
    This is used by toplevel_var to check collisions and perform hash consing. *)
let toplevel_vars = Toplevel_hashtbl.create 32

(** [toplevel_var]: return the toplevel variable with the given name.
  A new variable identifier is predeclared if the variable did not exist. *)
let toplevel_var ?(namespaces: string list = []) (name : string) : var =
  let id = Toplevel_id.from_qualified_name ~namespaces name in
  match Toplevel_hashtbl.find_opt toplevel_vars id with
  | Some var when var.namespaces = namespaces && var.name = name -> var
  | Some _ -> failwith "Hash conflict for toplevel variables. This should not happen unless you create a stupid amount of toplevel variables. If you are extremely unlucky, maybe try to add a seed to the Toplevel_id.from_qualified_name function."
  | None ->
      let var = { namespaces; name; id } in
      Toplevel_hashtbl.add toplevel_vars id var;
      var

(** A dummy variable for special cases. *)
let dummy_var = toplevel_var ""

(*****************************************************************************)

let trm_desc_to_string : trm_desc -> string =
  function
  | Trm_var _ -> "Trm_var"
  | Trm_lit _ -> "Trm_lit"
  | Trm_prim _ -> "Trm_prim"
  | Trm_array _ -> "Trm_array"
  | Trm_record _ -> "Trm_record"
  | Trm_let _ -> "Trm_let"
  | Trm_let_mult _ -> "Trm_let_mult"
  | Trm_let_fun _ -> "Trm_let_fun"
  | Trm_typedef _ -> "Trm_typedef"
  | Trm_if _ -> "Trm_if"
  | Trm_seq _ -> "Trm_seq"
  | Trm_apps _ -> "Trm_apps"
  | Trm_while _ -> "Trm_while"
  | Trm_for _ -> "Trm_for"
  | Trm_for_c _ -> "Trm_for_c"
  | Trm_do_while _ -> "Trm_do_while"
  | Trm_switch _ -> "Trm_switch"
  | Trm_abort _ -> "Trm_abort"
  | Trm_goto _ -> "Trm_goto"
  | Trm_arbitrary _ -> "Trm_arbitrary"
  | Trm_omp_routine _ -> "Trm_omp_routine"
  | Trm_extern _ -> "Trm_extern"
  | Trm_namespace _ -> "Trm_namespace"
  | Trm_template _ -> "Trm_template"
  | Trm_using_directive _ -> "Trm_using_directive"
  | Trm_fun _ -> "Trm_fun"

let resource_usage_opt_to_string = function
| None -> "None"
| Some Required -> "Required"
| Some Ensured -> "Ensured"
| Some ArbitrarilyChosen -> "ArbitrarilyChosen"
| Some SplittedFrac -> "SplittedFrac"
| Some ConsumedUninit -> "ConsumedUninit"
| Some ConsumedFull -> "ConsumedFull"
| Some JoinedFrac -> "JoinedFrac"
| Some Produced -> "Produced"

type tmap = trm Var_map.t

(* ************************* Resource constructors ************************* *)

let unknown_ctx (): ctx = {
  ctx_resources_before = None; ctx_resources_after = None;
  ctx_resources_usage = None; ctx_resources_contract_invoc = None;
  ctx_resources_post_inst = None;
}

(** The empty resource set. *)
let empty_resource_set = { pure = []; linear = []; fun_specs = Var_map.empty; aliases = Var_map.empty }

(** The empty function contract, printed as __pure(). *)
let empty_fun_contract =
  { pre = empty_resource_set; post = empty_resource_set }

(** The empty loop contract, like on a loop without any annotation *)
let empty_loop_contract =
  { loop_ghosts = []; invariant = empty_resource_set; parallel_reads = []; iter_contract = empty_fun_contract; strict = false }

(** The empty strict loop contract, that contains nothing *)
let empty_strict_loop_contract =
  { empty_loop_contract with strict = true }

(* **************************** Trm constructors *************************** *)

(** [trm_annot_default]: default trm annotation *)
let trm_annot_default = {
  trm_annot_attributes = [];
  trm_annot_marks = [];
  trm_annot_labels = [];
  trm_annot_stringrepr = None;
  trm_annot_pragma = [];
  trm_annot_cstyle = [];
  trm_annot_file = Inside_file;
  trm_annot_referent = None;
}

(** [incr_trm_alloc]: function called once per allocated node. *)
let incr_trm_alloc : (unit -> unit) ref = ref (fun () -> ())

(** [trm_make ~annot ?loc ?typ ?ctx ?errors desc]: builds trm [t] with description [desc] and other fields given
    as arguments. *)
let trm_make ?(annot : trm_annot = trm_annot_default) ?(loc : location)
    ?(typ : typ option) ?(ctx : ctx = unknown_ctx ()) ?(errors : string list = []) (desc : trm_desc) : trm =
  let t = {annot; loc; typ; desc; ctx; errors} in
  !incr_trm_alloc ();
  t

(** [trm_alter ~annot ?loc ?typ ?ctx ?desc t]: alters any of the fields of [t] that was provided as argument. *)
let trm_alter ?(annot : trm_annot option) ?(loc : location option) ?(typ : typ option) ?(ctx : ctx option) ?(errors : string list option) ?(desc : trm_desc option) (t : trm) : trm =
  let annot = match annot with Some x -> x | None -> t.annot in
  let loc = match loc with Some x -> x | None -> t.loc in
  let typ = match typ with | None -> t.typ | _ -> typ in
  let ctx = Option.value ~default:t.ctx ctx in
  let errors = Option.value ~default:t.errors errors in
  let desc = match desc with | Some x -> x | None -> t.desc in
  trm_make ~annot ?loc ?typ ~ctx ~errors desc

(** [trm_replace desc t]: an alias of [trm_alter] to alter only the descriptiong of [t]. *)
let trm_replace (desc : trm_desc) (t : trm) : trm =
  trm_alter ~desc t

(** [trm_like]: copies the annotations, location and type of the old trm into a new trm *)
let trm_like ~(old:trm) (t:trm): trm =
  trm_alter ~annot:old.annot ~loc:old.loc ~errors:old.errors ?typ:old.typ t

(* More specific constructors are in modules [Typ] and [Trm] *)

(* ********************************************************************************************** *)

(* TODO: move *)
(* TODO: rename to monoid *)
(** [local_ops]: type used for the local_name transformation. *)
type local_ops =
(* | Functional_monoid of trm * trm (* 0 and +; what about += ? *)
   | Imperative_monoid of trm * trm (* create 0 and mutating += (e.g. bag extend) and others (e.g. bag push) *)
   Maybe should only take += even for functional
      *)
  | Local_arith of lit * binary_op
  | Local_obj of var * var * var
