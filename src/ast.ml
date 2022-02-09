(* a record used to represent a specific location inside the code*)
type pos = {
    pos_line : int;
    pos_col : int; }

(* a record used to keep track of the node information like the file it belongs
    and the start and the end position inside the code.
*)
type node_loc = {
  loc_file : string;
  loc_start : pos;
  loc_end : pos;}



(* location of a node is given as an option term, that because sometimes for some nodes we
    cant' have the location. Of when generating new nodes it is hard for the transformations
    to now the exact location where the term is being added, hence it is left as empty
*)
type location = node_loc option

(* memory locations *)
type loc = int

(* marks *)
type mark = Mark.t

type 'a mlist = 'a Mlist.t

(* string representation of a term, as provided by the user *)
type strm = string

(* string representation of a type, as provided by the user *)
type styp = string

(* variables *)
type var = string

type vars = var list

(* name of type constructors (e.g. [list] in Ocaml's type [int list];
   or [vect] in C type [struct { int x,y }; *)
type typconstr = string

(* name of type variables (e.g. ['a] in type ['a list] *)
type typvar = var
type typvars = typvar list

(* unique identifier for typ constructors*)
type typconstrid = int

(* [next_typconstrid ()] generate and return a new integer *)
let next_typconstrid : (unit -> typconstrid) =
  Tools.fresh_generator ()

(* [init_typconstrid ()] reset the id generator for type constructors *)
let init_typconstrid (): unit =
  Tools.reset_generator ()

(* unique identifier used as keys for memoization of string representation of subterms *)
type stringreprid = int

(* [next_stringreprid ()] generate and return a new string representation id *)
let next_stringreprid : (unit -> stringreprid) =
  Tools.fresh_generator ()


(* ['a typmap] is a map from [typeid] to ['a] *)
module Typ_map = Map.Make(Int)
type 'a typmap = 'a Typ_map.t


(* struct fields and maps describing struct *)
type field = string

(* struct fields as a list of fields *)
type fields = field list

(* ['a varmap] is a map from string to ['a] *)
module String_map = Map.Make(String)
type 'a varmap = 'a String_map.t

(* labels (for records) *)
type label = string

(* Description of a term as a string (convenient for the user)
   LATER: this type might become generalized in the future. *)
type string_trm = string


(* constructor name (for enum and algebraic datatypes) *)
type constrname = string

(* array sizes *)
type size =
  | Undefined (* t[] *)
  | Const of int (* t[3] *)
  | Trm of trm (* t[2*nb] *)


(* Type used for the step of the loop *)
and loop_step =
  | Pre_inc
  | Post_inc
  | Pre_dec
  | Post_dec
  | Step of trm

(* Type used for the bound of the loop *)
and loop_dir =
  | DirUp
  | DirUpEq
  | DirDown
  | DirDownEq

and code_kind =
  | Lit of string
  | Atyp of string
  | Expr of string
  | Atypexpr of string
  | Stmt of string



(* types of expressions *)
and typ_desc =
  | Typ_const of typ (* e.g. [const int *] is a pointer on a [const int] type. *)
  | Typ_var of typvar * typconstrid (* e.g. ['a] in the type ['a -> 'a] -- *)
  | Typ_constr of typvar * typconstrid * typ list (* e.g. [int list] or [(int,string) map] or [vect] *)
  | Typ_auto
  | Typ_unit (* void *)
  | Typ_int
  | Typ_float
  | Typ_double
  | Typ_bool
  | Typ_char
  | Typ_string
  | Typ_ptr of  {ptr_kind : ptr_kind; inner_typ: typ } (* "int*" *)
  | Typ_array of typ * size (* int[3], or int[], or int[2*n] *)
  | Typ_fun of (typ list) * typ  (* int f(int x, int y) *)
  | Typ_record of record_type * typ
  | Typ_template_param of string
  | Typ_arbitrary of code_kind

(* references are considered as pointers that's why we need to distinguish the kind of the pointer *)
and ptr_kind =
  | Ptr_kind_mut
  | Ptr_kind_ref

(* annotation for types that can be build from the main ones *)
and typ_annot =
  | Unsigned
  | Long
  | Short

(* LATER
and typ_flags = {
    typ_flags_generated_star : bool;
}
*)
(* [typ] is a record containing the description, annotation and some attributes*)
and typ = {
  typ_desc : typ_desc;
  typ_annot : typ_annot list;
  typ_attributes : attribute list;
 (*  typ_flags : typ_flags  *) }
  (* IN THE FUTURE
  ty_env : env; --> tells you for every type what is its definition
  *)

(* [typedef] is a record containing the id of the type, the name of the new defined type
    for sum types there can be also more then one variable. And finally the body of the type
*)
and typedef = { (* e.g. [type ('a,'b) t = ...] *)
  typdef_loc : location;
  typdef_typid : typconstrid; (* the unique id associated with the type [t] *)
  typdef_tconstr : typconstr; (* the name [t] *)
  typdef_vars : typvars; (* the list containing the names ['a] and ['b];
    [typedef_vars] is always the empty list in C code without templates *)
  typdef_body : typdef_body;
   } (* the body of the definition, i.e. the description of [...] *)

(* constructed types *)
and typdef_body =
  | Typdef_alias of typ (* for abbreviations, e.g. [type 'a t = ('a * 'a) list] or [typdef vect t] *)
  | Typdef_prod of bool * (label * typ) list (* for records / struct, e.g. [type 'a t = { f : 'a; g : int } *)
  | Typdef_sum of (constrname * typ) list (* for algebraic definitions / enum, e.g. [type 'a t = A | B of 'a] *)
  (* Not sure if Typedef_enum is a sum type *)
  | Typdef_enum of (var * (trm option)) list (* for C/C++ enums *)

  (* NOTE: we don't need to support the enum from C, for the moment. *)
  (* DEPRECATED
  | Typedef_abbrev of typvar * typ  (* type x = t, where t could be a struct *)
  *)

(* used for function arguments *)
and typed_var = var * typ

and typed_vars = typed_var list

(* unary operators *)
and unary_op =
  | Unop_get (* the "*" operator as in *p  *)
  | Unop_address (* the "&" operator as in &p *)
  | Unop_bitwise_neg
  | Unop_neg
  | Unop_minus
  | Unop_plus
  | Unop_post_inc
  | Unop_post_dec
  | Unop_pre_inc
  | Unop_pre_dec
  | Unop_struct_access of field
  | Unop_struct_get of field
  | Unop_cast of typ (* cast operator towards the specified type *)

(* binary operators *)
and binary_op =
  | Binop_set (* type annotation?    lvalue = rvalue *)
  | Binop_array_access
  | Binop_array_get
  | Binop_eq
  | Binop_neq
  | Binop_sub
  | Binop_add (* LATER: numeric operation takes a type argument *)
  | Binop_mul
  | Binop_mod
  | Binop_div
  | Binop_le
  | Binop_lt
  | Binop_ge
  | Binop_gt
  | Binop_and
  | Binop_bitwise_and
  | Binop_or
  | Binop_bitwise_or
  | Binop_shiftl
  | Binop_shiftr
  | Binop_xor

(* consistency mode for C++ memory model *)
and consistency_mode =
  | Sequentially_consistent
  | Release
  | Acquire

(* primitives  *)
and prim =
  | Prim_unop of unary_op (* e.g. "!b" *)
  | Prim_binop of binary_op (* e.g. "n + m" *)
  | Prim_compound_assgn_op of binary_op (* e.g. "a += b" *)
  | Prim_overloaded_op of prim (* used for overloaded operators *)
  | Prim_new of typ (* "new T" *)
  | Prim_conditional_op (* "(foo) ? x : y" *)

(* literals *)
and lit =
  | Lit_unit (* void, e.g. "return;" is represented as "Lit_unit" *)
  | Lit_uninitialized (* e.g. "int x;" is "int x = Lit_uninitalized" *)
  | Lit_bool of bool
  | Lit_int of int (* LATER: add the string "as it was in the source" *)
  | Lit_double of float (* LATER: add the string "as it was in the source" *)
  | Lit_string of string

(* values *)
and value =
  | Val_lit of lit
  | Val_prim of prim
  | Val_ptr of loc
  (* These are values that can only be constructed during the program execution,
     and thus useful only for carrying out proofs about the program Generic *)
  (* LATER: add functions, which are also values that can be created at execution time *)

(* annotations are used to decorate this AST when it is built from the Clang AST
   in such a way to be able to print back the AST like the original C code.
*)

and trm_annot =
  | No_braces of int (* some sequences can be visible only internally *)
  | Access (* annotate applications of star operator that should not be printed *)
  | Multi_decl (* used to print back sequences that contain multiple declarations *)
  | Empty_cond (* used for loops with empty condition *)
  | App_and_set (* annotate uses of binop_set that unfold +=, -=, *= *)
  | Include of string (* to avoid printing content of included files *)
  | Main_file (* sequence annotated as the main file is not printed *)
  | Mutable_var_get (* Used for get(x) operations where x was a non-const stack allocated variable *)
  | As_left_value (* Used for reference encoding *) (* LATER: might become deprecated *)
  | Non_local_index (* Used for loops whose index is not declared inside the scope of the loop body *)
  | Display_arrow (* Used for struct accesses of the form ( *p ).x or p -> x, with this annotation the arrow syntax sugar is used *)
  | Reference (* Used to encode references as pointers with annotation Reference *)
  | Stackvar (* Used to encode stack variables *)
  | Annot_stringreprid of stringreprid (* Memoization id for the string representation of this node *)

(* symbols to add while printing a C++ program.*)
and special_operator =
  | Address_operator (* used to print the ampersand operator for declarations of the form int x = &b*)
  | Star_operator (* used to print the start operator when dereferencing a pointer , ex int y = *b *)

(* We only need to support two specific attributes for the time being *)
and attribute = (* LATER: rename to typ_annot when typ_annot disappears *)
  | Identifier of var
  | Aligned of trm
  | GeneratedTyp


and record_type =
  | Struct
  | Union
  | Class

(* [trm] is a record representing an ast node *)
and trm =
 { annot : trm_annot list;
   marks : mark list;
   desc : trm_desc;
   loc : location;
   is_statement : bool;
   add : special_operator list;
   typ : typ option;
   ctx : ctx option;
   attributes : attribute list }


and trms = trm list

(* A [typ_env] stores all the information about types, labels, constructors, etc. *)
(* [ctx_var] is useful for interpreting types that are provided in the user scripts *)
and ctx = {
  ctx_var : typ varmap; (* from [var] to [typ], i.e. giving the type of program variables *)
  ctx_tconstr : typconstrid varmap; (* from [typconstr] to [typconstrid] *)
  ctx_typedef : typedef typmap; (* from [typconstrid] to [typedef] *)
  ctx_label : typconstrid varmap; (* from [label] to [typconstrid] *)
  ctx_constr : typconstrid varmap; (* from [constr] to [typconstrid] *)
  } (* NOTE: ctx_label and ctx_constr *)

  (* Example ctx for the type definitions

        type t = { f : u }
        and u = A | B of t

    ctx_tconstr :
      "t" --> id0
      "u" --> id1

    ctx_typdef :
      id0 --> {  ...; typedef_body = Typdef_struct [ ("f", Typ_tconstr ("u", id1, []) ] }
      id1 -->   ...  (typ_tconstr ("t", id0))

    ctx_label :
      "f" --> id0

    ctx_constr :
      "A" --> id1
      "B" --> id1

  *)
(* Example recursive type in C
  typedef struct node { branches : node* } node;

  | trm_typedef of typedef
  in ctx_typdef add the binding from node to this typdef

  *)


 (* IN THE FUTURE
and trm =
 { desc : trm_desc;
   loc : location;
   kind : trm_kind;
   typ : typ option:
   env : env option; (can be used)
   annot : trm_annot list; }
*)

(* description of an ast node *)

and trm_desc =
  | Trm_val of value
  | Trm_var of varkind * var
  | Trm_array of trm mlist (* { 0, 3, 5} as an array *)
  | Trm_struct of trm mlist (* { 4, 5.3 } as a record *)
  | Trm_let of varkind * typed_var * trm (* int x = 3 *)
  | Trm_let_mult of varkind * typ * var list * trm list (* int a, b = 3, c; *)
  | Trm_let_fun of var * typ * (typed_vars) * trm
  | Trm_let_record of string * record_type * (label * typ) list * trm
  (* LATER: trm_fun  for anonymous functions *)
  (* LATER: mutual recursive functions via mutual recursion *)
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm (* if (x > 0) {x += 1} else{x -= 1} *)
  (* question: distinguish toplevel seq for other seqs? *)
  | Trm_seq of trm mlist (* { st1; st2; st3 } *)
  | Trm_apps of trm * (trms) (* f(t1, t2) *)
  | Trm_while of trm * trm (* while (t1) { t2 } *)
  | Trm_for of var * trm * loop_dir * trm * loop_step  * trm
  | Trm_for_c of trm * trm * trm * trm
  | Trm_do_while of trm * trm
  (*
    Trm_for_c (e0, e1, e2, e3) =
    for (e0; e1; e2) {e3;}
   *)
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
  | Trm_abort of abort (* return or break or continue *)
  | Trm_labelled of label * trm (* foo: st *)
  | Trm_goto of label
  | Trm_arbitrary of code_kind
  | Trm_omp_directive of directive
  | Trm_omp_routine of omp_routine
  | Trm_extern of string * trms
  | Trm_namespace of string * trm * bool
  | Trm_template of template_parameter_list * trm

and template_param_kind =
  | Type_name of typ option
  | NonType of typ * trm option
  | Template of template_parameter_list

and template_parameter_list = (string * template_param_kind * bool) list

(* type for the mutability of the varaible*)
and varkind =
  | Var_immutable
  | Var_mutable (* [Var_mutable] means that we had a declaration of a non-const stack-allocated variable. *)

(* ways of aborting *)
and abort =
  | Ret of trm option (* return;  or return 3; *)
  | Break of label option
  | Continue of label option


(* mode used for default OpenMP clause *)
and mode =
  | Shared_m
  | None_

(* expression representing the code inside an If OpenMP clause *)
and expression = string

(* scheduling type for OpenMP *)
and sched_type =
  | Static
  | Dynamic
  | Guided
  | Runtime

(* reduction operation for OpenMP reduction clause *)
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
(* map type for map OpenMP clause *)
and map_type =
  | Alloc
  | To
  | From
  | ToFrom
  | No_map

and proc_bind =
  | Master_pb
  | Close
  | Spread

and dependence_type =
  | In of vars
  | Out of vars
  | Inout of vars
  | Outin of vars
  | Sink of vars
  | Source

(* OpenMP clauses *)
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
  | Aligned_c of vars * int
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
  | Sections_c
  | For_c
  | Taskgroup_c
  | Proc_bind of proc_bind
  | Priority of var
  | Depend of dependence_type
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
(* atomic operations for atomic OpenMP directive *)
and atomic_operation =
  | Read
  | Write
  | Update
  | Capture

(* OpenMP directives *)
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
  | Taskwait
  | Taskyield
  | Teams of clause list
  | Teams_distribute of clause list
  | Teams_distribute_end of clause list
  | Teams_distribute_parallel_for of clause list
  | Teams_distribute_parallel_for_simd of clause list
  | Threadprivate of vars

(* OpenMP Routines *)
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


(* patterns *)
type pat = trm

(* rewrite_rule *)
type rewrite_rule = {
  rule_vars : typed_vars;
  rule_aux_vars : typed_vars;
  rule_from : pat;
  rule_to : pat }

(* basic rewrite rules *)
type base = rewrite_rule list

(* pattern instantiation *)
module Trm_map = Map.Make(String)



type tmap = trm Trm_map.t

type instantiation = trm Trm_map.t

(* used for defining the type of reordering for struct fields *)
type reorder =
  | Reorder_before of string
  | Reorder_after of string
  | Reorder_all

(* **************************Typ Construcors**************************** *)
let typ_const ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (t : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_const t; typ_attributes}

let typ_var ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (x : typvar) (tid : typconstrid) : typ =
  {typ_annot = annot; typ_desc = Typ_var (x, tid); typ_attributes}

let typ_constr ?(annot : typ_annot list = []) ?(typ_attributes = []) ?(tid : typconstrid = next_typconstrid ())
  ?(tl : typ list = []) (x : typvar) : typ =
  {typ_annot = annot; typ_desc = Typ_constr (x, tid, tl); typ_attributes}

let typ_auto ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_auto ; typ_attributes}

let typ_unit ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_unit; typ_attributes}

let typ_int ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_int; typ_attributes}

let typ_float ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_float; typ_attributes}

let typ_double ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_double; typ_attributes}

let typ_bool ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_bool; typ_attributes}

let typ_char ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_char; typ_attributes}

let typ_string ?(annot : typ_annot list = []) ?(typ_attributes = []) () : typ =
  {typ_annot = annot; typ_desc = Typ_string; typ_attributes}

let typ_ptr ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (kind : ptr_kind) (t : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_ptr {ptr_kind = kind; inner_typ = t}; typ_attributes}

let typ_array ?(annot : typ_annot list = []) ?(typ_attributes = []) (t : typ)
  (s : size) : typ =
  {typ_annot = annot; typ_desc = Typ_array (t, s); typ_attributes}

let typ_fun ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (args : typ list) (res : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_fun (args, res); typ_attributes}

let typ_record ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (rt : record_type) (name : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_record (rt, name); typ_attributes}

let typ_template_param ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (name : string) : typ =
  {typ_annot = annot; typ_desc = Typ_template_param name; typ_attributes}

let typdef_prod ?(recursive:bool=false) (field_list : (label * typ) list) : typdef_body =
  Typdef_prod (recursive, field_list)

(* [typ_ptr_generated ty] krejt a generated start used for encodings *)
let typ_ptr_generated (ty : typ) : typ =
  typ_ptr ~typ_attributes:[GeneratedTyp] Ptr_kind_mut ty

(* [typ_ref_inv ty] get the inner type of a reference *)
let typ_ref_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref; inner_typ = ty1} -> Some ty1
  | _ -> None

(* [typ_ptr_inv ty] get the inner type of a pointer *)
let typ_ptr_inv (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty1} -> Some ty1
  | _ -> None


(* [typ_str ~annot ~typ_attributes s] *)
let typ_str ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (s : code_kind) : typ =
  {typ_annot = annot; typ_desc = Typ_arbitrary s ; typ_attributes}

(* function that fails with given error message and points location in file *)
exception TransfoError of string

exception Resolve_target_failure of location option * string

let loc_to_string (loc : location) : string =
  match loc with
  | None -> ""
  | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
     (filename ^ " start_location [" ^ (string_of_int start_row) ^": " ^ (string_of_int start_column) ^" ]" ^
     " end_location [" ^ (string_of_int end_row) ^": " ^ (string_of_int end_column) ^" ]")

let fail (loc : location) (err : string) : 'a =
  match loc with
  | None -> raise (TransfoError err)
  | Some _ -> raise (TransfoError (loc_to_string loc ^ " : " ^ err))

(* *************************** Trm constructors *************************** *)

let trm_annot_add (a:trm_annot) (t:trm) : trm =
  { t with annot =  a :: t.annot }

let trm_annot_has (a : trm_annot) (t : trm) : bool =
  List.mem a t.annot

let trm_annot_filter (pred:trm_annot->bool) (t:trm) : trm =
  { t with annot = List.filter pred t.annot }

let trm_annot_remove (annot : trm_annot) (t : trm) : trm =
  { t with annot = Tools.list_remove annot t.annot }

let trm_get_stringreprid (t : trm) : stringreprid option =
  List.find_map (function Annot_stringreprid id -> Some id | _ -> None) t.annot

let trm_special_operator_add (a : special_operator) (t : trm) : trm =
  { t with add = a :: t.add }

let trm_special_operator_remove (sp : special_operator) (t : trm) : trm =
  { t with add = Tools.list_remove sp t.add }

let trm_val ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (v : value) : trm =
  {annot; marks = []; desc = Trm_val v; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_var ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) ?(kind : varkind = Var_mutable) (x : var) : trm =
  {annot; marks; desc = Trm_var (kind,x); loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_array ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (tl : trm mlist) : trm =
  {annot; marks; desc = Trm_array tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_struct ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (tl : trm mlist) : trm =
  {annot; marks; desc = Trm_struct tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_let ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (kind : varkind) (typed_var : typed_var) (init : trm): trm =
  {annot; marks; desc = Trm_let (kind,typed_var,init); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_let_mult ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (kind : varkind) (ty : typ) (vl : var list) (tl : trms) : trm =
  {annot; marks; desc = Trm_let_mult (kind, ty, vl, tl); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_let_fun ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (name : var) (ret_typ : typ) (args : typed_vars) (body : trm) : trm =
  {annot; marks; desc = Trm_let_fun (name,ret_typ,args,body); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_typedef ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (def_typ : typedef): trm =
  {annot; marks; desc = Trm_typedef def_typ; loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_if ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (cond : trm) (tb : trm) (eb : trm) : trm =
  {annot; marks; desc = Trm_if (cond, tb, eb); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_seq ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (tl : trm mlist) : trm =
  {annot; marks; desc = Trm_seq tl; loc = loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_seq_nomarks ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (tl : trms) : trm =
  trm_seq ~annot ~add ~loc ~attributes ~ctx (Mlist.of_list tl)

let trm_apps ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(typ = None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (f : trm)
  (args : trms) : trm =
  {annot; marks; desc = Trm_apps (f, args); loc = loc; is_statement; add; typ;
   attributes; ctx}

let trm_while ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (cond : trm) (body : trm) : trm =
  {annot; marks; desc = Trm_while (cond, body); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_do_while ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (body : trm) (cond : trm) : trm =
  {annot; marks; desc = Trm_do_while (body, cond); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_for_c?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (init : trm) (cond : trm) (step : trm) (body : trm) : trm =
  {annot; marks; desc = Trm_for_c (init, cond, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_switch ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (cond : trm) (cases : (trms * trm) list) : trm =
  {annot; marks; desc = Trm_switch (cond, cases); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_abort ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (a : abort) : trm =
  {annot; marks; desc = Trm_abort a; loc = loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_labelled ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (l : label) (t : trm) : trm =
  {annot; marks; desc = Trm_labelled (l, t); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_goto ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (l : label) : trm =
  {annot; marks; desc = Trm_goto l; loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_null ?(annot = []) ?(loc = None) ?(ctx : ctx option = None) (_ : unit) : trm =
  trm_val ~annot ~loc ~ctx  (Val_ptr 0)
(*
   no type for primitives and operators:
   we are only interested in the result of their application
 *)

let trm_unop ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (p : unary_op) : trm =
  trm_val ~annot ~loc ~add ~ctx (Val_prim (Prim_unop p))

let trm_binop ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (p : binary_op) : trm =
  trm_val ~annot:annot ~loc ~ctx ~add (Val_prim (Prim_binop p))

(* Get typ of a literal *)
let typ_of_lit (l : lit) : typ option =

  match l with
  | Lit_unit -> Some (typ_unit ())
  | Lit_uninitialized -> None
  | Lit_bool _ -> Some (typ_bool ())
  | Lit_int _ -> Some (typ_int ())
  | Lit_double _ -> Some (typ_double ())
  | Lit_string _ -> Some (typ_string ())

let trm_lit ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (l : lit) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx ~typ:(typ_of_lit l) (Val_lit l)

let trm_prim ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (p : prim) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx (Val_prim p)

let trm_set ?(annot = []) ?(loc = None) ?(is_statement : bool = false) ?(add = []) ?(ctx : ctx option = None)
  ?(typ : typ option = Some (typ_unit ())) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ~loc ~is_statement ~add ~ctx ~typ
    (trm_binop Binop_set) [t1; t2]

let trm_neq ?(annot = []) ?(loc = None) ?(is_statement : bool = false) ?(add = []) ?(ctx : ctx option = None)
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ~loc ~is_statement ~add ~ctx ~typ:(Some (typ_unit ()))
    (trm_binop Binop_neq) [t1; t2]

let trm_uninitialized ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
() : trm =
  {annot; marks = []; desc = Trm_val (Val_lit Lit_uninitialized); loc = loc; is_statement=false; add; typ; attributes; ctx}

let trm_for ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (index : var) (start : trm) (direction : loop_dir) (stop : trm) (step : loop_step) (body : trm) : trm =
  {annot; marks; desc = Trm_for (index, start, direction, stop, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let code ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) (code : code_kind) : trm =
  {annot; marks = []; desc = Trm_arbitrary code ; loc = loc; is_statement=false; add; typ; attributes; ctx}

let trm_omp_directive ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(directive : directive) : trm =
  {annot; marks; desc = Trm_omp_directive directive; loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_omp_routine ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(omp_routine : omp_routine) : trm =
  {annot; marks; desc = Trm_omp_routine omp_routine; loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_extern ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(lang : string) (tl : trms) : trm =
  {annot; marks; desc = Trm_extern (lang, tl); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_namespace ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(name : string) (t : trm ) (inline : bool) : trm =
  {annot; marks; desc = Trm_namespace (name, t, inline); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_let_record ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(name : string) (rt : record_type ) (lt : (label * typ) list) (t : trm) : trm =
  {annot; marks; desc = Trm_let_record (name, rt, lt, t); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_template ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(tpl : template_parameter_list) (t : trm ) : trm =
  {annot; marks; desc = Trm_template (tpl, t); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_cast (ty : typ) (t : trm) : trm =
  trm_apps (trm_unop (Unop_cast ty)) [t]

let trm_remove_marks (t : trm) : trm =
  match t.desc with 
  (* In the case of sequences, special treatment is needed for inbetween marks*)
  | Trm_seq tl -> {t with desc = Trm_seq {items = tl.items; marks = []}; marks = []}
  | _ -> {t with marks = []}

let trm_add_mark (m : mark) (t : trm) : trm =
  {t with marks = m :: t.marks}


let trm_filter_mark (pred : mark -> bool) (t : trm): trm =
  {t with marks = List.filter (fun m -> pred m) t.marks}

let trm_remove_mark (m : mark) (t : trm) : trm =
  trm_filter_mark (fun m1 -> m <> m1) t

let trm_add_mark_between (index : int) (m : mark) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_mark_at index m tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "trm_add_mark_between: expected a sequence"

let trm_remove_mark_between (m : mark) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.remove_mark m tl in
    trm_seq ~annot:t.annot ~marks:t.marks new_tl
  | _ -> fail t.loc "trm_remove_mark_between: expected a sequence"


(* get the singleton declaration variable in the case when [t] is a variable declaration or a list of variable in the case when
    we have multiple variable declarations in one line
*)
let rec trm_vardef_get_vars (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x, _), _) -> [x]
  | Trm_seq tl when List.mem Multi_decl t.annot -> List.flatten (List.map trm_vardef_get_vars (Mlist.to_list tl))
  | _ -> []



(* [trm_ret ~annot] special trm_abort case, used for return statements*)
let trm_ret ?(annot = []) ?(loc = None) ?(add = []) ?(marks : mark list = [])
  (a : trm option) : trm =
  trm_abort ~annot ~loc ~add ~marks (Ret a)

(* get the primitive operation *)
let trm_prim_inv (t : trm) : prim option =
  match t.desc with
  | Trm_val (Val_prim p) -> Some p
  | _ -> None


(* get the primitive value *)
let trm_lit_inv (t : trm) : lit option =
  match t.desc with
  | Trm_val (Val_lit v) -> Some v
  | _ -> None

(* convert an integer to an ast node *)
let trm_int (n : int) : trm = trm_lit (Lit_int n)

(* Unit trm, unvisble to the user *)
let trm_unit () : trm =
  trm_lit (Lit_unit)

(* convert a double/float to an ast node *)
let trm_double (d : float) : trm = trm_lit (Lit_double d)

(* convert an integer to an ast node *)
let trm_bool (b : bool) : trm = trm_lit (Lit_bool b)

(* ********************************************************************************************************************* *)

(* for target betweens marks are stored on the parent sequence,
  this function give the index that mark m targets to
*)
let get_mark_index (m : mark) (t : trm) : int option =
  match t.desc with
  | Trm_seq tl ->
    Tools.fold_lefti (fun i acc ml ->
      match acc with
      | Some _ -> acc
      | None ->
        if List.mem m ml then Some i else None
    ) None tl.marks
  | _ -> fail t.loc "get_mark_index: expected a sequence trm"

let is_included (t : trm) : bool =
 List.exists (function Include _ -> true | _ -> false) t.annot

(*
  compute a function that prints information related to some location in file
  only if the verbose flag is activated
 *)
let print_info (loc : location) : ('a, out_channel, unit) format -> 'a =
  if !Flags.verbose then
    match loc with
    | None -> Printf.printf
    | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
       Printf.kfprintf Printf.fprintf stdout ("<%s> from <%d>,<%d> to   <%d>,<%d>") filename start_row start_column end_row end_column
  else
    Printf.ifprintf stdout

(* concrete accesses in a trm *)
type trm_access =
  | Array_access_get of trm (* operator -> [i] *)
  | Array_access_addr of trm (* operator [i] *)
  | Struct_access_get of field (* operator->f *)
  | Struct_access_addr of field (* operator.f *)

(* This function is used when matching targets over struct or array accesses *)
(* get_nested_accesses t = (base, access list) where the succession of accesses
  applied to base gives t
  the list is nil if t is not a succession of accesses
 *)
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

(* From a list of accesses build the original trm *)
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

(* [loop_step_to_trm l_step] return the loop step as trm *)
let loop_step_to_trm (l_step: loop_step) : trm =
  match l_step with
  | Post_inc | Post_dec | Pre_inc | Pre_dec -> trm_lit (Lit_int 1)
  | Step s -> s

(* [is_step_one step] check if the step of the loop is one or not *)
let is_step_one (step : loop_step) : bool =
  match step with
  | Post_inc | Post_dec | Pre_inc | Pre_dec -> true
  | _ -> false

(* [apply_on_loop_step] used for functions which need to be applied in trms*)
let apply_on_loop_step (f : trm -> trm) (l_step : loop_step) : loop_step =
  match l_step with
  | Step st -> Step (f st)
  | _ as step ->  step


(* apply a function over ast nodes, if nodes are terminal nodes than specific treatment is considered
    depending on the definition of function f
    Note: This is an unoptimized version of trm_map_with_terminal
*)
let trm_map_with_terminal_unopt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_statement = t.is_statement in
  let typ = t.typ in
  let marks = t.marks in
  match t.desc with
  | Trm_array tl ->
    let tl' = Mlist.map (f false) tl in
    trm_array ~annot ~loc ~add ~typ ~marks tl'
  | Trm_struct tl ->
    let tl' = Mlist.map (f false) tl in
    trm_struct ~annot ~loc ~add ~typ ~marks tl'
  | Trm_let (vk, tv, init) ->
    let init' = f false init in
    trm_let ~annot ~marks ~loc ~is_statement ~add vk tv init'
  | Trm_let_fun (f', res, args, body) ->
    let body' = f false body in
    trm_let_fun ~annot ~marks ~loc ~is_statement ~add f' res args body'
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = f is_terminal then_ in
    let else_' = f is_terminal else_ in
    trm_if ~annot ~marks ~loc ~add cond' then_' else_'
  | Trm_seq tl ->
    let n = Mlist.length tl in
    let tl' = Mlist.mapi(fun i tsub ->
      let sub_is_terminal = is_terminal && i == n-1 in
      f sub_is_terminal tsub
    ) tl in
    trm_seq ~annot ~marks ~loc tl'
  | Trm_apps (f', args) ->
    let f'' = f false f' in
    let args' = List.map (f false) args in
     (*
       warning: f'' may have different type
       -> print and reparse to have the right type
      *)
    trm_apps ~annot ~loc ~is_statement ~add ~typ ~marks f'' args'
  | Trm_while (cond, body) ->
     let cond' = f false cond in
     let body' = f false body in
     trm_while ~annot ~marks ~loc ~add cond' body'
  | Trm_for_c (init, cond, step, body) ->
     let init' = f false init in
     let cond' = f false cond in
     let step' = f false step in
     let body' = f is_terminal body in
     trm_for_c ~annot ~marks ~loc ~add init' cond' step' body'
  | Trm_for (index, start, direction, stop, step, body) ->
    let m_step = match step with
    | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
    | Step sp -> Step (f is_terminal sp)
    in
    let start' = f false start in
    let stop' = f false stop in
    let body' = f is_terminal body in
    trm_for ~annot ~marks ~loc ~add index start' direction stop' m_step body'
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, f is_terminal body)) cases in
     trm_switch ~annot ~marks ~loc ~add cond' cases'
  | Trm_abort a ->
     begin match a with
     | Ret (Some t') -> trm_ret ~annot ~marks ~loc ~add (Some (f false t'))
     (* return without value, continue, break *)
     | _ -> t
     end
  | Trm_labelled (l, body) ->
     trm_labelled ~annot ~marks ~loc ~add l (f false body)
  | _ -> t

(* TODO:
  trm_map (f: bool -> trm -> trm) (t : trm) : trm =

  trm_map_with_terminal_opt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
    using trm_map, and only duplicating 5 cases
*)

(* trm_map_with_terminal derived from trm_map *)
let trm_map_with_terminal_opt (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_statement = t.is_statement in
  let typ = t.typ in
  let marks = t.marks in
  let aux = f is_terminal in

  (* [ret nochange t'] evaluates the condition [nochange]; if is true,
     it returns [t], because [f] has not performed any change on [t];
     else, it returns the new result [t'], which was computed as [f t]. *)
  let ret nochange t' =
    if nochange then t else t' in

  (* [flist tl] applies [f] to all the elements of a list [tl] *)
  let flist tl =
    let tl' = List.map (f false) tl in
    if List.for_all2 (==) tl tl' then tl else tl'
   in
  (* [fmlist] is like [flist] but for marked lists *)
  let fmlist is_terminal tl =
    let tl' = Mlist.map (f is_terminal) tl in
    if Mlist.for_all2 (==) tl tl' then tl else tl' in
  match t.desc with
  | Trm_array tl ->
    let tl' = fmlist false tl in
    ret (tl' == tl)
        (trm_array ~annot ~loc ~add ~typ ~marks tl')
  | Trm_struct tl ->
    let tl' = fmlist false tl in
    ret (tl' == tl)
        (trm_struct ~annot ~loc ~add ~typ ~marks tl')
  | Trm_let (vk, tv, init) ->
    let init' = f false init in
    ret (init' == init)
        (trm_let ~annot ~marks ~loc ~is_statement ~add vk tv init')
  | Trm_let_fun (f', res, args, body) ->
    let body' = f false body in
    ret (body' == body)
        (trm_let_fun ~annot ~marks ~loc ~is_statement ~add f' res args body')
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = aux then_ in
    let else_' = aux else_ in
    ret (cond' == cond && then_' == then_ && else_' == else_)
        (trm_if ~annot ~marks ~loc ~add cond' then_' else_')
  | Trm_seq tl ->
    let n = Mlist.length tl in
    let tl' = Mlist.mapi (fun i tsub ->
        let sub_is_terminal = (is_terminal && i == n-1) in
        f sub_is_terminal tsub
      ) tl in
    ret (Mlist.for_all2 (==) tl tl')
        (trm_seq ~annot ~marks ~loc tl')
  | Trm_apps (func, args) ->
    let func' = f false func in
    let args' = flist args in
    ret (func' == func && args' == args)
      (trm_apps ~annot ~loc ~is_statement ~add ~typ ~marks func' args')
  | Trm_while (cond, body) ->
    let cond' = f false cond in
    let body' = f false body in
    trm_while ~annot ~marks ~loc ~add cond' body'
  | Trm_for_c (init, cond, step, body) ->
     let init' = f false init in
     let cond' = f false cond in
     let step' = f false step in
     let body' = aux body in
     ret (init' == init && cond' == cond && step' == step && body' == body)
         (trm_for_c ~annot ~marks ~loc ~add init' cond' step' body')
  | Trm_for (index, start, direction, stop, step, body) ->
    let start' = f false start in
    let stop' = f false stop in
    let step' = match step with
      | Post_inc | Post_dec | Pre_inc | Pre_dec -> step
      | Step sp -> Step (aux sp)
      in
    let body' = aux body in
    ret (step' == step && start' == start && stop' == stop && body' == body)
        (trm_for ~annot ~marks ~loc ~add index start' direction stop' step' body')
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, aux body)) cases in
     ret (cond' == cond && List.for_all2 (fun (_tl1,body1) (_tl2,body2) -> body1 == body2) cases' cases)
         (trm_switch ~annot ~marks ~loc ~add cond' cases')
  | Trm_abort a ->
    begin match a with
    | Ret (Some t') -> trm_ret ~annot ~marks ~loc ~add (Some (f false t'))
    | _ -> t
    end
  | Trm_labelled (l, body) ->
    let body' = f false body in
    trm_labelled ~annot ~marks ~loc ~add l body'
  | _ -> t


let trm_map_with_terminal (is_terminal : bool)  (f : bool -> trm -> trm) (t : trm) : trm =
  trm_map_with_terminal_unopt is_terminal f t

let trm_map (f : trm -> trm) (t : trm) : trm =
  trm_map_with_terminal false (fun _is_terminal t -> f t) t


(* similar as trm_map for types *)
let typ_map (f : typ -> typ) (ty : typ) : typ =
  let annot = ty.typ_annot in
  let typ_attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr {ptr_kind= pk; inner_typ = ty} -> typ_ptr ~annot ~typ_attributes pk (f ty)
  | Typ_array (ty, n) -> typ_array ~annot ~typ_attributes (f ty) n
  | Typ_fun (tyl, ty) ->
     typ_fun ~annot ~typ_attributes (List.map f tyl) (f ty)
  (* var, unit, int, float, double, bool, char *)
  | _ -> ty

(* [label_subterms_with_fresh_stringreprids f t] annotates all the subterms of [t]
   that satisfy the boolean predicate [f] with a fresh string representation identifier.
   This operation should be performed to enable the term to doc function to memoize
   its results, and possibly export a table mapping subterms to their string representation. *)
let rec label_subterms_with_fresh_stringreprids (f : trm -> bool) (t : trm) : trm =
  let t2 =
    if not (f t) then t else begin
      let id = next_stringreprid () in
      let keep_annot = List.filter (function Annot_stringreprid _ -> false | _ -> true) t.annot in
       { t with annot = (Annot_stringreprid id)::keep_annot }
    end in
  trm_map (label_subterms_with_fresh_stringreprids f) t2


(* [contains_decl x t] check if t constains a subtem that is a redeclaration of variable x *)
let contains_decl (x : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_let (_, (y, _), _) when y = x -> true
    | Trm_seq tl -> Mlist.fold_left (fun acc t -> acc || aux t) false tl
    | Trm_for (y, _, _, _, _,body) -> y = x || aux body
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_for_c (init, _, _, body) -> aux init || aux body
    | _ -> false
  in aux t

(* return the name of the declared object as an optional type *)
let decl_name (t : trm) : var option =
  match t.desc with
  | Trm_let (_,(x,_),_) -> Some x
  | Trm_let_fun (f, _, _, _) -> Some f
  | Trm_typedef td -> Some td.typdef_tconstr
  | _ -> None

(* [vars_bound_in_trm_init t] get the list of variables that are bound inside 
   the initialization trm of the for_c loop*)
let vars_bound_in_trm_init (t : trm) : var list = 
  match t.desc with 
  | Trm_let (_, (x,_), _) -> [x]
  | Trm_let_mult (_, _, vl, _) -> vl
  | _ -> []

(* checks if two declarations are of the same category  *)
let same_node_type (t : trm) (t1 : trm) : bool =
  begin match t.desc, t1.desc with
  | Trm_let _ , Trm_let _ -> true
  | Trm_let_fun _, Trm_let_fun _ -> true
  | Trm_typedef _, Trm_typedef _ -> true
  | _ -> false
  end



(* check ia a typ is a type used only for optitrust encoding *)
let is_generated_typ (ty : typ) : bool =
  List.mem GeneratedTyp ty.typ_attributes

(* check if two arrays are of the same size *)
let same_sizes (sz1 : size) (sz2 : size) : bool =
 match sz1, sz2 with
 | Undefined, Undefined -> true
 | Const i1, Const i2 -> i1 = i2
 | Trm t1, Trm t2->  t1 = t2
 | _, _ -> false

(* check if two types are the same *)
let rec same_types ?(match_generated_star : bool = false) (typ_1 : typ) (typ_2 : typ) : bool =
  let aux = same_types ~match_generated_star in
  (typ_1.typ_annot = typ_2.typ_annot) &&
  (
  match typ_1.typ_desc, typ_2.typ_desc with
  | Typ_const typ_a1, Typ_const typ_a2 ->
    (aux typ_a1 typ_a2)
  | Typ_var (a1, _), Typ_var (a2, _) ->
    a1 = a2
  | Typ_constr (typ_var1, typ_id1, typ_list1), Typ_constr (typ_var2, typ_id2, typ_list2) ->
    (typ_var1 = typ_var2) && (typ_id1 = typ_id2) && (typ_list1 = typ_list2)
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
  | _, _ -> false
  )

(* get the value of a variable initialization *)
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
  | Trm_val (Val_prim (Prim_new _)) -> None
  | _ -> Some t



(* return the name of the index of the for loop *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for (index, _, _, _, _,  _) -> index
  | Trm_for_c (init, _, _, _) ->
     (*
       covered cases:
       - for (i = …; …)
       - for (int i = …; …)
      *)

     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [{desc = Trm_var (_, x); _}; _]) ->x
     | _ -> begin match decl_name init with
            | Some x -> x
            | None -> fail init.loc "for_loop_index: could't get the loop index"
            end
     end
  | _ -> fail t.loc "for_loop_index: expected for loop"


(* return the initial value of the loop index *)
let for_loop_init (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, _, _, _) ->
     (*
       covered cases:
       - for (i = n; …)
       - for (int i = n; …)
      *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; n]) -> n
     | Trm_let (_,(_, _), init) ->
        begin match get_init_val init with
        | Some v  -> v
        | None -> fail init.loc "for_loop_init: bad for loop initialization"
        end
     | _ -> fail init.loc "for_loop_init: bad for loop initialisation"
     end
  | _ -> fail t.loc "for_loop_init: expected for loop"

(* return the lower bound of the for loop *)
let for_loop_bound (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, cond, _, _) ->
     (*
       covered cases:
       - for (…; i < n; …)
       - for (…; i <= n; …)
       - for (…; i > n; …)
       - for (…; i >= n; …)
      *)
     begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _},
                 [_; n]) -> n
     | _ -> fail cond.loc "for_loop_bound: bad for loop condition"
     end
  | _ -> fail t.loc "for_loop_bound: expected for loop"

(* return the step increment of the for loop *)
let for_loop_step (t : trm) : trm =
  match t.desc with
  | Trm_for_c (_, _, step, _) ->
     (*
       covered cases:
       - for (…; …; i++)
       - for (…; …; ++i)
       - for (…; …; i--)
       - for (…; …; --i)
       - for (…; …; i += n) for n > 0
       - for (…; …; i -= n) for n > 0
      *)
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
        | _ -> fail step.loc "for_loop_step: bad for loop step"
        end
     | _ -> fail step.loc "for_loop_step: bad for loop step"
     end
  | _ -> fail t.loc "for_loop_step: expected for loop"

(* get the number of iterations of a for loop *)
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

(* get the list of trms from the body of the loop *)
let for_loop_body_trms (t : trm) : trm mlist =
  match t.desc with
  | Trm_for (_, _, _, _, _, body) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> fail body.loc "for_loop_body_trms: body of a simple loop should be a sequence"
    end
  | Trm_for_c (_, _, _,  body) ->
    begin match body.desc with
    | Trm_seq tl -> tl
    | _ -> fail body.loc "for_loop_body_trms: body of a generic loop should be a sequence"
    end
  | _ -> fail t.loc "for_loop_body_trms: expected a loop"

(* kind of the type used when parsing initialization lists*)
type typ_kind =
  | Typ_kind_undefined
  | Typ_kind_reference
  | Typ_kind_array
  | Typ_kind_sum
  | Typ_kind_prod
  | Typ_kind_basic of typ_desc
  | Typ_kind_fun
  | Typ_kind_var

(* convert a type kind to a string *)
let typ_kind_to_string (tpk : typ_kind) : string =
  begin match tpk with
  | Typ_kind_undefined -> "undefined"
  | Typ_kind_reference -> "reference"
  | Typ_kind_array -> "array"
  | Typ_kind_sum -> "sum"
  | Typ_kind_prod -> "prod"
  | Typ_kind_basic _ -> "basic"
  | Typ_kind_fun -> "fun"
  | Typ_kind_var -> "var"
  end
let is_atomic_typ (t : typ) : bool =
  match t.typ_desc with
  | Typ_int | Typ_unit | Typ_float | Typ_double | Typ_bool | Typ_char |Typ_string -> true
  | _ -> false

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
        | Typdef_prod _ -> Typ_kind_prod
        | Typdef_sum _| Typdef_enum _ -> Typ_kind_sum
        end
     end

  | _ -> Typ_kind_basic ty.typ_desc

(* bypass the pointer type used only for optitrust encoding *)
let get_inner_ptr_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_ptr {inner_typ = ty1;_} when is_generated_typ ty -> ty1
  | _ -> ty

(* [get_inner_const_type ty] bypass the const type *)
let get_inner_const_type (ty : typ) : typ = 
  match ty.typ_desc with 
  | Typ_const ty -> ty
  | _ -> ty

(* check if the type is a reference type or not *)
let is_reference (ty : typ) : bool =
  let ty = get_inner_ptr_type ty in
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_ref;_} -> true
  | _ -> false

(* check if the type is const or not *)
let is_typ_const (t : typ) : bool =
  begin match t.typ_desc with
  | Typ_array (tx, _) ->
    begin match tx.typ_desc with
    | Typ_const _ -> true
    | _ -> false
    end
  | Typ_const _ -> true
  | _ -> false
  end

(* some need some special bounds to define tiling correctly *)
type tile_bound = TileBoundMin | TileBoundAnd | TileBoundDivides

(* used for managing better annotated Noraces sequences *)
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

(* genereate a no_brace sequence with a fresh id *)
let trm_seq_no_brace ?(marks : mark list = [])(tl : trms) : trm=
    trm_annot_add (No_braces (Nobrace.current())) (trm_seq ~marks (Mlist.of_list tl))

(* get the id of the sequence annotated as No_braces *)
let get_nobrace_id (t : trm) : int option =
  let rec aux l = match l with
  | [] -> None
  | hd :: tl ->
    begin match hd with
    | No_braces i -> Some i
    | _ -> aux tl
    end in
    aux t.annot


(* This type is used for variable renaming, the uer can choose between renaming all the variables
    on one block, by giving the prefix to add or he can also  give the list of variable to
    be renamed together with their new name.
*)
type rename = | Suffix of string | Rename_list of (var * var) list

(* get the literal value from a trm_lit *)
let get_lit_from_trm_lit (t : trm) : lit =
  match t.desc with
  | Trm_val (Val_lit l) -> l
  | _ -> fail t.loc "get_lit_from_trm: this type of literal is not supported"

(* Check if the node has type unit *)
let is_type_unit (t : typ) : bool =
  match t.typ_desc with
  | Typ_unit -> true
  | _ -> false

(* Check if a trl is a trm_lit *)
let is_lit (t : trm) : bool =
  match t.desc with
  | Trm_val (Val_lit _) -> true
  | _ -> false

(* [add_star t] add the star operator to a trm *)
let add_star (t : trm) : trm =
  trm_special_operator_add Star_operator t


(* [is_typ_ptr ty] check if the type ty is a pointer type *)
let is_typ_ptr (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut;_} -> true
  | _ -> false

(* [is_get_operation t] check if [t] is a struct access get operation of a immutable variable get operation *)
let is_star_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some (Prim_unop Unop_get) -> true
    | _ -> false
    end
  | _ -> false

(* [is_get_operation t] check if [t] is a struct access get operation of a immutable variable get operation *)
let is_get_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps ({desc = Trm_val(Val_prim (Prim_unop Unop_get))}, _) -> true
  | _ -> false

(* [is_new_operation t] check if [t] is new operation *)
let is_new_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some (Prim_new _) -> true
    | _ -> false
    end
  | _ -> false

(* check if [t] is a set operation *)
let is_set_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match trm_prim_inv f with
    | Some (Prim_binop Binop_set)
     | Some (Prim_overloaded_op (Prim_binop Binop_set)) -> true
    | _ -> false
    end
  | _ -> false

(* [get_operation_arg t] get the arg of a get operation *)
let get_operation_arg (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) -> t1
  | _ -> t (* fail t.loc "get_operation_arg: this function should be called only on get operations " *)

(* [trm_let_mut ~annot ~is_statement ~add ~attributes ~ctx ~marks typed_var init] an extension of trm_let for creating mutable variable declarations *)
let trm_let_mut ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr_generated var_type in
  trm_let ~annot:(Stackvar :: annot) ~loc ~is_statement ~add ~attributes ~ctx ~marks Var_mutable (var_name, var_type_ptr) (trm_apps (trm_prim (Prim_new var_type)) [init])


(* [trm_let_ref ~annot ~is_statement ~add ~attributes ~ctx ~marks typed_var init] an extension of trm_let for creating reference variable declarations *)
let trm_let_ref ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr_generated var_type in
  trm_let ~annot:(Reference :: annot) ~loc ~is_statement ~add ~attributes ~ctx ~marks Var_mutable (var_name, var_type_ptr) init


(* [trm_let_IMmut ~annot ~is_statement ~add ~attributes ~ctx ~marks typed_var init] an extension of trm_let for creating immutable variable declarations *)
let trm_let_immut ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type = typ_const var_type in
  trm_let ~annot ~loc ~is_statement ~add ~attributes ~ctx ~marks Var_immutable (var_name, var_type) (init)

(* [trm_let_array ~annot ~is_statement ~add ~attributes ~ctx ~marks typed_var init] an extension of trm_let for creating array variable declarations *)
let trm_let_array ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (kind : varkind )(typed_var : typed_var) (sz : size)(init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_const = if kind = Var_immutable then typ_const var_type else var_type in 
  let var_type = typ_array var_type_const sz in
  let var_type_ptr = if kind = Var_mutable then typ_ptr_generated var_type else var_type in 
  let var_init = if kind = Var_immutable then init else trm_apps (trm_prim (Prim_new var_type)) [init]  in
  let res = trm_let ~annot ~loc ~is_statement  ~add ~attributes ~ctx ~marks kind (var_name, var_type_ptr) var_init in
  if kind = Var_mutable then trm_annot_add Stackvar res else res


(* [trm_for_c_inv_simple_init init] check if the init loop component is simple or not.
    It not then return None else return the index used in the init trm, its initial value and a boolean which states if
  the loop index is declared locally or belongs to another scope.
  Ex.:
    int x = a -> Some (x, a, true)
    x = a -> Some (x, a, false)
*)
let trm_for_c_inv_simple_init (init : trm) : (var * trm * bool) option =
  match init.desc with
  | Trm_let (_, (x, _), init_val) ->
    begin match get_init_val init_val with
    | Some init1 ->
      Some (x, init1, true)
    | _ -> None
    end
  | _ -> None


(* [trm_for_c_inv_simple_stop stop] check if the loop bound is simple or not.
      If not  then return None else return the bound the and the direction of the loop.
*)
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


(* [trm_for_c_inv_simple_step step] check if the loop step is simple or not.
    If not then return None else return the step that can be a literal or a variable.
*)
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
      (* begin match t'.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_add)); _},
                  [_; n]) ->
         Some (Step n)
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_sub)); _},
                  [_; n]) ->
         Some (Step n)
      | _ -> None
      end *)
  | _ -> None


(* [trm_for_of_trm_for_c t] checks if loops [t] is a simple loop or not, if yes then return the simple loop
    else returns [t]
 *)
let trm_for_of_trm_for_c (t : trm) : trm =
  match t.desc with
  | Trm_for_c (init, cond , step, body) ->
    let init_ops = trm_for_c_inv_simple_init init in
    let bound_ops = trm_for_c_inv_simple_stop cond in
    let step_ops = trm_for_c_inv_simple_step step in
    begin match init_ops, bound_ops, step_ops with
    | Some (index, start, is_local), Some (direction, stop), Some step ->
      let for_loop = trm_for index start direction stop step body in
      if (not is_local ) then trm_annot_add Non_local_index for_loop else for_loop
    | _ -> t
    end
  | _ -> fail t.loc "trm_for_of_trm_for_c: expected a for loop"

(* DEPRECATED: Remove it after removing ast_to_c.ml *)
(* before printing a simple loop first it should be converted to complex loop *)
let trm_for_to_trm_for_c ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (index : var) (start : trm) (direction : loop_dir) (stop : trm) (step : loop_step) (body : trm) : trm =
  let init = trm_let Var_mutable (index, typ_int()) start in
  let cond = begin match direction with
    | DirUp ->
      (trm_apps (trm_binop Binop_lt) [trm_var index;stop])
    | DirUpEq ->
      (trm_apps (trm_binop Binop_le) [trm_var index;stop])
    | DirDown ->
      (trm_apps (trm_binop Binop_gt) [trm_var index;stop])
    | DirDownEq ->
      (trm_apps (trm_binop Binop_ge) [trm_var index;stop])
   end in
  let step =
    begin match direction with
    | DirUp | DirUpEq ->
      begin match step with
      | Pre_inc ->
        trm_apps (trm_unop Unop_pre_inc) [trm_var index]
      | Post_inc ->
        trm_apps (trm_unop Unop_post_inc) [trm_var index]
      | Step st ->
        trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_add)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [st]])
      | _ -> fail body.loc "trm_for_to_trm_for_c: can't use decrementing operators for upper bounded for loops"
      end
    | DirDown | DirDownEq ->
      begin match step with
      | Pre_dec ->
        trm_apps (trm_unop Unop_pre_dec) [trm_var index]
      | Post_dec ->
        trm_apps (trm_unop Unop_post_dec) [trm_var index]
      | Step st ->
        trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_sub)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [st]])
      | _ -> fail body.loc "trm_for_to_trm_for_c: can't use decrementing operators for upper bounded for loops"
      end

    end in

    trm_for_c ~annot ~loc ~add ~attributes ~ctx init cond step body

type delocalize_ops =
  | Delocalize_arith of lit * binary_op
  | Delocalize_obj of string * string



let get_include_filename (t : trm) : string  =
  let f_name = List.fold_left (fun acc x ->
    match x with
    | Include s -> Some s
    | _ -> acc
  ) None t.annot in
  match f_name with
  | Some s -> s
  | _ -> fail t.loc "get_include_filename: couldn't get the requested filename"

(* simplifiy unary operations on literals*)
let compute_app_unop_value (p : unary_op) (v1:lit) : trm =
  match p, v1 with
  | Unop_neg, Lit_bool b -> trm_bool (not b)
  | Unop_post_inc, Lit_int n -> trm_int (n + 1)
  | Unop_pre_inc, Lit_int n -> trm_int (n + 1)
  | Unop_post_dec, Lit_int n -> trm_int (n - 1)
  | Unop_pre_dec, Lit_int n -> trm_int (n - 1)
  | _ -> fail None "compute_app_unop_value: only negation can be applied here"

(* simplifiy binary operations on literals*)
let compute_app_binop_value (p : binary_op) (v1 : lit) (v2 : lit) : trm =
  match p,v1, v2 with
  | Binop_eq , Lit_int n1, Lit_int n2 -> trm_bool (n1 == n2)
  | Binop_eq, Lit_double d1, Lit_double d2 -> trm_bool (d1 == d2)
  | Binop_neq , Lit_int n1, Lit_int n2 -> trm_bool (n1 <> n2)
  | Binop_neq, Lit_double d1, Lit_double d2 -> trm_bool (d1 <> d2)
  | Binop_sub, Lit_int n1, Lit_int n2 -> trm_int (n1 - n2)
  | Binop_sub, Lit_double d1, Lit_double d2 -> trm_double (d1 -. d2)
  | Binop_add, Lit_int n1, Lit_int n2 -> trm_int (n1 + n2)
  | Binop_add, Lit_double d1, Lit_double d2 -> trm_double (d1 +. d2)
  | Binop_mul, Lit_int n1, Lit_int n2 -> trm_int (n1 * n2)
  | Binop_mul, Lit_double n1, Lit_double n2 -> trm_double (n1 *. n2)
  | Binop_mod, Lit_int n1, Lit_int n2 -> trm_int (n1 mod n2)
  | Binop_div, Lit_int n1, Lit_int n2 -> trm_int (n1 / n2)
  | Binop_div, Lit_double d1, Lit_double d2 -> trm_double (d1 /. d2)
  | Binop_le, Lit_int n1, Lit_int n2 -> trm_bool (n1 <= n2)
  | Binop_le, Lit_double d1, Lit_double d2 -> trm_bool (d1 <= d2)
  | Binop_lt, Lit_int n1, Lit_int n2 -> trm_bool (n1 < n2)
  | Binop_lt, Lit_double d1, Lit_double d2 -> trm_bool (d1 < d2)
  | Binop_ge, Lit_int n1, Lit_int n2 -> trm_bool (n1 >= n2)
  | Binop_ge, Lit_double d1, Lit_double d2 -> trm_bool (d1 >= d2)
  | Binop_gt, Lit_int n1, Lit_int n2 -> trm_bool (n1 > n2)
  | Binop_gt, Lit_double d1, Lit_double d2 -> trm_bool (d1 > d2)
  | _ -> fail None "compute_app_binop_value: operator not supporeted"

(* convert a list of variable declarations to a list of paris where each pair
    consists of a variable and its type
*)
let decl_list_to_typed_vars (tl : trms) : typed_vars =
  List.map (fun t ->
    match t.desc with
    | Trm_let (_, (x, tx),_) -> (x, get_inner_ptr_type tx)
    | _ -> fail t.loc "decl_list_to_typed_vars: expected a list of declarations"
  ) tl

(* check if [t] is a variable occurrence or a value *)
let rec trm_is_val_or_var (t : trm) : bool =
  match t.desc with
  | Trm_val _ | Trm_var _ -> true
  | Trm_apps (_, [var_occ]) when is_get_operation t -> trm_is_val_or_var var_occ
  | _ -> false

type loop_range = var * trm * loop_dir * trm * loop_step

let trm_for_inv (t : trm) : (loop_range * trm)  option =
  match t.desc with
  | Trm_for (index, start, direction, stop, step, body) -> Some ((index, start, direction,stop ,step), body)
  | _ -> None

(* [is_trm_seq t] check if [t] is a sequence or not *)
let is_trm_seq (t : trm) : bool =
  match t.desc with
  | Trm_seq _ -> true  | _ -> false

(* [trm_fors rgs tbody] create nested loops with the main body [tbody] each nested loop
    takes its components from [rgs]
*)
let trm_fors (rgs : loop_range list) (tbody : trm) : trm =
  List.fold_right (fun x acc ->
    let index, start, direction,stop, step = x in
    trm_for index start direction stop step (if (is_trm_seq acc) then acc else trm_seq_nomarks [acc])
  ) rgs tbody


(* [trm_var_def_inv t] get the name type and the initialization value  *)
let trm_var_def_inv (t : trm) : (varkind * var * typ * trm option) option =
  match t.desc with
  | Trm_let (vk, (x,tx), init) ->
    let init1 = match get_init_val init with
    | Some init1 -> Some init1
    | _ -> None in
    Some (vk, x, get_inner_ptr_type tx, init1)
  | _ -> None

(* [trm_fors_inv nb t] got into a node of nested loops and return all the components
    of all the loops up to the depth of [nb]
 *)
let trm_fors_inv (nb : int) (t : trm) : (loop_range list * trm) option =
  let nb_loops = ref 0 in
  let body_to_return  = ref (trm_int 0) in
  let rec aux (t : trm) : loop_range list =
    match t.desc with
    | Trm_for (index, start, direction, stop, step, body) ->
      incr nb_loops;
      begin match body.desc with
      | Trm_seq tl when Mlist.length tl = 1 ->
        if !nb_loops = nb
          then begin
            body_to_return := body;
            (index, start, direction, stop, step) :: []
            end
          else
            (index, start, direction, stop, step) :: aux (Mlist.nth tl 0)
      | _ ->
        (index, start, direction, stop, step) :: []
      end

    | _ -> []
    in

  let loop_range_list = aux t in
  if List.length loop_range_list <> nb then None else Some (loop_range_list, !body_to_return)


let is_trm_uninitialized (t:trm) : bool =
  match t.desc with
  | Trm_val (Val_lit Lit_uninitialized) -> true
  | _ -> false

let is_trm_var (t : trm) : bool =
  match t.desc with
  | Trm_var _ -> true
  | _ -> false



exception Unknown_key

(* [tmap_to_list keys map] get the list of values for all keys [keys] in map [map] *)
let tmap_to_list (keys : vars) (map : tmap) : trms =
  List.map (fun x -> match Trm_map.find_opt x map with
    | Some v -> v
    | None -> raise Unknown_key
  ) keys

(* [tmap_filter keys tmap] remove all the bindings with [keys] in [map] and return that map *)
let tmap_filter (keys : vars) (map : tmap) : tmap =
  Trm_map.filter (fun k _ -> not (List.mem k keys)) map


(* [is_trm t] check if [t] is a proper ast node or not *)
let is_trm (t : trm) : bool =
  match t.desc with
  | Trm_arbitrary _ -> false
  | _ -> true

(* [is_typ ty] check if [ty] is a type ast or a string *)
let is_typ (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_arbitrary _ -> false
  | _ -> true

exception No_ast_or_code_provided
exception Ast_and_code_provided

(* [hide_function_bodies f_pred tl] all the toplevel function with their names satisfying
   [f_pred] will have hidden bodie. Others will be kept unchanged.
    The new ast is called the chopped_ast. This function wlll return the choped_ast and a map with keys
    the names of the functions whose body has been removed and values their removed body.
*)
let hide_function_bodies (f_pred : var -> bool) (t : trm) : trm * tmap =
  let t_map = ref Trm_map.empty in
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_let_fun (f,ty, tv, _) ->
        if f_pred f then begin
          t_map := Trm_map.add f t !t_map;
         trm_let_fun ~annot:t.annot ~marks:t.marks f ty tv (trm_lit  Lit_uninitialized) end
        else t
      | _ -> trm_map aux t
      in
  let res = aux t in
  res, !t_map

(* [update_chopped_ast chopped_ast chopped_fun_map] for all the functions whose bodies were removed during the creation
    of the chopped_ast restore their bodies by using [chopped_fun_map] which is map with keys the names of the functions
    that were chopped and values their actual declaration
*)
let update_chopped_ast (chopped_ast : trm) (chopped_fun_map : tmap): trm =
  match chopped_ast.desc with
  | Trm_seq tl ->
      let new_tl =
      Mlist.map (fun def -> match def.desc with
      | Trm_let_fun (f, _, _, _) ->
        begin match Trm_map.find_opt f chopped_fun_map with
        | Some tdef ->  tdef
        | _ -> def
        end
      |_ ->  def
    ) tl in trm_seq ~annot:chopped_ast.annot ~marks:chopped_ast.marks new_tl
  | _ -> fail chopped_ast.loc "update_ast_with_chopped_ast: ast of the main file should start with a top level sequence"


(* [is_infix_prim_fun p] check if the primitive function [p] is one of those which supports app and set operations or not*)
let is_infix_prim_fun (p : prim) : bool =
  match p with
  | Prim_compound_assgn_op __ -> true
  | _ -> false


(* [is_arith_fun p] check if the primitive function [p] is an arithmetic operation or not *)
let is_arith_fun (p : prim) : bool =
  match p with
  | Prim_binop bin_op ->
    begin match bin_op with
    | Binop_add | Binop_sub | Binop_mul | Binop_div | Binop_mod -> true
    | _ -> false
    end
  | _ -> false


(* [is_same_binop op1 op2 ] check if two primitive operations are the same or not.
    Used to decide if parentheses should be printed or not.
*)
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


(* [trm_access base field] create a dummy access without type checking*)
let trm_struct_access ?(typ : typ option = None) (base : trm) (field : var) : trm =
  trm_apps ~typ (trm_unop (Unop_struct_access field)) [base]

(* [trm_get t] generates a get operation in [t] *)
let trm_get ?(annot : trm_annot list = []) ?(typ : typ option = None) (t : trm) : trm =
  trm_apps ~annot (trm_unop Unop_get) [t]

(* [trm_var_get x] generates *x *)
let trm_var_get ?(typ : typ option = None) (x : var) : trm =
  trm_get ~typ (trm_var ~typ x)

(* [trm_var_possibly_mut ~const ty] *)
let trm_var_possibly_mut ?(const : bool = false) ?(typ : typ option = None) (x : var) : trm =
  if const then trm_var ~typ ~kind:Var_immutable x else trm_var_get ~typ x

(* [trm_new ty t] generates new ty (t) *)
let trm_new (ty : typ) (t : trm) : trm =
  trm_apps (trm_prim (Prim_new ty)) [t]

(* [trm_any_bool] generates ANY_BOOL () *)
let trm_any_bool : trm =
  trm_apps (trm_var "ANY_BOOL") []

(* [trm_eq ~loc ~ctx ~typ t1 t2] generates t1 = t2 *)
let trm_eq ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx  Binop_eq) [t1; t2]

(* [trm_neq t1 t2] generates t1 != t2 *)
let trm_neq ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_neq) [t1; t2]

(* [trm_sub t1 t2] generates t1 - t2 *)
let trm_sub ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_sub) [t1; t2]

(* [trm_add t1 t2] generates t1 + t2 *)
let trm_add ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_add) [t1; t2]

(* [trm_mul t1 t2] generates t1 * t2 *)
let trm_mul ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_mul) [t1; t2]

(* [trm_div t1 t2] generates t1 / t2 *)
let trm_div ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_div) [t1; t2]

(* [trm_le t1 t2] generates t1 <= t2 *)
let trm_le ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_le) [t1; t2]

(* [trm_lt t1 t2] generates t1 < t2 *)
let trm_lt ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_lt) [t1; t2]

(* [trm_ge t1 t2] generates t1 >= t2 *)
let trm_ge ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_ge) [t1; t2]

(* [trm_gt t1 t2] generates t1 > t2 *)
let trm_gt ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_gt) [t1; t2]

(* [trm_ineq ineq_sgn t1 t2] generates an inequality t1 # t2
    where # is one of the following operators <, <=, >, >=.
    The operator is provided implicitly through the [ineq_sng] arg
*)
let trm_ineq (ineq_sgn : loop_dir) (t1 : trm) (t2 : trm) : trm =
  match ineq_sgn with
  | DirUp -> trm_lt t1 t2
  | DirUpEq -> trm_le t1 t2
  | DirDown ->  trm_gt t1 t2
  | DirDownEq -> trm_ge t1 t2


(* [trm_and t1 t2] generates t1 && t2 *)
let trm_and ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_and) [t1;t2]

(* [trm_bit_and t1 t2] generates t1 && t2 *)
let trm_bit_and ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_bitwise_and) [t1;t2]

(* [trm_or t1 t2] generates t1 || t2 *)
let trm_or ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_or) [t1;t2]

(* [trm_bit_or t1 t2] generates t1 || t2 *)
let trm_bit_or ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_bitwise_or) [t1;t2]

(* [trm_shiftl t1 t2] generates t1 << t2*)
let trm_shiftl ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_shiftl) [t1; t2]

(* [trm_shiftr t1 t2] generates t1 >> t2*)
let trm_shiftr ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_shiftr) [t1; t2]

(* [trm_mod t1 t2] generates t1 % t2*)
let trm_mod ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_mod) [t1;t2]

(* [trm_xor t1 t2] generates t1 ^ t2 *)
let trm_xor ?(loc = None) ?(ctx : ctx option = None) ?(typ = None) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_xor) [t1; t2]

(* [trm_ands ts] generalized version of trm_and *)
let trm_ands (ts : trm list) : trm =
  Tools.fold_lefti (fun i acc t1 ->
    if i = 0 then t1 else trm_and acc t1
  ) (trm_bool true) ts


(* [trm_prim_compound ~loc ~is_statement ~ctx ~typ binop t1 t2] generates a compound operation, ex t1+=t2*)
let trm_prim_compound ?(marks : mark list = []) ?(loc = None) ?(is_statement : bool = false) ?(ctx : ctx option = None) ?(typ = None) ?(annot :trm_annot list = [])(binop : binary_op) (t1 : trm) (t2 : trm) : trm =
  trm_apps ~loc ~is_statement ~typ ~annot ~marks (trm_prim ~loc ~ctx (Prim_compound_assgn_op binop)) [t1; t2]

(* [trm_prim_compound ~loc ~is_statement ~ctx ~typ binop t1 t2] generates a compound operation, ex t1+=t2*)
let trm_prim_compound_encoded_as_set ?(loc = None) ?(is_statement = false) ?(ctx : ctx option = None) ?(typ = None) ?(annot : trm_annot list = []) (binop : binary_op) (tl : trm) (tr : trm) : trm =
  let annot = App_and_set :: annot in
  trm_set ~annot ~loc ~is_statement ~typ tl
    (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx binop) [tl; tr])


(* [code_to_str] extract the code from the nodes that contain the arbitrary code*)
let code_to_str (code : code_kind) : string =
  match code with
  | Lit l -> l
  | Atyp ty -> ty
  | Expr e -> e
  | Atypexpr tye -> tye
  | Stmt s -> s

module AstParser = struct

  let var = trm_var

  let lit l = code (Lit l)

  let atyp ty = typ_str (Atyp ty)

  let subst_dollar_number (inst : string list) (s : string) : string =
  Tools.fold_lefti (fun i acc insti ->
    Tools.string_subst ("${"^(string_of_int i) ^ "}") insti acc
  ) s inst

  let expr ?(vars : var list = []) (e : string)  =
    let e = if vars = [] then e else subst_dollar_number vars e in
    code (Expr e)

  let atypexpr tye = typ_str (Atypexpr tye)

  let stmt s = code (Stmt s)

end

(* [var_mutability_unkown] dummy value used for variable mutability*)
let var_mutability_unknown = Var_mutable

(* [top_level_fun_bindings t] return a map with keys the names of toplevel function names
    and values their bodies *)
let top_level_fun_bindings (t : trm) : tmap =
  let tmap = ref Trm_map.empty in
    let aux (t : trm) : unit =
      match t.desc with
      | Trm_seq tl ->
        Mlist.iter (fun t1 ->
          match t1.desc with
          | Trm_let_fun (f, _, _, body) -> tmap := Trm_map.add f body !tmap
          | _ -> ()
        ) tl
      | _ -> fail t.loc "top_level_fun_bindings: expected the global sequence that contains all the toplevel declarations"
   in
  aux t;
  !tmap

(* [get_common_top_fun tm1 tm2] takes two maps, binding function names to terms describing
   the function bodies, and returns the list of function names that ard bound to the same
   terms in the two maps. *)
let get_common_top_fun (tm1 : tmap) (tm2 : tmap) : vars =
  let common = ref [] in
  Trm_map.iter (fun f1 b1 ->
    match Trm_map.find_opt f1 tm2 with
    | Some b2 when b1 == b2 -> common := f1 :: !common
    | _ -> ()
  ) tm1;
  !common

(* [serialize_to_file filename t] writes a serialized version of the AST [t] into the file [filename]. *)
let serialize_to_file (filename : string) (t : trm) : unit =
  Tools.serialize_to_file filename t

(* [unserialize_from_file filename] reads a serialized AST from the file [filename]. *)
let unserialize_from_file (filename : string) : trm =
  Tools.unserialize_from_file filename

(* [empty_ast] *)
let empty_ast : trm =
  trm_seq_nomarks ~annot:[Main_file] []

(* [trm_simplify_addressof_and_get t] simplifies [&*t] and [*&t] to [t] *)
let trm_simplify_addressof_and_get (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) }
    ])
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [t1]) }
    ]) -> t1
  | _ -> t

(* [simpl_struct_get_get t] transform struct_get (get(t1), f) to get(struct_access (t1, f))*)
let simpl_struct_get_get (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as op, [t1]) ->
    begin match t1.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [t2]) ->
      {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [t2]))}]))}
    | _ -> t
    end
  | _ -> t

(* [simpl_array_get t] tranform array_get (get(t1), index) to get(array_access (t1), index) *)
let rec simpl_array_get_get (t : trm) : trm =
  let aux = simpl_array_get_get in 
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)));_} as op, [base; index]) ->
    begin match base.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_} as op1, [base1]) ->
       {t with desc = (Trm_apps (op1, [{t with desc = (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [base1; index]))}]))}
    | _ -> trm_map aux t
    end
  | _ -> trm_map aux t

(* [simpl_accesses t] *)
let simpl_accesses (t : trm) = 
 trm_simplify_addressof_and_get (simpl_struct_get_get (simpl_array_get_get t))


(* TODO: define simpl_accesses, a function that calls
   simpl_struct_get (trm_simplify_addressof_and_get t)
   use this function instead of trm_simplify_addressof_and_get
   in the fromto file.
   LATER: try to remove "begin match u1.desc with .." since it's
   already covered by simpl_accesses.

   TODO: define [Expr.simpl_accesses tg] to invoke this transformation
   on the AST.  If tg = [], use dRoot. *)

(* [array_access base index] generates array_access (base, index) *)
let array_access (base : trm) (index : trm) : trm =
  trm_apps (trm_binop Binop_array_access) [base; index]

(* [get_array_access base index] generates get(array_access (base, index)) *)
let get_array_access (base : trm) (index : trm) : trm =
  trm_get (array_access base index)

(* [get_array_access_inv t] returns the Some(base, index) of an array_access if [t]
     is of the form get(array_access(base, index) otherwise None *)
let get_array_access_inv (t : trm) : (trm * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg]) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access));_}, [base;index]) -> Some (base, index)
    | _ -> None
    end
  | _ -> None

(* [struct_access base index] generates struct_access (base, index) *)
let struct_access (f : field) (base : trm) : trm =
  trm_apps (trm_unop (Unop_struct_access f)) [base]

(* [get_struct_access base index] generates get(struct_access (base, index)) *)
let get_struct_access (f : field) (base : trm) : trm =
  trm_get (struct_access f base)

(* [get_struct_access_inv t] returns the Some(base, index) of an struct_access if [t]
     is of the form get(struct_access(base, index) otherwise None *)
let get_struct_access_inv (t : trm) : (string * trm) option =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [arg]) ->
    begin match arg.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)));_}, [base]) -> Some (f, base)
    | _ -> None
    end
  | _ -> None

(* [trm_var_assoc_list to_map al] creat a map from an association list wher keys are string and values are trms *)
let map_from_trm_var_assoc_list (al : (string * trm) list) : tmap = 
  let tm = Trm_map.empty in 
  List.fold_left (fun acc (k, v) -> Trm_map.add k v acc) tm al