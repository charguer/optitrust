(* a record used to represent a specifi location inside the code*)
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
let next_typconstrid : (unit -> int) =
  Tools.fresh_generator()

(* ['a typmap] is a map from [typeid] to ['a] *)
module Typ_map = Map.Make(Int)
type 'a typmap = 'a Typ_map.t



(* a map from int to int *)

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

(* Type used to defin the direction and the size of the step of the *)
type loop_dir =
 | DirUp
 | DirDown


(* array sizes *)
type size =
  | Undefined (* t[] *)
  | Const of int (* t[3] *)
  | Trm of trm (* t[2*nb] *)




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
  | Typ_ptr of  {ptr_kind : ptr_kind; inner_typ: typ } (* "int*" *)
  | Typ_array of typ * size (* int[3], or int[], or int[2*n] *)
  | Typ_fun of (typ list) * typ  (* int f(int x, int y) *)
  | Typ_record of record_type * typ
  | Typ_template_param of string
  (* LATER:  Typ_arbitrary of string *)

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
  | Unop_bitwise_neg
  | Unop_neg
  | Unop_opp
  | Unop_post_inc
  | Unop_post_dec
  | Unop_pre_inc
  | Unop_pre_dec
  | Unop_struct_field_addr of field
  | Unop_struct_field_get of field
  | Unop_cast of typ (* cast operator towards the specified type *)

(* binary operators *)
and binary_op =
  | Binop_set (* type annotation?    lvalue = rvalue *)
  | Binop_array_cell_addr
  | Binop_array_cell_get
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
  | Prim_new of typ (* "new T" *)
  | Prim_conditional_op (* "(foo) ? x : y" *)

(* literals *)
and lit =
  | Lit_unit (* void, e.g. "return;" is represented as "Lit_unit" *)
  | Lit_uninitialized (* e.g. "int x;" is "int x = Lit_uninitalized" *)
  | Lit_bool of bool
  | Lit_int of int
  | Lit_double of float
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
  | As_left_value (* Used for reference encoding *)
  | Any (* Used for only one specific transformation called delocalize *)
(* symbols to add while printing a C++ program.*)
and special_operator =
  | Address_operator (* used to print the ampersand operator for declarations of the form int x = &b*)
  | Star_operator (* used to print the start operator when dereferencing a pointer , ex int y = *b *)

(* We only need to support two specific attributes for the time being *)
and attribute = (* LATER: rename to typ_annot when typ_annot disappears *)
  | Identifier of var
  | Aligned of trm
  | GeneratedStar


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
  | Trm_var of var (* LATER: varkind * var *)
  | Trm_array of trm mlist (* { 0, 3, 5} as an array *)
  | Trm_struct of trm mlist (* { 4, 5.3 } as a record *)
  | Trm_let of varkind * typed_var * trm (* int x = 3 *)
  | Trm_let_fun of var * typ * (typed_vars) * trm
  | Trm_let_record of string * record_type * trms * trm
  (* LATER: trm_fun  for anonymous functions *)
  (* LATER: mutual recursive functions via mutual recursion *)
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm (* if (x > 0) {x += 1} else{x -= 1} *)
  (* question: distinguish toplevel seq for other seqs? *)
  | Trm_seq of trm mlist (* { st1; st2; st3 } *)
  | Trm_apps of trm * (trms) (* f(t1, t2) *)
  | Trm_while of trm * trm (* while (t1) { t2 } *)
  | Trm_for of var * loop_dir * trm * trm * trm  * trm
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
  | Trm_arbitrary of string
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
  rule_vars : vars;
  rule_aux_vars : vars;
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

(* function that fails with given error message and points location in file *)
exception TransfoError of string

exception Resolve_target_failure of location option * string

let fail (loc : location) (err : string) : 'a =
  match loc with
  | None -> failwith err
  | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
     raise (TransfoError (filename ^ " start_location [" ^ (string_of_int start_row) ^": " ^ (string_of_int start_column) ^" ]" ^
     " end_location [" ^ (string_of_int end_row) ^": " ^ (string_of_int end_column) ^" ]" ^ " : " ^ err))




(* *************************** Trm constructors *************************** *)

let trm_annot_add (a:trm_annot) (t:trm) : trm =
  { t with annot =  a :: t.annot }

let trm_annot_filter (pred:trm_annot->bool) (t:trm) : trm =
  { t with annot = List.filter pred t.annot }

let trm_add_operator (a : special_operator) (t : trm) : trm =
  { t with add = a :: t.add}

let trm_val ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (v : value) : trm =
  {annot; marks = []; desc = Trm_val v; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_var ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (x : var) : trm =
  {annot; marks; desc = Trm_var x; loc = loc; is_statement = false; add; typ;
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
  (* todo: add type for strings *)
  | Lit_string _ -> None

let trm_lit ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (l : lit) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx ~typ:(typ_of_lit l) (Val_lit l)

let trm_prim ?(annot = []) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (p : prim) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx (Val_prim p)

let trm_set ?(annot = []) ?(loc = None) ?(is_statement : bool = false) ?(add = []) ?(ctx : ctx option = None)
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ~loc ~is_statement ~add ~ctx ~typ:(Some (typ_unit ()))
    (trm_binop Binop_set) [t1; t2]

let trm_for ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
  (index : var) (direction : loop_dir) (start : trm) (stop : trm) (step : trm) (body : trm) : trm =
  {annot; marks; desc = Trm_for (index, direction, start, stop, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let code ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
(code : string) : trm =
  {annot; marks = []; desc = Trm_arbitrary code; loc = loc; is_statement=false; add; typ; attributes; ctx}

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
(name : string) (rt : record_type ) (tl : trms) (t : trm) : trm =
  {annot; marks; desc = Trm_let_record (name, rt, tl, t); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_template ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = [])
(tpl : template_parameter_list) (t : trm ) : trm =
  {annot; marks; desc = Trm_template (tpl, t); loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_cast (ty : typ) (t : trm) : trm =
  trm_apps (trm_unop (Unop_cast ty)) [t]

let trm_remove_marks (t : trm) : trm =
  {t with marks = []}

let trm_add_mark (m : mark) (t : trm) : trm =
  {t with marks = m :: t.marks}

(* LATER: Maybe trm_filter_mark could be useful here*)
let trm_remove_mark (m : mark) (t : trm) : trm =
  {t with marks = List.filter (fun m1 -> m <> m1) t.marks}

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

(* ********************************************************************************************************************* *)

(* for target betweens marks are stored on the parent sequence,
  this function give the index that mark m targets to
*)
let get_mark_index (m : mark) (t : trm) : int option =
  match t.desc with
  | Trm_seq tl ->
    Tools.foldi (fun i acc ml ->
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
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_field_addr f))); _},
              [t']) ->
     let (base, al) = get_nested_accesses t' in
     (base, Struct_access_addr f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_field_get f))); _},
              [t']) ->
     let (base, al) = get_nested_accesses t' in
     (base, Struct_access_get f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_cell_addr)); _},
              [t'; i]) ->
     let (base, al) = get_nested_accesses t' in
     (base, Array_access_addr i :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_cell_get)); _},
              [t'; i]) ->
     let (base, al) = get_nested_accesses t' in
     (base, Array_access_get i :: al)
  | _ -> (t, [])

(* From a list of accesses build the original trm *)
let build_nested_accesses (base : trm) (access_list : trm_access list) : trm =
  List.fold_left (fun acc access ->
    match access with
    | Struct_access_addr f ->
      trm_apps (trm_unop (Unop_struct_field_addr f)) [acc]
    | Struct_access_get f ->
      trm_apps (trm_unop (Unop_struct_field_get f)) [acc]
    | Array_access_addr i ->
      trm_apps (trm_binop (Binop_array_cell_addr)) [acc;i]
    | Array_access_get i ->
      trm_apps (trm_binop (Binop_array_cell_get)) [acc;i]
  ) base access_list

(* apply a function over ast nodes, if nodes are terminal nodes than specific treatment is considered
    depending on the definition of function f
*)
let trm_map_with_terminal (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_statement = t.is_statement in
  let typ = t.typ in
  let marks = t.marks in
  match t.desc with
  | Trm_array tl ->
    trm_array ~annot ~loc ~add ~typ ~marks (Mlist.map (f false) tl)
  | Trm_struct tl ->
    trm_struct ~annot ~loc ~add ~typ ~marks (Mlist.map (f false) tl)
  | Trm_let (vk, tv, init) ->
    trm_let ~annot ~marks ~loc ~is_statement ~add vk tv (f false init)
  | Trm_let_fun (f', res, args, body) ->
    trm_let_fun ~annot ~marks ~loc ~is_statement ~add f' res args (f false body)
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
    trm_seq ~annot ~marks tl'
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
  | Trm_for (index, direction, start, stop, step, body) ->
    trm_for ~annot ~marks ~loc ~add index direction (f is_terminal start) (f is_terminal stop) (f is_terminal step) (f is_terminal body)
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, f is_terminal body)) cases in
     trm_switch ~annot ~marks ~loc ~add cond' cases'
  | Trm_abort a ->
     begin match a with
     | Ret (Some t') -> trm_abort ~annot ~marks ~loc ~add (Ret (Some (f false t')))
     (* return without value, continue, break *)
     | _ -> t
     end
  | Trm_labelled (l, body) ->
     trm_labelled ~annot ~marks ~loc ~add l (f false body)
  | _ -> t

(* similart to trm_map_with_terminal but here terminal nodes are not treated differently *)
let trm_map (f : trm -> trm) (t : trm) : trm =
  trm_map_with_terminal false (fun _is_terminal t -> f t) t

(* same as trm_map for types *)
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

(* check if a trm contains a variable occurrence or not *)
let contains_variable (x : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_var y when y = x -> true
    | Trm_let (_, (_, _), init) -> aux init
    | Trm_apps (_, args) -> List.exists aux args
    | Trm_seq tl -> Mlist.fold_left (fun acc t -> acc || (aux t)) false tl
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_for (_, _, _, _, _, body) -> aux body
    | _ -> false
  in aux t


(* return the name of the declared object as an optional type *)
let decl_name (t : trm) : var option =
  match t.desc with
  | Trm_let (_,(x,_),_) -> Some x
  (* take into account heap allocated variables *)
  | Trm_let_fun (f, _, _, _) -> Some f
  | Trm_typedef td -> Some td.typdef_tconstr
  | _ -> None

(* checks if two declarations are of the same category  *)
let same_node_type (t : trm) (t1 : trm) : bool =
  begin match t.desc, t1.desc with
  | Trm_let _ , Trm_let _ -> true
  | Trm_let_fun _, Trm_let_fun _ -> true
  | Trm_typedef _, Trm_typedef _ -> true
  | _ -> false
  end

(* return the name of the index of the for loop *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for (index, _, _, _, _, _) -> index
  | Trm_for_c (init, _, _, _) ->
     (*
       covered cases:
       - for (i = …; …)
       - for (int i = …; …)
      *)

     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [{desc = Trm_var x; _}; _]) ->
        x
     | _ -> begin match decl_name init with
            | Some x -> x
            | None -> fail init.loc "for_loop_index: could't get the loop index"
            end
     end
  | _ -> fail t.loc "for_loop_index: expected for loop"

let for_loop_direction (t : trm) : loop_dir =
  match t.desc with
  | Trm_for_c (_, cond, _, _) ->
    begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _}, _) -> DirUp
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _}, _) -> DirDown
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _}, _) -> DirUp
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_ge)); _}, _) -> DirDown
     | _ -> fail cond.loc "for_loop_direction: bad for loop condition"
     end
  | _ -> fail t.loc "for_loop_direction: expected a for loop"
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
                 [_; n]) ->
        n
     | Trm_let (_,(_, _), init) ->
        begin match init.desc with
        | Trm_apps(_, [init1]) -> init1
        | _ -> init
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
       - for (…; i > n; …)
      *)
     begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _},
                 [_; n]) -> n
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_le)); _},
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
       - for (…; …; i--)
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
           trm_apps (trm_unop Unop_opp) [n]
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
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_opp)); _}, [step']) ->
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

(* check ia a typ is a type used only for optitrust encoding *)
let is_generated_star (ty : typ) : bool =
  List.mem GeneratedStar ty.typ_attributes

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
  | Typ_ptr {ptr_kind = pk1; inner_typ = typ_a1}, Typ_ptr {ptr_kind = pk2; inner_typ = typ_a2} ->
   if match_generated_star then (pk1 = pk2) && (is_generated_star typ_1 && is_generated_star typ_2) && (aux typ_a1 typ_a2)
    else (not (is_generated_star typ_1 || is_generated_star typ_2)) && (pk1 = pk2) && (aux typ_a1 typ_a2)
  | Typ_array (typa1, size1), Typ_array (typa2, size2) ->
      (same_types typa1 typa2) && (same_sizes size1 size2)
  | _, _ -> false
  )

(* used for distinguishing simple loops from complex ones *)
let is_simple_loop_component (t : trm) : bool =
  match t.desc with
  | Trm_apps (f,_) ->
    begin match f.desc with
    | Trm_val(Val_prim (Prim_unop (Unop_get))) -> true
    | Trm_val(Val_prim (Prim_unop (Unop_pre_inc))) -> false
    | Trm_val(Val_prim (Prim_unop (Unop_pre_dec))) -> false
    | _ -> true
    end
  | Trm_var _ -> true
  | Trm_val (Val_lit (Lit_int _)) -> true
  | Trm_let _ -> true
  | _ -> false


(* check if the loop t is simple or not, if it is then return its simplified ast
   else return the current ast
*)
let trm_for_of_trm_for_c (t : trm) : trm =
  begin match t.desc with
  | Trm_for_c (init,_, step, body) ->
    let index = for_loop_index t in
    let direction = for_loop_direction t in
    let start = for_loop_init t in
    let stop = for_loop_bound t in
    let step_size = for_loop_step t in
    let is_simple_loop =
       (is_simple_loop_component init)
    && (is_simple_loop_component start)
    && (is_simple_loop_component stop)
    && (is_simple_loop_component step) in

    if is_simple_loop
      then trm_for ~loc:t.loc index direction start stop step_size body
      else t
  | _ -> fail t.loc "trm_for_of_trm_for: expected a loop"
  end


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
  | Typ_int | Typ_unit | Typ_float | Typ_double | Typ_bool | Typ_char -> true
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


(* before printing a simple loop first it should be converted to complex loop *)
let trm_for_to_trm_for_c ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (index : var) (direction : loop_dir) (start : trm) (stop : trm) (step : trm) (body : trm) : trm =
  let init = trm_let Var_mutable (index, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int ())) (trm_apps (trm_prim ~loc:start.loc (Prim_new (typ_int ()))) [start]) in
  let cond = begin match direction with
    | DirUp -> (trm_apps (trm_binop Binop_lt)
      [trm_apps ~annot:[Mutable_var_get]
        (trm_unop Unop_get) [trm_var index];stop])
    | DirDown -> (trm_apps (trm_binop Binop_gt)
      [trm_apps ~annot:[Mutable_var_get]
        (trm_unop Unop_get) [trm_var index];stop])
    end
    in

  let step =
    begin match direction with
    | DirUp ->
        begin match step.desc with
        | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_post_inc) [trm_var index]
        | _ ->
          trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_add)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
        end
    | DirDown ->
        begin match step.desc with
        | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_post_dec) [trm_var index]
        | _ ->
          trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_sub)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
        end
    end
    in
  trm_for_c ~annot ~loc ~add ~attributes ~ctx init cond step body

(* bypass the pointer type used only for optitrust encoding *)
let get_inner_ptr_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_ptr {inner_typ = ty1;_} when is_generated_star ty -> ty1
  | _ -> ty

let is_typ_const (t : typ) : bool =
  begin match t.typ_desc with
  | Typ_ptr {inner_typ = tx;_} ->
    begin match tx.typ_desc with
    | Typ_const _ -> true
    | _ -> false
    end
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

(* get the value of a variable initialization *)
let rec get_init_val (t : trm) : trm option =
  match t.desc with 
  | Trm_let (_, (_, _), init) -> get_init_val init
  | Trm_apps(f,[base]) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_new _)) -> Some base
        | _ -> Some t
        end
  | Trm_val (Val_prim (Prim_new _)) -> None
  | _ -> Some t
  
  (* match t.desc with
  | Trm_let (_, (_, _), init) ->
      begin match init.desc with
      | Trm_apps(f,[base]) ->
        begin match f.desc with
        | Trm_val (Val_prim (Prim_new _)) -> Some base
        | _ -> Some init
        end
      | _-> init
      end
  | _ -> fail t.loc "get_init_val: expected a variable declaration" *)


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

(* Add the star operator to a trm *)
let add_star (t : trm) : trm =
  trm_add_operator Star_operator t


(* Check if the type ty is a pointer type *)
let is_typ_ptr (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_ptr {ptr_kind = Ptr_kind_mut;_} -> true
  | _ -> false

(* check if [t] is a struct access get operation of a immutable variable get operation *)
let is_get_operation (t : trm) : bool =
  List.exists (function | Access | Mutable_var_get -> true | _ -> false) t.annot

(* check if [t] is a set operation *)
let is_set_operation (t : trm) : bool =
  match t.desc with
  | Trm_apps (f, _) ->
    begin match f.desc with
    | Trm_val (Val_prim (Prim_binop Binop_set)) -> true
    | _ -> false
    end
  | _ -> false

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

(* get the singleton declaration variable in the case when [t] is a variable declaration or a list of variable in the case when
    we have multiple variable declarations in one line
*)
let rec trm_vardef_get_vars (t : trm) : var list =
  match t.desc with
  | Trm_let (_, (x, _), _) -> [x]
  | Trm_seq tl when List.mem Multi_decl t.annot -> List.flatten (List.map trm_vardef_get_vars (Mlist.to_list tl))
  | _ -> []

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

type loop_range = var * loop_dir * trm * trm * trm 

let trm_for_inv (t : trm) : (loop_range * trm)  option =
  match t.desc with 
  | Trm_for (index, direction, start, stop, step, body) -> Some ((index, direction, start, stop ,step), body)
  | _ -> None


(* [trm_fors rgs tbody] create a nested loops with the main body [tbody] each nested loop
    takes its components from [rgs]
*)
let trm_fors (rgs : loop_range list) (tbody : trm) : trm =
  List.fold_right (fun x acc -> 
    let index, loop_dir, start, stop, step = x in
    trm_for index loop_dir start stop step (trm_seq_nomarks [acc])
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

(* TOOD: Optimize this function later *)
(* [trm_fors_inv nb t] got into a node of nested loops and return all the components
    of all the loops up to the depth of [nb]
 *)
let trm_fors_inv (nb : int) (t : trm) : (loop_range list * trm) option = 
  let nb_loops = ref 0 in
  let body_to_return  = ref (trm_int 0) in
  let rec aux (t : trm) : loop_range list = 
    match t.desc with 
    | Trm_for (index, direction, start, stop, step, body) ->
      incr nb_loops;
      begin match body.desc with 
      | Trm_seq tl when Mlist.length tl = 1 -> 
        if !nb_loops = nb 
          then begin
            body_to_return := body;
            (index, direction, start, stop, step) :: []
            end
          else 
            (index, direction, start, stop, step) :: aux (Mlist.nth tl 0)
      | _ -> 
        (index, direction, start, stop, step) :: []
      end
      
    | _ -> []
    in
  
  let loop_range_list = aux t in
  if List.length loop_range_list <> nb then None else Some (loop_range_list, !body_to_return)
    
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
  

(* TOOD: From now on use these two constructors to add new variables, and later change the implementation of  
    trm_let so that it add the encoding automatically
*)
let trm_let_mut ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type_ptr = typ_ptr Ptr_kind_mut var_type ~typ_attributes:[GeneratedStar] in
  trm_let ~annot ~loc ~is_statement ~add ~attributes ~ctx ~marks Var_mutable (var_name, var_type_ptr) (trm_apps (trm_prim (Prim_new var_type)) [init])

let trm_let_immut ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (typed_var : typed_var) (init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type = typ_const var_type in
  trm_let ~annot ~loc ~is_statement ~add ~attributes ~ctx ~marks Var_immutable (var_name, var_type) (init)

let trm_let_array ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) ?(marks : mark list = []) (kind : varkind )(typed_var : typed_var) (sz : size)(init : trm): trm =
  let var_name, var_type = typed_var in
  let var_type = typ_array var_type sz in
  let var_type_ptr = if kind = Var_immutable then typ_const var_type else typ_ptr Ptr_kind_mut var_type ~typ_attributes:[GeneratedStar] in
  let var_init = if kind = Var_immutable then init else trm_apps (trm_prim (Prim_new var_type)) [init]  in
  trm_let ~annot ~loc ~is_statement  ~add ~attributes ~ctx ~marks kind (var_name, var_type_ptr) var_init


(* [is_trm t] check if [t] is a proper ast node or not *)
let is_trm (t : trm) : bool = 
  match t.desc with
  | Trm_arbitrary _ -> false
  | _ -> true