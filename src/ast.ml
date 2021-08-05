open Tools
 type pos = {
    pos_line : int;
    pos_col : int; }

type node_loc = {
  loc_file : string;
  loc_start : pos;
  loc_end : pos;}

type location = node_loc option

(* memory locations *)
type loc = int

(* variables *)
type var = string

(* name of type constructors (e.g. [list] in Ocaml's type [int list];
   or [vect] in C type [struct { int x,y }; *)
type typconstr = string

(* name of type variables (e.g. ['a] in type ['a list] *)
type typvar = var
type typvars = typvar list

(* unique identifier for typ constructors
   LATER: might rename to typconstrid *)
type typid = int

let next_typid : (unit -> int) =
  Tools.fresh_generator()

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
type constr = string

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
  | Typ_var of typvar * typid (* e.g. ['a] in the type ['a -> 'a] -- *)
  | Typ_constr of typvar * typid * typ list (* e.g. [int list] or [(int,string) map] or [vect] *)
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

and ptr_kind =
  | Ptr_kind_mut
  | Ptr_kind_ref
and typ_annot =
  | Unsigned
  | Long
  | Short

(* LATER
and typ_flags = {
    typ_flags_generated_star : bool;
}
*)

and typ = {
  typ_desc : typ_desc;
  typ_annot : typ_annot list;
  typ_attributes : attribute list;
 (*  typ_flags : typ_flags  *) }
  (* IN THE FUTURE
  ty_env : env; --> tells you for every type what is its definition
  *)

and typedef = { (* e.g. [type ('a,'b) t = ...] *)
  (* LATER: typdef_loc : location; *)
  typdef_typid : typid; (* the unique id associated with the type [t] *)
  typdef_tconstr : typconstr; (* the name [t] *)
  typdef_vars : typvars; (* the list containing the names ['a] and ['b];
    [typedef_vars] is always the empty list in C code without templates *)
  typdef_body : typdef_body;
   } (* the body of the definition, i.e. the description of [...] *)

and typdef_body =
  | Typdef_alias of typ (* for abbreviations, e.g. [type 'a t = ('a * 'a) list] or [typdef vect t] *)
  | Typdef_prod of bool * (label * typ) list (* for records / struct, e.g. [type 'a t = { f : 'a; g : int } *)
  | Typdef_sum of (constr * typ) list (* for algebraic definitions / enum, e.g. [type 'a t = A | B of 'a] *)
  (* Not sure if Typedef_enum is a sum type *)
  | Typdef_enum of (var * (trm option)) list (* LATER: document this, and understand why it's not just a 'typ' like for struct *)

  (* NOTE: we don't need to support the enum from C, for the moment. *)
  (* DEPRECATED
  | Typedef_abbrev of typvar * typ  (* type x = t, where t could be a struct *)
  *)

and typed_var = var * typ

(* primitives *)
and unary_op =
  | Unop_get (* the "*" operator as in *p  *)
  | Unop_bitwise_neg
  | Unop_neg
  | Unop_opp
  | Unop_inc
  | Unop_dec
  | Unop_struct_field_addr of field 
  | Unop_struct_field_get of field 
  | Unop_cast of typ (* cast operator towards the specified type *)

and binary_op =
  | Binop_set (* type annotation?    lvalue = rvalue *)
  | Binop_array_cell_addr 
  | Binop_array_cell_get
  | Binop_eq
  | Binop_neq
  | Binop_sub
  | Binop_add
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


and consistency_mode = 
  | Sequentially_consistent
  | Release
  | Acquire
and prim =
  | Prim_unop of unary_op (* e.g. "!b" *)
  | Prim_binop of binary_op (* e.g. "n + m" *)
  | Prim_new of typ (* "new T" *)
  | Prim_conditional_op (* "(foo) ? x : y" *)
  | Prim_fetch_add
  | Prim_atomic_get of consistency_mode
  | Prim_atomic_set of consistency_mode
  | Prim_compare_and_swap 

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
  (* The constructors below never appear in source code;
     these are values that can only be constructed during the program execution,
     and thus useful only for carrying out proofs about the program Generic *)
  | Val_ptr of loc
  | Val_array of value list
  | Val_struct of value list
  (* LATER: add functions, which are also values that can be created at execution time *)

(* annotations are used to decorate this AST when it is built from the
   Clang AST in such a way to be able to print back the AST like the original C code.
   *)
and trm_annot = 
  (* used to print back a c++ program *)
  | No_braces of int 
  (* annotate applications of star operator that should not be printed *)
  | Access
  (* used to print back seqs that contain multiple declarations *)
  | Multi_decl
  (*
    annotate the boolean literal "true" in for loop conditions to print it as no
    condition
   *)
  | Empty_cond
  (* annotate uses of binop_set that unfold +=, -=, *= *)
  | App_and_set
  (* to avoid printing content of included files *)
  | Include of string
  | Main_file
  | Grouped_binding (* Used for trms of the form int x = 3, y = 4 *)
  | Mutable_var_get (* Used for get(x) operations where x was a non-const stack allocated variable *)
  | As_left_value (* Used for reference encoding *)
  | Highlight of string * string (* Used in show transformations to hightlight a targeted trm*)
  | Any (* Used for only one specific transformation called delocalize *)
(* symbols to add while printing a C++ program.*)
and special_operator =
  | Add_address_of_operator (* used to print the ampersand operator for declarations of the form int x = &b*)
  | Add_star_operator (* used to print the start operator when dereferencing a pointer , ex int y = *b *)

(* We only need to support two specific attributes for the time being *)
and attribute = (* LATER: rename to typ_annot when typ_annot disappears *)
  | Identifier of var
  | Aligned of trm
  | GeneratedStar

(*
  annotated terms
  is_statement: true if the trm is an instruction in a seq
 *)
and trm =
 { annot : trm_annot list; 
   desc : trm_desc;
   loc : location;
   is_statement : bool; 
   add : special_operator list; 
   typ : typ option;
   ctx : ctx option;
   attributes : attribute list }

(* A [typ_env] stores all the information about types, labels, constructors, etc. *)
(* [ctx_var] is useful for interpreting types that are provided in the user scripts *)
and ctx = {
  ctx_var : typ varmap; (* from [var] to [typ], i.e. giving the type of program variables *)
  ctx_tconstr : typid varmap; (* from [typconstr] to [typid] *)
  ctx_typedef : typedef typmap; (* from [typid] to [typedef] *)
  ctx_label : typid varmap; (* from [label] to [typid] *)
  ctx_constr : typid varmap; (* from [constr] to [typid] *)
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

and trm_desc =
  | Trm_val of value
  | Trm_var of var (* LATER: varkind * var *)
  | Trm_array of trm list (* { 0, 3, 5} as an array *)
  | Trm_struct of trm list (* { 4, 5.3 } as a record *)
  | Trm_let of varkind * typed_var * trm (* int x = 3 *)
  | Trm_let_fun of var * typ * (typed_var list) * trm
  (* LATER: trm_fun  for anonymous functions *)
  (* LATER: mutual recursive functions via mutual recursion *)
  | Trm_typedef of typedef
  | Trm_if of trm * trm * trm
  (* question: distinguish toplevel seq for other seqs? *)
  | Trm_seq of trm list (* { st1; st2; st3 } *)
  | Trm_apps of trm * (trm list) (* f(t1, t2) *)
  | Trm_while of trm * trm (* while (t1) { t2 } LATER: other like do-while *)
  | Trm_for of var * loop_dir * trm * trm * trm  * trm
  | Trm_for_c of trm * trm * trm * trm
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
  | Trm_switch of trm * ((trm list * trm) list)
  | Trm_abort of abort (* return or break or continue *)
  | Trm_labelled of label * trm (* foo: st *)
  | Trm_goto of label
  | Trm_arbitrary of string
  | Trm_omp_directive of directive
  | Trm_omp_routine of omp_routine

and varkind =
  | Var_immutable
  | Var_mutable (* [Var_mutable] means that we had a declaration of a non-const stack-allocated variable. *)

(* ways of aborting *)
and abort =
  | Ret of trm option (* return;  or return 3; *)
  | Break (* LATER: could have label option *)
  | Continue (* LATER: could have label option *)


and mode = 
  | Shared
  | None_

and expression = string

and sched_type = 
  | Static
  | Dynamic
  | Guided
  | Runtime

and reduction_identifier =
  | Plus
  | Minus
  | Prod
  | And
  | Or
  | Power
  | BitAnd
  | BitOr

and map_type = 
  | Alloc
  | To
  | From
  | ToFrom

and clause = 
  (* Data sharing clauses *)
  | Default of mode
  | Shared_c of var list
  | Private of var list
  | FirstPrivate of var list
  | LastPrivate of var list
  | Linear of var list * int
  | Reduction of reduction_identifier * (var list)
  (* Data copying clasuses *)
  | Copyin of var list
  | CopyPrivate of var list
  | Map_c of map_type * var list 
  (* SIMD clauses *)
  | Safelen of int
  | Collapse of int
  | Simdlen of int
  | Aligned_c of var list * int
  | Uniform of var list
  | Inbranch
  | NotInbranch
  (* General clauses *)
  | Nowait
  | Ordered_c
  | If of expression
  | Device of int
  | NumThreads of int
  | Schedule of sched_type * int
  | Parallel_c
  | Sections_c
  | For_c
  | Taskgroup_c

and atomic_operation = 
  | Read
  | Write
  | Update
  | Capture

and directive =
  | Atomic of atomic_operation option
  | Atomic_capture 
  | Barrier
  | Cancel of clause * clause list
  | Cancellation_point of clause * clause list
  | Critical
  | Declare_simd of clause list
  | Declare_reduction of reduction_identifier * typvar list * expression * clause
  | Declare_target 
  | Distribute of clause list
  | Distribute_parallel_for of clause list
  | Distribute_parallel_for_simd of clause list
  | Distribute_simd
  | End_declare_target 
  | Flush of var list
  | For of clause list
  | For_simd of clause list
  | Master 
  | Ordered 
  | Parallel of clause list
  | Parallel_for
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
  | Threadprivate of var list

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
  | Set_default_device of int
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
  name : string;
  source : pat;
  target : string }

(* basic rewrite rules *)
type base = rewrite_rule list

(* pattern instantiation *)
module Trm_map = Map.Make(String)

type 'a tmap = 'a Trm_map.t

type instantiation = trm tmap


type simple_loop = var * trm * trm * trm * trm

(* **************************Typ Construcors**************************** *)
let typ_const ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (t : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_const t; typ_attributes}

let typ_var ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (x : typvar) (tid : typid) : typ =
  {typ_annot = annot; typ_desc = Typ_var (x, tid); typ_attributes}

let typ_constr ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (x : typvar) (tid : typid) (tl : typ list) : typ =
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



(* function that fails with given error message and points location in file *)
exception TransfoError of string

let fail (loc : location) (err : string) : 'a =
  match loc with
  | None -> failwith err
  | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
     raise (TransfoError (filename ^ " start_location [" ^ (string_of_int start_row) ^": " ^ (string_of_int start_column) ^" ]" ^
     " end_location [" ^ (string_of_int end_row) ^": " ^ (string_of_int end_column) ^" ]" ^ " : " ^ err))


(* *************************** Trm constructors *************************** *)

let trm_val ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (v : value) : trm =
  {annot = annot; desc = Trm_val v; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_var ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (x : var) : trm =
  {annot = annot; desc = Trm_var x; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_array ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (tl : trm list) : trm =
  {annot = annot; desc = Trm_array tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_struct ?(annot = []) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (tl : trm list) : trm =
  {annot = annot; desc = Trm_struct tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_let ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (kind : varkind) (typed_var : typed_var) (init : trm): trm =
  {annot = annot; desc = Trm_let (kind,typed_var,init); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_let_fun ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (name : var) (ret_typ : typ) (args : typed_var list) (body : trm) : trm =
  {annot = annot; desc = Trm_let_fun (name,ret_typ,args,body); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_typedef ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (def_typ : typedef): trm =
  {annot = annot; desc = Trm_typedef def_typ; loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_if ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (tb : trm) (eb : trm) : trm =
  {annot = annot; desc = Trm_if (cond, tb, eb); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_seq ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (tl : trm list) : trm =
  {annot = annot; desc = Trm_seq tl; loc = loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_apps ?(annot = []) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(typ = None) ?(attributes = []) ?(ctx : ctx option = None) (f : trm)
  (args : trm list) : trm =
  {annot = annot; desc = Trm_apps (f, args); loc = loc; is_statement; add; typ;
   attributes; ctx}

let trm_while ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (body : trm) : trm =
  {annot = annot; desc = Trm_while (cond, body); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_for_c?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (init : trm) (cond : trm) (step : trm) (body : trm) : trm =
  {annot; desc = Trm_for_c (init, cond, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_switch ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (cases : (trm list * trm) list) : trm =
  {annot; desc = Trm_switch (cond, cases); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_abort ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (a : abort) : trm =
  {annot = annot; desc = Trm_abort a; loc = loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_labelled ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (l : label) (t : trm) : trm =
  {annot; desc = Trm_labelled (l, t); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_goto ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (l : label) : trm =
  {annot; desc = Trm_goto l; loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_null ?(annot = []) ?(loc = None) ?(ctx : ctx option = None) (_ : unit) : trm =
  trm_val ~annot ~loc ~ctx (Val_ptr 0)
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

let trm_for ?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (index : var) (direction : loop_dir) (start : trm) (stop : trm) (step : trm) (body : trm) : trm =
  {annot; desc = Trm_for (index, direction, start, stop, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_arbitrary ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
(code : string) : trm =
  {annot = annot; desc = Trm_arbitrary code; loc = loc; is_statement=false; add; typ; attributes; ctx}



let trm_omp_directive ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
(directive : directive) : trm =
  {annot = annot; desc = Trm_omp_directive directive; loc = loc; is_statement = true; add ; typ; attributes; ctx}

let trm_omp_routine ?(annot = []) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
(omp_routine : omp_routine) : trm =
  {annot = annot; desc = Trm_omp_routine omp_routine; loc = loc; is_statement = true; add ; typ; attributes; ctx}

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

let trm_map_with_terminal (is_terminal : bool) (f: bool -> trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_statement = t.is_statement in
  let typ = t.typ in
  match t.desc with
  | Trm_array tl ->
    trm_array ~annot ~loc ~add ~typ (List.map (f false) tl)
  | Trm_struct tl ->
    trm_struct ~annot ~loc ~add ~typ (List.map (f false) tl)
  | Trm_let (vk, tv, init) ->
    trm_let ~annot ~loc ~is_statement ~add vk tv (f false init)
  | Trm_let_fun (f', res, args, body) ->
    trm_let_fun ~annot ~loc ~is_statement ~add f' res args (f false body)
  | Trm_if (cond, then_, else_) ->
    let cond' = f false cond in
    let then_' = f is_terminal then_ in
    let else_' = f is_terminal else_ in
    trm_if ~annot ~loc ~add cond' then_' else_'
  | Trm_seq tl ->
    let n = List.length tl in
    let tl' = List.mapi(fun i tsub ->
      let sub_is_terminal = is_terminal && i == n-1 in
      f sub_is_terminal tsub
    ) tl in
    trm_seq tl'
  | Trm_apps (f', args) ->
    let f'' = f false f' in
    let args' = List.map (f false) args in
     (*
       warning: f'' may have different type
       -> print and reparse to have the right type
      *)
    trm_apps ~annot ~loc ~is_statement ~add ~typ f'' args'
  | Trm_while (cond, body) ->
     let cond' = f false cond in
     let body' = f false body in
     trm_while ~annot ~loc ~add cond' body'
  | Trm_for_c (init, cond, step, body) ->
     let init' = f false init in
     let cond' = f false cond in
     let step' = f false step in
     let body' = f is_terminal body in
     trm_for_c~annot ~loc ~add init' cond' step' body'
  | Trm_for (index, direction, start, stop, step, body) ->
    trm_for ~annot ~loc ~add index direction (f is_terminal start) (f is_terminal stop) (f is_terminal step) (f is_terminal body)
  | Trm_switch (cond, cases) ->
     let cond' = f false cond in
     let cases' = List.map (fun (tl, body) -> (tl, f is_terminal body)) cases in
     trm_switch ~annot ~loc ~add cond' cases'
  | Trm_abort a ->
     begin match a with
     | Ret (Some t') -> trm_abort ~annot ~loc ~add (Ret (Some (f false t')))
     (* return without value, continue, break *)
     | _ -> t
     end
  | Trm_labelled (l, body) ->
     trm_labelled ~annot ~loc ~add l (f false body)
  | _ -> t

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

(* return the list of var declarations in the list of instructions *)
let rec var_declarations (tl : trm list) : trm list =
  match tl with
  | [] -> []
  | t :: tl ->
     begin match t.desc with
     | Trm_let (_,_,_) -> t :: var_declarations tl
     | _ -> var_declarations tl
     end

(* true if x is used/defined in t *)
(* LATER: useful generalization is to get the list of variables in t *)
let is_used_var_in (t : trm) (x : var) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_var y -> y = x
    | Trm_array tl
      | Trm_struct tl
      | Trm_seq tl ->
       List.exists aux tl
    | Trm_let (_,(y, _), init) -> y = x || aux init
    | Trm_let_fun (_,_,args,body) ->
      not (List.mem x (List.map fst args)) && aux body
    | Trm_if (cond, then_, else_) -> aux cond || aux then_ || aux else_
    | Trm_apps (f, args) -> aux f || List.exists aux args
    | Trm_while (cond, body) -> aux cond || aux body
    | Trm_for_c (init, cond, step, body) ->
       aux init || aux cond || aux step || aux body
    | Trm_for (_, _, _, _, _, body) -> aux body
    | Trm_switch (cond, cases) ->
       aux cond ||
       List.exists (fun (tl, body) -> List.exists aux tl || aux body) cases
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    (* val, break, continue, return without value *)
    | _ -> false
  in
  aux t

let contains_variable (x : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_var y when y = x -> true
    | Trm_let (_, (_, _), init) -> aux init
    | Trm_apps (_, args) -> List.exists aux args
    | Trm_seq tl -> List.fold_left (fun acc t -> acc || (aux t)) false tl
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_for (_, _, _, _, _, body) -> aux body
    | _ -> false
  in aux t

(* Check if the term t contains a call to function f *)
let contains_call_to_fun (f : var) (t : trm) : bool =
  let rec aux (t : trm) : bool =
    match t.desc with
    | Trm_array tl
      | Trm_struct tl
      | Trm_seq tl ->
       List.exists aux tl
    | Trm_let (_,(_, _), init) -> aux init
    | Trm_let_fun (_,_,_,body) -> aux body
    | Trm_if (cond, then_, else_) -> aux cond || aux then_ || aux else_
    | Trm_apps ({desc = Trm_var x; _}, args) -> x = f || List.exists aux args
    | Trm_apps (f', args) -> aux f' || List.exists aux args
    | Trm_while (cond, body) -> aux cond || aux body
    | Trm_for_c (init, cond, step, body) ->
       aux init || aux cond || aux step || aux body
    | Trm_for (_, _, _, _, _, body) -> aux body
    | Trm_switch (cond, cases) ->
       aux cond ||
       List.exists (fun (tl, body) -> List.exists aux tl || aux body) cases
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    (* val, var, break, continue, return without value, goto *)
    | _ -> false
  in
  aux t

(* assumption: f is called only once in t*)
let fun_call_args (f : var) (t : trm) : trm list =
  let rec aux (t : trm) : trm list =
    match t.desc with
    | Trm_array tl
      | Trm_struct tl
      | Trm_seq tl ->
       List.flatten (List.map aux tl)
    | Trm_let (_,(_, _), init) -> aux init
    | Trm_let_fun (_,_,_,body) -> aux body
    | Trm_if (cond, then_, else_) -> aux cond ++ aux then_ ++ aux else_
    | Trm_apps ({desc = Trm_var x; _}, args) when x = f -> args
    | Trm_apps (f', args) -> aux f' ++ (List.flatten (List.map aux args))
    | Trm_while (cond, body) -> aux cond ++ aux body
    | Trm_for_c (init, cond, step, body) ->
       aux init ++ aux cond ++ aux step ++ aux body
    | Trm_for (_, _, _, _, _,body) -> aux body
    | Trm_switch (cond, cases) ->
       aux cond ++
       List.flatten
         (List.map
            (fun (tl, body) -> List.flatten (List.map aux tl) ++ aux body)
            cases
         )
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    (* val, var, break, continue, return without value, goto *)
    | _ -> []
  in
  aux t

(* return the name of the declared object *)
let decl_name (t : trm) : var =
  match t.desc with
  | Trm_let (_,(x,_),_) -> x
  (* take into account heap allocated variables *)
  | Trm_let_fun (f, _, _, _) -> f
  | Trm_typedef td -> td.typdef_tconstr
  | _ -> fail t.loc "decl_name: expected declaration"

(* return the initialisation in the declaration *)
let decl_init_val (t : trm) : trm =
  match t.desc with
  | Trm_let (_,(_,_),init) -> init
  | _ -> fail t.loc "decl_init_val: expected variable declaration"

(* return the type of the declared var *)
let var_decl_type (t : trm) : typ =
  match t.desc with
  | Trm_let (_,(_,ty),_) -> ty
  | _ -> fail t.loc "var_decl_type: expected var declaration"

(* true if t is the declaration of a heap allocated variable *)
let is_heap_alloc (t : trm) : bool =
  match t.desc with
  | Trm_let (vk,(_,_),_) ->
      begin match vk with
      | Var_mutable -> true
      | _ -> false
      end
  | _ -> fail t.loc "is_heap_alloc: expected var declaration"

(* LATER: move these functions to for_c.ml specialized for handling the complex for loops *)
(* return the name of the index of the for loop *)
let for_loop_index (t : trm) : var =
  match t.desc with
  | Trm_for(index, _, _, _, _, _) -> index
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
     | _ -> decl_name init
     end
  | _ -> fail t.loc "for_loop_index: expected for loop"

let for_loop_direction (t : trm) : loop_dir =
  match t.desc with
  | Trm_for_c (_, cond, _, _) ->
    begin match cond.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_lt)); _}, _) -> DirUp
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_gt)); _}, _) -> DirDown
     | _ -> fail cond.loc "for_loop_bound: bad for loop condition"
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
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_inc)); _}, _) ->
        trm_lit (Lit_int 1)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_dec)); _}, _) ->
        (*
          choose this instead of trm_lit (Lit_int (- 1)) for the
          for_loop_nb_iter function
         *)
        trm_apps (trm_unop Unop_opp) [trm_lit (Lit_int 1)]
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

(*
  return the number of iterations of a for loop
 *)
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


let for_loop_body_trms (t : trm) : trm list =
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


(* Count the number of goto instructions targeting a given label, inside a term t *)
let nb_goto (l : label) (t : trm) : int =
  let add (n : int) (m : int) : int = n + m in
  let sum (il : int list) : int = List.fold_left add 0 il in
  let rec aux (t : trm) : int =
    match t.desc with
    | Trm_array tl
      | Trm_struct tl
      | Trm_seq tl ->
       sum (List.map aux tl)
    | Trm_let (_,(_, _), init) -> aux init
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_if (cond, then_, else_) -> aux cond + aux then_ + aux else_
    | Trm_apps (f, args) -> aux f + sum (List.map aux args)
    | Trm_while (cond, body) -> aux cond + aux body
    | Trm_for_c (init, cond, step, body) ->
       aux init + aux cond + aux step + aux body
    | Trm_for (_, _, _, _, _, body) -> aux body
    | Trm_switch (cond, cases) ->
       aux cond +
       sum
         (List.map (fun (tl, body) -> sum (List.map aux tl) + aux body)
            cases)
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    | Trm_goto l' when l = l' -> 1
    (* val, var, break, continue, return without value, goto other label *)
    | _ -> 0
  in
  aux t


let is_generated_star (ty : typ) : bool =
  List.mem GeneratedStar ty.typ_attributes

let same_sizes (sz1 : size) (sz2 : size) : bool =
 match sz1, sz2 with
 | Undefined, Undefined -> true
 | Const i1, Const i2 -> i1 = i2
 | Trm _, Trm _ -> false
 | _, _ -> false

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
  | Typ_array (typa1, size1), Typ_array (typa2, size2) -> (same_types typa1 typa2) && (same_sizes size1 size2)
  | _, _ -> false
  )

let is_simple_loop_component (t : trm) : bool =
  match t.desc with
  | Trm_apps(f,_) when f.desc = Trm_val(Val_prim (Prim_unop (Unop_get))) -> true
  | Trm_var _ -> true
  | Trm_val (Val_lit (Lit_int _)) -> true
  | Trm_apps _ -> true
  | _ -> false


(* Check if the loop t is simple or not, if is return its simplified ast else return the current ast *)
let trm_for_of_trm_for_c (t : trm) : trm =
  let body = begin match t.desc with
  | Trm_for_c (_, _, _, body) -> body
  | _ -> fail t.loc "trm_for_of_trm_for: expected a loop"
  end
  in
  let index = for_loop_index t in
  let direction = for_loop_direction t in
  let start = for_loop_init t in
  let stop = for_loop_bound t in
  let step = for_loop_step t in
  (* DEBUG *)
  (* printf "is simple loop start %b\n" (is_simple_loop_component start);
  printf "is simple loop stop %b\n" (is_simple_loop_component stop);
  printf "is simple loop step %b\n" (is_simple_loop_component step); *)

  let is_simple_loop =
      (is_simple_loop_component start)
    && (is_simple_loop_component stop)
    && (is_simple_loop_component step) in

  if is_simple_loop
    then trm_for ~loc:t.loc index direction start stop step body
    else t

type typ_kind =
  | Typ_kind_undefined
  | Typ_kind_reference
  | Typ_kind_array
  | Typ_kind_sum
  | Typ_kind_prod
  | Typ_kind_basic of typ_desc
  | Typ_kind_fun
  | Typ_kind_var


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

let trm_for_to_trm_for_c?(annot = []) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (index : var) (direction : loop_dir) (start : trm) (stop : trm) (step : trm) (body : trm) : trm =
  let init = trm_let Var_mutable (index, typ_ptr ~typ_attributes:[GeneratedStar] Ptr_kind_mut (typ_int ())) (trm_apps (trm_prim ~loc:start.loc (Prim_new (typ_int ()))) [start]) in
  let cond = begin match direction with
    | DirUp -> (trm_apps (trm_binop Binop_lt)
      [trm_apps ~annot:[Mutable_var_get]
        (trm_unop Unop_get) [trm_var index];stop])
    | DirDown -> (trm_apps (trm_binop Binop_lt)
      [trm_apps ~annot:[Mutable_var_get]
        (trm_unop Unop_get) [trm_var index];stop])
    end
    in

  let step =
    begin match direction with
    | DirUp ->
        begin match step.desc with
        | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_inc) [trm_var index]
        | _ ->
          trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_add)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
        end
    | DirDown ->
        begin match step.desc with
        | Trm_val (Val_lit (Lit_int 1)) -> trm_apps (trm_unop Unop_dec) [trm_var index]
        | _ ->
          trm_set (trm_var index ) ~annot:[App_and_set](trm_apps (trm_binop Binop_sub)
          [
            trm_var index;
            trm_apps ~annot:[Mutable_var_get] (trm_unop Unop_get) [step]])
        end
    end
    in
  trm_for_c ~annot ~loc ~add ~attributes ~ctx init cond step body


let get_inner_ptr_type (ty : typ) : typ =
  match ty.typ_desc with
  | Typ_ptr {inner_typ = ty;_} -> ty
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

type tile_bound = TileBoundMin | TileBoundAnd | TileBoundDivides

let get_sequence_trms (t : trm) : trm list =
  match t.desc with
  | Trm_seq tl -> tl
  | _ -> fail t.loc "get_sequence_trms: expected a sequence"



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


let trm_seq_no_brace (tl : trm list) : trm=
    trm_seq ~annot:[No_braces (Nobrace.current())] tl

let get_decorators (t : trm) : (string * string) =
  let rec aux l = match l with 
  | [] -> fail t.loc "get_decorators: empty annotation list"
  | hd :: tl ->
    begin match hd with 
    | Highlight (l, r) -> (l, r)
    | _ -> aux tl
    end
   in aux t.annot

let get_nobrace_id (t : trm) : int = 
  let rec aux l = match l with
  | [] -> -1
  | hd :: tl ->
    begin match hd with 
    | No_braces i -> i
    | _ -> aux tl
    end in 
    aux t.annot


(* This type is used for variable renaming, the uer can choose between renaming all the variables 
    on one block, by giving the prefix to add or he can also  give the list of variable to 
    be renamed together with their new name.
*)
type rename = | Postfix of string | Rename_list of (var * var) list

let get_initialization_trm (t : trm) : trm = 
  match t.desc with 
  | Trm_let (_, (_, _), init) -> 
      begin match init.desc with 
      | Trm_apps(f,[base]) ->
        begin match f.desc with 
        | Trm_val (Val_prim (Prim_new _)) -> base
        | _ -> init
        end
      | _-> init
      end
  | _ -> fail t.loc "get_initialization_trm: expected a variable declaration"

let remove_highlight (t_annot : trm_annot list) : trm_annot list =
  let rec aux l = match l with 
  | [] -> []
  | x :: xs ->
    begin match x with 
    | Highlight _ -> xs
    | _ -> x :: aux xs
    end
  in aux t_annot


let rec clean_highlights (t : trm) : trm =
  match t.desc with 
  | Trm_val _ -> {t with annot = remove_highlight t.annot}
  | Trm_var _ -> {t with annot = remove_highlight t.annot}
  | Trm_array tl -> {t with annot = remove_highlight t.annot; desc = Trm_array (List.map clean_highlights tl)}
  | Trm_struct tl -> {t with annot = remove_highlight t.annot; desc = Trm_struct (List.map clean_highlights tl)}
  | Trm_let (vk, (x, tx), init) -> {t with annot = remove_highlight t.annot; desc = Trm_let (vk, (x, tx), (clean_highlights init))}
  | Trm_let_fun (v, ty, ty_v, body) -> {t with annot = remove_highlight t.annot; desc = Trm_let_fun (v, ty, ty_v, clean_highlights body)}
  | Trm_typedef _ -> {t with annot = remove_highlight t.annot}
  | Trm_if (cond, then_, else_) -> {t with annot = remove_highlight t.annot; desc = Trm_if (clean_highlights cond, clean_highlights then_, clean_highlights else_)}
  | Trm_seq tl -> {t with annot = remove_highlight t.annot; desc = Trm_seq (List.map clean_highlights tl)}
  | Trm_apps (f, args) -> {t with annot = remove_highlight t.annot; desc = Trm_apps (clean_highlights f, List.map clean_highlights args)}
  | Trm_while (cond, body) -> {t with annot = remove_highlight t.annot; desc = Trm_while (clean_highlights cond, clean_highlights body)}
  | Trm_for (index, direction, start, stop, step, body) -> {t with annot = remove_highlight t.annot; desc = Trm_for (index, direction, clean_highlights start, clean_highlights stop, clean_highlights step, clean_highlights body)}
  | Trm_for_c (init, cond, step, body) -> {t with annot = remove_highlight t.annot; desc = Trm_for_c (clean_highlights init, clean_highlights cond, clean_highlights step, clean_highlights body)}
  | Trm_switch _-> {t with annot = remove_highlight t.annot}
  | Trm_abort _ -> {t with annot = remove_highlight t.annot}
  | Trm_labelled (l, t1) -> {t with annot = remove_highlight t.annot; desc = Trm_labelled (l, clean_highlights t1)}
  | Trm_goto _ -> {t with annot = remove_highlight t.annot}
  | Trm_arbitrary _ -> fail t.loc "clean_highlights: trm_arbitrary should never appear on the ast"
  | Trm_omp_directive _ -> {t with annot = remove_highlight t.annot}
  | Trm_omp_routine _ -> {t with annot = remove_highlight t.annot}
  

(* type instantiation = trm varmap *)

(* Check if rule is applicable *)
(* let is_rule_applicable (t : trm) (p : pat) : bool =
  *)

(* Rewrite rule transformation  *)
(* let rewrite (pl : target) (rule : base)  *)

(* tile bound type is used  as a parameter to the tile transformation so that the transformation can
    decide what kind of loop bound it should use
*)