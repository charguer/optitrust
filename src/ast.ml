open Tools

(* file locations: filename, line number *)
type location = (string * int * int * int * int) option

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

(* constructor name (for enum and algebraic datatypes) *)
type constr = string

(* array sizes *)
type size =
  | Undefined (* t[] *)
  | Const of int (* t[3] *)
  | Trm of trm (* t[2*nb] *)

(* types of expressions *)
and typ_desc =
  | Typ_const of typ (* e.g. [const int *] is a pointer on a [const int] type. *)
  | Typ_var of typvar (* e.g. ['a] in the type ['a -> 'a] *)
  | Typ_constr of typvar * typid * typ list (* e.g. [int list] or [vect] *)
  | Typ_unit (* void *)
  | Typ_int
  | Typ_float
  | Typ_double
  | Typ_bool
  | Typ_char
  | Typ_ptr of typ (* "int*" *)
  | Typ_array of typ * size (* int[3], or int[], or int[2*n] *)
  | Typ_fun of (typ list) * typ  (* int f(int x, int y) *)

and typ_annot =
  | Unsigned
  | Long
  | Short

and typ = {
  typ_desc : typ_desc;
  typ_annot : typ_annot list;
  typ_attributes : attribute list }
  (* IN THE FUTURE
  ty_env : env; --> tells you for every type what is its definition
  *)

and typedef = { (* e.g. [type ('a,'b) t = ...] *)
  typdef_typid : typid; (* the unique id associated with the type [t] *)
  typdef_tconstr : typconstr; (* the name [t] *)
  typdef_vars : typvars; (* the list containing the names ['a] and ['b];
    [typedef_vars] is always the empty list in C code without templates *)
  typdef_body : typdef_body; } (* the body of the definition, i.e. the description of [...] *)

and typdef_body =
  | Typdef_alias of typ (* for abbreviations, e.g. [type 'a t = ('a * 'a) list)] or [typdef vect t] *)
  | Typdef_prod of (label * typ) list (* for records / struct, e.g. [type 'a t = { f : 'a; g : int } *)
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
  | Unop_struct_access of field (* the ".f" operator TODO CHECK *)
  | Unop_struct_get of field (* the "->f" operator TODO CHECK *)
  | Unop_cast of typ (* cast operator towards the specified type *)

and binary_op =
  | Binop_set (* type annotation?    lvalue = rvalue *)
  | Binop_array_access (* TODO DOCUMENT *)
  | Binop_array_get
  | Binop_eq
  | Binop_neq
  | Binop_sub
  | Binop_addtypedef_env_add
  | Binop_mul
  | Binop_mod^ string tv ^ comma ^ st
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
  (* The constructors below never appear in source code;
     these are values that can only be constructed during the program execution,
     and thus useful only for carrying out proofs about the program Generic *)
  | Val_ptr of loc * accesses
  | Val_array of value list
  | Val_struct of value list
  (* LATER: add functions, which are also values that can be created at execution time *)

(* annotations are used to decorate this AST when it is built from the
   Clang AST in such a way to be able to print back the AST like the original C code.
   *)
and trm_annot =
  (* used to print back a c++ program *)
  | No_braces
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

(* symbols to add while printing a C++ program. TODO: document *)
and print_addition =
  | Add_address_of_operator
  | Add_star_operator

(* We only need to support two specific attributes for the time being *)
and attribute =
  | Identifier of var
  | Aligned of trm

(*
  annotated terms
  is_statement: true if the trm is an instruction in a seq
 *)
and trm =
 { annot : trm_annot option;
   desc : trm_desc;
   loc : location;
   is_statement : bool; (* TODO : generalize to trm_kind *)
   add : print_addition list; (* TODO: find better name *)
   typ : typ option;
   ctx : ctx option;
   attributes : attribute list }

(* A [typ_env] stores all the information about types, labels, constructors, etc. *)
(* [ctx_tvar] is useful for interpreting types that are provided in the user scripts *)
and ctx = {
  ctx_tvar : typ varmap; (* from [var] to [typ], i.e. giving the type of program variables *)
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
      id0 -->   Typdef_struct ...
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
  | Trm_for of trm * trm * trm * trm
  (*
    Trm_for (e0, e1, e2, e3) =
    for (e0; e1; e2) {e3;}
   *)
   (*Trm_typedefrm list * trm) list)
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
  | Trm_abort of abort (* return or break or continue *)
  | Trm_labelled of label * trm (* foo: st *)
  | Trm_goto of label
  | Trm_decoration of string * trm * string
  | Trm_any of trm

and varkind =
  | Var_immutable
  | Var_mutable (* [Var_mutable] means that we had a declaration of a non-const stack-allocated variable. *)

(* ways of aborting *)
and abort =
  | Ret of trm option (* return;  or return 3; *)
  | Break (* LATER: could have label option *)
  | Continue (* LATER: could have label option *)

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

(* **************************Typ Construcors**************************** *)
let typ_const ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (t : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_const t; typ_attributes}

let typ_var ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (x : typvar) : typ =
  {typ_annot = annot; typ_desc = Typ_var x; typ_attributes}

let typ_constr ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (x : typvar) (tid : typid) (tl : typ list) : typ =
  {typ_annot = annot; typ_desc = Typ_constr (x, tid, tl); typ_attributes}

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
  (t : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_ptr t; typ_attributes}

let typ_array ?(annot : typ_annot list = []) ?(typ_attributes = []) (t : typ)
  (s : size) : typ =
  {typ_annot = annot; typ_desc = Typ_array (t, s); typ_attributes}

let typ_struct ?(annot : typ_annot list = []) ?(typ_attributes = [])
   (fields : fields)(typ_field : typ varmap) (typ_name : typvar) : typ =
  {typ_annot = annot; typ_desc = Typ_struct (fields,typ_field, typ_name); typ_attributes}

let typ_fun ?(annot : typ_annot list = []) ?(typ_attributes = [])
  (args : typ list) (res : typ) : typ =
  {typ_annot = annot; typ_desc = Typ_fun (args, res); typ_attributes}

(* *************************** Trm constructors *************************** *)

let trm_val ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (v : value) : trm =
  {annot = annot; desc = Trm_val v; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_var ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (x : var) : trm =
  {annot = annot; desc = Trm_var x; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_array ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (tl : trm list) : trm =
  {annot = annot; desc = Trm_array tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_struct ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) ?(ctx : ctx option = None) (tl : trm list) : trm =
  {annot = annot; desc = Trm_struct tl; loc = loc; is_statement = false; add; typ;
   attributes; ctx}

let trm_let ?(annot = None) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (kind : varkind) (typed_var:typed_var) (init : trm): trm =
  {annot = annot; desc = Trm_let (kind,typed_var,init); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_let_fun ?(annot = None) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (name : var) (ret_typ : typ) (args : typed_var list) (body : trm) : trm =
  {annot = annot; desc = Trm_let_fun (name,ret_typ,args,body); loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_typedef ?(annot = None) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(attributes = []) ?(ctx : ctx option = None) (def_typ : typedef): trm =
  {annot = annot; desc = Trm_typedef def_typ; loc = loc; is_statement; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_if ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (tb : trm) (eb : trm) : trm =
  {annot = annot; desc = Trm_if (cond, tb, eb); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_seq ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (tl : trm list) : trm =
  {annot = annot; desc = Trm_seq tl; loc = loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_apps ?(annot = None) ?(loc = None) ?(is_statement : bool = false)
  ?(add = []) ?(typ = None) ?(attributes = []) ?(ctx : ctx option = None) (f : trm)
  (args : trm list) : trm =
  {annot = annot; desc = Trm_apps (f, args); loc = loc; is_statement; add; typ;
   attributes; ctx}

let trm_while ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (body : trm) : trm =
  {annot = annot; desc = Trm_while (cond, body); loc = loc; is_statement = false;
   add; typ = Some (typ_unit ()); attributes; ctx}

let trm_for ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (init : trm) (cond : trm) (step : trm) (body : trm) : trm =
  {annot; desc = Trm_for (init, cond, step, body); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_switch ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (cond : trm) (cases : (trm list * trm) list) : trm =
  {annot; desc = Trm_switch (cond, cases); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_abort ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (a : abort) : trm =
  {annot = annot; desc = Trm_abort a; loc = loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_labelled ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (l : label) (t : trm) : trm =
  {annot; desc = Trm_labelled (l, t); loc; is_statement = false; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_goto ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (l : label) : trm =
  {annot; desc = Trm_goto l; loc; is_statement = true; add;
   typ = Some (typ_unit ()); attributes; ctx}

let trm_decoration ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = []) ?(ctx : ctx option = None)
  (left : string) (right : string) (t : trm) : trm =
  {annot; desc = Trm_decoration (left, t, right); loc; is_statement = false; add;
  typ = Some (typ_unit ()); attributes; ctx}

let trm_null ?(annot = None) ?(loc = None) (_ : unit) : trm =
  trm_val ~annot ~loc (Val_ptr (0, []))
(*
   no type for primitives and operators:
   we are only interested in the result of their application
 *)

let trm_unop ?(annot = None) ?(loc = None) ?(add = []) (p : unary_op) : trm =
  trm_val ~annot ~loc ~add (Val_prim (Prim_unop p))

let trm_binop ?(annot = None) ?(loc = None) ?(add = []) (p : binary_op) : trm =
  trm_val ~annot:annot ~loc ~add (Val_prim (Prim_binop p))

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

let trm_lit ?(annot = None) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (l : lit) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx ~typ:(typ_of_lit l) (Val_lit l)

let trm_prim ?(annot = None) ?(loc = None) ?(add = []) ?(ctx : ctx option = None) (p : prim) : trm =
  trm_val ~annot:annot ~loc ~add ~ctx (Val_prim p)

let trm_set ?(annot = None) ?(loc = None) ?(is_statement : bool = false) ?(add = []) ?(ctx : ctx option = None)
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ~loc ~is_statement ~add ~ctx ~typ:(Some (typ_unit ()))
    (trm_binop Binop_set) [t1; t2]

let trm_any ?(annot = None) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = []) ?(ctx : ctx option = None)
(t : trm) : trm =
  {annot = annot; desc = Trm_any t; loc = loc; is_statement=false; add; typ; attributes; ctx}


let is_included (t : trm) : bool =
  match t.annot with
  | Some (Include _) -> true
  | _ -> false

(* function that fails with given error message and points location in file *)
exception TransfoError of string

let fail (loc : location) (err : string) : 'a =
  match loc with
  | None -> failwith err
  | Some (filename, start_row,end_row,start_column,end_column) ->
     raise (TransfoError (filename ^ " start_location [" ^ (string_of_int start_row) ^": " ^ (string_of_int start_column) ^" ]" ^
     " end_location [" ^ (string_of_int end_row) ^": " ^ (string_of_int end_column) ^" ]" ^ " : " ^ err))

(*
  compute a function that prints information related to some location in file
  only if the verbose flag is activated
 *)
let print_info (loc : location) : ('a, out_channel, unit) format -> 'a =
  if !Flags.verbose then
    match loc with
    | None -> Printf.printf
    | Some (filename, start_row,end_row,start_column,end_column) ->
       Printf.kfprintf Printf.fprintf stdout ("<%s> from <%d>,<%d> to   <%d>,<%d>") filename start_row start_column end_row end_column
  else
    Printf.ifprintf stdout

(* concrete accesses in a trm *)
type trm_access =
  | Array_access of trm (* operator [i] *)
  | Struct_access of field (* operator .f *)

(* TODO: add documentation
  compute_accesses t = (base, access list) where the succession of accesses
  applied to base gives t
  the list is nil if t is not a succession of accesses
 *)
let rec compute_accesses (t : trm) : trm * (trm_access list) =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f))); _},
              [t']) ->
     let (base, al) = compute_accesses t' in
     (base, Struct_access f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f))); _},
              [t']) ->
     let (base, al) = compute_accesses t' in
     (base, Struct_access f :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_access)); _},
              [t'; i]) ->
     let (base, al) = compute_accesses t' in
     (base, Array_access i :: al)
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get)); _},
              [t'; i]) ->
     let (base, al) = compute_accesses t' in
     (base, Array_access i :: al)
  | _ -> (t, [])



(* map f to t's subterms *)
let trm_map (f : trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_statement = t.is_statement in
  let typ = t.typ in
  match t.desc with
  | Trm_array tl ->
     trm_array ~annot ~loc ~add ~typ (List.map f tl)
  | Trm_struct tl ->
     trm_struct ~annot ~loc ~add ~typ (List.map f tl)
  | Trm_let (vk,tv,init) ->
    trm_let ~annot ~loc ~is_statement ~add  vk tv init
  | Trm_let_fun (f',res,args,body) ->
    trm_let_fun ~annot ~loc ~is_statement ~add f' res args (f body)
  | Trm_if (cond, then_, else_) ->
     let cond' = f cond in
     let then_' = f then_ in
     let else_' = f else_ in
     trm_if ~annot ~loc ~add cond' then_' else_'
  | Trm_seq tl ->
     trm_seq ~annot ~loc ~add (List.map f tl)
  | Trm_apps (f', args) ->
     let f'' = f f' in
     let args' = List.map f args in
     (*
       warning: f'' may have different type
       -> print and reparse to have the right type
      *)
     trm_apps ~annot ~loc ~is_statement ~add ~typ f'' args'
  | Trm_while (cond, body) ->
     let cond' = f cond in
     let body' = f body in
     trm_while ~annot ~loc ~add cond' body'
  | Trm_for (init, cond, step, body) ->
     let init' = f init in
     let cond' = f cond in
     let step' = f step in
     let body' = f body in
     trm_for ~annot ~loc ~add init' cond' step' body'
  | Trm_switch (cond, cases) ->
     let cond' = f cond in
     let cases' = List.map (fun (tl, body) -> (tl, f body)) cases in
     trm_switch ~annot ~loc ~add cond' cases'
  | Trm_abort a ->
     begin match a with
     | Ret (Some t') -> trm_abort ~annot ~loc ~add (Ret (Some (f t')))
     (* return without value, continue, break *)
     | _ -> t
     end
  | Trm_labelled (l, body) ->
     trm_labelled ~annot ~loc ~add l (f body)
  (* val, var *)
  | Trm_decoration (left, body, right) ->
    trm_decoration ~annot ~loc ~add left right (f body)
  | Trm_any t ->
    trm_any ~annot ~loc ~add (f t)
  | _ -> t

(* same as trm_map for types *)
let typ_map (f : typ -> typ) (ty : typ) : typ =
  let annot = ty.typ_annot in
  let typ_attributes = ty.typ_attributes in
  match ty.typ_desc with
  | Typ_ptr ty -> typ_ptr ~annot ~typ_attributes (f ty)
  | Typ_array (ty, n) -> typ_array ~annot ~typ_attributes (f ty) n
  | Typ_struct (tlist,tmap, x) ->
     typ_struct ~annot ~typ_attributes tlist (String_map.map f tmap) x
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
    | Trm_for (init, cond, step, body) ->
       aux init || aux cond || aux step || aux body
    | Trm_switch (cond, cases) ->
       aux cond ||
       List.exists (fun (tl, body) -> List.exists aux tl || aux body) cases
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    (* val, break, continue, return without value *)
    | Trm_decoration (_,t,_) -> aux t
    | _ -> false
  in
  aux t

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
    | Trm_for (init, cond, step, body) ->
       aux init || aux cond || aux step || aux body
    | Trm_switch (cond, cases) ->
       aux cond ||
       List.exists (fun (tl, body) -> List.exists aux tl || aux body) cases
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    | Trm_decoration (_,t,_) -> aux t
    (* val, var, break, continue, return without value, goto *)
    | _ -> false
  in
  aux t

(* assumption: f is called only once in t
  TODO check if this is indeed capturing the list of subterms that corresponds
  to arguments of calls to the function f *)
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
    | Trm_for (init, cond, step, body) ->
       aux init ++ aux cond ++ aux step ++ aux body
    | Trm_switch (cond, cases) ->
       aux cond ++
       List.flatten
         (List.map
            (fun (tl, body) -> List.flatten (List.map aux tl) ++ aux body)
            cases
         )
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    | Trm_decoration (_, t, _) -> aux t
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
  | Trm_typedef ty ->
    begin match ty with
    | Typedef_abbrev (ty,_) -> ty
    | Typedef_enum (ty, _) -> ty
    end
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
  | Trm_for (init, _, _, _) ->
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

(* return the initial value of the loop index *)
let for_loop_init (t : trm) : trm =
  match t.desc with
  | Trm_for (init, _, _, _) ->
     (*
       covered cases:
       - for (i = n; …)
       - for (int i = n; …)
      *)
     begin match init.desc with
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                 [_; n]) ->
        n
     | Trm_let (_,(_, _), init) -> init
     | _ -> fail init.loc "for_loop_init: bad for loop initialisation"
     end
  | _ -> fail t.loc "for_loop_init: expected for loop"

(* return the lower bound of the for loop *)
let for_loop_bound (t : trm) : trm =
  match t.desc with
  | Trm_for (_, cond, _, _) ->
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
  | Trm_for (_, _, step, _) ->
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

(*
  aliased_type X takes as argument the description of a file
  (that is a toplevel sequence), and it returns the type ty
  associated via a "typedef ty X" if there is one such definition
  LATER: check if this is subsumed by the environments carried by type variables
 *)
let rec aliased_type (x : typvar) (t : trm) : typ option =
  match t.desc with
  | Trm_typedef ty ->
    begin match ty with
    | Typedef_abbrev (y,ty) when y = x -> Some ty
    | _ -> None
    end
  | Trm_seq tl ->
     List.fold_left
       (fun tyo t ->
         match tyo with
         | Some _ -> tyo
         | None -> aliased_type x t
       )
       None
       tl
  | _ -> None

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
    | Trm_for (init, cond, step, body) ->
       aux init + aux cond + aux step + aux body
    | Trm_switch (cond, cases) ->
       aux cond +
       sum
         (List.map (fun (tl, body) -> sum (List.map aux tl) + aux body)
            cases)
    | Trm_abort (Ret (Some t)) -> aux t
    | Trm_labelled (_, t) -> aux t
    | Trm_decoration (_, t, _) -> aux t
    | Trm_goto l' when l = l' -> 1
    (* val, var, break, continue, return without value, goto other label *)
    | _ -> 0
  in
  aux t

(*
  point p = { 3, 4 }
->
  trm_seq ~annot:[init_instr] [
    point* p = new point;
    trm_set (p, [access "x"]) 3;
    trm_set (p, [access "y"]) 4
    ]


   for (e0; e1; e2) { ebody }

   =>

   ~annot:EncodedFor (e0; while (e1) { ebody; e2 })
    (prt<int> i = new int; i = i0); while ...
 TODO :find out if this one is used.
*)
(* let string_of_list ?(sep:string=";") ?(bounds:string list = ["[";"]"])(l : string list) : string =
  let rec aux = function
    | [] -> ""
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^ sep ^ " " ^ aux (s2 :: sl)
  in
  (List.nth bounds 0) ^ aux l ^ (List.nth bounds 1) *)

