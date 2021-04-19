(* file locations: filename, line number *)
type location = (string * int * int * int * int) option

(* memory locations *)
type loc = int

(* variables *)
type var = string

(* type variables *)
type typvar = var

(* struct fields and maps describing struct *)
type field = string

(* struct fields as a list of fields *)
type fields = field list

module Field_map = Map.Make(String)

type 'a fmap = 'a Field_map.t

(* labels *)
type label = string

(* patterns *)
type pat = trm 

(* rewrite_rule *)
type rewrite_rule = {name : string; source : pat; target : string}

(* basic rewrite rules *)
type base = rewrite_rule list 

(* pattern instantiation *)
module Trm_map = Map.Make(String) 

type 'a tmap = 'a Trm_map.t

type instantiation = trm tmap 

(* array sizes *)
type size =
  | Undefined (* t[] *)
  | Const of int (* t[3] *)
  | Trm of trm (* t[2*nb] *)

(* types of expressions *)
and typ_desc =
  | Typ_var of typvar (* int x *)
  | Typ_unit (* void *)
  | Typ_int
  | Typ_float
  | Typ_double
  | Typ_bool
  | Typ_char
  | Typ_ptr of typ (* "int*" *)
  | Typ_array of typ * size (* int[3] *)
  | Typ_struct of fields * (typ fmap) * typvar (* typedef { x: int, y:double } point *)
  | Typ_fun of (typ list) * typ  (* int f(int x, int y) *)

and typ_annot =
  | Unsigned
  | Long
  | Short

and typ = {ty_desc : typ_desc;
           ty_annot : typ_annot list;
           ty_attributes : attribute list}

and typed_var = var * typ

(* accesses in arrays/structures *) (* TODO: this does not appear to be used *)
and access =
  | Access_array of typ * int  (* the "[i]" operator, with result of type T *)
  | Access_field of typ * access (* the ".f" operator, with result of type T *)

and accesses = access list (* e.g. the "[i][j].f" operator *)

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
  | Unop_delete of bool (* "Unop_delete false" is "delete",
                           "Unop_delete true" is "delete[]" TODO CHANGE *)
  | Unop_cast of typ (* cast operator towards the specified type *)

and binary_op =
  | Binop_set (* type annotation?    lvalue = rvalue *)
  | Binop_array_access (* TODO DOCUMENT *)
  | Binop_array_get
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
     and thus useful only for carrying out proofs about the program transformations *)
  | Val_ptr of loc * accesses
  | Val_array of value list
  | Val_struct of value list
  (* LATER: add functions, which are also values that can be created at execution time *)

(* annotations are used to decorate this AST when it is built from the
   Clang AST in such a way to be able to print back the AST like the original C code.
   *)
and trm_annot =
  (* for declaration and elimination of heap allocated variables
    + dereferencing
   *)
  | Heap_allocated
  (* inside declaration of heap allocated variables *)
  | Initialisation_instruction
  (* for sequences containing elimination of heap allocated variables *)
  | Delete_instructions
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
  is_instr: true if the trm is an instruction in a seq
 *)
and trm =
 { annot : trm_annot option;
   desc : trm_desc;
   loc : location;
   is_instr : bool; (* TODO statement or expression? *)
   add : print_addition list; (* TODO: find better name *)
   typ : typ option; (* typ should be available from the AST that comes from Clang *)
   attributes : attribute list }

and trm_desc =
  | Trm_val of value
  | Trm_var of var
  | Trm_array of trm list (* { 0, 3, 5} as an array *)
  | Trm_struct of trm list (* { 4, 5.3 } as a record *)
  | Trm_decl of def (* variable or function or type definition *)
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
  | Trm_switch of trm * ((trm list * trm) list)
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
(* declarations *)
and def =
  | Def_var of typed_var * trm (* int x = t *)
  | Def_fun of var * typ * (typed_var list) * trm (* int f(int x ,double y) { st } *)
  | Def_typ of typvar * typ (* type abbreviation e.g. "typedef int** T" *)
  | Def_enum of typvar * ((var * (trm option)) list) (* typedef enum { X, Y } T  --TODO: check syntax*)

(* ways of aborting *)
and abort =
  | Ret of trm option (* return;  or return 3; *)
  | Break
  | Continue 

let typ_var ?(annot : typ_annot list = []) ?(ty_attributes = [])
  (x : typvar) : typ =
  {ty_annot = annot; ty_desc = Typ_var x; ty_attributes}
let typ_unit ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_unit; ty_attributes}
let typ_int ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_int; ty_attributes}
let typ_float ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_float; ty_attributes}
let typ_double ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_double; ty_attributes}
let typ_bool ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_bool; ty_attributes}
let typ_char ?(annot : typ_annot list = []) ?(ty_attributes = []) () : typ =
  {ty_annot = annot; ty_desc = Typ_char; ty_attributes}
let typ_ptr ?(annot : typ_annot list = []) ?(ty_attributes = [])
  (t : typ) : typ =
  {ty_annot = annot; ty_desc = Typ_ptr t; ty_attributes}
let typ_array ?(annot : typ_annot list = []) ?(ty_attributes = []) (t : typ)
  (s : size) : typ =
  {ty_annot = annot; ty_desc = Typ_array (t, s); ty_attributes}
let typ_struct ?(annot : typ_annot list = []) ?(ty_attributes = [])
   (fields : fields)(typ_field : typ fmap) (typ_name : typvar) : typ =
  {ty_annot = annot; ty_desc = Typ_struct (fields,typ_field, typ_name); ty_attributes}
let typ_fun ?(annot : typ_annot list = []) ?(ty_attributes = [])
  (args : typ list) (res : typ) : typ =
  {ty_annot = annot; ty_desc = Typ_fun (args, res); ty_attributes}

let trm_val ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) (v : value) : trm =
  {annot = annot; desc = Trm_val v; loc = loc; is_instr = false; add; typ;
   attributes}
let trm_var ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) (x : var) : trm =
  {annot = annot; desc = Trm_var x; loc = loc; is_instr = false; add; typ;
   attributes}
let trm_array ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) (tl : trm list) : trm =
  {annot = annot; desc = Trm_array tl; loc = loc; is_instr = false; add; typ;
   attributes}                                                                                                                                                                                                                                                           
let trm_struct ?(annot = None) ?(loc = None) ?(add = []) ?(typ = None)
  ?(attributes = []) (tl : trm list) : trm =
  {annot = annot; desc = Trm_struct tl; loc = loc; is_instr = false; add; typ;
   attributes}
let trm_decl ?(annot = None) ?(loc = None) ?(is_instr : bool = false)
  ?(add = []) ?(attributes = []) (d : def) : trm =
  {annot = annot; desc = Trm_decl d; loc = loc; is_instr; add;
   typ = Some (typ_unit ()); attributes}
let trm_if ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (cond : trm) (tb : trm) (eb : trm) : trm =
  {annot = annot; desc = Trm_if (cond, tb, eb); loc = loc; is_instr = false;
   add; typ = Some (typ_unit ()); attributes}
let trm_seq ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (tl : trm list) : trm =
  {annot = annot; desc = Trm_seq tl; loc = loc; is_instr = false; add;
   typ = Some (typ_unit ()); attributes}
let trm_apps ?(annot = None) ?(loc = None) ?(is_instr : bool = false)
  ?(add = []) ?(typ = None) ?(attributes = []) (f : trm)
  (args : trm list) : trm =
  {annot = annot; desc = Trm_apps (f, args); loc = loc; is_instr; add; typ;
   attributes}
let trm_while ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (cond : trm) (body : trm) : trm =
  {annot = annot; desc = Trm_while (cond, body); loc = loc; is_instr = false;
   add; typ = Some (typ_unit ()); attributes}
let trm_for ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (init : trm) (cond : trm) (step : trm) (body : trm) : trm =
  {annot; desc = Trm_for (init, cond, step, body); loc; is_instr = false; add;
   typ = Some (typ_unit ()); attributes}
let trm_switch ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (cond : trm) (cases : (trm list * trm) list) : trm =
  {annot; desc = Trm_switch (cond, cases); loc; is_instr = false; add;
   typ = Some (typ_unit ()); attributes}
let trm_abort ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (a : abort) : trm =
  {annot = annot; desc = Trm_abort a; loc = loc; is_instr = true; add;
   typ = Some (typ_unit ()); attributes}
let trm_labelled ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (l : label) (t : trm) : trm =
  {annot; desc = Trm_labelled (l, t); loc; is_instr = false; add;
   typ = Some (typ_unit ()); attributes}
let trm_goto ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (l : label) : trm =
  {annot; desc = Trm_goto l; loc; is_instr = true; add;
   typ = Some (typ_unit ()); attributes}
let trm_decoration ?(annot = None) ?(loc = None) ?(add = []) ?(attributes = [])
  (left : string) (right : string) (t : trm) : trm =  
  {annot; desc = Trm_decoration (left, t, right); loc; is_instr = false; add; 
  typ = Some (typ_unit ()); attributes }
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
let typ_of_lit (l : lit) : typ option =
  match l with
  | Lit_unit -> Some (typ_unit ())
  | Lit_uninitialized -> None
  | Lit_bool _ -> Some (typ_bool ())
  | Lit_int _ -> Some (typ_int ())
  | Lit_double _ -> Some (typ_double ())
  (* todo: add type for strings *)
  | Lit_string _ -> None
let trm_lit ?(annot = None) ?(loc = None) ?(add = []) (l : lit) : trm =
  trm_val ~annot:annot ~loc ~add ~typ:(typ_of_lit l) (Val_lit l)
let trm_prim ?(annot = None) ?(loc = None) ?(add = []) (p : prim) : trm =
  trm_val ~annot:annot ~loc ~add (Val_prim p)
let trm_set ?(annot = None) ?(loc = None) ?(is_instr : bool = false) ?(add = [])
  (t1 : trm) (t2 : trm) : trm =
  trm_apps ~annot:annot ~loc ~is_instr ~add ~typ:(Some (typ_unit ()))
    (trm_binop Binop_set) [t1; t2]
let trm_any ?(annot = None) ?(loc = None) ?(add =  []) ?(typ=None) ?(attributes = [])
(t : trm) : trm =
  {annot = annot; desc = Trm_any t; loc = loc; is_instr=false; add; typ; attributes}


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

let filter_out_heap_alloc : trm list -> trm list =
  List.filter
    (fun ({annot; _} : trm) ->
      match annot with
      | Some Heap_allocated -> false
      | _ -> true
    )

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

let (++) = List.append

(* fold left with access to the indices
  [foldi f a xs] computes  [ f 2 (f 1 (f 0 a x0) x1) x2) ] *)
let foldi (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = List.fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res

(* maps on functions TODO: find why not reusing maps *)
module Fun_map = Map.Make(String)
type 'a funmap = 'a Fun_map.t

(* sets on int lists *)
module IntList =
  struct
    type t = int list
    let rec compare il il' =
      match il, il' with
      | [], [] -> 0
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
      | i :: il, i' :: il' ->
         begin match Stdlib.compare i i' with
         | 0 -> compare il il'
         | c -> c
         end
  end
module IntListSet = Set.Make(IntList)
type ilset = IntListSet.t

(* foldi for int list sets *)
let intl_set_foldi (f : int -> int list -> 'a -> 'a) (ils : ilset)
  (a : 'a) : 'a =
  let (_, res) =
    IntListSet.fold (fun il (i, a) -> (i + 1, f i il a)) ils (0, a)
  in
  res

(* helper function for union of maps of int list sets *)
let ilset_funmap_union_aux (_ : Fun_map.key) (ils : ilset)
  (ils' : ilset) : ilset option =
  Some (IntListSet.union ils ils')
let ilset_funmap_union : ilset funmap -> ilset funmap -> ilset funmap =
  Fun_map.union ilset_funmap_union_aux
let (+@) = ilset_funmap_union

(* map f to t's subterms *)
let trm_map (f : trm -> trm) (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let add = t.add in
  let is_instr = t.is_instr in
  let typ = t.typ in
  match t.desc with
  | Trm_array tl ->
     trm_array ~annot ~loc ~add ~typ (List.map f tl)
  | Trm_struct tl ->
     trm_struct ~annot ~loc ~add ~typ (List.map f tl)
  | Trm_decl d ->
     begin match d with
     | Def_var (tx, init) ->
        trm_decl ~annot ~loc ~is_instr ~add (Def_var (tx, f init))
     | Def_fun (f', res, args, body) ->
        trm_decl ~annot ~loc ~is_instr ~add (Def_fun (f', res, args, f body))
     (* type def *)
     | _ -> t
     end
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
     trm_apps ~annot ~loc ~is_instr ~add ~typ f'' args'
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
  let annot = ty.ty_annot in
  let ty_attributes = ty.ty_attributes in
  match ty.ty_desc with
  | Typ_ptr ty -> typ_ptr ~annot ~ty_attributes (f ty)
  | Typ_array (ty, n) -> typ_array ~annot ~ty_attributes (f ty) n
  | Typ_struct (tlist,tmap, x) ->
     typ_struct ~annot ~ty_attributes tlist (Field_map.map f tmap) x
  | Typ_fun (tyl, ty) ->
     typ_fun ~annot ~ty_attributes (List.map f tyl) (f ty)
  (* var, unit, int, float, double, bool, char *)
  | _ -> ty

(* return the list of var declarations in the list of instructions *)
let rec var_declarations (tl : trm list) : trm list =
  match tl with
  | [] -> []
  | t :: tl ->
     begin match t.desc with
     | Trm_decl (Def_var _) -> t :: var_declarations tl
     (* take into account heap allocated variables TODO: document better *)
     | Trm_seq _ when t.annot = Some Heap_allocated -> t :: var_declarations tl
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
    | Trm_decl d ->
       begin match d with
       | Def_var ((y, _), init) -> y = x || aux init
       | Def_fun (_, _, args, body) ->
          not (List.mem x (List.map fst args)) && aux body
       | _ -> false
       end
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
    | Trm_decl d ->
       begin match d with
       | Def_var (_, init) -> aux init
       | Def_fun (_, _, _, body) -> aux body
       | _ -> false
       end
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
    | Trm_decl d ->
       begin match d with
       | Def_var (_, init) -> aux init
       | Def_fun (_, _, _, body) -> aux body
       | _ -> []
       end
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
  | Trm_decl (Def_var ((x, _), _)) -> x
  (* take into account heap allocated variables *)
  | Trm_seq ({desc = Trm_decl (Def_var ((x, _), _)); _} :: _) -> x
  | Trm_decl (Def_fun (f, _, _, _)) -> f
  | Trm_decl (Def_typ (ty, _)) -> ty
  | _ -> fail t.loc "decl_name: expected declaration"

(* return the initialisation in the declaration *)
let decl_init_val (t : trm) : trm =
  match t.desc with
  | Trm_decl (Def_var (_, init)) -> init
  (* take into account heap allocated variables *)
  | Trm_seq [{desc = Trm_decl _; _};
             {desc = Trm_apps (_, [_; init]);
              annot = Some Initialisation_instruction; _}] ->
     init
  | _ -> fail t.loc "decl_init_val: expected variable declaration"

(* return the type of the declared var *)
let var_decl_type (t : trm) : typ =
  match t.desc with
  | Trm_decl (Def_var ((_, ty), _)) -> ty
  (* take into account heap allocated variables *)
  | Trm_seq ({desc = Trm_decl (Def_var ((_, ty), _)); _} :: _) -> ty
  | _ -> fail t.loc "var_decl_type: expected var declaration"

(* true if t is the declaration of a heap allocated variable *)
let is_heap_alloc (t : trm) : bool =
  match t.desc with
  | Trm_decl (Def_var _) -> false
  | Trm_seq _ when t.annot = Some Heap_allocated -> true
  | _ -> fail t.loc "is_heap_alloc: expected var declaration"

(* return the name of the deleted variable *)
let deleted_var (t : trm) : var =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_delete _))); _},
              [{desc = Trm_var x; _}]) -> x
  | _ -> fail t.loc "deleted_var: expected var to be deleted"

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
     | Trm_decl (Def_var (_, n)) -> n
     (* take into account heap allocated variables *)
     | Trm_seq [{desc = Trm_decl (Def_var _); _};
                {desc =
                   Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set));
                              _}, [_; n]); _}] ->
        n
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
  aliasd_type X takes as argument the description of a file
  (that is a toplevel sequence), and it returns the type ty
  associated via a "typedef ty X" if there is one such definition
 *)
let rec aliased_type (x : typvar) (t : trm) : typ option =
  match t.desc with
  | Trm_decl (Def_typ (y, ty)) when y = x -> Some ty
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
    | Trm_decl d ->
       begin match d with
       | Def_var (_, init) -> aux init
       | Def_fun (_, _, _, body) -> aux body
       | _ -> 0
       end
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
