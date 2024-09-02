open Ast
open Typ
open Trm
open Path
open Target

(** {0:apac_constification Constification}

    {1:intro Introduction}

    The presence of the [const] qualifier in a function prototype helps us to
    determine whether the function modifies a given argument by side-effect,
    i.e. through a pointer or a reference, and classify the argument as
    read-write or read-only during the data dependency discovery (see
    [!Apac_taskify.trm_discover_dependencies] and [!Apac_taskify.taskify_on]).
    In C++ specifically, functions representing class member methods may carry
    the [const] qualifier too. In this case, it indicates that the method does
    not modify any of the class member variables.

    However, the programmers do not always make use of the [const] qualifier
    which makes the data dependency discovery in function calls harder. The goal
    of the constification is to add the [const] qualifier to arguments in
    function prototypes wherever it is possible and useful.

    For each function definition in the input abstract syntax tree, we build a
    function constification record of type [!type:f] featuring a separate
    argument constification record of type [!type:a] for each argument in the
    function prototype. We use these records throughout the constification
    process to determine to which arguments and functions, in the case of class
    member methods, we can add the [const] qualifier at the end. Note that we
    store the function constification records in a hash table of type [!type:r].

    {1:principle Working principle}

    At the beginning of the constification, we assume every argument in every
    function constifiable (see [!build_records]). We then perform an analysis of
    dependencies between function arguments and write operations involving the
    latter within the bodies of corresponding functions (see [!analyze]). When
    the analysis concludes an argument is written to, it modifies its
    constification record so as to mark the argument as unconsitifiable, i.e.
    passes the [const] element of the corresponding record of type [!type:a] to
    [false]. The same happens for class member methods modifying sibling class
    member variables (see the [const] element of function constification records
    of type [!type:f]). However, we do not modify the constification records
    during the analysis. Instead, we use a stack of type [!type:u] keeping trace
    of arguments and functions to unconstify at the end of the data dependency
    analysis on arguments and class memebers. When we pass an object as argument
    to a function, we must not constify it if it calls a non-[const] class
    member method. However, we do not have this information before processing
    the elements in the [!type:u] stack (see [!unconstify]). This is why we need
    a second phase to process the elements of a [!type:o] stack (see
    [!unconstify]).

    {2:aliases Aliasing}

    Pointers and references within a function definition might alias the same
    data as the arguments of the function. Therefore, when we constify an
    argument, we must constify its aliase(s) too. To keep track of argument
    aliases, we use a hash table of type [!type:l] (see
    [!Apac_prologue.find_unconstifiable]).

    {2:multi Multiple variable declarations}

    When we constify an argument, we must propagate the constification to the
    aliases of the argument as well (see type [!type:l] and
    [!Apac_prologue.constify_arguments]). This process is straightforward in the
    case of simple variable declarations. However, if an argument alias appears
    in a multiple variable declaration, we have two different situations to
    consider:

    1. the data type of the multiple variable declaration is already constant,
   
    {[ // i is a constant integer
       // j is a mutable pointer to a constant integer
       int const i, * j; ]}

    2. the data type of the multiple variable declaration is not constant.

    {[ // i is a mutable integer
       // j is a mutable pointer to a mutable integer
       int i, * j; ]}

    If we want to constify [j] in the first situation, it is possible without
    breaking the multiple variable declaration:

    {[int const i, * const j;}]

    In the second case, we cannot fully constify [j], i.e. [int const * const
    j], without constifying [i], which is not desirable. In this case, we have
    to split the multiple variable declaration into a sequence of simple
    variable declarations and then constify the declaration of [j] only.
    However, we cannot apply this transformation directly applied within
    [!Apac_prologue.constify_aliases]. Indeed, the introduction of a new
    sequence of instructions changes the scope of the declared variables. We
    cannot prevent this in a term to term transformation function such as
    [!Apac_prologue.constify_aliases], but in a transformation function modyfing
    the abstract syntax tree by side-effect, i.e. a target to unit
    transformation, in which we can call [!Nobrace_transfo.remove_after] to
    effectively remove the braces from the sequence of simple variable
    declarations so as to preserve their initial scope. Therefore,
    [!Apac_prologue.constify_aliases] should only mark those multiple variable
    declarations which need to be split into simple variable declarations and
    determine those to constify. To keep track of this information, so we can
    actually perform the transformations (using
    [!Apac_prologue.unfold_let_mult]), we use a hash table of type [!type:m]. *)

(** {1:types Data types} *)

(** [k]: variable kinds (simple variable, reference or pointer). *)
type k =
  (** The variable is a simple variable. *)
  | Variable
  (** The variable is a pointer, e.g. [int * a]. *)
  | Pointer
  (** The variable is a reference, e.g. [int & a]. *)
  | Reference
  (** The variable is an array, e.g. [int a\[N\]]. *)
  | Array

(** [a]: an argument constification record. *)
type a = {
    (** [self]: gives the kind of the argument (see [!type:k]). *)
    self : k;
    (** [const]: decides on the constification of the argument. *)
    mutable const : bool;
    (** [propagate]: a map of function variables of type [!type:var] (keys are
        0-based argument positions of type [!type:int]).
        
        If the argument is a reference or a pointer, i.e. when [!kind] is either
        a [Pointer] or a [Reference], and if the function modifies the value in
        memory it refers or points to, we must not constify the argument. Let us
        explain [propagate] on an example. Let [g] be a function we define as
        follows:
        
        {[void g(int &v) {
            v += 4;
        }]}
         
        and [f] a function we define as follows:
         
        {[int f(int a, int b) {
            g(b);
            return a + b;
        }]}
         
        In [g], [v] is a reference and the function modifies the target value.
        Therefore, we must not constify the argument [v]. However, [f] calls [g]
        and passes its argument [b] by reference to [g]. This way, [g] shall
        modify the value behind [b]. We already know we must not constify [v] in
        [g]. Due to this dependency, we must propagate this decision also to [b]
        in [f]. To achieve this and keep track of the dependency, we add [(f,
        1)] to the [propagate] map of the argument constification record of [v]
        in [g]. *)
    mutable propagate : int Var_map.t;
  }

(** [f]: a function constification record. *)
type f = {
    (** [args]: a map of argument constification records (see [!type:a], keys
        are 0-based argument positions of type [!type:int]). *)
    args : a Tools.Int_map.t;
    (** [const]: decides on the constification of the function (relevant only
        for class member methods). *)
    mutable const : bool;
    (** [kind]: determines the return value kind (see [!type:k]). *)
    return : k;
    (** [member]: tells whether the function is a class member method. *)
    mutable member : bool
  }

(** [r]: a type of hash table for function constification records (see
    [!section:intro]). *)
type r = f Var_Hashtbl.t
  
(** [l]: a type of hash table of argument aliases (see [!section:aliases]).
    
    Keys are the [!type:lvar] of the alias and values are pairs of two
    [!type:int] elements. The first corresponds to the 0-based position of the
    function argument being aliased and the second gives alias' number of levels
    of indirection, e.g. 2 in the case of [int ** tab] and 0 in the case of [int
    a]. *)
type l = (int * int) LVar_Hashtbl.t

(** [u]: type of stack of arguments, except for objects, we must not constify
    (see [!section:principle]).

    Elements are ([!type:var], [!type:int]) pairs where the [!type:var] element
    identifies the target function and the [!type:int] element identifies the
    position of the argument to unconstify. If the latter is set to -1, it means
    we have to unconstify the function itself. *)
type u = (var * int) Stack.t

(** [o]: type of stack of arguments, representing objects, we must not constify
    (see [!section:principle]).

    Elements are ([!type:var], [!type:int], [!type:var]) triplets where the
    first element identifies the target function, the second element gives the
    0-based position of the argument to unconstify and the third element
    represents the class member method called on that argument. Then, during
    unconstification, if we unconstify the method behind the third element, we
    do so for the argument (the second element) of the function behind the first
    element too. *)
type o = (var * int * var) Stack.t

(** [m]: type of hash table for multiple variable declarations we must transform
    into sequences of simple variable declarations while constifying some of
    them (see [!section:multi]).

    Keys are declaration marks (see type [!type:mark] which is actually a
    [!type:string]) and values are lists of booleans (type [!type:bool]). The
    latter have as many elements as there are declarations in the target
    multiple variable declaration. In other terms, a boolean value is associated
    to each variable declaration. If the value is [true], it means that we have
    to constify the corresponding variable declaration. *)
type m = (mark, bool list) Hashtbl.t

(** {1:functions Functions} *)

(** [typ_constify ty]: augment the type [ty] with the [const] qualifier so as to
    prevent a function with an argument of type [ty] to alter it by side-effect.
    Basically, if [ty] is a pointer or a reference type, i.e. we do not simply
    pass the argument by value, [typ_constify] ensures [ty] refers to [const]
    data, e.g. from [int * arg] it makes [const int * arg], but in the case of
    [int arg], it leaves [ty] unchanged. *)
let typ_constify (ty : typ) : typ =
  (** Alias values we refer a lot. *)
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  (** [typ_constify.aux ty]: auxiliary function to recursively constify [ty],
      e.g. [int ** arg] becomes [const int * const * arg]. *)
  let rec aux (ty : typ) : typ =
    match ty.typ_desc with
    (** When [ty] is a pointer, add the [const] qualifier to it. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (** When [ty] is a constant pointer, add the [const] qualifier to the inner
        type. *)
    | Typ_const { typ_desc =
                    Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty };
                  typ_annot = annot;
                  typ_attributes = attributes } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (** When [ty] is a user-defined constructed type and *)
    | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       (** if it is a [typedef] declaration *)
       | Some td ->
          begin match td.typdef_body with
          (** aliasing an another type, try to constify the latter. *) 
          | Typdef_alias ty -> aux ty
          (** Otherwise, constify the constructed type as is. *)
          | _ -> typ_const ty
          end
       | None -> typ_const ty
       end
    (** When [ty] is already of constant type. There is nothing to do. *)
    | Typ_const _ -> ty
    (** In any other case, add the [const] qualifier to [ty]. *)
    | _ -> typ_const ty
  in
  (** [typ_constify.first ty]: auxiliary function to skip the constification of
      the 0-th level of indirection of [ty] at the beginning of the process,
      e.g. we want [int ** arg] to become [const int * const * arg] and not
      [const int * const * const arg] so as [int arg] to stay [int arg]. A
      function always receives this value as a copy. *)
  let first (ty : typ) : typ =
    match ty.typ_desc with
    (** When [ty] is already of constant type, there is nothing to skip. We just
        have to ensure that the inner types of [ty] are [const] too. *)
    | Typ_const _ -> aux ty
    (** When [ty] is a reference, a pointer or an array, launch the
        constification on the inner types. *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
       typ_ref ~annot ~attributes (aux ty)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty; } ->
       typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty)
    | Typ_array (ty, s) -> typ_array ~annot ~attributes (aux ty) s
    (** Otherwise, we do not constify anything and leavy [ty] as is. *)
    | _ -> ty
  in
  (** If [ty] is a user-defined constructed type, *)
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       | Some td ->
          begin match td.typdef_body with
          (** try to identify which type it is aliasing before launching the
              constification. *)
          | Typdef_alias ty -> first ty
          | _ -> ty
          end
       (** If [ty] is not an alias, simply constify [ty] as is. *)
       | None -> typ_const ty
       end
  (** Otherwise, launch the constification. *)
  | _ -> first ty

(** [arg_explode v ty]: if the type [ty] of the argument variable [v] represents
    a structure or a class, this function returns a list of labelled variables
    (see type [!type:lvar]) corresponding to each member of the structure or the
    class. If a member of the latter is itself another structure or class, the
    function explores it recursively.

    For example, let us consider two structure types [Point] and [Polygone] we
    define as follows:

    {[
    typedef struct {
      int x;
      int y;
    } Point;

    typedef struct {
      Point* pts;
      long unsigned int angles;
    } Polygone;
    ]}

    For an argument variable [Polygone * poly], this function returns the list
    [(poly, poly#pts, poly#pts#x, poly#pts#y, poly#angles)]. *)
let arg_explode (v : var) (ty : typ) : lvar list =
  (** [arg_explode.inner ty]: an auxiliary function returning the inner type of
      [ty] when [ty] is a pointer type, const type or an array type. *)
  let rec inner (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_const ty -> inner ty
    | Typ_ptr {inner_typ = ty; _} -> inner ty
    | Typ_array (ty, _) -> inner ty
    | _ -> ty
  in
  (** [arg_explode.core v l ty lvs]: an auxiliary function allowing us to
      recursively explore the type [ty] of the argument variable [v] and to
      build a list of labelled variables [lvs] (see type [!type:lvar]), with [v]
      as the variable component and [l] as the label component, corresponding to
      each member of a structure or a class within [ty]. At the end of the
      explorations the function returns the final [lvs]. *)
  let rec core (v : var) (l : label) (ty : typ) (lvs : lvar list) : lvar list =
    match ty.typ_desc with
    (** When [ty] is a user-defined record type *)
    | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       | Some td ->
          begin match td.typdef_body with
          | Typdef_record fs ->
             (** recursively explore its members in order to represent them as
                 labelled variables we add to [lvs]. *)
             List.fold_left (fun acc (rf, _) ->
                 match rf with
                 | Record_field_member (l', ty') ->
                    let l'' = if l <> "" then l ^ "#" ^ l' else l' in
                    let lv : lvar = { v = v; l = l''} in
                    acc @ (lv :: (core v l'' ty' lvs))
                 | _ -> acc
               ) lvs fs
          (** Other types cannot have members, so there is nothing to explore,
              we can return [lvs]. *)
          | _ -> lvs
          end
       (** The same goes for user-defined types simply aliasing atomic types. *)
       | None -> lvs
       end
    (** When [ty] is a class, a structure or a union, explore the underlying
        record type. *)
    | Typ_record (_, ty) -> core v l ty lvs
    (** In any other case, there are no members to explore. *)
    | _ -> lvs
  in
  (** Get the inner type of [ty], i.e. strip pointer, reference, constant and
      array types from [ty]. *)
  let ty = inner ty in
  (** Build the top-level labelled variable from [v]. *)
  let lv : lvar = { v = v; l = "" } in
  (** When [ty] is a user-defined record type, a class, a structure or a union,
      explore its members. *)
  match ty.typ_desc with
  | Typ_constr _
    | Typ_record _ -> lv :: (core v "" ty [])
  (** Otherwise, simply return the toplevel labelled variable [lv]. *)
  | _ -> [lv]

(** [trm_resolve_var_in_unop_or_array_access_and_get t] tries to resolve the
    labelled variable behind a unary operation (++, --, &, get) or array access
    in [t]. *)
let trm_resolve_var_in_unop_or_array_access_and_get (t : trm) : lvar option =
  (** [trm_resolve_var_in_unop_or_array_access_and_get.aux l t]: auxiliary
      function to recursively inspect [t]. [l] is the label we use to produce
      the final labelled variable, if any. *)
  let rec aux (l : label) (t : trm) : lvar option =
    match t.desc with
    (** When [t] is a unary operation and *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin
         match op with
         (** more precisely a structure access or a structure get operation,
             i.e. a [structure.member] or a [structure->member], extract the
             label of the [member] field [f] involved in the operation and
             continue the resolution. *)
         | Unop_struct_access f -> aux f t
         | Unop_struct_get f -> aux f t
         (** and more precisely a get, an [&], a prefix or a postfix unary
             operation ([++i] or [i++]), continue the resolution on the
             underlying term. *)
         | Unop_get
           | Unop_address -> aux l t
         | _ when (is_prefix_unary op || is_postfix_unary op) -> aux l t
         (** Otherwise, there is nothing to resolve. *)
         | _ -> None
       end
    (** When [t] is a binary operation corresponding to an array access, *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            (** continue the resolution on the underlying term. *)
            _}, [t; _]) -> aux l t
    (** When [t] leads to a variable, *)
    | Trm_var (_, v) ->
       (** use the [!type:var] component [v] of the corresponding term and the
           label [l], if any, to build the associated labelled variable and
           return. *)
       let lv : lvar = { v = v; l = l } in Some lv
    (** Otherwise, there is nothing to resolve. *)
    | _ -> None
  in
  (** Call the auxiliary function. *)
  aux "" t

(** [trm_resolve_pointer_and_alias t a]: tries to resolve pointer operation [t]
    and checks in the hash table of arguments and aliases [a] (see [!type:l])
    whether the resulting pointer is an argument or an alias to an argument. If
    the resolution operation succeeds and if the resulting pointer is an
    argument or an alias to an argument, the function returns the labelled
    variable corresponding to the resulting pointer as well as the 0-based
    position of the argument it is aliasing. *)
let trm_resolve_pointer_and_alias (t : trm) (a : l) : (lvar * int) option =
  (** [trm_resolve_pointer_and_alias.aux nli l t]: auxiliary function to
      recursively inspect [t]. [l] is the label we use to produce the final
      labelled variable, if any. [nli] counts the number of levels of
      indirections in [t]. *)
  let rec aux (nli : int) (l : label) (t : trm) : (lvar * int) option =
    match t.desc with
    (** When [t] is a unary operation *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       (** and more precisely a get operation such as [*i], the number of
           levels of indirection decreases. *)
       | Unop_get -> aux (nli - 1) l t
       (** and more precisely an address operation such as [&i], the number of
           levels of indirection increases. *)
       | Unop_address -> aux (nli + 1) l t
       (** and more precisely a cast operation, the number of levels of
           indirection corresponds to the sum of the current number of levels of
           indirection and the number of levels of indirection of the
           destination type. *)
       | Unop_cast ty -> aux (nli + Apac_miscellaneous.typ_get_nli ty) l t
       (** more precisely a structure access or a structure get operation, i.e.
           a [structure.member] or a [structure->member], extract the label of
           the [member] field [f] involved in the operation and continue the
           resolution. *)
       | Unop_struct_access f -> aux nli f t
       | Unop_struct_get f -> aux nli f t
       (** Otherwise, there is nothing to resolve. *)
       | _ -> None
       end
    (** When [t] is a binary operation corresponding to an array access, *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            (** continue the resolution on the underlying term. *)
            _ }, [t; _]) -> aux (nli - 1) l t
    (** When [t] is another kind of binary operation, *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (** continue the resolution on both the left-hand side and the right
           hand-side operand. *)
       begin match (aux nli l lhs, aux nli l rhs) with
       | Some (res), None -> Some (res)
       | None, Some (res) -> Some (res)
       | None, None
         (** However, binary operations between two pointers do not lead to a
             valid alias of one of them. *)
         | Some (_), Some (_) -> None
       end
    (** When [t] leads to a variable, *)
    | Trm_var (_, v) ->
       (** use the [!type:var] component [v] of the corresponding term and the
           label [l], if any, to build the associated labelled variable and
           return. *)
       let lv : lvar = { v = v; l = l } in
       (** If [lv] is an argument or an alias to an argument, then return [lv]
           as well as the corresponding 0-based position of the argument it
           aliases. *)       
       begin match LVar_Hashtbl.find_opt a lv with
       | Some (tg, nli') when (nli + nli') > 0 -> Some (lv, tg)
       (** Otherwise, there is nothing to return. *)
       | _ -> None
       end
    (** Otherwise, there is nothing to resolve. *)
    | _ -> None
  in
  (** Call the auxiliary function. *)
  aux 0 "" t

(** [trm_let_update_aliases ?r tv ti a]: checks in the hash table of arguments
    and aliases [a] (see [!type:l]) whether the variable declaration the typed
    variable [tv] and the initializaion term [ti] specify creates an alias to a
    variable or an argument. If so, the function updates [a] and returns [1] if
    [tv] is a reference and [2] if [tv] is a pointer. Otherwise, it does nothing
    and returns [0].

    Note that when we apply the function on the elements of a simple variable
    declaration, i.e. a [!Ast.trm_let], the [!Trm.is_reference] function we use
    to check whether the new variable is a reference has no effect. This is due
    to differences in representing simple [!Ast.trm_let] and multiple
    [!Ast.trm_let_mult] variable declarations. In this case, we use the optional
    [r] parameter to ensure the test evaluates correctly. *)
let trm_let_update_aliases ?(r = false)
      (tv : typed_var) (ti : trm) (a : l) : int =
  (** Deconstruct the typed variable [tv] *)
  let (v, ty) = tv in
  (** and build the corresponding labelled variable. *)
  let lv : lvar = { v = v; l = String.empty } in
  (** If we are working with a reference, *)
  if is_reference ty || r then
    begin
      (** we need to go through potential access and reference operations in the
          initialization term [ti] to get the labelled variable [lv'] we might
          be creating an alias for (see
          [!trm_strip_accesses_and_references_and_get_lvar]). *)
      match (Apac_miscellaneous.trm_strip_and_get_lvar ti) with
      | Some lv' ->
         (** If [lv'] is in [a], i.e. it is an argument or an alias to an
             argument, we are about to create an alias to [lv']. *)
         if LVar_Hashtbl.mem a lv' then
           begin
             (** So we have to create a new entry in [a] to keep track of it. *)
             let (tg, _) = LVar_Hashtbl.find a lv' in
             (** Note that we consider that the number of level os indirections
                 of references is -1. *)
             LVar_Hashtbl.add a lv (tg, -1);
             (** Return 1 because we are declaring a reference. *)
             1
           end
             (** Otherwise, we are not creating an alias, return [0]. *)
         else 0
      (** If [ti] does not lead to a labelled variable, we are not
          creating an alias, return [0]. *)
      | None -> 0
    end
      (** If we are working with a pointer, *)
  else if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
    begin
      (** we need to go through potential pointer operations in the
          initialization term [ti] to get the labelled variable [lv'] we might
          be creating an alias for (see [!trm_resolve_pointer_and_alias]). *)
      match (trm_resolve_pointer_and_alias ti a) with
      (** If we are about to create an alias to [tg], create a new entry in [a]
          to keep track of it and return [2] because we are decalring a
          pointer. *)
      | Some (_, tg) -> LVar_Hashtbl.add a lv
                          (tg, Apac_miscellaneous.typ_get_nli ty); 2
      (** Otherwise, we are not creating an alias, return [0]. *)
      | None -> 0
    end
      (** Otherwise, we are not creating an alias, return [0]. *)
  else 0

(** [find_parent_typedef_record p]: goes back up the path [p] and returns the
    first term corresponding to a class or a structure definition, if any. We
    use this function to determine the parent class of a structure or a function
    in order to access to its members. *)
let find_parent_typedef_record (p : path) : trm option =
  (** We shall go back on our steps in the path, i.e. in the direction of the
      root of the abstract syntax tree, so we need to reverse [p]. *)
  let r = List.tl (List.rev p) in
  (** [find_parent_typedef_record.aux r]: an auxiliary function hiding the path
      reversal. Here, the [r] parameter refers to the reversed path. *)
  let rec aux (r : path) : trm option =
    (** Recursively inspect [r] and *)
    match r with
    | e :: f -> 
       begin
         (** if we detect a class or a structure definition, return it. *)
         let tg = target_of_path (List.rev r) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_typedef { typdef_body = Typdef_record _; _ } -> Some (t)
         | _ -> aux f
       end
    | [] -> None
  in
  (** Call the auxiliary function. *)
  aux r

(** [missing_record f a]: generates an error message telling that there is no
    constification record for the argument at a position [i] in the case of a
    function [f] and fails. [l] represents the name of the calling function to
    include in the message. If [i] is [-1], the resulting message states that no
    constification record exist for the function [f] all together. *)
let missing_record (l : string) (f : var) (i : int) : string =
  if i < 0 then
    Printf.sprintf
      "Apac_constification.%s: there is no constification record of '%s'."
      l (var_to_string f)
  else 
    Printf.sprintf
      "Apac_constification.%s: the constification record of '%s' has \
       no argument constification record for the argument on \
       position '%d'." l (var_to_string f) i

(** [build_records_on ]: see [!build_records]. *)
let build_records_on (crs : r) (t : trm) : unit =
  (** Deconstruct the function definition term [t]. *)
  let error = "Apac_constification.build_records_on: expected a target to a \
               function definition!" in
  let (f, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  (** If the function [f] is a class member method, its first argument is the
      [this] variable referring to the parent class. Ignore it. *)
  let args =
    (** This is necessary only if [fn] does not refer to the [main] function and
        if it has at least one argument. *)
    if f.name <> "main" && (List.length args) > 0 then
      (** In this case, extract the first argument of the function and if it is
          [this], discard it. *)
      let (first, _) = List.hd args in
      if first.name = "this" then List.tl args else args
    else args in
  (** Create constification records for the function's arguments. *)
  let args = List.mapi (fun i (arg, ty) ->
                 (i, {
                    self = if (is_typ_ptr ty) then Pointer
                           else if (is_typ_ref ty) then Reference
                           else if (is_typ_array ty) then Array
                           else Variable;
                    const = true;
                    propagate = Var_map.empty
               })) args in
  let args = Tools.Int_map.of_seq (List.to_seq args) in
  (** Create the constification record for the function itself *)
  let fcr : f = {
      args = args;
      const = true;
      return = if (is_typ_ptr ret_ty) then Pointer
               else if (is_typ_ref ret_ty) then Reference
               else if (is_typ_array ret_ty) then Array
               else Variable;
      member = false
    } in
  (** and add it to the hash table of function constification records [crs] if
      it is not present there already, e.g. in the case of a pre-declaration. *)
  if not (Var_Hashtbl.mem crs f) then
    Var_Hashtbl.add crs f fcr
      
(** [build_records crs tg]: expects the target [tg] to point at a function
    definition, builds a function constification record for it and stores the
    record in the hash table of function constification records [crs] (see type
    [!type:r]). *)
let build_records (crs : r) (tg : target) : unit =
  Target.iter_at_target_paths (build_records_on crs) tg

(** [analyze_on us ous crs p t]: see [!analyze]. *)
let analyze_on (us : u) (ous : o) (crs : r) (p : path) (t : trm) : unit =
  (** [analyze_on.aux aliases f t]: auxiliary function to recursively visit the
      abstract syntax tree term [t] within a function [f] in order to resolve
      dependencies between arguments and aliases. When first calling this
      function, [t] should be the statement sequence representing [f]'s body. *)
  let rec aux (aliases : l) (f : var) (t : trm) : unit =
    match t.desc with
    (** When [t] is a compound, an interation or a selection statement involving
        substatements, loop over the latter. *)
    | Trm_seq _
      | Trm_for _
      | Trm_for_c _
      | Trm_if _
      | Trm_switch _
      | Trm_while _ -> trm_iter (aux aliases f) t
    (** When [t] is a call to a function [f'], we may have to update the
        [propagate] elements in the constification records of its [args]. *)
    | Trm_apps ({ desc = Trm_var (_ , f'); _ }, args) ->
       (** If we have a function constification record for [f'], *)
       if Var_Hashtbl.mem crs f' then
         (** gather it. *)
         let fcr = Var_Hashtbl.find crs f' in
         (** In the case of a call to a class member method, the first argument
             is the [this] variable referring to the parent class instance, e.g.
             in [this->p(i, j)] the first argument is [this] and in [a.f(i, j)]
             the first argument is [a]. This is why the number of arguments in
             the function and the number of argument constification records
             associated with the function may not be the same. If [d], the
             difference of these two values, has a positive value, we know that
             the current function call is a call to a class member method. *)
         let d = (List.length args) - (Tools.Int_map.cardinal fcr.args) in
         (** For each argument of the function call, we *)
         List.iteri (fun i arg ->
             (** go through potential access and reference operations to obtain
                 the argument in the form of a labelled variable, if any, and *)
             match (Apac_miscellaneous.trm_strip_and_get_lvar arg) with
             | Some lv ->
                (** if the variable is an alias, *)
                if LVar_Hashtbl.mem aliases lv then
                  begin
                    (** we have to determine the position [tg] of the argument
                        of [f] the variable behind [arg] is aliasing. *)
                    let (tg, _) = LVar_Hashtbl.find aliases lv in
                    (** If we are calling a class member method and if the
                        parent is not [this], we may have to unconstify the
                        argument (see [!unconstify]). The constification does
                        not take into account class member variables for now. *)
                    if (i - d) < 0 && lv.v.name <> "this" then
                      Stack.push (f, tg, f') ous
                    else
                      (** In the opposite case, we have to gather the
                          constification record of [i']-th argument (without
                          counting [this]) of [f']. *)
                      let i' = i - d in
                      if (Tools.Int_map.mem i' fcr.args) then
                        begin
                          let acr = Tools.Int_map.find i' fcr.args in
                          (** and if the latter is a pointer, a reference or an
                              array, i.e. it is not a simple variable, *)
                          if acr.self <> Variable then
                            (** we may have to unconstify the argument of [f]
                                (at the position [tg]) the variable [lv] behind
                                [arg] is aliasing. *)
                            acr.propagate <- Var_map.add f tg acr.propagate
                        end
                      else
                        (** If it's not possible, fail. This is not normal! *)
                        fail t.loc (missing_record "analyze_on.aux" f i')
                  end
             (** If we pass something else than a variable as argument, there is
                 nothing to do. *)
             | None -> ()
           ) args;
       (** When we do not have a constification record for [f'], it means that
           we do not know the definition of [f'] and thus, we are not able to
           perform the constification as usual. In this case, we consider that
           [f'] may alter any of its arguments except for simple variables which
           we always pass by copy. *)
       else
         begin
           (** Let us warn the user about that. *)
           Printf.printf
             "Apac_constification.analyze_on.aux: missing definition of `%s', \
              considering the function may alter any of its arguments except \
              for simple variables.\n" (var_to_string f');
           (** Then, for each argument of the function call, *)
           List.iteri (fun i arg ->
               (** go through potential access and reference operations to
                   obtain the argument in the form of a labelled variable [lv],
                   if any. *)
               match (Apac_miscellaneous.trm_strip_and_get_lvar arg) with
               | Some lv ->
                  (** If [lv] is an alias, *)
                  if LVar_Hashtbl.mem aliases lv then
                    (** determine the position [tg] of the argument of [f] it is
                        aliasing and if, based on its constification record, the
                        argument is not a simple variable, unconstify it. *)
                    let (tg, _) = LVar_Hashtbl.find aliases lv in
                    let fcr = Var_Hashtbl.find crs f in
                    if (Tools.Int_map.mem i fcr.args) then
                      begin
                        let acr = Tools.Int_map.find i fcr.args in
                        if acr.self <> Variable then
                          Stack.push (f, tg) us
                      end
                    else
                      (** If there is no such record, fail. This is not
                          normal! *)
                      fail t.loc (missing_record "analyze_on.aux" f i)
               (** If we pass something else than a variable as argument, there
                   is nothing to do. *)
               | None -> ()
             ) args;
         end;
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is a declararion of a variable [tv] with an initialization term
        [ti], *)
    | Trm_let (_, tv, { desc = Trm_apps (_, [ti]); _ }, _) ->
       (** we may have to update the hash table of aliases. *)
       let r = trm_has_cstyle Reference t in
       let _ = trm_let_update_aliases ~r tv ti aliases in
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is a declararion of multiple variables [tvs] with optional
        initialization terms [tis], *)
    | Trm_let_mult (_, tvs, tis) ->
       (** we may have to update the hash table of aliases for each variable
           [tv] and optional initialization term [ti] in [tvs] and [tis],
           respectively. *)
       List.iter2 (
           fun tv ti ->
           let _ = trm_let_update_aliases tv ti aliases in ()
         ) tvs tis;
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is an assignment or a compound assignment of an [rval] to an
        [lval] term, we may have to update the unconstification stack [us]. *)
    | Trm_apps (_, [lval; rval]) when is_set_operation t ->
       begin
         (** We are modifying [lval] by assignment. Resolve the underlying
             labelled variable [lv] and determine whether it has been
             dereferenced based on the value of [deref]. *)
         match
           (Apac_miscellaneous.trm_resolve_binop_lval_and_get_with_deref
              ~plus:true lval)
         with
         | Some (lv, deref) ->
            (** If [lv] is [this], it means that the function we are in is
                modifying a member variable of the parent class. Therefore, we
                will have to unconstify the function itself. *)
            if lv.v.name = "this" then Stack.push (f, -1) us;
            (** If [lv] is an argument or an alias to an argument, *)
            if LVar_Hashtbl.mem aliases lv then
              (** determine the position [tg] of the argument [lv] stands for or
                  is aliasing together with the number of levels of indirection
                  [nli] of [lv], *)
              let (tg, nli) = LVar_Hashtbl.find aliases lv in
              let tg = if lv.v.name = "this" then tg + 1 else tg in
              (** acquire the constification record [fcr] of [f] as well as the
                  constification record [acr] of the argument of [f] the [lv]
                  variable represents or is aliasing. If these records do not
                  exist, fail. This is not normal! *)
              if not (Var_Hashtbl.mem crs f) then
                fail t.loc (missing_record "analyze_on.aux" f (-1));
              let fcr = Var_Hashtbl.find crs f in
              if not (Tools.Int_map.mem tg fcr.args) then
                fail t.loc (missing_record "analyze_on.aux" f tg);
              let acr = Tools.Int_map.find tg fcr.args in
              (** If [lv] represents a dereferenced pointer, an array acces or a
                  reference, we must unconstify it. *)
              if deref || (nli < 0 && acr.self <> Variable) then
                (** An alias of the same name may have been used multiple times
                    to alias different memory locations.
                    
                    For example, in:
                    
                    [{ L1: void f(int * a, int * b, int * c) {
                    L2:   int * d = a;
                    L3:   d = c; 
                    L4:   *d = 1;
                    L5: } ]}
                    
                    [d] is declared as an alias to [a] on L2. The function does
                    not modify The data pointed to by [a]. On L3, [d] becomes an
                    alias for [c]. Then, on L4, the function modifies the data
                    pointed to by [c] through its alias [d]. Therefore, the
                    analysis concludes that nor [c] nor [d] should be
                    constified. However, for [a] it concludes that we can
                    constify the argument as the function never modifies the
                    data it is pointing to. In the end, this produces a
                    compilation error as the function assignes [a], which became
                    [const], to the non-[const] variable [d] on L2.
                    
                    In order to avoid this situation, we must propagate the
                    unconstification to previously aliased arguments too. As
                    [aliases] stores all the values that were ever assigned to a
                    given key, we simply have to [find_all] of them and push
                    them to the unconstification stack. *)
                let all = LVar_Hashtbl.find_all aliases lv in
                List.iter (fun (tg, _) ->
                    (** Again, we do not consider parent class members because
                        the constification process does not analyze entire
                        classes for now. *)
                    if tg > -1 then Stack.push (f, tg) us
                  ) all
              else
                (** Otherwise, the alias comes to have a new target, i.e. we
                    did not dereference [lv]. *)
                begin
                  (** In this case, we have to resolve the pointer labelled
                      variable behind [rval] and check if it is an argument an
                      alias to an argument at [tg]-th position. *)
                  match (trm_resolve_pointer_and_alias rval aliases) with
                  (** If so, we have to add a new entry into [aliases]. This
                      happens, for example, on L3 in the above example. *)
                  | Some (_, tg) ->
                     (** The value of [deref] is incorrect when [lval] refers to
                         the parent [this] because we operate on [this] and not
                         on the underlying structure member. In this case, to
                         verify that [lv] was not dereferenced, we have to check
                         that [nli] is greater than [0]. *)
                     if (not deref) || (lv.v.name = "this" && nli <> 0) then
                       LVar_Hashtbl.add aliases lv (tg, nli)
                  (** If [rval] is something else than a variable, there is
                      nothing to do. *)
                  | None -> ()
                end
         (** If [lval] is something else than a variable, there is nothing to
             do. *)
         | _ -> ()
       end;
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is an increment or decrement unary operation on an operand [o],
        we may have to update the unconstification stack [us]. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _}, [o]) when
           (is_prefix_unary op) || (is_postfix_unary op) ->
       begin
         (** We are modifying [o] by assignment. Resolve the underlying labelled
             variable [lv] and determine whether it has been dereferenced based
             on the value of [deref]. *)
         match
           (Apac_miscellaneous.trm_resolve_binop_lval_and_get_with_deref o)
         with
         | Some (lv, deref) ->
            (** If [lv] is an argument or an alias to an argument, *)
            if LVar_Hashtbl.mem aliases lv then
              (** determine the position [tg] of the argument [lv] stands for or
                  is aliasing together with the number of levels of indirection
                  [nli] of [lv], *)
              let (tg, nli) = LVar_Hashtbl.find aliases lv in
              let tg = if lv.v.name = "this" then tg + 1 else tg in
              (** acquire the constification record [fcr] of [f] as well as the
                  constification record [acr] of the argument of [f] the [lv]
                  variable represents or is aliasing. Fail if these records do
                  not exist. This is not normal! *)
              if not (Var_Hashtbl.mem crs f) then
                fail t.loc (missing_record "analyze_on.aux" f (-1));
              let fcr = Var_Hashtbl.find crs f in
              if not (Tools.Int_map.mem tg fcr.args) then
                fail t.loc (missing_record "analyze_on.aux" f tg);
              let acr = Tools.Int_map.find tg fcr.args in
              (** If [lv] represents a dereferenced pointer, an array acces or a
                  reference, we must unconstify it and propagate the
                  unconstification to all previously aliased arguments. *)
              if deref || (nli < 0 && acr.self <> Variable) then
                let all = LVar_Hashtbl.find_all aliases lv in
                List.iter (fun (tg, _) ->
                    (** Again, we do not consider parent class members because
                        the constification process does not analyze entire
                        classes for now. *)
                    if tg > -1 then Stack.push (f, tg) us
                  ) all
         (** If [lval] is something else than a variable, then Houston, we have
             a problem. *)
         | None -> fail t.loc "Apac_constification.analyze_on.aux: unable to \
                               retrieve variable name"
       end;
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is a return statement returning [rt], we have to update the
        unconstification stack [us] if the return type of [f] is a pointer, a
        reference or an array access, i.e. it is not a simple variable, *)
    | Trm_abort (Ret (Some rt)) ->
       (** Acquire the constification record of [f]. If it does not exist, fail.
           This is not normal! *)
       if not (Var_Hashtbl.mem crs f) then
         fail t.loc (missing_record "analyze_on.aux" f (-1));
       let fcr = Var_Hashtbl.find crs f in
       (** If the return type of [f] is a reference or an array, *)
       if fcr.return = Reference || fcr.return = Array then
         begin
           (** then after resolving potential unary operations, e.g.
               dereferencements, and array accesses using
               [!trm_resolve_var_in_unop_or_array_access_and_get], there are two
               types of return term we can possibly deal with: *)
           match (trm_resolve_var_in_unop_or_array_access_and_get rt) with
           (** 1. in the case of a variable term [rv] corresponding to a
               reference or an array, *)
           | Some rv ->
              (** we must propagate the unconstification to all previously
                  aliased arguments. *)
              let all = LVar_Hashtbl.find_all aliases rv in
              List.iter (fun (tg, _) ->
                  (** Again, we do not consider parent class members because the
                      constification process does not analyze entire classes for
                      now. *)
                  if tg > -1 then Stack.push (f, tg) us
                ) all
           (** 2. in the case of a function call with reference return type,
               there is nothing to do as the function call cannot be an alias to
               an argument. *)
           | _ -> ()
         end
           (** If the return type of [f] is a pointer, *)
       else if fcr.return = Pointer then
         begin
           (** we have to go through potential pointer operations within the
               return term [rt] and try to determine if they lead to a pointer
               variable representing or aliasing an argument at position
               [tg]. *)
           match (trm_resolve_pointer_and_alias rt aliases) with
           (** If so, we must unconstify it. *)
           | Some (_, tg) -> 
              (** Again, we do not consider parent class members because the
                  constification process does not analyze entire classes for
                  now. *)
              if tg > -1 then Stack.push (f, tg) us
           (** If [rt] is something else than a variable, there is nothing to
               do. *)
           | None -> ()
         end;
       (** Continue the analysis on substatements, if any. *)
       trm_iter (aux aliases f) t
    (** When [t] is none of the above, try continuing the analysis on
        substatements, if any. *)
    | _ -> trm_iter (aux aliases f) t
  in
  (** Deconstruct the definition term [t] of the function [f]. *)
  let error = "Apac_constification.analyze_on: expected target to a function \
               definition." in
  let (f, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (** Find the constification record [fcr] of [f] in [crs]. If it does not
      exist, fail. This is not normal! *)
  if not (Var_Hashtbl.mem crs f) then
         fail t.loc (missing_record "analyze_on.aux" f (-1));
  let fcr = Var_Hashtbl.find crs f in
  (** Create a hash table of arguments and aliases to arguments. *)
  let aliases : l = LVar_Hashtbl.create 10 in
  (** Try to find the parent class of the function. If any, get all the member
      variables of that class in [siblings] and *)
  let siblings = match (find_parent_typedef_record p) with
    | Some (td) -> typedef_get_members td
    | None -> [] in
  (** add them to [fcr]. *)
  if (List.length siblings) > 0 then
    begin
      (** In the case of class member methods, the first argument of the
          function is the variable referring to the parent class instance, i.e.
          [this]. Get it. *)
      let (this, _) = List.hd args in
      (** Record that [f] is a class member method. *)
      fcr.member <- true;
      (** For each member variable of the parent class, *)
      List.iteri (fun i (label, ty) ->
          (** build the corresponding labelled variable using [this] and [label]
              and *)
          let lv : lvar = { v = this; l = label } in
          (** add it to [aliases] while determining its number of levels of
              indirection. *)
          LVar_Hashtbl.add aliases lv (
              (- i), Apac_miscellaneous.typ_get_nli ty
            )
        ) siblings
    end;
  (** If the function is not a class member method, we must not constify it. It
      wouldn't be taken into account by the compiler, anyways. *)
  if not fcr.member then
    fcr.const <- false;
  (** Then, of course, we have to add the arguments of the function itself to
      [fcr] so as to be able to identify possible aliases to these variables
      within the body of the function during the analysis using the above local
      auxiliary function. For class member methods, we do not add the first
      argument which is [this], a variable referring to the parent class
      instance. Indeed, we have already added sibling class members [fcr]. *)
  let args = if fcr.member then List.tl args else args in
  List.iteri (fun i (v, ty) ->
      (** If an argument is a structure or a class, we have to recursively
          include its members (see [!arg_explode]). *)
      let lvs = arg_explode v ty in
      (** For now, we do not perform the below tests for structure and class
          members. *)
      let nli = if (is_reference ty) then (-1)
                else Apac_miscellaneous.typ_get_nli ty in
      List.iter (fun lv -> LVar_Hashtbl.add aliases lv (i, nli)) lvs
    ) args;
  (** Analyze the body of [f] for data dependencies on arguments and their
      aliases. *)
  aux aliases f body

(** [analyze us ous crs tg]: expects the target [tg] to point at a function
    definition. It scans the body of the target function definition for data
    dependencies on arguments and their aliases while filling up the
    unconstification stacks [us] and [ous] and updating the hash table of
    function constification records [crs] (see [!section:principle]). *)
let analyze (us : u) (ous : o) (crs : r) (tg : target) : unit =
  Target.iter (fun t p -> analyze_on us ous crs p (get_trm_at_path p t)) tg

(** [unconstify us ous]: unconstify (see [!section:principle]) arguments and
    functions, i.e. pass the [const] element of their constification records to
    [false], depending on the items in the unconstification stack [us], the
    object unconstification stack [ous] and the hash table of function
    constification records [crs]. *)
let unconstify (us : u) (ous : o) (crs : r) : unit =
  (** [unconstify.non_objects]: unconstifies functions and arguments, except for
      those representing objects. *)
  let rec non_objects () : unit =
    (** Pop out an element from [us]. *)
    match Stack.pop_opt us with
    | Some (f, i) ->
       (** Find the corresponding function constification record in [crs]. *)
       let cr = Var_Hashtbl.find crs f in
       (** If the argument's position is set to [-1], *)
       if i = -1 then
         (** it means that the function is a class member method and a class
             sibling is being modified within its body. Therefore, we must not
             constify the function. *)
         begin if cr.member then cr.const <- false end
       else
         begin
           (** Otherwise, try to get the corresponding argument constification
               record. *)
           if (Tools.Int_map.mem i cr.args) then
             begin
               let a = Tools.Int_map.find i cr.args in
               (** Then, if needed, *)
               if a.const then
                 begin
                   (** unconstify the argument *)
                   a.const <- false;
                   (** and push to [us] all of the arguments we should
                       unconstify by propagation. In other terms, follow the
                       dependencies. *)
                   Var_map.iter (
                       fun k e -> Stack.push (k, e) us
                     ) a.propagate;
                 end
             end
           else
             (** If it is not possible, fail. This is not normal! *)
             let error =
               Printf.sprintf
                 "Apac_constification.unconstify.non_objects: the \
                  constification record of `%s' has no argument constification
                  record for the argument on position `%d'."
                 (var_to_string f) i
             in
             failwith error
         end;
       (** Continue the unconstification process until *)
       non_objects ()
    (** there are no more elements in the unconstification stack [us]. *)
    | None -> ()
  in
  (** [unconstify.objects]: unconstifies function arguments representing
      objects. *)
  let rec objects () : unit =
    (** Pop out an element from [ous]. *)
    match Stack.pop_opt ous with
    | Some (tg, i, f) ->
       (** Find the constification record of the callee, i.e. [f]. *)
       let crf = Var_Hashtbl.find crs f in
       (** If it is a non-[const] class member method, *)
       if crf.member && (not crf.const) then
         begin
           (** find the constification record of the caller of [f], i.e. [tg],
               and *)
           let crtg = Var_Hashtbl.find crs tg in
           (** try to gather the corresponding argument constification
               record. *)
           if (Tools.Int_map.mem i crtg.args) then
             begin
               let a = Tools.Int_map.find i crtg.args in
               (** Then, if needed, unconstify the argument. *)
               if a.const then a.const <- false
             end
           else
             (** If it is not possible, fail. This is not normal! *)
             let error =
               Printf.sprintf
                 "Apac_constification.unconstify.objects: the constification \
                  record of `%s' has no argument constification record for the \
                  argument on position `%d'."
                 (var_to_string tg) i
             in
             failwith error
         end;
       (** Continue the unconstification process until *)
       objects ()
    (** there are no more elements in the unconstification stack [ous]. *)
    | None -> ()
  in
  non_objects ();
  objects ()

(** [constify_prototypes_on ?crs t]: see [!constify_prototypes]. *)
let constify_prototypes_on ?(crs : r option = None) (t : trm) : trm =
  (** Deconstruct the definition term [t] of the function [f]. *)
  let error = "Apac_constification.constify_prototypes_on: expected a target \
               to a function definition." in
  let (f, ret_typ, args, body) = trm_inv ~error trm_let_fun_inv t in
  match crs with
  (** If there are no constification records, *)
  | None ->
     (** constify all of the arguments as well as *)
     let args = List.map (fun (v, ty) -> (v, (typ_constify ty))) args in
     (** the function itself. *)
     trm_add_cstyle Const_method (
         trm_let_fun ~annot:t.annot f ret_typ args body
       )
  (** Otherwise, consult the constification record of [f] to find out which of
      its arguments we should constify, if any, and whether we should constify
      the function itself. *)
  | Some crs when (Var_Hashtbl.mem crs f) ->
     let crf = Var_Hashtbl.find crs f in
     (** If the function is a class member method, its first argument is the
         [this] variable referring to the parent class. Ignore it. *)
     let args' = if crf.member then List.tl args else args in
     (** Loop over the list of arguments and *)
     let args' = List.mapi (fun i (v, ty) ->
                     (** after checking whether there is an argument
                         constification record for each argument of [f], *)
                     if (Tools.Int_map.mem i crf.args) then
                       let cra = Tools.Int_map.find i crf.args in
                       if cra.const then
                         (** constify it if the records says so *)
                         (v, (typ_constify ty))
                       else
                         (** or leave it as is. *)
                         (v, ty)
                     else
                       (** If an argument constification record is missing,
                           fail. This is not normal! *)
                       fail t.loc
                         (missing_record "constify_prototypes_on.aux" f i)
                   ) args' in
     (** Rebuild the definition term [t] of [f] using the updated list of
         arguments [args']. However, we have to bring back [this] to the list of
         arguments. If [f] is a class member method, [this] is a non-empty
         list. *)
     let args = if crf.member then (List.hd args) :: args' else args' in
     let t = trm_let_fun ~annot:t.annot f ret_typ args body in
     (** Constify the function too if its constification record says so. *)
     if crf.const then trm_add_cstyle Const_method t
     else t
  (** If there is no constification record for [f], fail. This is not normal! *)
  | _ -> fail t.loc (missing_record "constify_prototypes_on.aux" f (-1))

(** [constify_prototypes ?crs tg]: expects the target [tg] to point at a
    function definition. Then, based on the constification record of the
    function in the hash table of function constification records [crs], it
    contifies selected arguments of the function and if the constification
    record says so, it constifies the function itself too.

    One can ignore, e.g. for testing purposes, the constification records and
    constify all of the function's arguments as well as the function itself. For
    this, skip the [crs] argument or set it to [None]. *)
let constify_prototypes ?(crs : r option = None) (tg : target) : unit =
  Target.apply_at_target_paths (constify_prototypes_on ~crs) tg

(** [constify_aliases_on ?force cm crs t]: see [!constify_aliases]. *)
let constify_aliases_on ?(cm : m option = None) ?(crs : r option = None)
      (t : trm) : trm =
  (** [constify_aliases_on.aux aliases t]: auxiliary function to recursively
      constify the variable declarations of all the [aliases] in an abstract
      syntax tree term [t]. *)
  let rec aux (aliases : l) (t : trm) : trm =
    match t.desc with
    (** When [t] is a compound, an interation or a selection statement involving
        substatements, loop over the latter. *)
    | Trm_seq _
      | Trm_for _
      | Trm_for_c _
      | Trm_if _
      | Trm_switch _
      | Trm_while _ -> trm_map (aux aliases) t
    (** When [t] is a declaration of a variable [tv] with an initialization term
        [ti], we have to update the list of [aliases] and constify the
        declaration of [tv] if it is an alias to a constant variable. *)
    | Trm_let (_, tv, { desc = Trm_apps (_, [ti]); _ }, _) ->
       (** At first, check whether we are declaring a reference. *)
       let r = trm_has_cstyle Reference t in
       (** Then, check whether we are declaring an alias to an argument or to an
           alias to an argument and return the alias type, i.e. [1] if it is a
           reference, [2] if it is a pointer and [0] if we are not creating an
           alias at all. *)
       let a = trm_let_update_aliases ~r tv ti aliases in
       begin
         match (a, tv) with
         (** Otherwise, we have to constify the alias and rebuild its
             declaration term. *)
         | (1, (v, ty)) ->
            let ty = typ_ref (typ_constify (get_inner_ptr_type ty)) in
            trm_let_mut (v, ty) ti
         | (2, (v, ty)) ->
            let ty = typ_constify (get_inner_ptr_type ty) in
            trm_let_mut (v, get_inner_const_type ty) ti
         (** If we are not creating an alias, there is nothing to do, return the
             declaration term [t] as is. *)
         | _ -> t
       end
    (** When [t] is a declaration of multiple variables [tvs] of kind [vk] with
        optional initialization terms [tis], we may have to update the list of
        [aliases] depending on [ti] and constify the declarations of [tvs]
        representing aliases to constant variables. *)
    | Trm_let_mult (vk, tvs, tis) ->
       (** At first, check whether we are declaring any aliases in [tvs] and
           return the test results in the form of a list [a] of booleans. *)
       let a = List.map2 (fun tv ti ->
                   (trm_let_update_aliases tv ti aliases) > 0
                 ) tvs tis in
       (** If all the elements of [a] are [false], *)
       if (List.for_all (fun e -> e = false) a) then
         (** there are no aliases to constant variables and there is nothing
             todo. Return [t] as is. *)
         t
       else
         (** If all the elements of [a] are [true] or if the hash table of
             multiple variable declaration [cm] is not present, *)
         if (List.for_all (fun e -> e = true) a) || cm = None then
           (** we can safely constify all the variable declarations in [t]. *)
           let tvs = List.map (fun (v, ty) -> (v, typ_constify ty)) tvs in
           trm_let_mult vk tvs tis
         else
           begin
             (** If only some of the elements of [a] are [true] but the inner
                 type of [t] is already constant, *)
             let (_, ty) = List.nth tvs 0 in
             if is_typ_const (get_inner_type ty) then
               (** we can complete the constification of selected variables
                   within [t] without splitting the multiple variable
                   declaration into  simple variable declarations. *)
               let tvs = List.map2 (
                             fun (v, ty) const ->
                             if const then (v, typ_constify ty) else (v, ty)
                           ) tvs a in
               trm_let_mult vk tvs tis
             else
               (** Otherwise, we have to split the multiple variable delcaration
                   into a sequence of simple variable declarations and constify
                   selected variables according to the values in [a]. However,
                   we cannot do this within this transformation. Therefore, we
                   mark the multiple variable declaration and keep track of it
                   in the hash table [cm] (see type [!type:m]) and process it
                   later in [!constify]. *)
               let mark = Apac_macros.const_mult_mark ^ (Mark.next ()) in
               Hashtbl.add (Option.get cm) mark a;
               (** At this stage, we leave [t] unchanged. We only mark it. *)
               Mark.trm_add_mark mark t
           end
    (** When [t] is none of the above, loop over its child elements. *)
    | _ -> trm_map (aux aliases) t
  in
  (** Deconstruct the definition term [t] of the function [f]. *)
  let error = "Apac_constification.constify_aliases_on: expected a target to a \
               function definition." in
  let (f, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (** Create a hash table of arguments and aliases to arguments. *)
  let aliases : l = LVar_Hashtbl.create 10 in
  (** If the hash table of function constification records [crs] and the hash
      table of multiple variable declarations [cm] are present, consult the
      constification record [crf] of [f] to find out which of its arguments have
      been constified, if any. Then, add them to [aliases] so the auxiliary
      function constifies all of their aliases within the body of [f] and stores
      multiple variable declarations needing partial constification to [cm].

      The principle we adopt here is the opposite of [!analyze]. In the latter,
      we use a hash table of arguments and aliases to arguments (see type
      [!type:l]) to keep track of arguments and aliases to arguments we must not
      constify whereas, here, we use them to keep track of those arguments that
      have been constified and their aliases to which we have to propagate the
      constification process. *)
  begin
    match (cm, crs) with
    | (Some cm, Some crs) ->
       (** Find the constification record [fcr] of [f] in [crs]. If it does not
           exist, fail. This is not normal! *)
       if not (Var_Hashtbl.mem crs f) then
         fail t.loc (missing_record "constify_aliases_on.aux" f (-1));
       let fcr = Var_Hashtbl.find crs f in
       (** If the function is a class member method, its first argument is the
           [this] variable referring to the parent class. Ignore it. *)
       let args' = if fcr.member then List.tl args else args in
       (** Otherwise,  *)
       (** Loop over the list of arguments and *)
       List.iteri (fun i (v, ty) ->
           (** after checking whether there is an argument constification record
               for each argument of [f], *)
           if (Tools.Int_map.mem i fcr.args) then
             begin
               (** gather the argument constification record [acr] of the [i]-th
                   argument of [f]. *)
               let acr = Tools.Int_map.find i fcr.args in
               (** If it has been constified and if it does not represent a
                   simple variable, *)
               if acr.const && acr.self <> Variable then
                 (** add it to [aliases]. *)
                 let lv : lvar = { v = v; l = String.empty } in
                 LVar_Hashtbl.add
                   aliases lv (i, Apac_miscellaneous.typ_get_nli ty)
             end
           else
             (** If an argument constification record is missing, fail. This is
                 not normal! *)
             fail t.loc (missing_record "constify_aliases_on.aux" f i)
         ) args'
    | _ ->
       (** If either [crs] or [cm] are not present, consider all the arguments
           of [f] as [const]. Add them to [aliases], except for [this] in the
           case of class member methods, so the auxiliary function constifies
           all of their aliases within the body of [f]. *)
       List.iteri (fun i (v, ty) ->
           if v.name <> "this" then
             let lv : lvar = { v = v; l = String.empty } in
             LVar_Hashtbl.add aliases
               lv (i, Apac_miscellaneous.typ_get_nli ty)
         ) args
  end;
  (** Constify the declarations of aliases in the body of [f]. *)
  let body = aux aliases body in
  (** Rebuild and return the definition term [t] of [f]. *)
  trm_let_fun ~annot:t.annot f ret_ty args body

(** [constify_aliases ?force cm crs tg]: expects target the target [tg] to point
    at a function definition. Then, based on the constification record of the
    function in the hash table of function constification records [crs], it
    constifies the aliases to arguments that have been constified. The hash
    table [cm] collects information about multiple variable declarations we must
    split into simple variable declarations before constifying them (see type
    [!type:m]).

    One can ignore, e.g. for testing purposes, the constification records and
    consider that all the function's arguments as well as the function itself
    have been constified and then constify all of the relevant aliases. For
    this, omit either of the [cm] or the [crs] arguments or set either of them
    to [None]. *)
let constify_aliases ?(cm : m option = None) ?(crs : r option = None)
      (tg : target) : unit =
  Target.apply_at_target_paths (constify_aliases_on ~cm ~crs) tg

(** [constify_let_mult_on ?cl t]: see [!constify_let_mult]. *)
let constify_let_mult_on ?(cl : bool list option = None) (t : trm) : trm =
  (** Deconstruct the target multiple variable declaration term [t] into a
      variable kind [vk], a list of typed variables [tvs] being declared and a
      list of their initialization terms [tis]. *)
  let error = "Apac_constification.constify_let_mult_on: expected a target to \
               a multiple variable declaration." in
  let (vk, tvs, tis) = trm_inv ~error trm_let_mult_inv t in
  (** There are as many elements in the boolean list [cl] as there are
      declarations in [tvs]. A [true] element in [cl] means we have to constify
      the corresponding variable declaration in [vs]. This is the role of the
      mapping below. However, if the [cl] argument is not present, we build [cl]
      full of [false] in order to be able to proceed with the transformation. *)
  let cl = match cl with
    | Some cl -> cl
    | None -> List.init (List.length tvs) (Fun.const false)
  in
  let tvs = List.map2 (fun (v, ty) const ->
                if const then (v, typ_constify ty) else (v, ty)
              ) tvs cl in
  (** Transform the multiple variable declaration into a sequence of simple
      variable declarations. *)
  let s = List.map2 (fun tv ti -> trm_let vk tv ti) tvs tis in
  (** Return a new sequence (without braces) containing the simple variable
      declarations. *)
  Syntax.trm_seq_no_brace s

(** [constify_let_mult cl tg]: expects target [tg] to point at a multiple
    variable declaration. Then, it constifies those variable declaration within
    the lattern for which the boolean list [cl] contains [true] and replaces the
    multiple variable declaration by a sequence of simple variable declarations
    to ensure a correct constification (see type [!type:m]).

    For example:

    {[
    int a = 1, b = a;
    ]}

    becomes:

    {[
    int a = 1;
    int b = a;
    ]}

    One can ignore, e.g. for testing purposes, the constification and perform
    the declaration unfolding only. For this, omit the [cl] argument or set it
    to [None]. It is also possible to force the constification of all variables
    in the multiple variable declaration by providing in [cl] a list of [true]s,
    but pay attention to its length! There must be as many [true]s in [cl] as
    there are declarations in the multiple variable declaration. *)
let constify_let_mult ?(cl : bool list option = None) (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
      Target.apply_at_target_paths (constify_let_mult_on ~cl) tg)

(** [constify ?frs ?trans tg]: expects target [tg] to point at all the function
    definitions in the input source code. It constifies the function's arguments
    and the function itself whenever it is possible and useful. If a hash table
    of function records is present in the argument [frs], update the records in
    the latter with the results of the constification pass. If the [trans]
    option, by default set to [true], is set to [false], the pass updates [frs]
    but preserves the input source code and does not insert any [const]
    qualifier in it. If [frs] is not present, the results of the constification
    pass are lost.

    For example, let us consider the short C program below defining a function
    [add].

    {[
    void add(int * a, int * b, int c) {
      a[0] = a[0] + b[0] + c;
    }
    ]}

    The arguments [a] and [b] of [add] are pointers to non-[const] data. The
    function overwrites the content at the memory location in [a], but it does
    not modify the contents behind [b] and [c] is not a pointer. Therefore, the
    transformation concludes that it is safe to constify [b]:

    {[
    void add(int * a, const int * b, int c) {
      a[0] = a[0] + b[0] + c;
    }
    ]}

    Let us now analyze a more complex situation:

    {[
    int * g(int * out, int * in, int n) {
      for(int i = 0; i < n; i++)
        out[i] *= in[i];
      return out;
    }
    int * f(int * data, int * result) {
      result = g(result, data);
      return result;
    }
    ]}

    The above short C program defines two functions [g] and [f]. [g] has three
    formal arguments [out], [in] and [n]. [out] and [in] are pointers to
    non-[const] data and [n] is an integer. The function multiplies the elements
    of [out] with the elements of [in] while storing the result in [out]. [f]
    has two formal arguments [data] and [result] pointing to non-[const] data.
    It calls [g] while passing it [result] and [data] as arguments. At first, in
    the case of [g], the constification pass concludes that the argument [in]
    can be safely constified, but not [out], which is overwritten by the
    function. However, [f] calls [g] and passes [result] and [data] as actual
    arguments to [g]. We already know that the the corresponding formal
    arguments [out] and [in] of [g] are pointers to non-[const] and [const]
    data, respectively. Therefore, in the case of [f], the pass concludes that
    [data] can be safely constified, but not [result], pointing to data modified
    in [g]:

    {[
    int * g(int * out, const int * in, int n) {
      for(int i = 0; i < n; i++)
        out[i] *= in[i];
      return out;
    }
    int * f(const int * data, int * result) {
      result = g(result, data);
      return result;
    }
    ]} *)
let constify ?(frs : Apac_records.FunctionRecord.t Var_Hashtbl.t option = None)
      ?(trans : bool = true) (tg : target) : unit =
  (** Create a hash table of function constification records with an initial
      size of 10 entries. *)
  let crs : r = Var_Hashtbl.create 10 in
  (** Build constification records for all the function definitions in the input
      source code. *)
  build_records crs tg;
  (** Create unconstification stacks. *)
  let us : u = Stack.create () in
  let ous : o = Stack.create () in
  (** Analyze the functions definitions and determine which arguments and
      function, in the case of class member methods, we can constify. *)
  analyze us ous crs tg;
  (** After initially considering all arguments and functions as constifiable,
      unconstify those we cannot constify based on data accesses. *)
  unconstify us ous crs;
  if trans then
    begin
      (** Add the [const] keyword to the function prototypes based on the above
          analysis and unconstification passes. *)
      constify_prototypes ~crs:(Some crs) tg;
      (** Create a hash table, with an initial size of 10 entries, for multiple
          variable declarations subject to partial constification. *)
      let cm : m = Hashtbl.create 10 in
      (** Constify aliases to arguments or to previously declared aliases. *)
      constify_aliases ~cm:(Some cm) ~crs:(Some crs) tg;
      (** Partially constify the multiple variable declarations from [cm]. *)
      Hashtbl.iter (fun k cl ->
          constify_let_mult ~cl:(Some cl) (tg @ [cMark k])
        ) cm
    end;
  (** Propagate the results of the constification to the function records in
      [frs], if any. *)
  match frs with
  | Some frs ->
     (** For each constification record [cr] of a function [f], *)
     Var_Hashtbl.iter (fun f cr ->
         (** determine which arguments of [f] we have constified and retrieve
             the information in the form of a list of booleans [cl] with [true]
             for each [const] argument and [false] for each non-[const]
             argument, *)
         let cl =
           Tools.Int_map.fold (fun _ (acr : a) acc ->
               acc @ [acr.const]
             ) cr.args [] in
         (** find the function record [fr] of [f] in [frs] and *)
         if (Var_Hashtbl.mem frs f) then
           let fr = Var_Hashtbl.find frs f in
           (** update the access classification of [f]'s arguments [fr]. *)
           fr.args <- Apac_records.FunctionRecord.constify cl fr.args;
         else
           (** If [fr] does not exist, fail. *)
           let error = Printf.sprintf
                         "Apac_constification.constify: function `%s' has no \
                          function record." (var_to_string f) in
           failwith error           
       ) crs
  (** Otherwise, do nothing. *)
  | None -> ()
