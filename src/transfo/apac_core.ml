open Ast
open Typ
open Trm
open Target
open Path
open Apac_modules
open Apac_tasks

(*****************************)
(* PART I: TYPE DECLARATIONS *)
(* I.1 Constification        *)
(*****************************)

(* [const_arg]: an argument constification record. *)
type const_arg = {
    (* Tells whether the argument is a reference or a pointer. *)
    is_ptr_or_ref : bool;
    (* Tells whether the argument can be constified. *)
    mutable is_const : bool;
    (* If the argument is a reference or a pointer, i.e. when [is_ptr_or_ref] is
       true, and if the value in memory it refers or points to is modified
       within the body of the associated function, the argument must not be
       constified.
       
       To explain [to_unconst_by_propagation] let us consider a function [g]
       defined as follows:
       
         void g(int &val) { val += 4; }
       
       and a function [f] defined as follows:
       
         int f(int a, int b) { g(b); return a + b; }
       
       In [g], [val] is a reference and the referenced value is modified within
       the function's body. Therefore, the argument [val] must not be
       constified. However, [f] calls [g] and passes one of its arguments, i.e.
       [b], by reference to [g]. This way, the value referenced by [b] will be
       modified within [g]. We already know that [val] in [g] must not be
       constified. Because of this dependency, we must propagate this decision
       also to [b] in [f]. To achieve this and keep track of the dependency, we
       will add:
       
         (f, 1)
       
       to the [to_unconst_by_propagation] map (function variable [var] ->
       0-based argument position [int]) in the [const_arg] record in [g]. *)
    mutable to_unconst_by_propagation : int Var_map.t;
  }

(* [const_fun]: a function constification record. *)
type const_fun = {
    (* Map (0-based argument position -> constification record) of
       constification records for all the arguments of the function. *)
    const_args : const_arg Int_map.t;
    (* Tells whether the function can be constified. Note that this information
       is relevant only if the function is a class member method. *)
    mutable is_const : bool;
    (* Tells whether the return value is a pointer. *)
    is_ret_ptr : bool;
    (* Tells whether the return value is a reference. *)
    is_ret_ref : bool;
    (* Tells whether the function is a class member method. *)
    mutable is_class_method : bool;
    (* Augmented AST of the function's body storing data dependencies of its
       instructions. See [atrm] below. *)
    mutable task_graph : TaskGraph.t option;
    (* Hash table ([var] -> [int]) of locally-defined symbols, including
       arguments, storing their pointer degree. See [symbols] below. *)
    variables : symbols;
  }

(* [const_funs]: type for a hash table of [const_fun]. The keys are functions
   represented by terms of type [var]. *)
and const_funs = const_fun Var_Hashtbl.t

(* [symbols]: type for a hash table of symbols local to a given function
   definition (including the arguments, but excluding the name of the function
   itself). The table associates the varibles to their pointer degree.

   For example, in the case of the following function definition,

   void f(int a, int * tab) {
   float b;
   void ** data;
   ...
   } 

   [variables] would contain:

   a, 0
   tab, 1
   b, 0
   data, 2 *)
and symbols = int Var_Hashtbl.t

(* [const_aliases]: type for hash table of argument aliases.

   Pointers and references used within a function definition might be linked to
   the same data as the arguments of the function, i.e. they represent aliases
   to the function's arguments. Therefore, when we constify an argument, we must
   constify its aliase(s) too. To keep trace of argument aliases, we use a hash
   table data type where the key is the [lvar] of the alias and the value is a
   pair of two [int] elements. The first [int] element corresponds to the
   0-based position of the function argument being aliased. The second [int]
   element gives the pointer degree of the alias, if the latter is a pointer,
   e.g. the pointer degree of [int ** tab] is 2. *)
and const_aliases = (int * int) LVar_Hashtbl.t

(* [const_unconst]: type of stack of function arguments, except for objects,
   that must not be constified.

   At the beginning of the constification process, we assume that every function
   argument in every function can be constified, see
   [build_constification_records]. Then, we perform an analysis of dependencies
   between function arguments and write operations involving the latter within
   the body of corresponding functions, see [identify_mutables]. When the
   analysis concludes that a function argument is written to, it shall modify
   the associated constification record so as to mark the argument as
   non-consitifiable. The same goes also for class methods modifying sibling
   class members. Such methods should not be consitified either. See [const_arg]
   and [const_fun] for more details. 

   However, the constification records are not modified directly during the
   analysis. Instead, we use a stack keeping trace of functions arguments and
   functions that shall be unconstified once the analysis terminates, see
   [to_unconst] below. The elements of the stack are pairs of the [var]
   identifying the target function and the [int] identifying the position of its
   argument to unconstify. If the latter is set to -1, it means that the
   function itself should be unconstified. See
   [unconstify_mutables.unconstify_to_unconst_objects]. *)
and const_unconst = (var * int) Stack.t

(* [const_unconst_objects]: type of stack of function arguments, represented by
   objects, that must not be constified. 

   When an object is passed as argument to a function, it must not be constified
   if it is used to call a non-const class method which may modify one or more
   members of the class. However, this information is not know before the
   elements in a [const_unconst] stack are processed (see
   [unconstify_mutables]). This is why a second phase is necessary to process
   the elements of a [const_unconst_objects] stack (see [unconstify_mutables]).
   Here, an element is represented by a ([var], [int], [var]) triplet where the
   first (a [var]) element identifies the target function, the second (an [int])
   element gives the 0-based position of the that function's argument to
   unconstify and the third (another [var]) element represents the class member
   method called on that argument. Then, during the unconstification process
   (see [unconstify_mutables]), if the method represented by the third element
   ([var]) gets unconstified, the argument (the second element, the [int]) of
   the function targeted by the first [var] will be unconstified too. *)
and const_unconst_objects = (var * int * var) Stack.t

(* [const_mult]: type of hash table for multiple variable declarations that must
   be transformed into sequences of simple variable declarations while
   constifying one or more of the variables being declared.

   When a function argument is constified, it is necessary to propagate the
   constification to the aliases of the argument as well. This is done with the
   [Apac_basic.constify_arguments] function. This process is straightforward in
   the case of simple variable declarations. However, if an argument alias is
   declared within a multiple variable declaration, we have two different
   situations to consider: 

   1) the inner type of the multiple variable declaration is already constant:
   
   // [i] is a constant integer, [j] is a mutable pointer to a constant
   // integer
   int const i, * j;

   2) the inner type of the multiple variable declaration is not constant:

   // [i] is a mutable integer, [j] is a mutable pointer to a mutable integer
   int i, * j;

   If we want to constify [j] in 1), it is possible without breaking the parent
   multiple variable declaration:

   int const i, * const j;

   In 2) we cannot fully constify [j], i.e. [int const * const j], without
   constifying [i], which is not always desirable. In this case, we have to
   split the parent multiple variable declaration into a sequence of simple
   variable declarations and then constify the declaration of [j] only. However,
   this transformation cannot be directly applied within
   [Apac_basic.constify_aliases_on]. Indeed, the introduction of a new sequence
   of instructions changes the scope of the declared variables. This can be
   prevented, but not in a term to term transformation function such as
   [Apac_basic.constify_aliases_on]. It must be done in a transformation
   function modyfing the AST by side-effect, i.e. a target to unit
   transformation, in which we can call [Nobrace_transfo.remove_after] to
   effectively remove the braces from the sequence of simple variable
   declarations so as to preserve their initial scope. Therefore,
   [Apac_basic.constify_aliases_on] should only mark those multiple variable
   declarations which need to be split into simple variable declarations and
   determine which declarations should be constified. To keep track of this
   information, so we can actually perform the transformations (using
   [Apac_basic.unfold_let_mult]), we use a hash table where the keys are the
   marks (strings) added to the concerned multiple variable declarations and
   where the values are lists of booleans. The latter have as many elements as
   there are declarations in the target multiple variable declaration. In other
   terms, a boolean value is associated to each variable declaration. If the
   value is [true], it means that the corresponding variable declaration should
   be constified. *)
and const_mult = (mark, bool list) Hashtbl.t

(******************************************************************)
(* PART II: DECLARATION AND/OR INITIALIZATION OF GLOBAL VARIABLES *)
(* II.1 Constification                                            *)
(******************************************************************)

(* Create our hash table of [const_fun] with an initial size of 10. The size of
   the table will grow automatically if needed. *)
let const_records : const_funs = Var_Hashtbl.create 10

(* Create a stack of arguments that must not be constified. See [const_unconst]
   for more details. *)
let to_unconst : const_unconst = Stack.create ()
let to_unconst_objects : const_unconst_objects = Stack.create ()

(* [const_mult]: hash table for multiple variable declarations that must be
   transformed into sequences of simple variable declarations while constifying
   one or more of the variables being declared. See [const_mult]. *)
let to_const_mult : const_mult = Hashtbl.create 10

(**********************)
(* II.2 Taskification *)
(**********************)

(* [task_group_mark]: string used to mark instruction sequences targeted by task
   group insertion. See [Apac_basic.task_group] and
   [Apac_basic.use_goto_for_return]. *)
let task_group_mark : mark = "__apac_task_group"

(***********************)
(* II.2 Pre-processing *)
(***********************)

(* [goto_label]: label used when replacing return statements by gotos within
   the pre-processing stage. See [Apac_basic.use_goto_for_return]. *)
let goto_label : label = "__apac_exit"

(******************************)
(* PART III: HELPER FUNCTIONS *)
(* III.1 General-purpose      *)
(******************************)

(* [typ_is_alias ty]: checks if [ty] is a user-defined alias to a basic type. *)
let typ_is_alias (ty : typ) : bool =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias _ -> true
      | _  -> false
      end
    | None -> false
    end
  | _ -> false

(* [typ_get_alias ty]: if [ty] is a user-defined alias to a basic type, it
   returns the latter. *)
let typ_get_alias (ty : typ) : typ option =
  match ty.typ_desc with
  | Typ_constr (_, id, _) ->
    begin match Context.typid_to_typedef id with
    | Some (td) ->
      begin match td.typdef_body with
      | Typdef_alias ty -> Some (ty)
      | _  -> None
      end
    | None -> None
    end
  | _ -> None


(* [typ_get_degree ty]: computes and returns the pointer degree of the type
   [ty]. For example, for [int ** a], it returns 2. *)
let typ_get_degree (ty : typ) : int =
  (* Auxiliary function to actually compute the degree. *)
  let rec aux (degree : int) (ty : typ) : int =
    match ty.typ_desc with
    (* If [ty] is a constant type or a reference, keep the current degree value
       and recurse. *)
    | Typ_const ty -> aux degree ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux degree ty
    (* If [ty] is a pointer or an array, increase the degree value and
       recurse. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (degree + 1) ty
    | Typ_array (ty, _) -> aux (degree + 1) ty
    (* If [ty] is a constructed user-defined type and it is an alias to a basic
       type, resolve the latter and compute the degree. *)
    | Typ_constr _ when typ_is_alias ty ->
      begin match typ_get_alias ty with
      | Some (ty) -> aux degree ty
      | None -> fail None "Apac_core.typ_get_degree: unable to determine the \
                           basic type of a typedef alias."
      end
    | _ -> degree
  in
  (* Call the auxiliary function to compute the degree and hide the [degree]
     parameter to the outside world. *)
  aux 0 ty

(* [trm_strip_accesses_and_references_and_get_lvar t]: strips [*t, &t, ...]
   recursively and if [t] is a variable, it returns the associated labelled
   variable. *)
let trm_strip_accesses_and_references_and_get_lvar (t : trm) : lvar option =
  (* Internal auxiliary recursive function allowing us to hide the [l] parameter
     to the outside world. *)
  let rec aux (l : label) (t : trm) : lvar option =
    match t.desc with
    (* [t] is a unary operation *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin
         match op with
         (* Whenever we stumble upon a structure access or get operation, we
            extract the label of the structure field involved in the
            operation. *)
         | Unop_struct_access field -> aux field t
         | Unop_struct_get field -> aux field t
         (* In case of another unary operations, we simply recurse on the
            internal term. *)
         | _ -> aux l t
       end
    (* [t] is a binary operation corresponding to an array access *)
    | Trm_apps ({ desc = Trm_val (
                             Val_prim (Prim_binop array_access)
                             (* We continue to recurse on the internal term. *)
                           ); _ }, [t; _]) -> aux l t
    (* [t] actually leads to a variable *)
    | Trm_var (_, var) ->
       (* Use [var] and the label [l] to build the associated labelled
          variable and return it. *)
       let lv : lvar = { v = var; l = l } in Some lv
    | _ -> None
  in
  aux "" t

(* [trm_can_resolve_pointer t]: tries to resolve operation [t] to unique
   variable and returns [true] on success and [false] otherwise. *)
let rec trm_can_resolve_pointer (t : trm) : bool =
    match t.desc with
    (* [t] is unary operation: strip and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       | Unop_get
         | Unop_address
         | Unop_cast _ -> trm_can_resolve_pointer t
       | _ -> false
       end
    (* [t] is a binary operation corresponding to an array access: strip and
       recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> trm_can_resolve_pointer t
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (trm_can_resolve_pointer lhs) || (trm_can_resolve_pointer rhs)
    (* [t] actually leads to a variable: success. Return [true]. *)
    | Trm_var _ -> true
    | _ -> false

(* [trm_can_resolve_pointer t]: tries to resolve operation [t] to unique
   variable. It then returns the latter on success and [None] otherwise. *)
let trm_resolve_pointer_and_degree (t : trm) : (var * int) option =
  let rec aux (degree : int) (t : trm) : (var * int) option =
    match t.desc with
    (* [t] is unary operation: strip and recurse. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) ->
       begin match op with
       | Unop_get -> aux (degree - 1) t'
       | Unop_address -> aux (degree + 1) t'
       | Unop_cast ty -> aux (degree + typ_get_degree ty) t'
       | Unop_struct_access _
         | Unop_struct_get _ -> aux degree t'
       | _ -> None
       end
    (* [t] is a binary operation corresponding to an array access: strip and
       recurse. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t'; _]) -> aux (degree - 1) t'
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (* We continue to recurse on both the left and the right internal
          terms. *)
       begin
         match (aux degree lhs, aux degree rhs) with
         | Some (res), None -> Some (res)
         | None, Some (res) -> Some (res)
         | None, None
           (* In practice, binary operations between two pointers supported in
              C/C++ can not lead to a valid alias of one of them. *)
           | Some (_), Some (_) -> None
       end
    (* [t] actually leads to a variable. Return it. *)
    | Trm_var (vk, v) -> Some (v, degree)
    (* In all the other cases, return [None]. *)
    | _ -> None
  in
  aux 0 t

(************************)
(* III.2 Constification *)
(************************)

(* [typ_constify ty]: constifies [ty] by applying the 'const' keyword wherever
   it is possible. *)
let typ_constify (ty : typ) : typ =
  (* Aliases for often referenced values *)
  let annot = ty.typ_annot in
  let attributes = ty.typ_attributes in
  (* Auxiliary function to recursively constify [ty], e.g. 'int * a' becomes
     'const int * const a'. *)
  let rec aux (ty : typ) : typ =
    match ty.typ_desc with
    (* [ty] is a pointer. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (* [ty] is a constant pointer. *)
    | Typ_const { typ_desc =
                    Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty };
                  typ_annot = annot;
                  typ_attributes = attributes } ->
       typ_const (typ_ptr ~annot ~attributes Ptr_kind_mut (aux ty))
    (* [ty] is a user-defined constructed type. *)
    | Typ_constr (_, id, _) ->
       begin match Context.typid_to_typedef id with
       (* [ty] is a 'typedef' declaration. *)
       | Some td ->
          begin match td.typdef_body with
          (* If the constructed type is an alias to another type, recurse. *) 
          | Typdef_alias ty -> aux ty
          (* Otherwise, constify the constructed type and return. *)
          | _ -> typ_const ty
          end
       (* [ty] is not a 'typedef' declaration. *)
       | None -> typ_const ty
       end
    (* [ty] is already a constant type. There is nothing to do. *)
    | Typ_const _ -> ty
    (* Deal with any other case. *)
    | _ -> typ_const ty
  in
  (* Here begins the main entry point of the function, from where the auxiliary
     function is called. *)
  match ty.typ_desc with
  (* [ty] is a reference. *)
  | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
    begin match ty.typ_desc with
    (* [ty] is an rvalue reference, i.e. '&&' is used. *)
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } ->
      typ_lref ~annot ~attributes (aux ty)
    (* [ty] is a simple reference. *)
    | _ -> typ_ref ~annot ~attributes (aux ty)
    end
  (* [ty] is of any other type. *)
  | _ -> aux ty

(* [trm_resolve_binop_lval_and_get_with_deref] tries to resolve the variable
   behind an lvalue and check whether it has been dereferenced, i.e. following
   an array access or the use of [*]. Upon success, it returns the corresponding
   labelled variable. See [LVar] for more details on labelled variables. *)
let trm_resolve_binop_lval_and_get_with_deref (t : trm) : (lvar * bool) option =
  let rec aux (dereferenced : bool) (l : label) (t : trm) :
            (lvar * bool) option =
    match t.desc with
    (* We have found the variable, build and return the resulting labelled
       variable. *)
    | Trm_var (_, var) ->
       let lv : lvar = { v = var; l = l } in Some (lv, dereferenced)
    (* [t] is an array access, which means that the operand was dereferenced.
       Continue resolution on the latter. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux true l t
    (* [t] is a unary operation. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (op))); _ }, [t]) ->
       begin
         match op with
         (* A get operation, e.g. [*operand], as well as a structure access,
            e.g. [operand.field], both imply that the operand was dereferenced.
            Continue resolution on the latter. *)
         | Unop_get -> aux true l t
         | Unop_struct_access field -> aux true field t
         (* A structure access through pointer, e.g. [operand->field], means
            that the operand was not dereferenced. To finish finished resolving,
            iterate once more on [t]. *)
         | Unop_struct_get field -> aux dereferenced field t
         (* In case of another binary operation, do nothing and continue
            resolution on the operand. *)
         | _ -> aux dereferenced l t
       end
    | _ -> None
  in
  aux false "" t

(* [trm_resolve_var_in_unop_or_array_access_and_get t] tries to resolve the
   variable (including the associated label if we are dealing with a class
   member variable) involved in a unary operation (++, --, & or get) or array
   access [t] and return it in a form of a labelled variable. *)
let trm_resolve_var_in_unop_or_array_access_and_get (t : trm) : lvar option =
  (* Simply recurse over unary operations and array accesses. *)
  let rec aux (l : label) (t : trm) : lvar option =
    match t.desc with
    (* [t] is a unary operation *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [term]) ->
       begin
         match op with
         (* Whenever we stumble upon a structure access or get operation, we
            extract the label of the structure field involved in the
            operation. *)
         | Unop_struct_access field -> aux field term
         | Unop_struct_get field -> aux field term
         (* When [t] is a get, an [&] operation, a prefix or a postfix unary
            operation ([++i] or [i++]), we continue to recurse on the internal
            term. *)
         | Unop_get
           | Unop_address -> aux l term
         | _ when (is_prefix_unary op || is_postfix_unary op) -> aux l term
         (* Otherwise, there is nothing to resolve. *)
         | _ -> None
       end
    (* [t] is a binary operation corresponding to an array access *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            (* We continue to recurse on the internal term. *)
            _}, [term; _]) -> aux l term
    (* [t] actually leads to a variable *)
    | Trm_var (_, var) ->
       (* Use [var] and the label [l] to build the associated labelled
          variable and return it. *)
       let lv : lvar = { v = var; l = l } in Some lv
    | _ -> None
  in
  aux "" t

(* [trm_resolve_pointer_and_aliased_variable t aliases]: tries to resolve
   pointer operation [t] and checks in [aliases] whether the resulting pointer
   is an argument or an alias to an argument. If the pointer operation succeedes
   and if the resulting pointer is an argument or an alias to an argument, the
   function returns the labelled variable corresponding to the resulting pointer
   as well as the 0-based position of the aliased argument. *)
let trm_resolve_pointer_and_aliased_variable
      (t : trm) (aliases : const_aliases) : (lvar * int) option =
  (* Simply recurse over different kinds of operations. *)
  let rec aux (degree : int) (l : label) (t : trm) : (lvar * int) option =
    match t.desc with
    (* [t] is a unary operation *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
         (* When it is a get operation such as [*i], the pointer degree
            decreases. *)
       | Unop_get -> aux (degree - 1) l t
       (* When it is an [&] operation, the pointer degree raises. *)
       | Unop_address -> aux (degree + 1) l t
       (* When it is a cast operation, the pointer degree is represented by the
          sum of the current degree and the degree of the target type. *)
       | Unop_cast ty -> aux (degree + typ_get_degree ty) l t
       (* Whenever we stumble upon a structure access or get operation, we
          extract the label of the structure field involved in the operation.
          However, the degree remains the same. *)
       | Unop_struct_access field -> aux degree field t
       | Unop_struct_get field -> aux degree field t
       (* Otherwise, there is nothing to resolve. *)
       | _ -> None
       end
    (* [t] is a binary operation corresponding to an array access *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            (* We continue to recurse on the internal term. *)
            _ }, [t; _]) -> aux (degree - 1) l t
    (* [t] is a binary operation of another type *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (* We continue to recurse on both the left and the right internal
          terms. *)
       begin match (aux degree l lhs, aux degree l rhs) with
       | Some (res), None -> Some (res)
       | None, Some (res) -> Some (res)
       | None, None
         (* In practice, binary operations between two pointers supported in
            C/C++ can not lead to a valid alias of one of them. *)
         | Some (_), Some (_) -> None
       end
    (* [t] actually leads to a variable *)
    | Trm_var (_, v) ->
       (* Use [var] and the label [l] to build the associated labelled
          variable. *)
       let lv : lvar = { v = v; l = l } in
       (* Check if its an argument or an alias to an argument, then return the
          labelled variable as well as the corresponding 0-based position of the
          aliased argument. *)       
       begin match LVar_Hashtbl.find_opt aliases lv with
       | Some (aliased, deg) when (degree + deg) > 0 -> Some (lv, aliased)
       (* Otherwise, there is nothing to return. *)
       | _ -> None
       end
    | _ -> None
  in
  aux 0 "" t

(* [trm_let_update_aliases ?reference tv ti aliases]: checks in [aliases]
   whether the variable declaration specified by the typed variable [tv] and the
   initializaion term [ti] creates an alias to an already existing variable or
   function argument. If it is the case, it updates the alias hash table
   [aliases] accordingly and returns [1] if [tv] is a reference and [2] if [tv]
   is a pointer. Otherwise, it does nothing and returns [0].

   Note that when the function is applied on the elements of a simple variable
   declaration, i.e. a [trm_let], the [is_reference] function used to check
   whether the new variable is a reference has no effect. This is due to
   differences in representing simple [trm_let] and multiple [trm_let_mult]
   variable declarations in OptiTrust. In this case, the optional [reference]
   parameter can be used to ensure the test evaluates correctly. See a usage
   example in [const_compute_one]. *)
let trm_let_update_aliases ?(reference = false)
      (tv : typed_var) (ti : trm) (aliases : const_aliases) : int =
  (* Deconstruct the typed variable *)
  let (v, ty) = tv in
  (* and build the corresponding labelled variable. *)
  let lv : lvar = { v = v; l = String.empty } in
  (* If we are working with a reference, *)
  if is_reference ty || reference then
    begin
      (* we need to go through the access and reference operations to obtain the
         variable we might be aliasing in the form of a labelled variable. *)
      match (trm_strip_accesses_and_references_and_get_lvar ti) with
      | Some ti_lvar ->
         (* check whether it represents an alias to an existing variable or
            argument *)
         if LVar_Hashtbl.mem aliases ti_lvar then
           begin
             (* and if it is the case, create a new entry in [aliases] to keep
                trace of it. *)
             let (aliased, _) = LVar_Hashtbl.find aliases ti_lvar in
             (* Note that references are of pointer degree 0. *)
             LVar_Hashtbl.add aliases lv (aliased, 0);
             (* Return 1 because declared variable is a reference. *)
             1
           end
             (* There is nothing to do, return 0. *)
         else 0
      (* There is nothing to do, return 0. *)
      | None -> 0
    end
      (* If we are working with a pointer, *)
  else if is_typ_ptr (get_inner_const_type (get_inner_ptr_type ty)) then
    begin
      (* we have to go through all of the potential pointer operations within
         the initialization term and try to determine if they lead to a pointer
         variable which aliases an existing variable or argument. *)
      match (trm_resolve_pointer_and_aliased_variable ti aliases) with
      (* If it is the case, create a new entry in [aliases] to keep trace of
         it. *)
      | Some (_, aliased) ->
         begin
           LVar_Hashtbl.add aliases lv (aliased, typ_get_degree ty);
           (* Return 2 because declared variable is a pointer. *)
           2
         end
      (* There is nothing to do, return 0. *)
      | None -> 0
    end
      (* There is nothing to do, return 0. *)
  else 0

(* [find_parent_typedef_record p]: goes back up the path [p] and returns the
   first term corresponding to a class or a structure definition, if any. We use
   this function to determine the parent class of a structure or a function in
   order to access to the member variables of that class or structure. *)
let find_parent_typedef_record (p : path) : trm option =
  (* We shall go back on our steps in the path, i.e. in the direction of the
     root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (* We use an auxiliary function in order to hide to the outside world the need
     for the path reversal. *)
  let rec aux (p : path) : trm option =
    (* The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (* and if it detects a class or a structure definition, it returns
            it. *)
         (* FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = target_of_path (List.rev p) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_typedef { typdef_body = Typdef_record _; _ } -> Some (t)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed

(* [unconstify_mutables] proceeds in two phases, i.e. [unconstify_to_unconst]
   and [unconstify_to_unconst_objects]. At first, it pops out the elements from
   [to_unconst] and then from [to_unconst_objects] (global variables, see Part
   II.1) one by one and propagates the unconstification through the concerned
   function constification records in [const_records] (global variable, see Part
   II.1). *)
let unconstify_mutables () : unit =
  (* [unconstify_to_unconst] propagates the unconstification through the
     concerned function constification records but it does not apply to
     arguments representing objects. Indeed, such an argument cannot be
     constified if it is used to call a non-const class method which may modify
     one or more members of the class. However, this information is not know
     before the end of the unconstification propagation, i.e. before returning
     from [unconstify_to_unconst]. This is why a second phase is necessary, see
     [unconstify_to_unconst_objects]. *)
  let rec unconstify_to_unconst () : unit =
    (* Pop out an element from [to_unconst]. *)
    match Stack.pop_opt to_unconst with
    | Some (fn, ar) ->
       (* Find the corresponding function constification record in
          [const_records]. *)
       let const_record = Var_Hashtbl.find const_records fn in
       (* If the argument position is set to -1, *)
       if ar = -1 then
         (* it means that the function is a class member method and a class
            sibling is being modified within its body. Therefore, the function
            must not be consitifed. *)
         begin
           if const_record.is_class_method then const_record.is_const <- false
         end
       else
         begin
           (* Otherwise, we try to gather the corresponding argument
              constification record. *)
           if (Int_map.mem ar const_record.const_args) then
             begin
               let arg = Int_map.find ar const_record.const_args in
               (* Then, if needed, *)
               if arg.is_const then
                 begin
                   (* we unconstify the argument *)
                   arg.is_const <- false;
                   (* and push to [to_unconst] all of the function arguments
                      that should be unconstified by propagation. In other
                      terms, we need to follow the dependencies too. *)
                   Var_map.iter (
                       fun k e -> Stack.push (k, e) to_unconst
                     ) arg.to_unconst_by_propagation;
                 end
             end
           else
             (* If it is not possible, fail. This is not normal! *)
             begin
               let error =
                 Printf.sprintf
                   "Apac_core.unconstify_mutables.unconstify_to_unconst: \
                    the constification record of '%s' has no argument \
                    constification record for the argument on position '%d'."
                   (var_to_string fn)
                   ar
               in
               fail None error
             end
         end;
       (* Recurse. *)
       unconstify_to_unconst ()
    (* When the stack is empty, stop and return. *)
    | None -> ()
  in
  (* [unconstify_to_unconst_objects] propagates the unconstification through the
     concerned function constification records and applies exclusively to
     arguments representing objects. If such an argument is used to call a
     non-const class method which may modify one or more members of the class,
     the argument is unconstified. *)
  let rec unconstify_to_unconst_objects () : unit =
    (* Pop out an element from [to_unconst_objects]. *)
    match Stack.pop_opt to_unconst_objects with
    | Some (fn, ar, ff) ->
       (* Find the constification record of the function that has been called,
          i.e. [ff] . *)
       let const_record_ff = Var_Hashtbl.find const_records ff in
       (* If it is a class member method and it has been unconstified in the
          previous phase, *)
       if const_record_ff.is_class_method && not const_record_ff.is_const then
         begin
           (* find the constification record of the function that has called
              [ff], i.e. [fn], and *)
           let const_record_fn = Var_Hashtbl.find const_records fn in
           (* try to gather the corresponding argument constification record. *)
           if (Int_map.mem ar const_record_fn.const_args) then
             begin
               let arg = Int_map.find ar const_record_fn.const_args in
               (* Then, if needed, *)
               if arg.is_const then
                 begin
                   (* we unconstify the argument *)
                   arg.is_const <- false;
                 end
             end
           else
             (* If it is not possible, fail. This is not normal! *)
             begin
               let error =
                 Printf.sprintf
                   "Apac_core.unconstify_mutables.\
                    unconstify_to_unconst_objects: the constification record \
                    of '%s' has no argument constification record for the \
                    argument on position '%d'."
                   (var_to_string fn)
                   ar
               in
               fail None error
             end
         end;
       (* Recurse. *)
       unconstify_to_unconst_objects ()
    (* When the stack is empty, stop and return. *)
    | None -> ()
  in
  unconstify_to_unconst ();
  unconstify_to_unconst_objects ()

(***********************)
(* III.3 Taskification *)
(***********************)

(* [find_parent_function p]: goes back up the path [p] and looks for the first
   term corresponding to a function definition. If a function definition is
   found, it returns the name of the function as a variable. We use
   [find_parent_function] to determine the parent function of a task group
   sequence in order to access its constification record in [const_funs]. *)
let find_parent_function (p : path) : var option =
  (* We shall go back on our steps in the path, i.e. in the direction of the
     root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (* We use an auxiliary function in order to hide to the outside world the need
     for the path reversal. *)
  let rec aux (p : path) : var option =
    (* The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (* and if it detects a function definition, it returns it. *)
         (* FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = target_of_path (List.rev p) in
         let t = get_trm_at_exn tg in
         match t.desc with
         | Trm_let_fun (v, _, _, _, _) -> Some (v)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed

let emit_omp_task (t : Task.t) : trms =
  if t.wait then
    begin
      t.current
    end
  else if t.last then
    begin
      t.current
    end
  else
    begin
      let shared = [Default Shared_m] in
      let ins' = dep_set_to_list t.ins in
      let ins' = if (List.length ins') < 1 then [] else [In ins'] in
      let inouts' = dep_set_to_list t.inouts in
      let inouts' = if (List.length inouts') < 1 then [] else [Inout inouts'] in
      let depend = List.append ins' inouts' in
      let depend = if (List.length depend) < 1 then [] else [Depend depend] in
      let clauses = List.append shared depend in
      let pragma = Task clauses in
      let instr = if (List.length t.current) < 2 then
                    List.hd t.current
                  else
                    trm_seq_nomarks t.current in
      [trm_add_pragma pragma instr]
    end

let emit_profiler_task (t : Task.t) : trms =
  let get_begin (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_start = {pos_line = line; _}; _} -> string_of_int line
  in
  let get_end (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_end = {pos_line = line; _}; _} -> string_of_int line
  in     
  let reads = Dep_set.cardinal t.ins in
  let writes = Dep_set.cardinal t.inouts in
  let first = List.hd t.current in
  let first = get_begin first.loc in
  let last = (List.length t.current) - 1 in
  let last = List.nth t.current last in
  let last = get_end last.loc in
  let section = "ApacProfilerSection profsection(\"" ^
                  first ^ "-" ^ last ^ "\", " ^
                    (string_of_int reads) ^ ", " ^
                      (string_of_int writes) ^ ")" in
  let section = code (Instr section) in
  let ins' = dep_set_to_list t.ins in
  let ins' = List.map (fun e ->
                 let d = dep_to_string e in
                 let s = "profsection.addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                 code (Instr s)
               ) ins' in
  let inouts' = dep_set_to_list t.inouts in
  let inouts' = List.map (fun e ->
                    let d = dep_to_string e in
                    let s = "profsection.addParam(\"" ^ d ^ "\", " ^ d ^ ")" in
                    code (Instr s)
                  ) inouts' in
  let before = code (Instr "profsection.beforeCall()") in
  let after = code (Instr "profsection.afterCall()") in
  let preamble = (section :: ins') @ inouts' @ [before] in
  let postamble = [after] in
  preamble @ t.current @ postamble

let rec trm_from_task ?(backend : task_backend = OpenMP)
          (t : TaskGraph.V.t) : trms =
  let make (ts : trms) : trm =
    let ts' = Mlist.of_list ts in trm_seq ts'
  in
  (* Get the [Task] element of the current vertex. *)
  let task = TaskGraph.V.label t in
  let current = List.mapi (fun i instr ->
                    begin match instr.desc with
                    | Trm_for_c (init, cond, step, _, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                    (trm_from_task ~backend) cg in
                       let body = make body in
                       trm_for_c ~annot:instr.annot ~ctx:instr.ctx
                         init cond step body
                    | Trm_for (range, _, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       trm_for ~annot:instr.annot ~ctx:instr.ctx
                         range body
                    | Trm_if (cond, _, _) ->
                       let cg = List.nth task.children i in
                       let yes = List.nth cg 0 in
                       let no = List.nth cg 1 in
                       let yes = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) yes in
                       let yes = make yes in
                       let no = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) no in
                       let no = make no in
                       trm_if ~annot:instr.annot ~ctx:instr.ctx cond yes no
                    | Trm_while (cond, _) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       trm_while ~annot:instr.annot ~ctx:instr.ctx cond body
                    | Trm_do_while (_, cond) ->
                       let cg = List.nth task.children i in
                       let cg = List.hd cg in
                       let body = TaskGraphTraverse.codify
                                     (trm_from_task ~backend) cg in
                       let body = make body in
                       trm_do_while ~annot:instr.annot ~ctx:instr.ctx body cond
                    | Trm_switch (cond, cases) ->
                       let cg = List.nth task.children i in
                       let cgs = Queue.create () in
                       let _ = List.iter2 (fun (labels, _) block ->
                                   let block' = TaskGraphTraverse.codify
                                                  (trm_from_task ~backend)
                                                  block in
                                   let block' = make block' in
                                   Queue.push (labels, block') cgs) cases cg in
                       let cases' = List.of_seq (Queue.to_seq cgs) in
                       trm_switch ~annot:instr.annot ~ctx:instr.ctx cond cases'
                    | _ -> instr
                    end) task.current in
  let task = Task.update task current in
  match backend with
  | OpenMP -> emit_omp_task task
  | ApacProfiler -> emit_profiler_task task

(* [trm_look_for_dependencies t]: searches the term [t] for data accesses. It
   returns two lists. The first list holds the access terms where each term is
   paired with an access attribute. The second list contains all the variables
   involved in the data accesses. *)
let trm_discover_dependencies (locals : symbols)
      (t : trm) : (Dep_set.t * Dep_set.t)  =
  (* [trm_look_for_dependencies.aux depends nested attr t] builds [depends], a
     stack of data accesses in [t] and variables involved in the latter. Note
     that we build the stack by side-effect instead of returning a list. This is
     due to the usage of [trm_iter] for visiting [t]. [trm_iter] allows us to
     call [aux] on each term it visits but the latter has to have a [unit]
     return type. When [nested] is [true], the function does not push a new item
     to the stack, it simply continues to explore the AST. This happens, for
     example, in the case of a nested get operation such as [***ptr]. Finally,
     [attr] allows for passing access attributes between recursive calls to
     [aux], e.g. in the case of nested data accesses (see the [access_attr] type
     for more details). *)
  let rec aux (ins : dep Stack.t) (inouts : dep Stack.t)
            (filter : Var_set.t) (nested : bool) (attr : dep_attr)
            (t : trm) : unit =
    let error = Printf.sprintf "Apac_core.trm_look_for_dependencies.aux: '%s' \
                                is not a valid OpenMP depends expression"
                  (AstC_to_c.ast_to_string t) in
    (* We iteratively explore [t] and look for: *)
    (*let _ = Printf.printf "What term? %s\n" (trm_desc_to_string t.desc) in*)
    match t.desc with
    | Trm_var (_, v) when not nested && not (Var_set.mem v filter) ->
       (*let _ = Printf.printf "Trm_var %s\n" (var_to_string v) in*)
       if String.starts_with ~prefix:"sizeof(" v.name then
         let d = Dep_var (v) in Stack.push d ins
       else
         begin
           match attr with
           | Regular -> let d = Dep_var (v) in Stack.push d ins
           | FunArgIn ->
              let degree = Var_Hashtbl.find locals v in
              let d = var_to_dep v degree in
              Stack.push d ins
           | FunArgInOut ->
              let degree = Var_Hashtbl.find locals v in
              let d = var_to_dep v degree in
              Stack.push d inouts
         end
    (* - get operations ([*t'], [**t'], ...), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get))}, [t']) ->
       if not nested then
         begin
           match (trm_resolve_pointer_and_degree t') with
           | Some (v, degree) when not (Var_set.mem v filter) ->
              begin
                match attr with
                | Regular -> let d = Dep_trm (t', v) in Stack.push d ins
                | FunArgIn -> 
                   let degree' = Var_Hashtbl.find locals v in
                   let _ = Printf.printf "old %s : degree' is %d, degree is %d\n" (var_to_string v) degree' degree in
                   let d = trm_to_dep t' v (degree' - degree) in
                   Stack.push d ins
                | FunArgInOut ->
                   let degree' = Var_Hashtbl.find locals v in
                   let _ = Printf.printf "%s : degree' is %d, degree is %d, sum is %d\n" (var_to_string v) degree' degree (degree' + degree) in
                   for i = 0 to (degree' + degree) do
                     (* TODO: Pas sûr ! Avant, il y avait var_to_dep. *)
                     let d = trm_to_dep t' v i in
                     Stack.push d inouts
                   done
              end
           | Some _ -> ()
           | None -> fail t.loc error
         end;
       trm_iter (aux ins inouts filter true Regular) t'
    (* - address operations ([&t'], ...), *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address))}, [t']) ->
       (*let _ = Printf.printf "address\n" in*)
       if not nested then
         begin
           match (trm_resolve_pointer_and_degree t') with
           | Some (v, _) when not (Var_set.mem v filter) ->
              (*let _ = Printf.printf "address of %s\n" (var_to_string v) in*)
              begin
                match attr with
                | Regular
                  | FunArgIn -> let d = Dep_trm (t, v) in Stack.push d ins
                | FunArgInOut -> let d = Dep_trm (t, v) in Stack.push d inouts
              end
           | Some _ -> ()
           | None -> fail t.loc error
         end;
       trm_iter (aux ins inouts filter true Regular) t'
    (* - array accesses ([t'[i]], [t' -> [i]]), *)
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _)
      | Trm_apps ({desc = Trm_val
                            (Val_prim (Prim_binop Binop_array_get)); _}, _) ->
        let _ = Printf.printf "array\n" in
       let (base, accesses) = get_nested_accesses t in
       begin
         match (trm_resolve_pointer_and_degree base) with
         | Some (v, _) when not nested && not (Var_set.mem v filter) ->
            begin
              match attr with
              | Regular -> let d = Dep_trm (t, v) in Stack.push d ins
              | FunArgIn ->
                 let degree = List.length accesses in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = trm_to_dep t v (degree' - degree) in
                 Stack.push d ins
              | FunArgInOut ->
                 let degree = List.length accesses in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = trm_to_dep t v (degree' - degree) in
                 Stack.push d inouts
            end
         | Some _ -> ()
         | None -> fail t.loc error
       end;
       List.iter (
           fun a -> match a with
                    | Array_access_get t''
                      | Array_access_addr t'' ->
                       aux ins inouts filter false Regular t''
                    | _ -> ()
         ) accesses
    (* - unary increment and decrement operations ([t'++], [--t'], ...), *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) when
           (is_prefix_unary op || is_postfix_unary op) ->
       (* let _ = Printf.printf "unop\n" in *)
       begin
         match (trm_resolve_pointer_and_degree t') with
         | Some (v, degree) when not (Var_set.mem v filter) ->
            begin
              match attr with
              | Regular -> let d = Dep_var (v) in Stack.push d inouts
              | FunArgIn
                | FunArgInOut ->
                 let degree = degree + 1 in
                 let degree' = Var_Hashtbl.find locals v in
                 let d = var_to_dep v (degree' - degree) in
                 Stack.push d inouts
            end
         | Some _ -> ()
         | None -> fail t.loc error
       end;
    (* - function calls ([f(args)], ...). *)
    | Trm_apps ({ desc = Trm_var (_ , v); _ }, args) ->
       (*let _ = Printf.printf "call to %s\n" (var_to_string v) in*)
       if Var_Hashtbl.mem const_records v then
         let const_record : const_fun = Var_Hashtbl.find const_records v in
         List.iteri (
             fun pos arg ->
             if (Int_map.mem pos const_record.const_args) then
               begin
                 let const = Int_map.find pos const_record.const_args in
                 if const.is_const then
                   aux ins inouts filter false FunArgIn arg
                 else
                   aux ins inouts filter false FunArgInOut arg
               end
             else
               begin
                 let error =
                   Printf.sprintf
                     "Apac_core.trm_discover_dependencies: the constification \
                      record of '%s' has no argument constification record for
                      the argument on position '%d'."
                     (var_to_string v)
                     pos
                 in
                 fail None error
               end
           ) args
       else
         List.iter (
             fun arg ->
             trm_iter (aux ins inouts filter false FunArgInOut) arg
           ) args
    (* - set operation ([a = 1], [b = ptr], [*c = 42], ...), *)
    | Trm_apps _ when is_set_operation t ->
       let error' = "Apac_core.trm_look_for_dependencies.aux: expected set \
                     operation." in
       let (lval, rval) = trm_inv ~error:error' set_inv t in
       begin
         match (trm_resolve_binop_lval_and_get_with_deref lval) with
         | Some (lv, _) ->
            let d = Dep_trm (lval, lv.v) in
            Stack.push d inouts;
            trm_iter (aux ins inouts filter false Regular) rval
         | None -> fail t.loc error
       end
    | Trm_let (vk, (v, ty), init, _) ->
       let d = Dep_var (v) in
       let degree = (typ_get_degree ty) - 1 in
       Stack.push d inouts;
       for i = 1 to degree do
         let d' = var_to_dep v i in
         Stack.push d' inouts
       done;
       Var_Hashtbl.add locals v degree;
       trm_iter (aux ins inouts filter false Regular) init
    | Trm_let_mult (vk, tvs, inits) ->
       let (vs, _) = List.split tvs in
       let filter = Var_set.of_list vs in
       List.iter2 (fun (v, ty) init ->
           let d = Dep_var (v) in
           let degree = (typ_get_degree ty) - 1 in
           Stack.push d inouts;
           for i = 1 to degree do
             let d' = var_to_dep v i in
             Stack.push d' inouts
           done;
           Var_Hashtbl.add locals v degree;
           trm_iter (aux ins inouts filter false Regular) init
         ) tvs inits
    (* In the case of any other term, we just continue to explore the child
       terms. *)
    | _ -> (*let _ = Debug_transfo.trm ~style:Internal "other" t in*)
       trm_iter (aux ins inouts filter false attr) t
  in
  (* In the main part of the function, we begin by creating empty stacks to
     contain the discovered in and in-out dependencies. *)
  let ins : dep Stack.t = Stack.create () in
  let inouts : dep Stack.t = Stack.create () in
  (* Then, we launch the discovery process using the auxiliary function. *)
  let _ = aux ins inouts (Var_set.empty) false Regular t in
  (* Finally, we gather the results from the stacks and return them in lists. *)
  let ins' = dep_set_of_stack ins in
  let inouts' = dep_set_of_stack inouts in
  (ins', inouts')

(******************************************)
(* PART IV: CORE TRANSFORMATION FUNCTIONS *)
(* IV.1 Constification                    *)
(******************************************)

(* [build_constification_records_on]: see [build_constification_records]. *)
let build_constification_records_on (t : trm) : unit =
  (* Deconstruct the function definition term. *)
  let error = "Apac_basic.const_lookup_candidates: expected a target to a \
               function definition!" in
  let (var, ret_ty, args, _) = trm_inv ~error trm_let_fun_inv t in
  (* If the function is a class member method, its first argument is the [this]
     variable referring to the parent class. In this case, we do not need to
     include it in the resulting constification record. *)
  let args =
    if var.name <> "main" && (List.length args) > 0 then
      (* Extract the first argument of the function. *)
      let (first, _) = List.hd args in
      if first.name = "this" then List.tl args else args
    else args in
  (* Create a hash table for locally defined variables. *)
  let locals : symbols = Var_Hashtbl.create 10 in
  (* Create an argument constification record for the function's arguments and
     fill [locals] with function's arguments and their pointer degrees. *)
  let const_args = List.mapi (
                       fun pos (arg, ty) ->
                       let deg = typ_get_degree ty in
                       let _ = Var_Hashtbl.add locals arg deg in
                       (
                         pos,
                         {
                           is_ptr_or_ref =
                             is_typ_ptr ty || is_typ_ref ty ||
                               is_typ_array ty;
                           is_const = true;
                           to_unconst_by_propagation = Var_map.empty;
                         }
                       )
                     ) args in
  let const_args = Int_map.of_seq (List.to_seq const_args) in
  (* Create the constification record for the function itself *)
  let const : const_fun = {
      const_args = const_args;
      is_const = true;
      is_ret_ptr = is_typ_ptr ret_ty;
      is_ret_ref = is_typ_array ret_ty || is_typ_ref ret_ty;
      is_class_method = false;
      task_graph = None;
      variables = locals;
    } in
  (* and add it to [const_records] (global variable, see Part II.1) if it is not
     present in the hash table already, e.g. in the case of a
     pre-declaration. *)
  if not (Var_Hashtbl.mem const_records var) then
    begin
      Var_Hashtbl.add const_records var const
    end
      
(* [build_constification_records]: expects the target [tg] to point at a
   function definition. It adds a new entry into [const_records] (global
   variable, see Part II.1) based on the information about the function. *)
let build_constification_records (tg : target) : unit =
  Target.iter_at_target_paths (build_constification_records_on) tg

(* [identify_mutables_on p t]: see [identify_mutables]. *)
let identify_mutables_on (p : path) (t : trm) : unit =
  (* Auxiliary function which recursively visits all the terms of the body
     [fun_body] of the function [fun_var] in order to resolve dependencies
     between arguments and aliases. *)
  let rec aux (aliases : const_aliases) (fun_var : var)
            (fun_body : trm) : unit =
    match fun_body.desc with
    (* New scope *)
    | Trm_seq _
      | Trm_for _
      | Trm_for_c _
      | Trm_if _
      | Trm_switch _
      | Trm_while _ ->
       trm_iter (aux aliases fun_var) fun_body
    (* Function call: update dependencies. *)
    | Trm_apps ({ desc = Trm_var (_ , name); _ }, args) ->
       (* If we known the function's definition, i.e. the function was defined
          within the scope of the analysis, *)
       let _ = Printf.printf "%s calls %s\n" (var_to_string fun_var) (var_to_string name) in
       if Var_Hashtbl.mem const_records name then
         begin
           (* find the corresponding function constification record containing
              the constification records of all of the arguments. *)
           let fun_call_const = Var_Hashtbl.find const_records name in
           let fun_args_const = fun_call_const.const_args in
           (* In the case of a call to a class member method, the first argument
              is the variable referring to the parent class instance, e.g. in
              [this->p(i, j)] the first argument is [this] and in [a.f(i, j)]
              the first argument is [a]]. This is why the number of arguments in
              the function and the number of argument constification records
              associated with the function may not be the same. If [shift], the
              difference of these two values, has a positive value, we know that
              the current function call as a call to a class member method. *)
           let shift = (List.length args) - (Int_map.cardinal fun_args_const) in
           (* For each argument of the function call, we *)
           List.iteri (fun index arg ->
               (* go through the access and reference operations to obtain the
                  argument in the form of a labelled variable, if any, and *)
               match (trm_strip_accesses_and_references_and_get_lvar arg) with
               | Some arg_lvar ->
                  (* if the variable is an alias, we have to *)
                  if LVar_Hashtbl.mem aliases arg_lvar then
                    begin
                      (* determine the position of the argument it is
                         aliasing. *)
                      let (aliased, _) = LVar_Hashtbl.find aliases arg_lvar in
                      (* If a class member method has been called and if the
                         parent is not [this], we may have to unconstify the
                         argument. See [unconstify_mutables] for more details.
                         The constification transformation does not take into
                         account class member variables. *)
                      if (index - shift) < 0 && arg_lvar.v.name <> "this" then
                        Stack.push
                          (fun_var, aliased, name) to_unconst_objects
                      else
                        (* In the opposite case, *)
                        begin
                          (* there is the corresponding argument constification
                             record to be gathered *)
                          let pos = index - shift in
                          if (Int_map.mem pos fun_args_const) then
                            begin
                              let arg_const = Int_map.find pos fun_args_const in
                              (* and if the latter is a pointer or a
                                 reference, *)
                              if arg_const.is_ptr_or_ref then
                                begin
                                  (* we will have to unconstify it by
                                     propagation. *)
                                  arg_const.to_unconst_by_propagation <-
                                    Var_map.add fun_var aliased
                                      arg_const.to_unconst_by_propagation
                                end
                            end
                          else
                            (* If it is not possible, fail. This is not
                               normal! *)
                            begin
                              let error =
                                Printf.sprintf
                                  "Apac_core.identify_mutables_on.aux: \
                                   the constification record of '%s' has no \
                                   argument constification record for the \
                                   argument on position '%d'."
                                  (var_to_string fun_var)
                                  pos
                              in
                              fail None error
                            end
                        end
                    end
               | None -> ()
             ) args;
         end
           (* Otherwise, we do not have other choice but consider that the
              function call may modify its arguments by side-effect. Therefore,
              if the function call involves one or more arguments or alises to
              arguments of the currently processed function definition
              [fun_var], we have to unconstify them. *)
       else
         begin
           (* Let us warn the user about that. *)
           Printf.printf
             "WARNING: missing definition of '%s', considering all arguments \
              of the call as in-out dependencies\n" (var_to_string name);
           (* Then, for each argument of the function call, we *)
           List.iteri (fun index arg ->
               (* go through the access and reference operations to obtain the
                  argument in the form of a labelled variable, if any. *)
               match (trm_strip_accesses_and_references_and_get_lvar arg) with
               | Some arg_lvar ->
                  (* If the variable is an alias, we have to *)
                  if LVar_Hashtbl.mem aliases arg_lvar then
                    begin
                      (* determine the position of the argument it is aliasing
                         and *)
                      let (aliased, _) = LVar_Hashtbl.find aliases arg_lvar in
                      (* unconstify it. *)
                      Stack.push (fun_var, aliased) to_unconst
                    end
               | None -> ()
             ) args;
         end;
       trm_iter (aux aliases fun_var) fun_body
    (* Variable declaration: update list of aliases. *)
    | Trm_let (_, lval, { desc = Trm_apps (_, [rval]); _ }, _) ->
       let _ =
         trm_let_update_aliases ~reference:(trm_has_cstyle Reference fun_body)
           lval rval aliases in
       trm_iter (aux aliases fun_var) fun_body
    (* Multiple variable declaration: update list of aliases. *)
    | Trm_let_mult (_, lvals, rvals) ->
       List.iter2 (
           fun lval rval ->
           let _ = trm_let_update_aliases lval rval aliases in ()
         ) lvals rvals;
       trm_iter (aux aliases fun_var) fun_body
    (* Assignment or compound assignment: update the unconstification stack. *)
    | Trm_apps _ when is_set_operation fun_body ->
       let error = "Apac_basic.identify_mutables_on: expected set operation." in
       let (lval, rval) = trm_inv ~error set_inv fun_body in
       begin
         (* The lvalue has been modified by assignment. Resolve the labelled
            variable behind the lvalue and determine whether it has been
            dereferenced. *)
         match trm_resolve_binop_lval_and_get_with_deref lval with
         | Some (lval_lvar, lval_deref) ->
            (* If the lvalue is [this], it means that the function we are in is
               modifying a member variable of the parent class. Therefore, we
               will have to unconstify the method itself. *)
            if lval_lvar.v.name = "this" then
              begin
                Stack.push (fun_var, -1) to_unconst
              end;
            (* If it is an argument or an alias to an argument, we must not
               constify it. *)
            if LVar_Hashtbl.mem aliases lval_lvar then
              begin
                (* An alias of the same name may have been used multiple times
                   to alias different memory locations.
                   
                   For example, in:
                   
                   L1: void f(int * a, int * b, int * c) {
                   L2:   int * d = a;
                   L3:   d = c; 
                   L4:   *d = 1;
                   L5: }
                   
                   [d] is declared as an alias to [a] on L2. The data pointed to
                   by [a] is not modified within the function. On L3, [d]
                   becomes an alias for [c]. Then, on L4, the data pointed to by
                   [c] is modified through its alias [d]. Therefore, the
                   analysis will conclude that nor [c] nor [d] should be
                   constified. However, for [a] it will conclude that the
                   argument can be constified as the data it is pointing to is
                   never modified within the function. In the end, this will
                   produce a compilation error as [a], which became const, is
                   assigned to the non-const [d] on L2.
                   
                   In order to prevent this situation from happening, we must
                   propagate the unconstification to previously aliased
                   arguments too. As the [aliases] hash table stores all the
                   values that were ever assigned to a given key, we only have
                   to [find_all] of them and push them to the unconstification
                   stack. *)
                let all_aliases = LVar_Hashtbl.find_all aliases lval_lvar in
                List.iter (fun (aliased, _) ->
                    (* Again, we do not consider parent class members because
                       the constification process does not analyze entire
                       classes. *)
                    if aliased > -1 then
                      Stack.push (fun_var, aliased) to_unconst
                  ) all_aliases;
                (* When an alias changes a target, i.e. when the lvalue variable
                   was not dereferenced, *)
                begin
                  (* we have to resolve the pointer labelled variable behind the
                     rvalue and check if it is an argument an alias to an
                     argument. *)
                  match (
                    trm_resolve_pointer_and_aliased_variable rval aliases
                  ) with
                  (* If it is the case, we have to add a new entry into
                     [aliases]. This happens, for example, on L3 in the
                     aforementioned example. *)
                  | Some (_, aliased) ->
                     let (_, lval_degree) = List.nth all_aliases 0 in
                     (* The value of [lval_deref] is incorrect when the lvalue
                        refers to the parent [this]. This is because
                        [trm_resolve_binop_lval_and_get_with_deref] operates on
                        [this] and not on the concerned structure member. In
                        this case, to verify that the lvalue was not
                        dereferenced, we have to check that the pointer degree
                        of the lvalue is still greater than 0. *)
                     if
                       not lval_deref ||
                         (lval_lvar.v.name = "this" && lval_degree > 0) then
                       LVar_Hashtbl.add aliases
                         lval_lvar (aliased, lval_degree)
                  | None -> ()
                end
              end
         | _ -> ()
       end;
       trm_iter (aux aliases fun_var) fun_body
    (* Increment or decrement unary operation: update the unconstification
       stack. *)
    | Trm_apps _ when trm_is_unop_inc_or_dec fun_body ->
       let error = "Apac_basic.identify_mutables_on: unable to retrieve
                    variable name" in
       let var_lvar = trm_inv ~error
                        trm_resolve_var_in_unop_or_array_access_and_get
                        fun_body
       in
       (* Propagate the unconstification to all previously aliased arguments. *)
       let all_aliases = LVar_Hashtbl.find_all aliases var_lvar in
       List.iter (fun (aliased , _) ->
           (* Again, we do not consider parent class members because the
              constification process does not analyze entire classes yet. See an
              aforementioned TODO. *)
           if aliased > -1 then
             Stack.push (fun_var, aliased) to_unconst
         ) all_aliases;
       trm_iter (aux aliases fun_var) fun_body
    (* Return statement: update the unconstification stack if the return value
       is a reference or a pointer. *)
    | Trm_abort (Ret (Some ret)) ->
       let fun_const = Var_Hashtbl.find const_records fun_var in
       if fun_const.is_ret_ref then
         begin
           (* If the return type of the function is a reference, there are two
              return term types we can possibly deal with: *)
           match (trm_resolve_var_in_unop_or_array_access_and_get ret) with
           (* 1) a variable term corresponding to the reference, *)
           | Some ret_lvar ->
              begin
                (* Propagate the unconstification to all previously aliased
                   arguments. *)
                let all_aliases = LVar_Hashtbl.find_all aliases ret_lvar in
                List.iter (fun (aliased, _) ->
                    (* Again, we do not consider parent class members because
                       the constification process does not analyze entire
                       classes yet. See an aforementioned TODO. *)
                    if aliased > -1 then
                      Stack.push (fun_var, aliased) to_unconst
                  ) all_aliases
              end
           (* 2) a function call with reference return type. In this case, there
              is nothing to do as the function call cannot be an alias to an
              argument. *)
           | _ -> ()
         end
           (* If the return type of the function is a pointer, *)
       else if fun_const.is_ret_ptr then
         begin
           (* we have to go through all of the potential pointer operations
              within the return term and try to determine if they lead to a
              pointer variable which aliases an existing variable or
              argument. *)
           match (trm_resolve_pointer_and_aliased_variable ret aliases) with
           (* If it is the case, we need it to be unconstified. *)
           | Some (_, aliased) -> 
              (* Again, we do not consider parent class members because the
                 constification process does not analyze entire classes yet. See
                 an aforementioned TODO. *)
              if aliased > -1 then
                Stack.push (fun_var, aliased) to_unconst
           | None -> ()
         end;
       trm_iter (aux aliases fun_var) fun_body
    | _ -> trm_iter (aux aliases fun_var) fun_body
  in
  (* This is the main entry point of the function from where the auxiliary
     function shall be called. *)
  (* Deconstruct the target function definition term. *)
  let error = "Apac_basic.identify_mutables_on: expected target to a function \
               definition." in
  let (var, ret_ty, args, body) = trm_inv ~error trm_let_fun_inv t in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records var in
  (* Create a hash table for aliases to arguments of the function. *)
  let aliases : const_aliases = LVar_Hashtbl.create 10 in
  (* Try to find the parent class of the function. If any, get all the member
     variables of the class and *)
  let class_siblings = match (find_parent_typedef_record p) with
    | Some (td) -> typedef_get_members td
    | None -> [] in
  (* add them to the table of aliases in the form of labelled variables. *)
  if List.length class_siblings > 0 then
    begin
      (* In the case of class member methods, the first argument of the function
         is the variable referring to the parent class instance, i.e. [this]. *)
      let (this, _) = List.hd args in
      (* Record that this is a class member method. *)
      const_record.is_class_method <- true;
      (* For each member variable of the parent class, *)
      List.iteri (fun idx (label, ty) ->
          (* build the corresponding labelled variable and *)
          let lv : lvar = { v = this; l = label } in
          (* add it to the hash table of aliases. *)
          LVar_Hashtbl.add aliases lv ((- idx), typ_get_degree ty)
        ) class_siblings
    end;
  (* If the function is not a class member method, it cannot be constified. It
     is not taken into account in C/C++. *)
  if not const_record.is_class_method then
    const_record.is_const <- false;
  (* Then, of course, we have also to add the arguments of the function itself
     to the hash table of aliases. This is necessary in order to be able to
     identify possible aliases to these variables within the body of the
     function during the analysis using the auxiliary function defined above.

     Note that in the case of class member methods, we do not need to add the
     first argument which is a variable referring to the parent class instance.
     Indeed, we have already added sibling class member values into the hash
     table of aliases above. *)
  let args = if const_record.is_class_method then List.tl args else args in
  List.iteri (fun pos (v, ty) ->
      let lv : lvar = { v = v; l = String.empty } in
      LVar_Hashtbl.add aliases lv (pos, typ_get_degree ty)
    ) args;
  (* Actually compute the dependencies of the function definition at [path]
     and fill [to_unconst]. *)
  aux aliases var body

(* [identify_mutables tg]: expects the target [tg] to point at a function
   definition. It recurses over the body of the target function definition in
   order to figure out which function arguments and possible aliases to
   arguments should not be constified and adds them to the [to_unconst] stack
   (global variable, see Part II.1). *)
let identify_mutables (tg : target) : unit =
  Target.iter (fun t p -> identify_mutables_on p (get_trm_at_path p t)) tg

(**********************)
(* IV.2 Taskification *)
(**********************)

(* [taskify_on p t]: see [taskify]. *)
let taskify_on (p : path) (t : trm) : unit =
  (* Auxiliary function to transform a portion of the existing AST into a local
     fill_task_graphed AST (see [atrm]). *)
  let rec fill (s : symbols) (t : trm) (g : TaskGraph.t) : Task.t =
    match t.desc with
    | Trm_seq sequence ->
       let scope = var_set_of_var_hashtbl s in
       let instrs = Mlist.to_list sequence in
       let tasks = List.map (fun instr -> fill s instr g) instrs in
       let ins = List.fold_left (
                     fun acc (task : Task.t) -> Dep_set.union acc task.ins
                   ) Dep_set.empty tasks in
       let inouts = List.fold_left (
                        fun acc (task : Task.t) -> Dep_set.union acc task.inouts
                      ) Dep_set.empty tasks in
       let this = Task.create t scope ins inouts [] in
       let this' = TaskGraph.V.create this in
       let _ = TaskGraph.add_vertex g this' in
       let tasks = List.map (
                       fun task ->
                       let v = TaskGraph.V.create task in
                       TaskGraph.add_vertex g v; v
                     ) tasks in
       let _ = TaskGraph.add_edge g this' (List.hd tasks) in
       let nb_tasks = List.length tasks in
       for i = 0 to (nb_tasks - 1) do
         let vertex_i = List.nth tasks i in
         let task_i = TaskGraph.V.label vertex_i in
         for j = (i + 1) to (nb_tasks - 1) do
           let vertex_j = List.nth tasks j in
           let task_j = TaskGraph.V.label vertex_j in
           let op1 = Dep_set.inter task_i.inouts task_j.ins in
           let op2 = Dep_set.inter task_i.inouts task_j.inouts in
           let j_depends_on_i =
             not ((Dep_set.is_empty op1) && (Dep_set.is_empty op2)) in
           let j_depends_on_i = j_depends_on_i || task_j.last in
           if j_depends_on_i then
             begin
               TaskGraph.add_edge g vertex_i vertex_j
             end
           else
             begin
               () (*TaskGraph.add_edge g this' vertex_j*)
             end
         done
       done;
       for i = 0 to (nb_tasks - 1) do
         let vertex = List.nth tasks i in
         let degree = TaskGraph.in_degree g vertex in
         if degree < 1 then
           begin
             TaskGraph.add_edge g this' vertex
           end
       done;
       this      
    | Trm_for_c (init, cond, inc, instr, _) ->
       let scope = var_set_of_var_hashtbl s in
       let (ins, inouts) = trm_discover_dependencies s init in
       let (ins', inouts') = trm_discover_dependencies s cond in
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       let (ins', inouts') = trm_discover_dependencies s inc in
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       let c = TaskGraph.create() in
       let ct = fill s instr c in
       let (ins, inouts) =
         (Dep_set.union ins ct.ins, Dep_set.union inouts ct.inouts) in
       let _ = 
         TaskGraph.iter_vertex (fun vertex ->
             let lab : Task.t = TaskGraph.V.label vertex in
             Printf.printf "subgraph vertex: %s\n" (Task.to_string lab)) c in
       Task.create t scope ins inouts [[c]]
    | Trm_for (range, instr, _) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Explode the [range] specifier to allow for dependency discovery. *)
       let (index, init, _, cond, step, _) = range in
       (* Launch dependency discovery in the initialization term as well as *)
       let (ins, inouts) = trm_discover_dependencies s init in
       (* in the conditional statement representing the upper loop bound. *)
       let (ins', inouts') = trm_discover_dependencies s cond in
       (* Gather the discovered dependencies. *)
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       (* Check whether [step] is formed of a term. In other words, check
          whether it is not simply a unary increment or decrement, but something
          like [i += a * 2]. In this case, *)
       let (ins', inouts') = match step with
         (* we have to look for dependencies in this term. *)
         | Step st -> trm_discover_dependencies s st
         (* Otherwise, we do not have to do nothing, just keep the current in
            and inout dependency sets as they are. *)
         | _ -> (ins, inouts) in
       (* Gather the discovered dependencies, if any. *)
       let (ins, inouts) =
         (Dep_set.union ins ins', Dep_set.union inouts inouts') in
       (* Create a sub-graph for the body sequenc, i.e. [instr], of the [for]
          loop. *)
       let c = TaskGraph.create() in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let ct = fill s instr c in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [for] graph node, i.e. [ins] and [inouts],
          by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins ct.ins, Dep_set.union inouts ct.inouts) in
       (* Create the task corresponding to the current [for] graph node using
          all the elements computed above. *)
       Task.create t scope ins inouts [[c]] 
    | Trm_let _
      | Trm_let_mult _ ->
       (* Look for dependencies in the current variable declaration term and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s t in
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* Create a barrier corresponding to the current variable declaration
          term. Variable declarations should never appear in tasks. *)
       Task.wait t scope ins inouts []
    | Trm_apps _ ->
       (* Look for dependencies in the current term and initialize the in and
          in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s t in
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* Create the task corresponding to the current graph node using all the
          elements computed above. *)
       Task.create t scope ins inouts []
    | Trm_if (cond, yes, no) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [if] and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create sub-graphs for the [then] and the [else] branches. *)
       let gy = TaskGraph.create () in
       let gn = TaskGraph.create () in
       (* Taskify the branches while filling the correspoding sub-graphs. *)
       let ty = fill s yes gy in
       let tn = fill s no gn in
       (* Include the dependencies from the branches into the sets of
          dependencies of the current [if] graph node, i.e. [ins] and [inouts],
          by the means of union operations. *)
       let (ins, inouts) =
         (Dep_set.union ins ty.ins, Dep_set.union inouts ty.inouts) in
       let (ins, inouts) =
         (Dep_set.union ins tn.ins, Dep_set.union inouts tn.inouts) in
       (* Create the task corresponding to the current [if] graph node using all
          the elements computed above. *)
       Task.create t scope ins inouts [[gy; gn]]
    | Trm_while (cond, body) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [while] and
          initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create a sub-graph for the body sequence of the [while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [while] graph node, i.e. [ins] and
          [inouts], by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins tb.ins, Dep_set.union inouts tb.inouts) in
       (* Create the task corresponding to the current [while] graph node using
          all the elements computed above. *)
       Task.create t scope ins inouts [[gb]]
    | Trm_do_while (body, cond) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [do-while]
          and initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* Create a sub-graph for the body sequence of the [do-while]. *)
       let gb = TaskGraph.create () in
       (* Taskify the body sequence while filling the correspoding sub-graph. *)
       let tb = fill s body gb in
       (* Include the dependencies from the body sequence into the sets of
          dependencies of the current [do-while] graph node, i.e. [ins] and
          [inouts], by the means of a union operation. *)
       let (ins, inouts) =
         (Dep_set.union ins tb.ins, Dep_set.union inouts tb.inouts) in
       (* Create the task corresponding to the current [do-while] graph node
          using all the elements computed above. *)
       Task.create t scope ins inouts [[gb]]
    | Trm_switch (cond, cases) ->
       (* Keep a copy of the local scope of variables as a set. We need this
          because we do not want any variables potentially defined in child
          scopes (added to [s] within [trm_discover_dependencies]) to end up in
          the parent scope, which is the current scope. *)
       let scope = var_set_of_var_hashtbl s in
       (* Look for dependencies in the conditional expression of the [switch]
          and initialize the in and in-out dependency sets. *)
       let (ins, inouts) = trm_discover_dependencies s cond in
       (* We are about to process the blocks associated with the cases of the
          [switch]. To each case we will associate the corresponding [Task] and
          [TaskGraph]. As we can not directly map the elements of [cases] to
          another type of list elements, i.e. pairs of [Task] and [TaskGraph],
          we well keep these pairs in the temporary queue [cases']. *)
       let cases' = Queue.create () in
       (* For each block associated with one of the cases of the [switch],
          we: *)
       List.iter (fun (labels, block) ->
           (* - create a sub-graph for the block sequence, *)
           let gb = TaskGraph.create () in
           (* - taskify the block sequence while filling the
              corresponding graph, *)
           let tb = fill s block gb in
           (* - push both the task and the graph associated
              with the currently processed block sequence into
              the temporary stack. *)
           Queue.push (tb, gb) cases'
         ) cases;
       (* Get the [Task] and [TaskGraph] elements from the temporary stack as a
          pair of lists. *)
       let pairs = List.of_seq (Queue.to_seq cases') in
       let (tbs, gbs) = List.split pairs in
       (* Include the dependencies from the block sequences into the sets of
          dependencies of the current [switch] graph node, i.e. [ins] and
          [inouts], by the means of union operations. *)
       let (ins, inouts) = List.fold_left (fun (ins', inouts') (tb : Task.t) ->
                               (Dep_set.union ins' tb.ins,
                                Dep_set.union inouts' tb.inouts))
                             (ins, inouts) tbs in
       (* Create the task corresponding to the current [switch] graph node
          using all the elements computed above. *)
       Task.create t scope ins inouts [gbs]
    | Trm_delete (_, target) ->
       (* Look for dependencies in the target term of the [delete]. [delete] is
          a destructive operation, we need to consider all of the dependencies
          as in-out dependencies, of course. *)
       let (ins, inouts) = trm_discover_dependencies s target in
       let inouts = Dep_set.union ins inouts in
       (* Convert the local scope to a set *)
       let scope = var_set_of_var_hashtbl s in
       (* in order to be able to use it when creating the task corresponding to
          the current [delete] graph node. *)
       Task.create t scope Dep_set.empty inouts []
    | Trm_goto target ->
       (* If the target label of the [goto] is not the [Apac_core.goto_label] we
          use within the return statement replacement transformation
          [Apac_basic.use_goto_for_return], fail. Other goto statements than
          those we add are not allowed within a taskification target. *)
       if target <> goto_label then
         fail t.loc "Apac_core.taskify_on.fill: illegal goto statement"
       else
         (* If [target] is [Apac_core.goto_label], we can create a [Task]
            instance for it, even if it will actually nevery become a task.
            However, we need to do it for the sake of consistency as only
            instructions being part of the final task graph will be part of the
            transformed source code. We use the [last] method so as to ensure
            that the instruction will appear as the last one in the current
            scope. *)
         Task.last t
    | Trm_val v ->
       (* Retrieve the first label attribute of the current term, if any. *)
       let l = trm_get_labels t in
       let l = if (List.length l) > 0 then List.nth l 0 else "" in
       (* We have to check whether this value term is a goto label arising from
          [Apac_basic.use_goto_for_return]. *)
       begin match v with
         (* If it is the case, we can create a [Task] instance for it, even if
            it will actually nevery become a task. We use the [last] method so
            as to ensure that the instruction will appear as the last one in the
            current scope. *)
       | Val_lit (Lit_unit) when l = goto_label -> Task.last t
       (* Otherwise, fail. Other types of values are not allowed as first-level
          instructions within a task group. *)
       | _ -> fail t.loc "Apac_core.taskify_on.fill: illegal value term"
       end
    | Trm_omp_routine r ->
       (* Convert the local scope to a set. *)
       let scope = var_set_of_var_hashtbl s in
       (* When it comes to OpenMP routine calls, we consider two different
          situations: *)
       begin match r with
       (* 1) On the one hand, there are routines taking a variable as an
             argument. In this case, we have to perform dependency discovery. *)
       | Set_default_device v
       | Init_lock v 
       | Init_nest_lock v 
       | Destroy_lock v 
       | Destroy_nest_lock v
       | Set_lock v 
       | Set_nest_lock v 
       | Unset_lock v 
       | Unset_nest_lock v 
       | Test_lock v 
       | Test_nest_lock v ->
          (* Look for dependencies in the routine argument [v]. The above
             routines modify the variable they take as an argument. Therefore,
             we need to consider all of the dependencies as in-out
             dependencies. *)
       let (ins, inouts) = trm_discover_dependencies s (trm_var v) in
       let inouts = Dep_set.union ins inouts in
       (* Create the task corresponding to the current OpenMP routine call graph
          node. *)
       Task.create t scope Dep_set.empty inouts []
       (* 2) On the other hand, all the other routines do not involve any
             variables and thus do not require dependency discovery. *)
       | _ ->
          (* Create the task corresponding to the current OpenMP routine call
             graph node. *)
          Task.create t scope Dep_set.empty Dep_set.empty []
       end
    | _ ->
       let error = Printf.sprintf
                     "Apac_core.taskify_on.fill: '%s' should not appear in a \
                      task group" (trm_desc_to_string t.desc) in
       fail t.loc error
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.taskify_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = TaskGraph.create () in
  let _ = fill const_record.variables t g in
  let g' = TaskGraphOper.transitive_reduction g in
  const_record.task_graph <- Some (g');
  TaskGraph.iter_vertex (fun vertex ->
      let lab : Task.t = TaskGraph.V.label vertex in
      Printf.printf "vertex: %s\n" (Task.to_string lab)) g';
  export_task_graph g' "apac_task_graph.dot"
  (*fill const_record.variables t task_graph;
  Printf.printf "Augmented AST for <%s> follows:\n%s\n"
    (var_to_string f) (atrm_to_string aast)*)
    
let taskify (tg : target) : unit =
  Target.iter (fun t p -> taskify_on p (get_trm_at_path p t)) tg

(* [merge_on p t]: see [merge]. *)
let merge_on (p : path) (t : trm) : unit =
  let rec find_sequence (g : TaskGraph.t) (start : TaskGraph.V.t) :
            TaskGraph.V.t list =
    if (TaskGraph.out_degree g start) > 1 then
      begin
        [start]
      end
    else
      begin
        let child = TaskGraph.succ g start in
        if (List.length child) < 1 then
          begin
            [start]
          end
        else
          begin
            let child = List.hd child in
            if (TaskGraph.in_degree g child) < 2 then
              start::(find_sequence g child)
            else [start]
          end
      end
  in
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.merge_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.merge_on: Missing task graph. Did you \
                          taskify?" in
  let vertices = TaskGraph.fold_vertex (fun v acc -> v::acc) g [] in
  let nb_vertices = TaskGraph.nb_vertex g in
  for i = 0 to (nb_vertices - 1) do
    begin
      let vertex = List.nth vertices i in
      if (TaskGraph.mem_vertex g vertex) then
        begin
          let _ = Printf.printf "Vertex no. %d\n"  i in
          let sequence : TaskGraph.V.t list = find_sequence g vertex in 
          let _ = Printf.printf "Vertex no. %d AFTER\n"  i in
          let steps = List.length sequence in
          if steps > 1 then
            begin
              (*merge*)
              let start = List.hd sequence in
              let tail = List.tl sequence in
              let first = TaskGraph.V.label start in
              let task : Task.t = List.fold_left (fun t v ->
                                      let curr : Task.t = TaskGraph.V.label v in
                                      Task.merge t curr) first tail in
              let stop = List.nth sequence (steps - 1) in
              let pred = TaskGraph.pred g start in
              let succ = TaskGraph.succ g stop in
              let vertex' = TaskGraph.V.create task in
              TaskGraph.add_vertex g vertex';
              List.iter (fun v -> TaskGraph.add_edge g v vertex') pred;
              List.iter (fun v -> TaskGraph.add_edge g vertex' v) succ;
              List.iter (fun v -> TaskGraph.remove_vertex g v) sequence
            end
        end
    end
  done;
  export_task_graph g "apac_task_graph_merged.dot"
  (*fill const_record.variables t task_graph;
  Printf.printf "Augmented AST for <%s> follows:\n%s\n"
    (var_to_string f) (atrm_to_string aast)*)

let merge (tg : target) : unit =
  Nobrace.enter ();
  Target.iter (fun t p -> merge_on p (get_trm_at_path p t)) tg

(* [insert_tasks_on p t]: see [insert_tasks_on]. *)
(* TODO : Un parcours plus intelligent du graphe lors de la génération de code de sortie, pas seulement du BFS. *)
(* - consolider l'implémentation actuelle (ajouter les sous-graphes, tester sur des vrais codes)
   - mettre sur papier des idées de stratégies de transformation de graphes. *)
let insert_tasks_on (p : path) (t : trm) : trm =
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.insert_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.merge_on: Missing task graph. Did you \
                          taskify?" in
  let instrs = TaskGraphTraverse.codify (trm_from_task ~backend:OpenMP) g in
  let instrs = Mlist.of_list instrs in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx instrs in
  let _ = Debug_transfo.trm "output" result in
  result
  
    
let insert_tasks (tg : target) : unit =
  Target.apply (fun t p -> Path.apply_on_path (insert_tasks_on p) t p) tg

(* [profile_tasks_on p t]: see [profile_tasks_on]. *)
let profile_tasks_on (p : path) (t : trm) : trm =
  (* Find the parent function. *)
  let f = match (find_parent_function p) with
    | Some (v) -> v
    | None -> fail t.loc "Apac_core.profile_tasks_on: unable to find parent \
                          function. Task group outside of a function?" in
  (* Find the corresponding constification record in [const_records]. *)
  let const_record = Var_Hashtbl.find const_records f in
  (* Build the augmented AST correspoding to the function's body. *)
  let g = match const_record.task_graph with
    | Some (g') -> g'
    | None -> fail t.loc "Apac_core.profile_tasks_on: Missing task graph. Did \
                          you taskify?" in
  let instrs = TaskGraphTraverse.codify
                 (trm_from_task ~backend:ApacProfiler) g in
  let instrs = Mlist.of_list instrs in
  let result = trm_seq ~annot:t.annot ~ctx:t.ctx instrs in
  (* let _ = Debug_transfo.trm "output" result in *)
  result
  
    
let profile_tasks (tg : target) : unit =
  Target.apply (fun t p -> Path.apply_on_path (profile_tasks_on p) t p) tg

let include_apac_profiler_on (p : path) (t : trm) : trm =
  if p <> [] then
    fail t.loc "Apac_core.include_apac_profiler_on: expects to be applied on \
                the root of the AST"
  else
    begin
      match t.desc with
      | Trm_seq tl ->
         let directive = code (Expr "#include \"apac_profiler.hpp\"") in
         let tl' = Mlist.insert_at 0 directive tl in
         trm_seq ~annot:t.annot tl'
      | _ -> fail t.loc "Apac_core.include_apac_profiler_on: expects to be \
                         applied on a sequence"
    end

let include_apac_profiler (tg : target) : unit =
  Target.apply (fun t p ->
      Path.apply_on_path (include_apac_profiler_on p) t p) tg

