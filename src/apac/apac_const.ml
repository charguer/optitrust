open Ast
open Typ
open Trm
open Path
open Target
open Mark
open Tools
open Apac_lvar
open Apac_miscellaneous
open Apac_tasks

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
   represented by variables, i.e. [var]. *)
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
let trm_resolve_binop_lval_and_get_with_deref (t : trm) : (LVar.t * bool) option =
  let rec aux (dereferenced : bool) (l : label) (t : trm) :
            (LVar.t * bool) option =
    match t.desc with
    (* We have found the variable, build and return the resulting labelled
       variable. *)
    | Trm_var (_, var) ->
       let lv : LVar.t = { v = var; l = l } in Some (lv, dereferenced)
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
let trm_resolve_var_in_unop_or_array_access_and_get (t : trm) : LVar.t option =
  (* Simply recurse over unary operations and array accesses. *)
  let rec aux (l : label) (t : trm) : LVar.t option =
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
       let lv : LVar.t = { v = var; l = l } in Some lv
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
      (t : trm) (aliases : const_aliases) : (LVar.t * int) option =
  (* Simply recurse over different kinds of operations. *)
  let rec aux (degree : int) (l : label) (t : trm) : (LVar.t * int) option =
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
       let lv : LVar.t = { v = v; l = l } in
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
  let lv : LVar.t = { v = v; l = String.empty } in
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
         (* and if it detects a class or a structure definition, the function
            returns it. *)
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
