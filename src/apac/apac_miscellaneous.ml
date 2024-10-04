open Ast
open Typ

(** [excerpt ?max ast]: returns an excerpt of a string representation of an
    [ast] term at most [max] characters long. *)
let excerpt ?(max : int = 20) (ast : trm) =
  let instr = AstC_to_c.ast_to_string ast in
  let instr = String.split_on_char '\n' instr in
  let instr = List.hd instr in
  let instr = String.split_on_char '"' instr in
  let instr = List.hd instr in
  let instr = String.trim instr in
  let limit = String.length instr in
  let limit = if limit > max then max else limit in
  String.sub instr 0 limit

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

(** [typ_get_nli ty]: returns the number of levels of indirection of the type
    [ty], e.g. [2] for [int ** a]. *)
let typ_get_nli (ty : typ) : int =
  (** [typ_get_nli.aux nli ty]: auxiliary function hiding the counter [nli] of
      levels of indirection of [ty]. *)
  let rec aux (nli : int) (ty : typ) : int =
    match ty.typ_desc with
    (** When [ty] is a constant type or a reference, [nli] does not change,
        continue the computation on the inner type. *)
    | Typ_const ty -> aux nli ty
    | Typ_ptr { ptr_kind = Ptr_kind_ref; inner_typ = ty } -> aux nli ty
    (** When [ty] is a pointer or an array, increase [nli] and continue the
        computation on the inner type. *)
    | Typ_ptr { ptr_kind = Ptr_kind_mut; inner_typ = ty } -> aux (nli + 1) ty
    | Typ_array (ty, _) -> aux (nli + 1) ty
    (** When [ty] is a constructed user-defined type and it is an alias to a
        basic type, resolve the latter and continue computation. *)
    | Typ_constr _ when typ_is_alias ty ->
      begin match typ_get_alias ty with
      | Some (ty) -> aux nli ty
      | None -> failwith "Apac_miscellaneous.typ_get_nli: unable to determine \
                          the basic type behind an alias."
      end
    (** When [ty] is a basic type or a constructed user-defined type which is {b
        not} an alias to a basic type, we have finished computing, return the
        final number of levels fo indirection of [ty]. *)
    | _ -> nli
  in
  (** Start counting the levels of indirection of [ty] at level [0]. *)
  aux 0 ty

(* [trm_strip_accesses_and_references_and_get_lvar t]: strips [*t, &t, ...]
   recursively and if [t] is a variable, it returns the associated labelled
   variable. *)
let trm_strip_and_get_lvar (t : trm) : lvar option =
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
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux l t
    (* [t] is a binary operation of another type: strip and recurse on both left
       and right-hand sides. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop _ )); _ }, [lhs; rhs]) ->
       (* We continue to recurse on both the left and the right internal
          terms. *)
       begin
         match (aux l lhs, aux l rhs) with
         | Some (res), None -> Some (res)
         | None, Some (res) -> Some (res)
         | None, None
           (* In practice, binary operations between two pointers supported in
              C/C++ can not lead to a valid alias of one of them. *)
           | Some (_), Some (_) -> None
       end
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
       | Unop_cast ty -> aux (degree + typ_get_nli ty) t'
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

(** [trm_is_array_or_direct_access t]: checks whether the term [t] represents an
    array or a direct variable access. *)
let trm_is_array_or_direct_access (t : trm) : bool =
  match t.desc with
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _) ->
       true
    | Trm_var _ -> true
    | _ -> false

(** [trm_resolve_dereferenced_with_degree t]: if the term [t] represents a stack
    of dereference operations, it returns the underlying array or direct
    variable access term and the number of dereferencings, i.e. the degree. *)
let trm_resolve_dereferenced_with_degree (t : trm) : (trm * int) option =
  let rec aux (degree : int) (t : trm) : (trm * int) option =
    match t.desc with
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t']) ->
       begin match op with
       | Unop_get -> aux (degree + 1) t'
       | _ -> None
       end
    | Trm_apps ({desc = Trm_val
                          (Val_prim (Prim_binop Binop_array_access)); _}, _)
      | Trm_var _ -> Some (t, degree)
    | _ -> None
  in
  aux 0 t

(* [trm_resolve_binop_lval_and_get_with_deref] tries to resolve the variable
   behind an lvalue and check whether it has been dereferenced, i.e. following
   an array access or the use of [*]. Upon success, it returns the corresponding
   labelled variable. See [LVar] for more details on labelled variables. *)
let trm_resolve_binop_lval_and_get_with_deref ?(plus : bool = false)
      (t : trm) : (lvar * bool) option =
  let rec aux (d : int) (l : label) (t : trm) : (lvar * bool) option =
    match t.desc with
    (* We have found the variable, build and return the resulting labelled
       variable. *)
    | Trm_var (vk, var) ->
       let lv : lvar = { v = var; l = l } in
       let d' = if vk = Var_immutable && plus then d + 1 else d in
       Some (lv, d' > 0)
    (* [t] is an array access, which means that the operand was dereferenced.
       Continue resolution on the latter. *)
    | Trm_apps ({
            desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)));
            _ }, [t; _]) -> aux (d + 1) l t
    (* [t] is a unary operation. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop (op))); _ }, [t]) ->
       begin
         match op with
         (* A get operation, e.g. [*operand], as well as a structure access,
            e.g. [operand.field], both imply that the operand was dereferenced.
            Continue resolution on the latter. *)
         | Unop_get -> aux (d + 1) l t
         | Unop_address -> aux (d - 1) l t
         | Unop_cast ty -> aux (d + typ_get_nli ty) l t
         | Unop_struct_access field -> aux (d + 1) field t
         (* A structure access through pointer, e.g. [operand->field], means
            that the operand was not dereferenced. To finish resolving, iterate
            once more on [t]. *)
         | Unop_struct_get field -> aux d field t
         (* In case of another binary operation, do nothing and continue
            resolution on the operand. *)
         | _ -> aux d l t
       end
    | _ -> None
  in
  aux 0 "" t

(** [find_parent_function p]: goes back up the path [p] and looks for the first
    term corresponding to a function definition. If a function definition is
    found, it returns the name of the function as a variable. We use
    [find_parent_function] to determine the parent function of a task group
    sequence in order to access its constification record in [const_funs]. *)
let find_parent_function (p : Path.path) : var option =
  (** We shall go back on our steps in the path, i.e. in the direction of the
      root of the AST, so we need to reverse [p]. *)
  let reversed = List.tl (List.rev p) in
  (** We use an auxiliary function in order to hide to the outside world the
      need for the path reversal. *)
  let rec aux (p : Path.path) : var option =
    (** The function simply goes recursively through the reversed [p] *)
    match p with
    | e :: f -> 
       begin
         (** and if it detects a function definition, it returns it. *)
         (** FIXME : Optimize by passing non-reversed list as argument ? *)
         let tg = Target.target_of_path (List.rev p) in
         let t = Target.get_trm_at_exn tg in
         match t.desc with
         | Trm_let_fun (v, _, _, _, _) -> Some (v)
         | _ -> aux f
       end
    | [] -> None
  in
  aux reversed 
