open Ast
open Typ

(** [cwd]: returns the path to the current working directory. *)
let cwd () : string =
  Flags.process_program_name ();
  Filename.dirname (!Flags.program_name)

(** [gwd]: returns the path to a directory within the current working directory
    to store task candidate graphs in. If this directory does not exists, this
    function creates it. *)
let gwd () : string =
  (** Build the full path to the destination directory. *)
  let path = (cwd ()) ^ !Apac_macros.keep_graphs_in in
  if (Sys.file_exists path) then
    if (Sys.is_directory path) then
      (** If the path points to an existing directory, just return it. *)
      path
    else
      (** If the path points to an existing file which is not a directory,
          fail. *)
      let error = Printf.sprintf "Apac_miscellaneous.gwd: `%s' exists, but it \
                                  is not a directory." path in
      failwith error
  else
    (** Otherwise, create the destination directory with ususal permission set
        and return the path to it. *)
    begin
      Sys.mkdir path 644;
      path
    end

(** [gdot]: returns the full path (see [!gwd]) to a Dot file containing the task
    candidate graph of a function [f]. The name of the Dot file may carry an
    optional [suffix]. *)
let gdot ?(suffix : string = "") (f : var) : string =
  let dir = gwd () in
  let name = f.name ^ "-" ^ (string_of_int f.id) ^
               (if suffix <> "" then "-" ^ suffix else "") ^ ".dot" in
  dir ^ "/" ^ name

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
       let _ = Printf.printf "%s has finally %d derefs\n" var.name d' in
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
         | Unop_cast ty -> aux (d + typ_get_degree ty) l t
         | Unop_struct_access field -> aux (d + 1) field t
         (* A structure access through pointer, e.g. [operand->field], means
            that the operand was not dereferenced. To finish finished resolving,
            iterate once more on [t]. *)
         | Unop_struct_get field -> aux d field t
         (* In case of another binary operation, do nothing and continue
            resolution on the operand. *)
         | _ -> aux d l t
       end
    | _ -> None
  in
  aux 0 "" t
