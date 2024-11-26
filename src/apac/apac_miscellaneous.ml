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

(** [typ_is_alias ty]: checks if the type [ty] is a constructed type aliasing a
    basic type. *)
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

(** [typ_get_alias ty]: if the type [ty] is a constructed type aliasing a basic
    type, it returns the latter. *)
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
        final number of levels of indirection of [ty]. *)
    | _ -> nli
  in
  (** Start counting the levels of indirection of [ty] at level [0]. *)
  aux 0 ty

(** [trm_reduce_and_apply f t]: tries to reduce the term [t] to a unique memory
    location and applies the [f] function on the latter. If the reduction
    succeeds, the function returns the result of [f]. *)
let trm_reduce_and_apply
      (f : var -> varkind -> label -> int -> 'a option) (t : trm) : 'a option =
  (** [trm_reduce_and_apply.aux nli l t]: auxiliary function to recursively
      inspect [t]. If the latter features strcture or class member accesses, [l]
      will keep the track of the label of the member. [nli] counts the number of
      levels of indirections while accessing to the resulting memory location,
      if any. *)
  let rec aux (nli : int) (l : label) (t : trm) : 'a option =
    match t.desc with
    (** When [t] is a unary operation [op] and more precisely *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_unop op)); _ }, [t]) ->
       begin match op with
       (** a dereferencement, e.g. [*i], the number of levels of indirection
           decreases. *)
       | Unop_get -> aux (nli - 1) l t
       (** a referencement, e.g. [&i], the number of levels of indirection
           increases. *)
       | Unop_address -> aux (nli + 1) l t
       (** a cast, the number of levels of indirection corresponds to the sum of
           the current number of levels of indirection [nli] and the number of
           levels of indirection of the destination type. *)
       | Unop_cast ty -> aux (nli + typ_get_nli ty) l t
       (** a structure or a class member access, e.g. [structure.member] or
           [structure->member], extract the label of the member field [f]
           involved in the operation and continue the reduction. *)
       | Unop_struct_access f -> aux nli f t
       | Unop_struct_get f -> aux nli f t
       (** Otherwise, there is nothing to reduce. *)
       | _ -> None
       end
    (** When [t] is a binary operation [op] with a left-hand side term [lhs] and
        a right-hand side term [rhs], we have two possible situations. *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_binop op));
                  _ }, [lhs; rhs]) ->
       begin match op with
       (** When [t] is an array access, i.e. a dereferencement, the number of
           levels of indirection decreases and we continue the reduction on the
           accessed memory location [lhs]. *)
       | Binop_array_access
         | Binop_array_get -> aux (nli - 1) l lhs
       (** Otherwise, we continue the reduction on both [lhs] and [rhs]. *)
       | _ ->
          begin match (aux nli l lhs, aux nli l rhs) with
          | Some (res), None -> Some (res)
          | None, Some (res) -> Some (res)
          | None, None -> None
          (** For now, we do not know what to do when the binary expression
              involves more than one variable. *)
          | Some (_), Some (_) ->
             fail
               t.loc ("Apac_miscellaneous.trm_reduce_and_apply.aux: nor the \
                       dependency discovery nor the alias analysis support \
                       rvalues, such as`" ^
                        (AstC_to_c.ast_to_string t) ^
                          "', featuring more than one variable.")
          end
       end
    (** When [t] is a [new] operation, explore the initialization term *)
    | Trm_apps ({ desc = Trm_val (Val_prim (Prim_new _)); _ }, [t]) ->
       aux nli l t
    (** When [t] leads to a variable [v], call [f]. *)
    | Trm_var (vk, v) ->
       f v vk l nli
    (** Otherwise, there is nothing to reduce. *)
    | _ -> None
  in
  (** Call the auxiliary function. *)
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

(** [find_parent p f]: goes back up the path [p] and returns the path to the
    first term satisfying the predicate [f] or [None] if [p] does not feature
    such term. *)
let find_parent (p : Path.path) (f : trm -> bool) : Path.path option =
  (** We shall go back on our steps in the path, i.e. in the direction of the
      root of the AST, so we need to reverse [p]. *)
  let rev = List.tl (List.rev p) in
  (** We use an auxiliary function in order to hide to the outside world the
      need for the path reversal. *)
  let rec aux (p : Path.path) : Path.path option =
    (** The function simply goes recursively through [rev] and *)
    match p with
    | _ :: r -> 
       begin
         (** if it finds a term satisfying [f], it returns the path to it. *)
         let p' = List.rev p in
         let t = Path.get_trm_at_path p' (Trace.ast ()) in
         if f t then Some p' else aux r
       end
    | [] -> None
  in
  aux rev

(** [has_trm p f]: goes back up the path [p] and checks whether it features a
    term verifying the predicate [f]. *)
let has_trm (p : Path.path) (f : trm -> bool) : bool =
  (** Go recursively through the path [p], *)
  let rec aux (p : Path.path) : bool =
    match p with
    | _ :: r ->
       (** if the term [t] behind the current path [p] verifies the predicate
           [f], return [true] and stop exploring the path. Otherwise, continue
           on the remaining part [r] of [p]. *)
       let t = Path.get_trm_at_path (List.rev p) (Trace.ast ()) in
       if f t then true else aux r
    | [] -> false
  in
  (** Apply the auxiliary function on the reversed [p]. *)
  aux (List.rev p)
