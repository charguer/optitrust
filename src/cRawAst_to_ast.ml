open Ast

(* environment for storing the mutability of all the variables *)
type env = varkind String_map.t

(* empty environment *)
let env_empty =
  String_map.empty

(* [get_varkind env x] get mutability of variable [x] *)
let get_varkind (env : env) (x : var) : varkind =
  match String_map.find_opt x env with
  | Some m -> m
  | _ -> Var_immutable (* Functions that come from an external library are set to immutable by default *)

(* [is_var_mutable env x] check if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_varkind env x = Var_mutable

(* [env_extend env e varkind] add variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  String_map.add e varkind env

(* [add_var env x xm] add variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit =
  env := env_extend !env x xm

(* [trm_simplify_addressof_and_get t] simplifies [&*t] and [*&t] to [t] *)
let trm_simplify_addressof_and_get (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) }
    ])
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [
      {desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [t1]) }
    ]) -> t1
  | _ -> t

(* OPTIMIZATION (keep this code)
(* [trm_address_of ~simplify t] adds the address operator before [t].
    if [simplify] is true and [t] is of the form [*u] then it will return just [u] *)
let trm_address_of ?(simplify : bool = false) (t : trm) : trm =
  let aux t1 = trm_apps (trm_unop Unop_address) [t1] in
  if not simplify then  aux t
    else match t.desc with
    | Trm_apps (f, [t1]) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop Unop_get) -> t1
      | _ -> aux t
      end
    | _ -> aux t

(* [trm_get ~simplify t] adds the star operator before [t].
   if [simplify] is true and [t] is of the form [&u] then it will return just [u] *)
let trm_get ?(simplify : bool = false) (t : trm) : trm =
  let aux t1 = trm_apps (trm_unop Unop_get) [t1] in
  if not simplify then aux t
    else match t.desc with
    | Trm_apps (f, [t1]) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop Unop_address) -> t1
      | _ -> aux t
      end
    | _ -> aux t
*)

let trm_address_of ?(simplify : bool = false) (t : trm) : trm =
  assert (simplify);
  let u = trm_apps (trm_unop Unop_address) [t] in
  trm_simplify_addressof_and_get u

let trm_get ?(simplify : bool = false) (t : trm) : trm =
  assert (simplify);
  let u = trm_apps (trm_unop Unop_get) [t] in
  trm_simplify_addressof_and_get u


(* [onscope env t f] save the current environment before entering a new scope,
    the revert back to the saved env after leaving the scope.

    Usage:Global variables stay always in the environment but local variables like function args, loop indices etc are added before entering the scope
    and removed when leaving the scope *)
let onscope (env : env ref) (t : trm) (f : trm -> trm) : trm =
    let saved_env = !env in
    let res = f t in
    env := saved_env;
    res

(* [create_env] creates an empty environment *)
let create_env () = ref env_empty


(* [stackvar_elim t] replaces
    [int a = 5] with [<annotation:stackvar> int* a = new int(5)]
      and a variable occurrence [a] becomes [ * a]
   The transformation leaves [const int c = 5] unchanged.

   This transformation introduces many [*] operators. This may lead to the production of
   [&*p] patterns. They are simplified into [p] on the fly.

   For references, [int& b = a] becomes [<annotation:reference> int* b = a] as a simplification of [b = &*a]
   and [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [const int*].

   Note that in the input ast, [p->f] is represented as [( *p ).f @"annot:Display_arrow"].

   Here, the "reference" annotation is added to allow decoding.
   LATER: Support references on constants. *)
let stackvar_elim (t : trm) : trm =
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      (* x when x is mutable becomes *x, where the ' * 'is used only for encoding purposes, hence not visible to the user *)
      if is_var_mutable !env x
        then trm_annot_add Mutable_var_get (trm_get ~simplify:true t) (* Note: simplify not needed here *)
        else { t with desc = Trm_var (Var_immutable, x) }
    | Trm_let (_, (x, ty), tbody) ->
      (* mutability is deducted from the declaration of the variable, by checking if it has a const type or not *)
      let xm = if is_typ_const ty then Var_immutable else Var_mutable in
      add_var env x xm;
      begin match typ_ref_inv ty with
      | Some ty1 ->
        begin match xm with
        | Var_immutable -> fail t.loc "stackvar_elim: unsupported references on const variables"
        | _ ->
          trm_annot_add Reference {t with desc = Trm_let (xm, (x, typ_ptr_generated ty1), trm_address_of ~simplify:true (aux tbody))}
        end
      | None ->
        begin match xm with
        | Var_mutable ->
          trm_annot_add Stackvar {t with desc = Trm_let (xm, (x, typ_ptr_generated ty), trm_new ty (aux tbody) )}
        | Var_immutable ->
          trm_map aux t
        end
      end
    | Trm_seq _ -> onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody) ->
      (* function names are by default immutable *)
      add_var env f Var_immutable;
      onscope env t (fun t -> List.iter (fun (x, _tx) ->
       let mut = Var_immutable in (* if is_typ_ptr tx then Var_mutable else Var_immutable in *)
       add_var env x mut) targs; trm_map aux t)
    | Trm_for (index, _, _, _, _, _) ->
        onscope env t (fun t -> add_var env index Var_immutable; trm_map aux t)
    | Trm_for_c _ ->
        onscope env t (fun t -> trm_map aux t)
    (* OPTIMIZATION Simplification of [&*p] patterns
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _} as op, [t1]) ->
        let u1 = aux t1 in
        begin match u1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [u11]) -> u11
        | _ -> { t with desc = Trm_apps (op, [u1]) }
        end *)
    | _ -> trm_map aux t
   end in
   aux t


(* TODO: do we need to produce any Address_operator annotation?
   if we don't need, then we might need to change a few things in transformations that depend on this annotation. *)

(* [stackvar_intro t] is the reciprocal to [stackvar_elim]. It replaces [<annotation:stackvar> int *a = new int(5)] with [int a = 5]
    and a variable occurrence [*a] becomes [a] if it corresponds to a stack variable
    (as a simplification to [*(&a)])
    For references, [<annotation:reference> int* b = a] becomes [int& b = a],
      as a simplification of b = *(&a), where &a is obtained after translating a.
    and [<annotation:reference> int* x = &t[i]] becomes [int& x = t[i]], where t has type [const int*] as a simplification of x = *(&t[i])
*)
let stackvar_intro (t : trm) : trm =
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      if is_var_mutable !env x
        then trm_address_of ~simplify:true t (* ~simplify:true  is not technically needed *)
        else t
    | Trm_let (_, (x, tx), tbody) ->
      let vk = if is_typ_const tx then Var_immutable else Var_mutable  in
      add_var env x vk;
      if trm_annot_has Stackvar t
        then
          begin match tx.typ_desc , tbody.desc with
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, Trm_apps ({desc = Trm_val (Val_prim (Prim_new _));_}, [tbody1])  ->
              trm_annot_remove Stackvar {t with desc = Trm_let (vk, (x, tx1), aux tbody1)}
          | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Stackvar on this declaration"
          end
      else if List.mem Reference t.annot then
        begin match tx.typ_desc with
        | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1} ->
          trm_annot_remove Reference { t with desc = Trm_let (vk, (x,typ_ptr Ptr_kind_ref tx1), trm_get ~simplify:true (aux tbody))}
        | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Reference on this declaration"
        end
      else
        {t with desc = Trm_let (vk, (x, tx), aux tbody)}
    | Trm_seq _ ->
      onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody) ->
      add_var env f Var_immutable;
      onscope env t (fun t ->
      List.iter (fun (x, _tx) -> let mut = Var_immutable  in (add_var env x mut)) targs; trm_map aux t)
    | Trm_for (index, _, _, _, _, _) ->
      onscope env t (fun t -> begin add_var env index Var_immutable; trm_map aux t end)
    | Trm_for_c _ ->
      onscope env t (fun t -> trm_map aux t)
     | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var (_,x); _} as t1]) when is_var_mutable !env x  -> t1
    (* OPTIMIZATION Simplification of [*&p] patterns
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op, [t1]) ->
        let u1 = aux t1 in
        begin match u1.desc with
        | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _}, [u11]) -> u11
        | _ -> { t with desc = Trm_apps (op, [u1]) }
        end *)
    | _ -> trm_map aux t
    end in
   aux t

(* [caddress_elim_aux false t] eliminates the use of l-values in the AST
    [* t1]  becomes [get(t1)]
    [* t1 = t2] becomes as [set(t1, t2)]
    [t[i] = t[i] + 1] is encoded as [set(t+i, get(t+i) + 1)]
    [t.f = t.f + 1] is encoded as [set(t+offset(f), get(t + offset(f)) + 1)]
    [( * p).f.x] is encoded as [get(p+offset(f)+ offset(x))]
    [( * p).f.x = 3] is encoded as [set(p+offset(f) + offset(x), 3)]
    [( * p) [ i ] ]  is encoded as [get(p+i)]
    [t+i] is represented in optitrust as [Trm_apps (Prim_array_acces, [t;i])] in the AST
    [t+offset(f)] is represented in optitrust as [Trm_apps (Prim_struct_access "f", [t])]

    This transformation is implemented as [caddress_elim_aux lvalue t], where the
    boolean [lvalue] indicates whether we are currently translating a l-value
    or a normal instruction or expression (r-value).
*)
let rec caddress_elim_aux (lvalue : bool) (t : trm) : trm =
  let aux t = caddress_elim_aux false t in (* recursive calls for rvalues *)
  let access t = caddress_elim_aux true t in (* recursive calls for lvalues *)
  let mk ?(annot = []) td = {t with desc = td; annot = annot} in
  trm_simplify_addressof_and_get
  begin if lvalue then begin
    match t.desc with
     (* [t.f] is translated to [struct_access(access t, f)] *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as op, [t1]) ->
      let u1 = access t1 in
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [u1]))
      (* [t[i]] is translated to [array_access(access t, aux i)] *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get));_} as op, [t1; t2]) ->
      let u1 = aux t1 in (*DEPRECATED? access t1*)
      let u2 = aux t2 in
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u1; u2]))
      (* DEPRECATED begin match u1.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op1, [u11]) ->
        mk ~annot:u1.annot (Trm_apps (op1, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop Binop_array_access))}, [u11; u2]))]))
      | _ ->mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u1; u2]))
      end*)

      (* *t1 becomes to [*(aux t1)] if '*' is not a hidden get operation, otherwise it becomes  [aux t1] *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as _op, [t1]) ->
      (* DEPRECATED
      if List.mem Mutable_var_get t.annot
        then aux t1
        else mk (Trm_apps (op, [aux t1]))*)
      aux t1
    | Trm_var (_, x) -> fail t.loc (Printf.sprintf "caddress_elim: const variable '%s' cannot appear as lvalue (mutation of function arguments is not supported in OptiTrust)" x)
    | _ -> fail t.loc (Printf.sprintf "caddress_elim: invalid lvalue, %s\n------------\n%s\n" (Ast_to_rawC.ast_to_string t) (Ast_to_text.ast_to_string t))
  end else begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op _));_}, _) ->
      fail t.loc "caddress_elim_aux: compound assignments must be eliminated beforehand"
    (* [t1 = t2] is translated to  [set (access t1, aux t2) *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set));_} as op, [t1; t2]) ->
      let u1 = access t1 in
      let u2 = aux t2 in
      mk (Trm_apps (op, [u1; u2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f))); _} as op, [t1]) ->
      let u1 = aux t1 in
      begin match u1.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op1, [u11])  ->
        (* struct_get (get(t1), f) is encoded as get(struct_access(t1,f)) where get is a hidden '*' operator,
            in terms of C syntax: ( * t).f is compiled into * (t + offset(f)) *)
        mk ~annot:u1.annot (Trm_apps (op1, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [u11]))]))
      | _ -> mk (Trm_apps (op, [u1]))
      end

     | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get))); _} as _op, [t1; t2]) ->
        let u1 = aux t1 in
        let u2 = aux t2 in
        trm_get ~simplify:true { t with desc = Trm_apps ({ t with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u1; u2]) }

    (* DEPRECATED | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get))); _} as op, [t1; t2]) ->
      let u1 = aux t1 in
      let u2 = aux t2 in
      begin match u1.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op1, [u11]) ->
        (* array_get (get(t1), t2) is encoded as get(array_access (t1, t2) *)
        mk ~annot:u1.annot (Trm_apps (op1, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u11; u2]))]))
      | _ -> mk (Trm_apps (op, [u1;u2]))
      end*)
    (* OPTIMIZATION Simplification of [&*p] patterns
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address)); _} as op, [t1]) ->
      let u1 = aux t1 in
      begin match u1.desc with
      | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [u11]) -> u11
      | _ -> { t with desc = Trm_apps (op, [u1]) }
      end *)
    | _ -> trm_map aux t
    end
  end

let caddress_elim = caddress_elim_aux false


(* [is_access t] check if trm t is a struct access or an array access *)
let is_access (t : trm) : bool =
  match t.desc with
  | Trm_apps (tprim, _) ->
    begin match trm_prim_inv tprim with
    | Some (Prim_unop (Unop_struct_access _) |  (Prim_binop (Binop_array_access))) -> true
    | _ -> false
    end
  | _ -> false

(* [caddress_intro_aux false t] is the inverse of [caddress_elim]
    [get(t1)  becomes ][* t1]
    [set(t1, t2)] becomes as [* t1 = t2]
    [Trm_apps (Prim_struct_access "f", [t])] becomes [t.f] as lvalue
    [get(Trm_apps (Prim_struct_access "f", [t]))] becomes [t.f] as rlvalue
 *)

let rec caddress_intro_aux (lvalue : bool) (t : trm) : trm =
  let aux t = caddress_intro_aux false t in  (* recursive calls for rvalues *)
  let access t = caddress_intro_aux true t in (* recursive calls for lvalues *)
  let mk td = {t with desc = td} in
  trm_simplify_addressof_and_get (* Note: might not be needed *)
  begin if lvalue then begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f))); _} as op, [t1]) ->
      (* struct_access (f, t1) is reverted to struct_get (f, access t1) *)
      let u1 = access t1 in
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [u1]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _} as op, [t1; t2]) ->
      (* array_access (t1, t2) is reverted to array_get (aux t1, aux t2) *)
      let u1 = aux t1 in
      let u2 = aux t2 in
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)))}, [u1; u2]))
    | _ -> trm_get ~simplify:true (aux t)
    end
    else begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _}, [t1; t2]) ->
      (* t1 = t2 is translated to access t1 = aux t2 *)
      let u1 = access t1 in
      let u2 = aux t2 in
      mk (Trm_apps (trm_binop Binop_set, [u1; u2]))
    (* OPTIMIZATION
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) when is_access t1 ->
      (* get(access ) becomes access (get)*) (* LATER: the simplification could be performed a posteriori, if we use the following case *)
      access t1 *)
    | _ when is_access t ->
      (* [access(..)] becomes [& ...] *)
        trm_address_of ~simplify:true (access t)
    | _ -> trm_map aux t
    end
  end

let caddress_intro = caddress_intro_aux false

(* [cseq_items_void_type t] updates [t] in such a way that all instructions appearing in sequences
   have type [Typ_unit]. This might not be the case, for example on [x += 2], Menhir provides an
   [int] type, whereas [Clang] provides a [void] type. *)

let rec cseq_items_void_type (t : trm) : trm =
  let t2 = trm_map cseq_items_void_type t in
  match t2.desc with
  | Trm_seq ts ->
      let enforce_unit (u : trm) : trm =
        match u.typ with
        | Some { typ_desc = Typ_unit; _ } -> u
        | _ -> { u with typ = Some (typ_unit()) }
        in
      { t2 with desc = Trm_seq (Mlist.map enforce_unit ts) }
  | _ -> t2


(* Main entry points *)

(* [cfeatures_elim t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let cfeatures_elim (t : trm) : trm =
  cseq_items_void_type (caddress_elim (stackvar_elim t))

(* [cfeatures_intro t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let cfeatures_intro (t : trm) : trm =
  stackvar_intro (caddress_intro t)

(* [cfeatures_intro_aux lvalue t] is similar to [cfeatures_intro] but allows printing lvalues *)
let cfeatures_intro_aux (lvalue : bool) (t : trm) : trm =
  stackvar_intro (caddress_intro_aux lvalue t)

(* [trm_map_with_lvalue] is a variant of [trm_map] that provides the [is_lvalue] information to [f]. *)
let trm_map_with_lvalue (f : bool -> trm -> trm) (t : trm) : trm =
  match t.desc with
  | Trm_apps ({desc = Trm_val (Val_prim ((Prim_unop (Unop_struct_access _) | Prim_binop Binop_array_access))); _} as op, [t1]) ->
      (* struct_access (f, t1) or array_access (t1, t2) *)
      let u1 = f true t1 in
      { t with desc = Trm_apps (op, [u1]) }
  | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _} as op, [t1; t2]) ->
      (* [t1 = t2] assignment *)
      let u1 = f true t1 in
      let u2 = f false t2 in
      { t with desc = Trm_apps (op, [u1; u2]) }
  | _ -> trm_map (f false) t


(* See next function *)
let rec annotate_string_representation_aux (f : trm -> bool) (lvalue : bool) (t : trm) : trm =
  let t2 =
    if not (f t) then t else begin
      let strm =
        if !Flags.use_new_encodings
          then Ast_to_rawC.ast_to_string (cfeatures_intro_aux lvalue t)
          else Ast_to_c.ast_to_string t
          in
      let strm = if lvalue then "LVALUE " ^ strm else strm in
      (* LATER: is it needed to remove previous string annotations? probably not *)
      trm_annot_add (Ast.Annot_string_repr strm) t
    end in
  trm_map_with_lvalue (annotate_string_representation_aux f) t2

(* [annotate_string_representation f t] takes a term [t] (assumed to be a full program or a left value)
   and annotate all nodes in [t] satisfying the predicate [f] (typically filterning on the kind)
   with an annotation [Annot_string_repr] carrying the string representation, which may be used
   for regexp matching in constraint resolution, or for functions such as [view_subterms]. *)
let annotate_string_representation (f : trm -> bool) (t : trm) : trm =
  annotate_string_representation_aux f false t



