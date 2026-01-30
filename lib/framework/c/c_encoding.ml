open Ast
open Trm
open Typ
open Contextualized_error

type style = {
  typing : Style.typing_style;
  cstyle : Ast_to_c.style;
}

let style_of_output_style (style : Style.output_style) : style =
  match style.print with
  | Lang_C s -> { typing = style.typing; cstyle = s }
  | _ -> { typing = style.typing; cstyle = Ast_to_c.default_style () }

let default_style () : style =
  style_of_output_style (Style.default_style ())

let debug = false

let debug_before_after_trm (msg : string) (f : trm -> trm) : trm -> trm =
  if debug then (fun t ->
    File.put_contents (sprintf "/tmp/%s_before.txt" msg) (Ast_to_text.ast_to_string t);
    let t2 = f t in
    File.put_contents (sprintf "/tmp/%s_after.txt" msg) (Ast_to_text.ast_to_string t2);
    t2
  ) else f

let debug_current_stage (msg: string) : unit =
  if debug then Tools.debug "%s" msg

let var_uninitialized = Ast_to_c.var_uninitialized
let trm_uninitialized = Ast_to_c.trm_uninitialized

(*

  t[i]  =  get(array_access(t,i))  = array_read(t,i)
       // array_read_inv also
       // array_access(t,i) corresponds to the C code t+i, which is the same as (( void* )t)+i*(sizeof(typeof(t[0]))

  t->f  =  get(struct_access(t,f)) = struct_read(t,f)
       // struct_access(t,f)  corresponds to the C code for   t + offset(f)

  ( *t ).f  =  t->f

  t.f  where t is not a get(.)    -> struct_get(t,"f")

   Recall: struct_get(t,f)  means  trm_apps (Prim_unop (unop_struct_get "f")) [t] *)

(** [mutability]: type for the mutability of the variable *)
type mutability =
  | Var_immutable (* const variables *)
  | Var_mutable   (* non-const stack-allocated variable. *)

(** [env]: a map for storing all the variables as keys, and their mutability as values *)
type env = mutability Var_map.t

(** [env_empty]: empty environment *)
let env_empty =
  Var_map.empty

(** [get_mutability env x]: gets the mutability of variable [x]
   Note: Functions that come from an external library are set to immutable by default *)
let get_mutability (env : env) (x : var) : mutability =
  match Var_map.find_opt x env with
  | Some m -> m
  | _ -> Var_immutable

(** [is_var_mutable env x]: checks if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_mutability env x = Var_mutable

(** [env_extend env e mutability]: adds variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (mutability : mutability) : env =
  Var_map.add e mutability env

(** [add_var env x xm]: adds variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : mutability) : unit =
  env := env_extend !env x xm

(** [trm_address_of t]: adds the "&" operator before [t]
    Note: if for example t = *a then [trm_address_of t] = &( *a) = a *)
let trm_address_of (t : trm) : trm =
  let u = trm_address_of t in
  trm_simplify_addressof_and_get u

(** [trm_get t]: adds the "*" operator before [t]
    Note: if for example t = &a then [trm_get t] = *( &a) = a *)
let trm_get ?(typ: typ option) (t : trm) : trm =
  let u = trm_get ?typ t in
  trm_simplify_addressof_and_get u

(** [onscope env t f]: applies function [f] on [t] without loosing [env]
   Note: This function is used for keeping track of opened scopes *)
let onscope (env : env ref) (t : trm) (f : trm -> trm) : trm =
    let saved_env = !env in
    let res = f t in
    env := saved_env;
    res

(** [create_env]: creates an empty environment *)
let create_env () = ref env_empty

(** [decode_stackvar t]: applies the following encodings
    - [int a = 5] with [int* a = ref int(5)]
    - a variable occurrence [a] becomes [* a]
    - [const int c = 5] becomes [int c = 5]
    - simplify patterns of the form [&*p] and [*&p] into [p].
    - [int& b = a] becomes [<annotation:reference> int* b = &*a] which simplifies to [<annot..>int* b = a]
    - [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [int* const].
    - More complicated example: [int a = 5; int* b = &a] becomes [int* a = ref int(5); int** b = ref int*(a);]
    TODO: specify and improve support for arrays

   Note: "reference" annotation is added to allow decoding *)
 (* TODO: properly deal with const/mut array allocations on stack using Prim_ref_array *)
let decode_stackvar (t : trm) : trm =
  debug_current_stage "decode_stackvar";
  let env = create_env () in
  let extract_constness (ty: typ): typ * mutability =
    Pattern.pattern_match ty [
      Pattern.(typ_const !__) (fun ty () -> ty, Var_immutable);
      Pattern.(typ_fun __ __) (fun () -> ty, Var_immutable);
      Pattern.__ (fun () -> ty, Var_mutable)
    ]
  in
  let rec aux (t : trm) : trm =
    let t = match t.desc with
    | Trm_var x ->
      if is_var_mutable !env x
        then trm_like ~old:t (trm_get ?typ:t.typ (trm_var ?typ:(Option.map typ_ptr t.typ) x))
        else t
    | Trm_let ((x, ty), tbody) ->
      Pattern.pattern_match ty [
        Pattern.(typ_const !__ ^| !(typ_fun __ __)) (fun ty () ->
          add_var env x Var_immutable;
          let new_body = aux tbody in
          trm_replace (Trm_let ((x, ty), new_body)) t
        );
        Pattern.(typ_array !__ __) (fun elem_ty () ->
          add_var env x Var_immutable;
          let new_body = match trm_var_inv tbody with
            | Some v when var_eq v var_uninitialized -> trm_ref_uninit ty
            | _ -> trm_ref ty (aux tbody)
          in
          trm_replace (Trm_let ((x, typ_ptr elem_ty), new_body)) t
        );
        Pattern.(typ_ref !__) (fun ty () ->
          add_var env x Var_mutable;
          trm_add_cstyle Reference (trm_replace (Trm_let ((x, typ_ptr ty), trm_address_of (aux tbody))) t)
        );
        Pattern.__ (fun () ->
          add_var env x Var_mutable;
          let new_body = match trm_var_inv tbody with
            | Some v when var_eq v var_uninitialized -> trm_ref_uninit ty
            | _ -> trm_ref ty (aux tbody)
          in
          trm_replace (Trm_let ((x, typ_ptr ty), new_body)) t
        )
      ]
    | Trm_let_mult bs ->
      (* FIXME: Broken with arrays *)
      let bs = List.map (fun ((x, ty), tbody) ->
          let ty, xm = extract_constness ty in
          add_var env x xm;
          match xm with
          | Var_immutable -> ((x, ty), aux tbody)
          | Var_mutable ->
            let tbody = match trm_var_inv tbody with
            | Some v when var_eq v var_uninitialized -> trm_ref_uninit ty
            | _ -> trm_ref ty (aux tbody)
            in
            ((x, typ_ptr ty), tbody)
        ) bs
      in
      trm_replace (Trm_let_mult bs) t
    (*| Trm_predecl (x, ty) ->
      (* FIXME: Broken with arrays *)
      let ty, xm = extract_constness ty in
      add_var env x xm;
      begin match typ_ref_inv ty with
      | Some ty1 ->
        begin match xm with
        | Var_immutable -> trm_fail t "C_encoding.decode_stackvar: unsupported references on const variables"
        | _ -> trm_add_cstyle Reference (trm_replace (Trm_predecl (x, typ_ptr ty1)) t)
        end
      | None ->
        begin match xm with
        | Var_mutable -> trm_replace (Trm_predecl (x, typ_ptr ty)) t
        | Var_immutable -> trm_replace (Trm_predecl (x, ty)) t
        end
      end*)
    | Trm_seq (_, result) when not (trm_is_nobrace_seq t) ->
      onscope env t (fun t ->
        let t = trm_map aux t in
        match result with
        | Some r when is_var_mutable !env r -> trm_fail t "C_encoding.decode_stackvar: the result of a sequence cannot be a mutable variable"
        | _ -> t
      )
    | Trm_for (range, _, _, _) ->
        onscope env t (fun t -> add_var env range.index Var_immutable; trm_map aux t)
    | Trm_for_c _ ->
        onscope env t (fun t -> trm_map aux t)
    | _ -> trm_map aux t
    in
    let t = trm_simplify_addressof_and_get t in
    let ty = Option.map (fun ty -> fst (extract_constness ty)) t.typ in
    { t with typ = ty }
   in
   debug_before_after_trm "decode_stackvar" aux t


(** [encode_stackvar t]: is the inverse of [decode_stackvar], hence it applies the following decodings:
     - [int *a = ref int(5)] with [int a = 5]
     - [const int c = 5] remains unchanged
     - [<annotation:reference> int* b = a] becomes [int& b = *&(a)], which simplifies as [int& b = a]
        where &a is obtained after translating [a]
     - [<annotation:reference> int* x = &t[i]] becomes [int& x = *(&t[i])], where t has type [const int*]
       which simplifies to x = t[i]
     - [x] where [x] is mutable becomes [&x] *)
 (* TODO: some marks are lost in the printing, and this needs to be fixed,
    typically [*x] in the optitrust ast is printed as [x], and if the star operation
    was carrying a mark, then it is currently not displayed! *)
let encode_stackvar (t : trm) : trm =
  debug_current_stage "encode_stackvar";
  let env = create_env () in
  let add_const (ty: typ) : typ =
    Pattern.pattern_match ty [
      Pattern.(typ_array !__ !__) (fun ty size () -> typ_array ?size (typ_const ty));
      Pattern.(typ_fun __ __) (fun () -> ty);
      Pattern.__ (fun () -> typ_const ty)
    ]
  in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var x ->
      if is_var_mutable !env x
        then trm_address_of t
        else t
    | Trm_let ((x, tx), tbody) ->
      begin match typ_ptr_inv tx, trm_ref_maybe_init_inv tbody with
      | _, Some (tx, tbody) ->
        add_var env x (if is_typ_array tx then Var_immutable else Var_mutable);
        let tbody = Option.map_or aux trm_uninitialized tbody in
        trm_replace (Trm_let ((x, tx), tbody)) t
      | Some tx1, None when trm_has_cstyle Reference t ->
        add_var env x Var_mutable;
        trm_rem_cstyle Reference { t with desc = Trm_let ((x, typ_ref tx1), trm_get (aux tbody))}
      | Some tx1, None when trm_has_cstyle Constructed_init tbody ->
        add_var env x Var_mutable;
        trm_replace (Trm_let ((x, tx1), aux tbody)) t
      | _ ->
        add_var env x Var_immutable;
        trm_replace (Trm_let ((x, add_const tx), aux tbody)) t
      end
    | Trm_let_mult bs ->
      (* FIXME: Is it possible to handle C++ references and Constructed_init in let_mult ? *)
      let bs = List.map (fun ((x, tx), tbody) ->
        match typ_ptr_inv tx, trm_ref_maybe_init_inv tbody with
        | Some tx, Some (_, tbody)  ->
          add_var env x Var_mutable;
          let tbody = Option.map_or aux trm_uninitialized tbody in
          ((x, tx), tbody)
        | _ ->
          add_var env x Var_immutable;
          ((x, add_const tx), aux tbody)
      ) bs in
      trm_replace (Trm_let_mult bs) t
    (*| Trm_predecl (x, tx) ->
      (* LATER: There is a confusion between extern int A; and extern int* const A; that are both encoded in the same way although they seem to not link together. Currently we always display encode it as extern int A; in this case. *)
      begin match typ_ptr_inv tx with
      | Some tx1 when trm_has_cstyle Reference t ->
        add_var env x Var_mutable;
        trm_rem_cstyle Reference { t with desc = Trm_predecl (x, typ_ref tx1)}
      | Some tx1 ->
        add_var env x Var_mutable;
        trm_replace (Trm_predecl (x, tx1)) t
      | _ ->
        add_var env x Var_immutable;
        trm_replace (Trm_predecl (x, add_const tx)) t
      end*)
    | Trm_seq _ when not (trm_is_nobrace_seq t) ->
      onscope env t (trm_map aux)
    | Trm_for (range, _, _, _) ->
      onscope env t (fun t -> begin add_var env range.index Var_immutable; trm_map aux t end)
    | Trm_for_c _ -> onscope env t (fun t -> trm_map aux t)
    | Trm_apps ({desc = Trm_prim (_, Prim_unop Unop_get);_}, [{desc = Trm_var x; _} as t1], _, _) when is_var_mutable !env x -> t1
    | _ -> trm_map aux t
    end in
   aux t


(** [remove_const t]: removes all remaining const on types since those can mess with type unification *)
let remove_const (t: trm): trm =
  debug_current_stage "remove_const";
  let rec remove_const_in_typ (ty: typ): typ =
    match typ_const_inv ty with
    | Some ty -> remove_const_in_typ ty
    | None -> trm_map remove_const_in_typ ty
  in
  let rec aux (t: trm): trm =
    match t.desc with
    | Trm_let ((x, ty), body) ->
      trm_replace (Trm_let ((x, remove_const_in_typ ty), aux body)) t
    | Trm_let_mult bindings ->
      trm_replace (Trm_let_mult (List.map (fun ((x, ty), body) -> ((x, remove_const_in_typ ty), aux body)) bindings)) t
    | Trm_fun (args, rettyp, body, contract) ->
      let args = List.map (fun (x, ty) -> (x, remove_const_in_typ ty)) args in
      trm_replace (Trm_fun (args, remove_const_in_typ rettyp, aux body, contract)) t
    | Trm_prim (ty, prim) ->
      trm_replace (Trm_prim (remove_const_in_typ ty, prim)) t
    | _ -> trm_map aux t
  in
  aux t


(** [decode_caddress t]: applies the following encodings
     - [get(t).f] becomes get(t + offset(f))
     - [t.f] becomes t + offset(f) -- nothing to do in the code of the translation
     - [get(t)[i] ] becomes [get (t + i)]
     - [t[i]] becomes [t + i] -- nothing to do in the code of the translation
     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_prim (Prim_array_access, [t; i]))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_prim (Prim_struct_access "f"),[t])] *)
 (* TODO: properly deal with const/mut array allocations on stack using Prim_ref_array *)
let decode_caddress (t : trm) : trm =
  debug_current_stage "decode_caddress";
  let rec aux t =
    trm_simplify_addressof_and_get
      (Pattern.pattern_match t [
        Pattern.(trm_struct_get !__ !__) (fun t1 field () ->
          let field_typ = t.typ in
          let struct_typ = Option.unsome t1.typ in
          let u1 = aux t1 in
          match trm_get_inv u1 with
          | Some base ->
            (* struct_get (get(t1), f) is encoded as get(struct_access(t1,f)) where get is a hidden '*' operator,
                in terms of C syntax: ( *t).f is compiled into *(t + offset(f)) *)
            if trm_has_cstyle No_struct_get_arrow t then
              trm_like ~old:(trm_rem_cstyle No_struct_get_arrow t) (trm_get (trm_add_cstyle No_struct_get_arrow (trm_struct_access ?loc:t.loc ?field_typ ~struct_typ base field)))
            else
              trm_like ~old:t (trm_get (trm_struct_access ?loc:t.loc ?field_typ ~struct_typ base field))
          | None -> trm_like ~old:t (trm_struct_get ?loc:t.loc ?field_typ ~struct_typ u1 field)
        );
        Pattern.(trm_array_get !__ !__) (fun t1 t2 () ->
            let u1 = aux t1 in
            let u2 = aux t2 in
            trm_like ~old:t (trm_get (trm_array_access ?loc:t.loc u1 u2))
        );
        Pattern.__ (fun () -> trm_map aux t)
      ])
  in
  debug_before_after_trm "decode_caddress" aux t

(** [is_access t]: checks if trm [t] is a struct access or an array access *)
let is_access (t : trm) : bool =
  match t.desc with
  | Trm_apps (tprim, _, _, _) ->
    begin match trm_prim_inv tprim with
    | Some (_, Prim_unop (Unop_struct_access _) | (_, Prim_binop (Binop_array_access))) -> true
    | _ -> false
    end
  | _ -> false

(** [caddress_intro_aux t]: is the inverse of [decode_caddress], hence if applies the following decodings:
     - [get(t + offset(f))] becomes [get(t).f]
     - [t + offset(f)] becomes [t.f]
     - [get (t + i)] becomes [get(t)[i]]
     - [t + i] becomes [t[i]]

     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_prim (Prim_array_access, [t; i]))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_prim (Prim_struct_access "f"),[ŧ])] *)
let caddress_intro_aux (t : trm) : trm =
  let rec aux t = (* recursive calls for rvalues *)
    trm_simplify_addressof_and_get
      (if is_access t then
        (* [access(..)] becomes [& ...] *)
        trm_address_of (access t)
      else trm_map aux t)
  and access t =  (* recursive calls for lvalues *)
    trm_simplify_addressof_and_get (* Note: might not be needed *)
      (Pattern.pattern_match t [
        Pattern.(trm_struct_access !__ !__) (fun t1 f () ->
          (* struct_access (f, t1) is reverted to struct_get (f, access t1) *)
          let u1 = access t1 in
          trm_like ~old:t (trm_struct_get ~struct_typ:typ_auto u1 f)
        );
        Pattern.(trm_array_access !__ !__) (fun t1 t2 () ->
          (* array_access (t1, t2) is reverted to array_get (aux t1, aux t2) *)
          let u1 = aux t1 in
          let u2 = aux t2 in
          trm_like ~old:t (trm_array_get u1 u2)
        );
        Pattern.__ (fun () -> trm_get (aux t))
      ])
  in
  aux t

let encode_caddress =
  debug_current_stage "encode_caddress";
  caddress_intro_aux

(** [decode_expr_in_seq t]: updates [t] in such a way that all instructions appearing in sequences
   have type [unit] by inserting calls to [ignore] whenever necessary.
   This might not be the case when ignoring the result of an effectful expression.
   For example [x++;] is transformed into [ignore(x++)]. *)
let rec decode_expr_in_seq (t : trm) : trm =
  match trm_seq_inv t with
  | Some (ts, result) ->
    let ts = Mlist.map (fun u ->
        let u = decode_expr_in_seq u in
        match u.typ with
        | None -> u
        | Some typ when is_typ_unit typ -> u
        | Some typ when is_typ_auto typ -> failwith "%s: Cannot tell if there should be an ignore node or not on term %s" (loc_to_string t.loc) (Ast_to_c.ast_to_string u)
        | Some _ -> trm_ignore ~annot:u.annot (trm_alter ~annot:trm_annot_default u)
      ) ts
    in
    trm_like ~old:t (trm_seq ?result ts)
  | None ->
    trm_map decode_expr_in_seq t

(** [encode_expr_in_seq t]: remove calls to [ignore]
   For example [ignore(x++);] is transformed into [x++]. *)
let rec encode_expr_in_seq (t : trm) : trm =
  Pattern.pattern_match t [
    Pattern.(trm_apps1 (trm_specific_var var_ignore) !__) (fun expr () ->
      let annot = { expr.annot with
        trm_annot_stringrepr = Option.or_ t.annot.trm_annot_stringrepr expr.annot.trm_annot_stringrepr;
        trm_annot_marks = t.annot.trm_annot_marks @ expr.annot.trm_annot_marks;
        trm_annot_labels = t.annot.trm_annot_labels;
        trm_annot_pragma = t.annot.trm_annot_pragma @ expr.annot.trm_annot_pragma;
      } in
      encode_expr_in_seq (trm_alter ~annot expr)
    );
    Pattern.__ (fun () -> trm_map encode_expr_in_seq t)
  ]

let var_exact_div = toplevel_var "exact_div"

(** [decode_infix t]: encode unary and binary operators as Caml functions, for instance
    - [x++] becomes [++(&x)]
    - [x = y] becomes [=(&x, y)]
    - [x += y] becomes [+=(&x,y)]

  also, [exact_div(a, b)] becomes the primitive equivalent.
  *)
let decode_infix (t : trm) : trm =
  debug_current_stage "decode_infix";
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Convert [ x += y ]  into [ (+=)(&x, y) ]
         represented as [Trm_apps (Prim_compound_assign_op binop) [trm_addressof(x),y]],
       likewise for [ x -= y]*)
    | Trm_apps ({desc = Trm_prim (_, Prim_compound_assign_op binop)} as op, [tl; tr], _, _) ->
      trm_alter ~typ:typ_unit ~desc:(Trm_apps (op, [trm_address_of tl; tr], [], [])) t
    (* Convert [ x++ ] into [ (++)(&x) ], where [(++)] is like the [incr] function in OCaml *)
    | Trm_apps ({desc = Trm_prim (_, Prim_unop unop); _} as op, [base], _, _) when is_unary_compound_assign unop ->
      trm_replace (Trm_apps(op, [trm_address_of base], [], [])) t
    (* Convert [ x = y ] into [ (=)(&x, y) ] *)
    | Trm_apps ({desc = Trm_prim (_, Prim_binop Binop_set)} as op, [tl; tr], _, _) ->
      trm_replace (Trm_apps (op, [trm_address_of tl; tr], [], [])) t
    (* Recognize exact_div primitive *)
    | Trm_apps ({desc = Trm_var v}, [tl; tr], _, _) when var_eq v var_exact_div ->
      (* FIXME: this should probably not happen here *)
      trm_replace (Trm_apps (trm_prim (Option.value ~default:typ_int tl.typ) (Prim_binop Binop_exact_div), [tl; tr], [], [])) t
    | _ -> trm_map aux t
  in
  debug_before_after_trm "decode_infix" aux t

(** [encode_infix t]: decodes unary and binary oeprators back to C++ unary and binary operators
    [++(&x)] becomes [++x]
    [+=(&x, y)] becomes [x += y]
    [=(&x, y)] becomes [x = y]*)
let encode_infix (t : trm) : trm =
  debug_current_stage "encode_infix";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_prim (_, Prim_compound_assign_op binop)} as op, [tl; tr], _, _) ->
      trm_replace (Trm_apps(op, [trm_get tl; tr], [], [])) t
    | Trm_apps ({desc = Trm_prim (_, Prim_unop unop); _} as op, [base], _, _) when is_unary_compound_assign unop ->
      trm_replace (Trm_apps(op, [trm_get base], [], [])) t
    | Trm_apps ({desc = Trm_prim (_, Prim_binop Binop_set)} as op, [tl; tr], _, _) ->
      trm_replace (Trm_apps (op, [trm_get tl; tr], [], [])) t
    | _ -> trm_map aux t
  in aux t


(** [method_elim t]: encodes class method calls.  *)
let decode_method_call (t : trm) : trm =
  debug_current_stage "decode_method_call";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_apps ({desc = Trm_prim (class_typ, Prim_unop (Unop_struct_get f))}, [base], _, _)} as _tr, args, ghost_args, ghost_bind) ->
      (* LATER: Manage templated class *)
      let class_var =
        match typ_var_inv class_typ with
        | Some class_var -> class_var
        | None -> failwith "C_encoding.decode_method_call: unsupported base: %s\n" (Ast_to_text.ast_to_string base)
      in
      let class_var = remove_typ_namespace class_var in
      let namespaces = class_var.namespaces @ [class_var.name] in
      let t_var = trm_var (name_to_var ~namespaces f) in
      trm_add_cstyle Method_call (trm_apps ~ghost_args ~ghost_bind (t_var) ([trm_address_of base] @ args))
    | _ -> trm_map aux t
   in
   debug_before_after_trm "mcall" aux t

(** [encode_method_call t]: decodes class methods calls. *)
let encode_method_call (t : trm) : trm =
  debug_current_stage "encode_method_call";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, args, ghost_args, ghost_bind) when trm_has_cstyle Method_call t ->
      if List.length args < 1 then trm_fail t "C_encoding.encode_method_call: bad encodings.";
      let base, args = List.uncons args in
      let struct_access =
        begin match f.desc with
        | Trm_var f -> trm_struct_get ~struct_typ:typ_auto (trm_get base) f.name
        (* Special case when function_beta transformation is applied. *)
        | _ -> failwith "DEPRECATED?" (* f *)
        end in
      trm_alter ~desc:(Trm_apps (struct_access, args, ghost_args, ghost_bind)) t
    | _ -> trm_map aux t
     in aux t

(** [decode_class_member t]: encodes class members. *)
let decode_class_member (t : trm) : trm =
  debug_current_stage "decode_class_member";
  (* workaround to substitute untyped 'this' variables with typed 'this' variables required by 'decode_method_call' *)
  let to_subst = ref Var_map.empty in

  let get_class_typ (current_class : var option) (method_var : var) : typ =
    let current_class = match current_class with
    | Some current_class -> current_class
    | None -> failwith "unsupported member definition outside of class structure when serializing"
    in
    typ_ptr (typ_var current_class)
  in
  let rec aux (current_class : var option) (t : trm) : trm =
    Pattern.pattern_match t [
      Pattern.(trm_typedef !__) (fun td () ->
        let current_class = Some td.typedef_name in
        trm_map (aux current_class) t
      );
      Pattern.(trm_let_fun !__ !__ !__ !__ !__) (fun v ty vl body contract () ->
        if trm_has_cstyle Method t then begin
          let var_this = new_var "this" in
          let this_typ = get_class_typ current_class v in
          let typed_this = trm_var ~typ:this_typ var_this in
          to_subst := Var_map.add var_this typed_this !to_subst;
          trm_like ~old:t (trm_let_fun v ty ((var_this, this_typ) :: vl) body ~contract)
        end else if is_class_constructor t then begin
          let var_this = new_var "this" in
          let this_typ = get_class_typ current_class v in
          let this_body = trm_apps (trm_toplevel_var "malloc") [trm_sizeof this_typ] in
          let this_alloc = trm_let (var_this, this_typ) this_body in
          let typed_this = trm_var ~typ:this_typ var_this in
          to_subst := Var_map.add var_this typed_this !to_subst;
          begin match body.desc with
          | Trm_seq (tl, None) ->
            let new_tl = Mlist.map (trm_subst_var var_this typed_this) tl in
            let new_tl = Mlist.push_front this_alloc new_tl in
            let new_body = trm_alter ~desc:(Trm_seq (new_tl, Some var_this)) t in
            trm_like ~old:t (trm_let_fun v this_typ vl new_body ~contract)
          | _ -> trm_fail t "C_encoding.decode_class_member: ill defined class constructor."
          end
        end else raise Pattern.Next
      );
      Pattern.__ (fun () -> trm_map (aux current_class) t)
    ]
  in
  let on_subst old_t new_t =
    (* keep implicit this annotations etc *)
    trm_alter ~annot:old_t.annot ~loc:old_t.loc new_t
  in
  debug_before_after_trm "cmember" (fun t ->
    aux None t |> Scope_computation.infer_var_ids ~check_uniqueness:false |> trm_subst ~on_subst !to_subst
  ) t


(** [encode_class_member t]: decodes class members. *)
let encode_class_member (t : trm) : trm =
  debug_current_stage "encode_class_member";
  let rec aux (t : trm) : trm =
    match trm_let_fun_inv t with
    | Some (qv, ty, vl, body, contract) when trm_has_cstyle Method t ->
      let ((v_this, _), vl') = List.uncons vl in
      let rec fix_body t =
        match trm_var_inv t with
        | Some x when var_eq x v_this && (not (trm_has_cstyle Implicit_this t)) ->
          trm_like ~old:t (trm_this ())
        | _ -> trm_map fix_body t
      in
      let body' = fix_body body in
      trm_like ~old:t (trm_let_fun qv ty vl' body' ~contract)
    | Some (qv, ty, vl, body, contract) when is_class_constructor t ->
      begin match body.desc with
      | Trm_seq (tl, result) ->
        if Mlist.is_empty tl
          then t
          else
            (* FIXME: Here we should check that the first instruction is indeed the allocation *)
            let tl = Mlist.pop_front tl in
            let new_body = trm_alter ~desc:(Trm_seq (tl, None)) body in
            trm_like ~old:t (trm_let_fun qv typ_unit vl new_body ~contract)
      | _ -> trm_map aux t
      end
   | _ -> trm_map aux t
in aux t

(********************** Decode return ***************************)

let rec decode_return (t: trm): trm =
  let open Option.Monad in
  let rec replace_terminal_return (t: trm) : trm option =
    match t.desc with
    | Trm_abort (Ret expr) -> expr
    | Trm_seq (instrs, None) ->
      let* last_instr = Mlist.last instrs in
      let* terminal_expr = replace_terminal_return last_instr in
      let* typ = terminal_expr.typ in
      let other_instrs = Mlist.pop_back instrs in
      let is_bound_in_seq (v: var) =
        List.exists (fun instr -> match trm_let_inv instr with
          | Some (x, _, _) when var_eq x v -> true
          | _ -> false
        ) (Mlist.to_list other_instrs)
      in
      begin match trm_var_inv terminal_expr with
      | Some v when is_bound_in_seq v ->
        Some (trm_alter ~typ ~desc:(Trm_seq (other_instrs, Some v)) t)
      | _ ->
        let res_var = new_var "__res" in
        let new_last_instr = trm_like ~old:last_instr (trm_let (res_var, typ) terminal_expr) in
        Some (trm_alter ~typ ~desc:(Trm_seq (Mlist.push_back new_last_instr other_instrs, Some res_var)) t)
      end
    | Trm_if (tcond, tthen, telse) ->
      let* tthen = replace_terminal_return tthen in
      let* telse = replace_terminal_return telse in
      let* typ = tthen.typ in
      Some (trm_alter ~typ ~desc:(Trm_if (tcond, tthen, telse)) t)
    | _ -> None
  in
  let t = trm_map decode_return t in
  match t.desc with
  | Trm_fun (args, ret, body, contract) ->
    begin match replace_terminal_return body with
    | None -> t
    | Some body -> trm_like ~old:t (trm_fun args ret body ~contract)
    end
  (* Force the implicit return 0 at the end of the main function
     Maybe this is not a good idea since it breaks existing tests *)
  (*| Trm_let ((f, fty), main_body) when f.namespaces = [] && f.name = "main" ->
    Pattern.pattern_match main_body [
      Pattern.(trm_fun !__ !__ !(trm_seq !__ !__) !__) (fun args rettyp seq instrs result contract () ->
        match result with
        | Some _ -> t
        | None ->
          let res_var = new_var "__res" in
          let new_last_instr = trm_let (res_var, typ_int) (trm_int 0) in
          let seq = trm_alter ~typ:typ_int ~desc:(Trm_seq (Mlist.push_back new_last_instr instrs, Some res_var)) seq in
          let main_body = trm_alter ~desc:(Trm_fun (args, rettyp, seq, contract)) main_body in
          trm_alter ~desc:(Trm_let ((f, fty), main_body)) t
      );
      Pattern.__ (fun () ->
        Tools.warn "%s: main is not defined as a function" (loc_to_string t.loc);
        t
      )
    ]*)
  | _ -> t

let rec encode_return (t: trm): trm =
  let rec add_terminal_return t =
    match t.desc with
    | Trm_seq (instrs, Some v) ->
      begin match Mlist.last instrs with
      | Some last_instr when v.name = "__res" ->
        begin match trm_let_inv last_instr with
        | Some (x, _, expr) when var_eq x v ->
          let last_instr = match into_returning_instr expr with
          | Some t -> t
          | None -> trm_like ~old:last_instr (trm_ret (Some expr))
          in
          trm_like ~old:t (trm_seq (Mlist.push_back last_instr (Mlist.pop_back instrs)))
        | _ -> trm_like ~old:t (trm_seq (Mlist.push_back (trm_ret (Some (trm_var v))) instrs))
        end
      | _ -> trm_like ~old:t (trm_seq (Mlist.push_back (trm_ret (Some (trm_var v))) instrs))
      end
    | _ -> t
  and into_returning_instr t =
    match t.desc with
    | Trm_seq (_, Some _) -> Some (add_terminal_return t)
    | Trm_if (tcond, tthen, telse) when not (trm_has_cstyle Ternary_cond t || trm_has_cstyle Shortcircuit_and t || trm_has_cstyle Shortcircuit_or t) ->
      let process_branch t =
        match into_returning_instr t with
        | Some t -> t
        | None -> trm_ret (Some t)
      in
      let tthen = process_branch tthen in
      let telse = process_branch telse in
      Some (trm_like ~old:t (trm_if tcond tthen telse))
    | _ -> None
  in
  let t = trm_map encode_return t in
  match t.desc with
  | Trm_fun (args, ret, body, contract) ->
    trm_like ~old:t (trm_fun args ret (add_terminal_return body) ~contract)
  | _ -> t

(********************** Decode ghost arguments for applications ***************************)

open Resource_formula

let parse_ghost_args ghost_args_str =
  try
    Resource_cparser.ghost_arg_list Resource_clexer.lex_resources (Lexing.from_string ghost_args_str)
  with Resource_cparser.Error ->
    failwith "Failed to parse ghost arguments: %s" ghost_args_str

let parse_ghost_bind ghost_bind_str =
  try
    Resource_cparser.ghost_bind Resource_clexer.lex_resources (Lexing.from_string ghost_bind_str)
  with Resource_cparser.Error ->
    failwith "Failed to parse ghost bindings: %s" ghost_bind_str

let parse_ghost_args_and_bind arg_list =
  Pattern.pattern_match arg_list [
    Pattern.(trm_string !__ ^:: !__) (fun ghost_args_str arg_list () ->
      let ghost_args = parse_ghost_args ghost_args_str in
      let ghost_bind = Pattern.pattern_match arg_list [
        Pattern.(trm_string !__ ^:: nil) (fun ghost_bind_str () -> parse_ghost_bind ghost_bind_str);
        Pattern.nil (fun () -> []);
        Pattern.__ (fun () -> failwith "decode_ghost_annot: Invalid format for ghost arguments")
      ] in
      ghost_args, ghost_bind
    );
    Pattern.nil (fun () -> [], []);
    Pattern.__ (fun () -> failwith "decode_ghost_annot: Invalid format for ghost arguments")
  ]

let rec decode_ghost_annot (t: trm): trm =
  Pattern.pattern_match t [
    Pattern.(trm_apps (trm_var_with_name "__call_with") (trm_apps !__ !__ nil nil ^:: !__) __ __) (fun fn args ghost_args_opt () ->
        let fn = decode_ghost_annot fn in
        let args = List.map decode_ghost_annot args in
        let ghost_args, ghost_bind = parse_ghost_args_and_bind ghost_args_opt in
        trm_alter ~desc:(Trm_apps (fn, args, ghost_args, ghost_bind)) t
      );
    Pattern.(trm_let __ (trm_var_with_name "__ghost_unit") (trm_apps (trm_var_with_name "__ghost_call") (!__ ^:: !__) __ __)) (fun ghost_fn ghost_args_opt () ->
        let ghost_fn = decode_ghost_annot ghost_fn in
        let ghost_args, ghost_bind = parse_ghost_args_and_bind ghost_args_opt in
        trm_alter ~annot:{t.annot with trm_annot_attributes = [GhostInstr]} ~desc:(Trm_apps (ghost_fn, [], ghost_args, ghost_bind)) t
      );
    Pattern.(trm_apps (trm_var_with_name "__ghost_begin") (!__ ^:: !__) __ __) (fun ghost_fn ghost_args_opt () ->
        let ghost_fn = decode_ghost_annot ghost_fn in
        let ghost_args, ghost_bind = parse_ghost_args_and_bind ghost_args_opt in
        trm_apps (trm_var Resource_trm.var_ghost_begin) [
          trm_alter ~annot:{t.annot with trm_annot_attributes = [GhostInstr]} ~desc:(Trm_apps (ghost_fn, [], ghost_args, ghost_bind)) t
        ]
      );
    Pattern.(trm_apps (trm_var_with_name "__clear") (trm_string !__ ^:: __) __ __) (fun hyp_name () ->
        trm_alter ~loc:t.loc (Resource_trm.ghost_clear (name_to_var hyp_name))
      );
    Pattern.(trm_seq !__ !__) (fun seq result () -> trm_alter ~desc:(Trm_seq (Mlist.of_list (decode_ghost_annot_in_seq (Mlist.to_list seq)), result)) t);
    Pattern.__ (fun () -> trm_map decode_ghost_annot t)
  ]

and decode_ghost_annot_in_seq (ts: trm list): trm list =
  match ts with
  | [] -> []
  | t :: ts ->
    let rec grab_ghost_args ts =
      Pattern.pattern_match ts [
        Pattern.(trm_apps1 (trm_var_with_name "__with") (trm_string !__) ^:: !__)
          (fun ghost_args_str ts () ->
            let (args, bind, ts) = grab_ghost_args ts in
            (parse_ghost_args ghost_args_str @ args, bind, ts));
        Pattern.(trm_apps1 (trm_var_with_name "__bind") (trm_string !__) ^:: !__)
          (fun ghost_bind_str ts () ->
            let (args, bind, ts) = grab_ghost_args ts in
            (args, parse_ghost_bind ghost_bind_str @ bind, ts));
        Pattern.__ (fun () -> ([], [], ts))
      ]
    in

    let t = decode_ghost_annot t in
    let t, ts = Pattern.pattern_match t [
      Pattern.(trm_apps !__ !__ nil nil) (fun f args () ->
        let ghost_args, ghost_bind, ts = grab_ghost_args ts in
        (trm_alter ~desc:(Trm_apps (f, args, ghost_args, ghost_bind)) t, ts)
      );
      Pattern.(trm_let !__ !__ !(trm_apps !__ !__ nil nil)) (fun var typ body f args () ->
        let ghost_args, ghost_bind, ts = grab_ghost_args ts in
        (trm_alter ~desc:(Trm_let ((var, typ), trm_alter ~desc:(Trm_apps (f,args,ghost_args,ghost_bind)) body)) t, ts));
      Pattern.__ (fun () -> (t, ts))
    ]
    in
    t :: decode_ghost_annot_in_seq ts

let formula_to_string (style : style) (f: formula) : string =
  Ast_to_c.ast_to_string ~width:PPrint.infinity ~style:style.cstyle f

let var__with = trm_var (name_to_var "__with")
let var__bind = trm_var (name_to_var "__bind")
let var__call_with = trm_var (name_to_var "__call_with")
let var__ghost = trm_var (name_to_var "__ghost")

let encode_ghost_annot (style: style) (t: trm) : trm =
  let rec aux t =
    let ghost_args_to_trm_string ghost_args =
      trm_string (String.concat ", " (List.map (fun (ghost_var, ghost_formula) -> sprintf "%s := %s" (var_name ghost_var) (formula_to_string style ghost_formula)) ghost_args))
    in
    let ghost_bind_to_trm_string ghost_bind =
      let bound_var_name var = match var with
        | Some var -> var_name var
        | None -> "_"
      in
      trm_string (String.concat ", " (List.map (fun (bound_var, contract_var) -> sprintf "%s <- %s" (bound_var_name bound_var) (var_name contract_var)) ghost_bind))
    in
    let ghost_args_and_bind_to_opt_args ghost_args ghost_bind =
      if ghost_bind = [] then
        if ghost_args = [] then
          []
        else
          [ghost_args_to_trm_string ghost_args]
      else
        [ghost_args_to_trm_string ghost_args; ghost_bind_to_trm_string ghost_bind]
    in

    match t.desc with
    | Trm_apps (fn, args, (_ :: _ as ghost_args), ghost_bind) | Trm_apps (fn, args, ghost_args, (_ :: _ as ghost_bind)) ->
      (* Outside sequence add __call_with *)
      let t = trm_map aux t in
      trm_apps var__call_with (t :: ghost_args_and_bind_to_opt_args ghost_args ghost_bind)
    | Trm_seq (seq, result) ->
      (* Inside sequence add __with and __bind *)
      Nobrace.enter ();
      let seq = Mlist.map (fun t -> Pattern.pattern_match t [
        Pattern.(trm_apps !__ nil !__ !__) (fun fn ghost_args ghost_bind () ->
          if not (trm_has_attribute GhostInstr t) then raise_notrace Pattern.Next;
          let fn = aux fn in
          trm_like ~old:t (trm_apps var__ghost (fn :: ghost_args_and_bind_to_opt_args ghost_args ghost_bind))
        );
        Pattern.(trm_apps __ __ !__ !__) (fun ghost_args ghost_bind () ->
          Pattern.when_ (ghost_args <> [] || ghost_bind <> []);
          let t = trm_map aux t in
          Nobrace.trm_seq_nomarks (t :: (if ghost_args = [] then [] else [trm_apps var__with [ghost_args_to_trm_string ghost_args]]) @ (if ghost_bind = [] then [] else [trm_apps var__bind [ghost_bind_to_trm_string ghost_bind]]))
        );
        Pattern.(trm_let !__ !__ !(trm_apps __ __ !__ !__)) (fun var typ call ghost_args ghost_bind () ->
          Pattern.when_ (ghost_args <> [] || ghost_bind <> []);
          let call = trm_map aux call in
          Nobrace.trm_seq_nomarks ((trm_like ~old:t (trm_let (var, typ) call)) :: (if ghost_args = [] then [] else [trm_apps var__with [ghost_args_to_trm_string ghost_args]]) @ (if ghost_bind = [] then [] else [trm_apps var__bind [ghost_bind_to_trm_string ghost_bind]]))
        );
        Pattern.(trm_let !__ !__ (trm_apps1 (trm_specific_var Resource_trm.var_ghost_begin) !(trm_apps !__ nil !__ !__))) (fun ghost_pair typ ghost_call ghost_fn ghost_args ghost_bind () ->
          let ghost_fn = aux ghost_fn in
          trm_like ~old:(trm_error_merge ~from:ghost_call t) (trm_let (ghost_pair, typ) (trm_apps (trm_var Resource_trm.var_ghost_begin) (ghost_fn :: ghost_args_and_bind_to_opt_args ghost_args ghost_bind)))
        );
        Pattern.(trm_apps1 !(trm_specific_var Resource_trm.var_clear) (trm_var !__)) (fun f v () ->
          trm_like ~old:t (trm_apps f [trm_string (var_name v)])
        );
        Pattern.__ (fun () -> trm_map aux t)
      ]) seq in
      let nobrace_id = Nobrace.exit () in
      let seq = Nobrace.flatten_seq nobrace_id seq in
      trm_alter ~desc:(Trm_seq (seq, result)) t
    | _ -> trm_map aux t
  in
  aux t

let remove_ghost_annot (t : trm) : trm =
  Nobrace.remove_after_trm_op (Resource_trm.delete_annots_on ~delete_contracts:false ~delete_ghost:true) t

let encode_or_remove_ghost_annot (style: style) (t: trm) : trm =
  if style.typing.typing_ghost
    then encode_ghost_annot style t
    else remove_ghost_annot t

(********************** Decode contract annotations ***************************)

open Resource_contract

(* These pseudo-variables will never get an id since they disappear before *)
let __pure = name_to_var "__pure"
let __requires = name_to_var "__requires"
let __ensures = name_to_var "__ensures"
let __reads = name_to_var "__reads"
let __writes = name_to_var "__writes"
let __modifies = name_to_var "__modifies"
let __preserves = name_to_var "__preserves"
let __consumes = name_to_var "__consumes"
let __produces = name_to_var "__produces"

let __xrequires = name_to_var "__xrequires"
let __xensures = name_to_var "__xensures"
let __xreads = name_to_var "__xreads"
let __xwrites = name_to_var "__xwrites"
let __xmodifies = name_to_var "__xmodifies"
let __xpreserves = name_to_var "__xpreserves"
let __xconsumes = name_to_var "__xconsumes"
let __xproduces = name_to_var "__xproduces"
let __srequires = name_to_var "__srequires"
let __smodifies = name_to_var "__smodifies"
let __spreserves =  name_to_var "__spreserves"
let __sreads = name_to_var "__sreads"
let __strict = name_to_var "__strict"

let __reverts = name_to_var "__reverts"

let __ctx_res = name_to_var "__ctx_res"
let __produced_res = name_to_var "__produced_res"
let __used_res = name_to_var "__used_res"
let __framed_res = name_to_var "__framed_res"
let __joined_res = name_to_var "__joined_res"
let __contract_inst = name_to_var "__contract_inst"
let __post_inst = name_to_var "__post_inst"

let fun_clause_type_inv (clause: var) : fun_contract_clause_type option =
  match clause.name with
  | "__pure" -> Some Requires
  | "__requires" -> Some Requires
  | "__ensures" -> Some Ensures
  | "__reads" -> Some Reads
  | "__writes" -> Some Writes
  | "__modifies" -> Some Modifies
  | "__preserves" -> Some Preserves
  | "__consumes" -> Some Consumes
  | "__produces" -> Some Produces
  | "__xrequires" | "__xensures" | "__xreads" | "__xwrites" | "__xmodifies" | "__xpreserves"
  | "__xconsumes" | "__xproduces" | "__srequires" | "__sreads" | "__smodifies" | "__spreserves" | "__strict" ->
    failwith "Found the loop contract clause '%s' in a function contract" clause.name
  | _ -> None

let loop_clause_type_inv (clause: var) : loop_contract_clause_type option =
  match clause.name with
  | "__requires" -> Some LoopGhosts
  | "__xrequires" -> Some (Exclusive Requires)
  | "__xensures" -> Some (Exclusive Ensures)
  | "__xreads" -> Some (Exclusive Reads)
  | "__xwrites" -> Some (Exclusive Writes)
  | "__xmodifies" -> Some (Exclusive Modifies)
  | "__xpreserves" -> Some (Exclusive Preserves)
  | "__xconsumes" -> Some (Exclusive Consumes)
  | "__xproduces" -> Some (Exclusive Produces)
  | "__srequires" -> Some InvariantGhosts
  | "__sreads" -> Some SharedReads
  | "__smodifies" -> Some SharedModifies
  | "__spreserves" -> Some SharedPreserves
  | "__strict" -> Some Strict
  | "__pure" | "__ensures" | "__reads" | "__writes" | "__modifies" | "__preserves" | "__consumes" | "__produces" ->
    failwith "Found the function contract clause '%s' in a loop contract" clause.name
  | _ -> None

let encoded_clause_inv (clause_type_inv: var -> 'clause_type option) (t: trm): ('clause_type * string) option =
  let open Option.Monad in
  let* fn, args = trm_apps_inv t in
  let* fn_var = trm_var_inv fn in
  let* clause = clause_type_inv fn_var in
  let arg = Option.value ~default:(trm_string "") (List.nth_opt args 0) in
  let* arg = trm_lit_inv arg in
  let* arg =
    match arg with
    | Lit_string s -> Some s
    | _ -> None
  in
  Some (clause, arg)

let rec extract_encoded_contract_clauses (clause_type_inv: var -> 'clause_type option) (seq: trm mlist):
  ('clause_type * string) list * trm mlist =
  match Option.bind (Mlist.nth_opt seq 0) (encoded_clause_inv clause_type_inv) with
  | Some contract ->
    let cont, seq = extract_encoded_contract_clauses clause_type_inv (Mlist.pop_front seq) in
    contract::cont, seq
  | None -> [], seq

let extract_fun_contract (seq: trm mlist) : fun_contract option * trm mlist =
  let enc_contract, seq = extract_encoded_contract_clauses fun_clause_type_inv seq in
  match enc_contract with
  | [] -> None, seq
  | _ -> Some (parse_contract_clauses empty_fun_contract push_fun_contract_clause enc_contract), seq

let encoded_reverts_inv (t: trm): var option =
  Pattern.pattern_match t [
    Pattern.(trm_apps1 (trm_var (check (fun v -> v.name = "__reverts"))) (trm_var !__)) (fun revert_fn () ->
      Some revert_fn
    );
    Pattern.__ (fun () -> None)
  ]

let extract_fun_spec (seq: trm mlist) : fun_spec * trm mlist =
  match Option.bind (Mlist.nth_opt seq 0) encoded_reverts_inv with
  | Some reverts_fn -> FunSpecReverts reverts_fn, Mlist.pop_front seq
  | None ->
    let contract_opt, seq = extract_fun_contract seq in
    match contract_opt with
    | None -> FunSpecUnknown, seq
    | Some c -> FunSpecContract c, seq


let extract_loop_contract (seq: trm mlist) : loop_contract * trm mlist =
  let enc_contract, seq = extract_encoded_contract_clauses loop_clause_type_inv seq in
  let is_strict = ref false in
  let enc_contract = List.filter (fun (clause_type, _) ->
    if clause_type = Strict then
      (is_strict := true; false)
    else true) enc_contract in
  let start_contract = if !is_strict then empty_strict_loop_contract else empty_loop_contract in
  parse_contract_clauses start_contract push_loop_contract_clause enc_contract, seq


let decode_contract (t: trm): trm =
  debug_current_stage "decode_contract";
  let rec aux t =
  match t.desc with
  | Trm_fun (args, ty, body, spec) ->
    assert (spec = FunSpecUnknown);
    begin match trm_seq_inv body with
    | Some (body_seq, result) ->
      let spec, new_body = extract_fun_spec body_seq in
      let new_body = Mlist.map aux new_body in
      trm_alter ~desc:(Trm_fun (args, ty, trm_seq ?result new_body, spec)) t
    | None -> trm_map aux t
    end

  | Trm_for (range, mode, body, contract) ->
    assert (contract = empty_loop_contract);
    begin match trm_seq_inv body with
    | Some (body_seq, result) ->
      let contract, new_body = extract_loop_contract body_seq in
      let new_body = Mlist.map aux new_body in
      trm_alter ~desc:(Trm_for (range, mode, trm_seq ?result new_body, contract)) t
    | None -> trm_map aux t
    end

  | _ -> trm_map aux t
  in aux t

let named_formula_to_string (style: style) ?(used_vars = Var_set.empty) ?(aliases = Var_map.empty) (hyp, formula): string =
  let sformula = formula_to_string style formula in
  let shyp =
    if style.cstyle.print_var_id && not (is_anon_var hyp) then
      sprintf "%s/*#%d*/" hyp.name hyp.id
    else var_name hyp
  in
  match Var_map.find_opt hyp aliases with
  | Some alias_val ->
    let salias = formula_to_string style alias_val in
    Printf.sprintf "%s := %s : %s" shyp salias sformula
  | None ->
    if not (style.typing.print_generated_res_ids || Var_set.mem hyp used_vars) && String.starts_with ~prefix:"#" shyp
      then Printf.sprintf "%s" sformula
      else Printf.sprintf "%s: %s" shyp sformula

(** [seq_push code t]: inserts trm [code] at the begining of sequence [t],
    [code] - instruction to be added,
    [t] - ast of the outer sequence where the insertion will be performed. *)
let seq_push (code : trm) (t : trm) : trm =
  let error = "seq_push: expected a sequence where insertion is performed." in
  let tl, result = trm_inv ~error trm_seq_inv t in
  let new_tl = Mlist.push_front code tl in
  trm_replace (Trm_seq (new_tl, result)) t

let trm_array_of_string list =
  trm_array ~elem_typ:typ_string (List.map trm_string list)

let filter_pure_resources (pure_res: resource_item list): resource_item list =
  List.filter (fun (v, t) ->
    Pattern.pattern_match t [
      Pattern.(typ_fun __ __) (fun () -> false);
      Pattern.(typ_pure_fun __ __) (fun () -> false);
      Pattern.(typ_var (var_eq typ_type_var)) (fun () -> false);
      Pattern.__ (fun () -> not (String.starts_with ~prefix:"__" v.name))
    ]) pure_res

let ctx_resources_to_trm (style: style) (res: resource_set) : trm =
  let used_vars = Resource_set.used_vars res in
  let spure = trm_array_of_string (List.map (named_formula_to_string style ~used_vars ~aliases:res.aliases) (filter_pure_resources res.pure)) in
  let slin = trm_array_of_string (List.map (named_formula_to_string style) res.linear) in
  trm_apps (trm_var __ctx_res) [spure; slin]

let ctx_used_res_item_to_string (style: style) (res: used_resource_item) : string =
  let sinst = formula_to_string style res.inst_by in
  let sformula = formula_to_string style res.used_formula in
  Printf.sprintf "%s := %s : %s" (var_name res.hyp) sinst sformula

let ctx_used_res_to_trm (style: style) ~(clause: var) (used_res: used_resource_set) : trm =
  let spure = trm_array_of_string (List.map (ctx_used_res_item_to_string style) used_res.used_pure) in
  let slin = trm_array_of_string (List.map (ctx_used_res_item_to_string style) used_res.used_linear) in
  trm_apps (trm_var clause) [spure; slin]

let ctx_produced_res_item_to_string (style: style) ?(aliases = Var_map.empty) (contract_hyp_names: var Var_map.t) ((hyp, formula): resource_item) : string =
  let sformula = formula_to_string style formula in
  let salias = Option.map_or (fun alias ->
    sprintf " := %s" (formula_to_string style alias)
    ) "" (Var_map.find_opt hyp aliases) in
  Printf.sprintf "%s <- %s%s : %s" (var_name hyp) (Option.map_or var_name "?" (Var_map.find_opt hyp contract_hyp_names)) salias sformula

let ctx_produced_res_to_trm (style: style) (produced_res: produced_resource_set) : trm =
  let spure = trm_array_of_string (List.map (ctx_produced_res_item_to_string style produced_res.contract_hyp_names ~aliases:produced_res.produced_res.aliases) produced_res.produced_res.pure) in
  let slin = trm_array_of_string (List.map (ctx_produced_res_item_to_string style produced_res.contract_hyp_names) produced_res.produced_res.linear) in
  trm_apps (trm_var __produced_res) [spure; slin]

let ctx_usage_map_to_strings res_used =
  List.map (fun (hyp, usage) ->
    let name = var_name hyp in
    match usage with
    | Required -> sprintf "%s" name
    | Ensured -> sprintf "Ensured %s" name
    | ArbitrarilyChosen -> sprintf "Arbitrary %s" name
    | Cleared -> sprintf "Cleared %s" name
    | ConsumedFull -> sprintf "Full %s" name
    | ConsumedUninit -> sprintf "Uninit %s" name
    | SplittedFrac -> sprintf "Subfrac %s" name
    | JoinedFrac -> sprintf "JoinFrac %s" name
    | Produced -> sprintf "Produced %s" name)
    (Var_map.bindings res_used)

let debug_ctx_before = false

let display_ctx_resources (style: style) (t: trm): trm list =
  let t =
    match trm_let_inv t with
    | Some (_, _, body) -> { t with ctx = { t.ctx with ctx_resources_contract_invoc = body.ctx.ctx_resources_contract_invoc } }
    | None ->
      match trm_ignore_inv t with
      | Some ignored -> { t with ctx = { t.ctx with ctx_resources_contract_invoc = ignored.ctx.ctx_resources_contract_invoc } }
      | None -> t
  in
  let tl_used =
    if style.typing.typing_used_res
      then Option.to_list (Option.map (fun res_used ->
        let s_used = ctx_usage_map_to_strings res_used in
        trm_apps (trm_var __used_res) (List.map trm_string s_used)) t.ctx.ctx_resources_usage)
      else []
    in
  let tl = match t.ctx.ctx_resources_contract_invoc with
    | None -> [t]
    | Some contract_invoc ->
      (* TODO: use a combinator to factorize the pattern "if then else []" *)
      let tl_frame =
        if style.typing.typing_framed_res
          then [ trm_apps (trm_var __framed_res) (List.map trm_string (List.map (named_formula_to_string style) contract_invoc.contract_frame)) ]
          else []
        in
      let tl_inst =
        if style.typing.typing_contract_inst
          then [ctx_used_res_to_trm style ~clause:__contract_inst contract_invoc.contract_inst]
          else [] in
      let tl_produced =
        if style.typing.typing_produced_res
          then [ctx_produced_res_to_trm style contract_invoc.contract_produced ]
          else [] in
      let tl_joined =
        if style.typing.typing_joined_res
          then [trm_apps (trm_var __joined_res) (List.map trm_string
                  (List.map (fun (x, y) -> sprintf "%s <-- %s" (var_name x) (var_name y))
                   contract_invoc.contract_joined_resources)) ]
          else [] in
      tl_frame @ tl_inst @ [ t ] @ tl_produced @ tl_joined
  in
  let tl_before =
    if debug_ctx_before (* TODO : assert == *)
      then Option.to_list (Option.map (ctx_resources_to_trm style) t.ctx.ctx_resources_before)
      else []
    in
  let tl_after =
    if style.typing.typing_ctx_res
      then Option.to_list (Option.map (ctx_resources_to_trm style) t.ctx.ctx_resources_after)
      else []
    in
  (tl_before @ tl_used @ tl @ tl_after)

let computed_resources_intro (style: style) (t: trm): trm =
  let rec aux t =
    match t.desc with
    | Trm_seq (instrs, result) when not (trm_is_mainfile t) ->
      let tl_before =
        if style.typing.typing_ctx_res
          then Option.to_list (Option.map (ctx_resources_to_trm style) t.ctx.ctx_resources_before)
          else []
        in
      let tl_post_inst =
        if style.typing.typing_used_res
          then Option.to_list (Option.map (ctx_used_res_to_trm style ~clause:__post_inst) t.ctx.ctx_resources_post_inst)
          else []
        in
      let process_instr instr = Mlist.of_list (display_ctx_resources style (aux instr)) in
      let tl_instrs = Mlist.concat_map process_instr instrs in
      trm_like ~old:t (trm_seq ?result (Mlist.merge_list [Mlist.of_list tl_before; tl_instrs; Mlist.of_list tl_post_inst]))
    | _ -> trm_map ~keep_ctx:true aux t
  in
  aux t

let rec encode_contract (style: style) (t: trm): trm =
  if not style.typing.typing_contracts || trm_has_cstyle BodyHiddenForLightDiff t then t else

  (* debug_current_stage "encode_contract"; *)
  let push_named_formulas (contract_prim: var) ?(used_vars: Var_set.t option) (named_formulas: resource_item list) (t: trm): trm =
    List.fold_right (fun named_formula t ->
      let sres = named_formula_to_string style ?used_vars named_formula in
      let tres = trm_apps (trm_var contract_prim) [trm_string sres] in
      seq_push tres t) named_formulas t
  in

  let push_common_clauses ?(reads_clause: var option) ~(preserves_clause: var) ?(writes_clause: var option) (pure_with_fracs: resource_item list) (pre_linear: resource_item list) (post_linear: resource_item list) (t: trm): resource_item list * resource_item list * resource_item list * trm =
    let common_linear, pre_linear, post_linear = Resource_formula.filter_common_resources pre_linear post_linear in

    (* FIXME: This turns two reads formulas with a shared id into reads formulas with distinct id.
       Maybe this is not a problem in practice. *)
    let frac_to_remove = Hashtbl.create (List.length common_linear) in
    let reads_res, preserves_res =
      if reads_clause = None then
        [], common_linear
      else
        List.partition_map (fun (h, formula) ->
          match formula_read_only_inv formula with
          | Some { frac; formula = ro_formula } ->
            begin match trm_var_inv frac with
            | Some frac_atom ->
              Hashtbl.add frac_to_remove frac_atom ();
              Left (h, ro_formula)
            | None -> Right (h, formula)
            end
          | None -> Right (h, formula)) common_linear
    in

    let hyp_not_mem_before_pop (hyp, _) =
      if Hashtbl.mem frac_to_remove hyp then (
        Hashtbl.remove frac_to_remove hyp;
        false
      ) else true
    in
    let pre_pure = List.filter hyp_not_mem_before_pop pure_with_fracs in
    if (Hashtbl.length frac_to_remove != 0) then
      Tools.warn "Some fractions should have been discarded but they were not found in context: %s" (String.concat ", " (Hashtbl.fold (fun frac () acc -> (var_name frac) :: acc) frac_to_remove []));

    let t = match reads_clause with
      | Some reads_clause -> push_named_formulas reads_clause reads_res t
      | None -> t
    in
    let t, pre_linear, post_linear = match writes_clause with
      | Some writes_prim ->
        let writes_res, pre_linear, post_linear = Resource_formula.filter_common_resources ~compare:(fun tpre tpost -> if try Trm_unify.are_same_trm tpre (raw_formula_uninit tpost) with CannotTransformIntoUninit _ -> false then Some tpost else None) pre_linear post_linear in
        push_named_formulas writes_prim writes_res t, pre_linear, post_linear
      | _ -> t, pre_linear, post_linear
    in
    let t = push_named_formulas preserves_clause preserves_res t in
    (pre_pure, pre_linear, post_linear, t)
  in
  let push_common_clauses ?(force=false) = if style.cstyle.print_contract_internal_repr && not force then
    fun ?reads_clause ~preserves_clause ?writes_clause pure_with_fracs pre_linear post_linear t -> (pure_with_fracs, pre_linear, post_linear, t)
    else push_common_clauses
  in

  (* TODO: Inline into Trm_fun branch when Trm_let_fun disappears *)
  let add_contract_to_fun_body body contract =
    let body = encode_contract style body in
    match contract with
    | FunSpecContract contract when contract = empty_fun_contract ->
      seq_push (trm_apps (trm_var __pure) []) body
    | FunSpecContract contract ->
      let used_vars = fun_contract_used_vars contract in
      let preserves_clause = if !Flags.use_resources_with_models then __preserves else __modifies in
      let pre_pure, pre_linear, post_linear, body =
        push_common_clauses ~reads_clause:__reads ~preserves_clause ~writes_clause:__writes contract.pre.pure contract.pre.linear contract.post.linear body
      in
      let body = push_named_formulas __produces post_linear body in
      let body = push_named_formulas __ensures ~used_vars contract.post.pure body in
      let body = push_named_formulas __consumes pre_linear body in
      let body = push_named_formulas __requires ~used_vars pre_pure body in
      body
    | FunSpecReverts reverted_fn ->
      seq_push (trm_apps (trm_var __reverts) [trm_var reverted_fn]) body
    | FunSpecUnknown -> body
  in

  match t.desc with
  | Trm_fun (args, ty, body0, contract) ->
    let body = add_contract_to_fun_body body0 contract in
    if body == body0
      then t
      else trm_like ~old:t (trm_fun args ty body)

  | Trm_for (range, mode, body0, contract) ->
    let body = encode_contract style body0 in
    let used_vars = loop_contract_used_vars contract in
    let preserves_clause = if !Flags.use_resources_with_models then __xpreserves else __xmodifies in
    let loop_ghosts, pre_linear, post_linear, body =
      push_common_clauses ~reads_clause:__xreads ~preserves_clause ~writes_clause:__xwrites
        contract.loop_ghosts contract.iter_contract.pre.linear contract.iter_contract.post.linear body
    in
    let body = push_named_formulas __xproduces post_linear body in
    let body = push_named_formulas __xensures ~used_vars contract.iter_contract.post.pure body in
    let body = push_named_formulas __xconsumes pre_linear body in
    let body = push_named_formulas __xrequires ~used_vars contract.iter_contract.pre.pure body in
    List.iter (fun (_, formula) -> if formula_read_only_inv formula = None then failwith "parallel_reads contains non RO resources") contract.parallel_reads;
    let loop_ghosts, _, _, body =
      push_common_clauses ~force:true ~reads_clause:__sreads ~preserves_clause:__sreads
        loop_ghosts contract.parallel_reads contract.parallel_reads body
    in
    let spreserves_clause = if !Flags.use_resources_with_models then __spreserves else __smodifies in
    let body = push_named_formulas spreserves_clause ~used_vars contract.invariant.linear body in
    let body = push_named_formulas __srequires ~used_vars contract.invariant.pure body in
    let body = push_named_formulas __requires ~used_vars loop_ghosts body in
    let body = if contract.strict
      then seq_push (trm_apps (trm_var __strict) []) body
      else body in
    if body == body0
      then t
      else trm_like ~old:t (trm_for ~mode range body)

  | Trm_seq (instrs, result) ->
    trm_like ~old:t (trm_seq ?result (Mlist.map (encode_contract style) instrs))

  | _ -> trm_map (encode_contract style) t


(*************************************** Formula syntactic sugar *********************************************)

let rec decode_formula_sugar (t: trm): trm =
  if trm_has_cstyle ResourceFormula t then
    desugar_formula t
  else
    trm_map decode_formula_sugar t

let rec encode_formula_sugar (t: trm): trm =
  if trm_has_cstyle ResourceFormula t then
    encode_formula t
  else
    trm_map encode_formula_sugar t

(*************************************** C allocation functions *********************************************)

let typ_from_size (size: trm): typ =
  Pattern.pattern_match size [
    Pattern.(trm_sizeof !__) (fun typ () -> typ);
    Pattern.(trm_mul !__ (trm_sizeof !__)) (fun nb_elems typ () -> typ_array typ ~size:nb_elems)
  ]

let var_malloc = toplevel_var "malloc"
let var_calloc = toplevel_var "calloc"
let var_free = toplevel_var "free"

let rec decode_alloc (t: trm): trm =
  let annot = t.annot in
  let loc = t.loc in
  Pattern.pattern_match t [
    Pattern.(trm_cast !__ !__) (fun typto t () ->
      let t_in = decode_alloc t in
      match t_in.typ with
      | Some ty when Trm_unify.are_same_trm ty typto -> t_in
      | _ -> trm_cast ~annot ?loc typto t_in
    );
    Pattern.(trm_apps1 (trm_specific_var var_malloc) !__) (fun alloc_size () ->
      trm_new_uninit ~annot ?loc (typ_from_size alloc_size)
    );
    Pattern.(trm_apps2 (trm_specific_var var_calloc) !__ (trm_sizeof !__)) (fun nb_elems elem_typ () ->
      let typ = typ_array elem_typ ~size:nb_elems in
      trm_new ~annot ?loc typ (trm_null typ)
    );
    Pattern.(trm_apps1 (trm_specific_var var_free) !__) (fun t () ->
      trm_delete ~annot ?loc t
    );
    Pattern.__ (fun () -> trm_map decode_alloc t)
  ]

let encode_alloc (style: style) (t: trm): trm =
  let rec aux t =
    Pattern.pattern_match t [
      Pattern.(trm_new_uninit !__) (fun ty () ->
        match typ_array_inv ty with
        | Some (elemty, Some size) -> trm_like ~old:t (trm_cast (typ_ptr elemty) (trm_apps (trm_var var_malloc) [trm_mul_int size (trm_sizeof elemty)]))
        | _ -> trm_like ~old:t (trm_cast (typ_ptr ty) (trm_apps (trm_var var_malloc) [trm_sizeof ty]))
      );
      Pattern.(trm_new (typ_array !__ (some !__)) (trm_null __)) (fun basety nbelems () ->
        trm_like ~old:t (trm_cast (typ_ptr basety) (trm_apps (trm_var var_calloc) [nbelems; trm_sizeof basety]))
      );
      Pattern.(trm_delete !__) (fun tptr () ->
        trm_like ~old:t (trm_apps (trm_var var_free) [tptr])
      );
      Pattern.__ (fun () -> trm_map aux t)
    ]
  in
  if style.cstyle.c_alloc then aux t else t

(****************************** Alpha renaming of autogen variables ***************************************)

let autogen_alpha_rename style (t : trm) : trm =
  let map_binder (highest_h, renaming) var predecl =
    let return_next_name () =
      let new_v = { var with name = ("#_" ^ string_of_int (highest_h + 1)) } in
      (highest_h + 1, Var_map.add var new_v renaming), new_v
    in

    if is_anon_var var then
      return_next_name ()
    else if String.starts_with ~prefix:"#_" var.name then begin
        let var_hnum = String.sub var.name 2 (String.length var.name - 2) in
        match int_of_string_opt var_hnum with
        | None -> (highest_h, renaming), var
        | Some n when n > highest_h -> (n, renaming), var
        | Some n -> return_next_name ()
      end
    else
      (highest_h, renaming), var
  in

  let map_var (highest_h, renamings) v =
    match Var_map.find_opt v renamings with
    | Some v' -> v'
    | None -> v
    | exception UnsetVarId _ -> { v with name = v.name ^ "?" }
  in

  if style.typing.print_generated_res_ids
    then t (* When we want to print generated ids we prefer to keep them in sync with internal names *)
    else trm_rename_vars ~map_binder map_var (0, Var_map.empty) t

(* Temporary method to add a polymorphic function interface to the C frontend.
 We take a template term and replace it with the inner let, but with a modified function.
 The contract of the new function has all the arguments pushed inside.
 Normally, resource_computation takes care of this, but it would not know about the
 type variable, so we do it manually. We also change the type of the let to typ_auto
 because for these functions to work currently they need to be typed auto in the pure context.

 Note that we do this instead of simply adding a typechecking rule for trm_template because
 there is not a simple interface to add "T:Type" as an _argument_ to the inside function's
 contract instead of simply making T:Type available in the context for the recursive call. *)
let rec decode_template_decl (t : trm) : trm =
  Pattern.pattern_match t [
    Pattern.(trm_template !__ (trm_let !__ __ (trm_fun !__ !__ !__ !__)))
      (fun template_params name args rettyp body spec () ->
        let spec = match spec with
        | FunSpecContract contract ->
          let pre = List.fold_left (fun pre (arg_var, arg_typ) -> Resource_set.push_front_pure (arg_var, arg_typ) pre) contract.pre args in
          let pre' = List.fold_left (fun pre (param_var, param_kind) ->
            match param_kind with
            | Typename _ -> Resource_set.push_front_pure (param_var, typ_type) pre
            | _ -> pre) pre template_params in
          FunSpecContract { contract with pre = pre'}
        | _ -> failwith "Expected contract in template definition" in
        trm_let (name, typ_auto) (trm_fun args rettyp body ~contract:spec));
    Pattern.__ (fun () -> trm_map decode_template_decl t)
  ]

(****************************** GPU syntax sugar ***************************************)

let rec decode_gpu_sugar (t: trm) : trm =
  match (trm_seq_inv t) with
  | Some (instrs,res) ->
    let instrs = (Mlist.fold_left (fun acc instr ->
      let instr = decode_gpu_sugar instr in
      match acc with
      | [] -> instr :: acc
      | hd::tl ->
        match (hd.desc,instr.desc) with
        | (Trm_var v,Trm_for (range,mode,body,contract)) when (var_has_name "__threadfor" v) ->
          (trm_like ~old:instr (trm_for ~contract ~mode:GpuThread range body)) :: tl
        | _ -> instr :: acc
    ) [] instrs) |> List.rev |> Mlist.of_list in
    trm_alter ~desc:(Trm_seq (instrs,res)) t
  | _ -> trm_map decode_gpu_sugar t

(*************************************** Main entry points *********************************************)

(** [decode_from_c t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let decode_from_c: trm -> trm =
  debug_before_after_trm "decode_from_c" (fun t -> t |>
  decode_gpu_sugar |>
  decode_contract |>
  decode_ghost_annot |>
  decode_class_member |>
  decode_method_call |>
  Scope_computation.infer_var_ids ~check_uniqueness:false |>
  decode_infix |>
  decode_stackvar |>
  remove_const |>
  decode_caddress |>
  decode_return |>
  decode_expr_in_seq |>
  decode_formula_sugar |>
  decode_alloc |>
  decode_template_decl |>
  Scope_computation.infer_var_ids)

(** [encode_to_c t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let encode_to_c (style : style) : trm -> trm =
  debug_before_after_trm "encode_to_c" (fun t ->
  t |>
  Scope_computation.infer_var_ids |>
  encode_alloc style |>
  encode_formula_sugar |>
  encode_expr_in_seq |>
  encode_return |>
  encode_caddress |>
  encode_stackvar |>
  encode_infix |>
  encode_method_call |>
  encode_class_member |>
  autogen_alpha_rename style |>
  encode_or_remove_ghost_annot style |>
  encode_contract style
  )

(** [encode_meta t] adds into [t] all the "for-typing" operations
    and the contracts as C calls using the "__" prefix *)
let encode_meta ?(skip_var_ids = false) (style: style) : trm -> trm =
  fun t ->
  (if skip_var_ids then t else Scope_computation.infer_var_ids ~failure_allowed:false t) |>
  autogen_alpha_rename style |>
  encode_or_remove_ghost_annot style |>
  encode_contract style



(* Note: recall that currently const references are not supported
   Argument of why const ref is not so useful
before:
  const vect v = {0,1}
  f(v.x, v.x, v.x)

after:
  const vect v = {0,1}
  const int& a = v.x;
  f(a,a,a)

but it is equivalent to:
  const vect v = {0,1}
  const int a = v.x;
  f(a,a,a)

*)

(*

source:
T x(args)

before encoding
T x = (new(T,args)@ annot_constructor_arg)

after encoding
T* x = (T(args)@annot_new)


source
constructor T(a, b) : field1(a) { field2 = b;}


before encoding, with treating "this" as const variable

before encoding
void T(a, b) { @annot_constructor

  this->field1 = a;  @annot_member_initializer
  (this@annot_implicit_this) ->field2 = b;

}



after encoding
T* T(a, b) {


  T* this = alloc(size_of(T)); @annot_dont_encode
  this->field1 = a;  @annot_member_initializer
  (this@annot_implicit_this) ->field2 = b;

  return this; } @annot_constructor


*)
