open Ast
open Trm
open Typ

let debug = false

let debug_before_after_trm (msg : string) (f : trm -> trm) : trm -> trm =
  if debug then (fun t ->
    Xfile.put_contents (sprintf "/tmp/%s_before.txt" msg) (Ast_to_text.ast_to_string t);
    let t2 = f t in
    Xfile.put_contents (sprintf "/tmp/%s_after.txt" msg) (Ast_to_text.ast_to_string t2);
    t2
  ) else f

let debug_current_stage (msg: string) : unit =
  if debug then printf "%s\n" msg

(*

  t[i]  =  get(array_access(t,i))  = array_read(t,i)
       // array_read_inv also
       // array_access(t,i) corresponds to the C code t+i, which is the same as (( void* )t)+i*(sizeof(typeof(t[0]))

  t->f  =  get(struct_access(t,f)) = struct_read(t,f)
       // struct_access(t,f)  corresponds to the C code for   t + offset(f)

  ( *t ).f  =  t->f

  t.f  where t is not a get(.)    -> struct_get(t,"f")

   Recall: struct_get(t,f)  means  trm_apps (Prim_unop (unop_struct_get "f")) [t] *)

(* [env]: a map for storing all the variables as keys, and their mutability as values *)
type env = varkind Var_map.t

(* [env_empty]: empty environment *)
let env_empty =
  Var_map.empty

(* [get_varkind env x]: gets the mutability of variable [x]
   Note: Functions that come from an external library are set to immutable by default *)
let get_varkind (env : env) (x : var) : varkind =
  match Var_map.find_opt x env with
  | Some m -> m
  | _ -> Var_immutable

(* [is_var_mutable env x]: checks if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_varkind env x = Var_mutable

(* [env_extend env e varkind]: adds variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  Var_map.add e varkind env

(* [add_var env x xm]: adds variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit =
  env := env_extend !env x xm

let name_to_var ?(qualifier = []) (n : string) : var =
  { qualifier; name = n; id = -1 }

(* [trm_address_of t]: adds the "&" operator before [t]
    Note: if for example t = *a then [trm_address_of t] = &( *a) = a *)
let trm_address_of (t : trm) : trm =
  let u = trm_apps ?typ:t.typ (trm_unop Unop_address) [t] in
  trm_simplify_addressof_and_get u

(* [trm_get t]: adds the "*" operator before [t]
    Note: if for example t = &a then [trm_get t] = *( &a) = a *)
let trm_get (t : trm) : trm =
  let u = trm_apps ?typ:t.typ (trm_unop Unop_get) [t] in
  trm_simplify_addressof_and_get u

(* [onscope env t f]: applies function [f] on [t] without loosing [env]
   Note: This function is used for keeping track of opened scopes *)
let onscope (env : env ref) (t : trm) (f : trm -> trm) : trm =
    let saved_env = !env in
    let res = f t in
    env := saved_env;
    res

(* [create_env]: creates an empty environment *)
let create_env () = ref env_empty


(* [stackvar_elim t]: applies the following encodings
    - [int a = 5] with [<annotation:stackvar> int* a = new int(5)]
    - a variable occurrence [a] becomes [ * a]
    - [const int c = 5] remains unchange
    - simplify patterns of the form [&*p] and [*&p] into [p].
    - [int& b = a] becomes [<annotation:reference> int* b = &*a] which simplifies to [<annot..>int* b = a]
    - [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [int* const].
    - More complicated example: [int a = 5; int* b = &a] becomes [int* a = new int(5); int** b = new int*(&a);]
    TODO: specify and improve support for arrays

   Note: "reference" annotation is added to allow decoding *)
let stackvar_elim (t : trm) : trm =
  debug_current_stage "stackvar_elim";
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      if is_var_mutable !env x
        then trm_get (trm_replace (Trm_var (Var_mutable, x)) t)
        else trm_replace (Trm_var (Var_immutable, x)) t
    | Trm_let (_, (x, ty), tbody, bound_res) ->
      (* is the type of x (or elements of x in case it is a fixed-size array) a const type? *)
      let xm = if is_typ_const (get_inner_array_type ty) then Var_immutable else Var_mutable in
      add_var env x xm; (* Note: the onscope function will take care to remove this *)
      (* is the type of x a reference type? *)
      begin match typ_ref_inv ty with
      | Some ty1 ->
        begin match xm with
        | Var_immutable -> fail t.loc "Ast_fromto_AstC.tackvar_elim: unsupported references on const variables"
        | _ ->
          (* generate a pointer type, with suitable annotations *)
          trm_add_cstyle Reference (trm_replace (Trm_let (xm, (x, typ_ptr_generated ty1), trm_address_of (aux tbody), bound_res)) t)
        end
      | None ->
        begin match xm with
        | Var_mutable ->
          (* TODO: document the case that corresponds to Constructed_init *)
          let new_body = if trm_has_cstyle Constructed_init tbody then aux tbody else trm_new ty (aux tbody) in
          trm_add_cstyle Stackvar (trm_replace (Trm_let (xm, (x, typ_ptr_generated ty), new_body, bound_res)) t)
        | Var_immutable ->
          trm_map aux t
        end
      end
    | Trm_let_mult (_, tvl, tl) ->
      List.iter2 (fun (x, ty) tbody ->
        let xm = if is_typ_const (get_inner_array_type ty) then Var_immutable else Var_mutable in
        add_var env x xm
      ) tvl tl;
      trm_map aux t
    | Trm_seq _ when not (trm_is_nobrace_seq t) -> onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody, _) ->
      (* function names are by default immutable *)
      add_var env f Var_immutable;
      onscope env t (fun t -> List.iter (fun (x, _tx) ->
       let mut = Var_immutable in (* if is_typ_ptr tx then Var_mutable else Var_immutable in *)
       add_var env x mut) targs; trm_map aux t)
    | Trm_for (l_range, _, _) ->
        let (index, _, _, _, _, _) = l_range in
        onscope env t (fun t -> add_var env index Var_immutable; trm_map aux t)
    | Trm_for_c _ ->
        onscope env t (fun t -> trm_map aux t)
    | _ -> trm_map aux t
   end in
   aux t


(* [stackvar_intro t]: is the inverse of [stackvar_elim], hence it applies the following decodings:
     - [<annotation:stackvar> int *a = new int(5)] with [int a = 5]
     - [const int c = 5] remains unchanged
     - [<annotation:reference> int* b = a] becomes [int& b = *&(a)], which simplifies as [int& b = a]
        where &a is obtained after translating [a]
     - [<annotation:reference> int* x = &t[i]] becomes [int& x = *(&t[i])], where t has type [const int*]
       which simplifies to x = t[i]
     - [x] where [x] is mutable becomes [&x] *)
 (* TODO: some marks are lost in the printing, and this needs to be fixed,
    typically [*x] in the optitrust ast is printed as [x], and if the star operation
    was carrying a mark, then it is currently not displayed! *)
let stackvar_intro (t : trm) : trm =
  debug_current_stage "stackvar_intro";
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      (* Note: if AST invariants are preserved (are they?) the first argument of Trm_var should be equal to [Env.get !env x] *)
      if is_var_mutable !env x
        then trm_address_of (trm_replace (Trm_var (Var_mutable, x)) t)
        else t
    | Trm_let (_, (x, tx), tbody, bound_res) ->
      let vk = if is_typ_const (get_inner_array_type tx) then Var_immutable else Var_mutable in
      add_var env x vk;
      if trm_has_cstyle Stackvar t
        then
          begin match tx.typ_desc , tbody.desc with
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, Trm_apps ({desc = Trm_val (Val_prim (Prim_new _));_}, [tbody1])  ->
              trm_rem_cstyle Stackvar (trm_replace (Trm_let (vk, (x, tx1), aux tbody1, bound_res)) t)
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, _  when trm_has_cstyle Constructed_init tbody ->
            trm_rem_cstyle Stackvar (trm_replace (Trm_let (vk, (x, tx1), aux tbody, bound_res)) t)
          | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Stackvar on this declaration"
          end
      else if trm_has_cstyle Reference t then
        begin match tx.typ_desc with
        | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1} ->
          trm_rem_cstyle Reference { t with desc = Trm_let (vk, (x,typ_ptr Ptr_kind_ref tx1), trm_get (aux tbody), bound_res)}
        | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Reference on this declaration"
        end
      else trm_replace (Trm_let (vk, (x, tx), aux tbody, bound_res)) t
    | Trm_let_mult (_, tvl, tl) ->
      List.iter2 (fun (x, ty) tbody ->
        let xm = if is_typ_const (get_inner_array_type ty) then Var_immutable else Var_mutable in
        add_var env x xm
      ) tvl tl;
      trm_map aux t
    | Trm_seq _ when not (trm_is_nobrace_seq t) ->
      onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody, _) ->
      add_var env f Var_immutable;
      onscope env t (fun t ->
      List.iter (fun (x, _tx) -> let mut = Var_immutable in (add_var env x mut)) targs; trm_map aux t)
    | Trm_for (l_range, _, _) ->
      let (index, _, _, _, _, _) = l_range in
      onscope env t (fun t -> begin add_var env index Var_immutable; trm_map aux t end)
    | Trm_for_c _ -> onscope env t (fun t -> trm_map aux t)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var (_,x); _} as t1]) when is_var_mutable !env x  -> t1
    | _ -> trm_map aux t
    end in
   aux t


(* [caddress_elim t]: applies the following encodings
     - [get(t).f] becomes get(t + offset(f))
     - [t.f] becomes t + offset(f) -- nothing to do in the code of the translation
     - [get(t)[i] ] becomes [get (t + i)]
     - [t[i]] becomes [t + i] -- nothing to do in the code of the translation
     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_array_access, [t; i])))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_struct_access "f")),[t])] *)
let caddress_elim (t : trm) : trm =
  debug_current_stage "caddress_elim";
  let rec aux t =
  let mk ?(annot = trm_annot_default) td = trm_alter ~desc:td ~annot t in
  trm_simplify_addressof_and_get
  (begin
    match t.desc with
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
        trm_get { t with desc = Trm_apps ({ t with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u1; u2]) }
    | _ -> trm_map aux t
    end)
  in aux t

(* [is_access t]: checks if trm [t] is a struct access or an array access *)
let is_access (t : trm) : bool =
  match t.desc with
  | Trm_apps (tprim, _) ->
    begin match trm_prim_inv tprim with
    | Some (Prim_unop (Unop_struct_access _) |  (Prim_binop (Binop_array_access))) -> true
    | _ -> false
    end
  | _ -> false

(* [caddress_intro_aux false t]: is the inverse of [caddress_elim], hence if applies the following decodings:
     - [get(t + offset(f))] becomes [get(t).f]
     - [t + offset(f)] becomes [t.f]
     - [get (t + i)] becomes [get(t)[i]]
     - [t + i] becomes [t[i]]

     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_array_access, [t; i])))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_struct_access "f")),[ŧ])] *)
let rec caddress_intro_aux (is_access_t : bool) (t : trm) : trm =
  let aux t = caddress_intro_aux false t in  (* recursive calls for rvalues *)
  let access t = caddress_intro_aux true t in (* recursive calls for lvalues *)
  let mk td = trm_alter ~desc:td t in
  trm_simplify_addressof_and_get (* Note: might not be needed *)
  begin if is_access_t then begin
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
    | _ -> trm_get (aux t)
    end
    else begin
    match t.desc with
    | _ when is_access t ->
      (* [access(..)] becomes [& ...] *)
        trm_address_of (access t)
    | _ -> trm_map aux t
    end
  end

let caddress_intro =
  debug_current_stage "caddress_intro";
  caddress_intro_aux false

(* [cseq_items_void_type t]: updates [t] in such a way that all instructions appearing in sequences
   have type [Typ_unit]. This might not be the case, for example on [x += 2;], Menhir provides an
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

(* [infix_elim t]: encode unary and binary operators as OCaml functions, for instance
    - [x++] becomes ++(&x)
    - x = y becomes =(&x, y)
    - x += y becomes +=(&x,y) *)
let infix_elim (t : trm) : trm =
  debug_current_stage "infix_elim";
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Convert [ x += y ]  into [ (+=)(&x, y) ]
         represented as [Trm_apps (Prim_compound_assgn_op binop) [trm_addressof(x),y]],
       likewise for [ x -= y]*)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
      trm_alter ~typ:(typ_unit ()) ~desc:(Trm_apps(op, [trm_address_of tl; tr])) t
    (* Convert [ x++ ] into [ (++)(&x) ], where [(++)] is like the [incr] function in OCaml *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop unop)); _} as op, [base]) when is_postfix_unary unop ->
      trm_replace (Trm_apps(op, [trm_address_of base])) t
    (* Convert [ x = y ] into [ (=)(&x, y) ] *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set))} as op, [tl; tr]) ->
      trm_replace (Trm_apps (op, [trm_address_of tl;tr])) t
    | _ -> trm_map aux t
  in aux t

(* [infix_intro t]: decodes unary and binary oeprators back to C++ unary and binary operators
    [++(&x)] becomes [++x]
    [+=(&x, y)] becomes [x += y]
    [=(&x, y)] becomes [x = y]*)
let infix_intro (t : trm) : trm =
  debug_current_stage "infix_intro";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
      trm_replace (Trm_apps(op, [trm_get tl; tr])) t
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop unop)); _} as op, [base]) when is_postfix_unary unop ->
      trm_replace (Trm_apps(op, [trm_get base])) t
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set))} as op, [tl; tr]) ->
      trm_replace (Trm_apps (op, [trm_get tl;tr])) t
    | _ -> trm_map aux t
  in aux t


(* [method_elim t]: encodes class method calls.  *)
let method_call_elim (t : trm) : trm =
  debug_current_stage "method_call_elim";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [base])} as tr, args) ->
      let ((class_qualifier, class_name), _, _) =
        match Option.bind base.typ typ_constr_inv with
        | Some r -> r
        | None ->
        begin match Option.bind (trm_get_inv base) (fun gb ->
                    Option.bind gb.typ (fun gbt ->
                    Option.bind (typ_ptr_inv gbt) (fun btyp ->
                    typ_constr_inv btyp))) with
        | Some r -> r
        | None -> failwith (sprintf "Ast_fromto_AstC.method_call_elim: unsupported base: %s\n" (Ast_to_text.ast_to_string base))
        end
      in
      let qualifier = class_qualifier @ [class_name] in
      let t_var = begin match Ast_data.get_cursor_of_trm tr with
      | Some (cx) -> trm_add_cstyle (Clang_cursor cx) (trm_var (name_to_var ~qualifier f))
      | None -> fail t.loc "Ast_fromto_AstC.method_call_elim: method call witout cxcursor."
      end in
      trm_add_cstyle Method_call (trm_apps (t_var) ([trm_address_of base] @ args))
    | _ -> trm_map aux t
   in
   debug_before_after_trm "mcall" aux t

(* [method_call_intro t]: decodes class methods calls. *)
let method_call_intro (t : trm) : trm =
  debug_current_stage "method_call_intro";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, args) when trm_has_cstyle Method_call t ->
      if List.length args < 1 then fail t.loc "Ast_fromto_AstC.method_call_intro: bad encodings.";
      let base, args = Xlist.uncons args in
      let struct_access =
        begin match f.desc with
        | Trm_var (_, f) -> trm_struct_get (trm_get base) f.name
        (* Special case when function_beta transformation is applied. *)
        | _ -> failwith "DEPRECATED?" (* f *)
        end in
      trm_alter ~desc:(Trm_apps (struct_access, args)) t
    | _ -> trm_map aux t
     in aux t

(* [class_member_elim t]: encodes class members. *)
let class_member_elim (t : trm) : trm =
  debug_current_stage "class_member_elim";
  (* workaround to substitute untyped 'this' variables with typed 'this' variables required by 'method_call_elim' *)
  let to_subst = ref Var_map.empty in
  let get_class_typ (current_class : typvar option) (method_var : var) : typ =
    let c = match current_class with
    | Some c -> c
    | None -> snd (Xlist.unlast method_var.qualifier)
    in
    typ_ptr_generated (typ_constr ([], c) ~tid:(Clang_to_astRawC.get_typid_for_type [] c))
  in
  let rec aux (current_class : typvar option) (t : trm) : trm =
    begin match t.desc with
    | Trm_typedef td ->
      trm_map (aux (Some td.typdef_tconstr)) t
    | Trm_let_fun (v, ty, vl, body, contract) when trm_has_cstyle Method t ->
      let var_this = new_var "this" in
      let this_typ = get_class_typ current_class v in
      let typed_this = trm_var ~typ:this_typ var_this in
      to_subst := Var_map.add var_this typed_this !to_subst;
      trm_alter ~desc:(Trm_let_fun (v, ty, (var_this, this_typ) :: vl, body, contract)) t
    | Trm_let_fun (v, ty, vl, body, contract) when is_class_constructor t ->
      let this_mut = Var_immutable in
      let var_this = new_var "this" in
      let this_typ = get_class_typ current_class v in
      let this_body = trm_apps (trm_toplevel_free_var  "malloc") [trm_toplevel_free_var ("sizeof(" ^ v.name ^ ")")] in
      let this_alloc = trm_let this_mut (var_this, this_typ) this_body in
      let typed_this = trm_var ~typ:this_typ var_this in
      to_subst := Var_map.add var_this typed_this !to_subst;
      let ret_this = trm_ret (Some (trm_get typed_this)) in
      begin match body.desc with
      | Trm_seq tl ->
        let new_tl = Mlist.map (trm_subst_var var_this typed_this) tl in
        let new_tl = Mlist.push_front this_alloc new_tl in
        let new_tl = Mlist.push_back ret_this new_tl in
        let new_body = trm_alter ~desc:(Trm_seq new_tl) t in
        trm_alter ~desc:(Trm_let_fun (v, this_typ, vl, new_body, contract)) t
      | Trm_val (Val_lit Lit_uninitialized) ->  t
      | _ ->  fail t.loc "Ast_fromto_AstC.class_member_elim: ill defined class constructor."
      end
    | _ -> trm_map (aux current_class) t
    end
  in
  let on_subst old_t new_t =
    (* keep implicit this annotations etc *)
    trm_alter ~annot:old_t.annot ~loc:old_t.loc new_t
  in
  debug_before_after_trm "cmember" (fun t ->
    aux None t |> C_scope.infer_var_ids |> trm_subst ~on_subst !to_subst
  ) t


(* [class_member_intro t]: decodes class members. *)
let class_member_intro (t : trm) : trm =
  debug_current_stage "class_member_intro";
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_let_fun (qv, ty, vl, body, contract) when trm_has_cstyle Method t ->
      let ((v_this, _), vl') = Xlist.uncons vl in
      let rec fix_body t =
        match trm_var_inv t with
        | Some x when var_eq x v_this && (not (trm_has_cstyle Implicit_this t)) ->
          trm_like ~old:t (trm_this ())
        | _ -> trm_map fix_body t
      in
      let body' = fix_body body in
      trm_alter ~desc:(Trm_let_fun (qv, ty, vl', body', contract)) t
    | Trm_let_fun (qv, ty, vl, body, contract) when is_class_constructor t ->
      begin match body.desc with
      | Trm_seq tl ->
        if Mlist.is_empty tl
          then t
          else
            let tl = Mlist.(pop_front (pop_back tl)) in
            let new_body = trm_alter ~desc:(Trm_seq tl) body in
            trm_alter ~desc:(Trm_let_fun (qv, typ_unit(), vl, new_body, contract)) t
      | _ -> trm_map aux t
      end
   | _ -> trm_map aux t
in aux t

(********************** Decode contract annotations ***************************)

open Resources_contract

let __pure = name_to_var "__pure"
let __requires = name_to_var "__requires"
let __ensures = name_to_var "__ensures"
let __invariant = name_to_var "__invariant"
let __reads = name_to_var "__reads"
let __modifies = name_to_var "__modifies"
let __consumes = name_to_var "__consumes"
let __produces = name_to_var "__produces"
let __sequentially_reads = name_to_var "__sequentially_reads"
let __sequentially_modifies = name_to_var "__sequentially_modifies"
let __ctx_res = name_to_var "__ctx_res"
let __produced_res = name_to_var "__produced_res"
let __used_res = name_to_var "__used_res"
let __framed_res = name_to_var "__framed_res"
let __contract_inst = name_to_var "__contract_inst"
let __post_inst = name_to_var "__post_inst"

let encoded_contract_inv (t: trm): (contract_clause_type * string) option =
  let open Tools.OptionMonad in
  let* fn, args = trm_apps_inv t in
  let* fn_var = trm_var_inv fn in
  let* clause =
    match fn_var.name with
    | "__pure" -> Some Requires
    | "__requires" -> Some Requires
    | "__ensures" -> Some Ensures
    | "__invariant" -> Some Invariant
    | "__reads" -> Some Reads
    | "__modifies" -> Some Modifies
    | "__consumes" -> Some Consumes
    | "__produces" -> Some Produces
    | "__sequentially_reads" -> Some SequentiallyReads
    | "__sequentially_modifies" -> Some SequentiallyModifies
    | _ -> None
  in
  let arg = Option.value ~default:(trm_string "") (List.nth_opt args 0) in
  let* arg = trm_lit_inv arg in
  let* arg =
    match arg with
    | Lit_string s -> Some s
    | _ -> None
  in
  Some (clause, arg)

let rec extract_encoded_contract_clauses (seq: trm mlist):
  (contract_clause_type * string) list * trm mlist =
  match Option.bind (Mlist.nth_opt seq 0) encoded_contract_inv with
  | Some contract ->
    let cont, seq = extract_encoded_contract_clauses (Mlist.pop_front seq) in
    contract::cont, seq
  | None -> [], seq

let extract_contract (empty_contract: 'c) (push_contract_clause: contract_clause_type -> contract_resource -> 'c -> 'c) (seq: trm mlist) : 'c option * trm mlist =
  let enc_contract, seq = extract_encoded_contract_clauses seq in
  let contract = List.fold_left (fun contract (clause, desc) ->
      let contract = Option.value ~default:empty_contract contract in
      try
        let res_list = Resource_cparser.resource_list (Resource_clexer.lex_resources) (Lexing.from_string desc) in
        Some (List.fold_left (fun contract res -> push_contract_clause clause res contract) contract res_list)
      with Resource_cparser.Error ->
        failwith ("Failed to parse resource: " ^ desc)
    ) None enc_contract
  in
  (contract, seq)

let extract_fun_contract (seq: trm mlist) : fun_spec * trm mlist =
  extract_contract empty_fun_contract push_fun_contract_clause seq

let extract_loop_contract (seq: trm mlist) : loop_spec * trm mlist =
  extract_contract empty_loop_contract push_loop_contract_clause seq

let contract_elim (t: trm): trm =
  debug_current_stage "contract_elim";
  let rec aux t =
  match t.desc with
  | Trm_let_fun (qv, ty, args, body, contract) ->
    assert (contract = None);
    begin match trm_seq_inv body with
    | Some body_seq ->
      let contract, new_body = extract_fun_contract body_seq in
      let new_body = Mlist.map aux new_body in
      trm_alter ~desc:(Trm_let_fun (qv, ty, args, trm_seq new_body, contract)) t
    | None -> trm_map aux t
    end

  | Trm_for (range, body, contract) ->
    assert (contract = None);
    begin match trm_seq_inv body with
    | Some body_seq ->
      let contract, new_body = extract_loop_contract body_seq in
      let new_body = Mlist.map aux new_body in
      trm_alter ~desc:(Trm_for (range, trm_seq new_body, contract)) t
    | None -> trm_map aux t
    end

  | _ -> trm_map aux t
  in aux t

let rec formula_to_string (f: formula) : string =
  match formula_read_only_inv f with
  | Some { frac; formula } -> sprintf "RO(%s, %s)" (formula_to_string frac) (formula_to_string formula)
  | None ->
    match formula_model_inv f with
    | Some (t, formula) -> Printf.sprintf "%s ~> %s" (AstC_to_c.ast_to_string ~optitrust_syntax:true t) (formula_to_string formula)
    | None -> AstC_to_c.ast_to_string ~optitrust_syntax:true f (* LATER: use a custom printer for formulas *)

let named_formula_to_string (hyp, formula): string =
  let sformula = formula_to_string formula in
  if not !Flags.always_name_resource_hyp && hyp.name.[0] = '#'
    then Printf.sprintf "%s;" sformula
    else Printf.sprintf "%s: %s;" hyp.name sformula

(* FIXME: Copied from Sequence_core to avoid circular dependancy *)
(* [insert_aux index code t]: inserts trm [code] at index [index] in sequence [t],
    [index] - a valid index where the instruction can be added,
    [code] - instruction to be added as an arbitrary trm,
    [t] - ast of the outer sequence where the insertion will be performed. *)
let insert_aux (index : int) (code : trm) (t : trm) : trm =
  let error = "Sequence_core.insert_aux: expected the sequence on where insertion is performed." in
  let tl = trm_inv ~error trm_seq_inv t in
  let new_tl = Mlist.insert_at index code tl in
  (* TODO: Should use alter here ? *)
  trm_seq ~annot:t.annot new_tl

let ctx_resource_list_to_string ?(sep = " ") (res: resource_item list) : string =
  String.concat sep (List.map named_formula_to_string res)

let ctx_resources_to_trm (res: resource_set) : trm =
  let spure = ctx_resource_list_to_string res.pure in
  let slin = ctx_resource_list_to_string res.linear in
  trm_apps (trm_var __ctx_res) [trm_string spure; trm_string slin]

let ctx_used_res_item_to_string (res: used_resource_item) : string =
  let sinst = formula_to_string res.inst_by in
  let sformula = formula_to_string res.used_formula in
  Printf.sprintf "%s := %s : %s;" res.hyp_to_inst.name sinst sformula

let ctx_used_res_to_trm ~(clause: var) (used_res: used_resource_set) : trm =
  let spure = String.concat " " (List.map ctx_used_res_item_to_string used_res.used_pure) in
  let slin = String.concat " " (List.map ctx_used_res_item_to_string used_res.used_linear) in
  trm_apps (trm_var clause) [trm_string spure; trm_string slin]

let ctx_produced_res_item_to_string (res: produced_resource_item) : string =
  let sformula = formula_to_string res.produced_formula in
  Printf.sprintf "%s := %s : %s;" res.produced_hyp.name res.produced_from.name sformula

let ctx_produced_res_to_trm (produced_res: produced_resource_set) : trm =
  let spure = String.concat " " (List.map ctx_produced_res_item_to_string produced_res.produced_pure) in
  let slin = String.concat " " (List.map ctx_produced_res_item_to_string produced_res.produced_linear) in
  trm_apps (trm_var __produced_res) [trm_string spure; trm_string slin]

let display_ctx_resources (t: trm): trm list =
  let t =
    match t.desc with
    | Trm_let (_, _, body, _) -> { t with ctx = { body.ctx with ctx_resources_before = t.ctx.ctx_resources_before; ctx_resources_after = t.ctx.ctx_resources_after } }
    | _ -> t
  in
  let tl_used = Option.to_list (Option.map (fun res_used ->
      let s_used = String.concat " " (List.filter_map (function
          | _, NotUsed -> None
          | hyp, UsedReadOnly -> Some (sprintf "RO(%s);" hyp.name)
          | hyp, UsedFull -> Some (sprintf "%s;" hyp.name))
          (Hyp_map.bindings res_used))
      in
      trm_apps (trm_var __used_res) [trm_string s_used]) t.ctx.ctx_resources_usage) in
  let tl = match t.ctx.ctx_resources_contract_invoc with
    | None -> [t]
    | Some contract_invoc ->
      let t_frame = trm_apps (trm_var __framed_res) [trm_string (ctx_resource_list_to_string contract_invoc.contract_frame)] in
      let t_inst = ctx_used_res_to_trm ~clause:__contract_inst contract_invoc.contract_inst in
      let t_produced = ctx_produced_res_to_trm contract_invoc.contract_produced in
      [t_frame; t_inst; t; t_produced]
  in
  let tl_after = Option.to_list (Option.map ctx_resources_to_trm t.ctx.ctx_resources_after) in
  (tl_used @ tl @ tl_after)

let computed_resources_intro (t: trm): trm =
  let rec aux t =
    match t.desc with
    | Trm_seq instrs when not (List.mem Main_file (trm_get_files_annot t)) ->
      let tl_before = Option.to_list (Option.map ctx_resources_to_trm t.ctx.ctx_resources_before) in
      let tl_post_inst = Option.to_list (Option.map (ctx_used_res_to_trm ~clause:__post_inst) t.ctx.ctx_resources_post_inst) in
      trm_like ~old:t (trm_seq (Mlist.of_list (tl_before @ List.concat_map (fun instr -> display_ctx_resources (aux instr)) (Mlist.to_list instrs) @ tl_post_inst)))
    | _ -> trm_map_with_terminal_opt ~keep_ctx:true false (fun _ -> aux) t
  in
  aux t


let rec contract_intro (t: trm): trm =
  (* debug_current_stage "contract_intro"; *)
  let push_named_formulas (contract_prim: var) (named_formulas: resource_item list) (t: trm): trm =
    if named_formulas = [] then
      t
    else
      let sres = ctx_resource_list_to_string named_formulas in
      let tres = trm_apps (trm_var contract_prim) [trm_string sres] in
      insert_aux 0 tres t
  in

  let push_reads_and_modifies (reads_prim: var) (modifies_prim: var) (pre: resource_set) (post: resource_set) (t: trm): resource_set * resource_set * trm =
    let post_linear = ref post.linear in
    let rec try_remove_same_formula formula l =
      match l with
      | [] -> None
      | (_,f)::l when are_same_trm f formula -> Some l
      | res::l -> Option.map (fun l -> res::l) (try_remove_same_formula formula l)
    in
    let common_linear, pre_linear = List.partition
      (fun (_, formula) ->
        match try_remove_same_formula formula !post_linear with
        | None -> false
        | Some new_post_linear -> post_linear := new_post_linear; true)
      pre.linear
    in

    (* FIXME: This assumes that all fractions were auto-generated *)
    let frac_to_remove = Hashtbl.create (List.length common_linear) in
    let reads_res, modifies_res =
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
    let pre_pure = List.filter hyp_not_mem_before_pop pre.pure in
    assert (Hashtbl.length frac_to_remove = 0);

    let t = push_named_formulas reads_prim reads_res t in
    let t = push_named_formulas modifies_prim modifies_res t in
    ({ pre with pure = pre_pure; linear = pre_linear }, { post with linear = !post_linear }, t)
  in

  let push_fun_contract (contract: fun_contract) (body: trm): trm =
    let pre, post, body = push_reads_and_modifies __reads __modifies contract.pre contract.post body in
    let body = push_named_formulas __produces post.linear body in
    let body = push_named_formulas __ensures post.pure body in
    let body = push_named_formulas __consumes pre.linear body in
    let body = push_named_formulas __requires pre.pure body in
    body
  in

  match t.desc with
  | Trm_let_fun (qv, ty, args, body0, contract) ->
    let body = contract_intro body0 in
    let body =
      match contract with
      | Some contract when contract = empty_fun_contract ->
        insert_aux 0 (trm_apps (trm_var __pure) []) body
      | Some contract ->
        push_fun_contract contract body
      | None -> body
    in
    if body == body0
      then t
      else trm_like ~old:t (trm_let_fun qv ty args body)

  | Trm_for (range, body0, contract) ->
    let body = contract_intro body0 in
    let body =
      match contract with
      | Some contract when contract = empty_loop_contract ->
        insert_aux 0 (trm_apps (trm_var __pure) []) body
      | Some contract ->
        let body = push_fun_contract contract.iter_contract body in
        let _, invariant, body = push_reads_and_modifies __sequentially_reads __sequentially_modifies { contract.invariant with pure = contract.loop_ghosts @ contract.invariant.pure } contract.invariant body in
        assert (invariant.linear = []);
        push_named_formulas __invariant invariant.pure body
      | None -> body
    in
    if body == body0
      then t
      else trm_like ~old:t (trm_for range body)

  | Trm_seq instrs ->
    trm_like ~old:t (trm_seq (Mlist.map contract_intro instrs))

  | _ -> trm_map contract_intro t

(*************************************** Main entry points *********************************************)

(* [cfeatures_elim t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let cfeatures_elim: trm -> trm =
  debug_before_after_trm "cfeatures_elim" (fun t ->
  contract_elim t |>
  class_member_elim |>
  method_call_elim |>
  infix_elim |>
  C_scope.infer_var_ids |>
  stackvar_elim |>
  C_scope.infer_var_ids |>
  caddress_elim |>
  C_scope.infer_var_ids |>
  cseq_items_void_type |>
  C_scope.infer_var_ids)

(* [cfeatures_intro t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let cfeatures_intro : trm -> trm =
  debug_before_after_trm "cfeatures_intro" (fun t ->
  C_scope.infer_var_ids t |>
  caddress_intro |>
  stackvar_intro |>
  infix_intro |>
  method_call_intro |>
  class_member_intro |>
  contract_intro)

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
