open Ast

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

(* [is_qvar_mutable env x]: checks if variable [x] is mutable or not *)
let is_qvar_mutable (env : env) (x : qvar) : bool =
  get_varkind env x.qvar_var = Var_mutable

(* [env_extend env e varkind]: adds variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  Var_map.add e varkind env

(* [add_var env x xm]: adds variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit =
  env := env_extend !env x xm

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
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      if is_qvar_mutable !env x
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
      add_var env f.qvar_var Var_immutable;
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
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      (* Note: if AST invariants are preserved (are they?) the first argument of Trm_var should be equal to [Env.get !env x] *)
      if is_qvar_mutable !env x
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
      add_var env f.qvar_var Var_immutable;
      onscope env t (fun t ->
      List.iter (fun (x, _tx) -> let mut = Var_immutable in (add_var env x mut)) targs; trm_map aux t)
    | Trm_for (l_range, _, _) ->
      let (index, _, _, _, _, _) = l_range in
      onscope env t (fun t -> begin add_var env index Var_immutable; trm_map aux t end)
    | Trm_for_c _ -> onscope env t (fun t -> trm_map aux t)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var (_,x); _} as t1]) when is_qvar_mutable !env x  -> t1
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
let rec caddress_elim (t : trm) : trm =
  let aux t = caddress_elim t in (* recursive calls for rvalues *)
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

let caddress_intro = caddress_intro_aux false

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
  let rec aux (t : trm) : trm =
    match t.desc with
    (* Convert [ x += y ]  into [ (+=)(&x, y) ]
         represented as [Trm_apps (Prim_compound_assgn_op binop) [trm_addressof(x),y]],
       likewise for [ x -= y]*)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
      trm_replace (Trm_apps(op, [trm_address_of tl; tr])) t
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
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [base])} as tr, args) ->
      let t_var = begin match Ast_data.get_cursor_of_trm tr with
      | Some (cx) -> trm_add_cstyle (Clang_cursor cx) (trm_var f)
      | None -> fail t.loc "Ast_fromto_AstC.method_call_elim: method call witout cxcursor."
      end in
      trm_add_cstyle Method_call (trm_apps (t_var) ([base] @ args))
    | _ -> trm_map aux t
   in aux t


(* [method_call_intro t]: decodes class methods calls. *)
let method_call_intro (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (f, args) when trm_has_cstyle Method_call t ->
      if List.length args < 1 then fail t.loc "Ast_fromto_AstC.method_call_intro: bad encodings.";
      let base, args = Xlist.uncons args in
      let struct_access =
        begin match f.desc with
        | Trm_var (_, f) -> trm_struct_get base f.qvar_var
        (* Special case when function_beta transformation is applied. *)
        | _ -> f
        end in
      trm_alter ~desc:(Trm_apps (struct_access, args)) t
    | _ -> trm_map aux t
     in aux t

(* [class_member_elim t]: encodes class members. *)
let class_member_elim (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_let_fun (qv, ty, vl, body, contract) when is_class_constructor t ->
      let this_mut = Var_mutable in
      let q_var = qv.qvar_var in
      let this_typ = typ_ptr_generated (typ_constr q_var ~tid:(Clang_to_astRawC.get_typid_for_type q_var)) in
      let this_body = trm_apps (trm_var "malloc") [trm_var ("sizeof(" ^ q_var ^ ")")] in
      let this_alloc = trm_let this_mut ("this", this_typ) this_body in
      let ret_this = trm_ret (Some (trm_var_get "this")) in
      begin match body.desc with
      | Trm_seq tl ->
        let new_tl = Mlist.push_front this_alloc tl in
        let new_tl = Mlist.push_back ret_this new_tl in
        let new_body = trm_alter ~desc:(Trm_seq new_tl) t in
        trm_alter ~desc:(Trm_let_fun (qv, this_typ, vl, new_body, contract)) t
      | Trm_val (Val_lit Lit_uninitialized) ->  t
      | _ ->  fail t.loc "Ast_fromto_AstC.class_member_elim: ill defined class constructor."
      end
    | _ -> trm_map aux t
  in aux t


(* [class_member_intro t]: decodes class members. *)
let class_member_intro (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
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

let encoded_contract_inv (t: trm): (contract_clause_type * string) option =
  let open Tools.OptionMonad in
  let* fn, args = trm_apps_inv t in
  let* fn_name = trm_var_inv fn in
  let* clause =
    match fn_name with
    | "__requires" -> Some Requires
    | "__ensures" -> Some Ensures
    | "__invariant" -> Some Invariant
    | "__reads" -> Some Reads
    | "__modifies" -> Some Modifies
    | "__consumes" -> Some Consumes
    | "__produces" -> Some Produces
    | "__sequentially_modifies" -> Some SequentiallyModifies
    | _ -> None
  in
  let* arg = List.nth_opt args 0 in
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

let rec contract_elim (t: trm): trm =
  match t.desc with
  | Trm_let_fun (qv, ty, args, body, contract) ->
    assert (contract = None);
    let body_seq = trm_inv trm_seq_inv body in
    let contract, new_body = extract_fun_contract body_seq in
    let new_body = Mlist.map contract_elim new_body in
    trm_alter ~desc:(Trm_let_fun (qv, ty, args, trm_seq new_body, contract)) t
  | Trm_for (range, body, contract) ->
    assert (contract = None);
    let body_seq = trm_inv trm_seq_inv body in
    let contract, new_body = extract_loop_contract body_seq in
    let new_body = Mlist.map contract_elim new_body in
    trm_alter ~desc:(Trm_for (range, trm_seq new_body, contract)) t
  (*| Trm_for_c (init, cond, step, body, _) -> failwith "TODO"*)
  | _ -> trm_map contract_elim t

let rec formula_to_string (f: formula) : string =
  match formula_read_only_inv f with
  | Some formula -> sprintf "RO(%s)" (formula_to_string formula)
  | None ->
    match formula_var_model_inv f with
    | Some (x, formula) -> Printf.sprintf "%s => %s" x (formula_to_string formula)
    | None -> AstC_to_c.ast_to_string ~optitrust_syntax:true f (* LATER: use a custom printer for formulas *)

let named_formula_to_string (name, formula): string =
  let sformula = formula_to_string formula in
  match name with
  | None ->
      Printf.sprintf "%s;" sformula
  | Some name ->
      Printf.sprintf "%s: %s;" name sformula

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

let ctx_resource_list_to_string (res: resource_item list) : string =
  String.concat " " (List.map named_formula_to_string res)

let ctx_resources_to_trm ?(fn_name = "__ctx_res") (res: resource_set) : trm =
  let spure = ctx_resource_list_to_string res.pure in
  let slin = ctx_resource_list_to_string res.linear in
  trm_apps (trm_var fn_name) [trm_string spure; trm_string slin]


let display_ctx_resources (t: trm): trm list =
  let tl_after = Option.to_list (Option.map ctx_resources_to_trm t.ctx.ctx_resources_after) in
  let tl_frame = Option.to_list (Option.map (fun res_frame ->
      trm_apps (trm_var "__framed_res") [trm_string (ctx_resource_list_to_string res_frame)]) t.ctx.ctx_resources_frame) in
  let tl_used = Option.to_list (Option.map (ctx_resources_to_trm ~fn_name:"__used_res") t.ctx.ctx_resources_used) in
  let tl_produced = Option.to_list (Option.map (ctx_resources_to_trm ~fn_name:"__produced_res") t.ctx.ctx_resources_produced) in
  (tl_frame @ tl_used @ [t] @ tl_produced @ tl_after)

let computed_resources_intro (t: trm): trm =
  let rec aux t =
    match t.desc with
    | Trm_seq instrs when not (List.mem Main_file (trm_get_files_annot t)) ->
      let tl_before = Option.to_list (Option.map ctx_resources_to_trm t.ctx.ctx_resources_before) in
      trm_like ~old:t (trm_seq (Mlist.of_list (tl_before @ List.concat_map (fun instr -> display_ctx_resources (aux instr)) (Mlist.to_list instrs))))
    | _ -> trm_map_with_terminal_opt ~keep_ctx:true false (fun _ -> aux) t
  in
  aux t


let rec contract_intro (t: trm): trm =
  let push_named_formulas (contract_prim: var) (named_formulas: (var option * formula) list) (t: trm): trm =
    let sres = ctx_resource_list_to_string named_formulas in
    let tres = trm_apps (trm_var contract_prim) [trm_string sres] in
    insert_aux 0 tres t
  in

  let push_resource_set (pure_prim: var) (linear_prim: var) (res_set: resource_set) (t: trm): trm =
    let t =
      if res_set.linear <> []
        then push_named_formulas linear_prim res_set.linear t
        else t
    in
    if res_set.pure <> [] || res_set.linear == [] then
      push_named_formulas pure_prim res_set.pure t
    else t
  in

  match t.desc with
  | Trm_let_fun (qv, ty, args, body0, contract) ->
    let body = contract_intro body0 in
    let body = match contract with
      | Some contract ->
        let body = push_resource_set "__ensures" "__produces" contract.post body in
        push_resource_set "__requires" "__consumes" contract.pre body
      | None -> body
    in
    if body == body0
      then t
      else trm_like ~old:t (trm_let_fun (qvar_to_var qv) ty args body ?contract)

  | Trm_seq instrs ->
    trm_like ~old:t (trm_seq (Mlist.map contract_intro instrs))

  | _ -> trm_map contract_intro t

(*************************************** Main entry points *********************************************)

(* [cfeatures_elim t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let cfeatures_elim (t : trm) : trm =
  class_member_elim (cseq_items_void_type (caddress_elim (stackvar_elim (infix_elim (method_call_elim (contract_elim t))))))

(* [cfeatures_intro t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let cfeatures_intro (t : trm) : trm =
  contract_intro (method_call_intro (infix_intro (stackvar_intro (caddress_intro (class_member_intro t)))))

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
