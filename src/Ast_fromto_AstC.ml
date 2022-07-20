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
type env = varkind String_map.t

(* [env_empty]: empty environment *)
let env_empty =
  String_map.empty

(* [get_varkind env x]: gets the mutability of variable [x]
   Note: Functions that come from an external library are set to immutable by default *)
let get_varkind (env : env) (x : var) : varkind =
  match String_map.find_opt x env with
  | Some m -> m
  | _ -> Var_immutable

(* [is_qvar_mutable env x]: checks if variable [x] is mutable or not *)
let is_qvar_mutable (env : env) (x : qvar) : bool =
  get_varkind env x.qvar_var = Var_mutable

(* [env_extend env e varkind]: adds variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  String_map.add e varkind env

(* [add_var env x xm]: adds variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit =
  env := env_extend !env x xm

(* [trm_address_of t]: adds the "&" operator before [t]
    Note: if for example t = *a then [trm_address_of t] = &( *a) = a *)
let trm_address_of (t : trm) : trm =
  let u = trm_apps ~typ:t.typ (trm_unop Unop_address) [t] in
  trm_simplify_addressof_and_get u

(* [trm_get t]: adds the "*" operator before [t]
    Note: if for example t = &a then [trm_get t] = *( &a) = a *)
let trm_get (t : trm) : trm =
  let u = trm_apps ~typ:t.typ (trm_unop Unop_get) [t] in
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
    - [int a = 5] with [<annotation:stackvar> int* a = new int(5)] and a variable occurrence [a] becomes [ * a]
    - [const int c = 5] remains unchange
    - simplify patterns of the form [&*p] into [p].
    - [int& b = a] becomes [<annotation:reference> int* b = a] as a simplification of [b = &*a]
    - [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [const int*].

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
    | Trm_let (_, (x, ty), tbody) ->
      let xm = if is_typ_const (get_inner_array_type ty) then Var_immutable else Var_mutable in
      add_var env x xm;
      begin match typ_ref_inv ty with
      | Some ty1 ->
        begin match xm with
        | Var_immutable -> fail t.loc "Ast_fromto_AstC.tackvar_elim: unsupported references on const variables"
        | _ -> trm_add_cstyle Reference (trm_replace (Trm_let (xm, (x, typ_ptr_generated ty1), trm_address_of (aux tbody))) t)
        end
      | None ->
        begin match xm with
        | Var_mutable ->
          let new_body = if trm_has_cstyle Constructed_init tbody then aux tbody else trm_new ty (aux tbody) in 
          trm_add_cstyle Stackvar (trm_replace (Trm_let (xm, (x, typ_ptr_generated ty), new_body )) t)
        | Var_immutable ->
          trm_map aux t
        end
      end
    | Trm_seq _ when not (trm_is_nobrace_seq t) -> onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody) ->
      (* function names are by default immutable *)
      add_var env f.qvar_var Var_immutable;
      onscope env t (fun t -> List.iter (fun (x, _tx) ->
       let mut = Var_immutable in (* if is_typ_ptr tx then Var_mutable else Var_immutable in *)
       add_var env x mut) targs; trm_map aux t)
    | Trm_for (l_range, _) ->
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
     - [<annotation:reference> int* b = a] becomes [int& b = a], as a simplification of b = *(&a)
        where &a is obtained after translating [a]
     - [<annotation:reference> int* x = &t[i]] becomes [int& x = t[i]], where t has type [const int*]
       as a simplification of x = *(&t[i]) *)
let stackvar_intro (t : trm) : trm =
  let env = create_env () in
  let rec aux (t : trm) : trm =
    trm_simplify_addressof_and_get
    begin match t.desc with
    | Trm_var (_, x) ->
      if is_qvar_mutable !env x
        then trm_address_of (trm_replace (Trm_var (Var_mutable, x)) t)
        else t
    | Trm_let (_, (x, tx), tbody) ->
      let vk = if is_typ_const (get_inner_array_type tx) then Var_immutable else Var_mutable in
      add_var env x vk;
      if trm_has_cstyle Stackvar t
        then
          begin match tx.typ_desc , tbody.desc with
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, Trm_apps ({desc = Trm_val (Val_prim (Prim_new _));_}, [tbody1])  ->
              trm_rem_cstyle Stackvar (trm_replace (Trm_let (vk, (x, tx1), aux tbody1)) t)
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, _  when trm_has_cstyle Constructed_init tbody ->
              trm_rem_cstyle Stackvar (trm_replace (Trm_let (vk, (x, tx1), aux tbody)) t)
          | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Stackvar on this declaration"
          end
      else if trm_has_cstyle Reference t then
        begin match tx.typ_desc with
        | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1} ->
          trm_rem_cstyle Reference { t with desc = Trm_let (vk, (x,typ_ptr Ptr_kind_ref tx1), trm_get (aux tbody))}
        | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Reference on this declaration"
        end
      else trm_replace (Trm_let (vk, (x, tx), aux tbody)) t
    | Trm_seq _ when not (trm_is_nobrace_seq t) ->
      onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody) ->
      add_var env f.qvar_var Var_immutable;
      onscope env t (fun t ->
      List.iter (fun (x, _tx) -> let mut = Var_immutable in (add_var env x mut)) targs; trm_map aux t)
    | Trm_for (l_range, _) ->
      let (index, _, _, _, _, _) = l_range in
      onscope env t (fun t -> begin add_var env index Var_immutable; trm_map aux t end)
    | Trm_for_c _ -> onscope env t (fun t -> trm_map aux t)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var (_,x); _} as t1]) when is_qvar_mutable !env x  -> t1
    | _ -> trm_map aux t
    end in
   aux t


(* [caddress_elim t]: applies the following encodings
     - [get(t).f] becomes get(t + offset f)
     - [t.f] becomes t + offset(f)
     - [get(t)[i] ] becomes [get (t + i)]
     - [t[i]] becomes [t + i]
     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_array_access, [t; i])))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_struct_access "f")),[ŧ])] *)
let rec caddress_elim (t : trm) : trm =
  let aux t = caddress_elim t in (* recursive calls for rvalues *)
  let mk ?(annot = trm_annot_default) td = trm_alter ~desc:(Some td) ~annot:(Some annot) t in
  trm_simplify_addressof_and_get
  begin
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
    end

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
  let mk td = trm_alter ~desc:(Some td) t in
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
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
      trm_replace (Trm_apps(op, [trm_address_of tl; tr])) t
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop unop)); _} as op, [base]) when is_postfix_unary unop ->
      trm_replace (Trm_apps(op, [trm_address_of base])) t
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
    | Trm_apps ({desc = Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [base])}, args) ->
       trm_add_cstyle Method_call (trm_apps (trm_var f) ([base] @ args))
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
      trm_alter ~desc:(Some (Trm_apps (struct_access, args))) t
    | _ -> trm_map aux t
     in aux t

(* [class_member_elim t]: encodes class members. *)
let class_member_elim (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with 
    | Trm_let_fun (qv, ty, vl, body) when is_class_constructor t ->
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
        let new_body = trm_alter ~desc:(Some (Trm_seq new_tl)) t in 
        trm_alter ~desc:(Some (Trm_let_fun (qv, this_typ, vl, new_body))) t
      | Trm_val (Val_lit Lit_uninitialized) ->  t
      | _ ->  fail t.loc "Ast_fromto_AstC.class_member_elim: ill defined class constructor."
      end
    | _ -> trm_map aux t
  in aux t
  

(* [class_member_intro t]: decodes class members. *)
let class_member_intro (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with 
    | Trm_let_fun (qv, ty, vl, body) when is_class_constructor t ->
      begin match body.desc with 
      | Trm_seq tl -> 
        if Mlist.is_empty tl 
          then t
          else 
            let tl = Mlist.(pop_front (pop_back tl)) in 
            let new_body = trm_alter ~desc:(Some (Trm_seq tl)) body in 
            trm_alter ~desc:(Some (Trm_let_fun (qv, typ_unit(), vl, new_body))) t
      | _ -> trm_map aux t
      end
   | _ -> trm_map aux t 
in aux t

(***************************************  Main entry points *********************************************)

(* [cfeatures_elim t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let cfeatures_elim (t : trm) : trm =
  class_member_elim (cseq_items_void_type (caddress_elim (stackvar_elim (infix_elim (method_call_elim t)))))

(* [cfeatures_intro t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let cfeatures_intro (t : trm) : trm =
  method_call_intro (infix_intro (stackvar_intro (caddress_intro (class_member_intro t))))

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
