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

(* [get_varkind env x]: get the mutability of variable [x] 
   Note: Functions that come from an external library are set to immutable by default *)
let get_varkind (env : env) (x : var) : varkind =
  match String_map.find_opt x env with
  | Some m -> m
  | _ -> Var_immutable 

(* [is_var_mutable env x]: check if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_varkind env x = Var_mutable

(* [env_extend env e varkind]: add variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  String_map.add e varkind env

(* [add_var env x xm]: add variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit =
  env := env_extend !env x xm

(* [trm_address_of t]: add the "&" operator before [t]
    Note: if for example t = *a then [trm_address_of t] = &( *a) = a *)
let trm_address_of (t : trm) : trm =
  let u = trm_apps ~typ:t.typ (trm_unop Unop_address) [t] in
  trm_simplify_addressof_and_get u

(* [trm_get t]: add the "*" operator before [t]
    Note: if for example t = &a then [trm_get t] = *( &a) = a *)
let trm_get (t : trm) : trm =
  let u = trm_apps ~typ:t.typ (trm_unop Unop_get) [t] in
  trm_simplify_addressof_and_get u

(* [onscope env t f]: apply function [f] on [t] without loosing [env] *)
let onscope (env : env ref) (t : trm) (f : trm -> trm) : trm =
    let saved_env = !env in
    let res = f t in
    env := saved_env;
    res

(* [create_env]: create an empty environment *)
let create_env () = ref env_empty


(* [stackvar_elim t]: apply the following changes:
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
      if is_var_mutable !env x
        then trm_get {t with desc = Trm_var (Var_mutable, x)}
        else { t with desc = Trm_var (Var_immutable, x) }
    | Trm_let (_, (x, ty), tbody) ->
      let xm = if is_typ_const ty then Var_immutable else Var_mutable in
      add_var env x xm;
      begin match typ_ref_inv ty with
      | Some ty1 ->
        begin match xm with
        | Var_immutable -> fail t.loc "stackvar_elim: unsupported references on const variables"
        | _ ->
          trm_annot_add Reference {t with desc = Trm_let (xm, (x, typ_ptr_generated ty1), trm_address_of (aux tbody))}
        end
      | None ->
        begin match xm with
        | Var_mutable ->
          trm_annot_add Stackvar {t with desc = Trm_let (xm, (x, typ_ptr_generated ty), trm_new ty (aux tbody) )}
        | Var_immutable ->
          trm_map aux t
        end
      end
    | Trm_seq _ when not (is_nobrace_seq t) -> onscope env t (trm_map aux)
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
    | _ -> trm_map aux t
   end in
   aux t


(* [stackvar_intro t]: is the inverse of [stackvar_elim], hence it applies the following changes:
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
      if is_var_mutable !env x
        then trm_address_of {t with desc = Trm_var (Var_mutable, x)} 
        else t
    | Trm_let (_, (x, tx), tbody) ->
      let vk = if is_typ_const tx then Var_immutable else Var_mutable in
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
          trm_annot_remove Reference { t with desc = Trm_let (vk, (x,typ_ptr Ptr_kind_ref tx1), trm_get (aux tbody))}
        | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Reference on this declaration"
        end
      else
        {t with desc = Trm_let (vk, (x, tx), aux tbody)}
    | Trm_seq _ when not (is_nobrace_seq t) ->
      onscope env t (trm_map aux)
    | Trm_let_fun (f, _retty, targs, _tbody) ->
      add_var env f Var_immutable;
      onscope env t (fun t ->
      List.iter (fun (x, _tx) -> let mut = Var_immutable in (add_var env x mut)) targs; trm_map aux t)
    | Trm_for (index, _, _, _, _, _) ->
      onscope env t (fun t -> begin add_var env index Var_immutable; trm_map aux t end)
    | Trm_for_c _ -> onscope env t (fun t -> trm_map aux t)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var (_,x); _} as t1]) when is_var_mutable !env x  -> t1
    | _ -> trm_map aux t
    end in
   aux t


(* [caddress_elim t]: applies the following changes
     - [get(t).f] becomes get(t + offset f)
     - [get(t) + offset(f)] becomes get(t + offset(f))
     - [get(t)[i] ] becomes [get (t + i)]
       
     Note: [t + i] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_array_access, [t; i])))]
           [t + offset(f)] is represented in OptiTrust as [Trm_apps (Trm_val (Val_prim (Prim_struct_access "f")),[ŧ])] *)
let rec caddress_elim (t : trm) : trm =
  let aux t = caddress_elim t in (* recursive calls for rvalues *)
  let mk ?(annot = []) td = {t with desc = td; annot = annot} in
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

(* [is_access t]: check if trm [t] is a struct access or an array access *)
let is_access (t : trm) : bool =
  match t.desc with
  | Trm_apps (tprim, _) ->
    begin match trm_prim_inv tprim with
    | Some (Prim_unop (Unop_struct_access _) |  (Prim_binop (Binop_array_access))) -> true
    | _ -> false
    end
  | _ -> false

(* [caddress_intro_aux false t] is the inverse of [caddress_elim], hence if applies the following changes:
    [get(t + offset(f))] becomes [get(t).f]
    
    *)

let rec caddress_intro_aux (is_access_t : bool) (t : trm) : trm =
  let aux t = caddress_intro_aux false t in  (* recursive calls for rvalues *)
  let access t = caddress_intro_aux true t in (* recursive calls for lvalues *)
  let mk td = {t with desc = td} in
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

(* [cseq_items_void_type t] updates [t] in such a way that all instructions appearing in sequences
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

(* [iinfix_elim t] updates [t] special encodings for compound_assign operations, set operations and postfix  operations
    [x++] becomes ++(&x)
    x = y becomes =(&x, y)
    x += y becomes +=(&x,y)*)
let infix_elim (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
     {t with desc = Trm_apps(op, [trm_address_of tl; tr])}
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop unop)); _} as op, [base]) when is_postfix_unary unop ->
      {t with desc = Trm_apps(op, [trm_address_of base])}
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set))} as op, [tl; tr]) ->
      {t with desc = Trm_apps (op, [trm_address_of tl;tr])}
    | _ -> trm_map aux t
  in aux t

(* [infix_intro t] updates [t] clean special encodings for compound_assign operations, set operations and postfix unary operations
    [++(&x)] becomes [++x]
    [+=(&x, y)] becomes [x += y]
    [=(&x, y)] becomes [x = y]*)
let infix_intro (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_compound_assgn_op binop))} as op, [tl; tr]) ->
     {t with desc = Trm_apps(op, [trm_get tl; tr])}
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop unop)); _} as op, [base]) when is_postfix_unary unop ->
      {t with desc = Trm_apps(op, [trm_get base])}
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set))} as op, [tl; tr]) ->
      {t with desc = Trm_apps (op, [trm_get tl;tr])}
    | _ -> trm_map aux t
  in aux t

(* Main entry points *)

(* [cfeatures_elim t] converts a raw ast as produced by a C parser into an ast with OptiTrust semantics.
   It assumes [t] to be a full program or a right value. *)
let cfeatures_elim (t : trm) : trm =
  cseq_items_void_type (caddress_elim (stackvar_elim (infix_elim t)))

(* [cfeatures_intro t] converts an OptiTrust ast into a raw C that can be pretty-printed in C syntax *)
let cfeatures_intro (t : trm) : trm =
  infix_intro (stackvar_intro (caddress_intro t))

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
