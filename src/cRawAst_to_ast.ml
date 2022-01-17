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
  | _ -> fail None (Printf.sprintf "get_varkind: unbound variable %s\n" x)

(* [is_var_mutable env x] check if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_varkind env x = Var_mutable

(* [env_extend env e varkind] add variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  String_map.add e varkind env

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
  if not simplify then  aux t
    else match t.desc with
    | Trm_apps (f, [t1]) ->
      begin match trm_prim_inv f with
      | Some (Prim_unop Unop_address) -> t1
      | _ -> aux t
      end
    | _ -> aux t





(* [stackvar_elim t] replaces
    [int a = 5] with [<annotation:stackvar> int* a = new int(5)]
      and a variable occurrence [a] becomes [ * a]
   The transformation leaves [const int c = 5] unchanged.

   For references, [int& b = a] becomes [<annotation:reference> int* b = a] as a simplification of [b = &*a]
   and [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [const int*].
   Here, the "reference" annotation is added to allow decoding.
   LATER: Support references on constants
*)

let stackvar_elim (t : trm) : trm =
  let env = ref env_empty in
  let onscope (t : trm) (f : trm -> trm ) : trm = 
    let prev_env = !env in 
    let res = f t in 
    env := prev_env;
    res
    in 
  let onscope_extended (x : var) (mut : varkind) (t : trm) (f : trm -> trm) : trm = 
    onscope t (fun t -> begin env := env_extend !env x mut; f t end)
    in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_var (_, x) ->
      if is_var_mutable !env x then trm_annot_add Mutable_var_get (trm_get t)
      else {t with desc = Trm_var (Var_immutable, x)}
    | Trm_let (xm, (x, ty), tbody) ->
      env := env_extend !env x xm;
      begin match ty.typ_desc with
      | Typ_ptr {ptr_kind = Ptr_kind_ref; inner_typ = ty1} ->
        begin match xm with
        | Var_immutable -> fail t.loc "stackvar_elim: unsupported references on const variables"
        | _ ->
          {t with desc = Trm_let (xm, (x, typ_ptr_generated ty1), trm_address_of ~simplify:true (aux tbody)); annot = Reference :: t.annot}
        end
      | _ ->
        begin match xm with
        | Var_mutable ->
          {t with desc = Trm_let (xm, (x, typ_ptr_generated ty), trm_new ty (aux tbody) ); annot = Stackvar :: t.annot}
        | Var_immutable ->
          trm_map aux t
        end
      end
    | Trm_seq _ -> onscope t (trm_map aux)
    | Trm_let_fun (f, retty, targs, tbody) ->
      env := env_extend !env f Var_immutable;
      List.iter (fun (x, _) -> (env := env_extend !env x Var_immutable)) targs;
      {t with desc = Trm_let_fun (f , retty, targs, aux tbody)}
    | Trm_for (index, _, _, _, _, _) ->
      onscope_extended index Var_mutable t (trm_map aux)
      
    | _ -> trm_map aux t
   in aux t

(* [stackvar_intro t] is the reciprocal to [stackvar_elim]. It replaces [<annotation:stackvar> int *a = new int(5)] with [int a = 5]
    and a variable occurrence [*a] becomes [a] if it corresponds to a stack variable
    (as a simplification to [*(&a)])
    For references, [<annotation:reference> int* b = a] becomes [int& b = a],
      as a simplification of b = *(&a), where &a is obtained after translating a.
    and [<annotation:reference> int* x = &t[i]] becomes [int& x = t[i]], where t has type [const int*] as a simplification of x = *(&t[i])
*)
let stackvar_intro (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    (* TODO: have env with onscope function
     | Trm_apps (Prim_get, [Trm_var x as t1])
       when List.mem Mutable_var_get t.annot ->
        if not is_var_mutable env x then error "x was declared immutable, but appears inside an annotated get operation";
        ...  *)
    | Trm_var (vk, _x) ->
      begin match vk with
      | Var_mutable -> trm_address_of t
      | Var_immutable -> t
      end
    | Trm_let (vk, (x, tx), tbody) ->
      if List.mem Stackvar t.annot
        then
          begin match tx.typ_desc , tbody.desc with
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1}, Trm_apps ({desc = Trm_val (Val_prim (Prim_new _));_}, [tbody1])  ->
              trm_annot_filter (function | Stackvar -> false | _ -> true) {t with desc = Trm_let (vk, (x, tx1), aux tbody1)}
          | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Stackvar on this declaration"
          end
        else if List.mem Reference t.annot then
          begin match tx.typ_desc with
          | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx1} ->
            trm_annot_filter (function | Reference -> false | _ -> true) { t with desc = Trm_let (vk, (x,typ_ptr Ptr_kind_ref tx1), trm_get ~simplify:true (aux tbody))}
          | _ -> failwith "stackvar_intro: not the expected form for a stackvar, should first remove the annotation Reference on this declaration"
          end
        else
          {t with desc = Trm_let (vk, (x, tx), aux tbody)}
    | Trm_apps (_, [t1]) when List.mem Mutable_var_get t.annot -> t1
    | _ -> trm_map aux t
    in aux t

(* [caddress_elim false t] eliminates the use of l-values in the AST
    [* t1]  becomes [get(t1)]
    [* t1 = t2] becomes as [set(t1, t2)]
    [t[i] = t[i] + 1] is encoded as [set(t+i, get(t+i) + 1)]
    [t.f = t.f + 1] is encoded as [set(t+offset(f), get(t + offset(f)) + 1)]
    [( * p).f.x] is encoded as [get(p+offset(f)) + offset(x))]
    [( * p).f.x = 3] is encoded as [set(p+offset(f) + offset(x), 3)]
    [( * p) [ i ] ]  is encoded as [get(p+i)]
    [t+i] is represented in optitrust as [Trm_apps (Prim_array_acces, [t;i])] in the AST
    [t+offset(f)] is represented in optitrust as [Trm_apps (Prim_struct_access "f", [t])]

    This transformation is implemented as [caddress_elim_lvalue t], where the
    boolean [lvalue] indicates whether we are currentyl translating a l-value
    or a normal instruction or expression (r-value).
*)
let rec caddress_elim (lvalue : bool) (t : trm) : trm =
  let aux t = caddress_elim false t in
  let access t = caddress_elim true t in
  let mk td = {t with desc = td} in
  if lvalue then begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as t1, [t2]) ->
      mk (Trm_apps (trm_unop  ~annot:t1.annot (Unop_struct_access f), [access t2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get));_} as op, [t1; t2]) ->
      mk (Trm_apps (trm_binop ~loc:op.loc Binop_array_access, [access t1; aux t2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) ->
      aux t1
    | _  -> t
    (* | _ -> fail t.loc (Printf.sprintf "caddress_elim: invalid lvalue: %s\n" (Ast_to_text.ast_to_string t)) *)
    end
    else begin
         match t.desc with
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set));_}, [t1; t2]) ->
            trm_set (access t1) (aux t2)
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_address));_}, [t1]) ->
            access t1
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f))); _} as op, [t1]) ->
            let u1 = aux t1 in
            begin match u1.desc with
            | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [u11]) ->
              (* struct_get (get(t1), f) is encoded as get(struct_access(t1,f))
                 in terms of C syntax: ( * t).f is compiled into * (t + offset(f)) *)
              mk (Trm_apps (trm_unop Unop_get, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [u11]))]))
            | _ -> mk (Trm_apps (op, [u1]))
            end
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get))); _}, [t1; t2]) ->
            let u1 = aux t1 in
            let u2 = aux t2 in
            begin match u1.desc with
            | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [u11]) ->
              mk (Trm_apps (trm_unop Unop_get, [mk (Trm_apps (trm_binop Binop_array_access, [u11; u2]))]))
            | _ -> mk (Trm_apps (trm_binop (Binop_array_get), [u1; u2]))
            end
         | _ -> trm_map aux t
         end


(* [is_access t] check if trm t is a struct access or an array access *)
let is_access (t : trm) : bool =
  match t.desc with
  | Trm_apps (tprim, _) ->
    begin match trm_prim_inv tprim with
    | Some (Prim_unop (Unop_struct_access _) |  (Prim_binop (Binop_array_access))) -> true
    | _ -> false
    end
  | _ -> false

(* TODO: rename
  address_elim to address_elim_aux
  and
  let address_elim = address_elim_aux false *)

(* [caddress_intro false t ] is the inverse of [caddress_elim]

    [get(t1)  becomes ][* t1]
    [set(t1, t2)] becomes as [* t1 = t2]
    [Trm_apps (Prim_struct_access "f", [t])] becomes [t.f] as lvalue
    [get(Trm_apps (Prim_struct_access "f", [t]))] becomes [t.f] as rlvalue
 *)
let rec caddress_intro (lvalue : bool) (t : trm) : trm =
  let aux t = caddress_intro false t in
  let access t = caddress_intro true t in
  let mk td = {t with desc = td} in
  if lvalue then begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f))); _} as op, [t1]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [access t1]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _} as op, [t1; t2]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)))}, [access t1; aux t2]))
    | _ ->
      trm_get ~simplify:true (aux t)
    end
    else begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) when is_access t1 ->
      access t1
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _}, [t1; t2]) ->
      mk (Trm_apps (trm_binop Binop_set, [t1; t2]))
    | _ when is_access t ->
      trm_address_of ~simplify:true (access t)
    | _ -> trm_map aux t
    end

(* TODO: Use unit tests to chack that
  caddress_intro (caddress_elim t) = t and stackvar_intro (stackvar_elim t) = t

  proof:
    forall b, caddress_intro b (caddress_elim b t) = t

  assume:
    &*t = t
    *&t = t
  writing:
    p+i = array_access(p,i)
    p+f = get_access(p,i)

 *)
let encode (t : trm) : trm = (* TODO: rename cfeatures_elim *)
  caddress_elim false (stackvar_elim t)


let decode (t : trm) : trm =
  caddress_intro false (stackvar_intro t)

(* Note: in the unit tests, we could check that caddress_intro (stackvar_intro t) produces the same result  *)

(* TODO: Check decode (encode t) = t *)


(* unit test:
  int main () {
    // invalid code
    // const int x = 4
    // int& y = x;

    // valid code:
    int a = 3;
    int const &b = a;
    a = 4;
    printf("%d %d\n", a, b); // prints 4 4

   return 0;
  }

 *)

(*
https://en.cppreference.com/w/cpp/language/reference
*)
