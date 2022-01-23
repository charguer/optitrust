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
  | _ -> Var_immutable (* For functions that come from an external library are set to immutable by default *)

(* [is_var_mutable env x] check if variable [x] is mutable or not *)
let is_var_mutable (env : env) (x : var) : bool =
  get_varkind env x = Var_mutable

(* [env_extend env e varkind] add variable [e] into environment [env] *)
let env_extend (env : env) (e : var) (varkind : varkind) : env =
  String_map.add e varkind env

(* [add_var env x xm] add variable [x] into environemnt [env] with value [xm] *)
let add_var (env : env ref) (x : var) (xm : varkind) : unit = 
  env := env_extend !env x xm

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

(* [onscope env t f] save the current environment before entering a new scope,
    the revert back to the saved env after leaving the scope.*)
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

   For references, [int& b = a] becomes [<annotation:reference> int* b = a] as a simplification of [b = &*a]
   and [int& x = t[i]] becomes [<annotation:reference> int* x = &(t[i])] if t has type [const int*].
   Here, the "reference" annotation is added to allow decoding.
   LATER: Support references on constants. *)
let stackvar_elim (t : trm) : trm =
  let env = create_env () in 
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_var (_, x) ->
      if is_var_mutable !env x
        then trm_annot_add Mutable_var_get (trm_get t)
        else { t with desc = Trm_var (Var_immutable, x) }
    | Trm_let (xm, (x, ty), tbody) ->
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
      add_var env f Var_immutable;
      onscope env t (fun t -> List.iter (fun (x, _tx) ->
       let mut = Var_immutable in (* if is_typ_ptr tx then Var_mutable else Var_immutable in *)
       add_var env x mut) targs; trm_map aux t)
    | Trm_for (index, _, _, _, _, _) ->
        onscope env t (fun t -> add_var env index Var_immutable; trm_map aux t)
    | Trm_for_c _ ->
        onscope env t (fun t -> trm_map aux t)
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
  let env = create_env () in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_var (_, x) ->
      if is_var_mutable !env x
        then trm_address_of t
        else t
    | Trm_let (vk, (x, tx), tbody) ->
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
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));_}, [{desc = Trm_var _; _} as t1]) when trm_annot_has Mutable_var_get t -> t1
    | _ -> trm_map aux t
    in aux t

(* [caddress_elim_aux false t] eliminates the use of l-values in the AST    
    [* t1]  becomes [get(t1)]
    [* t1 = t2] becomes as [set(t1, t2)]
    [t[i] = t[i] + 1] is encoded as [set(t+i, get(t+i) + 1)]
    [t.f = t.f + 1] is encoded as [set(t+offset(f), get(t + offse                                         t(f)) + 1)]
    [( * p).f.x] is encoded as [get(p+offset(f)) + offset(x))]
    [( * p).f.x = 3] is encoded as [set(p+offset(f) + offset(x), 3)]
    [( * p) [ i ] ]  is encoded as [get(p+i)]
    [t+i] is represented in optitrust as [Trm_apps (Prim_array_acces, [t;i])] in the AST
    [t+offset(f)] is represented in optitrust as [Trm_apps (Prim_struct_access "f", [t])]

    This transformation is implemented as [caddress_elim_aux lvalue t], where the
    boolean [lvalue] indicates whether we are currently translating a l-value
    or a normal instruction or expression (r-value).
*)
let rec caddress_elim_aux (lvalue : bool) (t : trm) : trm =
  let aux t = caddress_elim_aux false t in
  let access t = caddress_elim_aux true t in
  let mk ?(annot = []) td = {t with desc = td; annot = annot} in
  if lvalue then begin
    match t.desc with
    (* TODO: would be nice to document in one line of readable text what each case corresponds to.
       For example, the first case corresponds to [t.f] translated to [access(aux t, f)] *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)));_} as op, [t2]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [access t2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get));_} as op, [t1; t2]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [access t1; aux t2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op, [t1]) ->
      if List.mem Mutable_var_get t.annot
        then aux t1
        else mk (Trm_apps (op, [aux t1]))
    | Trm_var (_, x) -> fail t.loc (Printf.sprintf "caddress_elim: const variable cannot appear as lvalue (mutation of function arguments is not supported in OptiTrust), %s" x)
    | _ -> fail t.loc (Printf.sprintf "caddress_elim: invalid lvalue, %s\n------------\n%s\n" (Ast_to_rawC.ast_to_string t) (Ast_to_text.ast_to_string t))
    end
    else begin
         match t.desc with (* TODO: a few comments here would be nice too *)
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set));_} as op, [t1; t2]) ->
            mk (Trm_apps (op, [access t1; aux t2]))
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f))); _} as op, [t1]) ->
            let u1 = aux t1 in
            begin match u1.desc with
            | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op1, [u11]) ->
              (* struct_get (get(t1), f) is encoded as get(struct_access(t1,f))
                 in terms of C syntax: ( * t).f is compiled into * (t + offset(f)) *)
              mk ~annot:u1.annot (Trm_apps (op1, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f)))}, [u11]))]))
            | _ -> mk (Trm_apps (op, [u1]))
            end
         | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_get))); _} as op, [t1; t2]) ->
            let u1 = aux t1 in
            let u2 = aux t2 in
            begin match u1.desc with
            | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _} as op1, [u11]) ->
              mk ~annot:u1.annot (Trm_apps (op1, [mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_access)))}, [u11; u2]))]))
            | _ -> mk (Trm_apps (op, [u1;u2]))
            end
         | _ -> trm_map aux t
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
  let aux t = caddress_intro_aux false t in
  let access t = caddress_intro_aux true t in
  let mk td = {t with desc = td} in
  if lvalue then begin
    match t.desc with (* TODO: a few comments *)
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_access f))); _} as op, [t1]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get f)))}, [access t1]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop (Binop_array_access))); _} as op, [t1; t2]) ->
      mk (Trm_apps ({op with desc = Trm_val (Val_prim (Prim_binop (Binop_array_get)))}, [access t1; aux t2]))
    | Trm_var _ -> Ast.trm_get ~annot:[Mutable_var_get] t
    | _ -> trm_map aux t
    end
    else begin
    match t.desc with
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _}, [t1; t2]) ->
      mk (Trm_apps (trm_binop Binop_set, [access t1; aux t2]))
    | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [t1]) when is_access t1 ->
      access  t1
    | _ -> trm_map aux t
    end

let caddress_intro = caddress_intro_aux false

let cfeatures_elim (t : trm) : trm =
  caddress_elim (stackvar_elim t)

let cfeatures_intro (t : trm) : trm =
  stackvar_intro (caddress_intro t)
