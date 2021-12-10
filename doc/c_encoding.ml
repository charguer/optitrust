(*
  during clang_to_cRawAst
  -   e->f  should be encoded as  (*e).f,  with an annotation "display_arrow" on that struct_get term
  -   functions without body are encoded with body "trm_uninitialized".
  -   trm_let should have the flag is_var_mutable set according to whether "const" appears in the type

  - LATER:
      a reference should be considered as "mutable_var" not depending on whether its type is "const",
      but depending on whether its initialization value (right-hand-side) is of type "const".
      For example, in [int a = 3; const int& b = a;], the reference "b" should be treated as
      "mutable reference". (the "const" only means that this reference is used only for read access
      to a mutable cell; it does not mean here that we can treat the cell as a pure value.)
  - LATER:
      we may want to insert "const" type explicitly on the type of the function arguments;
      for example f(int* p) should becomes f(int* const p)
*)


(* file cRawAst_to_ast *)

type env = varkind Var_map

let env_empty =
  Var_map.empty

let get_var_kind  (env : env) (x : var) : varkind =
  match Var_map.find_opt env x with
  | Some m -> m
  | None -> failwith "unbound variable"

let is_var_mutable (env : env) (x : var) : bool =
  get_var_kind env x = Var_mutable

let env_extend (env : env) (e : var) (varkind : varkind) : env =
  Var_map.add env e varkind


(* [trm_address_of t] returns the C term [&t];
   if ~simplify is true and [t] is of the form [*u], it returns just [u]. *)

let trm_address_of ?(simplify:bool=false) (t:trm) : trm =
  let aux t1 = Trm_apps (Prim_address_of, [t1]) in
   if not simplify
     then aux t
     else match t.desc with
         | Trm_apps (Prim_get, [t1]) -> t1 (* LATER: maybe we should keep some of the annotations from t? *)
         | _ -> aux t

(* [trm_get t] returns the C term [*t];
   if ~simplify is true and [t] is of the form [&u], it returns just [u]. *)

let trm_get ?(simplify:bool=false) (t:trm) : trm =
  let aux t1 = Trm_apps (Prim_get, [t1]) in
   if not simplify
     then aux t
     else match t.desc with
         | Trm_apps (Prim_address_of, [t1]) -> t1 (* LATER: maybe we should keep some of the annotations from t? *)
         | _ -> aux t


(* [stackvar_elim t]  replaces
     [int a = 5] with [<annotation:stackvar>int* a = new int(5)]
      and a variable occurence [a] becomes [*a].
   The transformation leaves [const int c = 5] unchanged.

   For references, [int& b = a] becomes [<annotation:reference> int* b = a], as a simplification of (b = &*a)
    and [int& x = t[i]] becomes [<annotation:reference> int* x = &t[i]] if t has type [const int*].
    Here, the "reference" annotation is added to allow decoding.  *)
   (* LATER: for now we simply don't support references on constants *)

let rec stackvar_elim (t : trm) : trm =
  let rec aux (env : env) (t : trm) : trm =
    match t.desc with
    | Trm_var x ->
        if is_var_mutable env x
          then trm_get t
          else t
    | Trm_seq ts ->
        (* LATER: see if we can factorize the pattern of applying a "trm_map with env" on a trm_seq *)
        let process_item (envi,acc) (ti : trm) : env * trm list =
          let (envi2, ti2) =
            match ti.desc with
            | Trm_let (is_x_mutable, ty, x, tbody) ->
                let ti2 =
                  match ty.desc with
                  | Typ_reference ty1 ->
                      if not is_x_mutable then failwith "unsupported references on const";
                      { ti with desc = Trm_let (is_x_mutable, typ_star ty1, x, trm_address_of ~simplify:true (aux tbody);
                                annot = Annot_reference :: ti.annot }
                  | _ ->
                    if is_x_mutable
                      then { ti with desc = Trm_let (is_x_mutable, typ_star ty, x, trm_new ty (aux tbody);
                                     annot = Annot_stackvar :: ti.annot }
                      else trm_map (aux envi) ti
                  in
                (env_extend envi x is_x_mutable, ti2)
             | Trm_let_fun (f, retty, targs, tbody) ->
                let envi2 = env_extend envi f Var_immutable in
                let envi2 = List.fold_left (fun envacc (x,ty) ->
                              env_extend envacc x Var_immutable) envi2 targs in
                (envi2, { ti with desc = Trm_let_fun (f, retty, targs, aux tbody) }
            | _ -> (envi, (aux env ti))
            in
          (envi2, ti2::acc) in
        let _,ts2 = List.fold_left process_item (env,[]) ts in
        { t with desc = Trm_seq (List.rev ts2) }
    | _ -> trm_map (aux env) t
    in
  aux env_empty


(* [stackvar_intro t] is the reciprocal to [stackvar_elim]. It [stackvar_elim t]  replaces
     [<annotation:stackvar> int* a = new int(5)] with [int a = 5] with
      and a variable occurence [*a] becomes [a] if it corresponds to a stack variable
      (as a simplification to [*(&a)])
   For references, [<annotation:reference> int* b = a] becomes [int& b = a],
      as simplification of  b = *(&a), where &a is obtained after translating a.
    and [<annotation:reference> int* x = &t[i]] becomes  [int& x = t[i]], where t has type [const int*]
     as simplification of  x = *(&t[i]) *)

let rec stackvar_intro (t : trm) : trm =
  let rec aux (env : env) (t : trm) : trm =
    match t.desc with
    (* Note: this shortcut does not seem needed:
       | Trm_apps (Prim_get, [Trm_var x as t1]) when is_var_mutable env x -> t1 *)
    | Trm_var x ->
        if is_var_mutable env x
          then trm_address_of t
          else t
    | Trm_seq ts ->
        let process_item (envi,acc) (ti : trm) : env * trm list =
          let (envi2, ti2) =
            match ti.desc with
            | Trm_let (is_x_mutable, ty, x, tbody) ->
                let ti2 =
                  if List.mem Annot_stackvar ti.annot then begin
                    match ty.desc, tbody.desc with
                    | Typ_star ty1, Trm_apps (Prim_new ty2, [tbody1]) ->
                        { ti with desc = Trm_let (is_x_mutable, ty1, x, (aux tbody1);
                                  annot = List.remove Annot_stackvar ti.annot }
                    | _ -> failwith "not the expected form for a stackvar, should first remove the annotation on this declaration"
                  end else if List.mem Annot_reference ti.annot then begin
                    match ty.desc with
                    | Typ_star ty1 ->
                        { ti with desc = Trm_let (is_x_mutable, typ_reference ty1, x, trm_get ~simplify:true (aux tbody1);
                                  annot List.remove Annot_reference ti.annot }
                    | _ -> failwith "not the expected form for a stackvar, should first remove the annotation on this declaration"
                  end else
                    trm_map (aux envi) ti
                  in
                (env_extend envi x is_x_mutable, ti2)
             | Trm_let_fun (f, retty, targs, tbody) ->
                let envi2 = env_extend envi f Var_immutable in
                let envi2 = List.fold_left (fun envacc (x,ty) ->
                              env_extend envacc x Var_immutable) envi2 targs in
                (envi2, { ti with desc = Trm_let_fun (f, retty, targs, aux tbody) }
            | _ -> (envi, (aux env ti))
            in
          (envi2, ti2::acc) in
        let _,ts2 = List.fold_left process_item (env,[]) ts in
        { t with desc = Trm_seq (List.rev ts2) }
    | _ -> trm_map (aux env) t
    in
  aux env_empty


(* [caddress_elim false t] eliminates the use of l-values in the AST.

   [*t1 = t2] is encoded as [set(t1, t2)].
   [t[i] = t[i] + 1]  is encoded as [set(t+i, get(t+i) + 1]
   [t.f = t.f + 1]   is encoded as [set(t+offset(f), get(t+offset(f)) + 1)].
   [(*p).f.x] is encoded as [get((p+offset(f))+offset(x))].

   [t+i] is represented as [Trm_apps (Prim_array_access, [t;i])] in the AST
   [t+offset(f)] is represented as [Trm_apps (Prim_struct_access "f", [t])].

   The transformation is implemented as [caddress_elim lvalue t], where the
   boolean [lvalue] indicates whether we are currently translating a l-value
   or a normal instruction or expression (r-value). *)

let rec caddress_elim (lvalue : bool) (t : trm) : trm =
  let aux ti = caddress_elim true ti in
  let access ti = caddress_elim false ti in
  let mk td = { t with desc = td }
  if lvalue then begin
    match t.desc with
    | Trm_apps (Prim_struct_get f, [t1]) ->
        mk (Trm_apps (Prim_struct_access f, [access t1]))
        (* TODO: if there is an annotation "Display_arrow" on the Prim_struct_get,
           we should copy this annotation onto the Prim_struct_access term.
           Note: we need to clarify whether the Display_arrow is on the Trm_apps
           or on the Prim_struct_get; I think the later would be easier to work with. *)
    | Trm_apps (Prim_array_get, [t1; t2])) ->
        mk (Trm_apps (Prim_array_access, [access t1; aux t2]))
    | Trm_apps (Prim_get t1) ->
        aux t1
    | _ -> failwith "invalid lvalue" (* TODO: for debugging, report location and print term *)
  end else begin (* rvalue *)
    match t.desc with
    | Trm_apps (Prim_array_set, [t1; t2]) ->
        mk (Trm_apps (Prim_array_set, [access t1; aux t2]))
    | Trm_apps (Prim_address_of, [t1]) ->
        access t1
    | Trm_apps (Prim_struct_get f, [t1])) ->
        let u1 = aux t1 in
        begin match u1.desc with
        | Trm_apps (Prim_get, [u11]) ->
            mk (Trm_apps (Prim_get, [Trm_apps (Prim_struct_access f, [u11])]))
        | mk (Trm_apps (Prim_struct_get f, [u1]))
        (* TODO: in the two cases above, if there is an annotation "Display_arrow"
            on the Prim_struct_get, we should copy this annotation onto the
            Prim_struct_access or the Prim_struct_get that we produce. *)
        end
    | Trm_apps (Prim_array_get, [t1; t2]) ->
        let u1 = aux t1 in
        let u2 = aux t2 in
        begin match u1.desc with
        | Trm_apps (Prim_get, [u11]) ->
            mk (Trm_apps (Prim_get, [Trm_apps (Prim_array_access, [u11; u2])]))
        | _ -> mk (Trm_apps (Prim_array_get, [u1; u2]))
        end
    | _ -> trm_map aux t
  end

(* [caddress_intro false t] is the inverse of [caddress_elim]. *)

let is_access (t : trm) : bool =
  match t with
  | Trm_apps (Prim_struct_access _, _)
  | Trm_apps (Prim_array_access, _)) -> true
  | _ -> false


let rec caddress_intro (lvalue : bool) (t : trm) : trm =
  let aux ti = caddress_intro true ti in
  let access ti = caddress_intro false ti in
  let mk td = { t with desc = td }
  if lvalue then begin
    match t.desc with
    | Trm_apps (Prim_struct_access f, [t1]) ->
        mk (Trm_apps (Prim_struct_get f), [access t1])
        (* TODO: we should preserve the annotation "Display_arrow" *)
    | Trm_apps (Prim_array_access, [t1; t2])) ->
        mk (Trm_apps (Prim_array_get, [access t1; aux t2]))
    | _ ->
        trm_get ~simplify:true (aux t)
  end else begin (* rvalue *)
    match t.desc with
    | _ when is_access t ->
        trm_address_of (access t)
    | Trm_apps (Prim_get, [t1]) when is_access t1 ->
        access t1
    | Trm_apps (Prim_array_set, [t1; t2]) ->
        mk (Trm_apps (Prim_array_set, [trm_get ~simplify:true (access t1); aux t2]))
    | _ -> trm_map aux t
  end


(* TODO: use unit tests to check that
    caddress_intro (caddress_elim t) = t
   and
    stackvar_intro (stackvar_elim t) = t
*)

let encode (t : trm) : trm =
  caddress_elim (stackvar_elim t)
  (* Note: in the unit tests, we could check that
     stackvar_elim (caddress_elim t) produces the same result *)


let decode (t : trm) : trm =
  stackvar_intro (caddress_intro t)
  (* Note: in the unit tests, we could check that
     caddress_intro (stackvar_intro t) produces the same result *)



(* unit test:

#include <cstdio>

int main() {
  // invalid code
  //    const int x = 4;
  //    int & y = x;

  // valid code:
  int a = 3;
  int const & b = a;
  a = 4;
  printf("%d %d\n", a, b); // prints 4 4
  return 0;
}
*)

(*
https://en.cppreference.com/w/cpp/language/reference
*)