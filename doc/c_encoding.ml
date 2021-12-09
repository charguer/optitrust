(*
  during clang_to_cRawAst
  -   e->f  should be encoded as  (*e).f,  with an annotation "display_arrow" on that struct_get term
  -   functions without body are encoded with body "trm_uninitialized".
  -   trm_let should have the flag is_var_mutable set according to whether "const" appears in the type
*)


(* file cRawAst_to_ast *)

type env = varkind Var_map

let env_empty =
  Var_map.empty

let is_var_mutable (env : env) (x : var) : varkind =
  match Var_map.find_opt env x with
  | Some m -> m
  | None -> failwith "unbound variable"

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

let trm_address_of ?(simplify:bool=false) (t:trm) : trm =
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

   (* LATER: we'll have to handle:   int a = 3; const int& b = a;
      but for now we simply don't support references on constants *)
   (* LATER: one day we'll have to deal with references in function arguments *)

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

             (* LATER: technically, we should extend the environment
               to include f, as this f might shadow another more,
               but let's leave that for later..
             | Trm_let_fun (retty, f, targs, tbody) -> *)

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
    (* shortcut, does not seem needed
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
             (* LATER: handle trm_fun *)
            | _ -> (envi, (aux env ti))
            in
          (envi2, ti2::acc) in
        let _,ts2 = List.fold_left process_item (env,[]) ts in
        { t with desc = Trm_seq (List.rev ts2) }
    | _ -> trm_map (aux env) t
    in
  aux env_empty


(* wip
...

let rec encode (lvalue : bool) (t : trm) : trm =
  let mk td = { t with desc = td }
  if not lvalue then begin
    match t.desc with
    | Trm_var x ->
        if is_var_mutable env x
          then trm_get (Trm_var x)
          else mk (Trm_var x)
    | Trm_apps (Prim_array_set, [t1; t2]) ->
        mk (Trm_apps (Prim_array_set, [access t1; aux t2])
    | Trm_apps (Prim_address_of, [t1]) ->
        access t1
    | Trm_apps (Prim_struct_get f, [t1])) ->
        let u1 = aux t1 in
        begin match u1.desc with
        | Trm_apps (Prim_get, [u11]) ->
            mk (Trm_apps (Prim_get, [Trm_apps (Prim_struct_access f, [u11])]))
        | mk (Trm_apps (Prim_struct_get f, [u1]))
        end
    | Trm_apps (Prim_array_get, [t1; t2]) ->
        let u1 = aux t1 in
        let u2 = aux t2 in
        begin match u1.desc with
        | Trm_apps (Prim_get, [u11]) ->
            mk (Trm_apps (Prim_get, [Trm_apps (Prim_array_access, [u11; u12])]))
        | _ -> mk (Trm_apps (Prim_array_get, [u1; u2]))
        end
    | Trm_apps (f, ts) ->
        mk (Trm_apps ((aux f), (List.map aux ts)))


  end else begin (* lvalue *)
    match t.desc with
    | Trm_var x when is_var_mutable env x ->
        mk (Trm_var x)
    | Trm_apps (Prim_struct_get f, [t1]) ->
        mk (Trm_apps (Prim_struct_access f, [access t1]))
    | Trm_apps (Prim_array_get, [t1; t2])) ->
        mk (Trm_apps (Prim_array_access, [access t1; aux t2]))
    | Trm_apps (Prim_get t1) ->
        aux t1
    | _ -> failwith "invalid lvalue"
  end

  *)




let encode (t : trm) : trm =
  access_intro (stackvar_elim t)
  (* Note: in the unit tests, we could check that
     stackvar_elim (access_intro t) produces the same result *)


let decode (t : trm) : trm =
  stackvar_intro (access_elim t)
  (* Note: in the unit tests, we could check that
     access_elim (stackvar_intro t) produces the same result *)



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