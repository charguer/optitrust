open Ast

exception Rule_mismatch
let new_rule_match ~higher_order_inst(*:bool*) (vars : typed_vars) (pat : trm) (t : trm) : tmap =

  (* [inst] maps each pattern variable to a term and to a type;
     when pattern variables are not yet instantiated,
     they are bound to the special term trm_uninitialized. *)
     (* LATER: we may need one day to introduce another special term Trm_uninstantiated  *)
  let inst = ref (List.fold_left (fun acc (x,ty) -> Trm_map.add x (ty, Ast.trm_uninitialized()) acc) Trm_map.empty vars) in
  let is_var (x : var) : bool =
    Trm_map.mem x !inst in
  let find_var (x : var) (u : trm) : unit =
    match Trm_map.find_opt x !inst with
    | None -> failwith "failure in rule_match: called find_var without first checking is_var"
    | Some (ty,t0) ->
        if Ast.is_trm_uninitialized t0 then
          inst := Trm_map.add x (ty,u) !inst
        else if not (Internal.same_trm t0 u) then begin
          Tools.printf "Mismatch on variable '%s' already bound to '%s' which is not identical to '%s'" x (Ast_to_c.ast_to_string t0) (Ast_to_c.ast_to_string u);
          raise Rule_mismatch
        end
    in
  let get_binding (x : var) : (var * typ) option =
    match Trm_map.find_opt x !inst with
    | None -> None
    | Some (ty,t0) -> match t0.desc with
       | Trm_var y -> Some (y,ty)
       | _ -> None
    in
  let with_binding (ty : typ) (x : var) (y : var) (f : unit -> unit) : unit =
     inst := Trm_map.add x (ty, trm_var y) !inst;
     f();
     inst := Trm_map.remove x !inst;
    (* Note: it would be incorrect to simply restore the map to its value before the call to [f],
       because other variables than [x] may have been instantiated during the call to [f]. *)
     in


  let rec aux (t1 : trm) (t2 : trm) : unit =
    let mismatch ?(t1:trm=t1) ?(t2:trm=t2) () : unit =
      Tools.printf "Comparing %s with %s" (Ast_to_c.ast_to_string t1) (Ast_to_c.ast_to_string t2);
      raise Rule_mismatch
      in
    let aux_list (ts1 : trms) (ts2 : trms) : unit =
      List.iter2 aux ts1 ts2 in
    (* [aux_with_bindings] is a function for matching two lists of terms,
       making sure to extend the [inst] map to take into account the
       variable names that are bound locally; for example,
         [int a = 3; return a]  should match  [int b = 3; return b]
      thus we need to introduce a binding from [a] to [b] using [with_binding]. *)
    let rec aux_with_bindings (ts1 : trms) (ts2 : trms) : unit =
      match ts1, ts2 with
      | [], [] -> ()
      | ({ desc = Trm_let (vk1, (x1,t1), init1); _ } as dt1) :: tr1,
        ({ desc = Trm_let (vk2, (x2,_t2), init2); _ } as dt2) :: tr2 ->
          if not (vk1 = vk2) then begin
            Tools.printf "Kind mismatch on trm_let\n";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end;
          (* TODO: find out why this test is making the fbody unit test fail
           if not (same_types t1 t2) then begin
            Tools.printf "Type mismatch on trm_let\n";
            mismatch ~t1:dt1 ~t2:dt2 ()
          end; *)
          aux init1 init2;
          with_binding t1 x1 x2 (fun () -> aux_with_bindings tr1 tr2)
      (* LATER: add support for Trm_let_fun, to allow matching local function definitions. *)
      | t1 :: tr1, t2 :: tr2 ->
          aux t1 t2;
          aux_with_bindings tr1 tr2
      | _ -> mismatch() (* note: in general, this should have been tested earlier on by comparing lengths *)
      in

    match t1.desc, t2.desc with

    (* Case for treating a match against a pattern variable *)
    | Trm_var x, _ when is_var x -> find_var x t2

    (* Case for treating a match against a pattern such as [body(i)],
       where [body] is a pattern variable that corresponds to a function. *)
    | Trm_apps ({ desc = Trm_var x; _}, ts1), _ when higher_order_inst && is_var x ->
        let msg1 i ti = fail None (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not a variable. It is the term: %s" i x (Ast_to_text.ast_to_string ti)) in
        let xargs = List.mapi (fun i ti -> match ti.desc with
          | Trm_var x
          (* LATER: find out if it is really correct to igore the get operation here *)
          | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get)); _}, [{desc = Trm_var x; _}]) -> x
          | _ -> msg1 i ti) ts1 in
        let msg2 i = fail None (Printf.sprintf "rule_match: the %d-th argument of the higher-order function variable %s is not found in the instantiation map" i x) in
        let targs = List.mapi (fun i xi -> match get_binding xi with Some typed_yi -> typed_yi | None -> msg2 i) xargs in
        let body = t2 in
        let func = trm_let_fun x (typ_unit()) targs body in
        find_var x func
        (* LATER: it would be equivalent, but slightly nicer, to use the types coming from the function type associated with x,
           rather that to take the local types associated with the variables provided as arguments to x. *)

    | Trm_var x1, Trm_var x2 when x1 = x2 -> ()

    | Trm_val v1, Trm_val v2 when Internal.same_val v1 v2 -> ()

    | Trm_for (index1, start1, _direction1, stop1, step1, body1),
      Trm_for (index2, start2, _direction2, stop2, step2, body2) ->
        aux start1 start2;
        aux stop1 stop2;
        begin match step1, step2 with
        | Step stept1, Step stept2 -> aux stept1 stept2
        | _ -> if step1 <> step2 then mismatch()
        end;
        with_binding (typ_int()) index1 index2 (fun () -> aux body1 body2)

    | Trm_for_c (init1, cond1, step1, body1), Trm_for_c (init2, cond2, step2, body2) ->
        aux_with_bindings [init1; cond1; step1; body1] [init2; cond2; step2; body2]

    | Trm_seq tl1, Trm_seq tl2 ->
        if Mlist.length tl1 <> Mlist.length tl2 then mismatch();
        aux_with_bindings (Mlist.to_list tl1) (Mlist.to_list tl2)

    | Trm_apps (f1, ts1), Trm_apps (f2, ts2) ->
        aux f1 f2;
        aux_list ts1 ts2;

    | _ -> mismatch()
    in
  aux pat t;
  Trm_map.map (fun (_ty,t) -> t) !inst
