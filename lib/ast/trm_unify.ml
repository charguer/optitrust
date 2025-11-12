open Trm
open Ast
open Matrix_trm
open Typ
open Contextualized_error

(* The value associated with an existential variable (evar).
   It is generic over the type of information stored when the value is unknown. *)
type 'a evar_resolution = Resolved of trm | Unknown of 'a

(* The unification context is a map from variables to evals.
   If a variable is not in the map, then it is not an evar, and should not be substituted/unified.
   If a variable is in the map, then it is an evar, and should be substituted/unified (i.e. it should eventually become Resolved). *)
type 'a unification_ctx = 'a evar_resolution varmap

(** [unfold_if_resolved_evar t evar_ctx] tries to unfold resolved evars inside
    [evar_ctx] occuring at the top of [t] (but not in depth). Since we can do it
    almost for free, it also performs simplifications in the [evar_ctx] when
    resolved evars are pointing to other resolved evars.

    It returns the possibly unfolded [trm], along with a possibly simplified
    [evar_ctx].

    On application nodes, this function tries to unfold the function and
    performs immediate beta reduction if possible. *)
let rec unfold_if_resolved_evar (t : trm) (evar_ctx : 'a unification_ctx) :
    trm * 'a unification_ctx =
  match t.desc with
  | Trm_var x -> (
      match Var_map.find_opt x evar_ctx with
      | Some (Resolved t) ->
          (* Avoid cycles in this function, this can help debugging *)
          let evar_ctx = Var_map.remove x evar_ctx in
          let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
          (* Path compression in case of cascading evars *)
          (t, Var_map.add x (Resolved t) evar_ctx)
      | _ -> (t, evar_ctx))
  (* Immediate beta redex replacement *)
  | Trm_apps (apps_fn, args, [], []) -> (
      (* LATER: Deal with ghost_args and ghost_bind ? *)
      let fn, evar_ctx = unfold_if_resolved_evar apps_fn evar_ctx in
      if fn == apps_fn then (t, evar_ctx)
      else
        match trm_fun_inv fn with
        | Some (params, ret, body, spec) ->
            let t =
              trm_subst
                (List.fold_left2
                   (fun subst_ctx arge (param, _) ->
                     Var_map.add param arge subst_ctx)
                   Var_map.empty args params)
                body
            in
            (t, evar_ctx)
        | None -> (trm_apps fn args, evar_ctx))
  | _ -> (t, evar_ctx)

(** [normalize_trm t evar_ctx]: tries to normalize [t], aiming to match
    equivalent trms with different syntax Performs the following normalisations:
    - normalize mindex :
      &(&x[MINDEX_(dims_in,inds_in,0,..,0)])[MINDEX_(dims_out,inds_out)] -->
      &x[MINDEX_(dims_in,inds_in,inds_out)] If normalization is not possible,
      return the same [t] *)
let rec normalize_trm (t : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst :
      trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    trm * 'a unification_ctx =
  let open Option.Monad in
  let aux t evar_ctx = normalize_trm  t evar_ctx validate_inst in
  let t, evar_ctx = unfold_if_resolved_evar t evar_ctx in
  let nochange = (t, evar_ctx) in
  match Matrix_trm.access_inv t with
  | Some (base_out, dims_out, inds_out) -> (
      let base_out, evar_ctx = aux base_out evar_ctx in
      match Matrix_trm.access_inv base_out with
      | Some (base_in, dims_in, inds_in) ->
          let n_dims_out = List.length dims_out in
          if List.length dims_in >= n_dims_out then
            let last_dims_in = List.take_last n_dims_out dims_in in
            let zeros = List.init n_dims_out (fun i -> trm_int 0) in
            let left_list = last_dims_in @ List.take_last n_dims_out inds_in in
            let right_list = dims_out @ zeros in
            let same_trms =
                List.fold_left2
                  (fun b arg arge ->
                     Option.is_some (trm_unify arg arge Var_map.empty (fun _ _ ctx -> Some ctx)) )
                  true left_list right_list

            in
            if same_trms then let indices = List.drop_last n_dims_out inds_in @ inds_out in
                (Matrix_trm.access base_in dims_in indices, evar_ctx)
              else nochange
          else nochange
      | _ -> nochange)
  | _ -> nochange

(** [trm_unify t1 t2 evar_ctx validate_inst] tries to unify [t1] with [t2],
    possibly instantiating and substituting evars that occur in [evar_ctx]. If
    the unification succeeds, returns an updated unification context, with the
    newly resolved evars. If it fails, returns None. For each potential
    unification, this function calls [validate_inst t info evar_ctx] that can
    itself perform chained unifications. *)
and trm_unify (t_left : trm) (t_right : trm)
    (evar_ctx : 'a unification_ctx)
    (validate_inst :
      trm -> 'a -> 'a unification_ctx -> 'a unification_ctx option) :
    'a unification_ctx option =
  let open Option.Monad in
  (* Pattern match on one component to get a warning if there is a missing one *)
  let check cond = if cond then Some evar_ctx else None in
  (* on-the-fly normalisation, unfold_if_resolved_vars is now included in normalize  *)
  let t_left, evar_ctx = normalize_trm t_left evar_ctx validate_inst in
  let t_right, evar_ctx = normalize_trm t_right evar_ctx validate_inst in
  let validate_and_subst evar t_subst evar_ctx =
    match Var_map.find evar evar_ctx with
    | Unknown info ->
        let* evar_ctx = validate_inst t_subst info evar_ctx in
        Some (Var_map.add evar (Resolved t_subst) evar_ctx)
    | Resolved _ ->
        failwith "Resolved evars should have been substituted before"
  in
  let unify_with_apps f args t_other =
    let* xf = trm_var_inv f in
    let* () = if Var_map.mem xf evar_ctx then Some () else None in
    let* subst_map, evar_ctx, rev_new_fun_args =
      List.fold_left
        (fun acc arg ->
          let* subst_map, evar_ctx, vars = acc in
          let arg, evar_ctx = unfold_if_resolved_evar arg evar_ctx in
          let* arg_var = trm_var_inv arg in
          let new_arg_var = new_var arg_var.name in
          let subst_map =
            Var_map.add arg_var (trm_var ?typ:arg.typ new_arg_var) subst_map
          in
          Some (subst_map, evar_ctx, (new_arg_var, typ_or_auto arg.typ) :: vars))
        (Some (Var_map.empty, evar_ctx, []))
        args
    in
    let new_fun_args = List.rev rev_new_fun_args in
    let evar_ctx = ref evar_ctx in
    let new_body =
      trm_map_vars
        (fun () (annot, loc, typ, _ctx) var ->
          let var_t = trm_var ~annot ?loc ?typ var in
          let resolution, new_evar_ctx =
            unfold_if_resolved_evar var_t !evar_ctx
          in
          evar_ctx := new_evar_ctx;
          trm_subst subst_map resolution)
        () t_other
    in
    let inst = trm_fun new_fun_args (typ_or_auto t_other.typ) new_body in
    validate_and_subst xf inst !evar_ctx
  in
  let res =
    match (t_left.desc, t_right.desc) with
    (* -- FIXME: hole hack *)
    | _, Trm_var h when String.starts_with ~prefix:"__hole" h.name ->
      Some evar_ctx
    | Trm_var h, _ when String.starts_with ~prefix:"__hole" h.name ->
      Some evar_ctx
    (* -- *)
    | Trm_var x_left, Trm_var x_right when var_eq x_left x_right ->
        Some evar_ctx
    | Trm_var x_left, _ when Var_map.mem x_left evar_ctx ->
        validate_and_subst x_left t_right evar_ctx
    | _, Trm_var x_right when Var_map.mem x_right evar_ctx ->
        validate_and_subst x_right t_left evar_ctx
    | Trm_apps (f, args, [], []), Trm_apps (fe, argse, [], []) ->
        let res_opt =
          (* LATER: Manage functions with ghost_args and ghost_bind *)
          let* evar_ctx = trm_unify f fe evar_ctx validate_inst in
          try
            List.fold_left2
              (fun evar_ctx arg arge ->
                let* evar_ctx = evar_ctx in
                trm_unify arg arge evar_ctx validate_inst)
              (Some evar_ctx) args argse
          with Invalid_argument _ -> None
          (* todo: better to check lengths first *)
        in
        let res_opt =
          Option.or_else res_opt (fun () -> unify_with_apps f args t_right)
        in
        let res_opt =
          Option.or_else res_opt (fun () -> unify_with_apps fe argse t_left)
        in
        res_opt
    | Trm_apps (f_left, args_left, [], []), _ ->
        unify_with_apps f_left args_left t_right
    | _, Trm_apps (f_right, args_right, [], []) ->
        unify_with_apps f_right args_right t_left
    | _, Trm_var _ ->
        (* t_right is a variable but it is not the same as t_left, and none is an unresolved evar *)
        None
    | _, Trm_lit le -> (
        let* l = trm_lit_inv t_left in
        match le with
        | Lit_unit | Lit_bool _ | Lit_string _ -> check (l = le)
        | Lit_int (te, ve) ->
            let* t, v =
              match l with Lit_int (t, v) -> Some (t, v) | _ -> None
            in
            let* _ = check (v = ve) in
            trm_unify  t te evar_ctx validate_inst
        | Lit_float (te, ve) ->
            let* t, v =
              match l with Lit_float (t, v) -> Some (t, v) | _ -> None
            in
            let* _ = check (v = ve) in
            trm_unify  t te evar_ctx validate_inst
        | Lit_null te ->
            let* t = match l with Lit_null t -> Some t | _ -> None in
            trm_unify  t te evar_ctx validate_inst)
    | _, Trm_prim (tye, pe) ->
        let* ty, p = trm_prim_inv t_left in
        (* FIXME: This can fail because primitives may recursively contain types and terms *)
        if pe = p then trm_unify  ty tye evar_ctx validate_inst
        else None
    | _, Trm_fun (argse, _, bodye, _) ->
        let* args, _, body, _ = trm_fun_inv t_left in
        (* Remember the masked context in case of shadowing, this is needed in case of recursive
        or higher order functions with evars. *)
        let* evar_ctx, masked_ctx =
          try
            Some
              (List.fold_left2
                 (fun (evar_ctx, masked) (arge, _) (arg, argty) ->
                   if var_eq arge arg then
                     (* This case is required to handle comparison of a trm with itself *)
                     (evar_ctx, masked)
                   else
                     let masked_entry = Var_map.find_opt arge evar_ctx in
                     let evar_ctx =
                       Var_map.add arge
                         (Resolved (trm_var ~typ:argty arg))
                         evar_ctx
                     in
                     (evar_ctx, (arge, masked_entry) :: masked))
                 (evar_ctx, []) argse args)
          with Invalid_argument _ -> None
        in
        let* evar_ctx =
          trm_unify  body bodye evar_ctx validate_inst
        in
        Some
          (List.fold_left
             (fun evar_ctx (arge, masked_entry) ->
               match masked_entry with
               | Some entry -> Var_map.add arge entry evar_ctx
               | None -> Var_map.remove arge evar_ctx)
             evar_ctx masked_ctx)
    | _, Trm_arbitrary _ ->
        failwith
          "trm_unify: found Trm_arbitrary during unification (a reparse is \
           missing)"
    | _, _ ->
        failwith "trm_unify: unhandled constructor (%s)"
          (Ast_to_text.ast_to_string t_right)
    (* TODO: Implement the rest of constructors *)
  in
  if Option.is_none res then ();
  res

(** [are_same_trm t1 t2] checks that [t1] and [t2] are alpha-equivalent (same
    modulo name of the binders). *)
let are_same_trm (t1 : trm) (t2 : trm) : bool =
  (* they are the same if they can be unified without allowing substitutions. *)
  Option.is_some (trm_unify t1 t2 Var_map.empty (fun _ _ ctx -> Some ctx))
