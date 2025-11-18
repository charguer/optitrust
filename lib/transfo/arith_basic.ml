open Prelude
open Target
include Arith_core


(** [show tg] annotates the target with a string describing the reified
    expression that corresponds to the arithmetic term considered. *)
let%transfo show ?(normalized : bool = true) (tg : target) : unit =
  Target.apply_at_target_paths (Arith_core.show_expr ~normalized) tg

(** [remove_show tg] unannotates the target with a string describing the reified
    expression that corresponds to the arithmetic term considered. *)
let%transfo remove_show (tg : target) : unit =
  Target.apply_at_target_paths (fun t ->
    match trm_apps_inv t with
    | Some (f_arith, [t_arg; s_arg]) when trm_is_var ~var:(toplevel_var "__ARITH") f_arith ->
      t_arg
      (* LATER: alternative would be to fail if no __ARITH found *)
      (* LATER: use trm_apps_var_inv and trm_apps_toplevel_var_inv *)
    | _ -> t
  ) tg

let arith_simpl = toplevel_var "arith_simpl"

let ghost_arith_rewrite r1 r2 =
  Resource_trm.ghost_admitted_rewrite r1 r2 (trm_var arith_simpl)

(** [apply op arg] expects the target [tg] to be pointing at any node of the ast
      then it applies the binary operation [op] at that node with the second argument
      of that operation being [arg] *)
(*let%transfo apply (op : binary_op) (arg : trm) (tg : target) : unit =
  Target.apply_at_target_paths (Arith_core.apply op arg) tg
*)

(** [simpl f] applies a arithmetic rewriting method from the module Arith_core:
   - gather  for grouping and cancelling out similar expressions in sums and produts
   - expand  for expanding products involving sums. *)
let%transfo simpl ?(indepth : bool = false) (f: (expr -> expr)) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_simpl_arith ();
  Trace.without_resource_computation_between_steps (fun () ->
    Target.apply_at_target_paths (fun t ->
      let f_postprocess (t: trm) (simpl_t: trm): trm =
        let open Resource_formula in
        if t != simpl_t && !Flags.use_resources_with_models && not (is_formula t) then begin
          let typ = Option.unsome ~error:"expected type" t.typ in
          let res = Resources.after_trm t in
          begin match Var_map.find_opt Resource_set.var_result res.aliases with
          | Some t_model ->
            let new_result = new_hyp "res" in
            let result = new_var "arith_res" in
            (* FIXME: requires aliases in specs:
            let contract = {
                pre = Resource_set.make ~pure:[(new_result, typ)]
                  ~aliases:(Var_map.singleton result (trm_var new_result)) ();
                post = Resource_set.make ~aliases:(Var_map.singleton result t_model) ();
              } in *)
            let result_ptr = new_var "arith_res" in
            (* TODO upgrade to multiple mem types (#24) *)
            let contract = {
              pre = Resource_set.make ~pure:[(new_result, typ)]
                ~linear:[new_anon_hyp (), formula_points_to ~mem_typ:mem_typ_any (trm_var result_ptr) (trm_var new_result)] ();
              post = Resource_set.make ~linear:[new_anon_hyp (), formula_points_to ~mem_typ:mem_typ_any (trm_var result_ptr) t_model] ();
            } in
            let maintain_res = Resource_trm.ghost_admitted contract in
            (* trm_seq_nomarks ~result [
              trm_let (result, typ) simpl_t;
              maintain_res;
            ] *)
            trm_seq_nomarks ~result [
              trm_let_mut (result_ptr, typ) simpl_t;
              maintain_res;
              trm_let (result, typ) (trm_get (trm_var result_ptr));
            ]
          | None ->
            simpl_t
          end
        end else begin
          simpl_t
        end in
      Arith_core.simplify indepth f ~f_postprocess t
    ) tg
  )

(** [simpl2 f] applies a arithmetic rewriting method from the module Arith_core:
   - gather  for grouping and cancelling out similar expressions in sums and produts
   - expand  for expanding products involving sums. *)
let%transfo simpl2 ?(indepth : bool = false) (f: arith_transfo) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_simpl_arith ();
  Trace.without_resource_computation_between_steps (fun () ->
    Target.apply_at_target_paths (Arith_core.simplify2 indepth f) tg
  )

(** [simpl_rec f tg] just an alias for [simpl ~indepth:true f tg] *)
let%transfo simpl_rec (f : (expr -> expr)) (tg : target) : unit =
  Trace.tag_simpl_arith ();
  simpl ~indepth:true f tg

(** [simpl2_rec f tg] just an alias for [simpl2 ~indepth:true f tg] *)
let%transfo simpl2_rec (f : arith_transfo) (tg : target) : unit =
  Trace.tag_simpl_arith ();
  simpl2 ~indepth:true f tg

(** [compose fs] returns the function obtained as the composition
    of the functions [fs] *)
let compose (fs : (expr -> expr) list) : (expr -> expr) =
  fun (e:expr) ->
    let rec aux fs e =
      match fs with
      | [] -> e
      | f::fs' -> aux fs' (f e)
      in
    aux fs e

(** [simpls fs tg] is like simpl with the composition of the functions fs *)
let%transfo simpls ?(indepth : bool = false) (fs : (expr -> expr) list) (tg : target) : unit =
  simpl ~indepth (compose fs) tg

(** [simpls_rec f tg] just an alias for simpl ~indepth:true tg *)
let%transfo simpls_rec (fs : (expr -> expr) list) (tg : target) : unit =
  Trace.tag_simpl_arith ();
  simpls ~indepth:true fs tg

(** [simplify ~indepth tg] applies simpl with the operation being gathering of
    arithmetic experssions *)
let%transfo simplify ?(indepth : bool = false) (tg : target) : unit =
  simpl ~indepth Arith_core.gather_rec tg

(* alias cPrimArith *)
let constr =
  cPrimPredCall is_prim_arith

(** [clear_nosimpl tg]: clears all the marks on all the instructions that where
    skipped by the simplifier *)
let%transfo clear_nosimpl (tg : target) : unit =
  Marks.remove Arith_core.mark_nosimpl [nbMulti; cMark Arith_core.mark_nosimpl]

(** [nosimplf tg]: mark all the instructions targeted by [tg] as "__arith_core_nosimpl" *)
let%transfo nosimpl (tg : target) : unit =
  Marks.add Arith_core.mark_nosimpl tg

(* LATER: have a stack of different marks to avoid loosing the previously existing ones *)

(** [with_nosimpl tg f]: after marking all the nodes targeted by [tg] with mark "__arith_core_with_nosimpl", applies the
    transformation [f] on all the nodes matched  by [tg], after the transformation has been applied succesfully,
    it will clean all the introduced marks *)
let with_nosimpl (tg : target) (f : unit -> unit) : unit =
  nosimpl tg;
  f();
  clear_nosimpl tg
