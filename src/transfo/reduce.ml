open Prelude
open Target

(* TODO: generalize this module to any monoid/group and reduction function. *)

let reduce_var = toplevel_var "reduce_spe1" (* FIXME: why is toplevel_var not working ? *)

(** [reduce_inv]: returns the list of arguments of a call to [reduce]. *)
let reduce_inv (t : trm) : (trm * trm * trm * trm * trm * trm) option =
  match trm_apps_inv t with
  | Some (f, [start; stop; input; n; m; j]) ->
    begin match trm_var_inv f with
    | Some v when var_eq v reduce_var ->
      Some (start, stop, input, n, m, j)
    | _ -> None
    end
  | _ -> None

(** <private> *)
let elim_basic_on (mark_alloc : mark) (mark_loop : mark) (t : trm) : trm =
  let error = "expected call to set" in
  let (out, red) = trm_inv ~error trm_set_inv t in
  let error = "expected call to reduce" in
  let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red in
  let acc = new_var "s" in
  let acc_typ = Option.get red.typ in
  let index = new_var "i" in
  let value = (trm_cast acc_typ (Matrix_trm.get input [n; m] [trm_var index; j])) in
  let loop_range = { index; start; direction = DirUp; stop; step = Post_inc } in
  trm_seq_nobrace_nomarks [
    trm_add_mark mark_alloc (trm_let_mut (acc, acc_typ) (trm_cast acc_typ (trm_int 0)));
    trm_add_mark mark_loop (trm_for loop_range (trm_seq_nobrace_nomarks [
      (* trm_prim_compound Binop_add (trm_var acc) value *)
      trm_set (trm_var acc) (trm_add (trm_var_get acc) value)
    ]));
    trm_set out (trm_var_get acc)
  ]

(** [elim_basic tg]: eliminates a call to [set(p, reduce)], expanding it to a for loop. *)
let%transfo elim_basic ?(mark_alloc : mark = no_mark) ?(mark_loop : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    Target.apply_at_path (elim_basic_on mark_alloc mark_loop) p
  ) tg)

(** <private> *)
let elim_inline_on (mark_simpl : mark) (t : trm) : trm =
  let error = "expected call to set" in
  let (out, red) = trm_inv ~error trm_set_inv t in
  let error = "expected call to reduce" in
  let (start, stop, input, n, m, j) = trm_inv ~error reduce_inv red in
  let nb_elems = Arith_core.(simplify true (fun e -> gather (compute e))) (trm_sub stop start) in
  match trm_int_inv nb_elems with
  | Some nb_elems ->
    let acc_typ = Option.get red.typ in
    if nb_elems = 0 then begin
      trm_set out (trm_cast acc_typ (trm_int 0))
    end else begin
      let values = List.init nb_elems (fun k ->
        let i = trm_add_mark mark_simpl (trm_add start (trm_int k)) in
        trm_cast acc_typ (Matrix_trm.get input [n; m] [i; j])
      ) in
      trm_set out (Xlist.reduce_left trm_add values)
    end
  | None ->
    trm_fail t "expected trivially constant loop range"

(** [elim_inline tg]: eliminates a call to [set(p, reduce)], expanding it to an inlined expression.
    TODO: later, implement this as combi (1. unroll; 2. inline accumulator; 3. simplify zero add)
    *)
let%transfo elim_inline ?(mark_simpl : mark = no_mark) (tg : target) =
  Nobrace_transfo.remove_after (fun () -> Target.iter (fun p ->
    Target.apply_at_path (elim_inline_on mark_simpl) p
  ) tg)

(** [elim_basic tg]: eliminates a call to [set(p, reduce)], expanding it to a for loop.

  - [unroll]: whether the reduction loop should be unrolled
  - [inline]: whether the reduction variable should be inlined (implies [unroll])
  *)
let%transfo elim ?(unroll : bool = false) ?(inline : bool = false) (tg : target) =
  Marks.with_marks (fun next_mark ->
    Target.iter (fun p ->
      if inline then begin
        let mark_simpl = next_mark () in
        elim_inline ~mark_simpl (target_of_path p);
        Arith.default_simpl [cMark mark_simpl];
      end else begin
        let mark_loop = next_mark () in
        elim_basic ~mark_loop (target_of_path p);
        if unroll then Loop.unroll [cMark mark_loop];
      end
    ) tg
  )
