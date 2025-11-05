open Prelude
include Accesses_basic

type transform_typ = address_pattern: compiled_pattern -> ?mark_to_prove : mark -> ?mark_preprocess: mark -> ?mark_postprocess: mark -> target -> unit

(** Applies the [Accesses_basic.transform] on a set of targets guided by a variable:
    - if [tg] points to a variable definition (Trm_let), then the span for [transform]
      ranges from that definition until the end of the parent sequence.
    - if [tg] points to an array write operation (typically an initialization),
      the span ranges from that write until the end of the sequence.
      For arrays, the base of the array needs to be a string, passed via [?array_base];
      LATER: the array_base would be deduced from the target, ideally. *)
let transform_var (transform : transform_typ) ?(array_base : trm option) (tg : target) : unit =
Marks.with_marks (fun next_mark ->
  Target.iter (fun p ->
    let (_i, p_seq) = Path.index_in_seq p in

    let detached_ret = ref false in (* used only for variables, not for arrays *)

    let address_pattern = (* includes preprocessing for detaching let-bound variables *)
      match array_base with
      | Some base -> Trm.(array_access base (pattern_var "i")) (* LATER: a better name than i? does it matter?*)
      | None ->
        let typed_var_ret = ref (dummy_var, typ_auto) in
          Variable.detach_if_needed ~detached_ret ~typed_var_ret (target_of_path p);
          let (var, typ) = !typed_var_ret in
          if Option.is_none (typ_ptr_inv typ)
            then failwith "expected mutable variable, consider calling `_immut` version or making this combi smarted";
          trm_var ~typ var
      in
    let address_pattern = pattern_compile address_pattern in

    let mark_let = Marks.add_next_mark_on next_mark p in
    let mark_to_prove = next_mark () in
    let mark_preprocess = next_mark () in
    let mark_postprocess = next_mark () in
    transform ~address_pattern ~mark_to_prove ~mark_preprocess ~mark_postprocess [cPath p_seq; tSpan [(if array_base = None then tAfter else tBefore); cMark mark_let] [tLast]];
    if array_base = None then begin
      Instr.move ~dest:[tBefore; cMark mark_let] [tSpan [cMark mark_to_prove] [cMarkSpanStart mark_preprocess]];
      if !detached_ret then Variable.init_attach [cMark mark_let];
    end;
    Instr.delete [nbAny; cMarkSpan mark_preprocess];
    Instr.delete [nbAny; cMarkSpan mark_postprocess];
  ) tg
)

(** Like [transform_arith], but targeting a variable declaration instead of a scope. *)
let%transfo transform_arith_var ~(op:transform_arith_op) ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) ?(array_base : trm option) (tg : target) : unit =
  transform_var (Accesses_basic.transform_arith ~op ~inv ~factor ~mark) ?array_base tg

(* TODO %transfo *)
let scale_var = transform_arith_var ~op:Transform_arith_mul
let shift_var = transform_arith_var ~op:Transform_arith_add

let%transfo transform_arith ?(inv : bool = false) ~(op:transform_arith_op) ~(factor : trm)
 ~(address_pattern : pattern)
 ?(mark : mark = no_mark)
 ?(mark_preprocess : mark = no_mark) ?(mark_postprocess : mark = no_mark)
 ?(uninit_pre : bool = false) ?(uninit_post : bool = false)
 (tg : target) =
 (* TODO: generic transform to factorize with shift *)
  let user_mark_pre = mark_preprocess in
  let user_mark_post = mark_postprocess in
  Marks.with_marks (fun next_mark ->
    let mark_preprocess = next_mark () in
    let mark_postprocess = next_mark () in
    let address_pattern = pattern_compile address_pattern in
    Accesses_basic.transform_arith ~inv ~op ~factor ~address_pattern ~mark ~mark_preprocess ~mark_postprocess tg;
    if uninit_pre then Instr.delete [nbAny; cMarkSpan mark_preprocess];
    if uninit_post then Instr.delete [nbAny; cMarkSpan mark_postprocess];
    Marks.add_on_all_span user_mark_pre [nbAny; cMarkSpan mark_preprocess];
    Marks.add_on_all_span user_mark_post [nbAny; cMarkSpan mark_postprocess];
  )

(* TODO %transfo *)
let scale = transform_arith ~op:Transform_arith_mul
let shift = transform_arith ~op:Transform_arith_add
