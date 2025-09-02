open Prelude
include Accesses_basic

type transform_typ = address_pattern: compiled_pattern -> ?mark_to_prove : mark -> ?mark_preprocess: mark -> ?mark_postprocess: mark -> target -> unit

let transform_var (transform : transform_typ) (tg : target) : unit =
Marks.with_marks (fun next_mark ->
  Target.iter (fun p ->
    let detached_ret = ref false in
    let typed_var_ret = ref (dummy_var, typ_auto) in
    let (i, p_seq) = Path.index_in_seq p in
    let mark_let = Marks.add_next_mark_on next_mark p in
    Variable.detach_if_needed ~detached_ret ~typed_var_ret (target_of_path p);
    let mark_to_prove = next_mark () in
    let mark_preprocess = next_mark () in
    let mark_postprocess = next_mark () in
    let (var, typ) = !typed_var_ret in
    if Option.is_none (typ_ptr_inv typ) then
      failwith "expected mutable variable, consider calling `_immut` version or making this combi smarted";
    let address_pattern = pattern_compile (trm_var ~typ var) in
    transform ~address_pattern ~mark_to_prove ~mark_preprocess ~mark_postprocess [cPath p_seq; tSpan [tAfter; cMark mark_let] [tLast]];
    Instr.move ~dest:[tBefore; cMark mark_let] [tSpan [cMark mark_to_prove] [cMarkSpanStart mark_preprocess]];
    Instr.delete [nbAny; cMarkSpan mark_preprocess];
    Instr.delete [nbAny; cMarkSpan mark_postprocess];
    if !detached_ret then Variable.init_attach [cMark mark_let];
  ) tg
)

(** Like [transform_arith], but targeting a variable declaration instead of a scope. *)
let%transfo transform_arith_var ~(op:transform_arith_op) ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  transform_var (transform_arith ~op ~inv ~factor ~mark) tg

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
