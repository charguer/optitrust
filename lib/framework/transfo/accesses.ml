open Prelude
include Accesses_basic

type transform_typ = address_pattern: trm -> pattern_evars: eval varmap -> ?mark_preprocess: mark -> ?mark_postprocess: mark -> target -> unit

let transform_var (transform : transform_typ) (tg : target) : unit =
Marks.with_marks (fun next_mark ->
  Target.iter (fun p ->
    let detached_ret = ref false in
    let typed_var_ret = ref (dummy_var, typ_auto) in
    let (i, p_seq) = Path.index_in_seq p in
    let mark_let = Marks.add_next_mark_on next_mark p in
    Variable.detach_if_needed ~detached_ret ~typed_var_ret (target_of_path p);
    let mark_preprocess = next_mark () in
    let mark_postprocess = next_mark () in
    let (var, typ) = !typed_var_ret in
    let address_pattern = trm_var ~typ var in
    let pattern_evars = Var_map.empty in
    transform ~address_pattern ~pattern_evars ~mark_preprocess ~mark_postprocess [cPath p_seq; tSpan [tAfter; cMark mark_let] [tLast]];
    Instr.delete [nbAny; cMarkSpan mark_preprocess];
    Instr.delete [nbAny; cMarkSpan mark_postprocess];
    if !detached_ret then Variable.init_attach [cMark mark_let];
  ) tg
)

(** Like [scale], but targeting a variable declaration instead of a scope. *)
let%transfo scale_var ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  transform_var (scale ~inv ~factor ~mark) tg

(** Like [shift], but targeting a variable declaration instead of a scope. *)
let%transfo shift_var ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  transform_var (shift ~inv ~factor ~mark) tg
