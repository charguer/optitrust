open Prelude
open Target

(** [transform ~reparse f_get f_set tg]: expects the target [tg] to point at a trm inside a set or a get operation.
    Then the transformation will search for the first get or set operation surrounding the targeted trm and call
    the transform core transformation on that trm. If the first founded operation was a get operation then [f_get]
    will be applied on the node represented by target [tg]. If it was a set operation then [f_set] will be applied
    on the second argument of the targeted node. *)
let%transfo transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) (tg: target) : unit =
  Target.iter (fun p ->
    let get_or_set_path = Internal.get_ascendant_path (fun t -> (is_get_operation t) || (is_set_operation t)) p (Trace.ast ()) in
    if get_or_set_path <> [] then Target.apply_at_path (Accesses_core.transform_on f_get f_set) get_or_set_path
  ) tg

(** <private> *)
let transform_immut_on
  (f_init : trm -> trm) (f_use : trm -> trm)
  (i : int) (seq : trm) : trm =
  let instrs = trm_inv ~error:"expected seq" trm_seq_inv seq in
  let var = ref dummy_var in
  let update let_t =
    Pattern.pattern_match let_t [
      Pattern.(trm_let !__ !__ !__) (fun v typ init () ->
        var := v;
        trm_let ~annot:let_t.annot (v, typ) (f_init init)
      )
    ]
  in
  let rec fix t =
    let var = !var in
    Pattern.pattern_match t [
      Pattern.(trm_var (var_eq var)) (fun () ->
        f_use t
      );
      Pattern.__ (fun () ->
        trm_map fix t
      )
    ]
  in
  let instrs' = Mlist.update_at_index_and_fix_beyond i update fix instrs in
  trm_seq ~annot:seq.annot instrs'

(** like {!transform}, but for immutable variables.
    target should be variable decl. *)
let%transfo transform_immut (f_init : trm -> trm) (f_use : trm -> trm) (tg : target) : unit =
  Target.iter (fun p ->
    let (i, p_seq) = Path.index_in_seq p in
    Target.apply_at_path (transform_immut_on f_init f_use i) p_seq
  ) tg

(** [scale ~inv ~factor tg]: this transformation just calls the [transform] function  with [f_get] and [f_set] args
   defined as a multiplication and a division operation respectively. If [inv] is set to true then these two
   operations will be swapped. *)
let%transfo scale ?(inv:bool=false) ~(factor:trm) ?(mark : mark = no_mark) (tg : target) : unit =
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  (* TODO: Trace.justif "correct when factor is not 0 and is a float type" *)
  let op_get, op_set = if inv then (trm_mul, trm_div) else (trm_div, trm_mul) in
  let f_get t = trm_add_mark mark (op_get ~typ t factor) in
  let f_set t = trm_add_mark mark (op_set ~typ t factor) in
  transform f_get f_set tg

(* TODO: check / 0 *)
let%transfo scale_immut ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  (* TODO: Trace.justif "correct when not / 0"; *)
  let op_get, op_set = if inv then (trm_mul, trm_div) else (trm_div, trm_mul) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  transform_immut f_init f_use tg

(** [shift ~inv ~factor tg]: this transformation just calls the [transform] function with [f_get] and [f_set] args
   defined as a multiplication and a division respectively. If [inv] is set to true then these two operations
   will be swapped. *)
let%transfo shift ?(inv:bool=false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_add, trm_sub) else (trm_sub, trm_add) in
  let f_get t = trm_add_mark mark (op_get ~typ t factor) in
  let f_set t = trm_add_mark mark (op_set ~typ t factor) in
  transform f_get f_set tg

let%transfo shift_immut ?(inv:bool=false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  let typ = Option.unsome ~error:"Arith.scale: factor needs to have a known type" factor.typ in
  let op_get, op_set = if inv then (trm_add, trm_sub) else (trm_sub, trm_add) in
  let f_use t = trm_add_mark mark (op_get ~typ t factor) in
  let f_init t = trm_add_mark mark (op_set ~typ t factor) in
  transform_immut f_init f_use tg

(** [intro tg]: expects the target [tg] to be pointing at any node that could contain struct accesses, preferably
   a sequence, then it will transform all the encodings of the form struct_get (get (t), f) to
   get (struct_access (t, f)) . *)
let%transfo intro (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_at_target_paths (Accesses_core.intro_on) tg
