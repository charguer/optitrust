open Syntax
open Target

(* [transform ~reparse f_get f_set tg]: expects the target [tg] to point at a trm inside a set or a get operation.
    Then the transformation will search for the first get or set operation surrounding the targeted trm and call
    the transform core transformation on that trm. If the first founded operation was a get operation then [f_get]
    will be applied on the node represented by target [tg]. If it was a set operation then [f_set] will be applied
    on the second argument of the targeted node. *)
let%transfo transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) (tg: target) : unit =
  Target.apply_on_targets (fun t p -> let get_or_set_path = Internal.get_ascendant_path (fun t ->
    (is_get_operation t) || (is_set_operation t)) p t in
    if get_or_set_path = []
      then t
      else Accesses_core.transform f_get f_set t get_or_set_path
  ) tg

(* [scale ~inv ~factor tg]: this transformation just calls the [transform] function  with [f_get] and [f_set] args
   defined as a multiplication and a division operation respectively. If [inv] is set to true then these two
   operations will be swapped. *)
let%transfo scale ?(inv:bool=false) ~factor:(factor:trm) (tg : target) : unit =
  let op_get, op_set = if inv then (Binop_mul, Binop_div) else (Binop_div, Binop_mul) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform f_get f_set tg

(* [shift ~inv ~factor tg]: this transformation just calls the [transform] function with [f_get] and [f_set] args
   defined as a multiplication and a division respectively. If [inv] is set to true then these two operations
   will be swapped. *)
let%transfo shift ?(inv:bool=false) ~factor:(factor : trm) (tg : target) : unit =
  let op_get, op_set = if inv then (Binop_add, Binop_sub) else (Binop_sub, Binop_add) in
  let f_get t = Arith_core.apply_aux op_get factor t in
  let f_set t = Arith_core.apply_aux op_set factor t in
  transform f_get f_set tg

(* [intro tg]: expects the target [tg] to be pointing at any node that could contain struct accesses, preferably
   a sequence, then it will transform all the encodings of the form struct_get (get (t), f) to
   get (struct_access (t, f)) . *)
let%transfo intro (tg : target) : unit =
  Trace.justif_always_correct ();
  Target.apply_on_targets (Accesses_core.intro) tg
