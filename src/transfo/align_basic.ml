open Prelude
open Target


(* [def vec_align tg]: expects the target [tg] to point at an array declaration, then it will add the alignas attribute
    to its type with [vec_align] alignment size. *)
let def (vec_align : trm) : Target.Transfo.t =
  Target.apply_on_targets(Align_core.def vec_align)


(* [header ()]: inserts "#include \"stdalign.h\"" at the top of the main file. *)
let header () =
  Sequence_basic.insert (stmt "#include \"stdalign.h\"") [tFirst; dRoot]

(* [assume alignment var tg]: expects the target [tg] to be a relative target, then it will insert an instruction of the
    form var = __builtin_assume_aligned(var, alignment) at that location.*)
let assume (alignment : trm) (var : var) : Transfo.t =
  let call_to_insert = trm_apps (trm_toplevel_var "__builtin_assume_aligned") [trm_var var; alignment] in
  Sequence_basic.insert call_to_insert
