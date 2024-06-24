open Prelude
open Target

(* LATER: ARTHUR: find out whether other languages consider label
   as instructions, that is, items of sequences, and if we should
   follow that view in optitrust. If so, Label_basic.add could take as
   argument a target_between (e.g. Label_basic.add_between).
   Example:  Label_basic.add_between [cAfter; cWhile()]
   to mean add a Label_basic after a loop.
   It could be the case that OptiTrust features too kinds of labels
   that is   Trm_label of string * trm option
   - label as a standalone instruction (=> currently encoded as Trm_label ("foo", trm_unit))
   - or label around an instruction. *)

(** [add label tg]: adds a C-label named [label] to the front of the terms
   matching the target [tg].
   Does nothing if [label = no_label].

   @correctness: always correct. *)
let%transfo add (label : string) (tg:target) : unit =
  Trace.justif_always_correct();
  if label = no_label then () else
  Target.apply_at_target_paths (trm_add_label label) tg

(** [remove label tg]: removes a C-label named [label] matched by th target [tg]. *)
let%transfo remove (tg: target) : unit =
  Target.apply_at_target_paths trm_rem_labels tg
