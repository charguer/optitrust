open Ast
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



(*==============TODO : new*)

(* [new_add_on label t]: adds label [label] to trm [t]
      [label] - label that is going to label trm [t]
      [t] - a trm *)
let new_add_on (label : string) (t : trm) : trm =
  trm_add_label label t

(* [new_add_at label t p]: applies [add_aux] at the trm [t] with path [p] *)
let new_add_at (label : string) : Target.Transfo.local =
  Target.apply_on_path (new_add_on label)

(* [add label tg]: adds a C-label named [label] to the front of the terms
   matching the target [tg].

   @correctness: always correct. *)
let new_add (label : string) : Target.Transfo.t =
  Target.apply (new_add_at label)
  (*
  Target.apply_at_target_paths (new_add_on label)
  *)




(* [add label tg]: adds a C-label named [label] to the front of the terms
   matching the target [tg].

   @correctness: always correct. *)
let add (label : string) : Target.Transfo.t =
  Target.apply_on_targets (Label_core.add label)

(* [remove label tg]: removes a C-label named [label] matched by th target [tg]. *)
let remove : Target.Transfo.t =
  Target.apply_on_targets (Label_core.remove)

(* [remove_multiple tgs]: removes a list  of C-labels. *)
let remove_multiple (tgs : target list) =
  List.fold_left (fun () x-> remove x )() tgs
