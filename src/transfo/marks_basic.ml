open Ast
open Target

(* [add m tg]: adds mark [m] to the trm that correpsonds to target [tg].
   NOTE: if m = "" then does nothing. *)
let%transfo add (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  if m = "" then () else Target.apply_on_targets (Marks_core.add m) tg

(* [add_between m]: adds mark [m] at the location of the relative target [tg].
   NOTE: if m = "" then does nothing. *)
let%transfo add_between (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  if m = "" then ()
    else
      Target.apply_on_targets_between(
        fun t (p,i) ->
         Target.apply_on_path (fun t -> trm_add_mark_between i m t) t p) tg

(* [remove m tg]: removes mark m from the trm that corresponds to target [tg]. *)
let%transfo remove (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply_on_targets (Marks_core.remove m) tg

let%transfo remove_between (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply (fun t p ->
    let (p_seq, _) = Path.last_dir_before_inv_success p in
    Path.apply_on_path (trm_rem_mark_between m) t p_seq) tg

(* [clean tg]: cleans all the marks from the trm that corresponds to target [tg]. *)
let%transfo clean (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply_on_targets (Marks_core.clean) tg