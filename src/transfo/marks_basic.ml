open Prelude
open Target

(* [add m tg]: adds mark [m] to the term or interstice that correpsonds to target [tg].
   NOTE: if m = "" then does nothing. *)
let%transfo add (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  if m = "" then () else Target.iter (fun _ p ->
    match Path.last_dir_before_inv p with
    | Some (p_seq, i) -> Target.apply_at_path (trm_add_mark_between i m) p_seq
    | None -> Target.apply_at_path (trm_add_mark m) p
  ) tg

  (* DEPRECATED
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
 *)

(* [remove m tg]: removes mark m from the term or interstice that corresponds to target [tg]. *)
let%transfo remove (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.iter (fun _ p ->
    match Path.last_dir_before_inv p with
    | Some (p_seq, i) -> Target.apply_at_path (trm_rem_mark_between m) p_seq
    | None -> Target.apply_at_path (trm_rem_mark m) p
  ) tg

  (* DEPRECATED
let%transfo remove_between (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply (fun t p ->
    let (p_seq, _) = Path.last_dir_before_inv_success p in
    Path.apply_on_path (trm_rem_mark_between m) t p_seq) tg
*)

(* [clean tg]: cleans all the marks from the trm that corresponds to target [tg]. *)
let%transfo clean (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply_at_target_paths trm_remove_marks tg
