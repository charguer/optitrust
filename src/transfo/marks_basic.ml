(* Don't load prelude to avoid circular dependencies
   because prelude loads show, and show uses marks *)
(* open Prelude *)
open Ast
open Trm
open Typ
open Target
open Mark

let justif () =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks"

(* [add m tg]: adds mark [m] to the term or interstice that correpsonds to target [tg].
   NOTE: if m = "" then does nothing. *)
let%transfo add (m : mark) (tg : target) : unit =
  justif();
  if m = "" then () else Trace.preserves_resource_typing (fun () ->
    Target.iter (fun p ->
    match Path.extract_last_dir p with
    | p_seq, Before i -> Target.apply_at_path (trm_add_mark_between i m) p_seq
    | p_seq, Span span -> Target.apply_at_path (trm_add_mark_span span m) p_seq
    | _ -> Target.apply_at_path (trm_add_mark m) p
  ) tg)

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

(*** [remove m tg]: removes mark m from the term or interstice that corresponds to target [tg]. *)
let%transfo remove (m : mark) (tg : target) : unit =
  justif();
  Trace.preserves_resource_typing (fun () -> Target.iter (fun p ->
    match Path.last_dir_before_inv p with
    | Some (p_seq, i) -> Target.apply_at_path (trm_rem_mark_between m) p_seq
    | None -> Target.apply_at_path (trm_rem_mark m) p
  ) tg)

  (* DEPRECATED
let%transfo remove_between (m : mark) (tg : target) : unit =
  Trace.justif_always_correct ();
  Trace.tag_trivial ();
  Trace.tag "marks";
  Target.apply (fun t p ->
    let (p_seq, _) = Path.extract_last_dir_before p in
    Path.apply_on_path (trm_rem_mark_between m) t p_seq) tg
*)

(** [remove_st pred tg]: cleans all the marks satisfying [pred] from the trm
   that corresponds to target [tg]. Use [~indepth:true] to recurse in subterms. *)
let%transfo remove_st ?(indepth:bool=false) (pred: mark->bool) (tg : target) : unit =
  justif();
  let rec aux (t:trm) : trm =
    trm_remove_marks pred (trm_map aux t) in
  Trace.preserves_resource_typing (fun () ->
  if indepth
    then Target.apply_at_target_paths aux tg
    else Target.apply_at_target_paths (trm_remove_marks pred) tg)

(* [clean tg]: removes all the marks form the term that is matched by target [tg].
   Use [~indepth:true] to recurse in subterms. *)
let clean ?(indepth:bool=false) (tg : target) : unit =
  remove_st ~indepth (fun _ -> true) tg
