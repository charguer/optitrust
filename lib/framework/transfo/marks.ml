include Marks_basic

open Prelude
open Tools

let remove_maybe_spans (ms : String_set.t) =
  if String_set.is_empty ms then ()
  else begin
    let ms = String_set.fold (fun m acc ->
      let m_begin, m_end = span_marks m in
      String_set.(acc |> add m_begin |> add m_end)
    ) ms ms in
    remove_st ~indepth:true (fun m -> String_set.mem m ms) []
  end

let with_fresh_mark (f : mark -> unit) : unit =
  let m = Mark.next () in
  f m;
  remove_maybe_spans (String_set.singleton m)

let with_fresh_mark_on (p : path) (f : mark -> unit) : unit =
  with_fresh_mark (fun m ->
    add m (target_of_path p);
    f(m)
  )

let with_marks (k : (unit -> mark) -> unit) : unit =
  let marks_to_remove = ref String_set.empty in
  let next () =
    let m = Mark.next () in
    marks_to_remove := String_set.add m !marks_to_remove;
    m
  in
  k(next);
  remove_maybe_spans !marks_to_remove

let add_next_mark (next_m : unit -> mark) (tg : target) : mark =
  let m = next_m () in
  add m tg;
  m

let add_next_mark_on (next_m : unit -> mark) (p : path) : mark =
  add_next_mark next_m (target_of_path p)


(** [add_fake_instr m tg]: adds a mark [m] at the location of the target [tg] as a fake sequence item.
   NOTE: if m = "" then does nothing. *)
let%transfo add_fake_instr (m : mark) (tg : target) : unit =
  Marks_basic.justif ();
  if m = "" then ()
  else
    Sequence_basic.insert (trm_add_mark m (trm_seq Mlist.empty)) tg

(** [remove_fake_instr m tg]: remove a fake sequence item mark. *)
let%transfo remove_fake_instr (tg : target) : unit =
  Marks_basic.justif ();
  Target.iter (fun p ->
    let t = Target.resolve_path p in
    let error ="Marks.remove_fake_instr: should target a fake instr mark" in
    let t_seq, result = trm_inv ~error trm_seq_inv t in
    if Mlist.length t_seq <> 0 || Option.is_some result then failwith "%s" error;
    Sequence_basic.delete (target_of_path p)
  ) tg

let%transfo add_on_all_span (mark : mark) (tg : target) : unit =
  Marks_basic.justif ();
  if mark = no_mark then ()
  else begin
    Target.iter (fun p ->
      let (p_seq, span) = Path.extract_last_dir_span p in
      Target.apply_at_path (fun t_seq ->
        update_span_helper span t_seq (fun instrs ->
          [TrmMlist (Mlist.map (trm_add_mark mark) instrs)]
        )
      ) p_seq
    ) tg
  end
