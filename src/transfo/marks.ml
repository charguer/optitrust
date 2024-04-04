include Marks_basic

open Prelude

let with_fresh_mark (f : mark -> unit) : unit =
  let m = Mark.next () in
  f m;
  remove m [nbAny; cMark m]

let with_fresh_mark_on (p : path) (f : mark -> unit) : unit =
  with_fresh_mark (fun m ->
    add m (target_of_path p);
    f(m)
  )

let with_marks (k : (unit -> mark) -> unit) : unit =
  let marks_to_remove = ref [] in
  let next () =
    let m = Mark.next () in
    marks_to_remove := m :: !marks_to_remove;
    m
  in
  k(next);
  (* TODO: Marks.remove_all, would be much more efficient --see clean_all? *)
  List.iter (fun m -> remove m [nbAny; cMark m]) !marks_to_remove

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
    Sequence.insert (trm_add_mark m (trm_seq Mlist.empty)) tg

(** [remove_fake_instr m tg]: remove a fake sequence item mark. *)
let%transfo remove_fake_instr (tg : target) : unit =
  Marks_basic.justif ();
  Target.iter (fun p ->
    let t = Target.resolve_path p in
    let error ="Marks.remove_fake_instr: should target a fake instr mark" in
    let t_seq = trm_inv ~error trm_seq_inv t in
    if Mlist.length t_seq <> 0 then failwith error;
    Sequence.delete (target_of_path p)
  ) tg

