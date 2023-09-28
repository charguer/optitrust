include Marks_basic

open Target
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
  (* TODO: Marks.remove_all, would be much more efficient *)
  List.iter (fun m -> remove m [nbAny; cMark m]) !marks_to_remove