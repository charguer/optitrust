open Prelude
include Accesses_basic

(** Like [scale], but targeting a variable declaration instead of a scope. *)
let scale_var = scale
(*
let%transfo scale_var ?(inv : bool = false) ~(factor : trm) ?(mark : mark = no_mark) (tg : target) : unit =
  Marks.with_marks (fun next_mark ->
    Target.iter (fun p ->
      let detached = ref false in
      let (i, p_seq) = Path.index_in_seq p in
      let mark_let = Marks.add_next_mark_on next_mark p in
      Variable.detach_if_needed ~detached (target_of_path p);
      let mark_preprocess = next_mark () in
      let mark_postprocess = next_mark () in
      scale ~inv ~factor ~mark ~mark_preprocess ~mark_postprocess [tSpan [tAfter; cMark mark_let] [tLast; cPath p_seq]];
      Instr.delete [nbAny; cMark mark_preprocess];
      Instr.delete [nbAny; cMark mark_postprocess];
      if !detached then Variable.init_attach [cMark mark_let];
    ) tg
  )
*)
