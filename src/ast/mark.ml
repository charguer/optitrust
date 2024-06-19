open Ast
open Trm

(** [next_int]: generates an integer *)
let next_int : unit -> int =
  Tools.fresh_generator()

(** [next]: creates a mark based on [next_int] generator *)
let next : unit -> Ast.mark =
  fun () -> "__" ^ string_of_int (next_int()) (* a prefix for generated marks *)

(** [reuse_or_next next m] returns [next m] if [m = ""], [m] otherwise. *)
let reuse_or_next (next : unit -> mark) (m : mark) : mark =
  if m = "" then next () else m

let span_marks (m: mark) : mark * mark =
  (m ^ "__begin", m ^ "__end")

(**** Marks  ****)

(** [apply_on_marks f t]: applies [f] on the marks of [t]. *)
let apply_on_marks (f : marks -> marks) (t : trm) : trm =
  let t_annot_marks = f (t.annot.trm_annot_marks) in
  let t_annot = {t.annot with trm_annot_marks=t_annot_marks} in
  trm_alter ~annot:t_annot t

(** [trm_add_mark m] adds mark [m] to the trm [t].
   Returns [t] unchanged if [m = ""]. *)
let trm_add_mark (m : mark) (t : trm) : trm =
  if m = no_mark then t else apply_on_marks (fun marks -> m :: marks) t

(** [trm_add_mark_between index m t] adds mark [m] at [index] in the mlist of [t], where [t] should be a sequence.
   Returns [t] unchanged if [m = ""].
  *)
let trm_add_mark_between (index : int) (m : mark) (t : trm) : trm =
  if m = "" then t else
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.insert_mark_at index m tl in
    trm_seq ~annot:t.annot ?loc:t.loc ~ctx:t.ctx new_tl
  | _ -> trm_fail t "Ast.trm_add_mark_between: expected a sequence"

let trm_add_mark_span ({ start; stop }: Dir.span) (m: mark) (t: trm): trm =
  let (m_begin, m_end) = span_marks m in
  let t = trm_add_mark_between start m_begin t in
  let t = trm_add_mark_between stop m_end t in
  t

(** [trm_filter_mark m t]: keeps only marks that satisfy the predicate [pred].
   Only operates on marks on the term, does not handle mark-between in sequences. *)
let trm_filter_mark (pred : mark -> bool) (t : trm): trm =
  apply_on_marks (fun marks -> List.filter pred marks) t

(** [trm_rem_mark m t]: removes mark [m] from trm [t]. *)
let trm_rem_mark (m : mark) (t : trm) : trm =
  trm_filter_mark (fun m1 -> m <> m1) t

(** [trm_remove_marks pred t]: removes all the marks from trm [t]
   that satisfy the predicate [pred]. Including mark-between in sequences. *)
let trm_remove_marks (pred: mark->bool) (t : trm) : trm =
  let res =
  match t.desc with
  (* In the case of sequences, special treatment is needed for in between marks*)
  | Trm_seq tl -> trm_replace (Trm_seq (Mlist.filter_marks (fun m -> not (pred m)) tl)) t
  | _ -> t in
  trm_filter_mark (fun m -> not (pred m)) res

(** [trm_rem_mark_between m t]: removes the between mark [m] from trm [t] *)
let trm_rem_mark_between (m : mark) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let new_tl = Mlist.remove_mark m tl in
    trm_seq ~annot:t.annot ~ctx:t.ctx ?loc:t.loc new_tl
  | _ -> trm_fail t "Ast.trm_rem_mark_between: expected a sequence"

(** [trm_rem_mark_between m t]: removes the span mark [m] from trm [t] *)
let trm_rem_mark_span (m: mark) (t: trm): trm =
  let (m_begin, m_end) = span_marks m in
  let t = trm_rem_mark_between m_begin t in
  let t = trm_rem_mark_between m_end t in
  t

(** [trm_has_mark m t]: checks if trm [t] has mark [m].
   Returns [false] if [m = no_mark]. *)
let trm_has_mark (m : mark) (t : trm) : bool =
  (m <> no_mark) && List.mem m t.annot.trm_annot_marks

(** [trm_add_mark_is_noop m t]: checks if trm [t] has mark [m].
   Returns [true] if [m = no_mark]. *)
let trm_add_mark_is_noop (m : mark) (t : trm) : bool =
  (m = no_mark) || List.mem m t.annot.trm_annot_marks

(** [trm_get_marks t]: returns all the marks of [t]. *)
let trm_get_marks (t : trm) : marks =
  t.annot.trm_annot_marks

(** [trm_pass_marks t1 t2]: passes the marks of trm [t1] to trm [t2]. *)
let trm_pass_marks (t1 : trm) (t2 : trm) : trm =
  let t1_marks = trm_get_marks t1 in
  let t2_marks = trm_get_marks t2 in
  let t2_annot = {t2.annot with trm_annot_marks = t2_marks @ t1_marks} in
  trm_alter ~annot:t2_annot t2


(** [get_mark_index m t]: for relative targets marks are stored on the parent sequence, this function give the index
   that mark m targets to *)
let get_mark_index (m : mark) (t : trm) : int option =
  match t.desc with
  | Trm_seq tl ->
    Xlist.fold_lefti (fun i acc ml ->
      match acc with
      | Some _ -> acc
      | None ->
        if List.mem m ml then Some i else None
    ) None (Mlist.get_marks tl)
  | _ -> trm_fail t "Ast.get_mark_index: expected a sequence trm"
