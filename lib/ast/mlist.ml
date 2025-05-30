type mark = string (* must be the same as Ast.mark *)

(** ['a t]: mlist is a record of two fields, items is the list of objects it contains
   and marks is the list of marks for each in-between location. *)
type 'a t =
  { items : 'a list;
    marks : (mark list) list  }

(* LATER:
  type marks = mark list
  type 'a t = (marks * 'a * marks) list
   *)

(** [length ml]: the number of items inside [ml]. *)
let length (ml : 'a t) : int =
  List.length ml.items

(** [of_list l]: creates a mlist from a list of objects of type 'a .*)
let of_list (l : 'a list) : 'a t =
  { items = l;
    marks = List.init (List.length l + 1) (fun _i -> [])}

(** [to_list ml]: extracts the items from mlist [ml]. *)
let to_list (ml : 'a t) : 'a list =
  ml.items

(** [get_marks ml]: return a list of marks inside mlist [ml] *)
let get_marks (ml: 'a t) : mark list list =
  ml.marks

(** [empty] : empty mlist. *)
let empty : 'a t =
  {items = [];
   marks = [[]]}

(*********************** List module equivalent functions *********************)
(** [mapi f ml]: applies List.mapi f to ml.items. *)
let mapi (f : int -> 'a -> 'b) (ml : 'a t) : 'b t =
  let new_items = List.mapi f ml.items in
  { ml with items = new_items }

(** [map f ml]: applies List.map f to ml.items. *)
let map (f : 'a -> 'b) (ml : 'a t) : 'b t =
  mapi (fun _i x -> f x) ml

(** [iteri f ml]: applies List.iteri f to ml.items. *)
let iteri (f : int -> 'a -> unit) (ml : 'a t) : unit =
  List.iteri f (to_list ml)

(** [iter f ml]: applies List.iter f to ml.items. *)
let iter (f : 'a -> unit) (ml : 'a t) : unit =
  iteri (fun _i x -> f x) ml

(** [find_map f ml]: applies List.find_map f to ml.items. *)
let find_map (f : 'a -> 'b option) (ml : 'a t) : 'b option =
  List.find_map f ml.items (* LATER: now in the List stdlib module *)

(** [fold_left acc_f acc ml]: applies List.fold_left to ml.items. *)
let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  List.fold_left acc_f acc ml.items

(** [nth ml index]: get the nth item from [ml]. *)
let nth (ml : 'a t) (index : int) : 'a =
  List.nth ml.items index

(** [nth_opt ml index]: get the nth item from [ml]. *)
let nth_opt (ml : 'a t) (index : int) : 'a option =
  if index >= 0 then List.nth_opt ml.items index else None

(** [fold_lefti acc_f acc ml]: applies List.fold_lefti to ml.items. *)
let fold_lefti (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  List.fold_lefti acc_f acc ml.items

let for_all p ml =
  List.for_all p (to_list ml)

(** [for_all2 p ml1 ml2]: applies List.for_all2 to ml1.items and ml2.items. *)
let for_all2 p ml1 ml2 =
  List.for_all2 p (to_list ml1) (to_list ml2)

(** [rev ml]: applies List.rev to ml.items and ml.marks. *)
let rev (ml : 'a t) : 'a t =
  { items = List.rev ml.items;
    marks = List.rev ml.marks }


(***********************************************************************)

(** [is_empty ml]: checks if [ml] is empty or not. *)
let is_empty (ml :'a t) : bool =
  length ml = 0

(** [replace_at index x ml]: replaces the item at [index] in [ml] with element [x].*)
let replace_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  { ml with items = List.update_nth index (fun _ -> x) ml.items }

(** [insert_mark_at index m ml]: inserts at [index] in ml mark [m]. *)
let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  { ml with marks = List.update_nth index (fun ms -> m :: ms) ml.marks}

(** [insert_marks_at index m ml]: inserts at [index] in ml marks [m]. *)
let insert_marks_at (index : int) (m : mark list) (ml : 'a t) : 'a t =
  { ml with marks = List.update_nth index (fun ms -> m @ ms) ml.marks}

(** [filter_marks f ml]: remove all marks that do not satisfy [f m]. *)
let filter_marks (f: mark -> bool) (ml: 'a t) : 'a t =
  let new_marks = List.map (fun ms -> List.filter f ms) ml.marks in
  { ml with marks = new_marks }

(** [remove_at index m ml] removes mark [m] in [ml]. *)
let remove_mark (m : mark) (ml : 'a t) : 'a t =
  filter_marks (fun x -> x <> m) ml

(** [remove_all_marks ml] removes all marks from [ml]. *)
let remove_all_marks (ml: 'a t): 'a t =
  of_list ml.items

(** [split ~left_bias index ml]: splits mlist [ml] at [index]
    [left_bias] - if true then the boundary marks will go with the first part of the mlist. *)
let split ?(left_bias : bool = true) (index : int) (ml : 'a t) : 'a t * 'a t =
  let items1, items2 = List.split_at index ml.items in
  let marks1a, marks2a = List.split_at (index + if left_bias then 1 else 0) ml.marks in
  let marks1 = if left_bias then marks1a else marks1a @ [[]] in
  let marks2 = if left_bias then [] :: marks2a else marks2a in
  ({items = items1; marks = marks1}, {items = items2; marks = marks2})

(** [split_on_marks index ml]: splits mlist [ml] at [index], returning the marks in the middle *)
let split_on_marks (index : int) (ml : 'a t) : 'a t * mark list * 'a t =
  let left, { items; marks } = split ~left_bias:false index ml in
  match marks with
  | middle_marks :: right_marks -> left, middle_marks, { items; marks = [] :: right_marks }
  | _ -> failwith "split_on_marks: mark list length is wrong"

(** [merge ml1 ml2]: merges mlists [ml1] and [ml2]. *)
let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let marks1, tmp_marks1 = List.unlast ml1.marks in
  let tmp_marks2, marks2 = List.uncons ml2.marks in
  let merged_marks = [tmp_marks1 @ tmp_marks2] in
  { items = ml1.items @ ml2.items; marks = marks1 @ merged_marks @ marks2 }

(** [merge_list mll]: converts a list of mlists into a single mlist. *)
let merge_list (mll : 'a t list) : 'a t =
  List.fold_left (fun acc ml -> merge acc ml) empty mll

(** [extract ~start_left_bias ~stop_left_bias start nb ml]: extracts mlist from index [start] to [start + nb]. *)
let extract ?(start_left_bias : bool = true) ?(stop_left_bias : bool = true) (start : int) (nb : int) (ml : 'a t) : 'a t * 'a t =
  let ml1, ml23 = split ~left_bias:start_left_bias start ml in
  let ml2, ml3 = split ~left_bias:stop_left_bias nb ml23 in
  let ml13 = merge ml1 ml3 in
  (ml13, ml2)

(** [remove start nb ml]: removes items that fall in the range [start, start + nb)].  *)
let remove (start : int) (nb : int) (ml : 'a t) : 'a t =
  fst (extract ~stop_left_bias:false start nb ml)

(** [concat_mapi f ml] maps all the elements of [ml] to a list using [f],
    and flatten this list inside the sequence.
    Preserves all marks between elements groups, mergins them when needed. *)
let concat_mapi (f: int -> 'a -> 'b t) (ml: 'a t) : 'b t =
  let rec aux i items marks =
    match items, marks with
    | [], [m] -> { items = []; marks = [m] }
    | [], _ | _, [] -> assert false
    | it :: items, m :: marks ->
      let new_items = f i it in
      let new_items = insert_marks_at 0 m new_items in
      let tail = aux (i+1) items marks in
      merge new_items tail
  in
  aux 0 ml.items ml.marks

(** Same as concat_mapi but the generated list does not depend on the index inside the list *)
let concat_map (f: 'a -> 'b t) (ml: 'a t) : 'b t =
  concat_mapi (fun _ -> f) ml

(** Similar to List.filteri but maintains marks as required. *)
let filteri (f : int -> 'a -> bool) (ml : 'a t) : 'a t =
  concat_mapi (fun i x -> if f i x then of_list [x] else empty) ml

(* Same as MList.filteri but the filtering decision does not depend on the index inside the list. *)
let filter (f: 'a -> bool) (ml: 'a t) : 'a t =
  filteri (fun _ -> f) ml

(** [insert_sublist_at index sl ml]: inserts mlist [sl] at [index] in [ml]. *)
let insert_sublist_at (index : int) (sl : 'a list) (ml : 'a t) : 'a t =
   let sz = length ml in
   assert (0 <= index && index <= sz);
   let lfront, lback = split index ml in
   let x = of_list sl in
   let new_ml = merge lfront x in
   merge new_ml lback

(** [insert_at index x ml]: just a specialization of insert_sublist_at for a mlist with only one item. *)
let insert_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  insert_sublist_at index [x] ml

(** [push_front x ml]: inserts the element [x] at the beginning of the mlist [ml]. *)
let push_front (x : 'a) (ml : 'a t) : 'a t =
  (* DEPRECATED: weird marks behaviour
    insert_at 0 x ml *)
  merge (of_list [x]) ml

(** [push_back x ml]: inserts the element [x] at the end of the mlist [ml]. *)
let push_back (x : 'a) (ml : 'a t) : 'a t =
  (* DEPRECATED: weird marks behaviour
    insert_at (length ml) x ml *)
  merge ml (of_list [x])

(** [pop_front ml]: removes the first element from the mlist [ml]. *)
let pop_front (ml : 'a t) : 'a t =
  remove 0 1 ml

(** [pop_back ml]: removes the last element from the mlist [ml]. *)
let pop_back (ml : 'a t) : 'a t =
  let ln = length ml in
  remove (ln - 1) 1 ml

(** [last ml]: returns the last element from the mlist [ml]. *)
let last (ml : 'a t) : 'a option =
  let len = length ml in
  if len = 0 then None else Some (nth ml (len - 1))

(** [update_nth n transfo ml]: applies function [transfo] at the item with index [n] in mlist [ml]. *)
let update_nth (n : int) (transfo : 'a -> 'a) (ml : 'a t) : 'a t =
  { ml with items = List.update_nth n transfo ml.items }

(** [update_at_index_and_fix_beyond index f_update_at f_update_further ml]: applies [f_update_at] on the element
    at [index] and modify accordingly all the elements that come after using [f_update_further]. *)
let update_at_index_and_fix_beyond ?(delete : bool = false) (index : int) (f_update_at : 'a -> 'a) (f_update_further : 'a -> 'a) (ml : 'a t) : 'a t =
  let lfront, lback = split index ml in
  let element, lback = split 1 lback in
  let element = if delete then empty else update_nth 0 f_update_at element in
  let lback = map f_update_further lback in
  merge_list [lfront; element; lback]

(** [get_item_and_its_relatives index trms]: for an  item [t] with index [index] in the mlist its belongs to,
    returns the list of items before [t], [t] itself and the list of items that come after [t]. *)
let get_item_and_its_relatives (index : int) (items : 'a t) : ('a t * 'a * 'a t) =
  let lfront, lback = split index items in
  let element, lback = split 1 lback in
  let element =
    if length element = 1
      then nth element 0
      else failwith "Mlist.get_item_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)

(** [flatten_marks f_item f_marks]: interleave items generated by [f_marks] and [f_item] into a single list *)
let flatten_marks (f_item: 'a -> 'b) (f_marks: mark list -> 'b) (ml: 'a t): 'b list =
  let rec aux items marks =
    match items, marks with
    | [], [[]] -> []
    | [], [m] -> [f_marks m]
    | [], _ | _, [] -> assert false
    | i :: is, [] :: ms ->
      f_item i :: aux is ms
    | i :: is, m :: ms ->
      f_marks m :: f_item i :: aux is ms
  in
  aux ml.items ml.marks
