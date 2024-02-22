type mark = string (* must be the same as Ast.mark *)

(* ['a t]: mlist is a record of two fields, items is the list of objects it contains
   and marks is the list of marks for each in-between location. *)
type 'a t =
  { items : 'a list;
    marks : (mark list) list  }

(* LATER:
  type marks = mark list
  type 'a t = (marks * 'a * marks) list
   *)

(* [length ml]: the number of items inside [ml]. *)
let length (ml : 'a t) : int =
  List.length ml.items

(* [of_list l]: creates a mlist from a list of objects of type 'a .*)
let of_list (l : 'a list) : 'a t =
  { items = l;
    marks = List.init (List.length l + 1) (fun _i -> [])}

(* [to_list ml]: extracts the items from mlist [ml]. *)
let to_list (ml : 'a t) : 'a list =
  ml.items

(* [empty] : empty mlist. *)
let empty : 'a t =
  {items = [];
   marks = [[]]}

(*********************** List module equivalent functions *********************)
(* [mapi f ml]: applies List.mapi f to ml.items. *)
let mapi (f : int -> 'a -> 'b) (ml : 'a t) : 'b t =
  let new_items = List.mapi f ml.items in
  { ml with items = new_items }

(* [map f ml]: applies List.map f to ml.items. *)
let map (f : 'a -> 'b) (ml : 'a t) : 'b t =
  mapi (fun _i x -> f x) ml

(* [iteri f ml]: applies List.iteri f to ml.items. *)
let iteri (f : int -> 'a -> unit) (ml : 'a t) : unit =
  List.iteri f (to_list ml)

(* [iter f ml]: applies List.iter f to ml.items. *)
let iter (f : 'a -> unit) (ml : 'a t) : unit =
  iteri (fun _i x -> f x) ml

(* [find_map f ml]: applies Xlist.find_map f to ml.items. *)
let find_map (f : 'a -> 'b option) (ml : 'a t) : 'b option =
  Xlist.find_map f ml.items (* LATER: now in the List stdlib module *)

(* [findi f ml]: returns the first element, together with its index, of
   [ml.items] that satisfies the predicate [f]. *)
let findi (f : 'a -> bool) (ml : 'a t) : (int * 'a) option =
  let rec aux (l : 'a list) (i : int) : (int * 'a) option =
    match l with
    | e :: r -> if f e then Some (i, e) else aux r (i + 1)
    | [] -> None
  in
  aux ml.items 0

(* [fold_left acc_f acc ml]: applies List.fold_left to ml.items. *)
let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  List.fold_left acc_f acc ml.items

(* [nth ml index]: get the nth item from [ml]. *)
let nth (ml : 'a t) (index : int) : 'a =
  List.nth ml.items index

(* [nth_opt ml index]: get the nth item from [ml]. *)
let nth_opt (ml : 'a t) (index : int) : 'a option =
  List.nth_opt ml.items index

(* [fold_lefti acc_f acc ml]: applies Xlist.fold_lefti to ml.items. *)
let fold_lefti (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  Xlist.fold_lefti acc_f acc ml.items

let for_all p ml =
  List.for_all p (to_list ml)

(* [for_all2 p ml1 ml2]: applies List.for_all2 to ml1.items and ml2.items. *)
let for_all2 p ml1 ml2 =
  List.for_all2 p (to_list ml1) (to_list ml2)

(* [rev ml]: applies List.rev to ml.items and ml.marks. *)
let rev (ml : 'a t) : 'a t =
  { items = List.rev ml.items;
    marks = List.rev ml.marks }

(* [partition ml]: applies List.partition to ml.items *)
let partition (pred : 'a -> bool) (ml : 'a t) : ('a t * 'a t) =
  let ml_items_sat, ml_items = List.partition pred ml.items in
  (of_list ml_items_sat, of_list ml_items  )

(***********************************************************************)

(* [is_empty ml]: checks if [ml] is empty or not. *)
let is_empty (ml :'a t) : bool =
  length ml = 0

(* [replace_at index x ml]: replaces the item at [index] in [ml] with element [x].*)
let replace_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  { ml with items = Xlist.update_nth index (fun _ -> x) ml.items }

(* [insert_at index m ml]: inserts at [index] in ml mark [m]. *)
let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  { ml with marks = Xlist.update_nth index (fun ms -> m :: ms) ml.marks}

(* [remove_at index m ml]: removes at [index] in ml mark [m]. *)
let remove_mark (m : mark) (ml : 'a t) : 'a t =
  let new_marks = List.map (fun ms -> List.filter (fun x -> x <> m) ms) ml.marks in
  { ml with marks = new_marks }

(* [split ~left_bias index ml]: splits mlist [ml] at [index]
    [left_bias] - if true then the boundary marks will go with the first part of the mlist. *)
let split ?(left_bias : bool = true) (index : int) (ml : 'a t) : 'a t * 'a t=
  let items1, items2 = Xlist.split_at index ml.items in
  let marks1a, marks2a = Xlist.split_at (index + if left_bias then 1 else 0) ml.marks in
  let marks1 = if left_bias then marks1a else marks1a @ [] in
  let marks2 = if left_bias then [] :: marks2a else marks2a in
  ({items = items1; marks = marks1}, {items = items2; marks = marks2})

(* [merge ml1 ml2]: merges mlists [ml1] and [ml2]. *)
let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let marks1, tmp_marks1 = Xlist.unlast ml1.marks in
  let tmp_marks2, marks2 = Xlist.uncons ml2.marks in
  let merged_marks = [tmp_marks1 @ tmp_marks2] in
  { items = ml1.items @ ml2.items; marks = marks1 @ merged_marks @ marks2 }

(* [merge_list mll]: converts a list of mlists into a single mlist. *)
let merge_list (mll : 'a t list) : 'a t =
  List.fold_left (fun acc ml -> merge acc ml) empty mll

(* [extract ~start_left_bias ~stop_left_bias start nb ml]: extracts mlist from index [start] to [start + nb]. *)
let extract ?(start_left_bias : bool = true) ?(stop_left_bias : bool = true) (start : int) (nb : int) (ml : 'a t) : 'a t * 'a t =
  let ml1, ml23 = split ~left_bias:start_left_bias start ml in
  let ml2, ml3 = split ~left_bias:stop_left_bias nb ml23 in
  let ml13 = merge ml1 ml3 in
  (ml13, ml2)

(* [remove start nb ml]: removes items that fall in the range [start, start + nb).  *)
let remove (start : int) (nb : int) (ml : 'a t) : 'a t =
  fst (extract start nb ml)

(* [insert_sublist_at index sl ml]: inserts mlist [sl] at [index] in [ml]. *)
let insert_sublist_at (index : int) (sl : 'a list) (ml : 'a t) : 'a t =
   let sz = length ml in
   assert (0 <= index && index <= sz);
   let lfront, lback = split index ml in
   let x = of_list sl in
   let new_ml = merge lfront x in
   merge new_ml lback

(* [insert_at index x ml]: just a specialization of insert_sublist_at for a mlist with only one item. *)
let insert_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  insert_sublist_at index [x] ml

(* [push_front x ml]: inserts the element [x] at the beginning of the mlist [ml]. *)
let push_front (x : 'a) (ml : 'a t) : 'a t =
  insert_at 0 x ml

(* [push_back x ml]: inserts the element [x] at the end of the mlist [ml]. *)
let push_back (x : 'a) (ml : 'a t) : 'a t =
  insert_at (length ml) x ml

(* [pop_front ml]: removes the first element from the mlist [ml]. *)
let pop_front (ml : 'a t) : 'a t =
  remove 0 1 ml

(* [pop_back ml]: removes the last element from the mlist [ml]. *)
let pop_back (ml : 'a t) : 'a t =
  let ln = length ml in
  remove (ln - 1) 1 ml


(* [update_nth n transfo ml]: applies function [transfo] at the item with index [n] in mlist [ml]. *)
let update_nth (n : int) (transfo : 'a -> 'a) (ml : 'a t) : 'a t =
  { ml with items = Xlist.update_nth n transfo ml.items }

(* [update_at_index_and_fix_beyond index f_update_at f_update_further ml]: applies [f_update_at] on the element
    at [index] and modify accordingly all the elements that come after using [f_update_further]. *)
let update_at_index_and_fix_beyond ?(delete : bool = false) (index : int) (f_update_at : 'a -> 'a) (f_update_further : 'a -> 'a) (ml : 'a t) : 'a t =
  let lfront, lback = split index ml in
  let element, lback = split 1 lback in
  let element = if delete then empty else update_nth 0 f_update_at element in
  let lback = map f_update_further lback in
  merge_list [lfront; element; lback]

(* [get_item_and_its_relatives index trms]: for an  item [t] with index [index] in the mlist its belongs to,
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