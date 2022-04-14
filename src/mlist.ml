type mark = Mark.t

(* mlist is a record of two fields, items is the list of objects it contains 
   and marks is the list of marks for each in-between location *)
type 'a t =
  { items : 'a list;
    marks : (mark list) list  }

(* [length ml] the number of items inside [ml] *)
let length (ml : 'a t) : int =
  List.length ml.items

(* [of_list l] createa mlist from a list of objects of type 'a *)
let of_list (l : 'a list) : 'a t =
  { items = l;
    marks = List.init (List.length l + 1) (fun _i -> [])}

(* [to_list ml] extract the items from mlist [ml] *)
let to_list (ml : 'a t) : 'a list =
  ml.items

(*********************** List module equivalent functions *********************)
let mapi (f : int -> 'a -> 'b) (ml : 'a t) : 'b t =
  let new_items = List.mapi f ml.items in
  { ml with items = new_items }

let map (f : 'a -> 'b) (ml : 'a t) : 'b t =
  mapi (fun _i x -> f x) ml

let iteri (f : int -> 'a -> unit) (ml : 'a t) : unit =
  List.iteri f (to_list ml)

let iter (f : 'a -> unit) (ml : 'a t) : unit =
  iteri (fun _i x -> f x) ml

let find_map (f : 'a -> 'b option) (ml : 'a t) : 'b option =
  Tools.find_map f ml.items (* LATER: now in the List stdlib module *)

let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  List.fold_left acc_f acc ml.items

let nth (ml : 'a t) (index : int) : 'a =
  List.nth ml.items index

let fold_lefti (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  Tools.fold_lefti acc_f acc ml.items

let fold_righti (acc_f : int -> 'a -> 'b -> 'b)  (ml : 'a t) (acc : 'b) : 'b =
  Tools.fold_righti acc_f ml.items acc

let for_all2 p ml1 ml2 =
  List.for_all2 p (to_list ml1) (to_list ml2)

let rev (ml : 'a t) : 'a t =
  { items = List.rev ml.items;
    marks = List.rev ml.marks }

(***********************************************************************)

(* [replace_at index x ml] replace the item at [index] in [ml] with element [x] *)
let replace_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  { ml with items = Tools.update_nth index (fun _ -> x) ml.items }

(* [insert_at index m ml] insert at [index] in ml mark m *)
let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  { ml with marks = Tools.update_nth index (fun ms -> m :: ms) ml.marks}

(* [remove_at index m ml] remove at [index] in ml mark m *)
let remove_mark (m : mark) (ml : 'a t) : 'a t =
  let new_marks = List.map (fun ms -> List.filter (fun x -> x <> m) ms) ml.marks in
  { ml with marks = new_marks }

(* [split ~left_bias index ml] split mlist [ml] at index ml *)
let split ?(left_bias : bool = true) (index : int) (ml : 'a t) : 'a t * 'a t=
  let items1, items2 = Tools.split_list_at index ml.items in
  let marks1a, marks2a = Tools.split_list_at (index + if left_bias then 1 else 0) ml.marks in
  let marks1 = if left_bias then marks1a else marks1a @ [] in
  let marks2 = if left_bias then [] :: marks2a else marks2a in
  ({items = items1; marks = marks1}, {items = items2; marks = marks2}) 

(* [merge ml1 ml2] merge mlists [ml1] and [ml2] *)
let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let marks1, tmp_marks1 = Tools.unlast ml1.marks in
  let tmp_marks2, marks2 = Tools.uncons ml2.marks in
  let merged_marks = [tmp_marks1 @ tmp_marks2] in
  { items = ml1.items @ ml2.items; marks = marks1 @ merged_marks @ marks2 }

(* [extract ~start_left_bias ~stop_left_bias start nb ml] extract mlist from index start to (start + nb) *)
let extract ?(start_left_bias : bool = true) ?(stop_left_bias : bool = true) (start : int) (nb : int) (ml : 'a t) : 'a t * 'a t =
  let ml1, ml23 = split ~left_bias:start_left_bias start ml in
  let ml2, ml3 = split ~left_bias:stop_left_bias nb ml23 in
  let ml13 = merge ml1 ml3 in
  (ml13, ml2)

(* [remove start nb ml] remove items that all in the range [start, start + nb)  *)
let remove (start : int) (nb : int) (ml : 'a t) : 'a t =
  fst (extract start nb ml)

(* [insert_sublist_at index sl ml] insert mlist [sl] at [index] in [ml] *)
let insert_sublist_at (index : int) (sl : 'a list) (ml : 'a t) : 'a t =
   let sz = length ml in
   assert (0 <= index && index <= sz);
   let lfront, lback = split index ml in
   let x = of_list sl in 
   let new_ml = merge lfront x in
   merge new_ml lback

(* [insert_at index x ml] just a specialization of insert_sublist_at for a mlist with only one item *)
let insert_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  insert_sublist_at index [x] ml

(* [update_nth n transfo ml] apply function [transfo] to the item at index [n] in mlist [ml]*)
let update_nth (n : int) (transfo : 'a -> 'a) (ml : 'a t) : 'a t =
  { ml with items = Tools.update_nth n transfo ml.items }






