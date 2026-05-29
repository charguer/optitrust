type mark = string (* must be the same as Ast.mark *)

type 'a seq = 'a Alist.t

(** ['a t] is a marked list represented by two chunked sequences:
    - [items] stores the logical elements.
    - [marks] stores marks for positions before, between, and after items.
    - [length] is the number of logical elements in [items].

    Invariant: [marks] contains [length + 1] mark slots. *)
type 'a t =
  { items : 'a seq;
    marks : mark list seq;
    length : int }

type 'a mlist = 'a t

let length (ml : 'a t) : int =
  ml.length

let of_list (l : 'a list) : 'a t =
  let length = List.length l in
  { items = Alist.of_list l;
    marks = Alist.of_list (List.init (length + 1) (fun _i -> []));
    length }

let to_list (ml : 'a t) : 'a list =
  Alist.to_list ml.items

let get_marks (ml : 'a t) : mark list list =
  Alist.to_list ml.marks

let empty () : 'a t =
  { items = Alist.empty;
    marks = Alist.of_list [[]];
    length = 0 }

let is_empty (ml : 'a t) : bool =
  length ml = 0

(******************************************)

let mapi (f : int -> 'a -> 'b) (ml : 'a t) : 'b t =
  { items = Alist.mapi f ml.items;
    marks = ml.marks;
    length = ml.length }

let map (f : 'a -> 'b) (ml : 'a t) : 'b t =
  mapi (fun _i x -> f x) ml

let iteri (f : int -> 'a -> unit) (ml : 'a t) : unit =
  Alist.iteri f ml.items

let iter (f : 'a -> unit) (ml : 'a t) : unit =
  iteri (fun _i x -> f x) ml

let find_map (f : 'a -> 'b option) (ml : 'a t) : 'b option =
  Alist.find_map f ml.items

let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  Alist.fold_left acc_f acc ml.items

let nth (ml : 'a t) (index : int) : 'a =
  Alist.nth ml.items index

let nth_opt (ml : 'a t) (index : int) : 'a option =
  Alist.nth_opt ml.items index

let fold_lefti (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  let current = ref acc in
  iteri (fun i item -> current := acc_f i !current item) ml;
  !current

let for_all p ml =
  Alist.for_all p ml.items

let for_all2 p ml1 ml2 =
  List.for_all2 p (to_list ml1) (to_list ml2)

let rev (ml : 'a t) : 'a t =
  { items = Alist.rev ml.items;
    marks = Alist.rev ml.marks;
    length = ml.length }

(***********************************************************************)

let replace_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  { ml with items = Alist.update_nth index (fun _ -> x) ml.items }

let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  { ml with marks = Alist.update_nth index (fun ms -> m :: ms) ml.marks }

let insert_marks_at (index : int) (m : mark list) (ml : 'a t) : 'a t =
  { ml with marks = Alist.update_nth index (fun ms -> m @ ms) ml.marks }

let filter_marks (f: mark -> bool) (ml: 'a t) : 'a t =
  { ml with marks = Alist.map (fun ms -> List.filter f ms) ml.marks }

let remove_mark (m : mark) (ml : 'a t) : 'a t =
  filter_marks (fun x -> x <> m) ml

let remove_all_marks (ml: 'a t): 'a t =
  of_list (to_list ml)

let make (items : 'a seq) (marks : mark list seq) : 'a t =
  let length = Alist.length items in
  assert (Alist.length marks = length + 1);
  { items; marks; length }

let split ?(left_bias : bool = true) (index : int) (ml : 'a t) : 'a t * 'a t =
  if index < 0 || index > ml.length then invalid_arg "Mlist_new.split";
  let items1, items2 = Alist.split index ml.items in
  let marks1a, marks2a =
    Alist.split (index + if left_bias then 1 else 0) ml.marks
  in
  let marks1 =
    if left_bias
      then marks1a
      else Alist.insert (Alist.length marks1a) [] marks1a
  in
  let marks2 =
    if left_bias
      then Alist.insert 0 [] marks2a
      else marks2a
  in
  make items1 marks1, make items2 marks2

let split_on_marks (index : int) (ml : 'a t) : 'a t * mark list * 'a t =
  let left, right = split ~left_bias:false index ml in
  let middle_marks = Alist.nth right.marks 0 in
  let _, right_marks = Alist.split 1 right.marks in
  left, middle_marks, make right.items (Alist.insert 0 [] right_marks)

let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let items = Alist.merge ml1.items ml2.items in
  let marks1, last_marks1 = Alist.split ml1.length ml1.marks in
  let first_marks2, marks2 = Alist.split 1 ml2.marks in
  let merged_marks =
    Alist.of_list [Alist.nth last_marks1 0 @ Alist.nth first_marks2 0]
  in
  make items (Alist.merge (Alist.merge marks1 merged_marks) marks2)

let merge_list (mll : 'a t list) : 'a t =
  List.fold_left (fun acc ml -> merge acc ml) (empty ()) mll

let extract ?(start_left_bias : bool = true) ?(stop_left_bias : bool = true) (start : int) (nb : int) (ml : 'a t) : 'a t * 'a t =
  let ml1, ml23 = split ~left_bias:start_left_bias start ml in
  let ml2, ml3 = split ~left_bias:stop_left_bias nb ml23 in
  let ml13 = merge ml1 ml3 in
  (ml13, ml2)

let remove (start : int) (nb : int) (ml : 'a t) : 'a t =
  fst (extract ~stop_left_bias:false start nb ml)

let concat_mapi (f: int -> 'a -> 'b t) (ml: 'a t) : 'b t =
  let rec aux i items marks =
    match items, marks with
    | [], [m] ->
      { items = Alist.empty;
        marks = Alist.of_list [m];
        length = 0 }
    | [], _ | _, [] -> assert false
    | it :: items, m :: marks ->
      let new_items = f i it in
      let new_items = insert_marks_at 0 m new_items in
      let tail = aux (i + 1) items marks in
      merge new_items tail
  in
  aux 0 (to_list ml) (get_marks ml)

let concat_map (f: 'a -> 'b t) (ml: 'a t) : 'b t =
  concat_mapi (fun _ -> f) ml

let filteri (f : int -> 'a -> bool) (ml : 'a t) : 'a t =
  concat_mapi (fun i x -> if f i x then of_list [x] else empty ()) ml

let filter (f: 'a -> bool) (ml: 'a t) : 'a t =
  filteri (fun _ -> f) ml

let insert_sublist_at (index : int) (sl : 'a list) (ml : 'a t) : 'a t =
   let sz = length ml in
   assert (0 <= index && index <= sz);
   let lfront, lback = split index ml in
   let x = of_list sl in
   let new_ml = merge lfront x in
   merge new_ml lback

let insert_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  insert_sublist_at index [x] ml

let push_front (x : 'a) (ml : 'a t) : 'a t =
  merge (of_list [x]) ml

let push_back (x : 'a) (ml : 'a t) : 'a t =
  merge ml (of_list [x])

let pop_front (ml : 'a t) : 'a t =
  remove 0 1 ml

let pop_back (ml : 'a t) : 'a t =
  let ln = length ml in
  remove (ln - 1) 1 ml

let last (ml : 'a t) : 'a option =
  let len = length ml in
  if len = 0 then None else Some (nth ml (len - 1))

let update_nth (n : int) (transfo : 'a -> 'a) (ml : 'a t) : 'a t =
  { ml with items = Alist.update_nth n transfo ml.items }

let update_at_index_and_fix_beyond ?(delete : bool = false) (index : int) (f_update_at : 'a -> 'a) (f_update_further : 'a -> 'a) (ml : 'a t) : 'a t =
  let lfront, lback = split index ml in
  let element, lback = split 1 lback in
  let element = if delete then empty () else update_nth 0 f_update_at element in
  let lback = map f_update_further lback in
  merge_list [lfront; element; lback]

let get_item_and_its_relatives (index : int) (items : 'a t) : ('a t * 'a * 'a t) =
  let lfront, lback = split index items in
  let element, lback = split 1 lback in
  let element =
    if length element = 1
      then nth element 0
      else failwith "Mlist.get_item_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)

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
  aux (to_list ml) (get_marks ml)
