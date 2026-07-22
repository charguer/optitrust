(** Marked lists used by the OptiTrust AST.

    [Mlist] stores a logical sequence of items together with marks attached to
    positions around those items. Marks can appear before the first item,
    between two items, or after the last item. This is used in the AST to keep
    transformation metadata, comments, and other annotations attached to stable
    positions while sequences are split, merged, filtered, or edited.

    The public API is intentionally close to the standard [List] API for common
    operations such as [map], [mapi], [iter], [fold_left], [nth], [rev],
    [filter], and [concat_map]. It also provides mark-aware editing operations:
    [split], [split_on_marks], [merge], [extract], [remove], [insert_at],
    [insert_sublist_at], [replace_at], [update_nth], and mark operations such as
    [insert_mark_at], [insert_marks_at], [filter_marks], [remove_mark], and
    [flatten_marks].

    Internally, items and mark slots are stored in [Alist], a chunked-array
    sequence representation that replaces the previous list-backed storage for
    better indexed access and local edits. *)

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

let alist_of_short_list (length : int) (items : 'a list) : 'a Alist.t =
  if length = 0 then Alist.empty
  else if length <= Alist.short_limit then Alist.List items
  else Alist.of_list_with_length length items

let make_from_short_lists (length : int) (items : 'a list) (marks : mark list list) : 'a t =
  { items = alist_of_short_list length items;
    marks = alist_of_short_list (length + 1) marks;
    length }

let length (ml : 'a t) : int =
  ml.length

let of_list (l : 'a list) : 'a t =
  let length = List.length l in
  let marks = List.init (length + 1) (fun _i -> []) in
  { items = alist_of_short_list length l;
    marks = alist_of_short_list (length + 1) marks;
    length }

let to_list (ml : 'a t) : 'a list =
  match ml.items with
  | Alist.List items -> items
  | _ -> Alist.to_list ml.items

let get_marks (ml : 'a t) : mark list list =
  match ml.marks with
  | Alist.List marks -> marks
  | _ -> Alist.to_list ml.marks

let empty () : 'a t =
  { items = Alist.empty;
    marks = Alist.of_list [[]];
    length = 0 }

let is_empty (ml : 'a t) : bool =
  length ml = 0

(******************************************)

let mapi (f : int -> 'a -> 'b) (ml : 'a t) : 'b t =
  match ml.items with
  | Alist.List items ->
    { items = alist_of_short_list ml.length (List.mapi f items);
      marks = ml.marks;
      length = ml.length }
  | _ ->
    { items = Alist.mapi f ml.items;
      marks = ml.marks;
      length = ml.length }

let map (f : 'a -> 'b) (ml : 'a t) : 'b t =
  match ml.items with
  | Alist.List items ->
    { items = alist_of_short_list ml.length (List.map f items);
      marks = ml.marks;
      length = ml.length }
  | _ ->
    { items = Alist.map f ml.items;
      marks = ml.marks;
      length = ml.length }

let iteri (f : int -> 'a -> unit) (ml : 'a t) : unit =
  match ml.items with
  | Alist.List items -> List.iteri f items
  | _ -> Alist.iteri f ml.items

let iter (f : 'a -> unit) (ml : 'a t) : unit =
  match ml.items with
  | Alist.List items -> List.iter f items
  | _ -> Alist.iter f ml.items

let find_map (f : 'a -> 'b option) (ml : 'a t) : 'b option =
  match ml.items with
  | Alist.List items -> List.find_map f items
  | _ -> Alist.find_map f ml.items

let fold_left (acc_f : 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  match ml.items with
  | Alist.List items -> List.fold_left acc_f acc items
  | _ -> Alist.fold_left acc_f acc ml.items

let nth (ml : 'a t) (index : int) : 'a =
  match ml.items with
  | Alist.List items -> List.nth items index
  | _ -> Alist.nth ml.items index

let nth_opt (ml : 'a t) (index : int) : 'a option =
  match ml.items with
  | Alist.List items -> if index >= 0 then List.nth_opt items index else None
  | _ -> Alist.nth_opt ml.items index

let fold_lefti (acc_f : int -> 'b -> 'a -> 'b) (acc : 'b) (ml : 'a t) : 'b =
  match ml.items with
  | Alist.List items -> List.fold_lefti acc_f acc items
  | _ ->
    let current = ref acc in
    iteri (fun i item -> current := acc_f i !current item) ml;
    !current

let for_all p ml =
  match ml.items with
  | Alist.List items -> List.for_all p items
  | _ -> Alist.for_all p ml.items

let for_all2 p ml1 ml2 =
  List.for_all2 p (to_list ml1) (to_list ml2)

let rev (ml : 'a t) : 'a t =
  match ml.items, ml.marks with
  | Alist.List items, Alist.List marks ->
    { items = alist_of_short_list ml.length (List.rev items);
      marks = alist_of_short_list (ml.length + 1) (List.rev marks);
      length = ml.length }
  | _ ->
    { items = Alist.rev ml.items;
      marks = Alist.rev ml.marks;
      length = ml.length }

(***********************************************************************)

let replace_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  match ml.items with
  | Alist.List items ->
    { ml with items = alist_of_short_list ml.length (List.update_nth index (fun _ -> x) items) }
  | _ ->
    { ml with items = Alist.update_nth index (fun _ -> x) ml.items }

let insert_mark_at (index : int) (m : mark) (ml : 'a t) : 'a t =
  match ml.marks with
  | Alist.List marks ->
    { ml with marks = alist_of_short_list (ml.length + 1) (List.update_nth index (fun ms -> m :: ms) marks) }
  | _ ->
    { ml with marks = Alist.update_nth index (fun ms -> m :: ms) ml.marks }

let insert_marks_at (index : int) (m : mark list) (ml : 'a t) : 'a t =
  match ml.marks with
  | Alist.List marks ->
    { ml with marks = alist_of_short_list (ml.length + 1) (List.update_nth index (fun ms -> m @ ms) marks) }
  | _ ->
    { ml with marks = Alist.update_nth index (fun ms -> m @ ms) ml.marks }

let filter_marks (f: mark -> bool) (ml: 'a t) : 'a t =
  match ml.marks with
  | Alist.List marks ->
    { ml with marks = alist_of_short_list (ml.length + 1) (List.map (fun ms -> List.filter f ms) marks) }
  | _ ->
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
  match ml.items, ml.marks with
  | Alist.List items, Alist.List marks ->
    let right_length = ml.length - index in
    let items1, items2 = List.split_at index items in
    let marks1a, marks2a =
      List.split_at (index + if left_bias then 1 else 0) marks
    in
    let marks1 = if left_bias then marks1a else marks1a @ [[]] in
    let marks2 = if left_bias then [] :: marks2a else marks2a in
    ( { items = alist_of_short_list index items1;
        marks = alist_of_short_list (index + 1) marks1;
        length = index },
      { items = alist_of_short_list right_length items2;
        marks = alist_of_short_list (right_length + 1) marks2;
        length = right_length } )
  | _ ->
    let items1, items2 = Alist.split index ml.items in
    let marks1a, marks2a =
      Alist.split (index + if left_bias then 1 else 0) ml.marks
    in
    let marks1 =
      if left_bias
        then marks1a
        else Alist.push_back [] marks1a
    in
    let marks2 =
      if left_bias
        then Alist.push_front [] marks2a
        else marks2a
    in
    ( { items = items1;
        marks = marks1;
        length = index },
      { items = items2;
        marks = marks2;
        length = ml.length - index } )

let split_on_marks (index : int) (ml : 'a t) : 'a t * mark list * 'a t =
  let left, right = split ~left_bias:false index ml in
  let middle_marks = Alist.nth right.marks 0 in
  let _, right_marks = Alist.split 1 right.marks in
  left, middle_marks, { right with marks = Alist.push_front [] right_marks }

let merge (ml1 : 'a t) (ml2 : 'a t) : 'a t =
  let length = ml1.length + ml2.length in
  match ml1.items, ml2.items, ml1.marks, ml2.marks with
  | Alist.List items1, Alist.List items2, Alist.List marks1_all, Alist.List marks2_all
    when length + 1 <= Alist.short_limit ->
    let marks1, last_marks1 = List.unlast marks1_all in
    let first_marks2, marks2 = List.uncons marks2_all in
    { items = alist_of_short_list length (items1 @ items2);
      marks = alist_of_short_list (length + 1) (marks1 @ [last_marks1 @ first_marks2] @ marks2);
      length }
  | _ ->
    let items = Alist.merge ml1.items ml2.items in
    { items;
      marks = Alist.merge_touching ( @ ) ml1.marks ml2.marks;
      length }

let merge_list (mll : 'a t list) : 'a t =
  List.fold_left (fun acc ml -> merge acc ml) (empty ()) mll

let extract_short_lists
  ~(start_left_bias : bool)
  ~(stop_left_bias : bool)
  (start : int)
  (nb : int)
  (length : int)
  (items : 'a list)
  (marks : mark list list) : 'a t * 'a t =
  let before_items, after_start_items = List.split_at start items in
  let extracted_items, after_items = List.split_at nb after_start_items in
  let before_marks, start_and_after_marks = List.split_at start marks in
  let start_marks, after_start_marks = List.uncons start_and_after_marks in
  let middle_marks, stop_and_after_marks = List.split_at (nb - 1) after_start_marks in
  let stop_marks, after_stop_marks = List.uncons stop_and_after_marks in
  let rest_boundary_marks =
    (if start_left_bias then start_marks else [])
    @ (if stop_left_bias then [] else stop_marks)
  in
  let extracted_start_marks =
    if start_left_bias then [] else start_marks
  in
  let extracted_stop_marks =
    if stop_left_bias then stop_marks else []
  in
  let rest_items = before_items @ after_items in
  let rest_marks = before_marks @ [rest_boundary_marks] @ after_stop_marks in
  let extracted_marks = extracted_start_marks :: middle_marks @ [extracted_stop_marks] in
  make_from_short_lists (length - nb) rest_items rest_marks,
  make_from_short_lists nb extracted_items extracted_marks

let extract ?(start_left_bias : bool = true) ?(stop_left_bias : bool = true) (start : int) (nb : int) (ml : 'a t) : 'a t * 'a t =
  if start < 0 || nb < 0 || start + nb > ml.length then invalid_arg "Mlist.extract";
  if nb = 0 then begin
    let ml1, ml23 = split ~left_bias:start_left_bias start ml in
    let ml2, ml3 = split ~left_bias:stop_left_bias nb ml23 in
    let ml13 = merge ml1 ml3 in
    (ml13, ml2)
  end else match ml.items, ml.marks with
  | Alist.List items, Alist.List marks ->
    extract_short_lists
      ~start_left_bias
      ~stop_left_bias
      start
      nb
      ml.length
      items
      marks
  | _ ->
    let stop = start + nb in
    let rest_items, extracted_items = Alist.extract start nb ml.items in
    let rest_marks, extracted_marks =
      Alist.extract_between_with_boundary_maps start stop
        (fun start_marks stop_marks ->
          (if start_left_bias then start_marks else [])
          @ (if stop_left_bias then [] else stop_marks))
        (fun start_marks -> if start_left_bias then [] else start_marks)
        (fun stop_marks -> if stop_left_bias then stop_marks else [])
        ml.marks
    in
    ( { items = rest_items;
        marks = rest_marks;
        length = ml.length - nb },
      { items = extracted_items;
        marks = extracted_marks;
        length = nb } )

let remove (start : int) (nb : int) (ml : 'a t) : 'a t =
  if start < 0 || nb < 0 || start + nb > ml.length then invalid_arg "Mlist.remove";
  if nb = 0 then ml else
  let stop = start + nb in
  { items = Alist.remove start nb ml.items;
    marks = Alist.remove_between_and_merge start stop ( @ ) ml.marks;
    length = ml.length - nb }

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
  match sl with
  | [] -> ml
  | _ ->
    let inserted_length = List.length sl in
    { items = Alist.insert_list index sl ml.items;
      marks = Alist.insert_list (index + 1) (List.init inserted_length (fun _ -> [])) ml.marks;
      length = ml.length + inserted_length }

let insert_at (index : int) (x : 'a) (ml : 'a t) : 'a t =
  let sz = length ml in
  assert (0 <= index && index <= sz);
  if index = ml.length then
    { items = Alist.push_back x ml.items;
      marks = Alist.push_back [] ml.marks;
      length = ml.length + 1 }
  else
    { items = Alist.insert index x ml.items;
      marks = Alist.insert (index + 1) [] ml.marks;
      length = ml.length + 1 }

let push_front (x : 'a) (ml : 'a t) : 'a t =
  { items = Alist.push_front x ml.items;
    marks = Alist.push_front [] ml.marks;
    length = ml.length + 1 }

let push_back (x : 'a) (ml : 'a t) : 'a t =
  insert_at ml.length x ml

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
