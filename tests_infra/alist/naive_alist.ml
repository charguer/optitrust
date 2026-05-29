(** Test-only reference implementation for [Alist].

    This module intentionally uses ordinary OCaml lists and favors obvious
    behavior over efficiency. It is the model that the Monolith harness compares
    against the chunked-array implementation. *)

type 'a t = 'a list

let empty : 'a t =
  []

let is_empty (seq : 'a t) : bool =
  seq = []

let of_list (items : 'a list) : 'a t =
  items

let of_array (items : 'a array) : 'a t =
  Array.to_list items

let to_list (seq : 'a t) : 'a list =
  seq

let length (seq : 'a t) : int =
  List.length seq

let nth (seq : 'a t) (index : int) : 'a =
  if index < 0 then failwith "Naive_alist.nth: negative index";
  try List.nth seq index with Failure _ ->
    failwith "Naive_alist.nth: index out of bounds"

let nth_opt (seq : 'a t) (index : int) : 'a option =
  if index < 0 then None else
  try Some (List.nth seq index) with Failure _ -> None

let mapi (f : int -> 'a -> 'b) (seq : 'a t) : 'b t =
  List.mapi f seq

let map (f : 'a -> 'b) (seq : 'a t) : 'b t =
  List.map f seq

let iteri (f : int -> 'a -> unit) (seq : 'a t) : unit =
  List.iteri f seq

let iter (f : 'a -> unit) (seq : 'a t) : unit =
  List.iter f seq

let fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (seq : 'a t) : 'b =
  List.fold_left f acc seq

let find_map (f : 'a -> 'b option) (seq : 'a t) : 'b option =
  List.find_map f seq

let for_all (p : 'a -> bool) (seq : 'a t) : bool =
  List.for_all p seq

let rev (seq : 'a t) : 'a t =
  List.rev seq

let split (index : int) (seq : 'a t) : 'a t * 'a t =
  let len = length seq in
  if index < 0 || index > len then invalid_arg "Naive_alist.split";
  let rec aux i acc rest =
    if i = 0 then List.rev acc, rest else
    match rest with
    | [] -> assert false
    | x :: xs -> aux (i - 1) (x :: acc) xs
  in
  aux index [] seq

let insert_list (index : int) (inserted : 'a list) (seq : 'a t) : 'a t =
  let left, right = split index seq in
  left @ inserted @ right

let insert_array (index : int) (inserted : 'a array) (seq : 'a t) : 'a t =
  insert_list index (Array.to_list inserted) seq

let insert (index : int) (item : 'a) (seq : 'a t) : 'a t =
  insert_list index [item] seq

let update_nth (index : int) (f : 'a -> 'a) (seq : 'a t) : 'a t =
  if index < 0 || index >= length seq then invalid_arg "Naive_alist.update_nth";
  List.mapi (fun i x -> if i = index then f x else x) seq

let merge (seq1 : 'a t) (seq2 : 'a t) : 'a t =
  seq1 @ seq2
