include Stdlib.List

(** [range ~rev a b]: returns the list of integers between [a] and [b] inclusive.
   If [a > b] then it returns the list [a (a-1) .. (b+1) b].*)
let range ?rev:(reverse : bool = false) (a : int) (b : int) : int list =
  let rec aux a b =
    if a > b
      then []
      else a :: aux (a + 1) b
    in
  let res =
  if a > b
    then rev (aux b a)
    else aux a b
    in
  if reverse then rev res else res

(** [fold_lefti f a xs]: similar to [fold_left] but with access to the indices.
    It computes, e.g., [f 2 (f 1 (f 0 a x0) x1) x2)]. *)
let fold_lefti (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res

(** [iteri2 f al bl]: iterates over two lists at the same time. *)
let iteri2 (f : int -> 'a -> 'b -> unit) (al : 'a list) (bl : 'b list) : unit =
  ignore (fold_left2 (fun i a b -> f i a b; i+1) 0 al bl)

(** [list_all_true bl]: returns [true] if all the booleans in the list [bl] are [true]. *)
let all_true (bl : bool list) : bool =
  for_all (fun b -> b = true) bl

(** [split_at n l]: splits the list [l] just before the element at index [n],
   and return the two sublists (which could be empty). *)
let split_at (i : int) (l : 'a list) : ('a list) * ('a list) =
  if i < 0 then failwith "List.split_list_at: negative index";
  let rec aux i acc l =
    match i, l with
    | 0, l -> (rev acc, l)
    | _, [] ->  failwith "List.split_list_at: index out of bound"
    | _, x::t -> aux (i-1) (x::acc) t
    in
  aux i [] l

(** [filteri p l]: implementation of [filteri], to be removed soon. *)
let list_filteri (p : int -> 'a -> bool ) (l : 'a list) : 'a list =
  let rec aux i acc = function
  | [] -> rev acc
  | x::l -> aux (i + 1) (if p i x then x::acc else acc) l
  in
  aux 0 [] l

(** [filter_selected indices l]: keeps only the elements from [l] whose indices belong to the list [indices].
   Negative indices are counted from the end. *)
let filter_selected (indices : int list) (l : 'a list) : 'a list =
  let len = length l in
  list_filteri (fun i _ -> mem i indices || mem (i-len) indices) l

(** [remove x xs]: remove item [x] from list [xs]. *)
let remove (x : 'a) (xs : 'a list) : 'a list =
  filter (fun y -> y <> x) xs

(** [remove_duplicates xs]: removes duplicates from list [xs]. *)
let remove_duplicates (lst : 'a list) =
  let unique_set = Hashtbl.create (length lst) in
  (* Warning: behavior would be slightly different if
    filter was implemented in a different way wrt
    order of evaluation *)
  filter (fun x ->
    if (Hashtbl.mem unique_set x)
      then false
      else (Hashtbl.replace unique_set x (); true)) lst

(** [update_nth f l i]: returns a copy of the list [l] where the element [x] at index [i] is replaced with [f x].
   NOTE: The index [i] must be valid. *)
let update_nth (i : int) (f : 'a -> 'a) (l : 'a list) : 'a list =
  mapi (fun j a -> if j = i then f a else a) l


(** [chop_after x xs]: gets a prefix of [xs] where all the elemenets after the item [x] are removed, including [x].
    If [x] does not occur in the list, then a copy of [xs] is returned *)
let rec chop_after (x : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | y::tl -> if y = x then [] else y:: chop_after x tl


(** [insert_sublist_at i l' l]: inserts the elements of [l'] at [l] starting from index [i].
     The index [i] should be in the range [0] to [length l], inclusive.
     The current item at index [i] in list [l] will have index equal to [i + length l'].
     One can insert a sublist also at index [length l] then the result will be [l @ l']. *)
let insert_sublist_at (i : int) (el : 'a list) (l : 'a list) : 'a list =
  if i = 0
    then el @ l
    else if i = length l
      then l @ el
    else
      let first_part, last_part = split_at i l in
      first_part @ el @ last_part

(** [insert_at i e l]: inserts an element [e] at index [i] in the list [l].
   The [index] should be in the range [0] to [length l], inclusive.
   In particular, if [index = length l], then the operation returns [l @ [e]]. *)
let insert_at (i : int) (e : 'a) (l : 'a list) : 'a list =
  insert_sublist_at i [e] l

(** [uncons l]: returns [(x,l')] such that [l = x::l'].
    NOTE: fails on empty lists. *)
let uncons (l : 'a list) : 'a * 'a list =
  match l with
  | [] -> failwith "List.uncons: the input list should not be empty."
  | x::l' -> (x,l')


(** [unlast l] returns [(l',x)] such that [l = l'@[x]].
    NOTE: fails on empty lists. *)
let unlast (l : 'a list) : 'a list * 'a =
  match rev l with
  | [] -> failwith "List.unlast: the input list should not be empty."
  | x::l' -> (rev l', x)

  (** [index_of x l]: returns [Some i], where [i] is the index of element [x] in list [l],
    or [None] if [x] does not belong to the list. *)
let index_of (x : 'a) (l : 'a list) : int option =
  fold_lefti (fun i acc y -> if x = y then Some i else acc) None l

(* Same as find_index, but only available in 5.1. *)
let find_index (f : 'a -> bool) (l : 'a list) : int option =
  fold_lefti (fun i acc y ->
    if Option.is_some acc then acc
    else begin
      if f y then Some i else acc
    end) None l

(** [Invalid_permutation]: exception raised by [check_permutation. *)
exception Invalid_permutation

(** [check_permutation nb order]: checks if the list [order] is a permutation of the range list [0,nb].
     If that's not the case then this transformation will raise [Invalid_permutation]. *)

let check_permutation (nb : int) (order : int list) : unit =
  if length order <> nb then raise Invalid_permutation;
  iter (fun k -> if not (mem k order) then raise Invalid_permutation) (range 0 (nb -1))

(** [reorder order l]: reorders the list [l] based on the permutation of indices [order].
   The i-th item from the output list corresponds to the j-th item from
   the input list, where [j] is the [i-th] item from the list [order].
   In other words, the output list is (using the bracket notation for nth):
   [ l[order[0]] ; l[order[1]]; ... l[order[length l-1]].
   The list [order] must have the same length as [l].
   All the values in the list [order] must be in the range [0 <= .. < length l]. *)
let reorder (order : int list) (l : 'a list) : 'a list =
  map (fun k -> match nth_opt l k with
              | None -> failwith "List.reorder: invalid index."
              | Some v -> v) order

(** [rotate n l]: retursn a copy of list where the [n] elements from the end
   of the list are moved to the front. If [l = l1 ++ l2] with [length l2 = n],
   then the output list is [l2 ++ l1]. *)
let rotate (n : int) (l : 'a list) : 'a list =
  if n > length l
   then failwith "List.rotate: the number of elements to rotate should not exceed the length of the input list.";
  let ls, rs = split_at n l in
  rs @ ls


(** [split_pairs_fst l]: returns the first element of the pair generated after calling [split] on [l]. *)
let split_pairs_fst (l : ('a * 'b) list) : 'a list =
  fst (split l)

(** [split_pairs_fst l]: returns the second element of the pair generated after calling [split] on [l]. *)
let split_pairs_snd (l : ('a * 'b) list) : 'b list =
  snd (split l)


(** [extract l start nb]: returns a sublist of [l] and the complement of that sublist in [l].
    The sublist contains all the elements of [l] whose indices fall in the range [start, start + nb) range. *)
let extract (l : 'a list) (start : int) (nb : int) : ('a list * 'a list) =
  let lfront, lback = try split_at start l with Failure _ ->  failwith "Xextract: please enter a valid starting index"in
  let ext, lback = try split_at nb lback with Failure _ -> failwith "List.extract: [start] + [nb] -1 should be a valid index for list [l]. "in
  ext , lfront @ lback

(** [extract_element l index]: extracts the element with [index] from list [l].*)
let extract_element (l : 'a list) (index : int) : ('a * 'a list) =
  let l, l1 = extract l index 1 in
  (nth l 0), l1

(** [drop_one_or_else f l] drops the first element from [l].
    If [l] is empty, returns [f ()] instead.
    *)
let drop_one_or_else (f : unit -> 'a list) (l : 'a list) : 'a list =
  match l with
  | [] -> f ()
  | _::l' -> l'

(** [drop n l]: drops the first [n] elements from [l].

  Fails if there are not enough elements to drop.
    *)
let rec drop (n : int) (l: 'a list) : 'a list =
  if n < 0 then failwith "drop: invalid negative argument"; (* could be outside of recursion *)
  if n = 0 then l else
    drop (n - 1) (drop_one_or_else (fun () -> failwith "drop: not enough elements to drop") l)

(** [take n l]: take the first [n] elements from [l]. *)
let rec take (n : int) (l: 'a list) : 'a list =
  if n < 0 then failwith "take: invalid negative argument"; (* could be outside of recursion *)
  if n = 0 then [] else
    match l with
    | [] -> failwith "take: not enough many elements to take"
    | x::l' -> x :: (take (n - 1) l')

(** [drop_last n l]: drops the last [n] elements from [l]. *)
let drop_last (n : int) (l: 'a list) : 'a list =
  let nb_take = length l - n in
  if nb_take < 0 then failwith "drop_last: list not long enough";
  take nb_take l

(** [take_last n l]: takes the last [n] elements from [l]. *)
let take_last (n : int) (l : 'a list) : 'a list =
  drop ((length l) - n) l

(** [diff l1 l2]: takes the difference between lists [l1] and [l2].
   *)
let diff l1 l2 = filter (fun x -> not (mem x l2)) l1

(** [reduce_left f l] is equivalent to [fold_left f (hd l) (tl l)]

  Fails if [l] is empty.
  *)
let reduce_left f l =
  match l with
  | [] -> failwith "reduce_left: empty list"
  | hd :: tl -> fold_left f hd tl

(** [reduce_right f l] is equivalent to [fold_right f (unlast l) (last l)]

  Fails if [l] is empty.
  *)
let reduce_right f l =
  let firsts, last = unlast l in
  fold_right f firsts last
