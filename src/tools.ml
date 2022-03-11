open PPrint

let printf = Printf.printf
let sprintf = Printf.sprintf


(*-----------Extensions for List of integers------------*)

(* [range a b] returns the list of integers between [a] and [b] inclusive.
   If [a > b] then it returns the list [a (a-1) .. (b+1) b].*)
let range ?(rev : bool = false) (a : int) (b : int) : int list =
  let rec aux a b =
    if a > b
      then []
      else a :: aux (a + 1) b
    in
  let res =
  if a > b
    then List.rev (aux b a)
    else aux a b
    in
  if rev then List.rev res else res

(*-----------Extensions to lists-------------*)

(* LATER: create a module Xlist with all these functions,
   and remove the prefix [list_] for functions that have them. *)

(* [fold_lefti f a xs] is a [List.fold_left] with access to the indices.
   It computes, e.g., [f 2 (f 1 (f 0 a x0) x1) x2)]. *)
let fold_lefti (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = List.fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res

(* [fold_righti f a xs] is a [List.fold_right] with access to the indices.
   It computes, e.g. [f 0 (f 1 (f 2 a x2) x1) x0)]. *)
let fold_righti (f : int -> 'b -> 'a -> 'a) (bl : 'b list) (a : 'a) : 'a =
  let (_, res) = List.fold_right (fun b (i,a) -> (i + 1, f i b a)) bl (0, a) in
  res

let iteri2 (f : int -> 'a -> 'b -> unit) (al : 'a list) (bl : 'b list) : unit =
  ignore (List.fold_left2 (fun i a b -> f i a b; i+1) 0 al bl)

(* [fold_lefti2] is a [List.fold_left2] with access to the indices. *)
let fold_lefti2 (f : int -> 'a -> 'b -> 'c -> 'a) (a : 'a) (bl : 'b list) (cl : 'c list) : 'a =
  let (_, res) = List.fold_left2 (fun (i, a) b c -> (i + 1, f i a b c)) (0, a) bl cl in
  res

(* [list_all_true bl] returns [true] if all the booleans in the list [bl] are [true]. *)
let list_all_true (bl : bool list) : bool =
  List.for_all (fun b -> b = true) bl

(* [split_list_at n l] splits the list [l] just before the element at index [n],
   and returns the two sublists (which could be empty). *)
let split_list_at (i : int) (l : 'a list) : ('a list) * ('a list) =
  if i < 0 then failwith "split_list_at: negative index";
  let rec aux i acc l =
    match i, l with
    | 0, l -> (List.rev acc, l)
    | _, [] ->  failwith "split_list_at: index out of bound"
    | _, x::t -> aux (i-1) (x::acc) t
    in
  aux i [] l

(* [update_nth f l i] returns a copy of the list [l] where the element
   [x] at index [i] is replaced with [f x]. The index [i] must be valid. *)
let update_nth (i : int) (f : 'a -> 'a) (l : 'a list) : 'a list =
  List.mapi (fun j a -> if j = i then f a else a) l

(* LATER: replace this function with List.filteri, offered by new versions of OCaml. *)
let list_filteri p l =
  let rec aux i acc = function
  | [] -> List.rev acc
  | x::l -> aux (i + 1) (if p i x then x::acc else acc) l
  in
  aux 0 [] l

(* [filter_not_selected indices l] keeps only the elements from [l] whose indices belong
    to the list [indices]. *)
let filter_selected (indices : int list) (l : 'a list) : 'a list =
  list_filteri (fun i _ -> List.mem i indices) l

(* [list_remove x xs] removes the item [x] from the list [xs]. *)
let list_remove (x : 'a) (xs : 'a list) : 'a list =
  List.filter (fun y -> y <> x) xs

(* [list_remove_duplicates xs] returns a list made of the items from [xs]
   with the duplicates removed. *)
let list_remove_duplicates (lst : 'a list) =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []

(* [list_intersect xs1 xs2] computs the intersection of the lists [xs1] and [xs2].
   The order of the return elements is not specified. *)
let list_intersect (xs1:'a list) (xs2:'a list) : 'a list =
  List.filter (fun x -> List.mem x xs1) xs2
  (* LATER: could be optimized like list_remove_duplicates, e.g.:
    let h = Hashtbl.create (List.length xs1) in
    List.filter (fun x -> Hashtbl.mem h x) xs2 *)

(* [list_chop_after x xs] returns a prefix of [xs] where all elements
    after the item [x] are removed, including [x]. If [x] does not
    occur in the list, then the original list [xs] is returned. *)
let rec list_chop_after (x : 'a) (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | y::tl -> if y = x then [] else y:: list_chop_after x tl

(* [insert_sublist_at i el l] inserts the elemtns [el] at index [i] in the list [l].
   The [i] should be in the range [0] to [length l], inclusive.
   In particular, if [i = length l], then the operation returns [l @ el]. *)
let insert_sublist_at (i : int) (el : 'a list) (l : 'a list) : 'a list =
  if i = 0 then el @ l
  else if i = List.length l
    then l @ el
  else
    let first_part, last_part = split_list_at i l in
    first_part @ el @ last_part

(* [insert_at i e l] inserts an element [e] at index [i] in the list [l].
   The [index] should be in the range [0] to [length l], inclusive.
   In particular, if [index = length l], then the operation returns [l @ [e]]. *)
let insert_at (i : int) (e : 'a) (l : 'a list) : 'a list =
  insert_sublist_at i [e] l

(* [uncons l] returns [(x,l')] such that [l = x::l']. If fails on empty lists. *)
let uncons (l : 'a list) : 'a * 'a list =
  match l with
  | [] -> failwith "uncons: the input list should not be empty."
  | x::l' -> (x,l')

(* [unlast l] returns [(l',x)] such that [l = l'@[x]]. If fails on empty lists. *)
let unlast (l : 'a list) : 'a list * 'a =
  match List.rev l with
  | [] -> failwith "unlast: the input list should not be empty."
  | x::l' -> (List.rev l', x)

(* LATER: in recent versions of OCaml, this function is in the List stdlib module. *)
let find_map f t =
  let rec loop = function
    | [] -> None
    | x :: l ->
        match f x with
        | None -> loop l
        | Some _ as r -> r
  in
  loop t

(* [index_of x l] returns the [Some i], where [i] is the index of element
   [x] in list [l], or [None] if [x] does not belong to the list. *)
let index_of (x : 'a) (l : 'a list) : int option =
  fold_lefti (fun i acc y -> if x = y then Some i else acc) None l

exception Invalid_permutation

(* [check_permutation nb order] checks if the list [order] is a permutation
   of the list of the [nb] first integers (zero inclusive, [nb] exclusive).
   It raises [Invalid_permutation] if it is not the case. *)
let check_permutation (nb : int) (order : int list) : unit =
  if List.length order <> nb then raise Invalid_permutation;
  List.iter (fun k -> if not (List.mem k order) then raise Invalid_permutation) (range 0 (nb -1))

(* [list_reorder order l] returns a reordered version of the list [l].
   The i-th item from the output list corresponds to the j-th item from
   the input list, where [j] is the [i-th] item from the list [order].
   In other words, the output list is (using the bracket notation for nth):
   [ l[order[0]] ; l[order[1]]; ... l[order[length l-1]].
   The list [order] must have the same length as [l].
   All the values in the list [order] must be in the range [0 <= .. < length l]. *)
let list_reorder (order : int list) (l : 'a list) : 'a list =
  List.map (fun k -> match List.nth_opt l k with
                     | None -> failwith "list_reorder: invalid index."
                    | Some v -> v) order

(* [list_rotate n l] returns a copy of list where the [n] elements from the end
   of the list are moved to the front. If [l = l1 ++ l2] with [length l2 = n],
   then the output list is [l2 ++ l1]. *)
let list_rotate (n : int) (l : 'a list) : 'a list =
  if n > List.length l
   then failwith "list_rotate: the number of elements to rotate should not exceed the length of the input list.";
  let ls, rs = split_list_at n l in
  rs @ ls




(*-----------Extensions to Backtrace-------------*)

module type DebugSig = sig

  exception Breakpoint

  val counter : int ref

  val backtrace : (unit -> unit) -> unit

end


module Debug = struct

  exception Breakpoint

  let counter = ref 0

  let backtrace f =
    try f() with Breakpoint ->
      flush stdout;
      let s = Printexc.get_backtrace() in
      Printf.eprintf "%s\n" s
end


exception Failure_expected_did_not_fail

(* [failure_expected f] executes the unit function [f], and checks that
   it raises the exception [Failure_expected_did_not_fail]. If it does
   not, then an error is triggered. *)
let failure_expected (f : unit -> unit) : unit =
  try
    f();
    raise Failure_expected_did_not_fail
  with
    | Failure_expected_did_not_fail -> failwith "failure_expected: the operation was supposed to fail but it didn't"
    |_ -> ()


(*-----------Extensions to Pprint-------------*)

let print_node (s : string) : document =
  string s ^^ blank 1

(* [parens d] adds parentheses around a document. *)
let parens (d : document) : document =
  soft_surround 2 1 lparen d rparen

(* [print_list ?sep dl] display a list of items, separated with [sep] and a space.
   By default, [sep] is a semicolon. *)
let print_list ?(sep : string = ";") (dl : document list) : document =
  surround 2 1 lbracket (separate (string sep ^^ break 1) dl) rbracket

(* [list_to_doc] is a generalization of [print_list] with special treatment for
   the empty list. LATER: merge with [print_list], making [empty] an optional argument? *)
let list_to_doc ?(empty : document = underscore) ?(sep:document = semi) ?(bounds:document list = [string "["; string "]"]) (l : document list) : document =
  let rec aux = function
    | [] -> empty
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^^ sep ^^ string " " ^^ aux (s2 :: sl)
  in
  (List.nth bounds 0) ^^ aux l ^^ (List.nth bounds 1)

(* [print_object dl] displays a list of documents in the form [{x, y, z}]. *)
let print_object (dl : document list) : document =
  surround 2 1 lbrace (separate (comma ^^ break 1) dl) rbrace

(* [print_pair dl] displays a document in the form [(d1,d2)]. *)
let print_pair (d1 : document) (d2 : document) : document =
  parens (d1 ^^ comma ^/^ d2)

(* [document_to_string d] converts a document into a string *)
let document_to_string ?(width:PPrint.requirement=80) (d : document) : string =
  let b = Buffer.create 15 in (* the vast majority of string representation have less than 15 chars *)
  ToBuffer.pretty 0.9 width b d;
  Buffer.contents b

(*-----------Extensions for Fresh name generation-------------*)

(* [fresh_generator()] generate a function that can be used to return
   the next integer at each invokation. *)
(* LATER: this option ?init seems boggus, because it resets the generator
   at every call...  *)
let fresh_generator ?(init : bool = false) () : (unit -> int) =
  let n = ref 0 in
  fun () ->
    if init then n := 0 else incr n;
    !n

(* [reset_generator ()] reset the generator to avoid id clashes when reparsing *)
let reset_generator () : unit =
  let _x = fresh_generator ~init:true () in ()
(* TODO: this function is boggus remove it *)

(* [resetable_fresh_generator()] returns a pair of a generator and its reset function *)
let resetable_fresh_generator () : (unit -> int) * (unit -> unit) =
  let n = ref 0 in
  (fun () -> incr n; !n), (fun () -> n := 0)



(*-----------Extensions for Strings-------------*)

(* [list_to_string ?sep ?bounds l] gives a string representation of list of string.
   By default, it produces [  [s1; s2; s3] ]. *)
let list_to_string ?(sep:string=";") ?(bounds:string list = ["[";"]"]) (l : string list) : string =
  let (bl,br) = match bounds with
    | [bl; br] -> (bl,br)
    | _ -> failwith "list_to_string: bounds argument must be a list of length 2"
    in
  let rec aux = function
    | [] -> ""
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^ sep ^ " " ^ aux (s2 :: sl)
  in
  bl ^ aux l ^ br

(* [pattern_matches pattern s] check whether the string [s] matches the regexp [pattern]. *)
let pattern_matches (pattern : string) (s : string) : bool =
  try let _ = Str.search_forward (Str.regexp pattern) s 0 in true
  with Not_found -> false

(* [string_subst pattern replacement s] replace all occurences of [pattern] inside [s]
   with the string [replacement]. *)
let string_subst (pattern : string) (replacement : string) (s : string) : string =
  Str.global_replace (Str.regexp_string pattern) replacement s

(* [string_subst_first pattern replacement s] replace the first occurence of [pattern] inside [s]
   with the string [replacement]. *)
let string_subst_first (pattern : string) (replacement : string) (s : string) : string =
  Str.replace_first (Str.regexp_string pattern) replacement s

(* [spaces nb] returns a string made of [nb] spaces *)
let spaces (nb : int) : string =
  String.make nb ' '

(*-----------Extensions for Time-------------*)

(* [miliseconds_between t0 t1] compute the difference between the dates
   [t0] and [t1], as measured by [Unix.time], and returns the result in
   the form of an integer number of miliseconds. *)
let milliseconds_between (t0 : float) (t1 : float) : int =
  int_of_float (1000. *. (t1 -. t0))


(*-----------Extensions for File and Marshall-------------*)

(* [serialize_to_file filename t] dumps the object [obj] of type 'a into file [filename]. *)
let serialize_to_file (filename : string) (obj : 'a) : unit =
  let out_file = open_out filename in
  Marshal.to_channel out_file obj []

(* [unserialize_from_file filename] reconstruct the object previously dumped in file [filename]. *)
let unserialize_from_file (filename : string) : 'a =
  let in_file = open_in filename in
  Marshal.from_channel in_file

(* [is_file_newer_than filename1 filename2] returns true if the file [filename1] has a
   modification date greater than that of the file [filename2]. *)
let is_file_newer_than (filename1 : string) (filename2 : string) : bool =
  let t_f1 = Unix.((stat filename1).st_mtime) in
  let t_f2 = Unix.((stat filename2).st_mtime) in
  t_f1 >= t_f2


(*-----------Extensions for Pervasives-------------*)

(* [int_to_bool i] convert an integer (only 0 or 1) into a boolean. *)
let int_to_bool (i : int) : bool =
  match i with
  | 0 -> false
  | 1 -> true
  | _ -> failwith "int_to_bool: converts only 0 and 1 to boolean values"



(*-----------Extensions for Option-------------*)

let option_map (f : 'a -> 'b) (o : 'a option) : 'b option =
  match o with
  | None -> None
  | Some v -> Some (f v)

(*-----------Extensions for Hashtbl-------------*)

let hashtbl_map_values (f : 'a -> 'b -> 'c) (h : ('a,'b) Hashtbl.t) : ('a,'c) Hashtbl.t =
  let r = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.add r k (f k v)) h;
  r


(*-----------Extensions for Maps-------------*)

(* maps on functions -- LATER: move to ast.ml  *)

module Fun_map = Map.Make(String)

type 'a funmap = 'a Fun_map.t


(*-----------Extensions for Sets of list of integers-------------*)
(* LATER: check where this is used; then if we keep it fix the documentation *)

module IntList =
  struct
    type t = int list
    let rec compare il il' =
      match il, il' with
      | [], [] -> 0
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
      | i :: il, i' :: il' ->
         begin match Stdlib.compare i i' with
         | 0 -> compare il il'
         | c -> c
         end
  end

module IntListSet = Set.Make(IntList)
type ilset = IntListSet.t

(* [intl_set_fold_lefti] is a [fold_lefti] for int list sets. *)
let intl_set_fold_lefti (f : int -> int list -> 'a -> 'a) (ils : ilset)
  (a : 'a) : 'a =
  let (_, res) =
    IntListSet.fold (fun il (i, a) -> (i + 1, f i il a)) ils (0, a)
  in
  res

(* helper function for union of maps of int list sets *)
let ilset_funmap_union_aux (_ : Fun_map.key) (ils : ilset)
  (ils' : ilset) : ilset option =
  Some (IntListSet.union ils ils')

let ilset_funmap_union : ilset funmap -> ilset funmap -> ilset funmap =
  Fun_map.union ilset_funmap_union_aux

let (+@) = ilset_funmap_union

(* convert a string to a boolean if s is "true" or "false" otherwise do nothing *)
let bool_of_var (s : string) : bool option =
  try Some (bool_of_string s )
  with | Invalid_argument _ -> None


(* [unsome x_opt] extracts an objects outsied the optional trm its belongs to *)
let unsome (x_opt : 'a option) : 'a =
  match x_opt with
  | Some x -> x
  | None -> failwith "unsome: please check if the optional is different from none before calling this function"

