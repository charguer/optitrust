open PPrint

let printf = Printf.printf
let sprintf = Printf.sprintf

(* print the ast *)
let node (s : string) : document = string s ^^ blank 1

let surround : document -> document -> document -> document = surround 2 1

let parens (d : document) : document = soft_surround 2 1 lparen d rparen

let print_list ?(sep : string = ";") (dl : document list) : document =
  surround lbracket (separate (string sep ^^ break 1) dl) rbracket

let print_object (dl : document list) : document =
  surround lbrace (separate (comma ^^ break 1) dl) rbrace

let print_pair (d1 : document) (d2 : document) : document =
  parens (d1 ^^ comma ^/^ d2)

let document_to_string (d : document) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b d;
  Buffer.contents b

(* fold left with access to the indices
  [foldi f a xs] computes  [ f 2 (f 1 (f 0 a x0) x1) x2) ] *)
let foldi (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = List.fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res

let foldi2 (f : int -> 'a -> 'b -> 'c -> 'a) (a : 'a) (bl : 'b list) (cl : 'c list) : 'a =
  let (_, res) = List.fold_left2 (fun (i, a) b c -> (i + 1, f i a b c)) (0, a) bl cl in
  res


(* inline a list in another list starting from the given index, it removes the elment at the given index *)
let rec insert_sublist_in_list (sublist : 'a list) (i : int) (xs : 'a list) = match xs with
| [] -> []
| h :: t -> if i = 0 then sublist @ t else h :: insert_sublist_in_list sublist (i-1) t


(* convert a list of strings to a string *)
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

(* convert a list of docs to doc *)
let doc_list_to_doc ?(empty : document = underscore) ?(sep:document = semi) ?(bounds:document list = [string "["; string "]"]) (l : document list) : document =
  let rec aux = function
    | [] -> empty
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^^ sep ^^ string " " ^^ aux (s2 :: sl)
  in
  (List.nth bounds 0) ^^ aux l ^^ (List.nth bounds 1)

(* check if all the elements of a list fulfill predicate f *)
let list_all_true (bl : bool list) : bool =
  List.for_all (fun b -> b = true) bl

(* split list at index n and returns the splitted parts *)
let split_list_at (n : int) (al : 'a list) : ('a list) * ('a list) =
  if n < 0 then failwith "split_list_at: negative index";
  let rec aux n acc l =
    match n, l with
    | 0, l -> (List.rev acc, l)
    | _, [] ->  failwith "split_list_at: index out of bound"
    | _, x::t -> aux (n-1) (x::acc) t
    in
  aux n [] al

(* return the list where the nth element is transformed *)
let update_nth (transfo : 'a -> 'a) (al : 'a list) (n : int) : 'a list =
  List.mapi (fun i a -> if i = n then transfo a else a) al

module type DebugSig = sig

  exception Breakpoint

  val counter : int ref

  val backtrace : (unit -> unit) -> unit

end

(* module used for debugging *)
module Debug = struct

  exception Breakpoint

  let counter = ref 0

  let backtrace f =
    try f() with Breakpoint ->
      flush stdout;
      let s = Printexc.get_backtrace() in
      Printf.eprintf "%s\n" s
end

(* generate a positive integer *)
let fresh_generator () : (unit -> int) =
  let n = ref 0 in
  fun () ->
    incr n;
    !n

(* used for unit tests *)
let failure_expected (f : unit -> unit) : unit =
  try f(); failwith "failure_expected: the operation was supposed to fail but it didn't"
  with _ -> ()


(* remove all the elements from a list starting from a element x *)
let rec chop_list_after x xs = match xs with
  (* | [] -> failwith "did not find x" *)
  | [] -> []
  | y::tl -> if y = x then [] else y:: chop_list_after x tl


(* return a list of positive integers starting with a and ending with b *)
let range a b =
  let rec aux a b =
    if a > b then [] else a :: aux(a + 1) b
  in
    if a > b then List.rev (aux b a) else aux a b

(* generate special optitrust labels *)
let optitrust_label : string =
  let rnd_nb = Random.int 1000 in
  "__optitrust__" ^ (string_of_int rnd_nb)


(* LATER: arthur use stdlib *)
(* copy of List.filteri used for old versions of Ocaml *)
let list_filteri p l =
  let rec aux i acc = function
  | [] -> List.rev acc
  | x::l -> aux (i + 1) (if p i x then x::acc else acc) l
  in
  aux 0 [] l

(* remove all the elements from a list which belong to another list *)
let filter_not_selected (indices :int list) (list : 'a list) : 'a list =
  (* List.filteri *)
  list_filteri (fun i _ -> List.mem i indices) list


let list_remove_duplicate xs x = if List.mem x xs then xs else x :: xs

let list_remove_duplicates xs = List.rev (List.fold_left list_remove_duplicate [] xs)

(* [list_intersect xs1 xs2] computs the intersection of the sets describe dy the two lists *)
let list_intersect (xs1:'a list) (xs2:'a list) : 'a list =
  List.filter (fun x -> List.mem x xs1) xs2
  (*  LATER:
    let h = Hashtbl.create (List.length xs1) in
    List.filter (fun x -> Hashtbl.mem h x) xs2
  *)

  (* [list_union xs1 xs2] returns a list that extends [xs1] with the elements from [xs2] that are not in [xs1] already

    let h = Hashtbl.create (List.length xs1) in
    List.fold_left (fun acc x -> if Hashtbl.mem h x then acc else x::acc) xs1 xs2
   *)




(* maps on functions *)
module Fun_map = Map.Make(String)
type 'a funmap = 'a Fun_map.t

(* sets on int lists *)
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

(* foldi for int list sets *)
let intl_set_foldi (f : int -> int list -> 'a -> 'a) (ils : ilset)
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

(* return the list where the nth element is transformed *)
let map_at (transfo : 'a -> 'a) (al : 'a list) (n : int) : 'a list =
  List.mapi (fun i a -> if i = n then transfo a else a) al

(* [insert_sublist_at index el l] inserts the elemtns [el] at index [index] in the list [l].
   The [index] should be in the range [0] to [length l], inclusive.
   In particular, if [index = length l], then the operation returns [l @ el]. *)
let insert_sublist_at (index : int) (el : 'a list) (l : 'a list) : 'a list =
  if index = List.length l
    then l @ el
    else foldi (fun i acc x ->
      if i = ((List.length l) - index - 1) then el @ x :: acc else x :: acc) [] (List.rev l)

(* [insert_at index e l] inserts an element [e] at index [index] in the list [l].
   The [index] should be in the range [0] to [length l], inclusive.
   In particular, if [index = length l], then the operation returns [l @ [e]]. *)
let insert_at (index : int) (e : 'a) (l : 'a list) : 'a list =
  insert_sublist_at index [e] l

(* [extract start stop l] returns a pair of lists:    start inclusive, start+nb exclusive
   - the list with items outside of the range from [start] to [stop]
   - the list with items inside the range from [start] to [stop].
   TODO: the two returns values should ideally be in the other order, that is, the slice comes first
   TODO: specify the length of the slice, e.g. [stop - start (?+1)].
   TODO: specify if the indices [start] and [stop] are inclusive or exclusive;
      in general, the convention that works best is to to take start inclusive and stop exclusive;
      but in fact, the even better convention is to take [start] and [nb] as argument,
      and deduce the index of the last item as [start+nb-1]. It is much more frequent
      to know [nb] than to know [stop]. *)
let extract (start : int) (stop : int) (l : 'a list) : ('a list * 'a list) =
  let rev_start = (List.length l - (stop + 1)) in
  let rev_stop = (List.length l - (start + 1)) in
  foldi (fun i (f_acc, s_acc) x -> if i >= rev_start && i <= rev_stop then (f_acc, x :: s_acc) else (x :: f_acc, s_acc)) ([],[]) (List.rev l)

(* get the first and the last element of the list [l]
   TODO: I've never seen this fucntion before.
   if the list as size 2, you should match it against [x;y];
   else you could call get_first and get_last two separate functions;
   but it's very rare to need this. *)
let get_first_last (l : 'a list) : 'a * 'a =
  let n = List.length l in
  if n = 0 then failwith "get_first_last: empty list"
    else (List.nth l 0, List.nth l (n-1))

(* [uncons l] returns [(x,l')] such that [l = x::l']. If fails on empty lists. *)
let uncons (l : 'a list) : 'a * 'a list =
  match l with
  | [] -> invalid_arg "uncons"
  | x::l' -> (x,l')

(* [unlast l] returns [(l',x)] such that [l = l'@[x]]. If fails on empty lists. *)
let unlast (l : 'a list) : 'a list * 'a =
  match List.rev l with
  | [] -> invalid_arg "uncons"
  | x::l' -> (List.rev l', x)

