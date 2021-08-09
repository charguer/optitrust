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


(* inline a list in another list starting from the given index, it removes the elment at the given index *)
let rec insert_sublist_in_list (sublist : 'a list) (i : int) (xs : 'a list) = match xs with
| [] -> []
| h :: t -> if i = 0 then sublist @ t else h :: insert_sublist_in_list sublist (i-1) t


(* convert a list of strings to a string *)
let list_to_string ?(sep:string=";") ?(bounds:string list = ["[";"]"]) (l : string list) : string =
  let rec aux = function
    | [] -> ""
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^ sep ^ " " ^ aux (s2 :: sl)
  in
  (List.nth bounds 0) ^ aux l ^ (List.nth bounds 1)

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
let list_update_nth (transfo : 'a -> 'a) (al : 'a list) (n : int) : 'a list =
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

