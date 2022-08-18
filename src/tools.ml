open PPrint

(******************************************************************************)
(*                        Extensions to backtrace                             *)
(******************************************************************************)
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

(* [failure_expected f]: executes the unit function [f], and checks that
   it raises the exception [Failure_expected_did_not_fail]. If it does
   not, then an error is triggered. *)
let failure_expected (f : unit -> unit) : unit =
  try
    f();
    raise Failure_expected_did_not_fail
  with
    | Failure_expected_did_not_fail -> failwith "failure_expected: the operation was supposed to fail but it didn't"
    |_ -> ()


(******************************************************************************)
(*                          Extensions to Pprint                              *)
(******************************************************************************)

(* [print_node s]: converts string the [s] to pprint document and add a trailing space *)
let print_node (s : string) : document =
  string s ^^ blank 1

(* [parens d]: adds parentheses around a pprint document. *)
let parens (d : document) : document =
  soft_surround 2 1 lparen d rparen

(* [print_list ~sep dl]: displays a list of items, separated with [sep] and a space. By default, [sep] is a semicolon. *)
let print_list ?(sep : string = ";") (dl : document list) : document =
  surround 2 1 lbracket (separate (string sep ^^ break 1) dl) rbracket

(* [list_to_doc]: advanced version of [print_list] that supports special treatment for empty lists.
    LATER: merge with [print_list], making [empty] an optional argument? *)
let list_to_doc ?(empty : document = empty) ?(sep:document = semi) ?(bounds = [empty; empty]) (l : document list) : document =
  let lb = List.nth bounds 0 in 
  let rb = List.nth bounds 1 in 
  let rec aux = function
    | [] -> empty
    | [s] -> s
    | s1 :: s2 :: sl -> s1 ^^ sep ^^ string " " ^^ aux (s2 :: sl)
  in
   lb ^^ aux l ^^ rb

(* [print_object dl]: prints a list of documents in the form [{x, y, z}]. *)
let print_object (dl : document list) : document =
  surround 2 1 lbrace (separate (comma ^^ break 1) dl) rbrace

(* [print_pair dl]: prints documents [d1] and [d2] in the form [(d1,d2)]. *)
let print_pair (d1 : document) (d2 : document) : document =
  parens (d1 ^^ comma ^/^ d2)

(* [document_to_string d]: converts a document into a string. *)
let document_to_string ?(width:PPrint.requirement=80) (d : document) : string =
  let b = Buffer.create 15 in (* the vast majority of string representation have less than 15 chars *)
  ToBuffer.pretty 0.9 width b d;
  Buffer.contents b

(******************************************************************************)
(*                 Extensions for fresh name generation                       *)
(******************************************************************************)

(* [fresh_generator()]: generates a function that can be used to return
   the next integer at each invokation. *)
(* LATER: this option ?init seems boggus, because it resets the generator at every call...  *)
let fresh_generator ?(init : bool = false) () : (unit -> int) =
  let n = ref 0 in
  fun () ->
    if init then n := 0 else incr n;
    !n

(* [resetable_fresh_generator()]: returns a pair of a generator and its reset function *)
let resetable_fresh_generator () : (unit -> int) * (unit -> unit) =
  let n = ref 0 in
  (fun () -> incr n; !n), (fun () -> n := 0)


(******************************************************************************)
(*                          Extensions for string                             *)
(******************************************************************************)

(* [list_to_string ?sep ?bounds l]: returns a string representation of a list of strings.
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

(* [pattern_matches pattern s]: checks whether the string [s] matches the regexp [pattern]. *)
let pattern_matches (pattern : string) (s : string) : bool =
  try let _ = Str.search_forward (Str.regexp pattern) s 0 in true
  with Not_found -> false

(* [string_subst pattern replacement s]: replaces all occurences of [pattern] inside [s]
   with the string [replacement]. *)
let string_subst (pattern : string) (replacement : string) (s : string) : string =
  Str.global_replace (Str.regexp_string pattern) replacement s

(* [string_subst_first pattern replacement s]: replaces the first occurence of [pattern] inside [s]
   with the string [replacement]. *)
let string_subst_first (pattern : string) (replacement : string) (s : string) : string =
  Str.replace_first (Str.regexp_string pattern) replacement s

(* [spaces nb]: returns a string made of [nb] spaces *)
let spaces (nb : int) : string =
  String.make nb ' '

(* [add_prefix prefix ss]: adds [prefix] to all the strings in the list [ss] *)
let add_prefix (prefix : string) (indices : string list) : string list =
    List.map (fun x -> prefix ^ x) indices


(* [clean_class_name class_name]: avoids printing the angle bracket for template class constructors. *)
let clean_class_name (class_name : string) : string =
  let splitted_string = Str.split (Str.regexp_string "<") class_name in 
  if List.length splitted_string < 2 then class_name
    else List.nth splitted_string 0
 

(******************************************************************************)
(*                          Extensions for Time                               *)
(******************************************************************************)

(* [miliseconds_between t0 t1]: computes the difference between the dates
   [t0] and [t1], as measured by [Unix.time], and returns the result in
   the form of an integer number of miliseconds. *)
let milliseconds_between (t0 : float) (t1 : float) : int =
  int_of_float (1000. *. (t1 -. t0))


(******************************************************************************)
(*                        Extensions for Pervasives                           *)
(******************************************************************************)

(* [int_to_bool i]: converts an integer (only 0 or 1) into a boolean. *)
let int_to_bool (i : int) : bool =
  match i with
  | 0 -> false
  | 1 -> true
  | _ -> failwith "Tools.int_to_bool: converts only 0 and 1 to boolean values"

(* [bool_of_var s]: converts a string to a boolean if [s] is "true" or "false" otherwise do nothing *)
let bool_of_var (s : string) : bool option =
  try Some (bool_of_string s )
  with | Invalid_argument _ -> None


(******************************************************************************)
(*                          Extensions to Option                              *)
(******************************************************************************)

(* [option_map]: applies [f] on optional objects *)
let option_map (f : 'a -> 'b) (o : 'a option) : 'b option =
  match o with
  | None -> None
  | Some v -> Some (f v)


(* [unsome x_opt]: extracts the underlying object of [x_opt] is there is one such object. *)
let unsome (x_opt : 'a option) : 'a =
  match x_opt with
  | Some x -> x
  | None -> failwith "Tools.unsome: please check if the optional is different from none before calling this function"


(******************************************************************************)
(*                          Extensions for Hashtbl                            *)
(******************************************************************************)


(* [hashtbl_map_values f h]: applies [f] on the values of hashtable [h]. *)
let hashtbl_map_values (f : 'a -> 'b -> 'c) (h : ('a,'b) Hashtbl.t) : ('a,'c) Hashtbl.t =
  let r = Hashtbl.create (Hashtbl.length h) in
  Hashtbl.iter (fun k v -> Hashtbl.add r k (f k v)) h;
  r

(* [hashtbl_keys_to_list h]: returns all the keys of [h] as a list. *)
let hashtbl_keys_to_list (h : ('a, 'b) Hashtbl.t) : 'a list =
  Hashtbl.fold (fun k _ acc ->
    match Hashtbl.find_opt h k with 
    | Some _ -> k :: acc
    | None -> acc
  ) h []

(* [hashtbl_to_list h]: returns all pairs of key/value of [h] as a list. *)
let hashtbl_to_list (h : ('a, 'b) Hashtbl.t) : ('a * 'b) list =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []