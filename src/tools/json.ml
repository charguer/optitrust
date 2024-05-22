
(* [Json]: A module for producing JS definitions *)

open PPrint

(* [t]: representation of a json object *)
type t =
  | Raw of string  (* string without quotes *)
  | Str of string  (* string with quotes, with or without <br> for line returns *)
  | Bool of bool
  | Int of int
  | Float of float
  | List of t list
  | Obj of (t * t) list

(* Auxiliary functions *)

(* [quote x]: adds quotes around string x, and escape the quotes *)
let quote (x:string) : string =
  let x = Str.global_replace (Str.regexp "\"") "\\\"" x in
  "\"" ^ x ^ "\""

(* [to_html_newlines] converts line returns [\n] into [<br/>] tags *)

let to_html_newlines (x:string) : string =
  Str.global_replace (Str.regexp "\n") "<br/>" x

(* [to_backslashn] converts line returns [\n] characters into '\' followed with 'n' *)

let to_backslashn (x:string) : string =
  Str.global_replace (Str.regexp "\n") "\\n" x

(* Smart constructors *)

(* [raw x]: creates a piece of Json, as a raw string without quotes *)
let raw (x:string) : t =
  Raw x

(* [base64 str]: creates the JS expression [window.atob("...")],
   which is equal to the string [str]. *)
let base64 (str :string) : t =
  Raw (Printf.sprintf "window.atob(\"%s\")" (Base64.encode_exn str))

(* [str x]: creates a Json string, to be displayed in quotes,
   and optionally converts line returns in one way or another (should not do both) *)
let str ?(html_newlines : bool = false) ?(escape_newlines : bool = true) (x:string) : t =
  let x = if escape_newlines then to_backslashn x else x in
  let x = if html_newlines then to_html_newlines x else x in
  Str x

(* [int n]: creates a Json int *)
let int (n:int) : t =
  Int n

(* [float f]: creates a Json float *)
let float (f:float) : t =
  Float f

(* [bool b]: creates a Json bool *)
let bool (b:bool) : t =
  Bool b

(* [list l]: creates a Json list from a list of Json elements *)
let list (xs:t list) : t =
  List xs

(* [listof k l]: creates a Json list from a list of OCaml elements *)
let listof (k:'a -> t) (xs:'a list) : t =
  list (List.map k xs)

(* [optionof k l]: creates a Json value or 'undefined' from an OCaml option value *)
let optionof (k:'a -> t) (o:'a option) : t =
  match o with
  | None -> raw "undefined"
  | Some v -> k v

(* [obj xvs]: creates a Json object from a list of pairs of Json elements *)
let obj (xvs:(t*t) list) : t =
  Obj xvs

(* [obj_quoted_keys xvs]: creates a Json object from a list of pairs of string and elements *)
let obj_quoted_keys (xvs:(string*t) list) : t =
 obj (List.map (fun (x,v) -> (Str x, v)) xvs)

(* [empty_obj]: the void object is defined as [{}]. *)
let empty_obj : t =
  Obj []

(* [print_object dl]: prints a list of documents in a  JavaScript Json datastructure *)
let print_object (dl : document list) : document =
  surround 2 1 lbrace (separate (comma ^^ break 1) dl) rbrace

(* [to_doc j]: converts a Json.t to a pprint document *)
let rec to_doc (j : t) : document =
  match j with
  | Raw s -> string s
  | Str s -> string (quote s)
  | Bool b-> string (string_of_bool b)
  | Int i -> string (string_of_int i)
  | Float f-> string (string_of_float f)
  | List l -> Tools.list_to_doc ~sep:(string ", ") ~bounds:[lbracket; rbracket] (List.map to_doc l)
  | Obj o -> Tools.print_object (List.map (fun (k,j) -> to_doc k ^^ string ": " ^^ to_doc j) o)

(* [to_string j]: converts a Json.t to a string *)
let to_string (j: t) : string =
  Tools.document_to_string (to_doc j)


