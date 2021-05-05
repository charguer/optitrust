open PPrint

(* print the ast *)
let node (s : string) : document = string s ^^ blank 1

let surround : document -> document -> document -> document = surround 2 1

let parens (d : document) : document = soft_surround 2 1 lparen d rparen

let print_list (dl : document list) : document =
  surround lbracket (separate (semi ^^ break 1) dl) rbracket

let print_pair (d1 : document) (d2 : document) : document =
  parens (d1 ^^ comma ^/^ d2)

let document_to_string (d : document) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b d;
  Buffer.contents b

let (++) = List.append

(* fold left with access to the indices
  [foldi f a xs] computes  [ f 2 (f 1 (f 0 a x0) x1) x2) ] *)
let foldi (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = List.fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res
(* maps on functions TODO: find why not reusing maps *)
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

(* Inline a list in another list starting from the given index, it removes the elment at the given index *)
let rec insert_sublist_in_list (sublist : 'a list) (i : int) (xs : 'a list) = match xs with 
| [] -> []
| h :: t -> if i = 0 then sublist @ t else h :: insert_sublist_in_list sublist (i-1) t

(* insert a after rank n in the list *)
let rec list_insert (n : int) (a : 'a) (al : 'a list) : 'a list =
  if n < 0 then a :: al else List.hd al :: list_insert (n - 1) a (List.tl al)
(* Removes one list element at given index *)
let rec list_remove_at (i : int) (list : 'a list) : 'a list = match list with 
  | [] -> failwith "Empty list"
  | x :: xs -> if i = 0 then xs else x :: list_remove_at (i-1) xs 

let list_remove_at_set (ys : int list) (xs : 'a list) : 'a list = List.fold_left (fun acc y -> list_remove_at y acc) xs ys 

(* Inline a list in another list starting from the given index, it doesn't remove the elment at the given index *)
let rec insert_sublist_at (sublist : 'a list) (i : int) (xs : 'a list) : 'a list =  match xs with 
  | [] -> failwith "Empty list"
  | h :: t -> if i = 0 then sublist @ h :: t else h :: insert_sublist_at sublist (i-1) t 

let rec insert_before x local_l list = match list with 
| [] -> []
| hd :: tl -> if hd = x then local_l @ hd :: tl else hd :: (insert_before x local_l tl)

let rec insert_list keys_list temp_field_list field_list1 = match keys_list with 
| [] -> field_list1
| hd :: tl -> let field_list1 = insert_before hd (List.hd temp_field_list) field_list1 in insert_list tl (List.tl temp_field_list ) field_list1

let list_remove x xs = List.filter (fun y -> y <> x) xs 


let list_remove_set ys xs = List.fold_left (fun acc y -> list_remove y acc) xs ys 

let move_fields_after x local_l l = 
let l = list_remove_set local_l  l in 
let rec aux acc = function 
| [] -> acc (* raise an error x not part of the list *)
| hd :: tl -> if hd = x then aux (local_l @ hd :: acc) tl (* local_l @ hd :: acc @ tl *)
else aux (hd :: acc) tl 
in aux [] (List.rev l)

(* 
  - tail recursive approach => more efficient 
  - non-tail rec => easier to read 

     let rec insert_after x xs l =
        match l with
        | [] -> error
        | y::q -> if x = y then xs@l else y::(insert_after x xs q)
*)

let move_fields_before x local_l l = 
  let l = list_remove_set local_l l in 
  let rec aux acc = function 
    | [] -> acc
    | hd :: tl -> 
        if hd = x
          then aux (hd :: local_l @ acc) tl 
          else aux (hd :: acc) tl 
    in
  aux [] (List.rev l)

(* return the last element of a list together with its index *)
let last (l : 'a list) : int * 'a =
  let rec aux n = function
    | [] -> failwith "last: empty list"
    | [a] -> (n, a)
    | _ :: b :: al -> aux (n + 1) (b :: al)
  in
  aux 0 l

(* before operator *)
    let get_index (a : 'a) (al : 'a list) : int option =
      let rec aux (n : int) = function
        | [] -> None
        | a' :: _ when a = a' -> Some n
        | _ :: al -> aux (n + 1) al
      in
      aux 0 al


(* Initialize a two arrays for the json ast and source code *)
let initialization (out_prefix : string) : unit =
    let file_js = out_prefix ^ ".js" in 
    let out_js = open_out file_js in 
    let content = PPrint.string "var" ^^ PPrint.blank 1 ^^ PPrint.string "contents" ^^ PPrint.equals ^^ PPrint.brackets PPrint.empty in
    let source =  PPrint.string "var" ^^ PPrint.blank 1 ^^ PPrint.string "source" ^^ PPrint.equals ^^ PPrint.brackets PPrint.empty in
    PPrintEngine.ToChannel.pretty 0.9 80 out_js content;
    PPrintEngine.ToChannel.pretty 0.9 80 out_js source

