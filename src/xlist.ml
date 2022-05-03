(* [fold_lefti f a xs]: similar to [List.fold_left] but with access to the indices.
    It computes, e.g., [f 2 (f 1 (f 0 a x0) x1) x2)]. *)
let fold_lefti (f : int -> 'a -> 'b -> 'a) (a : 'a) (bl : 'b list) : 'a =
  let (_, res) = List.fold_left (fun (i, a) b -> (i + 1, f i a b)) (0, a) bl in
  res

(* [iteri2 f al bl]: iterate over two lists at the same time *)
let iteri2 (f : int -> 'a -> 'b -> unit) (al : 'a list) (bl : 'b list) : unit =
  ignore (List.fold_left2 (fun i a b -> f i a b; i+1) 0 al bl)

(* [list_all_true bl]: return [true] if all the booleans in the list [bl] are [true] *)
let all_true (bl : bool list) : bool =
  List.for_all (fun b -> b = true) bl

(* [split_at n l]: split the list [l] just before the element at index [n],
   and return the two sublists (which could be empty) *)
let split_at (i : int) (l : 'a list) : ('a list) * ('a list) =
  if i < 0 then failwith "Xlist.split_list_at: negative index";
  let rec aux i acc l =
    match i, l with
    | 0, l -> (List.rev acc, l)
    | _, [] ->  failwith "Xlist.split_list_at: index out of bound"
    | _, x::t -> aux (i-1) (x::acc) t
    in
  aux i [] l
