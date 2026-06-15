(** Generic chunked-array sequence representation used to optimize [Mlist].

    [Alist] provides the internal sequence storage that replaces the previous
    list-backed representation used by [Mlist]. It is designed for efficient
    indexed access and local edits on sequences ranging from small lists to a
    few thousand elements.

    This module only manages the array-of-arrays representation. Mark handling
    belongs in [Mlist]. *)

(** Maximum number of values in one chunk, that is, an Internal Array. *)
let chunk_size : int = 32

let short_limit : int = 2 * chunk_size

(** ['a t] is a sequence represented by a top-level array of chunks.

    Invariants for [Short chunks]:
    - Length between 1 and [2 * chunk_size], inclusive.

    Invariants for [Long chunks]:
    - [chunks] is non-empty.
    - The sum of the length of the chunks must exceed  [chunk_size].
    - every chunk has length between 1 and [chunk_size], inclusive.
    - Any two consecutive chunks store more than [chunk_size] elements. *)
type 'a t =
  | Empty
  | Short of 'a array
  | Long of 'a array array

let check ( seq : 'a t) : unit =
  match seq with
  | Empty -> ()
  | Short items ->
    let len = Array.length items in
    if len = 0 || len > short_limit then
      failwith "Alist.check: invariant violation - short length out of bounds"
  | Long chunks ->
    if Array.length chunks = 0 then failwith "Alist.check: invariant violation - empty chunks array";
    let total_length = Array.fold_left (fun total chunk -> total + Array.length chunk) 0 chunks in
    if total_length <= chunk_size then
      failwith "Alist.check: invariant violation - long length should exceed chunk_size";
    let rec check_chunks i =
      if i >= Array.length chunks then () else
      let chunk_length = Array.length chunks.(i) in
      if chunk_length = 0 || chunk_length > chunk_size then
        failwith "Alist.check: invariant violation - chunk length out of bounds";
      if i > 0 then
        let prev_chunk_length = Array.length chunks.(i - 1) in
        if prev_chunk_length + chunk_length <= chunk_size then
          failwith "Alist.check: invariant violation - consecutive chunks should not be mergeable";
      check_chunks (i + 1)
    in
    check_chunks 0
let empty : 'a t =
  Empty

let is_empty (seq : 'a t) : bool =
  match seq with
  | Empty -> true
  | Short _ -> false
  | Long _ -> false

(** Private: Converts an array into a chunked list. *)
let chunks_of_array (items : 'a array) : 'a array array =
  let length = Array.length items in
  if length = 0 then [||] else
  let chunk_count = (length + chunk_size - 1) / chunk_size in
  Array.init chunk_count (fun chunk_index ->
    let start = chunk_index * chunk_size in
    let len = min chunk_size (length - start) in
    Array.init len (fun i -> items.(start + i)))

let length_of_chunks (chunks : 'a array array) : int =
  Array.fold_left (fun total chunk -> total + Array.length chunk) 0 chunks

let array_of_chunks (chunks : 'a array array) : 'a array =
  Array.concat (Array.to_list chunks)

(** Converts an array into an alist *)
let of_array (items : 'a array) : 'a t =
  let len = Array.length items in
  if len = 0 then Empty
  else if len <= short_limit then Short (Array.copy items)
  else Long (chunks_of_array items)


(** Converts a List into an alist // not the fastest implementation , might be improved later *)
let of_list (items : 'a list) : 'a t =
  items
  |> Array.of_list
  |> of_array

let to_list (seq : 'a t) : 'a list =
  match seq with
  | Empty -> []
  | Short items -> Array.to_list items
  | Long chunks -> List.flatten (Array.to_list (Array.map Array.to_list chunks))

let length (seq : 'a t) : int =
  match seq with
  | Empty -> 0
  | Short items -> Array.length items
  | Long chunks -> length_of_chunks chunks

  (* Private helper function to locate the chunk and item index for a given logical index.
     Raises exception for index out of bounds. *)
let locate (chunks : 'a array array) (index : int) : int * int =
  if index < 0 then failwith "Alist.locate: negative index";
  let rec aux chunk_index skipped =
    if chunk_index >= Array.length chunks then failwith "Alist.locate: index out of bounds";
    let chunk_length = Array.length chunks.(chunk_index) in
    if index < skipped + chunk_length
      then chunk_index, index - skipped
      else aux (chunk_index + 1) (skipped + chunk_length)
  in
  aux 0 0

let nth (seq : 'a t) (index : int) : 'a =
  match seq with
  | Empty -> failwith "Alist.nth: index out of bounds"
  | Short items ->
    if index < 0 || index >= Array.length items then failwith "Alist.nth: index out of bounds"
    else items.(index)
  | Long chunks ->
    let chunk_index, item_index = locate chunks index in
    chunks.(chunk_index).(item_index)

let nth_opt (seq : 'a t) (index : int) : 'a option =
  if index < 0 || index >= length seq then None else Some (nth seq index)

let mapi (f : int -> 'a -> 'b) (seq : 'a t) : 'b t =
  match seq with
  | Empty -> Empty
  | Short items -> Short (Array.mapi f items)
  | Long chunks ->
    let index = ref 0 in
    Long (                    (* Assumes left to right traversal order for Array.map *)
      Array.map (fun chunk ->
        Array.map (fun item ->
          let i = !index in
          incr index;
          f i item)
        chunk)
      chunks)

let map (f : 'a -> 'b) (seq : 'a t) : 'b t =
  mapi (fun _ item -> f item) seq

  (* Change to use adapters *)
let iteri (f : int -> 'a -> unit) (seq : 'a t) : unit =
  match seq with
  | Empty -> ()
  | Short items -> Array.iteri f items
  | Long chunks ->
    let index = ref 0 in
    Array.iter (fun chunk ->
      Array.iter (fun item ->
        let i = !index in
        incr index;
        f i item)
      chunk)
    chunks

let iter (f : 'a -> unit) (seq : 'a t) : unit =
  iteri (fun _ item -> f item) seq

  (* Normalizes an array of chunks by merging small chunks with their neighbors when possible. *)
  (* This function could be optimized further. *)
let normalize_chunks (chunks : 'a array array) : 'a array array =
  let rec aux acc current chunks =
    match chunks with
    | [] ->
      begin match current with
      | None -> List.rev acc
      | Some chunk -> List.rev (chunk :: acc)
      end
    | chunk :: chunks when Array.length chunk = 0 ->
      aux acc current chunks
    | chunk :: chunks ->
      match current with
      | None -> aux acc (Some chunk) chunks
      | Some current_chunk ->
        if Array.length current_chunk + Array.length chunk <= chunk_size
          then aux acc (Some (Array.append current_chunk chunk)) chunks
          else aux (current_chunk :: acc) (Some chunk) chunks
  in
  chunks
  |> Array.to_list
  |> aux [] None
  |> Array.of_list

let insert_array (index : int) (inserted : 'a array) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if index < 0 || index > seq_length then invalid_arg "Alist.insert_array_at";
  if Array.length inserted = 0 then seq else
  match seq with
  | Empty -> of_array inserted
  | Short items ->
    let item_length = Array.length items in
    let inserted_length = Array.length inserted in
    let new_length = item_length + inserted_length in
    let new_items =
      Array.init new_length (fun i ->
        if i < index then items.(i)
        else if i < index + inserted_length then inserted.(i - index)
        else items.(i - inserted_length))
    in
    if new_length <= short_limit then Short new_items else Long (chunks_of_array new_items)
  | Long chunks ->
    let chunk_index, item_index =
      if index = seq_length
        then
          let last_chunk_index = Array.length chunks - 1 in
          last_chunk_index, Array.length chunks.(last_chunk_index)
        else locate chunks index
    in
    let chunk = chunks.(chunk_index) in
    let chunk_length = Array.length chunk in
    let inserted_length = Array.length inserted in
    if chunk_length + inserted_length <= chunk_size then begin
      let new_chunk =
        Array.init (chunk_length + inserted_length) (fun i ->
          if i < item_index then chunk.(i)
          else if i < item_index + inserted_length then inserted.(i - item_index)
          else chunk.(i - inserted_length))
      in
      let new_chunks = Array.copy chunks in
      new_chunks.(chunk_index) <- new_chunk;
      Long new_chunks
    end else begin
      let left = Array.sub chunk 0 item_index in
      let right = Array.sub chunk item_index (chunk_length - item_index) in
      let replacement =
        let left_and_inserted = Array.append left inserted in
        let left_chunks = chunks_of_array left_and_inserted in
        if Array.length right = 0 then left_chunks else Array.append left_chunks [|right|]
      in
      let new_chunks =
        normalize_chunks (Array.concat [
          Array.sub chunks 0 chunk_index;
          replacement;
          Array.sub chunks (chunk_index + 1) (Array.length chunks - chunk_index - 1);
        ])
      in
      if Array.length new_chunks = 0 then Empty else Long new_chunks
    end

let insert_list (index : int) (inserted : 'a list) (seq : 'a t) : 'a t =
  insert_array index (Array.of_list inserted) seq

let insert (index : int) (item : 'a) (seq : 'a t) : 'a t =
  insert_array index [|item|] seq

let fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (seq : 'a t) : 'b =
  match seq with
  | Empty -> acc
  | Short items -> Array.fold_left f acc items
  | Long chunks ->
    let current = ref acc in
    Array.iter (fun chunk ->
      Array.iter (fun item -> current := f !current item) chunk)
      chunks;
    !current

 (* Maybe find is enough *)
let find_map (f : 'a -> 'b option) (seq : 'a t) : 'b option =
  let rec aux_chunks chunks chunk_index =
    if chunk_index >= Array.length chunks then None else
    let chunk = chunks.(chunk_index) in
    let rec aux_items item_index =
      if item_index >= Array.length chunk then aux_chunks chunks (chunk_index + 1) else
      match f chunk.(item_index) with
      | Some _ as res -> res
      | None -> aux_items (item_index + 1)
    in
    aux_items 0
  in
  match seq with
  | Empty -> None
  | Short items ->
    let rec aux_items item_index =
      if item_index >= Array.length items then None else
      match f items.(item_index) with
      | Some _ as res -> res
      | None -> aux_items (item_index + 1)
    in
    aux_items 0
  | Long chunks -> aux_chunks chunks 0

let for_all (p : 'a -> bool) (seq : 'a t) : bool =
  fold_left (fun acc item -> acc && p item) true seq

let rev (seq : 'a t) : 'a t =
  let array_rev (a : 'a array) : 'a array =
    let len = Array.length a in
    Array.init len (fun i -> a.(len - 1 - i))
  in
  match seq with
  | Empty -> Empty
  | Short items -> Short (array_rev items)
  | Long chunks ->
    let chunk_count = Array.length chunks in
    Long (Array.init chunk_count (fun i -> array_rev chunks.(chunk_count - 1 - i)))

let update_nth (index : int) (f : 'a -> 'a) (seq : 'a t) : 'a t =
  if index < 0 then invalid_arg "Alist.update_nth";
  match seq with
  | Empty -> invalid_arg "Alist.update_nth"
  | Short items ->
    if index >= Array.length items then invalid_arg "Alist.update_nth";
    let new_items = Array.copy items in
    new_items.(index) <- f new_items.(index);
    Short new_items
  | Long chunks ->
    let chunk_index, item_index = locate chunks index in
    let new_chunks = Array.copy chunks in
    let new_chunk = Array.copy chunks.(chunk_index) in
    new_chunk.(item_index) <- f new_chunk.(item_index);
    new_chunks.(chunk_index) <- new_chunk;
    Long new_chunks

let of_chunks (chunks : 'a array array) : 'a t =
  let chunks = normalize_chunks chunks in
  let len = length_of_chunks chunks in
  if len = 0 then Empty
  else if len <= short_limit then Short (array_of_chunks chunks)
  else Long chunks

  (* Private *)
let maybe_merge_boundary (left_chunks : 'a array array) (right_chunks : 'a array array) : 'a array array =
  normalize_chunks (Array.append left_chunks right_chunks)

let split (index : int) (seq : 'a t) : 'a t * 'a t =
  let seq_length = length seq in
  if index < 0 || index > seq_length then invalid_arg "Alist.split";
  match seq with
  | Empty -> Empty, Empty
  | Short items ->
    let left = Array.sub items 0 index in
    let right = Array.sub items index (Array.length items - index) in
    of_array left, of_array right
  | Long chunks ->
    if index = 0 then Empty, seq else
    if index = seq_length then seq, Empty else
    let chunk_index, item_index = locate chunks index in
    let chunk = chunks.(chunk_index) in
    let before_chunks = Array.sub chunks 0 chunk_index in
    let after_chunks = Array.sub chunks (chunk_index + 1) (Array.length chunks - chunk_index - 1) in
    let left_chunk = Array.sub chunk 0 item_index in
    let right_chunk = Array.sub chunk item_index (Array.length chunk - item_index) in
    let left_chunks = Array.append before_chunks [|left_chunk|] in
    let right_chunks = Array.append [|right_chunk|] after_chunks in
    of_chunks left_chunks, of_chunks right_chunks

let merge (seq1 : 'a t) (seq2 : 'a t) : 'a t =
  match seq1, seq2 with
  | Empty, _ -> seq2
  | _, Empty -> seq1
  | Short items1, Short items2 ->
    let len1 = Array.length items1 in
    let len2 = Array.length items2 in
    if len1 + len2 <= short_limit then Short (Array.append items1 items2)
    else Long (Array.append (chunks_of_array items1) (chunks_of_array items2))
  | Short items, Long chunks ->
    of_chunks (Array.append (chunks_of_array items) chunks)
  | Long chunks, Short items ->
    of_chunks (Array.append chunks (chunks_of_array items))
  | Long chunks1, Long chunks2 ->
    of_chunks (maybe_merge_boundary chunks1 chunks2)
