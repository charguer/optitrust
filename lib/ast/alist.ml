(** Generic chunked-array sequence representation used to optimize [Mlist].

    [Alist] provides the internal sequence storage that replaces the previous
    list-backed representation used by [Mlist]. It is designed for efficient
    indexed access and local edits on sequences ranging from small lists to a
    few thousand elements.

    This module only manages the array-of-arrays representation. Mark handling
    belongs in [Mlist].

    If changing this file, there is a test inside [tests_infra/alist] that allows checking the implementation via fuzzing.

    *)

(** Maximum number of values in one chunk, that is, an Internal Array. *)
let chunk_size : int = 32

(** Whether to use arrays or lists for short sequences. *)
let short_as_array : bool = false
let short_limit : int = 2 * chunk_size

(** ['a t] is a sequence represented by a top-level array of chunks.

    Invariants for [List]:
    - used if [short_as_array] is false
    - Length between 1 and [short_limit], inclusive.

    Invariants for [Short chunks]:
    - used if [short_as_array] is true
    - Length between 1 and [short_limit], inclusive.

    Invariants for [Long chunks]:
    - [chunks] is non-empty.
    - The sum of the length of the chunks must exceed  [chunk_size].
    - every chunk has length between 1 and [chunk_size], inclusive.
    - Any two consecutive chunks store more than [chunk_size] elements. *)
type 'a t =
  | Empty
  | List of 'a list
  | Short of 'a array
  | Long of 'a array array

let check ( seq : 'a t) : unit =
  match seq with
  | Empty -> ()
  | List items ->
    let len = List.length items in
    if len = 0 || len > short_limit then
      failwith "Alist.check: invariant violation - list length out of bounds"
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
  | List _ -> false
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

let short_of_owned_array (items : 'a array) : 'a t =
  let len = Array.length items in
  if len = 0 then Empty
  else if short_as_array then Short items
  else List (Array.to_list items)

let short_of_array (items : 'a array) : 'a t =
  let len = Array.length items in
  if len = 0 then Empty
  else if short_as_array then Short (Array.copy items)
  else List (Array.to_list items)

let short_of_list (items : 'a list) : 'a t =
  match items with
  | [] -> Empty
  | _ -> if short_as_array then Short (Array.of_list items) else List items

let of_list_with_length (length : int) (items : 'a list) : 'a t =
  if length < 0 then invalid_arg "Alist.of_list_with_length";
  if length = 0 then Empty
  else if length <= short_limit then short_of_list items
  else Long (chunks_of_array (Array.of_list items))

let short_to_array (seq : 'a t) : 'a array =
  match seq with
  | List items -> Array.of_list items
  | Short items -> items
  | Empty | Long _ -> invalid_arg "Alist.short_to_array"

let short_to_list (seq : 'a t) : 'a list =
  match seq with
  | List items -> items
  | Short items -> Array.to_list items
  | Empty | Long _ -> invalid_arg "Alist.short_to_list"

let chunks_of_short (seq : 'a t) : 'a array array =
  match seq with
  | List items -> chunks_of_array (Array.of_list items)
  | Short items -> chunks_of_array items
  | Empty | Long _ -> invalid_arg "Alist.chunks_of_short"

let list_length_at_most (limit : int) (items : 'a list) : bool =
  let rec aux remaining = function
    | [] -> true
    | _ :: xs -> remaining > 0 && aux (remaining - 1) xs
  in
  aux limit items

let insert_array_into_array (index : int) (inserted : 'a array) (items : 'a array) : 'a array =
  let item_length = Array.length items in
  let inserted_length = Array.length inserted in
  Array.init (item_length + inserted_length) (fun i ->
    if i < index then items.(i)
    else if i < index + inserted_length then inserted.(i - index)
    else items.(i - inserted_length))

let insert_array_into_list (index : int) (inserted : 'a array) (items : 'a list) : 'a list =
  let inserted_items = Array.to_list inserted in
  let rec aux i left_rev right =
    if i = 0 then List.rev_append left_rev (inserted_items @ right)
    else
      match right with
      | [] -> invalid_arg "Alist.insert_array_into_list"
      | x :: xs -> aux (i - 1) (x :: left_rev) xs
  in
  aux index [] items

let insert_list_into_list (index : int) (inserted : 'a list) (items : 'a list) : 'a list =
  let rec aux i left_rev right =
    if i = 0 then List.rev_append left_rev (inserted @ right)
    else
      match right with
      | [] -> invalid_arg "Alist.insert_list_into_list"
      | x :: xs -> aux (i - 1) (x :: left_rev) xs
  in
  aux index [] items

let insert_one_into_list (index : int) (inserted : 'a) (items : 'a list) : 'a list =
  let rec aux i left_rev right =
    if i = 0 then List.rev_append left_rev (inserted :: right)
    else
      match right with
      | [] -> invalid_arg "Alist.insert_one_into_list"
      | x :: xs -> aux (i - 1) (x :: left_rev) xs
  in
  aux index [] items

let update_list_nth (index : int) (f : 'a -> 'a) (items : 'a list) : 'a list =
  let rec aux i left_rev right =
    match right with
    | [] -> invalid_arg "Alist.update_nth"
    | x :: xs ->
      if i = 0 then List.rev_append left_rev (f x :: xs)
      else aux (i - 1) (x :: left_rev) xs
  in
  aux index [] items

let split_list_at (index : int) (items : 'a list) : 'a list * 'a list =
  let rec aux i left_rev right =
    if i = 0 then List.rev left_rev, right
    else
      match right with
      | [] -> invalid_arg "Alist.split_list_at"
      | x :: xs -> aux (i - 1) (x :: left_rev) xs
  in
  aux index [] items

let sub_list (start : int) (nb : int) (items : 'a list) : 'a list =
  let rec drop i xs =
    if i = 0 then xs else
    match xs with
    | [] -> invalid_arg "Alist.sub_list"
    | _ :: xs -> drop (i - 1) xs
  in
  let rec take i acc xs =
    if i = 0 then List.rev acc else
    match xs with
    | [] -> invalid_arg "Alist.sub_list"
    | x :: xs -> take (i - 1) (x :: acc) xs
  in
  take nb [] (drop start items)

let remove_list_range (start : int) (nb : int) (items : 'a list) : 'a list =
  let stop = start + nb in
  let rec aux i left_rev right =
    match right with
    | [] ->
      if i >= stop then List.rev left_rev
      else invalid_arg "Alist.remove_list_range"
    | x :: xs ->
      if i < start then aux (i + 1) (x :: left_rev) xs
      else if i < stop then aux (i + 1) left_rev xs
      else List.rev_append left_rev right
  in
  aux 0 [] items

let remove_array_range (start : int) (nb : int) (items : 'a array) : 'a array =
  let len = Array.length items in
  Array.init (len - nb) (fun i ->
    if i < start then items.(i) else items.(i + nb))

let remove_between_and_merge_list (left_index : int) (right_index : int) (f : 'a -> 'a -> 'a) (items : 'a list) : 'a list =
  let rec find_left i left_rev right =
    match right with
    | [] -> invalid_arg "Alist.remove_between_and_merge"
    | x :: xs ->
      if i = left_index then find_right left_rev x (i + 1) xs
      else find_left (i + 1) (x :: left_rev) xs
  and find_right left_rev left_item i right =
    match right with
    | [] -> invalid_arg "Alist.remove_between_and_merge"
    | x :: xs ->
      if i = right_index then List.rev_append left_rev (f left_item x :: xs)
      else find_right left_rev left_item (i + 1) xs
  in
  find_left 0 [] items

let remove_between_and_merge_array (left_index : int) (right_index : int) (f : 'a -> 'a -> 'a) (items : 'a array) : 'a array =
  let removed_count = right_index - left_index in
  let len = Array.length items in
  Array.init (len - removed_count) (fun i ->
    if i < left_index then items.(i)
    else if i = left_index then f items.(left_index) items.(right_index)
    else items.(i + removed_count))

let sub_with_mapped_ends_list (start : int) (count : int) (f_first : 'a -> 'a) (f_last : 'a -> 'a) (items : 'a list) : 'a list =
  let stop = start + count - 1 in
  let rec aux i acc right =
    if i > stop then List.rev acc else
    match right with
    | [] -> invalid_arg "Alist.sub_with_mapped_ends"
    | x :: xs ->
      if i < start then aux (i + 1) acc xs
      else
        let x =
          if i = start then f_first x
          else if i = stop then f_last x
          else x
        in
        aux (i + 1) (x :: acc) xs
  in
  aux 0 [] items

let extract_between_with_boundary_maps_list
  (left_index : int)
  (right_index : int)
  (f_rest : 'a -> 'a -> 'a)
  (f_first : 'a -> 'a)
  (f_last : 'a -> 'a)
  (items : 'a list) : 'a list * 'a list =
  let rec before i rest_rev right =
    match right with
    | [] -> invalid_arg "Alist.extract_between_with_boundary_maps"
    | x :: xs ->
      if i = left_index then between x (i + 1) rest_rev [f_first x] xs
      else before (i + 1) (x :: rest_rev) xs
  and between left_item i rest_rev extracted_rev right =
    match right with
    | [] -> invalid_arg "Alist.extract_between_with_boundary_maps"
    | x :: xs ->
      if i = right_index then
        let rest = List.rev_append rest_rev (f_rest left_item x :: xs) in
        let extracted = List.rev (f_last x :: extracted_rev) in
        rest, extracted
      else
        between left_item (i + 1) rest_rev (x :: extracted_rev) xs
  in
  before 0 [] items

let extract_list_range (start : int) (nb : int) (items : 'a list) : 'a list * 'a list =
  let rec before i prefix_rev right =
    if i = start then inside nb [] prefix_rev right else
    match right with
    | [] -> invalid_arg "Alist.extract"
    | x :: xs -> before (i + 1) (x :: prefix_rev) xs
  and inside remaining extracted_rev prefix_rev right =
    if remaining = 0 then
      List.rev_append prefix_rev right, List.rev extracted_rev
    else
      match right with
      | [] -> invalid_arg "Alist.extract"
      | x :: xs -> inside (remaining - 1) (x :: extracted_rev) prefix_rev xs
  in
  before 0 [] items

let extract_array_range (start : int) (nb : int) (items : 'a array) : 'a array * 'a array =
  let len = Array.length items in
  let rest =
    Array.init (len - nb) (fun i ->
      if i < start then items.(i) else items.(i + nb))
  in
  let extracted = Array.sub items start nb in
  rest, extracted

let extract_between_with_boundary_maps_array
  (left_index : int)
  (right_index : int)
  (f_rest : 'a -> 'a -> 'a)
  (f_first : 'a -> 'a)
  (f_last : 'a -> 'a)
  (items : 'a array) : 'a array * 'a array =
  let removed_count = right_index - left_index in
  let len = Array.length items in
  let rest =
    Array.init (len - removed_count) (fun i ->
      if i < left_index then items.(i)
      else if i = left_index then f_rest items.(left_index) items.(right_index)
      else items.(i + removed_count))
  in
  let extracted_count = removed_count + 1 in
  let extracted =
    Array.init extracted_count (fun i ->
      let item = items.(left_index + i) in
      if i = 0 then f_first item
      else if i = extracted_count - 1 then f_last item
      else item)
  in
  rest, extracted

let unlast_list (items : 'a list) : 'a t * 'a =
  let rec aux prefix_rev = function
    | [] -> invalid_arg "Alist.unlast"
    | [x] -> short_of_list (List.rev prefix_rev), x
    | x :: xs -> aux (x :: prefix_rev) xs
  in
  aux [] items

let merge_touching_list (f : 'a -> 'a -> 'a) (left : 'a list) (right : 'a list) : 'a list =
  match right with
  | [] -> invalid_arg "Alist.merge_touching"
  | first_right :: right_tail ->
    let rec aux left_rev = function
      | [] -> invalid_arg "Alist.merge_touching"
      | [last_left] -> List.rev_append left_rev (f last_left first_right :: right_tail)
      | x :: xs -> aux (x :: left_rev) xs
    in
    aux [] left

let merge_touching_array (f : 'a -> 'a -> 'a) (left : 'a array) (right : 'a array) : 'a array =
  let left_len = Array.length left in
  let right_len = Array.length right in
  if left_len = 0 || right_len = 0 then invalid_arg "Alist.merge_touching";
  Array.init (left_len + right_len - 1) (fun i ->
    if i < left_len - 1 then left.(i)
    else if i = left_len - 1 then f left.(left_len - 1) right.(0)
    else right.(i - left_len + 1))

let merge_touching_chunks (f : 'a -> 'a -> 'a) (left_chunks : 'a array array) (right_chunks : 'a array array) : 'a array array =
  if Array.length left_chunks = 0 || Array.length right_chunks = 0 then
    invalid_arg "Alist.merge_touching";
  let last_left_chunk_index = Array.length left_chunks - 1 in
  let last_left_chunk = left_chunks.(last_left_chunk_index) in
  let first_right_chunk = right_chunks.(0) in
  let last_left_item_index = Array.length last_left_chunk - 1 in
  let merged_boundary = f last_left_chunk.(last_left_item_index) first_right_chunk.(0) in
  let new_last_left_chunk = Array.copy last_left_chunk in
  new_last_left_chunk.(last_left_item_index) <- merged_boundary;
  let left_prefix =
    Array.sub left_chunks 0 last_left_chunk_index
  in
  let right_tail =
    let first_right_chunk_length = Array.length first_right_chunk in
    let remaining_chunks = Array.sub right_chunks 1 (Array.length right_chunks - 1) in
    if first_right_chunk_length = 1 then remaining_chunks
    else
      Array.append
        [|Array.sub first_right_chunk 1 (first_right_chunk_length - 1)|]
        remaining_chunks
  in
  Array.concat [left_prefix; [|new_last_left_chunk|]; right_tail]

let to_array (seq : 'a t) : 'a array =
  match seq with
  | Empty -> [||]
  | List items -> Array.of_list items
  | Short items -> items
  | Long chunks -> array_of_chunks chunks

(** Converts an array into an alist *)
let of_array (items : 'a array) : 'a t =
  let len = Array.length items in
  if len = 0 then Empty
  else if len <= short_limit then short_of_array items
  else Long (chunks_of_array items)


(** Converts a List into an alist // not the fastest implementation , might be improved later *)
let of_list (items : 'a list) : 'a t =
  match items with
  | [] -> Empty
  | _ ->
    of_list_with_length (List.length items) items

let to_list (seq : 'a t) : 'a list =
  match seq with
  | Empty -> []
  | List items -> items
  | Short items -> Array.to_list items
  | Long chunks -> List.flatten (Array.to_list (Array.map Array.to_list chunks))

let length (seq : 'a t) : int =
  match seq with
  | Empty -> 0
  | List items -> List.length items
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
  | List items ->
    if index < 0 then failwith "Alist.nth: index out of bounds";
    List.nth items index
  | Short items ->
    if index < 0 || index >= Array.length items then failwith "Alist.nth: index out of bounds"
    else items.(index)
  | Long chunks ->
    let chunk_index, item_index = locate chunks index in
    chunks.(chunk_index).(item_index)

let nth_opt (seq : 'a t) (index : int) : 'a option =
  if index < 0 then None else
  match seq with
  | Empty -> None
  | List items -> List.nth_opt items index
  | Short items ->
    if index >= Array.length items then None else Some items.(index)
  | Long chunks ->
    if index >= length seq then None else Some (nth seq index)

let mapi (f : int -> 'a -> 'b) (seq : 'a t) : 'b t =
  match seq with
  | Empty -> Empty
  | List items -> short_of_list (List.mapi f items)
  | Short items -> short_of_owned_array (Array.mapi f items)
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
  match seq with
  | Empty -> Empty
  | List items -> short_of_list (List.map f items)
  | Short items -> short_of_owned_array (Array.map f items)
  | Long chunks -> Long (Array.map (Array.map f) chunks)

  (* Change to use adapters *)
let iteri (f : int -> 'a -> unit) (seq : 'a t) : unit =
  match seq with
  | Empty -> ()
  | List items -> List.iteri f items
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
  | List items ->
    let item_length = List.length items in
    let inserted_length = Array.length inserted in
    let new_length = item_length + inserted_length in
    let new_items = insert_array_into_list index inserted items in
    if new_length <= short_limit then
      short_of_list new_items
    else
      Long (chunks_of_array (Array.of_list new_items))
  | Short items ->
    let item_length = Array.length items in
    let inserted_length = Array.length inserted in
    let new_length = item_length + inserted_length in
    let new_items = insert_array_into_array index inserted items in
    if new_length <= short_limit then short_of_owned_array new_items else Long (chunks_of_array new_items)
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
      let new_chunk = insert_array_into_array item_index inserted chunk in
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
  match inserted with
  | [] ->
    let seq_length = length seq in
    if index < 0 || index > seq_length then invalid_arg "Alist.insert_list";
    seq
  | _ ->
    let seq_length = length seq in
    if index < 0 || index > seq_length then invalid_arg "Alist.insert_list";
    match seq with
    | Empty -> of_list inserted
    | List items ->
      let new_length = List.length items + List.length inserted in
      let new_items = insert_list_into_list index inserted items in
      if new_length <= short_limit then short_of_list new_items
      else Long (chunks_of_array (Array.of_list new_items))
    | Short _ | Long _ ->
      insert_array index (Array.of_list inserted) seq

let insert (index : int) (item : 'a) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if index < 0 || index > seq_length then invalid_arg "Alist.insert";
  match seq with
  | Empty -> of_array [|item|]
  | List items ->
    let new_length = List.length items + 1 in
    let new_items = insert_one_into_list index item items in
    if new_length <= short_limit then short_of_list new_items
    else Long (chunks_of_array (Array.of_list new_items))
  | Short items ->
    let new_length = Array.length items + 1 in
    let new_items = insert_array_into_array index [|item|] items in
    if new_length <= short_limit then short_of_owned_array new_items
    else Long (chunks_of_array new_items)
  | Long _ ->
    insert_array index [|item|] seq

let push_front (item : 'a) (seq : 'a t) : 'a t =
  match seq with
  | Empty -> of_list [item]
  | List items ->
    if list_length_at_most (short_limit - 1) items then List (item :: items)
    else insert 0 item seq
  | Short items ->
    let len = Array.length items in
    if len < short_limit then
      short_of_owned_array (Array.init (len + 1) (fun i ->
        if i = 0 then item else items.(i - 1)))
    else insert 0 item seq
  | Long _ -> insert 0 item seq

let push_back (item : 'a) (seq : 'a t) : 'a t =
  match seq with
  | Empty -> of_list [item]
  | List items ->
    let rec aux len acc = function
      | [] ->
        let new_items = List.rev (item :: acc) in
        if len < short_limit then List new_items
        else Long (chunks_of_array (Array.of_list new_items))
      | x :: xs -> aux (len + 1) (x :: acc) xs
    in
    aux 0 [] items
  | Short items ->
    let len = Array.length items in
    if len < short_limit then
      short_of_owned_array (Array.init (len + 1) (fun i ->
        if i < len then items.(i) else item))
    else insert len item seq
  | Long _ -> insert (length seq) item seq

let fold_left (f : 'b -> 'a -> 'b) (acc : 'b) (seq : 'a t) : 'b =
  match seq with
  | Empty -> acc
  | List items -> List.fold_left f acc items
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
  | List items ->
    let rec aux = function
      | [] -> None
      | x :: xs ->
        match f x with
        | Some _ as res -> res
        | None -> aux xs
    in
    aux items
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
  | List items -> short_of_list (List.rev items)
  | Short items -> short_of_owned_array (array_rev items)
  | Long chunks ->
    let chunk_count = Array.length chunks in
    Long (Array.init chunk_count (fun i -> array_rev chunks.(chunk_count - 1 - i)))

let update_nth (index : int) (f : 'a -> 'a) (seq : 'a t) : 'a t =
  if index < 0 then invalid_arg "Alist.update_nth";
  match seq with
  | Empty -> invalid_arg "Alist.update_nth"
  | List items ->
    short_of_list (update_list_nth index f items)
  | Short items ->
    if index >= Array.length items then invalid_arg "Alist.update_nth";
    let new_items = Array.copy items in
    new_items.(index) <- f new_items.(index);
    short_of_owned_array new_items
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
  else if len <= short_limit then short_of_owned_array (array_of_chunks chunks)
  else Long chunks

  (* Private *)
let maybe_merge_boundary (left_chunks : 'a array array) (right_chunks : 'a array array) : 'a array array =
  normalize_chunks (Array.append left_chunks right_chunks)

let sub (start : int) (nb : int) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if start < 0 || nb < 0 || start + nb > seq_length then invalid_arg "Alist.sub";
  if nb = 0 then Empty else
  match seq with
  | Empty -> Empty
  | List items -> short_of_list (sub_list start nb items)
  | Short items -> short_of_owned_array (Array.sub items start nb)
  | Long chunks ->
    let stop = start + nb in
    let start_chunk_index, start_item_index = locate chunks start in
    let stop_chunk_index, stop_item_index =
      if stop = seq_length then
        let last_chunk_index = Array.length chunks - 1 in
        last_chunk_index, Array.length chunks.(last_chunk_index)
      else locate chunks stop
    in
    if start_chunk_index = stop_chunk_index then
      of_chunks [|Array.sub chunks.(start_chunk_index) start_item_index nb|]
    else begin
      let first_chunk = chunks.(start_chunk_index) in
      let last_chunk = chunks.(stop_chunk_index) in
      let first_part =
        Array.sub first_chunk start_item_index (Array.length first_chunk - start_item_index)
      in
      let middle_count = stop_chunk_index - start_chunk_index - 1 in
      let middle_chunks =
        if middle_count <= 0 then [||]
        else Array.sub chunks (start_chunk_index + 1) middle_count
      in
      let last_part = Array.sub last_chunk 0 stop_item_index in
      let parts =
        Array.concat [
          if Array.length first_part = 0 then [||] else [|first_part|];
          middle_chunks;
          if Array.length last_part = 0 then [||] else [|last_part|];
        ]
      in
      of_chunks parts
    end

let remove (start : int) (nb : int) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if start < 0 || nb < 0 || start + nb > seq_length then invalid_arg "Alist.remove";
  if nb = 0 then seq else
  match seq with
  | Empty -> Empty
  | List items -> short_of_list (remove_list_range start nb items)
  | Short items -> short_of_owned_array (remove_array_range start nb items)
  | Long chunks ->
    let stop = start + nb in
    let start_chunk_index, start_item_index = locate chunks start in
    let stop_chunk_index, stop_item_index =
      if stop = seq_length then
        let last_chunk_index = Array.length chunks - 1 in
        last_chunk_index, Array.length chunks.(last_chunk_index)
      else locate chunks stop
    in
    let before_chunks = Array.sub chunks 0 start_chunk_index in
    let after_chunks =
      Array.sub chunks (stop_chunk_index + 1) (Array.length chunks - stop_chunk_index - 1)
    in
    let replacement =
      if start_chunk_index = stop_chunk_index then begin
        let chunk = chunks.(start_chunk_index) in
        let left = Array.sub chunk 0 start_item_index in
        let right = Array.sub chunk stop_item_index (Array.length chunk - stop_item_index) in
        let merged = Array.append left right in
        if Array.length merged = 0 then [||] else [|merged|]
      end else begin
        let start_chunk = chunks.(start_chunk_index) in
        let stop_chunk = chunks.(stop_chunk_index) in
        let left = Array.sub start_chunk 0 start_item_index in
        let right = Array.sub stop_chunk stop_item_index (Array.length stop_chunk - stop_item_index) in
        Array.concat [
          if Array.length left = 0 then [||] else [|left|];
          if Array.length right = 0 then [||] else [|right|];
        ]
      end
    in
    of_chunks (Array.concat [before_chunks; replacement; after_chunks])

let extract (start : int) (nb : int) (seq : 'a t) : 'a t * 'a t =
  let seq_length = length seq in
  if start < 0 || nb < 0 || start + nb > seq_length then invalid_arg "Alist.extract";
  if nb = 0 then seq, Empty else
  match seq with
  | Empty -> Empty, Empty
  | List items ->
    let rest, extracted = extract_list_range start nb items in
    short_of_list rest, short_of_list extracted
  | Short items ->
    let rest, extracted = extract_array_range start nb items in
    short_of_owned_array rest, short_of_owned_array extracted
  | Long _ ->
    remove start nb seq, sub start nb seq

let remove_between_and_merge (left_index : int) (right_index : int) (f : 'a -> 'a -> 'a) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if left_index < 0 || right_index < left_index || right_index >= seq_length then
    invalid_arg "Alist.remove_between_and_merge";
  if left_index = right_index then update_nth left_index (fun x -> f x x) seq else
  match seq with
  | Empty -> invalid_arg "Alist.remove_between_and_merge"
  | List items ->
    short_of_list (remove_between_and_merge_list left_index right_index f items)
  | Short items ->
    short_of_owned_array (remove_between_and_merge_array left_index right_index f items)
  | Long _ ->
    of_array (remove_between_and_merge_array left_index right_index f (to_array seq))

let sub_with_mapped_ends (start : int) (count : int) (f_first : 'a -> 'a) (f_last : 'a -> 'a) (seq : 'a t) : 'a t =
  let seq_length = length seq in
  if start < 0 || count < 0 || start + count > seq_length then invalid_arg "Alist.sub_with_mapped_ends";
  if count = 0 then Empty else
  if count = 1 then
    short_of_list [f_last (f_first (nth seq start))]
  else
    match seq with
    | Empty -> Empty
    | List items ->
      short_of_list (sub_with_mapped_ends_list start count f_first f_last items)
    | Short items ->
      short_of_owned_array (Array.init count (fun i ->
        let item = items.(start + i) in
        if i = 0 then f_first item
        else if i = count - 1 then f_last item
        else item))
    | Long _ ->
      let items = to_array seq in
      of_array (Array.init count (fun i ->
        let item = items.(start + i) in
        if i = 0 then f_first item
        else if i = count - 1 then f_last item
        else item))

let extract_between_with_boundary_maps
  (left_index : int)
  (right_index : int)
  (f_rest : 'a -> 'a -> 'a)
  (f_first : 'a -> 'a)
  (f_last : 'a -> 'a)
  (seq : 'a t) : 'a t * 'a t =
  let seq_length = length seq in
  if left_index < 0 || right_index <= left_index || right_index >= seq_length then
    invalid_arg "Alist.extract_between_with_boundary_maps";
  match seq with
  | Empty -> invalid_arg "Alist.extract_between_with_boundary_maps"
  | List items ->
    let rest, extracted =
      extract_between_with_boundary_maps_list left_index right_index f_rest f_first f_last items
    in
    short_of_list rest, short_of_list extracted
  | Short items ->
    let rest, extracted =
      extract_between_with_boundary_maps_array left_index right_index f_rest f_first f_last items
    in
    short_of_owned_array rest, short_of_owned_array extracted
  | Long _ ->
    remove_between_and_merge left_index right_index f_rest seq,
    sub_with_mapped_ends left_index (right_index - left_index + 1) f_first f_last seq

let uncons (seq : 'a t) : 'a * 'a t =
  match seq with
  | Empty -> invalid_arg "Alist.uncons"
  | List items ->
    begin match items with
    | [] -> invalid_arg "Alist.uncons"
    | x :: xs -> x, short_of_list xs
    end
  | Short items ->
    let len = Array.length items in
    if len = 0 then invalid_arg "Alist.uncons";
    items.(0), short_of_owned_array (Array.sub items 1 (len - 1))
  | Long chunks ->
    let first_chunk = chunks.(0) in
    let first_chunk_length = Array.length first_chunk in
    let item = first_chunk.(0) in
    let new_chunks =
      if first_chunk_length = 1 then
        Array.sub chunks 1 (Array.length chunks - 1)
      else begin
        let chunks = Array.copy chunks in
        chunks.(0) <- Array.sub first_chunk 1 (first_chunk_length - 1);
        chunks
      end
    in
    item, of_chunks new_chunks

let unlast (seq : 'a t) : 'a t * 'a =
  match seq with
  | Empty -> invalid_arg "Alist.unlast"
  | List items -> unlast_list items
  | Short items ->
    let len = Array.length items in
    if len = 0 then invalid_arg "Alist.unlast";
    short_of_owned_array (Array.sub items 0 (len - 1)), items.(len - 1)
  | Long chunks ->
    let last_chunk_index = Array.length chunks - 1 in
    let last_chunk = chunks.(last_chunk_index) in
    let last_chunk_length = Array.length last_chunk in
    let item = last_chunk.(last_chunk_length - 1) in
    let new_chunks =
      if last_chunk_length = 1 then
        Array.sub chunks 0 last_chunk_index
      else begin
        let chunks = Array.copy chunks in
        chunks.(last_chunk_index) <- Array.sub last_chunk 0 (last_chunk_length - 1);
        chunks
      end
    in
    of_chunks new_chunks, item

let split (index : int) (seq : 'a t) : 'a t * 'a t =
  if index < 0 then invalid_arg "Alist.split";
  match seq with
  | Empty ->
    if index = 0 then Empty, Empty else invalid_arg "Alist.split"
  | List items ->
    let left, right = split_list_at index items in
    short_of_list left, short_of_list right
  | Short items ->
    if index > Array.length items then invalid_arg "Alist.split";
    let left = Array.sub items 0 index in
    let right = Array.sub items index (Array.length items - index) in
    of_array left, of_array right
  | Long chunks ->
    let seq_length = length_of_chunks chunks in
    if index > seq_length then invalid_arg "Alist.split";
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
  | (List _ | Short _), (List _ | Short _) ->
    let len = length seq1 + length seq2 in
    if len <= short_limit then begin
      if short_as_array then
        Short (Array.append (short_to_array seq1) (short_to_array seq2))
      else
        List (short_to_list seq1 @ short_to_list seq2)
    end else
      Long (Array.append (chunks_of_short seq1) (chunks_of_short seq2))
  | (List _ | Short _), Long chunks ->
    of_chunks (Array.append (chunks_of_short seq1) chunks)
  | Long chunks, (List _ | Short _) ->
    of_chunks (Array.append chunks (chunks_of_short seq2))
  | Long chunks1, Long chunks2 ->
    of_chunks (maybe_merge_boundary chunks1 chunks2)

let merge_touching (f : 'a -> 'a -> 'a) (seq1 : 'a t) (seq2 : 'a t) : 'a t =
  match seq1, seq2 with
  | Empty, _ | _, Empty -> invalid_arg "Alist.merge_touching"
  | List items1, List items2 ->
    of_list (merge_touching_list f items1 items2)
  | Short items1, Short items2 ->
    of_array (merge_touching_array f items1 items2)
  | (List _ | Short _), Long chunks2 ->
    of_chunks (merge_touching_chunks f (chunks_of_short seq1) chunks2)
  | Long chunks1, (List _ | Short _) ->
    of_chunks (merge_touching_chunks f chunks1 (chunks_of_short seq2))
  | Long chunks1, Long chunks2 ->
    of_chunks (merge_touching_chunks f chunks1 chunks2)
  | _ ->
    of_array (merge_touching_array f (to_array seq1) (to_array seq2))
