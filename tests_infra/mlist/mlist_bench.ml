open Optitrust

module Old = Mlist_old
module New = Mlist

module type MLIST = sig
  type 'a t

  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val length : 'a t -> int
  val nth : 'a t -> int -> 'a
  val nth_opt : 'a t -> int -> 'a option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val iter : ('a -> unit) -> 'a t -> unit
  val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val split : ?left_bias:bool -> int -> 'a t -> 'a t * 'a t
  val merge : 'a t -> 'a t -> 'a t
  val extract : ?start_left_bias:bool -> ?stop_left_bias:bool -> int -> int -> 'a t -> 'a t * 'a t
  val remove : int -> int -> 'a t -> 'a t
  val insert_at : int -> 'a -> 'a t -> 'a t
  val insert_sublist_at : int -> 'a list -> 'a t -> 'a t
  val push_front : 'a -> 'a t -> 'a t
  val push_back : 'a -> 'a t -> 'a t
  val replace_at : int -> 'a -> 'a t -> 'a t
  val update_nth : int -> ('a -> 'a) -> 'a t -> 'a t
  val insert_mark_at : int -> string -> 'a t -> 'a t
  val insert_marks_at : int -> string list -> 'a t -> 'a t
  val remove_mark : string -> 'a t -> 'a t
  val flatten_marks : ('a -> 'b) -> (string list -> 'b) -> 'a t -> 'b list
end

module Old_adapter : MLIST = struct
  include Old
end

module New_adapter : MLIST = struct
  include New
end

type pattern =
  | Beginning
  | Middle
  | End
  | Deterministic
  | Mark_heavy
  | Transform_like

type bench = {
  operation : string;
  size : int;
  pattern : pattern;
  repetitions : int;
  tier : string;
  old_setup : unit -> unit -> int;
  new_setup : unit -> unit -> int;
}

let normal_repetitions = 10

let large_repetitions = 3

let normal_sizes = [16; 32; 64; 1_000; 10_000; 100_000]

let large_sizes = [1_000_000]

let string_of_pattern = function
  | Beginning -> "beginning"
  | Middle -> "middle"
  | End -> "end"
  | Deterministic -> "deterministic"
  | Mark_heavy -> "mark-heavy"
  | Transform_like -> "transform-like"

let index_for_pattern size = function
  | Beginning -> 0
  | Middle -> size / 2
  | End -> max 0 (size - 1)
  | Deterministic -> if size = 0 then 0 else (size * 17 + 13) mod size
  | Mark_heavy -> size / 2
  | Transform_like -> size / 3

let position_for_pattern size pattern =
  match pattern with
  | End -> size
  | _ -> index_for_pattern size pattern

let nb_for_pattern size start =
  if size = 0 then 0 else
  let remaining = size - start in
  max 0 (min remaining (max 1 (size / 5)))

let input size : int list =
  List.init size (fun i -> i)

let sublist_for_size size : int list =
  let len = if size < 32 then 4 else 32 in
  List.init len (fun i -> -i - 1)

let access_count size : int =
  min size 2048

let deterministic_random_index size i : int =
  if size = 0 then 0 else ((i * 7919) + 104729) mod size

let mark_every size : int =
  if size <= 64 then 1 else max 1 (size / 64)

let string_of_marks marks =
  "[" ^ String.concat ";" marks ^ "]"

let consume_int_list xs : int =
  List.fold_left ( + ) 0 xs

let consume_string_list xs : int =
  List.fold_left (fun acc s -> acc + String.length s) 0 xs

let make_for_impl (module M : MLIST) operation size pattern : unit -> unit -> int =
  let add_marks (ml : int M.t) : int M.t =
    let every = mark_every size in
    let rec aux index acc =
      if index > size then acc
      else
        let marks = ["mark-" ^ string_of_int index; "boundary"] in
        aux (index + every) (M.insert_marks_at index marks acc)
    in
    aux 0 ml
  in
  let consume_pair_lengths (left, right : int M.t * int M.t) : int =
    M.length left + M.length right
  in
  let consume_indexed_access (ml : int M.t) (indices : int list) : int =
    List.fold_left (fun acc index -> acc + M.nth ml index) 0 indices
  in
  let with_ml f : unit -> unit -> int =
    fun () ->
      let ml = M.of_list (input size) in
      fun () -> f ml
  in
  let with_marked_ml f : unit -> unit -> int =
    fun () ->
      let ml = M.of_list (input size) in
      let ml = add_marks ml in
      fun () -> f ml
  in
  let index = index_for_pattern size pattern in
  let position = position_for_pattern size pattern in
  let nb = nb_for_pattern size position in
  match operation with
  | "of_list" ->
    let items = input size in
    fun () -> fun () -> M.length (M.of_list items)
  | "to_list" ->
    with_ml (fun ml -> consume_int_list (M.to_list ml))
  | "length" ->
    with_ml M.length
  | "nth" ->
    if size = 0 then
      with_ml (fun ml -> match M.nth_opt ml 0 with None -> 0 | Some x -> x)
    else
      with_ml (fun ml -> M.nth ml index)
  | "nth_opt" ->
    with_ml (fun ml -> match M.nth_opt ml index with None -> 0 | Some x -> x)
  | "map" ->
    with_ml (fun ml -> M.fold_left ( + ) 0 (M.map succ ml))
  | "mapi" ->
    with_ml (fun ml -> M.fold_left ( + ) 0 (M.mapi (fun i x -> i + x) ml))
  | "iter" ->
    with_ml (fun ml ->
      let acc = ref 0 in
      M.iter (fun x -> acc := !acc + x) ml;
      !acc)
  | "fold_left" ->
    with_ml (fun ml -> M.fold_left ( + ) 0 ml)
  | "nth_sequential_scan" ->
    with_ml (fun ml ->
      let count = access_count size in
      let indices = List.init count (fun i -> i) in
      consume_indexed_access ml indices)
  | "nth_stride_scan" ->
    with_ml (fun ml ->
      let count = access_count size in
      let stride = if count = 0 then 1 else max 1 (size / count) in
      let indices = List.init count (fun i -> if size = 0 then 0 else (i * stride) mod size) in
      consume_indexed_access ml indices)
  | "nth_random_scan" ->
    with_ml (fun ml ->
      let count = access_count size in
      let indices = List.init count (deterministic_random_index size) in
      consume_indexed_access ml indices)
  | "split" ->
    with_marked_ml (fun ml -> consume_pair_lengths (M.split position ml))
  | "merge" ->
    fun () ->
      let ml = M.of_list (input size) in
      let left, right = M.split position ml in
      fun () -> M.length (M.merge left right)
  | "extract" ->
    with_marked_ml (fun ml -> consume_pair_lengths (M.extract position nb ml))
  | "remove" ->
    with_marked_ml (fun ml -> M.length (M.remove position nb ml))
  | "insert_at" ->
    with_marked_ml (fun ml -> M.length (M.insert_at position (-1) ml))
  | "insert_sublist_at" ->
    with_marked_ml (fun ml -> M.length (M.insert_sublist_at position (sublist_for_size size) ml))
  | "push_front" ->
    with_ml (fun ml -> M.length (M.push_front (-1) ml))
  | "push_back" ->
    with_ml (fun ml -> M.length (M.push_back (-1) ml))
  | "replace_at" ->
    if size = 0 then with_ml M.length
    else with_ml (fun ml -> M.fold_left ( + ) 0 (M.replace_at index 777 ml))
  | "update_nth" ->
    if size = 0 then with_ml M.length
    else with_ml (fun ml -> M.fold_left ( + ) 0 (M.update_nth index succ ml))
  | "insert_mark_at" ->
    with_marked_ml (fun ml -> M.length (M.insert_mark_at position "bench-mark" ml))
  | "insert_marks_at" ->
    with_marked_ml (fun ml -> M.length (M.insert_marks_at position ["bench-a"; "bench-b"] ml))
  | "remove_mark" ->
    with_marked_ml (fun ml -> M.length (M.remove_mark "boundary" ml))
  | "flatten_marks" ->
    with_marked_ml (fun ml ->
      consume_string_list (M.flatten_marks string_of_int string_of_marks ml))
  | "repeated_split_merge" ->
    with_marked_ml (fun ml ->
      let reps = if size < 1000 then 100 else 20 in
      let cur = ref ml in
      for i = 1 to reps do
        let pos = if M.length !cur = 0 then 0 else (i * 17) mod (M.length !cur + 1) in
        let left, right = M.split pos !cur in
        cur := M.merge left right
      done;
      M.length !cur)
  | "repeated_insert_remove" ->
    with_marked_ml (fun ml ->
      let reps = if size < 1000 then 100 else 20 in
      let cur = ref ml in
      for i = 1 to reps do
        let pos = if M.length !cur = 0 then 0 else (i * 19) mod (M.length !cur + 1) in
        cur := M.insert_at pos i !cur;
        let remove_pos = if M.length !cur = 0 then 0 else min (M.length !cur - 1) pos in
        cur := M.remove remove_pos 1 !cur
      done;
      M.length !cur)
  | "transform_like" ->
    with_marked_ml (fun ml ->
      let cur = ref ml in
      let reps = if size < 1000 then 80 else 16 in
      for i = 1 to reps do
        let len = M.length !cur in
        if len > 0 then begin
          let pos = (i * 23) mod len in
          let nb = min 3 (len - pos) in
          let rest, extracted = M.extract pos nb !cur in
          let extracted = M.map (fun x -> x + 1) extracted in
          cur := M.merge rest extracted
        end
      done;
      M.length !cur)
  | _ ->
    failwith ("Unknown benchmark operation: " ^ operation)

let operations =
  [ "of_list", [Deterministic];
    "to_list", [Deterministic];
    "length", [Deterministic];
    "nth", [Beginning; Middle; End; Deterministic];
    "nth_opt", [Beginning; Middle; End; Deterministic];
    "map", [Deterministic];
    "mapi", [Deterministic];
    "iter", [Deterministic];
    "fold_left", [Deterministic];
    "nth_sequential_scan", [Beginning];
    "nth_stride_scan", [Deterministic];
    "nth_random_scan", [Deterministic];
    "split", [Beginning; Middle; End; Mark_heavy];
    "merge", [Beginning; Middle; End];
    "extract", [Beginning; Middle; End; Mark_heavy];
    "remove", [Beginning; Middle; End; Mark_heavy];
    "insert_at", [Beginning; Middle; End; Mark_heavy];
    "insert_sublist_at", [Beginning; Middle; End; Mark_heavy];
    "push_front", [Beginning];
    "push_back", [End];
    "replace_at", [Beginning; Middle; End];
    "update_nth", [Beginning; Middle; End];
    "insert_mark_at", [Beginning; Middle; End; Mark_heavy];
    "insert_marks_at", [Beginning; Middle; End; Mark_heavy];
    "remove_mark", [Mark_heavy];
    "flatten_marks", [Mark_heavy];
    "repeated_split_merge", [Transform_like];
    "repeated_insert_remove", [Transform_like];
    "transform_like", [Transform_like] ]

let large_operations =
  [ "of_list", [Deterministic];
    "to_list", [Deterministic];
    "length", [Deterministic];
    "nth", [Beginning; Middle; End; Deterministic];
    "nth_opt", [Beginning; Middle; End; Deterministic];
    "map", [Deterministic];
    "iter", [Deterministic];
    "fold_left", [Deterministic];
    "split", [Beginning; Middle; End];
    "merge", [Middle];
    "push_front", [Beginning];
    "push_back", [End] ]

let make_bench ~repetitions ~tier operation size pattern : bench =
  { operation;
    size;
    pattern;
    repetitions;
    tier;
    old_setup = make_for_impl (module Old_adapter) operation size pattern;
    new_setup = make_for_impl (module New_adapter) operation size pattern }

let benches_for_sizes ~repetitions ~tier sizes operations : bench list =
  List.concat_map (fun size ->
    List.concat_map (fun (operation, patterns) ->
      List.map (fun pattern -> make_bench ~repetitions ~tier operation size pattern) patterns)
      operations)
    sizes

let all_benches () : bench list =
  benches_for_sizes ~repetitions:normal_repetitions ~tier:"normal" normal_sizes operations
  @ benches_for_sizes ~repetitions:large_repetitions ~tier:"large" large_sizes large_operations

let sink : int ref = ref 0

let progress fmt =
  Printf.ksprintf (fun msg ->
    Printf.printf "[mlist-bench] %s\n%!" msg)
    fmt

let measure (f : unit -> int) : int * float * Gc.stat * Gc.stat =
  Gc.compact ();
  let before = Gc.stat () in
  let t0 = Unix.gettimeofday () in
  let value = f () in
  let t1 = Unix.gettimeofday () in
  let after = Gc.stat () in
  sink := !sink + value;
  value, (1000. *. (t1 -. t0)), before, after

let run_measured ~implementation bench iteration setup : unit =
  progress "running operation=%s size=%d pattern=%s iteration=%d/%d implementation=%s"
    bench.operation
    bench.size
    (string_of_pattern bench.pattern)
    iteration
    bench.repetitions
    implementation;
  let f = setup () in
  let value, elapsed_ms, before, after = measure f in
  let minor_words, major_words, promoted_words, minor_collections, major_collections =
    Benchmark_logger.gc_stats_fields before after
  in
  let memory_notes = Benchmark_logger.gc_stats_notes before after in
  Benchmark_logger.set_implementation implementation;
  Benchmark_logger.write_csv_row
    ~subdir:"micro"
    ~filename:"mlist_microbench.csv"
    { Benchmark_logger.category = "micro";
      test_name = "mlist_bench";
      phase = "mlist-operation";
      operation = bench.operation;
      size = string_of_int bench.size;
      pattern = string_of_pattern bench.pattern;
      iteration = string_of_int iteration;
      elapsed_ms = string_of_float elapsed_ms;
      minor_words;
      major_words;
      promoted_words;
      minor_collections;
      major_collections;
      status = "pass";
      notes = "tier=" ^ bench.tier ^ ";result=" ^ string_of_int value ^ ";" ^ memory_notes };
  progress "done operation=%s size=%d pattern=%s iteration=%d/%d implementation=%s elapsed_ms=%.6f result=%d"
    bench.operation
    bench.size
    (string_of_pattern bench.pattern)
    iteration
    bench.repetitions
    implementation
    elapsed_ms
    value

let run_bench bench : unit =
  progress "case tier=%s operation=%s size=%d pattern=%s repetitions=%d warmup=old,new"
    bench.tier
    bench.operation
    bench.size
    (string_of_pattern bench.pattern)
    bench.repetitions;
  ignore ((bench.old_setup ()) ());
  ignore ((bench.new_setup ()) ());
  for iteration = 1 to bench.repetitions do
    run_measured ~implementation:"old-mlist" bench iteration bench.old_setup;
    run_measured ~implementation:"new-alist-mlist" bench iteration bench.new_setup
  done

let run () : unit =
  if not (Benchmark_logger.feature_is_enabled Benchmark_logger.mlist_bench) then begin
    print_endline "Mlist microbenchmarks are disabled. Pass -mlist-bench to run them."
  end else begin
    let benches = all_benches () in
    Benchmark_logger.log
      ~subdir:"micro"
      ~filename:"mlist_microbench.log"
      (Printf.sprintf "Starting %d Mlist microbenchmarks: normal_repetitions=%d large_repetitions=%d normal_sizes=%s large_sizes=%s"
        (List.length benches)
        normal_repetitions
        large_repetitions
        (String.concat ";" (List.map string_of_int normal_sizes))
        (String.concat ";" (List.map string_of_int large_sizes)));
    List.iter run_bench benches;
    Benchmark_logger.set_implementation "new-alist-mlist";
    let summary =
      Printf.sprintf "Mlist microbenchmarks complete: %d benchmark cases, normal_repetitions=%d, large_repetitions=%d, sink=%d"
        (List.length benches)
        normal_repetitions
        large_repetitions
        !sink
    in
    Benchmark_logger.log ~subdir:"micro" ~filename:"mlist_microbench.log" summary;
    print_endline summary
  end

let () =
  Flags.process_cmdline_args ();
  run ()
