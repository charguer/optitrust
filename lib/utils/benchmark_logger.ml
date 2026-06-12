(** Permanent opt-in logging support for benchmarks.

    This module is intentionally inert by default. Callers must enable it before
    any directory is created or any file is written. *)

type csv_row = {
  category : string;
  test_name : string;
  phase : string;
  operation : string;
  size : string;
  pattern : string;
  iteration : string;
  elapsed_ms : string;
  minor_words : string;
  major_words : string;
  promoted_words : string;
  minor_collections : string;
  major_collections : string;
  status : string;
  notes : string;
}

let enabled : bool ref = ref false
let benchmark_dir : string ref = ref "benchmark"
let run_id : string option ref = ref None
let implementation : string ref = ref "new-alist-mlist"

let metadata_written : bool ref = ref false

(** Registered opt-in benchmark features.

    Adding a new benchmark should usually require only:
    - registering a feature here, e.g. [let my_bench = register_feature "-my-bench"],
    - exposing a CLI flag that calls [set_feature_ref my_bench true],
    - guarding the benchmark code with [feature_is_enabled my_bench]. *)
let features : (string * bool ref) list ref = ref []

let register_feature (name : string) : bool ref =
  match List.assoc_opt name !features with
  | Some feature -> feature
  | None ->
    let feature = ref false in
    features := !features @ [name, feature];
    feature

let report_exectime : bool ref = register_feature "-report-exectime"
let report_test_time : bool ref = register_feature "-report-test-time"
let mlist_correctness : bool ref = register_feature "-mlist-correctness"
let mlist_bench : bool ref = register_feature "-mlist-bench"
let mlist_profile : bool ref = register_feature "-mlist-profile"

let is_enabled () : bool =
  !enabled || List.exists (fun (_name, feature) -> !feature) !features

let feature_is_enabled (feature : bool ref) : bool =
  !feature

let set_feature_ref (feature : bool ref) (b : bool) : unit =
  feature := b

let set_feature (name : string) (b : bool) : unit =
  set_feature_ref (register_feature name) b

let set_enabled (b : bool) : unit =
  enabled := b

let set_report_exectime (b : bool) : unit =
  set_feature_ref report_exectime b

let set_report_test_time (b : bool) : unit =
  set_feature_ref report_test_time b

let set_mlist_correctness (b : bool) : unit =
  set_feature_ref mlist_correctness b

let set_mlist_bench (b : bool) : unit =
  set_feature_ref mlist_bench b

let set_mlist_profile (b : bool) : unit =
  set_feature_ref mlist_profile b

let set_benchmark_dir (dir : string) : unit =
  benchmark_dir := dir

let set_run_id (id : string) : unit =
  run_id := Some id

let set_implementation (name : string) : unit =
  implementation := name

let timestamp () : string =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let timestamp_for_filename () : string =
  let s = timestamp () in
  String.map (function ':' -> '-' | c -> c) s

let get_run_id () : string =
  match !run_id with
  | Some id -> id
  | None ->
    let id = timestamp_for_filename () in
    run_id := Some id;
    id

let rec ensure_dir (dir : string) : unit =
  if dir = "" || dir = "." then ()
  else if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      invalid_arg (Printf.sprintf "Benchmark_logger.ensure_dir: %s exists and is not a directory" dir)
  end else begin
    ensure_dir (Filename.dirname dir);
    Unix.mkdir dir 0o755
  end

let run_dir () : string =
  Filename.concat (Filename.concat !benchmark_dir "runs") (get_run_id ())

let path_in_run (parts : string list) : string =
  List.fold_left Filename.concat (run_dir ()) parts

let ensure_run_dir () : unit =
  if is_enabled () then ensure_dir (run_dir ())

let ensure_subdir (subdir : string) : string =
  let dir = path_in_run [subdir] in
  if is_enabled () then ensure_dir dir;
  dir

let command_output_line (cmd : string) : string =
  try
    let ic = Unix.open_process_in cmd in
    let line =
      try input_line ic
      with End_of_file -> ""
    in
    ignore (Unix.close_process_in ic);
    line
  with _ -> ""

let read_first_existing_line (files : string list) : string =
  let rec aux = function
    | [] -> ""
    | file :: files ->
      if Sys.file_exists file then begin
        match File.get_lines_or_empty file with
        | line :: _ -> line
        | [] -> aux files
      end else aux files
  in
  aux files

let json_string (s : string) : string =
  Json.to_string (Json.str s)

let default_flags () : (string * bool) list =
  List.map (fun (name, feature) -> name, !feature) !features
  @ ["benchmark-logger-enabled", !enabled]

let write_metadata ?(flags : (string * bool) list option) () : unit =
  if is_enabled () && not !metadata_written then begin
    ensure_run_dir ();
    let flags = Option.value ~default:(default_flags ()) flags in
    let flags_json =
      flags
      |> List.map (fun (name, value) -> Printf.sprintf "%s: %s" (json_string name) (string_of_bool value))
      |> String.concat ", "
    in
    let command = String.concat " " (Array.to_list Sys.argv) in
    let metadata =
      Printf.sprintf
        "{\n\
         \  \"timestamp\": %s,\n\
         \  \"run_id\": %s,\n\
         \  \"implementation\": %s,\n\
         \  \"git_commit\": %s,\n\
         \  \"git_branch\": %s,\n\
         \  \"ocaml_version\": %s,\n\
         \  \"dune_version\": %s,\n\
         \  \"machine\": %s,\n\
         \  \"cpu_model\": %s,\n\
         \  \"command\": %s,\n\
         \  \"benchmark_dir\": %s,\n\
         \  \"flags\": { %s }\n\
         }\n"
        (json_string (timestamp ()))
        (json_string (get_run_id ()))
        (json_string !implementation)
        (json_string (command_output_line "git rev-parse HEAD 2>/dev/null"))
        (json_string (command_output_line "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
        (json_string Sys.ocaml_version)
        (json_string (command_output_line "dune --version 2>/dev/null"))
        (json_string (Unix.gethostname ()))
        (json_string (read_first_existing_line ["/proc/cpuinfo"]))
        (json_string command)
        (json_string !benchmark_dir)
        flags_json
    in
    File.put_contents (path_in_run ["metadata.json"]) metadata;
    metadata_written := true
  end

let csv_escape (s : string) : string =
  if String.exists (fun c -> c = ',' || c = '"' || c = '\n' || c = '\r') s then
    "\"" ^ Str.global_replace (Str.regexp_string "\"") "\"\"" s ^ "\""
  else
    s

let csv_line (fields : string list) : string =
  String.concat "," (List.map csv_escape fields)

let benchmark_csv_header : string =
  csv_line [
    "timestamp";
    "run_id";
    "implementation";
    "category";
    "test_name";
    "phase";
    "operation";
    "size";
    "pattern";
    "iteration";
    "elapsed_ms";
    "minor_words";
    "major_words";
    "promoted_words";
    "minor_collections";
    "major_collections";
    "status";
    "notes";
  ]

let csv_row_to_line (row : csv_row) : string =
  csv_line [
    timestamp ();
    get_run_id ();
    !implementation;
    row.category;
    row.test_name;
    row.phase;
    row.operation;
    row.size;
    row.pattern;
    row.iteration;
    row.elapsed_ms;
    row.minor_words;
    row.major_words;
    row.promoted_words;
    row.minor_collections;
    row.major_collections;
    row.status;
    row.notes;
  ]

let append_line (filename : string) (line : string) : unit =
  File.append_contents filename (line ^ "\n")

let write_csv_row ~(subdir : string) ~(filename : string) (row : csv_row) : unit =
  if is_enabled () then begin
    write_metadata ();
    let dir = ensure_subdir subdir in
    let path = Filename.concat dir filename in
    if not (Sys.file_exists path) then append_line path benchmark_csv_header;
    append_line path (csv_row_to_line row)
  end

let log ~(subdir : string) ~(filename : string) (message : string) : unit =
  if is_enabled () then begin
    write_metadata ();
    let dir = ensure_subdir subdir in
    let path = Filename.concat dir filename in
    append_line path (Printf.sprintf "[%s] %s" (timestamp ()) message)
  end

let write_text ~(subdir : string) ~(filename : string) (contents : string) : unit =
  if is_enabled () then begin
    write_metadata ();
    let dir = ensure_subdir subdir in
    File.put_contents (Filename.concat dir filename) contents
  end

let write_json ~(subdir : string) ~(filename : string) (json : Json.t) : unit =
  write_text ~subdir ~filename (Json.to_string json ^ "\n")

let gc_stats_fields (before : Gc.stat) (after : Gc.stat) : string * string * string * string * string =
  let float_diff f = string_of_float (f after -. f before) in
  let int_diff f = string_of_int (f after - f before) in
  ( float_diff (fun s -> s.Gc.minor_words),
    float_diff (fun s -> s.Gc.major_words),
    float_diff (fun s -> s.Gc.promoted_words),
    int_diff (fun s -> s.Gc.minor_collections),
    int_diff (fun s -> s.Gc.major_collections) )

let gc_stats_notes (before : Gc.stat) (after : Gc.stat) : string =
  let int_field name value =
    name ^ "=" ^ string_of_int value
  in
  let float_field name value =
    name ^ "=" ^ string_of_float value
  in
  String.concat ";"
    [ int_field "heap_words_before" before.Gc.heap_words;
      int_field "heap_words_after" after.Gc.heap_words;
      int_field "top_heap_words_before" before.Gc.top_heap_words;
      int_field "top_heap_words_after" after.Gc.top_heap_words;
      int_field "live_words_before" before.Gc.live_words;
      int_field "live_words_after" after.Gc.live_words;
      int_field "free_words_before" before.Gc.free_words;
      int_field "free_words_after" after.Gc.free_words;
      int_field "fragments_before" before.Gc.fragments;
      int_field "fragments_after" after.Gc.fragments;
      int_field "compactions_before" before.Gc.compactions;
      int_field "compactions_after" after.Gc.compactions;
      float_field "allocated_words_delta"
        ((after.Gc.minor_words -. before.Gc.minor_words)
         +. (after.Gc.major_words -. before.Gc.major_words)) ]
