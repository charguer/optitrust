open Optitrust

module Old = Mlist_old
module New = Mlist

type mark_spec = int * string list

type test_case = {
  pattern : string;
  items : int list;
  marks : mark_spec list;
}

type observation =
  | Value of string
  | Exn of string

let feature_enabled () : bool =
  Benchmark_logger.feature_is_enabled Benchmark_logger.mlist_correctness

let classify_exn (exn : exn) : string =
  match exn with
  | Invalid_argument _ -> "Invalid_argument"
  | Failure _ -> "Failure"
  | Assert_failure _ -> "Assert_failure"
  | Not_found -> "Not_found"
  | _ -> Printexc.exn_slot_name exn

let observe (f : unit -> string) : observation =
  match f () with
  | value -> Value value
  | exception exn -> Exn (classify_exn exn)

let observation_to_string = function
  | Value value -> "Value(" ^ value ^ ")"
  | Exn exn -> "Exn(" ^ exn ^ ")"

let same_observation old_obs new_obs : bool =
  match old_obs, new_obs with
  | Value old_value, Value new_value -> String.equal old_value new_value
  | Exn old_exn, Exn new_exn -> String.equal old_exn new_exn
  | _ -> false

let string_of_int_list xs : string =
  "[" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

let string_of_string_list xs : string =
  "[" ^ String.concat ";" xs ^ "]"

let string_of_marks marks : string =
  "[" ^ String.concat ";" (List.map string_of_string_list marks) ^ "]"

let string_of_string_output xs : string =
  "[" ^ String.concat ";" xs ^ "]"

let render_old (ml : int Old.t) : string =
  Printf.sprintf "{items=%s;marks=%s}"
    (string_of_int_list (Old.to_list ml))
    (string_of_marks (Old.get_marks ml))

let render_new (ml : int New.t) : string =
  Printf.sprintf "{items=%s;marks=%s}"
    (string_of_int_list (New.to_list ml))
    (string_of_marks (New.get_marks ml))

let render_old_pair (left, right : int Old.t * int Old.t) : string =
  Printf.sprintf "(%s,%s)" (render_old left) (render_old right)

let render_new_pair (left, right : int New.t * int New.t) : string =
  Printf.sprintf "(%s,%s)" (render_new left) (render_new right)

let render_old_split_on_marks (left, marks, right : int Old.t * string list * int Old.t) : string =
  Printf.sprintf "(%s,%s,%s)" (render_old left) (string_of_string_list marks) (render_old right)

let render_new_split_on_marks (left, marks, right : int New.t * string list * int New.t) : string =
  Printf.sprintf "(%s,%s,%s)" (render_new left) (string_of_string_list marks) (render_new right)

let render_int_option = function
  | None -> "None"
  | Some x -> "Some(" ^ string_of_int x ^ ")"

let render_bool b : string =
  string_of_bool b

let render_int x : string =
  string_of_int x

let truncate (s : string) : string =
  let max_len = 700 in
  if String.length s <= max_len then s else String.sub s 0 max_len ^ "..."

let old_from_case (case : test_case) : int Old.t =
  List.fold_left
    (fun ml (index, marks) -> Old.insert_marks_at index marks ml)
    (Old.of_list case.items)
    case.marks

let new_from_case (case : test_case) : int New.t =
  List.fold_left
    (fun ml (index, marks) -> New.insert_marks_at index marks ml)
    (New.of_list case.items)
    case.marks

let valid_positions len : int list =
  if len = 0 then [0] else [0; len / 2; len]

let valid_indices len : int list =
  if len = 0 then [] else [0; len / 2; len - 1]

let unique_ints xs : int list =
  List.sort_uniq compare xs

let cases : test_case list =
  let base_items =
    [ "empty", [];
      "one", [1];
      "two", [1; 2];
      "small", List.init 8 (fun i -> i + 1);
      "medium", List.init 40 (fun i -> i - 20) ]
  in
  List.concat_map (fun (name, items) ->
    let len = List.length items in
    let middle = len / 2 in
    let dense = List.init (len + 1) (fun i -> i, ["d" ^ string_of_int i]) in
    [ { pattern = name ^ ":no-marks"; items; marks = [] };
      { pattern = name ^ ":start-mark"; items; marks = [0, ["start"]] };
      { pattern = name ^ ":middle-mark"; items; marks = [middle, ["middle"]] };
      { pattern = name ^ ":end-mark"; items; marks = [len, ["end"]] };
      { pattern = name ^ ":stacked-marks"; items; marks = [middle, ["a"; "b"]; middle, ["c"]] };
      { pattern = name ^ ":dense-marks"; items; marks = dense } ])
    base_items

let empty_row ~(operation : string) ~(size : int) ~(pattern : string) ~(status : string) ~(notes : string) : Benchmark_logger.csv_row =
  { Benchmark_logger.category = "correctness";
    test_name = "mlist_correctness";
    phase = "mlist-vs-mlist-old";
    operation;
    size = string_of_int size;
    pattern;
    iteration = "0";
    elapsed_ms = "0";
    minor_words = "";
    major_words = "";
    promoted_words = "";
    minor_collections = "";
    major_collections = "";
    status;
    notes }

let total_cases = ref 0
let failed_cases = ref 0

let record_case ~(operation : string) ~(case : test_case) (old_obs : observation) (new_obs : observation) : unit =
  incr total_cases;
  let passed = same_observation old_obs new_obs in
  if not passed then incr failed_cases;
  let status = if passed then "pass" else "fail" in
  let notes =
    if passed then ""
    else truncate (Printf.sprintf "old=%s new=%s"
      (observation_to_string old_obs)
      (observation_to_string new_obs))
  in
  if feature_enabled () then begin
    Benchmark_logger.write_csv_row
      ~subdir:"correctness"
      ~filename:"mlist_correctness.csv"
      (empty_row
        ~operation
        ~size:(List.length case.items)
        ~pattern:case.pattern
        ~status
        ~notes);
    Benchmark_logger.log
      ~subdir:"correctness"
      ~filename:"mlist_correctness.log"
      (Printf.sprintf "%s size=%d pattern=%s status=%s %s"
        operation
        (List.length case.items)
        case.pattern
        status
        notes)
  end

let check ~(operation : string) ~(case : test_case) ~(old_f : unit -> string) ~(new_f : unit -> string) : unit =
  record_case ~operation ~case (observe old_f) (observe new_f)

let check_case (case : test_case) : unit =
  let len = List.length case.items in
  let old_ml () = old_from_case case in
  let new_ml () = new_from_case case in
  check ~operation:"of_list" ~case
    ~old_f:(fun () -> render_old (Old.of_list case.items))
    ~new_f:(fun () -> render_new (New.of_list case.items));
  check ~operation:"to_list" ~case
    ~old_f:(fun () -> string_of_int_list (Old.to_list (old_ml ())))
    ~new_f:(fun () -> string_of_int_list (New.to_list (new_ml ())));
  check ~operation:"get_marks" ~case
    ~old_f:(fun () -> string_of_marks (Old.get_marks (old_ml ())))
    ~new_f:(fun () -> string_of_marks (New.get_marks (new_ml ())));
  check ~operation:"length" ~case
    ~old_f:(fun () -> render_int (Old.length (old_ml ())))
    ~new_f:(fun () -> render_int (New.length (new_ml ())));
  check ~operation:"is_empty" ~case
    ~old_f:(fun () -> render_bool (Old.is_empty (old_ml ())))
    ~new_f:(fun () -> render_bool (New.is_empty (new_ml ())));
  List.iter (fun index ->
    check ~operation:("nth:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_int (Old.nth (old_ml ()) index))
      ~new_f:(fun () -> render_int (New.nth (new_ml ()) index));
    check ~operation:("update_nth:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.update_nth index succ (old_ml ())))
      ~new_f:(fun () -> render_new (New.update_nth index succ (new_ml ())));
    check ~operation:("replace_at:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.replace_at index 777 (old_ml ())))
      ~new_f:(fun () -> render_new (New.replace_at index 777 (new_ml ()))))
    (valid_indices len);
  List.iter (fun index ->
    check ~operation:("nth_opt:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_int_option (Old.nth_opt (old_ml ()) index))
      ~new_f:(fun () -> render_int_option (New.nth_opt (new_ml ()) index)))
    (unique_ints [-1; 0; len / 2; len; len + 1]);
  check ~operation:"map" ~case
    ~old_f:(fun () -> render_old (Old.map succ (old_ml ())))
    ~new_f:(fun () -> render_new (New.map succ (new_ml ())));
  check ~operation:"mapi" ~case
    ~old_f:(fun () -> render_old (Old.mapi (fun i x -> i + x) (old_ml ())))
    ~new_f:(fun () -> render_new (New.mapi (fun i x -> i + x) (new_ml ())));
  check ~operation:"fold_left" ~case
    ~old_f:(fun () -> render_int (Old.fold_left ( + ) 0 (old_ml ())))
    ~new_f:(fun () -> render_int (New.fold_left ( + ) 0 (new_ml ())));
  check ~operation:"fold_lefti" ~case
    ~old_f:(fun () -> render_int (Old.fold_lefti (fun i acc x -> acc + i + x) 0 (old_ml ())))
    ~new_f:(fun () -> render_int (New.fold_lefti (fun i acc x -> acc + i + x) 0 (new_ml ())));
  check ~operation:"rev" ~case
    ~old_f:(fun () -> render_old (Old.rev (old_ml ())))
    ~new_f:(fun () -> render_new (New.rev (new_ml ())));
  check ~operation:"find_map" ~case
    ~old_f:(fun () -> render_int_option (Old.find_map (fun x -> if x mod 2 = 0 then Some x else None) (old_ml ())))
    ~new_f:(fun () -> render_int_option (New.find_map (fun x -> if x mod 2 = 0 then Some x else None) (new_ml ())));
  check ~operation:"for_all" ~case
    ~old_f:(fun () -> render_bool (Old.for_all (fun x -> x >= -20) (old_ml ())))
    ~new_f:(fun () -> render_bool (New.for_all (fun x -> x >= -20) (new_ml ())));
  List.iter (fun index ->
    check ~operation:("split-left:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old_pair (Old.split ~left_bias:true index (old_ml ())))
      ~new_f:(fun () -> render_new_pair (New.split ~left_bias:true index (new_ml ())));
    check ~operation:("split-right:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old_pair (Old.split ~left_bias:false index (old_ml ())))
      ~new_f:(fun () -> render_new_pair (New.split ~left_bias:false index (new_ml ())));
    check ~operation:("split_on_marks:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old_split_on_marks (Old.split_on_marks index (old_ml ())))
      ~new_f:(fun () -> render_new_split_on_marks (New.split_on_marks index (new_ml ()))))
    (valid_positions len);
  List.iter (fun index ->
    check ~operation:("insert_at:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.insert_at index 99 (old_ml ())))
      ~new_f:(fun () -> render_new (New.insert_at index 99 (new_ml ())));
    check ~operation:("insert_sublist_at:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.insert_sublist_at index [99; 100] (old_ml ())))
      ~new_f:(fun () -> render_new (New.insert_sublist_at index [99; 100] (new_ml ())));
    check ~operation:("insert_mark_at:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.insert_mark_at index "new-mark" (old_ml ())))
      ~new_f:(fun () -> render_new (New.insert_mark_at index "new-mark" (new_ml ())));
    check ~operation:("insert_marks_at:" ^ string_of_int index) ~case
      ~old_f:(fun () -> render_old (Old.insert_marks_at index ["new-a"; "new-b"] (old_ml ())))
      ~new_f:(fun () -> render_new (New.insert_marks_at index ["new-a"; "new-b"] (new_ml ()))))
    (valid_positions len);
  check ~operation:"merge" ~case
    ~old_f:(fun () -> render_old (Old.merge (old_ml ()) (Old.of_list [42; 43])))
    ~new_f:(fun () -> render_new (New.merge (new_ml ()) (New.of_list [42; 43])));
  List.iter (fun start ->
    let max_nb = len - start in
    List.iter (fun nb ->
      check ~operation:(Printf.sprintf "extract:%d:%d" start nb) ~case
        ~old_f:(fun () -> render_old_pair (Old.extract start nb (old_ml ())))
        ~new_f:(fun () -> render_new_pair (New.extract start nb (new_ml ())));
      check ~operation:(Printf.sprintf "remove:%d:%d" start nb) ~case
        ~old_f:(fun () -> render_old (Old.remove start nb (old_ml ())))
        ~new_f:(fun () -> render_new (New.remove start nb (new_ml ()))))
      (unique_ints [0; max_nb / 2; max_nb])
    )
    (valid_positions len);
  check ~operation:"push_front" ~case
    ~old_f:(fun () -> render_old (Old.push_front 11 (old_ml ())))
    ~new_f:(fun () -> render_new (New.push_front 11 (new_ml ())));
  check ~operation:"push_back" ~case
    ~old_f:(fun () -> render_old (Old.push_back 11 (old_ml ())))
    ~new_f:(fun () -> render_new (New.push_back 11 (new_ml ())));
  if len > 0 then begin
    check ~operation:"pop_front" ~case
      ~old_f:(fun () -> render_old (Old.pop_front (old_ml ())))
      ~new_f:(fun () -> render_new (New.pop_front (new_ml ())));
    check ~operation:"pop_back" ~case
      ~old_f:(fun () -> render_old (Old.pop_back (old_ml ())))
      ~new_f:(fun () -> render_new (New.pop_back (new_ml ())));
    check ~operation:"last" ~case
      ~old_f:(fun () -> render_int_option (Old.last (old_ml ())))
      ~new_f:(fun () -> render_int_option (New.last (new_ml ())))
  end;
  check ~operation:"filter_marks" ~case
    ~old_f:(fun () -> render_old (Old.filter_marks (fun mark -> mark <> "middle") (old_ml ())))
    ~new_f:(fun () -> render_new (New.filter_marks (fun mark -> mark <> "middle") (new_ml ())));
  check ~operation:"remove_mark" ~case
    ~old_f:(fun () -> render_old (Old.remove_mark "start" (old_ml ())))
    ~new_f:(fun () -> render_new (New.remove_mark "start" (new_ml ())));
  check ~operation:"remove_all_marks" ~case
    ~old_f:(fun () -> render_old (Old.remove_all_marks (old_ml ())))
    ~new_f:(fun () -> render_new (New.remove_all_marks (new_ml ())));
  check ~operation:"flatten_marks" ~case
    ~old_f:(fun () -> string_of_string_output (Old.flatten_marks string_of_int string_of_string_list (old_ml ())))
    ~new_f:(fun () -> string_of_string_output (New.flatten_marks string_of_int string_of_string_list (new_ml ())));
  check ~operation:"filter" ~case
    ~old_f:(fun () -> render_old (Old.filter (fun x -> x mod 2 = 0) (old_ml ())))
    ~new_f:(fun () -> render_new (New.filter (fun x -> x mod 2 = 0) (new_ml ())));
  check ~operation:"filteri" ~case
    ~old_f:(fun () -> render_old (Old.filteri (fun i _x -> i mod 2 = 0) (old_ml ())))
    ~new_f:(fun () -> render_new (New.filteri (fun i _x -> i mod 2 = 0) (new_ml ())));
  check ~operation:"concat_mapi" ~case
    ~old_f:(fun () -> render_old (Old.concat_mapi (fun i x -> Old.of_list [x; i]) (old_ml ())))
    ~new_f:(fun () -> render_new (New.concat_mapi (fun i x -> New.of_list [x; i]) (new_ml ())))

let run () : unit =
  Benchmark_logger.set_implementation "new-alist-mlist";
  if feature_enabled () then
    Benchmark_logger.log
      ~subdir:"correctness"
      ~filename:"mlist_correctness.log"
      "Starting Mlist versus Mlist_old correctness comparison";
  List.iter check_case cases;
  let passed = !total_cases - !failed_cases in
  let summary =
    Printf.sprintf "Mlist correctness: %d passed, %d failed, %d total"
      passed !failed_cases !total_cases
  in
  print_endline summary;
  if feature_enabled () then begin
    Benchmark_logger.log ~subdir:"correctness" ~filename:"mlist_correctness.log" summary;
    Benchmark_logger.write_json
      ~subdir:"correctness"
      ~filename:"mlist_correctness_summary.json"
      (Json.obj_quoted_keys [
        "passed", Json.int passed;
        "failed", Json.int !failed_cases;
        "total", Json.int !total_cases;
      ])
  end;
  if !failed_cases > 0 then exit 1

let () =
  Flags.process_cmdline_args ();
  run ()
