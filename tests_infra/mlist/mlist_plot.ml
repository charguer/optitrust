open Optitrust

type row = {
  implementation : string;
  operation : string;
  size : int;
  pattern : string;
  elapsed_ms : float;
}

type stats = {
  count : int;
  mean : float;
  median : float;
  min_v : float;
  max_v : float;
  stddev : float;
  q1 : float;
  q3 : float;
}

let default_operations =
  [ "split";
    "merge";
    "extract";
    "remove";
    "insert_at";
    "insert_sublist_at";
    "nth";
    "nth_random_scan";
    "nth_stride_scan";
    "transform_like" ]

let split_on_char (sep : char) (s : string) : string list =
  let rec aux start i acc =
    if i = String.length s then
      List.rev (String.sub s start (i - start) :: acc)
    else if s.[i] = sep then
      aux (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
    else
      aux start (i + 1) acc
  in
  aux 0 0 []

let ensure_dir (dir : string) : unit =
  let rec aux dir =
    if dir = "" || dir = "." then ()
    else if Sys.file_exists dir then begin
      if not (Sys.is_directory dir) then
        invalid_arg (Printf.sprintf "%s exists and is not a directory" dir)
    end else begin
      aux (Filename.dirname dir);
      Unix.mkdir dir 0o755
    end
  in
  aux dir

let absolute_path (path : string) : string =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let run_dir_from_csv (csv_path : string) : string =
  csv_path |> absolute_path |> Filename.dirname |> Filename.dirname

let index_of (name : string) (headers : string array) : int =
  let rec aux i =
    if i >= Array.length headers then
      failwith ("Mlist_plot: missing CSV column " ^ name)
    else if headers.(i) = name then i
    else aux (i + 1)
  in
  aux 0

let parse_rows (csv_path : string) : row list =
  match File.get_lines csv_path with
  | [] -> []
  | header :: lines ->
    let headers = Array.of_list (split_on_char ',' header) in
    let implementation_i = index_of "implementation" headers in
    let operation_i = index_of "operation" headers in
    let size_i = index_of "size" headers in
    let pattern_i = index_of "pattern" headers in
    let elapsed_i = index_of "elapsed_ms" headers in
    List.filter_map (fun line ->
      match split_on_char ',' line with
      | fields when List.length fields > elapsed_i ->
        let field i = List.nth fields i in
        Some {
          implementation = field implementation_i;
          operation = field operation_i;
          size = int_of_string (field size_i);
          pattern = field pattern_i;
          elapsed_ms = float_of_string (field elapsed_i);
        }
      | _ -> None)
      lines

let percentile (values : float array) (p : float) : float =
  let n = Array.length values in
  if n = 0 then 0. else
  let pos = (float_of_int (n - 1)) *. p in
  let lo = int_of_float (floor pos) in
  let hi = int_of_float (ceil pos) in
  if lo = hi then values.(lo)
  else
    let w = pos -. float_of_int lo in
    values.(lo) +. ((values.(hi) -. values.(lo)) *. w)

let summarize (values : float list) : stats =
  let arr = Array.of_list (List.sort compare values) in
  let count = Array.length arr in
  if count = 0 then
    { count = 0; mean = 0.; median = 0.; min_v = 0.; max_v = 0.; stddev = 0.; q1 = 0.; q3 = 0. }
  else
    let sum = Array.fold_left ( +. ) 0. arr in
    let mean = sum /. float_of_int count in
    let variance =
      if count <= 1 then 0.
      else
        Array.fold_left (fun acc x -> acc +. ((x -. mean) *. (x -. mean))) 0. arr
        /. float_of_int (count - 1)
    in
    { count;
      mean;
      median = percentile arr 0.5;
      min_v = arr.(0);
      max_v = arr.(count - 1);
      stddev = sqrt variance;
      q1 = percentile arr 0.25;
      q3 = percentile arr 0.75 }

let group_key operation size pattern implementation =
  Printf.sprintf "%s|%d|%s|%s" operation size pattern implementation

let case_key operation size pattern =
  Printf.sprintf "%s|%d|%s" operation size pattern

let safe_filename (s : string) : string =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' -> ()
    | _ -> Bytes.set b i '_'
  done;
  Bytes.to_string b

let split_key key =
  match split_on_char '|' key with
  | [operation; size; pattern] -> operation, int_of_string size, pattern
  | _ -> failwith ("Mlist_plot: bad key " ^ key)

let grouped_values (rows : row list) : (string, float list) Hashtbl.t =
  let h = Hashtbl.create 257 in
  List.iter (fun row ->
    let key = group_key row.operation row.size row.pattern row.implementation in
    let prev = Option.value ~default:[] (Hashtbl.find_opt h key) in
    Hashtbl.replace h key (row.elapsed_ms :: prev))
    rows;
  h

let write_summary (rows : row list) (summary_path : string) : unit =
  let grouped = grouped_values rows in
  let cases = Hashtbl.create 257 in
  Hashtbl.iter (fun key _values ->
    match split_on_char '|' key with
    | [operation; size; pattern; _implementation] ->
      Hashtbl.replace cases (case_key operation (int_of_string size) pattern) ()
    | _ -> ())
    grouped;
  let out = open_out summary_path in
  output_string out "operation,size,pattern,old_count,new_count,old_mean_ms,new_mean_ms,speedup_old_over_new,new_mean_over_old_mean,speedup_percent_new_vs_old,old_median_ms,new_median_ms,old_min_ms,new_min_ms,old_max_ms,new_max_ms,old_stddev_ms,new_stddev_ms\n";
  let case_keys = Hashtbl.fold (fun key () acc -> key :: acc) cases [] in
  let case_keys =
    List.sort (fun a b ->
      let oa, sa, pa = split_key a in
      let ob, sb, pb = split_key b in
      compare (oa, sa, pa) (ob, sb, pb))
      case_keys
  in
  List.iter (fun key ->
    let operation, size, pattern = split_key key in
    let old_values = Hashtbl.find_opt grouped (group_key operation size pattern "old-mlist") in
    let new_values = Hashtbl.find_opt grouped (group_key operation size pattern "new-alist-mlist") in
    match old_values, new_values with
    | Some old_values, Some new_values ->
      let old_s = summarize old_values in
      let new_s = summarize new_values in
      let speedup = if new_s.mean = 0. then 0. else old_s.mean /. new_s.mean in
      let new_over_old = if old_s.mean = 0. then 0. else new_s.mean /. old_s.mean in
      let speedup_percent = if old_s.mean = 0. then 0. else ((old_s.mean -. new_s.mean) /. old_s.mean) *. 100. in
      Printf.fprintf out "%s,%d,%s,%d,%d,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g,%.9g\n"
        operation size pattern
        old_s.count new_s.count
        old_s.mean new_s.mean speedup new_over_old speedup_percent
        old_s.median new_s.median
        old_s.min_v new_s.min_v
        old_s.max_v new_s.max_v
        old_s.stddev new_s.stddev
    | _ -> ())
    case_keys;
  close_out out

let svg_escape (s : string) : string =
  s
  |> Str.global_replace (Str.regexp_string "&") "&amp;"
  |> Str.global_replace (Str.regexp_string "<") "&lt;"
  |> Str.global_replace (Str.regexp_string ">") "&gt;"
  |> Str.global_replace (Str.regexp_string "\"") "&quot;"

let log10_safe (x : float) : float =
  log (max x 0.000001) /. log 10.

let case_keys_from_rows (rows : row list) : string list =
  rows
  |> List.map (fun row -> case_key row.operation row.size row.pattern)
  |> List.sort_uniq compare

let plot_operation (rows : row list) (operation : string) (output_path : string) : bool =
  let rows = List.filter (fun row -> row.operation = operation) rows in
  if rows = [] then false else
  let grouped = grouped_values rows in
  let groups =
    rows
    |> List.map (fun row -> row.size, row.pattern)
    |> List.sort_uniq compare
  in
  let all_values = List.map (fun row -> row.elapsed_ms) rows in
  let y_min = log10_safe (List.fold_left min max_float all_values) in
  let y_max = log10_safe (List.fold_left max min_float all_values) in
  let y_min, y_max =
    if y_min = y_max then y_min -. 1., y_max +. 1.
    else
      let pad = (y_max -. y_min) *. 0.08 in
      y_min -. pad, y_max +. pad
  in
  let group_count = List.length groups in
  let width = max 1000 (150 + (group_count * 96)) in
  let height = 620 in
  let left = 82. in
  let right = 28. in
  let top = 54. in
  let bottom = 150. in
  let plot_w = float_of_int width -. left -. right in
  let plot_h = float_of_int height -. top -. bottom in
  let x_group i =
    if group_count = 1 then left +. (plot_w /. 2.)
    else left +. ((float_of_int i) *. (plot_w /. float_of_int (group_count - 1)))
  in
  let y_of_log log_ms =
    top +. ((y_max -. log_ms) /. (y_max -. y_min) *. plot_h)
  in
  let y ms = y_of_log (log10_safe ms) in
  let b = Buffer.create 65536 in
  let add fmt = Printf.bprintf b fmt in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n" width height width height;
  add "<rect width=\"100%%\" height=\"100%%\" fill=\"white\"/>\n";
  add "<text x=\"%.0f\" y=\"28\" font-family=\"sans-serif\" font-size=\"20\" font-weight=\"700\">%s elapsed time by size/pattern</text>\n" left (svg_escape operation);
  add "<text x=\"%.0f\" y=\"48\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#555\">Box = IQR, line = median, dot = iteration, diamond = mean. Y axis is log10(ms).</text>\n" left;
  for tick = int_of_float (floor y_min) to int_of_float (ceil y_max) do
    let yy = y_of_log (float_of_int tick) in
    if yy >= top && yy <= top +. plot_h then begin
      add "<line x1=\"%.0f\" y1=\"%.2f\" x2=\"%.0f\" y2=\"%.2f\" stroke=\"#e5e7eb\" stroke-width=\"1\"/>\n" left yy (left +. plot_w) yy;
      add "<text x=\"%.0f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"11\" text-anchor=\"end\" fill=\"#555\">%.6g ms</text>\n" (left -. 8.) (yy +. 4.) (10. ** float_of_int tick)
    end
  done;
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left top left (top +. plot_h);
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left (top +. plot_h) (left +. plot_w) (top +. plot_h);
  List.iteri (fun i (size, pattern) ->
    let gx = x_group i in
    add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"10\" text-anchor=\"middle\" fill=\"#333\">%d</text>\n" gx (height - 92) size;
    add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\" fill=\"#555\">%s</text>\n" gx (height - 78) (svg_escape pattern);
    begin
      let old_values = Hashtbl.find_opt grouped (group_key operation size pattern "old-mlist") in
      let new_values = Hashtbl.find_opt grouped (group_key operation size pattern "new-alist-mlist") in
      match old_values, new_values with
      | Some old_values, Some new_values when old_values <> [] && new_values <> [] ->
        let old_s = summarize old_values in
        let new_s = summarize new_values in
        let speedup_percent = if old_s.mean = 0. then 0. else ((old_s.mean -. new_s.mean) /. old_s.mean) *. 100. in
        add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\" fill=\"#111\">speedup=%.1f%%</text>\n" gx (height - 64) speedup_percent
      | _ -> ()
    end;
    List.iter (fun (implementation, color, offset) ->
      let values = Option.value ~default:[] (Hashtbl.find_opt grouped (group_key operation size pattern implementation)) in
      if values <> [] then begin
        let stats = summarize values in
        let x = gx +. offset in
        let q1_y = y stats.q1 in
        let q3_y = y stats.q3 in
        let med_y = y stats.median in
        let min_y = y stats.min_v in
        let max_y = y stats.max_v in
        let mean_y = y stats.mean in
        let box_top = min q1_y q3_y in
        let box_h = max 1. (abs_float (q3_y -. q1_y)) in
        add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.2\"/>\n" x min_y x max_y color;
        add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.2\"/>\n" (x -. 7.) min_y (x +. 7.) min_y color;
        add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.2\"/>\n" (x -. 7.) max_y (x +. 7.) max_y color;
        add "<rect x=\"%.2f\" y=\"%.2f\" width=\"18\" height=\"%.2f\" fill=\"%s\" fill-opacity=\"0.22\" stroke=\"%s\" stroke-width=\"1.2\"/>\n" (x -. 9.) box_top box_h color color;
        add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"2\"/>\n" (x -. 10.) med_y (x +. 10.) med_y color;
        add "<path d=\"M %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f Z\" fill=\"%s\"/>\n" x (mean_y -. 5.) (x +. 5.) mean_y x (mean_y +. 5.) (x -. 5.) mean_y color;
        values
        |> List.sort compare
        |> List.iteri (fun j value ->
          let jitter = float_of_int ((j mod 5) - 2) *. 1.8 in
          add "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"2.2\" fill=\"%s\" fill-opacity=\"0.55\"/>\n" (x +. jitter) (y value) color)
      end)
      [ "old-mlist", "#d95f02", -15.;
        "new-alist-mlist", "#1b9e77", 15. ])
    groups;
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#d95f02\" fill-opacity=\"0.45\" stroke=\"#d95f02\"/>\n" left (height - 44);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">old-mlist</text>\n" (left +. 18.) (height - 34);
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#1b9e77\" fill-opacity=\"0.45\" stroke=\"#1b9e77\"/>\n" (left +. 120.) (height - 44);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">new-alist-mlist</text>\n" (left +. 138.) (height - 34);
  add "</svg>\n";
  File.put_contents output_path (Buffer.contents b);
  true

let plot_case (rows : row list) operation size pattern output_path : bool =
  let rows =
    List.filter (fun row ->
      row.operation = operation && row.size = size && row.pattern = pattern)
      rows
  in
  if rows = [] then false else
  let values implementation =
    rows
    |> List.filter (fun row -> row.implementation = implementation)
    |> List.map (fun row -> row.elapsed_ms)
  in
  let old_values = values "old-mlist" in
  let new_values = values "new-alist-mlist" in
  let all_values = old_values @ new_values in
  if all_values = [] then false else
  let y_min = log10_safe (List.fold_left min max_float all_values) in
  let y_max = log10_safe (List.fold_left max min_float all_values) in
  let y_min, y_max =
    if y_min = y_max then y_min -. 1., y_max +. 1.
    else
      let pad = (y_max -. y_min) *. 0.12 in
      y_min -. pad, y_max +. pad
  in
  let width = 720 in
  let height = 500 in
  let left = 92. in
  let right = 42. in
  let top = 66. in
  let bottom = 86. in
  let plot_w = float_of_int width -. left -. right in
  let plot_h = float_of_int height -. top -. bottom in
  let y_of_log log_ms =
    top +. ((y_max -. log_ms) /. (y_max -. y_min) *. plot_h)
  in
  let y ms = y_of_log (log10_safe ms) in
  let b = Buffer.create 32768 in
  let add fmt = Printf.bprintf b fmt in
  let draw_box label color x values =
    if values <> [] then begin
      let stats = summarize values in
      let q1_y = y stats.q1 in
      let q3_y = y stats.q3 in
      let med_y = y stats.median in
      let min_y = y stats.min_v in
      let max_y = y stats.max_v in
      let mean_y = y stats.mean in
      let box_top = min q1_y q3_y in
      let box_h = max 1. (abs_float (q3_y -. q1_y)) in
      add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.5\"/>\n" x min_y x max_y color;
      add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.5\"/>\n" (x -. 18.) min_y (x +. 18.) min_y color;
      add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"1.5\"/>\n" (x -. 18.) max_y (x +. 18.) max_y color;
      add "<rect x=\"%.2f\" y=\"%.2f\" width=\"56\" height=\"%.2f\" fill=\"%s\" fill-opacity=\"0.22\" stroke=\"%s\" stroke-width=\"1.5\"/>\n" (x -. 28.) box_top box_h color color;
      add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" stroke-width=\"2.2\"/>\n" (x -. 34.) med_y (x +. 34.) med_y color;
      add "<path d=\"M %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f Z\" fill=\"%s\"/>\n" x (mean_y -. 6.) (x +. 6.) mean_y x (mean_y +. 6.) (x -. 6.) mean_y color;
      values
      |> List.sort compare
      |> List.iteri (fun j value ->
        let jitter = float_of_int ((j mod 7) - 3) *. 3. in
        add "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"3\" fill=\"%s\" fill-opacity=\"0.58\"/>\n" (x +. jitter) (y value) color);
      add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"14\" text-anchor=\"middle\">%s</text>\n" x (height - 50) (svg_escape label);
      add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"11\" text-anchor=\"middle\" fill=\"#555\">n=%d mean=%.6g ms</text>\n" x (height - 32) stats.count stats.mean
    end
  in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n" width height width height;
  add "<rect width=\"100%%\" height=\"100%%\" fill=\"white\"/>\n";
  add "<text x=\"%.0f\" y=\"28\" font-family=\"sans-serif\" font-size=\"20\" font-weight=\"700\">%s size=%d pattern=%s</text>\n" left (svg_escape operation) size (svg_escape pattern);
  add "<text x=\"%.0f\" y=\"50\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#555\">Old vs new for one benchmark case. Dots are measured iterations; diamond is mean; y axis is log10(ms).</text>\n" left;
  for tick = int_of_float (floor y_min) to int_of_float (ceil y_max) do
    let yy = y_of_log (float_of_int tick) in
    if yy >= top && yy <= top +. plot_h then begin
      add "<line x1=\"%.0f\" y1=\"%.2f\" x2=\"%.0f\" y2=\"%.2f\" stroke=\"#e5e7eb\" stroke-width=\"1\"/>\n" left yy (left +. plot_w) yy;
      add "<text x=\"%.0f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"11\" text-anchor=\"end\" fill=\"#555\">%.6g ms</text>\n" (left -. 8.) (yy +. 4.) (10. ** float_of_int tick)
    end
  done;
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left top left (top +. plot_h);
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left (top +. plot_h) (left +. plot_w) (top +. plot_h);
  draw_box "old-mlist" "#d95f02" (left +. (plot_w *. 0.35)) old_values;
  draw_box "new-alist-mlist" "#1b9e77" (left +. (plot_w *. 0.65)) new_values;
  begin match old_values, new_values with
  | _ :: _, _ :: _ ->
    let old_s = summarize old_values in
    let new_s = summarize new_values in
    let speedup_percent = if old_s.mean = 0. then 0. else ((old_s.mean -. new_s.mean) /. old_s.mean) *. 100. in
    add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"13\" text-anchor=\"middle\" fill=\"#111\">speedup = %.2f%%</text>\n" (left +. (plot_w /. 2.)) (height - 12) speedup_percent
  | _ -> ()
  end;
  add "</svg>\n";
  File.put_contents output_path (Buffer.contents b);
  true

let csv_path = ref ""
let operations_arg = ref (String.concat "," default_operations)
let plots_dir_arg = ref None
let summary_dir_arg = ref None

let spec =
  [ "-operations", Arg.Set_string operations_arg, " comma-separated list of operations to plot";
    "-plots-dir", Arg.String (fun s -> plots_dir_arg := Some s), " output directory for SVG plots";
    "-summary-dir", Arg.String (fun s -> summary_dir_arg := Some s), " output directory for summary CSV" ]

let () =
  Arg.parse spec
    (fun arg ->
      if !csv_path = "" then csv_path := arg
      else failwith ("Unexpected argument: " ^ arg))
    "Usage: mlist_plot <mlist_microbench.csv> [-operations op1,op2] [-plots-dir dir] [-summary-dir dir]";
  if !csv_path = "" then failwith "Mlist_plot: missing CSV path";
  let rows = parse_rows !csv_path in
  let run_dir = run_dir_from_csv !csv_path in
  let plots_dir = Option.value ~default:(Filename.concat run_dir "plots") !plots_dir_arg in
  let summary_dir = Option.value ~default:(Filename.concat run_dir "summary") !summary_dir_arg in
  let case_plots_dir = Filename.concat plots_dir "mlist_cases" in
  ensure_dir plots_dir;
  ensure_dir case_plots_dir;
  ensure_dir summary_dir;
  let summary_path = Filename.concat summary_dir "mlist_microbench_summary.csv" in
  write_summary rows summary_path;
  Printf.printf "Wrote summary: %s\n" summary_path;
  let operations =
    !operations_arg
    |> split_on_char ','
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  List.iter (fun operation ->
    let path = Filename.concat plots_dir ("mlist_" ^ operation ^ ".svg") in
    if plot_operation rows operation path then
      Printf.printf "Wrote plot: %s\n" path)
    operations;
  let operation_is_selected operation = List.mem operation operations in
  let case_count = ref 0 in
  case_keys_from_rows rows
  |> List.iter (fun key ->
    let operation, size, pattern = split_key key in
    if operation_is_selected operation then begin
      let filename =
        safe_filename (Printf.sprintf "mlist_%s_size_%d_pattern_%s.svg" operation size pattern)
      in
      let path = Filename.concat case_plots_dir filename in
      if plot_case rows operation size pattern path then
        incr case_count
    end);
  Printf.printf "Wrote %d per-case plots under: %s\n" !case_count case_plots_dir
