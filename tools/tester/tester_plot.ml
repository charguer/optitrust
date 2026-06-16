open Optitrust

type row = {
  implementation : string;
  test_name : string;
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

let split_on_char sep s =
  let rec aux start i acc =
    if i = String.length s then
      List.rev (String.sub s start (i - start) :: acc)
    else if s.[i] = sep then
      aux (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
    else
      aux start (i + 1) acc
  in
  aux 0 0 []

let csv_escape s =
  if String.exists (fun c -> c = ',' || c = '"' || c = '\n' || c = '\r') s then
    "\"" ^ Str.global_replace (Str.regexp_string "\"") "\"\"" s ^ "\""
  else
    s

let csv_line fields =
  String.concat "," (List.map csv_escape fields)

let svg_escape s =
  s
  |> Str.global_replace (Str.regexp_string "&") "&amp;"
  |> Str.global_replace (Str.regexp_string "<") "&lt;"
  |> Str.global_replace (Str.regexp_string ">") "&gt;"
  |> Str.global_replace (Str.regexp_string "\"") "&quot;"

let ensure_dir dir =
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

let run_dir_from_csv path =
  let absolute =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  absolute |> Filename.dirname |> Filename.dirname

let index_of name headers =
  let rec aux i =
    if i >= Array.length headers then failwith ("tester_plot: missing CSV column " ^ name)
    else if headers.(i) = name then i
    else aux (i + 1)
  in
  aux 0

let parse_csv path =
  match File.get_lines path with
  | [] -> []
  | header :: lines ->
    let headers = Array.of_list (split_on_char ',' header) in
    let implementation_i = index_of "implementation" headers in
    let test_name_i = index_of "test_name" headers in
    let elapsed_i = index_of "elapsed_ms" headers in
    let final_status_i = index_of "final_status" headers in
    List.filter_map (fun line ->
      match split_on_char ',' line with
      | fields when List.length fields > final_status_i ->
        let field i = List.nth fields i in
        Some {
          implementation = field implementation_i;
          test_name = field test_name_i;
          elapsed_ms = float_of_string (field elapsed_i);
        }
      | _ -> None)
      lines

let percentile values p =
  let n = Array.length values in
  if n = 0 then 0. else
  let pos = float_of_int (n - 1) *. p in
  let lo = int_of_float (floor pos) in
  let hi = int_of_float (ceil pos) in
  if lo = hi then values.(lo)
  else
    let w = pos -. float_of_int lo in
    values.(lo) +. ((values.(hi) -. values.(lo)) *. w)

let summarize values =
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

let group_values rows =
  let h = Hashtbl.create 257 in
  List.iter (fun row ->
    let key = row.test_name, row.implementation in
    let prev = Option.value ~default:[] (Hashtbl.find_opt h key) in
    Hashtbl.replace h key (row.elapsed_ms :: prev))
    rows;
  h

let test_names rows =
  rows
  |> List.map (fun row -> row.test_name)
  |> List.sort_uniq compare

let values grouped test implementation =
  Option.value ~default:[] (Hashtbl.find_opt grouped (test, implementation))

let write_summary rows summary_path =
  let grouped = group_values rows in
  let lines =
    csv_line
      [ "test_name";
        "old_count";
        "new_count";
        "old_mean_ms";
        "new_mean_ms";
        "speedup_old_over_new";
        "new_mean_over_old_mean";
        "speedup_percent_new_vs_old";
        "old_median_ms";
        "new_median_ms";
        "old_min_ms";
        "new_min_ms";
        "old_max_ms";
        "new_max_ms";
        "old_stddev_ms";
        "new_stddev_ms" ]
    :: List.filter_map (fun test ->
      let old_values = values grouped test "old-mlist" in
      let new_values = values grouped test "new-alist-mlist" in
      if old_values = [] && new_values = [] then None else
      let old_s = summarize old_values in
      let new_s = summarize new_values in
      let speedup =
        if old_s.count = 0 || new_s.count = 0 || old_s.mean = 0. then 0.
        else ((old_s.mean -. new_s.mean) /. old_s.mean) *. 100.
      in
      let speedup_factor =
        if old_s.count = 0 || new_s.count = 0 || new_s.mean = 0. then 0.
        else old_s.mean /. new_s.mean
      in
      let new_over_old =
        if old_s.count = 0 || new_s.count = 0 || old_s.mean = 0. then 0.
        else new_s.mean /. old_s.mean
      in
      Some (csv_line
        [ test;
          string_of_int old_s.count;
          string_of_int new_s.count;
          Printf.sprintf "%.9g" old_s.mean;
          Printf.sprintf "%.9g" new_s.mean;
          Printf.sprintf "%.9g" speedup_factor;
          Printf.sprintf "%.9g" new_over_old;
          Printf.sprintf "%.9g" speedup;
          Printf.sprintf "%.9g" old_s.median;
          Printf.sprintf "%.9g" new_s.median;
          Printf.sprintf "%.9g" old_s.min_v;
          Printf.sprintf "%.9g" new_s.min_v;
          Printf.sprintf "%.9g" old_s.max_v;
          Printf.sprintf "%.9g" new_s.max_v;
          Printf.sprintf "%.9g" old_s.stddev;
          Printf.sprintf "%.9g" new_s.stddev ]))
      (test_names rows)
  in
  File.put_contents summary_path (String.concat "\n" lines ^ "\n")

let log10_safe x =
  log (max x 0.000001) /. log 10.

let safe_filename s =
  let b = Bytes.of_string s in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' -> ()
    | _ -> Bytes.set b i '_'
  done;
  Bytes.to_string b

let plot_overview rows output_path limit =
  let grouped = group_values rows in
  let tests =
    test_names rows
    |> List.map (fun test ->
      let old_s = summarize (values grouped test "old-mlist") in
      let new_s = summarize (values grouped test "new-alist-mlist") in
      let score = max old_s.mean new_s.mean in
      test, score)
    |> List.sort (fun (_a, score_a) (_b, score_b) -> compare score_b score_a)
    |> List.map fst
    |> (fun xs ->
      let rec take n xs =
        if n <= 0 then [] else match xs with [] -> [] | x :: xs -> x :: take (n - 1) xs
      in
      take limit xs)
  in
  if tests = [] then false else
  let all_values = List.map (fun row -> row.elapsed_ms) rows in
  let y_min = log10_safe (List.fold_left min max_float all_values) in
  let y_max = log10_safe (List.fold_left max min_float all_values) in
  let y_min, y_max =
    if y_min = y_max then y_min -. 1., y_max +. 1.
    else
      let pad = (y_max -. y_min) *. 0.08 in
      y_min -. pad, y_max +. pad
  in
  let group_count = List.length tests in
  let width = max 1000 (160 + (group_count * 120)) in
  let height = 650 in
  let left = 88. in
  let right = 30. in
  let top = 58. in
  let bottom = 190. in
  let plot_w = float_of_int width -. left -. right in
  let plot_h = float_of_int height -. top -. bottom in
  let x_group i =
    if group_count = 1 then left +. (plot_w /. 2.)
    else left +. (float_of_int i *. (plot_w /. float_of_int (group_count - 1)))
  in
  let y ms =
    top +. ((y_max -. log10_safe ms) /. (y_max -. y_min) *. plot_h)
  in
  let b = Buffer.create 65536 in
  let add fmt = Printf.bprintf b fmt in
  let draw_values color x values =
    let stats = summarize values in
    values
    |> List.sort compare
    |> List.iteri (fun j value ->
      let jitter = float_of_int ((j mod 7) - 3) *. 2.4 in
      add "<circle cx=\"%.2f\" cy=\"%.2f\" r=\"3\" fill=\"%s\" fill-opacity=\"0.55\"/>\n" (x +. jitter) (y value) color);
    if stats.count > 0 then
      add "<path d=\"M %.2f %.2f L %.2f %.2f L %.2f %.2f L %.2f %.2f Z\" fill=\"%s\"/>\n"
        x (y stats.mean -. 5.) (x +. 5.) (y stats.mean) x (y stats.mean +. 5.) (x -. 5.) (y stats.mean) color
  in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n" width height width height;
  add "<rect width=\"100%%\" height=\"100%%\" fill=\"white\"/>\n";
  add "<text x=\"%.0f\" y=\"30\" font-family=\"sans-serif\" font-size=\"20\" font-weight=\"700\">Tester timing by test</text>\n" left;
  add "<text x=\"%.0f\" y=\"50\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#555\">Top tests by elapsed time. Dots are measured iterations; diamond is mean. Y axis is log10(ms).</text>\n" left;
  for tick = int_of_float (floor y_min) to int_of_float (ceil y_max) do
    let yy = top +. ((y_max -. float_of_int tick) /. (y_max -. y_min) *. plot_h) in
    if yy >= top && yy <= top +. plot_h then begin
      add "<line x1=\"%.0f\" y1=\"%.2f\" x2=\"%.0f\" y2=\"%.2f\" stroke=\"#e5e7eb\"/>\n" left yy (left +. plot_w) yy;
      add "<text x=\"%.0f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"11\" text-anchor=\"end\" fill=\"#555\">%.6g ms</text>\n" (left -. 8.) (yy +. 4.) (10. ** float_of_int tick)
    end
  done;
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\"/>\n" left top left (top +. plot_h);
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\"/>\n" left (top +. plot_h) (left +. plot_w) (top +. plot_h);
  List.iteri (fun i test ->
    let gx = x_group i in
    let old_values = values grouped test "old-mlist" in
    let new_values = values grouped test "new-alist-mlist" in
    let old_s = summarize old_values in
    let new_s = summarize new_values in
    if old_s.count > 0 then draw_values "#d95f02" (gx -. 12.) old_values;
    if new_s.count > 0 then draw_values "#1b9e77" (gx +. 12.) new_values;
    if old_s.count > 0 && new_s.count > 0 && old_s.mean <> 0. then begin
      let speedup = ((old_s.mean -. new_s.mean) /. old_s.mean) *. 100. in
      let speedup_factor = if new_s.mean = 0. then 0. else old_s.mean /. new_s.mean in
      add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\">speedup=%.1f%% (%.2fx)</text>\n" gx (height - 82) speedup speedup_factor
    end;
    add "<text x=\"%.2f\" y=\"%d\" transform=\"rotate(-35 %.2f %d)\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"end\">%s</text>\n"
      gx (height - 58) gx (height - 58) (svg_escape (Filename.basename test)))
    tests;
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#d95f02\" fill-opacity=\"0.45\" stroke=\"#d95f02\"/>\n" left (height - 30);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">old-mlist</text>\n" (left +. 18.) (height - 20);
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#1b9e77\" fill-opacity=\"0.45\" stroke=\"#1b9e77\"/>\n" (left +. 120.) (height - 30);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">new-alist-mlist</text>\n" (left +. 138.) (height - 20);
  add "</svg>\n";
  File.put_contents output_path (Buffer.contents b);
  true

let plot_speedup rows output_path limit =
  let grouped = group_values rows in
  let tests =
    test_names rows
    |> List.filter (fun test ->
      values grouped test "old-mlist" <> []
      && values grouped test "new-alist-mlist" <> [])
    |> List.map (fun test ->
      let old_s = summarize (values grouped test "old-mlist") in
      let new_s = summarize (values grouped test "new-alist-mlist") in
      let score = max old_s.mean new_s.mean in
      test, score)
    |> List.sort (fun (_a, score_a) (_b, score_b) -> compare score_b score_a)
    |> List.map fst
    |> (fun xs ->
      let rec take n xs =
        if n <= 0 then [] else match xs with [] -> [] | x :: xs -> x :: take (n - 1) xs
      in
      take limit xs)
  in
  if tests = [] then false else
  let speedup_cutoff = 4. in
  let y_max = speedup_cutoff in
  let group_count = List.length tests in
  let width = max 1000 (160 + (group_count * 120)) in
  let height = 650 in
  let left = 88. in
  let right = 30. in
  let top = 58. in
  let bottom = 190. in
  let plot_w = float_of_int width -. left -. right in
  let plot_h = float_of_int height -. top -. bottom in
  let x_group i =
    if group_count = 1 then left +. (plot_w /. 2.)
    else left +. (float_of_int i *. (plot_w /. float_of_int (group_count - 1)))
  in
  let y factor =
    top +. ((y_max -. factor) /. y_max *. plot_h)
  in
  let b = Buffer.create 65536 in
  let add fmt = Printf.bprintf b fmt in
  let draw_bar x width color factor =
    let display_factor = min factor speedup_cutoff in
    let y0 = y 0. in
    let y1 = y display_factor in
    add "<rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" fill-opacity=\"0.74\"/>\n"
      (x -. (width /. 2.)) y1 width (max 1. (y0 -. y1)) color;
    if factor > speedup_cutoff then begin
      add "<line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"#111827\" stroke-width=\"2\" stroke-dasharray=\"3 3\"/>\n"
        (x -. (width /. 2.) -. 3.) (top +. 6.) (x +. (width /. 2.) +. 3.) (top +. 6.);
      add "<path d=\"M %.2f %.2f L %.2f %.2f L %.2f %.2f\" fill=\"none\" stroke=\"#111827\" stroke-width=\"1.4\"/>\n"
        (x -. 6.) (top +. 13.) x (top +. 6.) (x +. 6.) (top +. 13.)
    end
  in
  add "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">\n" width height width height;
  add "<rect width=\"100%%\" height=\"100%%\" fill=\"white\"/>\n";
  add "<text x=\"%.0f\" y=\"30\" font-family=\"sans-serif\" font-size=\"20\" font-weight=\"700\">Tester speedup by test</text>\n" left;
  add "<text x=\"%.0f\" y=\"50\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#555\">Y axis is speedup factor capped at 4x. Dashed marks mean the real new bar is higher.</text>\n" left;
  for tick = 0 to 5 do
    let factor = y_max *. float_of_int tick /. 5. in
    let yy = y factor in
    add "<line x1=\"%.0f\" y1=\"%.2f\" x2=\"%.0f\" y2=\"%.2f\" stroke=\"#e5e7eb\" stroke-width=\"1\"/>\n" left yy (left +. plot_w) yy;
    add "<text x=\"%.0f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"11\" text-anchor=\"end\" fill=\"#555\">%.2fx</text>\n" (left -. 8.) (yy +. 4.) factor
  done;
  let baseline_y = y 1. in
  add "<line x1=\"%.0f\" y1=\"%.2f\" x2=\"%.0f\" y2=\"%.2f\" stroke=\"#111827\" stroke-width=\"1.4\" stroke-dasharray=\"4 4\"/>\n" left baseline_y (left +. plot_w) baseline_y;
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left top left (top +. plot_h);
  add "<line x1=\"%.0f\" y1=\"%.0f\" x2=\"%.0f\" y2=\"%.0f\" stroke=\"#111\" stroke-width=\"1\"/>\n" left (top +. plot_h) (left +. plot_w) (top +. plot_h);
  List.iteri (fun i test ->
    let gx = x_group i in
    let old_s = summarize (values grouped test "old-mlist") in
    let new_s = summarize (values grouped test "new-alist-mlist") in
    let factor = if new_s.mean = 0. then 0. else old_s.mean /. new_s.mean in
    let percent = if factor = 0. then 0. else (1. -. (1. /. factor)) *. 100. in
    let label_y = y (min factor speedup_cutoff) -. 6. in
    draw_bar (gx -. 15.) 20. "#d95f02" 1.;
    draw_bar (gx +. 15.) 20. "#1b9e77" factor;
    add "<text x=\"%.2f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\" fill=\"#111\">%.2fx</text>\n" (gx +. 15.) label_y factor;
    add "<text x=\"%.2f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"middle\" fill=\"#111\">%.1f%%</text>\n" gx (height - 88) percent;
    add "<text x=\"%.2f\" y=\"%d\" transform=\"rotate(-35 %.2f %d)\" font-family=\"sans-serif\" font-size=\"9\" text-anchor=\"end\">%s</text>\n"
      gx (height - 58) gx (height - 58) (svg_escape (Filename.basename test)))
    tests;
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#d95f02\" fill-opacity=\"0.74\"/>\n" left (height - 30);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">old baseline 1.00x</text>\n" (left +. 18.) (height - 20);
  add "<rect x=\"%.0f\" y=\"%d\" width=\"12\" height=\"12\" fill=\"#1b9e77\" fill-opacity=\"0.74\"/>\n" (left +. 170.) (height - 30);
  add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"12\">new speedup factor</text>\n" (left +. 188.) (height - 20);
  add "</svg>\n";
  File.put_contents output_path (Buffer.contents b);
  true

let plot_case rows test output_path =
  let rows = List.filter (fun row -> row.test_name = test) rows in
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
  let width = 760 in
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
  add "<text x=\"%.0f\" y=\"28\" font-family=\"sans-serif\" font-size=\"20\" font-weight=\"700\">%s</text>\n" left (svg_escape (Filename.basename test));
  add "<text x=\"%.0f\" y=\"50\" font-family=\"sans-serif\" font-size=\"12\" fill=\"#555\">Old vs new for one tester workload. Dots are measured iterations; diamond is mean; y axis is log10(ms).</text>\n" left;
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
    let speedup_factor = if new_s.mean = 0. then 0. else old_s.mean /. new_s.mean in
    add "<text x=\"%.0f\" y=\"%d\" font-family=\"sans-serif\" font-size=\"13\" text-anchor=\"middle\" fill=\"#111\">speedup = %.2f%% (%.2fx)</text>\n" (left +. (plot_w /. 2.)) (height - 12) speedup_percent speedup_factor
  | _ -> ()
  end;
  add "</svg>\n";
  File.put_contents output_path (Buffer.contents b);
  true

let csv_paths = ref []
let plots_dir_arg = ref None
let summary_dir_arg = ref None
let limit = ref 80

let spec =
  [ "-plots-dir", Arg.String (fun s -> plots_dir_arg := Some s), " output directory for SVG plots";
    "-summary-dir", Arg.String (fun s -> summary_dir_arg := Some s), " output directory for summary CSV";
    "-limit", Arg.Set_int limit, " maximum tests in overview plot" ]

let () =
  Arg.parse spec
    (fun arg -> csv_paths := arg :: !csv_paths)
    "Usage: tester_plot <per_test_timing.csv> [more.csv ...] [-plots-dir dir] [-summary-dir dir] [-limit n]";
  let csv_paths = List.rev !csv_paths in
  if csv_paths = [] then failwith "tester_plot: missing CSV path";
  let rows = List.concat_map parse_csv csv_paths in
  let run_dir = run_dir_from_csv (List.hd csv_paths) in
  let plots_dir = Option.value ~default:(Filename.concat run_dir "plots") !plots_dir_arg in
  let summary_dir = Option.value ~default:(Filename.concat run_dir "summary") !summary_dir_arg in
  let case_plots_dir = Filename.concat plots_dir "tester_cases" in
  ensure_dir plots_dir;
  ensure_dir case_plots_dir;
  ensure_dir summary_dir;
  let summary_path = Filename.concat summary_dir "tester_timing_summary.csv" in
  write_summary rows summary_path;
  Printf.printf "Wrote summary: %s\n" summary_path;
  let plot_path = Filename.concat plots_dir "tester_tests.svg" in
  if plot_overview rows plot_path !limit then
    Printf.printf "Wrote plot: %s\n" plot_path;
  let speedup_path = Filename.concat plots_dir "tester_tests_speedup.svg" in
  if plot_speedup rows speedup_path !limit then
    Printf.printf "Wrote speedup plot: %s\n" speedup_path;
  let case_count = ref 0 in
  test_names rows
  |> List.iter (fun test ->
    let filename = safe_filename (Printf.sprintf "tester_%s.svg" test) in
    let path = Filename.concat case_plots_dir filename in
    if plot_case rows test path then
      incr case_count);
  Printf.printf "Wrote %d per-test plots under: %s\n" !case_count case_plots_dir
