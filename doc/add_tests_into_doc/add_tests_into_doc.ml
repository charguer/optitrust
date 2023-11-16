(**
  Currently, should be called from current folder.
    *)

open Soup
open Printf
open Optitrust

let dump_trace = true
let debug = true
let run_tester = false
let verbose = true
let path_to_webview_folder = "../../tools/web_view"


let current_module = "Loop"
let prefix = (String.lowercase_ascii current_module) ^ "_"

let tmp_file = Filename.temp_file "ocaml_excerpt" ".txt"


let do_or_die (cmd : string) : unit =
  if debug then printf "Exec: %s\n" cmd;
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

type test_map = (string * string) list

let compute_test_map () : test_map =
  (* LATER: batch find for multiple modules. *) (* NOTE: excluding with_lines.ml seems no longer needed *)
  do_or_die(sprintf "find ../../tests/ -name '%s*_doc.ml' -and -not -name '*_with_lines.ml' > %s" prefix tmp_file);
  if debug && false then begin
    printf "List of *_doc.ml find found:\n";
    do_or_die (sprintf "cat %s" tmp_file);
  end;
  let remove_doc_suffix (name : string) : string =
    let n = String.length name in
    if n < 4 then failwith "no _doc suffix";
    String.sub name 0 (n - 4) in
  Xfile.get_lines tmp_file |> List.map (fun p ->
    let test_name = p |> Filename.basename |> Filename.chop_extension |> remove_doc_suffix in
    if verbose then Printf.printf "Collected unit test for: '%s'\n" test_name;
    (test_name, p)
  )

let insert_contents_for_test (test_name: string) (test_path:string) (target_div : 'a node) : unit =
  let test_base = Filename.remove_extension test_path in
  let add : 'a node -> unit = append_child target_div in
  (* Generate a div for the ml excerpt *)
  do_or_die (sprintf "../extract_demo.sh %s %s" test_path tmp_file);
  let ml_excerpt = Xfile.get_contents_or_empty tmp_file in
  add (create_element ~classes:["code-unit-test"] "pre" ~inner_text:ml_excerpt);
  (* Generate a div for the diff
     with class "diff-unit-test" and id e.g. "variable_inline" *)
    (* LATER:could add a prefix to the id *)
  let input_cpp_file = test_base ^ ".cpp" in
  let output_cpp_file = test_base ^ "_out.cpp" in
  if not (Sys.file_exists input_cpp_file) then begin
    eprintf "Could not find file %s\n" input_cpp_file
  end else if not (Sys.file_exists output_cpp_file) then begin
    eprintf "Could not find file %s\n" output_cpp_file
  end else begin
    do_or_die (sprintf "git diff --ignore-blank-lines --ignore-all-space --no-index -U100 %s %s | base64 -w 0 > %s" input_cpp_file output_cpp_file tmp_file);
    let diff_string = Xfile.get_contents_or_empty tmp_file in
    add (create_element ~id:test_name ~classes:["diff-unit-test"] "div" ~inner_text:diff_string)
  end;
  (* Generate a div with a link *)
  if dump_trace then begin
    do_or_die (sprintf "../../tools/build_trace.sh %s" test_base);
    let dest = test_base ^ "_trace.html" in
    add (create_element ~classes:["doc-unit-test"] "a" ~attributes:[("href", dest)] ~inner_text:"view trace")
  end


let process_spec (test_map : test_map) (spec : 'a node) : unit =
  let id = spec $ ".anchored" |> id |> Option.get in
  let name = String.sub id 4 (String.length id - 4) in (* Remove the 'val-' prefix *)
  let test_name = prefix ^ name in (* e.g. variable_inline *)
  if verbose then Printf.printf "Reached documentation for %s --> " test_name;
  match List.assoc_opt test_name test_map with
  | None -> if verbose then Printf.printf "Not found\n"; ()
  | Some path ->
      if verbose then Printf.printf "Found\n";
      (* Generate a div with class "doc-unit-test" *)
      let div = create_element ~classes:["doc-unit-test"] "div" in
      append_child spec div;
      insert_contents_for_test test_name path div

let build_headers () : string = (* TODO: disable escaping *)
  let base = "
   <link rel='stylesheet' type='text/css' href='{WEBVIEW_FOLDER}/lib/highlight.js_github.min.css' />\
   <link rel='stylesheet' type='text/css' href='{WEBVIEW_FOLDER}/lib/diff2html.min.css' />\
   <script type='text/javascript' src='{WEBVIEW_FOLDER}/lib/diff2html-ui.min.js'></script>" in
   Str.global_replace (Str.regexp_string "{WEBVIEW_FOLDER}") path_to_webview_folder base


let process_documentation (test_map : test_map) : unit =
  (* Parse input *)
  let c = open_in "odoc_spec.html" in
  let soup = read_channel c |> parse in
  close_in c;
  (* Insert headers *)
  ignore build_headers;
  (* TODO: FIX HEADER ESCAPING
  soup $ "head" |> (fun head ->
    let defs = create_text (build_headers()) in
    append_child head defs);
  *)
  (* Patch specs *)
  soup $$ ".odoc-spec" |> iter (process_spec test_map);
  (* Write output *) (* TODO: pretty_print option *)
  let out = open_out "odoc_spec_after.html" in
  write_channel out (to_string soup);
  close_out out


let _ =
  (* Run all documentation unit tests *)
  (* TODO : add an arg -skip-run to set run_tester to false *)
  (* TODO: add an option for dump-trace *)
  if run_tester then do_or_die "../../tester run _doc.ml";
  (* Build a map from test names to test paths *)
  let test_map = compute_test_map () in
  (* Patch the html documentation to insert test material *)
  process_documentation test_map
  (* TODO : apply process_documentation to all .html files
     that could contain transformations;
     take "prefix" as argument of functions above *)

