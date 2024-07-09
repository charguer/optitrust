(** Should be called from projet root. *)

open Soup
open Printf
open Optitrust

let debug = false
let verbose = false

(* let input_doc_folder = "_build/default/_doc/_html/optitrust/Optitrust/" *)
let path_to_doc_folder = "_doc/optitrust/Optitrust/"
let path_to_doc_root = "../../../"
let path_from_doc_to_project_root = "../../../../"

let tmp_file = Filename.temp_file "ocaml_excerpt" ".txt"


let do_or_die (cmd : string) : unit =
  if debug then printf "Exec: %s\n" cmd;
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

type test_map = (string * string) list

let compute_test_map () : test_map =
  (* NOTE: module prefix and excluding with_lines.ml seems no longer needed: -name '%s*_doc.ml' -and -not -name '*_with_lines.ml' *)
  do_or_die (sprintf "find tests/ -name '*_doc.ml' > %s" tmp_file);
  if debug && false then begin
    printf "List of *_doc.ml find found:\n";
    do_or_die (sprintf "cat %s" tmp_file);
  end;
  let remove_doc_suffix (name : string) : string =
    let n = String.length name in
    if n < 4 then failwith "no _doc suffix";
    String.sub name 0 (n - 4) in
  let tests = File.get_lines tmp_file |> List.map (fun p ->
    let test_name = p |> Filename.basename |> Filename.chop_extension |> remove_doc_suffix in
    (test_name, p)
  ) in
  if verbose then Printf.printf "Collected unit tests for: '%s'\n" (Tools.list_to_string (List.map (fun (t, _) -> t) tests));
  tests

let insert_contents_for_test (test_name: string) (test_path: string) (target_div : 'a node) : unit =
  let test_base = Filename.remove_extension test_path in
  let add : 'a node -> unit = append_child target_div in
  (* Generate a div for the ml excerpt *)
  do_or_die (sprintf "doc/extract_demo.sh %s %s" test_path tmp_file);
  let ml_excerpt = File.get_contents_or_empty tmp_file in
  add (create_element ~classes:["code-unit-test"] "pre" ~inner_text:ml_excerpt);
  (* Generate a div for the diff
     with class "diff-unit-test" and id e.g. "variable_inline" *)
    (* LATER:could add a prefix to the id *)
  let input_cpp_file = test_base ^ ".cpp" in
  let expected_cpp_file = test_base ^ "_exp.cpp" in
  if not (Sys.file_exists input_cpp_file) then begin
    eprintf "Could not find file %s\n" input_cpp_file
  end else if not (Sys.file_exists expected_cpp_file) then begin
    eprintf "Could not find file %s\n" expected_cpp_file
  end else begin
    do_or_die (sprintf "git diff --ignore-blank-lines --ignore-all-space --no-index -U100 %s %s | base64 -w 0 > %s" input_cpp_file expected_cpp_file tmp_file);
    let diff_string = File.get_contents_or_empty tmp_file in
    if diff_string = ""
    (* TODO: use Tools.warn *)
    then Printf.printf "WARNING: empty diff for '%s'\n" test_base
    else add (create_element ~id:test_name ~classes:["diff-unit-test"] "div" ~inner_text:diff_string)
  end;
  (* Generate a div with a link *)
  let trace_file = test_base ^ "_trace.html" in
  if (Sys.file_exists trace_file) then begin
    (* DEPRECATED: done outside of this executable:
         do_or_die (sprintf "tools/build_trace.sh %s" test_base); *)
    (* ALTERNATIVE: don't assume trace is there, have click generate it on the fly. *)
    let dest = path_from_doc_to_project_root ^ trace_file in
    add (create_element ~classes:["doc-unit-test"] "a" ~attributes:[("href", dest)] ~inner_text:"view trace")
  end


let process_spec (prefix : string) (deprecated_suffix : string) (test_map : test_map) (spec : 'a node) : unit =
  let id = spec $ ".anchored" |> id |> Option.get in
  let name = String.sub id 4 (String.length id - 4) in (* Remove the 'val-' prefix *)
  let test_name = prefix ^ name in (* e.g. variable_inline *)
  if verbose then Printf.printf "Reached documentation for %s --> " test_name;
  match List.assoc_opt (test_name ^ deprecated_suffix) test_map with
  | None -> if verbose then Printf.printf "Not found\n"; ()
  | Some path ->
      if verbose then Printf.printf "Found\n";
      (* Generate a div with class "doc-unit-test" *)
      let div = create_element ~classes:["doc-unit-test"] "div" in
      append_child spec div;
      insert_contents_for_test test_name path div

let process_specs (current_module_lowercase) (test_map : test_map) (soup : 'a node) : unit =
  (* String.lowercase_ascii *)
  let (prefix, deprecated_suffix) =
    if String.ends_with ~suffix:"_basic" current_module_lowercase
    then (String.sub current_module_lowercase 0 (String.length current_module_lowercase - (String.length "basic")), "_basic") (* DEPRECATED _basic case *)
    else (current_module_lowercase ^ "_", "")
  in
  soup $$ ".odoc-spec" |> iter (process_spec prefix deprecated_suffix test_map)

let parse_html (html_path : string) : soup node =
  let c = open_in html_path in
  let soup = read_channel c |> parse in
  close_in c;
  soup

(* TODO: pretty_print option *)
let write_html (soup : 'a node) (html_path : string) : unit =
  let c = open_out html_path in
  write_channel c (to_string soup);
  close_out c

let insert_headers (soup : 'a node) : unit =
  let base = "
   <link rel='stylesheet' type='text/css' href='{ROOT}/tools/web_view/lib/highlight.js_github.min.css' />\
   <link rel='stylesheet' type='text/css' href='{ROOT}/tools/web_view/lib/diff2html.min.css' />\
   <link rel='stylesheet' type='text/css' href='{ROOT}/doc/odoc_extra.css' />\
   \
   <script type='text/javascript' src='{ROOT}/tools/web_view/lib/highlight_custom/highlight.pack.js'></script>\
   <script type='text/javascript' src='{ROOT}/tools/web_view/lib/jquery.min.js'></script>\
   <script type='text/javascript' src='{ROOT}/tools/web_view/lib/jquery-ui.min.js'></script>\
   <script type='text/javascript' src='{ROOT}/tools/web_view/lib/diff2html-ui.min.js'></script>\
   <script type='text/javascript' src='{ROOT}/doc/odoc_extra.js'></script>" in
  let headers = Str.global_replace (Str.regexp_string "{ROOT}") path_from_doc_to_project_root base in
  (* Printf.printf "headers:\n%s\n" headers; *)
  soup $ "head" |> (fun head ->
    let defs = parse headers in
    append_child head defs)

let process_documentation (current_module_lowercase : string) (test_map : test_map) : unit =
  let html_path = sprintf "%s/%s/index.html" path_to_doc_folder (String.capitalize_ascii current_module_lowercase) in
  let soup = parse_html html_path in (* FOR TESTS: "odoc_spec.html" *)
  insert_headers soup;
  process_specs current_module_lowercase test_map soup;
  (* TODO: backup option *)
  write_html soup html_path (* FOR TESTS: "odoc_spec_after.html" *)

let _ =
  let test_map = compute_test_map () in

  do_or_die (sprintf "find src/transfo -name '*.ml' > %s" tmp_file);
  File.get_lines tmp_file |> List.iter (fun module_src ->
    let current_module_lowercase = Filename.remove_extension (Filename.basename module_src) in
    if verbose then Printf.printf "-- Processing '%s'\n" current_module_lowercase;
    process_documentation current_module_lowercase test_map
  )

