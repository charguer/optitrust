(**
  Currently, should be called from current folder.
    *)

open Soup
open Printf
open Optitrust

let run_tester = false
let verbose = true
let path_to_webview_folder = "../../tools/web_view"

let current_module = "Variable"
let prefix = (String.lowercase_ascii current_module) ^ "_"

let example_diff_string = "ZGlmZiAtLWdpdCBhL3ZhcmlhYmxlX2lubGluZV9kb2NfYmVmb3JlLmNwcCBiL3ZhcmlhYmxlX2lubGluZV9kb2NfYWZ0ZXIuY3BwCmluZGV4IDBjZTEwNmI3Li5jMTBhOGQ4MiAxMDA2NDQKLS0tIGEvdmFyaWFibGVfaW5saW5lX2RvY19iZWZvcmUuY3BwCisrKyBiL3ZhcmlhYmxlX2lubGluZV9kb2NfYWZ0ZXIuY3BwCkBAIC0xLDUgKzEsNCBAQAogaW50IG1haW4oKSB7Ci0gIGNvbnN0IGludCBhID0gMzsKICAgY29uc3QgaW50IGIgPSA0OwotICBpbnQgciA9IGEgKyBhICsgYjsKKyAgaW50IHIgPSAzICsgMyArIGI7CiB9Cg=="

let do_or_die (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

type test_map = (string * string) list

let compute_test_map () : test_map =
  (* let tmp_file = Filename.temp_file "all" ".tests" in *)
  let tmp_file = "all.tests" in
  (* LATER: batch find for multiple modules. *) (* NOTE: excluding with_lines.ml seems no longer needed *)
  do_or_die(sprintf "find ../../tests/ -name '%s*_doc.ml' -and -not -name '*_with_lines.ml' > %s" prefix tmp_file);
  let remove_doc_suffix (name : string) : string =
    let n = String.length name in
    if n < 4 then failwith "no _doc suffix";
    String.sub name 0 (n - 4) in
  Xfile.get_lines tmp_file |> List.map (fun p ->
    let test_name = p |> Filename.basename |> Filename.chop_extension |> remove_doc_suffix in
    if verbose then Printf.printf "Collected unit test for: '%s'\n" test_name;
    (test_name, p)
  )

let insert_contents_for_test (test_name: string) (_path:string) (target_div : 'a node) : unit =
  (* Generate a div with class "diff-unit-test" and id e.g. "variable_inline" *) (* LATER:could add a prefix to the id *)
  let diff_div = create_element ~id:test_name ~classes:["diff-unit-test"] "div" ~inner_text:example_diff_string in
  append_child target_div diff_div

  (* Extract the OCaml source code *)
  (*let mldiv = create_element ~classes:["spec-unit-test"] "div" in
  *)

 (**

# Generate an OCaml file containing the script executed by the demo
%_doc.txt: %.ml
	$(V)$(OPTITRUST)/doc/extract_demo.sh $<
	@echo Produced $@

	@echo "function get_diff_$*() { return window.atob(\"`git diff  --ignore-blank-lines --ignore-all-space --no-index -U100 $*_doc.cpp $*_doc_out.cpp | base64 -w 0`\"); }" > $@


	  JSFILE="${BASENAME}_doc.js"
  echo "<script src="${JSFILE}"></script>" >> ${OUTFILE}
  echo "<div class="test" id="${BASENAME}"></div>" >> ${OUTFILE}
*)




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
  (* TODO: FIX ESCAPING
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
  if run_tester then do_or_die "../../tester run _doc.ml";
  (* Build a map from test names to test paths *)
  let test_map = compute_test_map () in
  (* Patch the html documentation to insert test material *)
  process_documentation test_map


