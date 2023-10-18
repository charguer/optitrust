(**
  Currently, should be called from current folder.
    *)

open Soup
open Printf
open Optitrust

let verbose = true

let current_module = "Variable"
let prefix = (String.lowercase_ascii current_module) ^ "_"

let do_or_die (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

let compute_unit_tests () =
  (* let tmp_file = Filename.temp_file "all" ".tests" in *)
  let tmp_file = "all.tests" in
  (* LATER: batch find for multiple modules. *)
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

let _ =
  let unit_tests = compute_unit_tests () in
  let c = open_in "odoc_spec.html" in
  let soup = read_channel c |> parse in
  close_in c;

  soup $$ ".odoc-spec" |> iter (fun spec ->
    let id = spec $ ".anchored" |> id |> Option.get in
    let name = String.sub id 4 (String.length id - 4) in (* Remove the 'val-' prefix *)
    let test_name = prefix ^ name in
    if verbose then Printf.printf "Reached documentation for %s --> " test_name;
    match List.assoc_opt test_name unit_tests with
    | None -> if verbose then Printf.printf "Not found\n"; ()
    | Some path ->
        if verbose then Printf.printf "Found\n";
        let p = create_element ~inner_text:path "p" in
        let div = create_element ~classes:["spec-unit-test"] "div" in
        append_child div p;
        append_child spec div
  );

  let out = open_out "odoc_spec_after.html" in
  (* TODO: pretty_print option *)
  write_channel out (to_string soup);
  close_out out
