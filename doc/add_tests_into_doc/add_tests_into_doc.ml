(**
  Currently, should be called from current folder.
    *)

open Soup
open Printf
open Optitrust


let current_module = "Loop"
let prefix = (String.lowercase_ascii current_module) ^ "_"

let do_or_die (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

let compute_unit_tests () =
  (* let tmp_file = Filename.temp_file "all" ".tests" in *)
  let tmp_file = "all.tests" in
  (* LATER: batch find for multiple modules. *)
  do_or_die(sprintf "find ../../tests/ -name '%s*.ml' -and -not -name '*_with_lines.ml' > %s" prefix tmp_file);
  Xfile.get_lines tmp_file |> List.map (fun p ->
    let test_name = p |> Filename.basename |> Filename.chop_extension in
    (test_name, p)
  )

let _ =
  let unit_tests = compute_unit_tests () in
  let c = open_in "odoc_spec.html" in
  let soup = read_channel c |> parse in
  close_in c;

  soup $$ ".odoc-spec" |> iter (fun spec ->
    let id = spec $ ".anchored" |> id |> Option.get in
    Printf.printf "%s\n" id;
    let name = String.sub id 4 (String.length id - 4) in
    match List.assoc_opt (prefix ^ name) unit_tests with
    | None -> ()
    | Some path ->
      let p = create_element ~inner_text:path "p" in
      let div = create_element ~classes:["spec-unit-test"] "div" in
      append_child div p;
      append_child spec div
  );

  let out = open_out "odoc_spec_after.html" in
  (* TODO: pretty_print option *)
  write_channel out (to_string soup);
  close_out out