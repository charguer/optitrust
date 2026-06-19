open Optitrust
open Target
module OL = Optitrust_optilambda.Optilambda

(** Regression test for the C/C++ frontend to OptiLambda translation.
    It parses [printcpp.cpp] and writes the printed OptiLambda AST to
    [printcpp_out.opti], for comparison with [printcpp_exp.opti].
    When [OPTITRUST_C_PARSER] is set, the test calls that parser directly. *)

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") && Sys.file_exists (Filename.concat dir "optitrust.opam") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failwith "Could not find OptiTrust repository root"
    else find_repo_root parent

let repo_root = find_repo_root (Sys.getcwd ())
let test_input = "../../../../tests_infra/optilambda/printcpp.cpp"
let test_input_absolute = Filename.concat repo_root "tests_infra/optilambda/printcpp.cpp"
let test_output = Filename.concat repo_root "tests_infra/optilambda/printcpp_out.opti"

let () = Flags.optitrust_root := repo_root

let print_ast trm =
  File.put_contents test_output (Printf.sprintf "-- %s --\n%s\n\n" "CPP test" (OL.trm_to_string trm))

let parse_with_direct_parser parser =
  let input = Unix.realpath test_input_absolute in
  let parser = Unix.realpath parser in
  let exitcode = Sys.command (Printf.sprintf "cd \"%s\" && \"%s\" %s" repo_root parser (Filename.quote input)) in
  if exitcode <> 0 then failwith (Printf.sprintf "C parser returned with error code %d" exitcode);
  let ser_file = open_in_bin (input ^ ".ser") in
  Fun.protect
    ~finally:(fun () -> close_in ser_file)
    (fun () ->
      let _deps = Marshal.from_channel ser_file in
      let _, raw_ast = Marshal.from_channel ser_file in
      if !Flags.bypass_cfeatures then Scope_computation.infer_var_ids raw_ast else C_encoding.decode_from_c raw_ast)

let _ =
  match Sys.getenv_opt "OPTITRUST_C_PARSER" with
  | Some parser -> print_ast (parse_with_direct_parser parser)
  | None ->
      Run.script_cpp ~filename:test_input (fun _ ->
        print_ast (Trace.ast()))
