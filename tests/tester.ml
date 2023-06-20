open Optitrust
open Printf

module StringSet = Set.Make(String)

(**

  This file tester.ml is the source for the program ./tester.exe, for executing unit tests.
  Assume for simplicity that ./tester.exe is executed from the project root (containing dune-project), using, e.g:

  ./tester args

  It features:
  - caching of AST representation for input and expected output files
  - management of dependencies on the source code of optitrust
  - selection of a subset of tests to process (${args} is by default "all")
    (either via filenames, or via a 'key' name refering to a groupe of files)
  - comparison either at the AST level or at the cpp source level via diff
  - call to "./tester last" reuses the arguments provided to the last call
    (these arguments are saved in the file ./last.tests,
    the target "all" is taken if this file does not exist yet)

  It produces as output:
  - information on which test fails
  - if requested or else only for tests that fail, produce the output files *_out.cpp
  - generates a bash script to be used for approving changes on expected files
    in case of test failures that result from intended changes.



LATER: add options to control whether to generate for each test
(1) the JS trace, and (2) the JS documentation snippet

VERY-LATER: mode for compiling sources with gcc at a given standard

*)

let last_args_file = "last.tests"
  (* LATER: decide if this file could be in _build/default/.. *)

let do_is_ko (cmd : string) : bool =
  let exit_code = Sys.command cmd in
  exit_code != 0

let _do_is_ok (cmd : string) : bool = not (do_is_ko cmd)

let do_or_die (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

    (* command_output
      command_output_lines *)



(*****************************************************************************)
(* Options *)

(* List of keys process; a key may be a special keyword or a path to a .ml test file
   e.g. ["basic/variable_inline.ml"; "combi"].
   The list gets later expanded into a list of paths, without duplicates. *)
let keys_to_process : string list ref = ref []

(* Flag for controlling whether or not to generate *_out.cpp files. *)
type outfile_gen =
  | Outfile_gen_always
  | Outfile_gen_only_on_failure (* failure or missing expected file *)
  | Outfile_gen_never

let outfile_gen : outfile_gen ref = ref Outfile_gen_only_on_failure

let string_to_outfile_gen = function
  | "always" -> Outfile_gen_always
  | "never" -> Outfile_gen_never
  | "onfailure" -> Outfile_gen_only_on_failure
  | _ -> failwith "Invalid argument for -out"

let set_outfile_gen str = outfile_gen := string_to_outfile_gen str

(* Flag to ignore all cached data *)
let ignore_cache : bool ref = ref false

(* Flag to discard all cached data *)
let discard_cache : bool ref = ref false

(* Flag to enable verbose mode *)
let verbose_mode : bool ref = ref false

(* Flag to control at which level the comparison is performed (AST or text).
   If Comparison_method_text, then implies Outfile_gen_always. *)
type comparison_method =
  | Comparison_method_ast (* TODO: do we have this? *)
  | Comparison_method_text

let comparison_method : comparison_method ref = ref Comparison_method_text

let _remove_later = comparison_method := Comparison_method_ast;
  comparison_method := Comparison_method_text


(*****************************************************************************)
(* Saving arguments of the last call in the last target *)

(* Save to the file [last_args_file] the targets provided, assuming the target
   does not contain "last" *)
let save_last_tests (keys : string list) : unit =
  if not (List.mem "last" keys)
    then Xfile.put_lines last_args_file keys
  else if not (Sys.file_exists last_args_file)
    then Xfile.put_lines last_args_file ["all"]

(*****************************************************************************)
(* Description of keys *)

module File_set = Set.Make(String)

let filename_concat folder filename =
  if folder = "." then filename else Filename.concat folder filename


let get_alias_targets (alias_filename: string) : string list =
  let folder = Filename.dirname alias_filename in
  let lines = Xfile.get_lines_or_empty alias_filename in
  List.filter_map (fun l ->
      if String.starts_with ~prefix:"#" l || String.length l = 0
        then None
        else Some (filename_concat folder l)) lines

let get_tests_in_dir (folder: string) : string list * File_set.t =
  let ignored_files = get_alias_targets (filename_concat folder "ignored.tests") in
  let ignored_files = List.fold_left (fun acc f -> File_set.add f acc) File_set.empty ignored_files in
  let folder_files = Sys.readdir folder in
  let test_files = List.filter_map (fun f ->
      if String.ends_with ~suffix:".ml" f && not (String.ends_with ~suffix:"_with_lines.ml" f)
      then
        let filename = filename_concat folder f in
        if File_set.mem filename ignored_files
          then None
          else Some filename
      else None) (Array.to_list folder_files) in
  List.sort String.compare test_files, ignored_files

let rec resolve_test_targets (target_list: string list) : string list * File_set.t =
  let test_files, ignored_file_set =
    List.fold_right (fun target (test_files, ignored_files) ->
    if String.ends_with ~suffix:".ml" target then
      (target :: test_files, ignored_files)
    else
      let alias_filename = target ^ ".tests" in
      let (new_test_files, new_ignored_files) =
        if Sys.file_exists alias_filename then
          let sub_targets = get_alias_targets alias_filename in
          resolve_test_targets sub_targets
        else
          get_tests_in_dir target
      in
      (new_test_files @ test_files, File_set.union ignored_files new_ignored_files)
  ) target_list ([], File_set.empty)
  in
  (test_files, ignored_file_set)


(* Takes the list of target arguments on the command line;
   and expand the 'targets', remove duplicates and ignored tests.
   Returns (tests_to_process, ignored_tests)
*)
let compute_tests_to_process (targets: string list): (string list * string list) =
  let test_files, ignored_file_set = resolve_test_targets targets in
  let test_files = Xlist.remove_duplicates test_files in
  (* Tests that were otherwise selected are not ignored *)
  let ignored_file_set =
    List.fold_left (fun acc t -> File_set.remove t acc) ignored_file_set test_files
  in
  (test_files, File_set.elements ignored_file_set)


(*****************************************************************************)
(* Parsing of options *)

(* [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-out", Arg.String set_outfile_gen, " generate output file: 'always', or 'never', or 'onfailure' (default)");
     ("-ignore-cache", Arg.Set ignore_cache, " ignore the serialized AST, force reparse of source files; does not modify the existing serialized data");
     ("-discard-cache", Arg.Set discard_cache, " clear all serialized AST; save serizalize data for
     tests that are executed.");
     ("-v", Arg.Set verbose_mode, " report details on the testing process.");
  ]

let _main : unit =
  Arg.parse
    (Arg.align (spec @ Flags.spec))
    (fun other_arg -> keys_to_process := other_arg :: !keys_to_process)
    ("usage: ./tester.exe [options] target1 .. targetN");

  (* Flag Comparison_method_text implies generation of output file *)
  if !comparison_method = Comparison_method_text
    then outfile_gen := Outfile_gen_always;

  (* Default target is "all" *)
  let keys_to_process =
    if !keys_to_process = [] then
     ["all"]
    else
      List.rev !keys_to_process in
  save_last_tests keys_to_process;
  let (tests_to_process, ignored_tests) = compute_tests_to_process keys_to_process in

  if List.length tests_to_process > 1
    then Flags.print_backtrace_on_error := false;

  (* TODO: We cache the "raw ast".
  from a trm t, we need to serialize Ast_fromto_AstC.cfeatures_intro t;
  this is what is provided in trace.ml to  the function
   AstC_to_c.ast_to_outchannel ~beautify_mindex ~comment_pragma:use_clang_format out_prog t_after_cfeatures_intro;

    LATER: deal with script_cpp ~filename by searching 'batch.ml'

  for each test:
    test.cpp must exit
    if test.ser exist and .ser up to date: OK
    else update .ser by parsing it from .cpp and serialize

  for each test:
    if test_exp.cpp exist:
      if test_exp.ser exist and  .ser up to date: OK
      else update .ser by parsing it from _exp.cpp and serialize
    else:
      create test_exp.ser with empty ast
  *)
  (* Need to save the cached_inputs and cached_expoutputs :
     -> we want save the ones that were serialized before,
        and update the ones that have just been reparsed *)

  let batch_args = String.concat " " tests_to_process in
  if !verbose_mode
      then eprintf "Tester files processed: \n  %s\n"
        (String.concat "\n  " tests_to_process);
  (* for debug: if true then exit 0; *)

  let _check_all_files_exist =
    List.iter (fun test_file ->
      if not (Sys.file_exists test_file)
        then failwith (sprintf "File not found:"))
      tests_to_process;
    in

  (* TODO : tester si la liste de test est vide *)

  (* TODO: faire la boucle en caml sur l'appel à sed
   à chaque fois afficher un commentaire (* CURTEST=... *)
     TODO: add 'batch_prelude' and 'batch_postlude' calls.
     LATER: ajouter ici l'option ~expected_ast , et concatener l'appel à Run.batch_postlude logfilename *)
  do_or_die ("tests/batch_tests.sh " ^ batch_args ^ " > " ^ "tests/batch/batch.ml");

  let delete_output test =
    (* LATER: the following line raises Invalid_argument("Filename.chop_extension")
        in case the test is not a valid path, ie to a file with an extension *)
    let test_prefix = Filename.chop_extension test in
    let filename_out = sprintf "%s_out.cpp" test_prefix in
    ignore (do_is_ko (sprintf "rm -f %s > /dev/null" filename_out))
    (* LATER: remove without displaying error messages or missing files *)
  in
  List.iter delete_output tests_to_process;

  do_or_die "cp tests/batch/dune_disabled tests/batch/dune";
  do_or_die "dune build tests/batch/batch.cmxs; rm tests/batch/dune";
  printf "\n";

  (* LATER: if -dump_trace is requested, use _with_lines files *)

  (* TODO: rediriiger l'erreur dans un fichier  2>&
    Sys.command en version booléenne
    ERRLINE = cat errorlog | head -n 1 | awk '{print $2}'
     ou grep ", line "
    head -n ${ERRLINE} batch.ml | grep "batching" | tail -1
     *)
  (* TODO: flags *)
  (* DEPRECATED:
     do_or_die "OCAMLRUNPARAM=b dune exec runner/optitrust_runner.exe -- tests/batch/batch.cmxs"; *)
  begin try
    Flags.program_name := "tester.ml";
    Dynlink.loadfile "tests/batch/batch.cmxs"
  with
    Dynlink.Error err -> begin
      let sbt = Printexc.get_backtrace() in
      Printf.eprintf "%s\n%s" (Dynlink.error_message err) sbt;
      exit 1
    end
  end;

  (* c'est le code de batch.ml
     qui fait la gestion des cached_inputs/cached_outputs

     et qui pourrait faire la comparaison au niveau AST

     ./batch_controller.ml prendrait en argument presque tous les arguments de tester.ml

     tester.ml would do only the computation of the list of tests
     and the generation of batch.ml and the launching of batcher.exe

  *)

  let ok_count = ref 0 in
  let ko_count = ref 0 in
  let meld_args = ref [] in
  let ignore_args = ref [] in
  let error_tests = ref [] in
  (* Compare all text outputs if necessary *)
  if !comparison_method = Comparison_method_text then
    let check_output test =
      let test_prefix = Filename.chop_extension test in
      let filename_out = sprintf "%s_out.cpp" test_prefix in
      let filename_exp = sprintf "%s_exp.cpp" test_prefix in
      if Sys.file_exists filename_out then begin
        if Sys.file_exists filename_exp then begin
          if do_is_ko (sprintf "./tests/diff.sh %s %s > /dev/null" filename_out filename_exp) then begin
            printf "ERROR: unexpected output for %s\n" test_prefix;
            meld_args := sprintf "--diff %s %s" filename_out filename_exp :: !meld_args;
            ignore_args := sprintf "echo \"%s.ml\" >> %s/ignored.tests && sed -i '\\,%s.ml,d' errors.tests" (Filename.basename test_prefix) (Filename.dirname test_prefix) (test_prefix) :: !ignore_args;
            error_tests := sprintf "%s.ml" test_prefix :: !error_tests;
            incr ko_count;
          end else
            incr ok_count;
        end else begin
            printf "MISSING: no expected output for %s\n   cp %s %s; git add %s\n" test_prefix filename_out filename_exp filename_exp;
            incr ko_count;
        end
      end else begin
        (* NOTE: we assume that only a script exception can
           lead to the absence of a _out.cpp;
           this error is already displayed *)
        (* printf "No output %s\n" test_prefix;*)
        incr ko_count;
      end
    in
    List.iter check_output tests_to_process;

  begin match !meld_args with
  | [] -> ()
  | args -> printf "%s" (Tools.list_to_string ~sep:"" ~bounds:["  meld "; "\n"] args)
  end;
  begin match !ignore_args with
  | [] -> ()
  | args -> printf "%s" (Tools.list_to_string ~sep:"\n  " ~bounds:["  "; "\n"] args)
  end;
  Xfile.put_lines "errors.tests" !error_tests;

  printf "%i tests passed, %i tests failed, %i tests ignored\n" !ok_count !ko_count (List.length ignored_tests);
  (*
     Produire une liste de (testname, result)

     type result =
       | Result_failure of string * string    --> short and full descr
       | Result_match
       | Result_mismatch

     if only one failure, print full descr for this one;
     in more, generate a file with all full_descr

     print all succeeded first
       print all mismatch (short failure descr => one per line)
       print all failed execs
     at last, print a summary: ALL SUCCEED, NB OF EXEC/DIFF FAILED, NB OF SKIPPED

     generate a file "failed.txt" with the list of tests that have Result_mismatch,
     for use with bash script for accepting changes

     LATER: generate a file "report.txt" or both with the list of tests that have succeeded

     *)

     (* NOTE:

Xfile.
let serialize_to_file (filename : string) (obj : 'a) : unit =
let unserialize_from_file (filename : string) : 'a =
let is_newer_than (filename1 : string) (filename2 : string) : bool =

need to compare dependency on
- optitrust/ast.cmxa installed needs to be more recent than serialized files
*)

(* Level 2 :
   if optitrust lib
     AND test.cpp
     AND test_out.cpp
     AND test.ml
     have not changed since production of report.ser
   then skip this test.
*)

(* --- accept.sh

  for each test in failed.txt
    meld test_out.cpp test_exp.cpp
    if diff test_out.cpp test_exp.cpp remains nonempty
    echo "accept change ? Y / N / A"
    x = input
    si Y, faire un cp
    si N, continue
    si A, exit

*)

(* --makefile:
    make tester   pour compiler tester.ml
*)
