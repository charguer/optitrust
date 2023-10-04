open Optitrust
open Printf

module StringSet = Set.Make(String)

(**

  This file is the source for the program `./tester.exe` used for executing unit tests.
  This program is meant to be called by the bash script `./tester`.
  It expects as arguments:

  1. the relative path from the optitrust root to the folder from which the user invoked the tester
  2. the name of an action to perform, e.g. 'run' (see further for other actions)
  3. other arguments, e.g. a list of test names, of test folder names.

  Usage: `tester [folder] [action] [options] [arg1] .. [argN]`.

  Options
  =======
  "-dry": only display list of tests to process
  "-hide-stdout": hide stdout contents produced by tests
  "-dump-trace": generate an html pages describing every steps performed by each test
  "-with-ignored": do not take into consideration the `ignored.tests`
  "-only-ignored": filter-out files that do not appear in an `ignored.tests` file

  Action 'run'
  ==============
  Each [argi] must correspond to one of:
  - the full path to a `.ml` file, e.g., 'tests/loop/loop_fusion.ml'
  - the basename of a `.ml` file, e.g., 'loop_fusion.ml'
  - the basename without extension, e.g. 'loop_fusion'
  - the name of a folder appearing in 'tests/' or 'case_studies/', possibly in depth, e.g. 'loop'
  - a substring of a test name, e.g. 'fus'.

  If no [argi] is provided, then:
  - if [folder] is "." (the root of optitrust), then `tests case_studies` are used
    as the two [argi] arguments.
  - otherwise, [folder] is used as an [argi] argument.

  The tester program produces as output information on which test fails.
  It generates a number of `*.tests` files:
  - `ignored.tests`: tests that have been ignored
  - `failed.tests`: tests whose execution raised an error
  - `missing_exp.tests`: tests not accompanied with a `_exp.cpp` file
  - `wrong.tests`: tests whose `_out.cpp` does not match the `_exp.cpp`.
  These files are exploited by other actions meant for handling unsuccessful tests.

  The tester program ignores a `.ml` test file if its name appears in a file
  named `ignored.tests` and appearing in one of the folders containing that test file.

  Action 'create'
  ==============
  Each [argi] must be a path to a test to create, e.g. `tests/loop/loop_fusion.ml`.
  The `.ml` extension is optional.
  The tester program creates in that folder `loop_fusion.ml`, `loop_fusion.cpp`
  as well as `loop_fusion_doc.ml`, `loop_fusion_doc.cpp`.

  Action 'addexp'
  ==============
  If no [argi] is provided, the list of tests found in `missing_exp.tests` is used.
  For each test `foo` considered, the tester program checks that `foo_exp.cpp` does
  not yet exist, then copies the `foo_out.cpp` test to a `foo_exp.cpp`.

  Action 'fixexp'
  ==============
  If no [argi] is provided, the list of tests found in `wrong.tests` is used.
  For each test `foo` considered, the tester program copies checks that `foo_exp.cpp`
  exists, then copies the `foo_out.cpp` test to a `foo_exp.cpp`.

  Action 'ignore'
  ==============
  If no [argi] is provided, the list of tests found in `failed.tests` and
  `wrong.tests` is used.
  For each test `foo` considered, the tester program adds the relative path to
  `foo.ml` as a line at the end of the `ignore.tests` located at the OptiTrust root.
  The purpose of this root `ignore.tests` is to act as a short-term buffer of broken
  tests, unlike the `ignore.tests` located in depth in the folders, which are meant
  list of tests that are meant to remain broken for a while.

  Action 'code'
  ==============
  If no [argi] is provided, the list of tests found in `failed.tests` and
  `wrong.tests` is used.
  For each test `foo` considered, the files `foo.ml` and `foo.cpp` are opened in VSCode.

  Action 'diff'
  ==============
  If no [argi] is provided, the list of tests found in `wrong.tests` is used.
  For each test `foo` considered, the command `code -d foo_out.cpp foo_exp.cpp`
  is executed.

  Action 'meld'
  ==============
  If no [argi] is provided, the list of tests found in `wrong.tests` is used.
  For each test `foo` considered, the command `meld foo_out.cpp foo_exp.cpp`
  is executed. Technically, a single call to `meld` is performed, opening
  all the pairs of files at once.


  TODO: action 'sort':
  interactively navigate through the list of unsuccessful tests,
  (wrong.tests + failed.tests + missing_exp)
  and for each test ask an input key to decide wether to
  open in code, put in ignore, open diff, open meld, addexp, fixexp.
*)

(*****************************************************************************)
(** Tools (LATER: move to tools/*.ml) *)

let ref_list_add (r:'a ref) (x:'a) : unit :=
  r := x :: !r

let (~~) iter l f =
  iter f l

let do_is_ko (cmd : string) : bool =
  let exit_code = Sys.command cmd in
  exit_code != 0

let _do_is_ok (cmd : string) : bool =
  not (do_is_ko cmd)

let do_or_die (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

let do_or_die_with_print (cmd : string) : unit =
  printf "%s\n" cmd;
  do_or_die cmd

  (* LATER: rediriiger des erreurs dans un fichier  2>&
    Sys.command en version booléenne
    ERRLINE = cat errorlog | head -n 1 | awk '{print $2}'
     ou grep ", line "
    head -n ${ERRLINE} batch.ml | grep "batching" | tail -1
     *)

(*****************************************************************************)
(** Options *)

(* Folders where tests might be located *)
let tests_folders = ["tests"; "case_studies"]

(* Folder from which the user invoked 'tester' *)
let caller_folder : string = ref ""

(* Action requested, e.g. 'run' *)
let action : string = ref ""

(* List of the [argi] arguments provided. *)
let args : string list ref = ref []

(* Flag to enable verbose mode *)
let verbose_mode : bool ref = ref false

(* Flag to enable dry-run mode *)
let dry_run : bool ref = ref false

(* Flag to include tests appearing in ignored.tests files *)
let flags_with_ignored : bool ref = ref false

(* Flag to filter-out tests that do not appear in ignored.tests files *)
let flags_only_ignored : bool ref = ref false

(* Automatically git add new files *)
let git_add_new_files : bool ref = ref false


(*****************************************************************************)
(** FUTURE OPTIONS *) (* use of cache, and controlling output file generation *)

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

let set_outfile_gen str =
  outfile_gen := string_to_outfile_gen str

(* Flag to ignore all cached data *)
let ignore_cache : bool ref = ref false

(* Flag to discard all cached data *)
let discard_cache : bool ref = ref false

(* Flag to control at which level the comparison is performed (AST or text).
   If Comparison_method_text, then implies Outfile_gen_always. *)
type comparison_method =
| Comparison_method_ast (* TODO: do we have this? *)
| Comparison_method_text

let comparison_method : comparison_method ref = ref Comparison_method_text

let _remove_later = comparison_method := Comparison_method_ast;
  comparison_method := Comparison_method_text

  (* Flag Comparison_method_text implies generation of output file
  if !comparison_method = Comparison_method_text
    then outfile_gen := Outfile_gen_always;
  *)

(*****************************************************************************)
(** Parsing of options *)

(* [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-dry", Arg.Set dry_run, " only display the list of tests to process");
     ("-hide-stdout", Arg.Set Flags.hide_stdout, " hide the contents that tests print on standard output ");
     ("-dump-trace", Arg.Set Flags.dump_trace, " generate html pages containing a full trace of the steps performed by every test  ");
     ("-with-ignored", Arg.Set flags_with_ignored, " do not take into consideration the `ignore.tests` files  ");
     ("-only-ignored", Arg.Set flags_only_ignored, " filter-out files that do not appear in an `ignore.tests` file  ");
     ("-gitadd", Arg.Set git_add_new_files, " automatically call git add on files generated by `create` and `addexp`.");
     ("-v", Arg.Set verbose_mode, " report details on the testing process.");
     (* NOT YET IMPLEMENTED *)
     ("-out", Arg.String set_outfile_gen, " generate output file: 'always', or 'never', or 'onfailure' (default)");
     ("-ignore-cache", Arg.Set ignore_cache, " ignore the serialized AST, force reparse of source files; does not modify the existing serialized data");
     ("-discard-cache", Arg.Set discard_cache, " clear all serialized AST; save serizalize data for
     tests that are executed.");
  ]


(*****************************************************************************)
(** Processing of tests list *)

(* [run_action] is like [do_or_die], with an option to print the command,
   and only prints the command if [-dry] flag has been set *)
let run_action ?(print = false) (cmd : string) : unit =
  let pr () = printf "%s\n" cmd in
  if !dry_run then begin
    pr()
  end else begin
    if print then pr();
    do_or_die cmd
  end


(*****************************************************************************)
(** Processing of lists of tests provided as arguments *)

module File_set = Set.Make(String)

let file_set_ref_add r x =
  r := File_set.add x !r

let tmpfile = Filename.temp_file "command_output" ".txt"

(** Compute the list of all ignored tests *)
let find_all_tests_to_ignore () : File_set =
  let target = String.concat " " tests_folders in
  (* LATER: use a version of Sys.command that captures the output *)
  do_or_die (sprintf "find %s -name 'ignore.tests' > %s" target tmpfile);
  let ignore_tests_files = Xfile.get_lines_or_empty tmpfile in
  let result = ref File_set.empty in
  (* For each file named `ignore.tests` *)
  ~~ List.iter ignore_tests_files (fun ignore_tests_file ->
    let folder = Filename.dirname ignore_tests_file in
    let ignore_tests = Xfile.get_lines_or_empty ignore_tests_file in
    (* For each test listed in that `ignore.tests` *)
    ~~ List.iter ignore_tests (fun ignore_test ->
      let test = folder ^ "/" ^ ignore_test in
      file_set_ref_add result test));
  (* Return result *)
  !result

(** Resolve one [argi] to a list of tests (without filtering ignored tests) *)
let resolve_arg (arg : string) : string list =
  if Sys.file_exists arg && not (Sys.is_directory arg) then begin
     (* Case exactly a file name *)
     arg
  end else begin
    let folders_to_search_from, pattern_on_the_name : string list * string option =
      begin match resolve_arg_as_folder arg then
      | Some folder ->
          (* Case [arg] is exactly a directory name, possibly in depth *)
          [folder], None
      | None ->
          (* Else [arg] is treated as a substring of a test name
             (which must be a [*.ml] file and not a [_with_lines.ml] file) *)
          tests_folders, Some arg
      in
    let sfolders = String.concat " " folders_to_search_from in
    let sname =
      match pattern_on_the_name with
      | None -> ""
      | Some pat -> sprintf "-name '*%s*' -and -name '*.ml' -and -not -name '*_with_lines.ml'" pat
      in
    (* LATER: use a version of Sys.command that captures the output *)
    do_or_die (sprintf "find %s %s > %s" sfolders sname tmpfile);
    let tests = Xfile.get_lines_or_empty tmpfile in
    tests

(** Takes the [argi] arguments, and resolve them to tests, then filter out
   ignore tests based on the contents of all the `ignore.tests` files,
   each of which refer to tests with path relative to the location of the
   `ignore.tests` file that contains them. Variation applies if the flags
   [-only-ignored] or [-with-ignored] have been provided.
   Returns [tests_to_process, ignored_tests]. *)
let get_tests_and_ignored (args : string list) : (string list * string list) =
  let tests = File_set.of_list (List.concat_map resolve_arg args) in
  let ignores = find_all_tests_to_ignore () in
  let tests_not_ignored = File_set.diff tests ignores in
  let tests_and_ignored = File_set.inter tests ignores in
  let set_to_process, set_to_ignore =
    if !flags_with_ignored then
      (tests, [])
    else if !flags_only_ignored then
      (tests_and_ignored, tests_not_ignored)
    else
      (tests_not_ignored, tests_and_ignored) in
  File_set.to_list set_to_process, File_set.to_list set_to_ignore


(*****************************************************************************)
(** Action 'run' *)

let action_run (tests : string list) : unit =

  (* Compute tests to proces *)
  let (tests_to_process, tests_ignored) = get_tests_and_ignored tests in
  let nb_tests_to_process = List.length tests_to_process in
  let tests_to_process_string = String.concat " " tests_to_process in
  if !dry_run || !verbose_mode
   then eprintf "Tester considering: \n  %s\n"
        (String.concat "\n  " tests_to_process);
  if !dry_run then exit 0;
  if nb_tests_to_process = 0 then eprintf "Empty set of tests considered.";

  (* Enable backtrace display only when running an individual test *)
  if nb_tests_to_process > 1
    then Flags.print_backtrace_on_error := false;

  (* Generate a `batch.ml` program that contains the contatenation of the source
     code of every test considered *)
  (* LATER: could re-implement batch_tests.sh in OCaml *)
  do_or_die (sprintf "tests/batch_tests.sh %s > tests/batch/batch.ml" tests_to_process_string);

  (* Delete existing output files to avoid considering them in case an error occurs *)
  let delete_output test =
    let prefix = Filename.remove_extension test in
    let filename_out = sprintf "%s_out.cpp" prefix in
    ignore (do_is_ko (sprintf "rm -f %s > /dev/null" filename_out))
    (* LATER: remove without displaying error messages or missing files *)
  in
  List.iter delete_output tests_to_process;

  (* Compile the `batch.ml` file, using dune hacks *)
  do_or_die "cp tests/batch/dune_disabled tests/batch/dune";
  do_or_die "dune build tests/batch/batch.cmxs; rm tests/batch/dune";
  (* DEPRECATED printf "\n"; *)

  (* If -hide-stdout option is used, start redirecting stdout into
     "_tests_stdout.txt" during the execution of the unit tests *)
  let oldstdout = Unix.dup Unix.stdout in
  let newstdout = ref None in
  if !Flags.hide_stdout then begin
    let c = open_out "_tests_stdout.txt" in
    Unix.dup2 (Unix.descr_of_out_channel c) Unix.stdout;
    newstdout := Some c;
  end;
  let close_redirected_stdout () : unit =
    if !Flags.hide_stdout then begin
      flush stdout;
      let c = Option.get !newstdout in
      close_out c;
      Unix.dup2 oldstdout Unix.stdout;
    end in

  (* Execute the `batch.ml` program *)
  begin try
    Flags.program_name := "tester.ml";
    Dynlink.loadfile "tests/batch/batch.cmxs"
  with
    Dynlink.Error err -> begin
      close_redirected_stdout();
      let sbt = Printexc.get_backtrace() in
      Printf.eprintf "%s\n%s" (Dynlink.error_message err) sbt;
      exit 1
    end
  end;
  close_redirected_stdout();

  (* Analyse test results *)
  let tests_failed = ref [] in
  let tests_noexp = ref [] in
  let tests_wrong = ref [] in
  let tests_success = ref [] in
  ~~ List.iter tests_to_process (fun test ->
    let prefix = Filename.chop_extension test in
    let filename_out = sprintf "%s_out.cpp" prefix in
    let filename_exp = sprintf "%s_exp.cpp" prefix in
    if Sys.file_exists filename_out then begin
      if Sys.file_exists filename_exp then begin
        (* TODO: use -q in the diff? *)
        let mismatches_expected = do_is_ko (sprintf "./tests/diff.sh %s %s > /dev/null" filename_out filename_exp) in
        if mismatches_expected
          then ref_list_add tests_wrong test;
          else ref_list_add tests_success test;
      end else begin
          ref_list_add tests_noexp test;
      end
    end else begin
      (* Missing _out.cpp file means test has failed *)
      ref_list_add tests_failed test;
    end);

  (* Produce summary of errors *)
  if !tests_failed <> [] then
    printf "Failed tests:\n%s\n" (Tools.list_to_string ~sep:"\n  " ~bounds:["   "; "\n"] !tests_failed);
  if !tests_noexp <> [] then
    printf "Missing expected:\n%s\n" (Tools.list_to_string ~sep:"\n  " ~bounds:["   "; "\n"] !tests_noexp);
  if !tests_wrong <> [] then
    printf "Wrong tests:\n%s\n" (Tools.list_to_string ~sep:"\n  " ~bounds:["   "; "\n"] !tests_wrong);

  (* Produce .tests files *)
  Xfile.put_lines "ignored.tests" tests_ignored;
  Xfile.put_lines "failed.tests" !tests_failed;
  Xfile.put_lines "wrong.tests" !tests_wrong;
  Xfile.put_lines "missing_exp.tests" !tests_noexp;

  (* Produce general summary *)
  let print_count (name: string) (tests: string list): unit =
    let len = List.length tests in
    if len > 0 then printf "%i %s, " len name
  in
  print_count "failed" !tests_failed;
  print_count "missing exp" !tests_noexp;
  print_count "wrong" !tests_wrong;
  print_count "ignored" tests_ignored;
  print_count "success" !tests_success;
  printf "\n"


(*****************************************************************************)
(** Action 'create' *)

let action_create (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let extensions = [".ml", ".cpp", "_doc.ml", "_doc.cpp"] in
    let files = List.map (fun s -> prefix ^ s) extensions in
    printf "Creating %s.{ml,cpp,_doc.ml,_doc.cpp}\n" prefix;
    ~~ List.iter files (fun file ->
      run_action (sprintf "touch %s" file);
      if !git_add_new_files then ifnotdry_do_or_die (sprintf "git add %s" file);
      );
  )

(*****************************************************************************)
(** Action 'addexp' *)

let action_addexp (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let outfile = prefix ^ "_out.cpp" in
    let expfile = prefix ^ "_exp.cpp" in
    run_action ~print:true (sprintf "cp %s %s" outfile expfile);
    if !git_add_new_files then do_or_die (sprintf "git add %s" expfile);
  )


(*****************************************************************************)
(** Action 'fixexp' *)

let action_fixexp (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let outfile = prefix ^ "_out.cpp" in
    let expfile = prefix ^ "_exp.cpp" in
    if Sys.file_exists expfile
      then run_action ~print:true (sprintf "cp %s %s" outfile expfile)
      else eprintf "tester fixexp: missing file %s\n" expfile;
  )


(*****************************************************************************)
(** Action 'ignore' *)

let action_ignore (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let ignorefile = "./ignore_tests" in
    run_action (sprintf "echo '%s' >> %s" test ignorefile)
  )


(*****************************************************************************)
(** Action 'code' *)

let action_code (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let mlfile = prefix ^ ".ml" in
    let cppfile = prefix ^ ".cpp" in
    run_action (sprintf "code %s %s" mlfile cppfile);
  )


(*****************************************************************************)
(** Action 'diff' *)

let action_diff (tests : string list) : unit =
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let outfile = prefix ^ "_out.cpp" in
    let expfile = prefix ^ "_exp.cpp" in
    run_action (sprintf "code -d %s %s" outfile expfile);
  )

(*****************************************************************************)
(** Action 'meld' *)

let action_meld (tests : string list) : unit =
  let meldargs = ref [] in
  ~~ List.iter tests (fun test ->
    let prefix = Filename.remove_extension test in
    let outfile = prefix ^ "_out.cpp" in
    let expfile = prefix ^ "_exp.cpp" in
    ref_list_add meldargs (sprintf "--diff %s %s " outfile expfile);
  );
  run_action (sprintf "meld %s" (String.concat "" !meldargs));


(*****************************************************************************)
(** Main *)

let _main : unit =
  (* Parsing of command line *)
  Arg.parse
    (Arg.align (spec @ Flags.spec))
    (fun arg ->
       if !caller_folder = ""
        then caller_folder := arg
      else if !action = ""
        then action := arg
      else
        ref_list_add args arg)

  (* Check caller_folder has been provided *)
  if !caller_folder = ""
    then failwith "Invalid usage: a folder must be provided as first argument.";

  (* Check action has been provided *)
  if !action = ""
    then failwith "Invalid usage: an action must be provided as second argument.";

  (* Handle the case of an empty list of [argi], depending on [action] *)
  if !args = [] then begin
    if !action = "run" then begin
        if !caller_folder = "."
          then args := tests_folders
          else args := !caller_folder
      end;
    end else if List.mem !action ["ignore"; "code"] then begin
      args := tests_from_file "failed.tests"
            @ tests_from_file "wrong.tests";
    end else if List.mem !action ["ignore"; "code"] then begin
      args := tests_from_file "wrong.tests";
    end
  end;

  (* Switch according to action *)
  let args := !args in
  match !action with
  | "run" -> action_run args
  | "create" -> action_create args
  | "addexp" -> action_addexp args
  | "fixexp" -> action_fixexp args
  | "ignore" -> action_ignore args
  | "code" -> action_code args
  | "diff" -> action_diff args
  | "meld" -> action_meld args
  | x -> failwith (sprintf "Invalid usage: unknown action %s" x)


(*****************************************************************************)
(* DEPREACTED/FUTURE *)
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

(* -
NOT YET IMPLEMENTED
- caching of AST representation for input and expected output files
- management of dependencies on the source code of optitrust
- selection of a subset of tests to process (${args} is by default "all")
  (either via filenames, or via a 'key' name refering to a groupe of files)
- comparison either at the AST level or at the cpp source level via diff
----------
*)


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

(* c'est le code de batch.ml
     qui fait la gestion des cached_inputs/cached_outputs

     et qui pourrait faire la comparaison au niveau AST

     ./batch_controller.ml prendrait en argument presque tous les arguments de tester.ml

     tester.ml would do only the computation of the list of tests
     and the generation of batch.ml and the launching of batcher.exe

  *)


(*****************************************************************************)
(** Processing of tests list DEPRECATED *)
(*
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
let get_tests_and_ignored (targets: string list): (string list * string list) =
  let test_files, ignored_file_set = resolve_test_targets targets in
  let test_files = Xlist.remove_duplicates test_files in
  (* Tests that were otherwise selected are not ignored *)
  let ignored_file_set =
    List.fold_left (fun acc t -> File_set.remove t acc) ignored_file_set test_files
  in
  let _check_all_files_exist =
    List.iter (fun test_file ->
      if not (Sys.file_exists test_file)
        then failwith (sprintf "File not found:"))
      test_files;
    in
  (test_files, File_set.elements ignored_file_set)
*)


(*

      RES=$(find tests -name "*${arg}*.ml" -and -not -name "*_with_lines.ml")
*)
