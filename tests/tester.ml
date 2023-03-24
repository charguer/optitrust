(**

  This file tester.ml is the source for the program ./tester.exe, for executing unit tests.
  Assume for simplicity that ./tester.exe is executed from the current folder.

  It features:
  - caching of AST representation for input and expected output files
  - management of dependencies on the source code of optitrust
  - selection of a subset of tests to process (by default "all")
    (either via filenames, or via a 'key' name refering to a groupe of files)
  - comparison either at the AST level or at the cpp source level via diff

  It produces as output:
  - information on which test fails
  - if requested or else only for tests that fail, produce the output files *_out.cpp
  - generates a bash script to be used for approving changes on expected files
    in case of test failures that result from intended changes.


LATER: add options to control whether to generate for each test
(1) the JS trace, and (2) the JS documentation snippet

VERY-LATER: mode for compiling sources with gcc at a given standard

*)


(*****************************************************************************)
(* Description of keys *)

(* Gather the list of *.ml files in a directory *)
let get_list_of_tests_in_dir (folder : string) : string list =
 (* TODO  ignore (Sys.command ("mkdir -p " ^ foldername)); *)
 (* rediriger la sortie du ls dand un fichier
   éventuellement  Xfiles.get_lines file
   sinon trouver la fonction caml qui va bien Unix.execv peut être *)
 []

(* Takes the list of target arguments on the command line;
   and expand the 'keys' and remove duplicates. *)

let compute_tests_to_process (tests : string list) : string list =
  tests (* TODO , use Xlist.remove_duplicates*)
  (* example:
      if key is folder name of /tests
      "combi" => get_list_of_tests_in_dir "combi"
      "mygroup" =>  [... ]

      "pic"  => specific case study
      "mm"
      "studies" => all case studies
    *)



(*****************************************************************************)
(* Options *)

(* List of paths to tests to process;
   This list at the time of parsing the command line may include 'keys'.
   e.g. ["basic/variable_inline.ml"; "combi"].
   The list gets later expanded into a list of filenames only, without duplicates. *)
let tests_to_process : string list ref = ref ["all"]

(* Flag for controlling whether or not to generate *_out.cpp files. *)
type outfile_gen =
  | Outfile_gen_always
  | Outfile_gen_only_on_failure (* failure or missing expected file *)
  | Outfile_gen_never

let outfile_gen : outfile ref = ref Outfile_gen_only_on_failure

let set_outfile_gen = function
  | "always" -> Outfile_gen_always
  | "never" -> Outfile_gen_never
  | "onfailure" -> Outfile_gen_only_on_failure
  | _ -> failwith "Invalid argument for -out"

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


(*****************************************************************************)
(* Parsing of options *)

(* [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-out", Arg.String set_outfile_gen, " generate output file: 'always', or 'never', or 'onfailure' (default)");
     ("-ignore-cache", Arg.Set ignore_cache, " ignore the serialized AST, force reparse of source files; does not modify the existing serialized data");
     ("-discard-cache", Arg.Set discard_cache, " clear all serialized AST; save serizalize data for tests that are executed.");
     (* ("-v", Arg.Set verbose_mode, " enable verbose regarding files processed out produced (not fully implemented yet)."); *)
  ]


compute_tests_to_process

let _main : unit =
  Arg.parse
    (Arg.align spec)
    (fun other_arg -> tests_to_process := other_arg :: !tests_to_process)
    ("usage: ./tester.exe [options] target1 .. targetN");
  tests_to_process := List.rev !tests_to_process;

  if !comparison_method = Comparison_method_text
    then outfile_gen := Outfile_gen_always;

  let tests = compute_tests_to_process !tests_to_process in
  (* We cache the "raw ast".
  from a trm t, we need to serialize Ast_fromto_AstC.cfeatures_intro t;
  this is what is provided in trace.ml to  the function
   AstC_to_c.ast_to_outchannel ~beautify_mindex ~comment_pragma:use_clang_format out_prog t_after_cfeatures_intro;
   *)
  let cached_inputs = (* load the serialized file of ast (without encoding) *) in
  let select_inputs = (* parmis les tests requested,
    either we have it in cached_inputs and the date is older than *.cpp file,
    or we have to parse the cpp file *)
  let cached_expoutputs = (* load the serialized file of ast (without encoding) *) in
  let all_expoutputs = (* for each tests, either the output is up to date and in
    cached_outputs, or we need to parse it *)

  (* Need to save the cached_inputs and cached_expoutputs :
     -> we want save the ones that were serialized before,
        and update the ones that have just been reparsed *)

  (* Call batch_tests.sh test1 ... testN to generate  batch.ml

     linker batch.ml avec batcher.ml  où batcher.ml serait celui
     qui fait la gestion des cached_inputs/cached_outputs

     tester.ml would do only the computation of the list of tests
     and the generation of batch.ml and the launching of batcher.exe

  *)

  (* For each test, execute on the input the test
     - selon Outfile_gen, genère ou pas le output file
     - selon le comparison_method, compare la sortie.
          pour le mode texte: ./diff.sh
          on peut supposer que on execute tester dans le dossier courant tests/

     Produire une liste de (testname, result)

     type result =
       | Result_failure of string * string    --> short and full descr
       | Result_match
       | Result_mismatch

     if only one failure, print full descr for this one;
     in more, generate a file with all full_descr

     at last, print a summary: ALL SUCCEED, LIST OF FAILED
       print short failure descr => one per line

     generate a file "failed.txt" with the list of tests that have Result_mismatch,
     for use with bash script for accepting changes

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
