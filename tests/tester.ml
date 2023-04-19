open Optitrust

module StringSet = Set.Make(String)

(**

  This file tester.ml is the source for the program ./tester.exe, for executing unit tests.
  Assume for simplicity that ./tester.exe is executed from the project root (containing dune-project), using, e.g:

  ./tester args

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

let tmp_file = "/tmp/optitrust_tester"

let run_command (cmd : string) : unit =
  let exit_code = Sys.command cmd in
  if exit_code != 0 then
    failwith (Printf.sprintf "command '%s' failed with exit code '%i'" cmd exit_code)

    (* command_output
      command_output_lines *)
(*****************************************************************************)
(* Description of keys *)

(* Gather the list of *.ml files in a directory.

   Ignores *_with_lines.ml files.
   *)
let get_list_of_tests_in_dir (folder : string) : string list =
  run_command (Printf.sprintf
    "find %s -name \"*.ml\" -and -not -name \"*_with_lines.ml\" > %s"
    folder tmp_file);
  Xfile.get_lines tmp_file

(* Gather the list of *.ml files for a given key. May contain duplicates. *)
let rec list_of_tests_from_key (key : string) : string list =
  let aux = list_of_tests_from_key in
  match key with
  | "all" -> (aux "basic") @
             (aux "combi") @
             (aux "target") @
             (aux "ast") @
             (aux "case_studies")
  (* TODO: factorize dir keywords? *)
  | "basic" -> get_list_of_tests_in_dir "tests/basic"
  | "combi" -> get_list_of_tests_in_dir "tests/combi"
  | "target" -> get_list_of_tests_in_dir "tests/target"
  | "ast" -> get_list_of_tests_in_dir "tests/ast"
  | "case_studies" -> (aux "mm") @
                      (aux "harris")
  (* TODO: pic *)
  | "mm" -> ["case_studies/matmul/matmul.ml"]
  (* TODO: box_blur *)
  | "harris" -> ["case_studies/harris/harris.ml"]
  | file -> [file]

  (* TODO: target "ignore" specially treated to ignore the ignore list
     *)

(* TODO: read from a file instead? *)
let basic_tests_to_ignore = [
  (* TO FIX: *)
  "function_uninline.ml";
  "record_method_to_const.ml";
	"function_rename_args.ml";
	"align_def.ml";
	"matrix_insert_access_dim_index.ml";
	"record_reorder_fields.ml";
  (* NO FIX: *)
	"aos_to_soa_typedef.ml";
	"aos_to_soa_sized_array.ml";
	"variable_change_type.ml";
  (* ??? *)
	"record_update_fields_type.ml";
  "record_modif.ml";
  "record_to_variables.ml";
  "variable_ref_to_var.ml";
]
let combi_tests_to_ignore = [
  (* TO FIX: *)
  "record_align_field.ml";
	"loop_hoist.ml";
	"loop_fission.ml";
  "loop_unfold_bound.ml";
	"record_align_field.ml";
	"apac_heapify_nested_seq.ml";
  (* NO FIX: *)
  "aos_to_soa.ml";
  (* ??? *)
  "swap_coords_fixed.ml";
  "swap_coords_vari.ml";
  "specialize_function_arg.ml";
]
let target_tests_to_ignore = [
  (* TO FIX: *)
  "target_one.ml";
  "target_type.ml";
  (* NOT A TEST: *)
	"target_regexp.ml";
	"target_debug.ml";
  (* ??? *)
  "target_accesses.ml";
  "target_get_set.ml";
]
let ast_tests_to_ignore = [
  (* TO FIX: *)
	"c_raw.ml";
	"c_serialize.ml";
]
let tests_to_ignore =
  (List.map (fun f -> "tests/basic/" ^ f) basic_tests_to_ignore) @
  (List.map (fun f -> "tests/combi/" ^ f) combi_tests_to_ignore) @
  (List.map (fun f -> "tests/target/" ^ f) target_tests_to_ignore) @
  (List.map (fun f -> "tests/ast/" ^ f) ast_tests_to_ignore)

(* Takes the list of target arguments on the command line;
   and expand the 'keys', remove duplicates and ignored tests.
*)
let compute_tests_to_process (keys : string list) : string list =
  let tests = List.concat_map list_of_tests_from_key keys in
  let unique_tests = Xlist.remove_duplicates tests in
  (* TODO: report ignored tests to user *)
  let filtered_tests = List.filter (fun x -> not (List.mem x tests_to_ignore)) unique_tests in
  (* TODO : garder quand meme les keys *)
  filtered_tests

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
(* Parsing of options *)

(* [cmdline_args]: a list of possible command line arguments. *)
type cmdline_args = (string * Arg.spec * string) list

(* [spec]: possible command line arguments. *)
let spec : cmdline_args =
   [ ("-out", Arg.String set_outfile_gen, " generate output file: 'always', or 'never', or 'onfailure' (default)");
     ("-ignore-cache", Arg.Set ignore_cache, " ignore the serialized AST, force reparse of source files; does not modify the existing serialized data");
     ("-discard-cache", Arg.Set discard_cache, " clear all serialized AST; save serizalize data for tests that are executed.");
     ("-v", Arg.Set verbose_mode, " report details on the testing process.");
  ]

let _main : unit =
  Arg.parse
    (Arg.align spec)
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
  let tests_to_process = compute_tests_to_process keys_to_process in



  (* TODO: not always necessary? *)
  run_command "dune install";

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
      then Printf.eprintf "Tester files processed: \n  %s\n"
        (String.concat "\n  " tests_to_process);
  (* TODO: faire la boucle en caml sur l'appel à sed,
   à chaque fois afficher un commentaire (* CURTEST=... *)
     TODO: add 'batch_prelude' and 'batch_postlude' calls.
     LATER: ajouter ici l'option ~expected_ast , et concatener l'appel à Run.batch_postlude logfilename *)
  run_command ("tests/batch_tests.sh " ^ batch_args ^ " > " ^ "tests/batch/batch.ml");
  run_command "dune build tests/batch/batch.cmxs";
  (* TODO: rediriiger l'erreur dans un fichier  2>&
    Sys.command en version booléenne
    ERRLINE = cat errorlog | head -n 1 | awk '{print $2}'
     ou grep ", line "
    head -n ${ERRLINE} batch.ml | grep "batching" | tail -1
     *)
  (* TODO: flags *)
  run_command "OCAMLRUNPARAM=b dune exec runner/optitrust_runner.exe _build/default/tests/batch/batch.cmxs";

  (* c'est le code de batch_controller.ml
     qui fait la gestion des cached_inputs/cached_outputs

     ./batch_controller.ml prendrait en argument presque tous les arguments de tester.ml

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
