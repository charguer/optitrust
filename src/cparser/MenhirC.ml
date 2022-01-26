
open Driveraux

(* Configuration of the parser for OptiTrust's needs *)

let _ =
  Printexc.record_backtrace true;
  Machine.config := Machine.x86_64;
  Elab.generate_static_func_names := false;
  Elab.generate_implicit_return_on_main := false;
  Elab.allow_variables_as_array_size := true;
  Elab.allow_compound_initializer_in_return := true;
  Elab.keep_for_loops_untransformed := true


(* Preprocessor *)

let debug_preprocessor = true

let compcert_include_path =
  try Sys.getenv("OPTITRUST") ^ "src/cparser/include";
  with Not_found ->
    let p = "/usr/local/lib/compcert" in
    if not (Sys.file_exists p)
      then failwith "Please either install src/cparser/include into /usr/local/lib/compcert, or export the environment variable OPTITRUST";
    p

let preprocessor_command ifile =
  [ "gcc";
    "-xc";
    "-m64";
    "-U__GNUC__";
    "-U__SIZEOF_INT128__";
    "-E"; (* -std=c99 *)
    "-U__STDC_IEC_559_COMPLEX__";
    "-D__STDC_NO_ATOMICS__";
    "-D__STDC_NO_COMPLEX__";
    "-D__STDC_NO_THREADS__";
    "-D__STDC_NO_VLA__";
    "-I" ^ compcert_include_path;
    ifile
  ]

let preprocess ifile ofile =
  let output = Some ofile in
  let cmd = preprocessor_command ifile in
  if false && debug_preprocessor
    then Printf.eprintf "preprocessor command: %s\n" (String.concat " " cmd);
  let exc = command ?stdout:output cmd in
  if exc <> 0 then begin
    safe_remove ofile;
    command_error "preprocessor" exc;
  end

(* Input an AST *)

let read_file sourcefile = (* also in xfile.ml, as [get_contents] *)
  let ic = open_in_bin sourcefile in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic;
  text

let parse_string name text =
  let log_fuel = Camlcoq.Nat.of_int 50 in
  match Parser.translation_unit_file log_fuel (Lexer.tokens_stream name text)
  with
  | Parser.MenhirLibParser.Inter.Parsed_pr (ast, _ ) ->
      (ast: Cabs.definition list)
  | _ -> (* Fail_pr or Fail_pr_full or Timeout_pr, depending
            on the version of Menhir.
            Fail_pr{,_full} means that there's an inconsistency
            between the pre-parser and the parser.
            Timeout_pr means that we ran for 2^50 steps. *)
      Diagnostics.fatal_error Diagnostics.no_loc "internal error while parsing"

let parse_preprocessed_c_file sourcename sourcefile =
  Debug.init_compile_unit sourcename;
  Sections.initialize();
  (* CPragmas.reset(); *)
  Diagnostics.reset();
  let check_errors x =
    Diagnostics.check_errors(); x in
  read_file sourcefile
  |> Timing.time2 "Parsing" parse_string sourcename
  |> Timing.time "Elaboration" Elab.elab_file
  |> check_errors

let parse_c_file sourcename =
  ensure_inputfile_exists sourcename;
  let preproname =
    if debug_preprocessor
      then output_filename sourcename (Filename.extension sourcename) ".i"
      else tmp_file ".i"
    in
  preprocess sourcename preproname;
  parse_preprocessed_c_file sourcename preproname

(* Filter AST global entries that originate in a specific file *)

let filter_origin sourcename ast =
  let select_globdecl g =
    fst g.C.gloc = sourcename in
  List.filter select_globdecl ast

(* Output an AST *)

let print_ast pp ast =
  Cprint.program pp ast

let print_ast_to_stdout ast =
  let pp = Format.std_formatter in
  print_ast pp ast;
  flush stdout

let print_ast_to_file f ast =
  let oc = open_out f in
  print_ast (Format.formatter_of_out_channel oc) ast;
  close_out oc

let get_parsed_ast_filename sourcename =
    Filename.remove_extension sourcename
  ^ "_parsed"
  ^ Filename.extension sourcename

