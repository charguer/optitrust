open Optitrust_utils
open Optitrust_ast
open Printf
open Ast
open Trm
open Flags

(* [light_verbose_info] documents calls to [verbose_info] that could be
   turned on without turning all other [verbose_info] calls on.
   Could be defined as [Tools.info] for debugging. *)
let light_verbose_info = verbose_info

(** [dune exec optitrust_parser foo.cpp] produces a file [foo.ser],
    that contains the AST of the file (type [trm] of [src/ast/ast.ml],
    serialized using OCaml's marshal.

    Build: dune build parser/parser.exe
    Test: dune exec optitrust_parser -- -v -f tests/ast/cpp_small.cpp
**)

(* TODO: Get rid of this aweful C(++) only include handling *)
(** [get_cpp_includes filename]: gets the list of file includes syntactically visible
   on the first lines of a CPP file -- this implementation is quite restrictive. *)
let get_c_includes (filename : string) : string =
  (* make sure the include list is clean *)
  let includes = ref "" in
  let c_in = open_in filename in
  try
    while (true) do
      let s = input_line c_in in
      (* FIXME: Probably broken: preprocessor directives can have spaces after the # *)
      if Str.string_match (Str.regexp "^#include") s 0 then
        includes := !includes ^ s ^ "\n\n";
    done;
    !includes
  with
  | End_of_file -> close_in c_in; !includes

let clangml_options : Clang.Ast.Options.t = {
  ignore_implicit_cast = false;
  ignore_paren = true;
  ignore_paren_in_types = true;
  ignore_expr_with_cleanups = true;
  ignore_materialize_temporary_expr = true;
  ignore_bind_temporary_expr = true;
  ignore_injected_class_names = true;
  ignore_implicit_constructors = true;
  ignore_implicit_methods = true;
  ignore_anonymous_fields = true;
  ignore_indirect_fields = true;
  ignore_using_types = true;
  convert_integer_literals = true;
  convert_floating_literals = true;
  init_list_form = Semantic;
}

let raw_parser (filename: string): trm =
  let command_line_include =
    List.map Clang.Command_line.include_directory
      (Filename.concat optitrust_root "include" :: Clang.default_include_directories ()) in
  let command_line_warnings = ["-Wno-parentheses-equality"; "-Wno-c++11-extensions"] in
  let precompiled_stdlib_filename = Filename.concat optitrust_root "include/precompiled_stdlib.pch" in
  let command_line_pch =
    if Sys.file_exists precompiled_stdlib_filename then
      ["-include-pch"; precompiled_stdlib_filename]
    else begin
      Tools.warn "Could not find the precompiled stdlib: parsing may be very slow; did you do 'make precompile' (%s)" precompiled_stdlib_filename;
      []
    end in
  let command_line_stdlib = ["-stdlib=libc++"] in (* force use of Clang's stdlib, not GCC's *)
  let command_line_args = command_line_warnings @ command_line_include @ command_line_pch @ command_line_stdlib in
  Clang_to_ast.tr_ast (Clang.Ast.parse_file ~command_line_args ~options:clangml_options filename)

let parse (filename: string) : unit =
  (* "ser" means serialized *)
  let ser_filename = filename ^ ".ser" in
  (* Load existing serialized file, if any *)
  let is_up_to_date =
    if !force then begin
      verbose_info "forced to refresh the serialization of %s" ser_filename;
      false
    end else if not (Sys.file_exists ser_filename) then begin
      light_verbose_info "no previously generated serialized ast %s" ser_filename;
      false
    end else if not (File.is_newer_than ser_filename program_path) then begin
      light_verbose_info "parser have changed since last serialization of %s" ser_filename;
      false
    end else if not (File.is_newer_than ser_filename filename) then begin
      light_verbose_info "serialized ast %s is outdated" ser_filename;
      false
    end else begin
      try
        let ser_file = open_in_bin ser_filename in
        let deps = Marshal.from_channel ser_file in
        if List.for_all (File.is_newer_than ser_filename) deps then begin
          verbose_info "serialized ast %s is up to date" ser_filename;
          true
        end else begin
          light_verbose_info "serialized ast %s is outdated because of header files" ser_filename;
          false
        end
      with _ ->
        Tools.warn "failure unserializing ast from %s, will reparse." ser_filename;
        false
    end
  in
  if not is_up_to_date then begin
    verbose_info "parsing ast: %s." filename;
    (* Parsing per se *)
    let header = get_c_includes filename in (* header contains include *)
    let ast = raw_parser filename in
    let toplevel_seq, _ = trm_inv trm_seq_inv ast in
    let deps = Mlist.fold_left (fun deps instr ->
      match trm_include_inv instr with
      | Some filename -> filename :: deps
      | None -> deps
      ) [] toplevel_seq in
    verbose_info "ast depends on header files: %s." (String.concat ", " deps);
    (* Save to serialization file, if applicable *)
    try
      let out_file = open_out_bin ser_filename in
      Marshal.to_channel out_file deps [];
      Marshal.to_channel out_file (header, ast) [];
      close_out out_file;
      verbose_info "Parser produced serialized file %s\n" ser_filename;
    with e ->
      Tools.error "failure serializing ast to %s. Error: %s" ser_filename (Printexc.to_string e);
  end

let _ =
  (* Parsing of command line *)
  let filenames = ref [] in
  Arg.parse
    (Arg.align spec)
    (fun arg -> filenames := arg :: !filenames)
    "Usage: ./optitrust_parser arg1.cpp .. argN.cpp\n";

  filenames := List.rev !filenames;
  List.iter parse !filenames
