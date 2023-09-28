open Prelude

(* TODO: Get rid of this aweful C(++) only include handling *)
(* [get_cpp_includes filename]: gets the list of file includes syntactically visible
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

let clang_raw_parser (filename: string): trm =
  let command_line_include =
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ()) in
  let command_line_warnings = ["-Wno-parentheses-equality"; "-Wno-c++11-extensions"] in
  let command_line_args = command_line_warnings @ command_line_include in
  Clang_to_astRawC.tr_ast (Clang.Ast.parse_file ~command_line_args filename)

let menhir_raw_parser (filename: string): trm =
  CMenhir_to_astRawC.tr_ast (Compcert_parser.MenhirC.parse_c_file_without_includes filename)

(* use all C parsers, and ensure consistency of the results *)
let all_c_raw_parsers (filename: string): trm =
  let rawAstClang = clang_raw_parser filename in
  let rawAtMenhir = menhir_raw_parser filename in
  let strAstClang = AstC_to_c.ast_to_string rawAstClang in
  let strAstMenhir = AstC_to_c.ast_to_string rawAtMenhir in
  if strAstClang <> strAstMenhir then begin
    (* LATER: we could add a prefix based on the filename, but this is only for debug *)
    Xfile.put_contents "ast_clang.cpp" strAstClang;
    Xfile.put_contents "ast_menhir.cpp" strAstMenhir;
  fail None "Trace.parse: [-cparser all] option detected discrepencies;\n meld ast_clang.cpp ast_menhir.cpp";
  end else
  (* If the two ast match, we can use any one of them (only locations might differ); let's use the one from the default parser. *)
  rawAstClang

let c_parser (raw_parser: string -> trm) (filename: string) =
  let includes = get_c_includes filename in
  let rawAst = raw_parser filename in
  if !Flags.bypass_cfeatures
    then (includes, rawAst)
    else (includes, Ast_fromto_AstC.cfeatures_elim rawAst)

let clang = c_parser clang_raw_parser
let menhir = c_parser menhir_raw_parser
let all = c_parser all_c_raw_parsers

let get_default () =
  match !Flags.c_parser_name with
  | "default" | "clang" -> clang
  | "menhir" -> menhir
  | "all" -> all
  | _ -> failwith "the available cparser options are 'default', 'clang', 'menhir' and 'all'"
