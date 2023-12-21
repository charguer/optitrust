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
      (Filename.concat !Flags.optitrust_root "include" :: Clang.default_include_directories ()) in
  let command_line_warnings = ["-Wno-parentheses-equality"; "-Wno-c++11-extensions"] in
  let precompiled_stdlib_filename = Filename.concat !Flags.optitrust_root "include/precompiled_stdlib.pch" in
  let command_line_pch = if Sys.file_exists precompiled_stdlib_filename then
    ["-include-pch"; precompiled_stdlib_filename]
  else begin
    Tools.warn "Could not find the precompiled stdlib: parsing may be very slow; did you do 'make precompile'";
    []
  end in
  let command_line_args = command_line_warnings @ command_line_include @ command_line_pch in
  Clang_to_astRawC.tr_ast (Clang.Ast.parse_file ~command_line_args filename)

(* FOR FUTURE USE
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
  failwith "Trace.parse: [-cparser all] option detected discrepencies;\n meld ast_clang.cpp ast_menhir.cpp";
  end else
  (* If the two ast match, we can use any one of them (only locations might differ); let's use the one from the default parser. *)
  rawAstClang
*)



  (*  TODO: currently a 'make clean_ser' is needed when the ast.ml file changes;
     it would be better to compare against the timestamp of ast.ml, however
     this requires obtaining the path to this files, somehow. *)
let c_parser (raw_parser: string -> trm) (filename: string) : string * trm=
  (* "ser" means serialized *)
  let ser_filename = filename ^ ".ser" in
  (* Load existing serialized file, if any *)
  let existing_ser_contents_opt =
    if !Flags.ignore_serialized
       || not (Sys.file_exists ser_filename)
       || not (Xfile.is_newer_than ser_filename filename) then
       None
    else begin
      if debug_serialization then Tools.info (sprintf "loading ast from %s." ser_filename);
      try
        let header, ast = Xfile.unserialize_from ser_filename in
        begin try
          let ast = Scope_computation.infer_var_ids ast in
          Some (header,ast)
        with _ ->
          Tools.info (sprintf "failure in infer_var_ids on unserialized ast for %s, reparsing." ser_filename);
          None
        end
      with _ ->
        Tools.info (sprintf "failure reading ast from %s, reparsing." ser_filename);
        None
    end
    in
  (* Parse, if not using serialized contents *)
  let header, ast =
    match existing_ser_contents_opt with
    | Some header_and_ast -> header_and_ast
    | None ->
        if debug_serialization then Tools.info (sprintf "parsing ast from %s." filename);
        (* Parsing per ser *)
        let header = get_c_includes filename in (* header contains include *)
        let ast = raw_parser filename in
        header, ast
    in
  (* Save to serialization file, if applicable *)
  if not !Flags.dont_serialize
     && existing_ser_contents_opt = None then begin
    try
      let clean_ast = Trm.prepare_for_serialize ast in
      Xfile.serialize_to ser_filename (header, clean_ast);
      if debug_serialization then Tools.info (sprintf "saved ast to %s." filename);
    with e ->
      Tools.info (sprintf "failure saving ast to %s, skipping. Error: %s\n" ser_filename (Printexc.to_string e));
  end;
  (* Possibly ably the decoding *)
  let ast = if !Flags.bypass_cfeatures then ast else Ast_fromto_AstC.cfeatures_elim ast in
  (* Return the header and the ast *)
  (header, ast)


let clang = c_parser clang_raw_parser
(*  FOR FUTURE USE
let menhir = c_parser menhir_raw_parser
let all = c_parser all_c_raw_parsers *)

let get_default () =
  match !Flags.c_parser_name with
  | "default" | "clang" -> clang
  | _ -> failwith "the available cparser options are 'default', 'clang'"
  (* FOR FUTURE USE
  | "menhir" -> menhir
  | "all" -> all
  | _ -> failwith "the available cparser options are 'default', 'clang', 'menhir' and 'all'"*)
