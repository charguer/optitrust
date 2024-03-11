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
   this requires obtaining the path to this files, somehow.
   We should instead add a checksum of ast.ml with a ppx, and check against it here. *)
let c_parser ~(serialize:bool) (raw_parser: string -> trm) (filename: string) : string * trm =
  (* "ser" means serialized *)
  let ser_filename = filename ^ ".ser" in
  (* Load existing serialized file, if any *)
  let existing_ser_contents_opt =
    if not serialize
      || !Flags.ignore_serialized
      || not (Sys.file_exists ser_filename)
      || not (Xfile.is_newer_than ser_filename filename) then
      None
    else begin
      if !Flags.debug_parsing_serialization
        then Tools.info (sprintf "unserializing ast: %s." ser_filename);
      try
        let ser_file = open_in_bin ser_filename in
        let deps = Marshal.from_channel ser_file in
        if List.for_all (Xfile.is_newer_than ser_filename) deps then
          let header, ast = Marshal.from_channel ser_file in
          begin try
            let ast = Scope_computation.infer_var_ids ast in
            Some (deps,header,ast)
          with _ ->
            Tools.info (sprintf "failure in infer_var_ids on unserialized ast for %s, reparsing." ser_filename);
            None
          end
        else begin
          Tools.info (sprintf "serialized ast %s is outdated" ser_filename);
          None
        end
      with _ ->
        Tools.info (sprintf "failure unserializing ast from %s, will reparse." ser_filename);
        None
    end
    in
  (* Parse, if not using serialized contents *)
  let deps, header, ast =
    match existing_ser_contents_opt with
    | Some header_and_ast -> header_and_ast
    | None ->
        if !Flags.debug_parsing_serialization then
          Tools.info (sprintf "parsing ast: %s." filename);
        (* Parsing per se *)
        let header = get_c_includes filename in (* header contains include *)
        let ast = raw_parser filename in
        let toplevel_seq = trm_inv trm_seq_inv ast in
        let deps = Mlist.fold_left (fun deps instr ->
          match trm_include_inv instr with
          | Some filename -> filename :: deps
          | None -> deps
          ) [] toplevel_seq in
        if !Flags.debug_parsing_serialization then
          Tools.info (sprintf "ast depends on header files: %s." (String.concat ", " deps));
        deps, header, ast
    in
  (* Save to serialization file, if applicable *)
  if (not serialize || not !Flags.dont_serialize)
     && existing_ser_contents_opt = None then begin
    try
      let clean_ast = Trm.prepare_for_serialize ast in
      let out_file = open_out_bin ser_filename in
      Marshal.to_channel out_file deps [];
      Marshal.to_channel out_file (header, clean_ast) [];
      if !Flags.debug_parsing_serialization
        then Tools.info (sprintf "serialized ast: %s." ser_filename);
    with e ->
      Tools.warn (sprintf "failure serializing ast to %s, skipping serialization. Error: %s\n" ser_filename (Printexc.to_string e));
  end;
  (* Possibly ably the decoding *)
  let ast = if !Flags.bypass_cfeatures then ast else Ast_fromto_AstC.cfeatures_elim ast in
  (* Return the header and the ast *)
  (header, ast)


let clang ~(serialize:bool) =
  c_parser ~serialize clang_raw_parser
(*  FOR FUTURE USE
let menhir = c_parser menhir_raw_parser
let all = c_parser all_c_raw_parsers *)

let get_default ~(serialize:bool) () =
  match !Flags.c_parser_name with
  | "default" | "clang" -> clang ~serialize
  | _ -> failwith "the available cparser options are 'default', 'clang'"
  (* FOR FUTURE USE
  | "menhir" -> menhir
  | "all" -> all
  | _ -> failwith "the available cparser options are 'default', 'clang', 'menhir' and 'all'"*)
