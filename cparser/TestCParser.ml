
let read_file sourcefile =
  let ic = open_in_bin sourcefile in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic;
  text

let parse_string name text =
  let log_fuel = Camlcoq.Nat.of_int 50 in
  match
    Parser.translation_unit_file log_fuel (Lexer.tokens_stream name text)
  with
  | Parser.MenhirLibParser.Inter.Parsed_pr (ast, _ ) ->
      (ast: Cabs.definition list)
  | _ -> (* Fail_pr or Fail_pr_full or Timeout_pr, depending
            on the version of Menhir.
            Fail_pr{,_full} means that there's an inconsistency
            between the pre-parser and the parser.
            Timeout_pr means that we ran for 2^50 steps. *)
      Diagnostics.fatal_error Diagnostics.no_loc "internal error while parsing"

(* TEMP
let get_ast sourcefile =
  let name = Filename.basename sourcefile in
  Diagnostics.reset();
  let check_errors x =
    Diagnostics.check_errors(); x in
  (* Reading the whole file at once may seem costly, but seems to be
     the simplest / most robust way of accessing the text underlying
     a range of positions. This is used when printing an error message.
     Plus, I note that reading the whole file into memory leads to a
     speed increase: "make -C test" speeds up by 3 seconds out of 40
     on my machine. *)
  read_file sourcefile
  |> Timing.time2 "Parsing" parse_string name
  |> Timing.time "Elaboration" Elab.elab_file
  |> check_errors
*)

let print_ast ast =
  let pp = Format.std_formatter in
  Cprint.program pp ast;
  flush stdout


let () = (* subset of Driver.ml setup operations*)
  Gc.set { (Gc.get()) with
              Gc.minor_heap_size = 524288; (* 512k *)
              Gc.major_heap_increment = 4194304 (* 4M *)
         };
  Printexc.record_backtrace true;
  Machine.config := Machine.x86_64

  (*   Frontend.init ()* *)
  (*  Driver.parse_cmdline cmdline_actions
       *)


(* From preprocessed C to C AST *)

let myparse_c_file sourcename ifile =
  Debug.init_compile_unit sourcename;
  Sections.initialize();
  (* CPragmas.reset(); *)
  (* Simplification options *)
  let simplifs = "" (*
    "b" (* blocks: mandatory *)
  ^ (if !option_fstruct_passing then "s" else "")
  ^ (if !option_fpacked_structs then "p" else "") *)
  in
  (* Parsing and production of a simplified C AST *)
  let ast = Parse.preprocessed_file simplifs sourcename ifile in
  (* Save C AST if requested *)
  Cprint.print_if ast;
  ast


(* Take filename of the cpp file as argument, else use a default name *)
let sourcename =
  if Array.length Sys.argv > 2
    then Sys.argv.(1)
    else "TestCParser.cpp"

let _ =
  let ifile = sourcename in
  (* TEMP get_ast sourcefile *)

  (* Parse the ast *)
  Cprint.destination := Some "TestCParser.parsed.c";
  let _csyntax = myparse_c_file sourcename ifile in
  (* Print the ast
  print_ast csyntax *)
  ()




(* Interpret the code
Machine.config := Machine.compcert_interpreter !Machine.config;
Interp.execute csyntax;
*)


