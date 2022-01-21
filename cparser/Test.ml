
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
  Frontend.init ()
  (*  Driver.parse_cmdline cmdline_actions
      Cprint.destination := Some "out.parsed.c" *)



let _ =
  let sourcename = "Test.cpp" in
  let ifine = sourcename in
  (* TEMP  get_ast sourcefile *)


  (* Parse the ast *)
  let csyntax = Frontend.parse_c_file sourcename ifile in


(*
    Machine.config := Machine.compcert_interpreter !Machine.config;
    let csyntax = parse_c_file sourcename preproname in
    Interp.execute csyntax;
*)


let compile_c_file sourcename ifile ofile =
  (* Prepare to dump Clight, RTL, etc, if requested *)
  let set_dest dst opt ext =
    dst := if !opt then Some (output_filename sourcename ".c" ext)
      else None in
  set_dest Cprint.destination option_dparse ".parsed.c";
  set_dest PrintCsyntax.destination option_dcmedium ".compcert.c";
  set_dest PrintClight.destination option_dclight ".light.c";
  set_dest PrintCminor.destination option_dcminor ".cm";
  set_dest PrintRTL.destination option_drtl ".rtl";
  set_dest Regalloc.destination_alloctrace option_dalloctrace ".alloctrace";
  set_dest PrintLTL.destination option_dltl ".ltl";
  set_dest PrintMach.destination option_dmach ".mach";
  set_dest AsmToJSON.destination option_sdump !sdump_suffix;
  (* Parse the ast *)
  let csyntax = parse_c_file sourcename ifile in
