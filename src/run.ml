
(******************************************************************************)
(*                                 Debug                                      *)
(******************************************************************************)
include Tools

(* [set_exn_backtrace b]: based on [b] enable or disable backtracing in case an exception was thrown *)
let set_exn_backtrace (b : bool) : unit =
  Printexc.record_backtrace b

(* by default, all exceptions are backtraced *)
let _ = set_exn_backtrace true


(******************************************************************************)
(*                               Perf                                         *)
(******************************************************************************)

(* Use higher default value for the GC parameters *)
let _ =
  Gc.set { (Gc.get()) with
              Gc.minor_heap_size = 524288; (* 512k *)
              Gc.major_heap_increment = 4194304 (* 4M *) }


(******************************************************************************)
(*                                 Run                                        *)
(******************************************************************************)

(* [add_cmdline_args]: registers additional command line flags; see Flags.ml *)
let process_cmdline_args (args : Flags.cmdline_args) : unit =
   Flags.process_cmdline_args ~args ()

(* [script f]: serves as "main" function for an Optitrust script. It cakes care of parsing
    the command line arguments and handling the errors, in addition to running the function [f] provided. *)
let script (f : unit -> unit) : unit =
  Flags.process_cmdline_args();
  try
    let stats0 = Stats.get_cur_stats () in
    f();
    let stats1 = Stats.get_cur_stats () in
    if !Flags.analyse_stats
      then
        let stats_str = Stats.stats_diff_str stats0 stats1 in
        Printf.printf "%s\n" stats_str;
  with | Failure s | Ast.TransfoError s ->
    Trace.finalize();
    (* failwith s *)
    let sbt = Printexc.get_backtrace() in
    Printf.eprintf "%s\n" sbt;
    Printf.eprintf "=======\nFailure: %s\n" s;
    exit 1

(* [generated_source_with_inlined_header_cpp input_file inline output_file]:
   takes a file [input_file] and produces a file [output_file] obtained by
   inlining in the source the "#include" corresponding to the files listed
   in the list of filenames [inline].
  If an [#include "bar.h"] has already been substituted once, then the other occurrences are ignored.
  If an item "bar.hc" (LATER or .hcpp) is provided in the list, then [#include "bar.h"] will be
  intrepreted as [#include "bar.h"; #include "bar.cpp"], and the substitutions will be
  performed as if the [~inline] argument contained ["bar.cpp"; "bar.h"].
  If an item "bar.h-" is provided, then [#include "bar.h"] will be deleted. *)

(* [debug_inline_cpp]: only for debugging purposes *)
let debug_inline_cpp = false

(* [generated_source_with_inlined_header_cpp input_file inline output_file]: takes the input file [input_file]
     and finds all the include directives, then its going to inline all those files whose name belongs to
     in [inline]  *)
let generated_source_with_inlined_header_cpp (input_file:string) (inline:string list) (output_file:string) : unit =
  let s = ref (Xfile.get_contents input_file) in
  let perform_inline finline =
      let include_instr = "#include \"" ^ finline ^ "\"" in
      if debug_inline_cpp then Printf.printf "Inlined %s\n" include_instr;
      let contents = Xfile.get_contents finline in
      s := Tools.string_subst_first include_instr contents !s;
      s := Tools.string_subst include_instr "" !s
    in
  let process_item finline =
    let basename = Filename.basename finline in
    let extension = Filename.extension finline in
    if extension = ".hc" then begin
      let corename = Filename.chop_extension basename in
      let fheader = corename ^ ".h" in
      let fimplem = corename ^ ".c" in
      let include_instr = "#include \"" ^ fheader ^ "\"" in
      let include_instr_new = "#include \"" ^ fheader ^ "\"\n#include \"" ^ fimplem ^ "\"" in
      if debug_inline_cpp then Printf.printf "Prepare inline of implementation for %s\n" include_instr;
      s := Tools.string_subst include_instr include_instr_new !s;
      perform_inline fimplem;
      perform_inline fheader;
    end else if extension = ".h-" then begin
      let corename = Filename.chop_extension basename in
      let fheader = corename ^ ".h" in
      let include_instr = "#include \"" ^ fheader ^ "\"" in
      s := Tools.string_subst include_instr "" !s;
    end else begin
      perform_inline finline
    end
  in
  List.iter process_item inline;
  Xfile.put_contents output_file !s

(* [get_program_basename ()]: returns the basename of the current binary program being used.
    It takes care to remove the leading './' and takes care to remove the "with_lines" suffix. *)
let get_program_basename () : string =
  Flags.process_program_name();
  let basename = Filename.chop_extension (!Flags.program_name) in
  (* remove the "_with_lines" suffix *)
  let suffix = "_with_lines" in
  let nsuffix = String.length suffix in
  let nbasename = String.length basename in
  let b =
    if nbasename >= nsuffix && (String.sub basename (nbasename - nsuffix) nsuffix) = suffix
      then String.sub basename 0 (nbasename - nsuffix)
      else basename
    in
  (* remove the leading './' *)
  if String.length b > 2 && b.[0] = '.' && b.[1] = '/'
    then String.sub b 2 (String.length b - 2)
    else b

(* [Stop]: exception for stopping the execution of the script *)
exception Stop

(* [stop ()]: raises exception Stop *)
let stop () : unit =
  raise Stop

(* [script_cpp ~batching ~filename ~prepro ~inline ~check_exit_at_end ~prefix ~parser f]:
     is a specialized version of [script f] that:
   - automatically invokes [Trace.init "foo.cpp"] at start, where "foo" is the basename of
     the current script named "foo.ml" (alternatively, this name can be specified using the ~filename argument).
   - automatically invokes [Trace.dump] at the end of the script;
     (the main output file is named "foo_out.cpp").
   - [~check_exit_at_end:false] is an option for deactivating the implicit call to [Trace.check_exit_and_step()]
    at the end of the execution of [f] (LATER: will be deprecated)
      This flag only has an effect if a [-exit_line] option was passed on the command line.
   - [~prefix:string] allows providing the basename for the output files produced
   - [~inline:["foo.cpp";"bar.h"]] allows to perform substitution of "#include" directives
     with the contents of the corresponding files; the substitutions are performed one after
     the other, meaning that "bar.h" will be inlined if included from "foo.cpp".
     See the specification of [generated_source_with_inlined_header_cpp] for additional features.
   - [~batching:filename] is a shorthand for [~filename:filename ~prefix:filename] and also it activates
     the printing of progress for batch mode; this is used by the "make batch" command for unit tests *)
let script_cpp ?(batching : string = "") ?(filename : string = "") ?(prepro : string list = []) ?(inline : string list = [])
  ?(check_exit_at_end : bool = true) ?(prefix : string = "") ?(parser : Parsers.cparser = Default) (f : unit -> unit) : unit =
    Clflags.prepro_options := prepro;
    let saved_parser = Parsers.get_selected() in
    Parsers.select_if_not_default parser;
    Target.show_next_id_reset();

    (* Handles batch mode *)
    let filename,prefix =
      if batching <> "" then begin
        Printf.printf "Batch test executing: %s\n" batching;
        let basename = Filename.chop_extension batching in
        let prefix = if prefix <> "" then prefix else basename in
        let filename = if filename <> "" then filename else (basename ^ ".cpp") in
        filename, prefix
      end else
        filename, prefix
      in

    (* Compute the default basename *)
    let default_basename = get_program_basename() in
    (* Handles the prefix and the filename *)
    let prefix =
      if prefix <> "" then prefix else default_basename in
    let filename =
      if filename <> "" then filename else default_basename ^ ".cpp" in
    (* DEBUG: Printf.printf "script_cpp default_basename=%s filename=%s prefix=%s \n" default_basename filename prefix; *)

    (* Handles on-the-fly inlining *)
    let input_file =
      match inline with
      | [] -> filename
      | _ ->
          let basename = Filename.chop_extension filename in
          let inlinefilename = basename ^ "_inlined.cpp" in
          generated_source_with_inlined_header_cpp filename inline inlinefilename;
          if debug_inline_cpp then Printf.printf "Generated %s\n" inlinefilename;
          inlinefilename
      in

    (* Set the input file, execute the function [f], dump the results. *)
    script (fun () ->
      Trace.init ~prefix ~parser input_file;
      begin
        try f()
        with
        | Stop -> ()
        | e -> Printf.eprintf "===> Script failed: %s\n" prefix; raise e
      end;
      flush stdout;
      if check_exit_at_end && Flags.get_exit_line() <> None
        then Trace.dump_diff_and_exit ();
      (* Stores the current ast to the end of the history *)
      Trace.dump ~prefix (); (* LATER: in theory, providing the prefix in function "init" should suffice; need to check, however, what happens when the file is not in the current folder *)
      (* Dump full trace if [-dump-trace] option was provided;
         in this case, record the last step in the history *)
      if !Flags.dump_trace || !Flags.analyse_stats then begin
        Trace.check_exit_and_step ~is_small_step:false ();
        Stats.report_full_stats ();
      end;
      if !Flags.dump_trace then begin
        Trace.dump_traces_to_js ~prefix ();
      end;
      begin match !Flags.dump_big_steps with
      | None -> ()
      | Some foldername-> Trace.dump_steps ~onlybig:true ~prefix foldername
      end;
      begin match !Flags.dump_small_steps with
      | None -> ()
      | Some foldername-> Trace.dump_steps ~prefix foldername
      end;
      Trace.finalize();
    );
    Parsers.select saved_parser (* restore original parser *)
    (* Printf.printf "END  %s\n" basename *)

(* [doc_script_cpp ~batching ~parser f src]: is a variant of [script_cpp] that takes as input a piece of source code [src]
    as a string, and stores this contents into [foo_doc.cpp], where [foo.ml] is the name of the current file. It then
    executes the transformation [f] using [script_cpp ~batching:"foo_doc.ml"]  *)
let doc_script_cpp ?(batching : string = "") ?(parser : Parsers.cparser = Parsers.Default) (f : unit -> unit) (src : string) : unit =
  (* Handle names *)
  let basename =
    if batching = "" then get_program_basename() else (Filename.chop_extension batching) in
  let docbasename = basename ^ "_doc" in
  let docfilename = docbasename ^ ".cpp" in
  let batching =
    if batching = "" then "" else docbasename ^ ".ml" in
  (* Special flag to save the diff of the first step *)
  Flags.documentation_save_file_at_first_check := docbasename;
  (* Write the contents of the cpp file; note that it will be overwritten at the first '!!' from the script. *)
  Xfile.put_contents docfilename src;
  (* Involve [script_cpp] *)
  if batching <> ""
    then script_cpp ~batching:batching ~parser ~check_exit_at_end:false f
    else script_cpp ~prefix:docbasename ~filename:docfilename ~parser ~check_exit_at_end:false f;
  Flags.documentation_save_file_at_first_check := ""

(* LATER:   add  script_rust  following script_cpp *)
