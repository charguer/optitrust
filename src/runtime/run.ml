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

(* [generate_source_with_inlined_header_cpp input_file inline output_file]:
   takes a file [input_file] and produces a file [output_file] obtained by
   inlining in the source the "#include" corresponding to the files listed
   in the list of filenames [inline].
  If an [#include "bar.h"] has already been substituted once, then the other occurrences are ignored.
  If an item "bar.hc" (LATER or .hcpp) is provided in the list, then [#include "bar.h"] will be
  intrepreted as [#include "bar.h"; #include "bar.cpp"], and the substitutions will be
  performed as if the [~inline] argument contained ["bar.cpp"; "bar.h"].
  If an item "bar.h-" is provided, then [#include "bar.h"] will be deleted.

  LATER: currently this test only works to include files that are in the same directory
  as the cpp file, we should generalize this in the support. *)

(* [debug_inline_cpp]: only for debugging purposes *)
let debug_inline_cpp = false

let generate_source_with_inlined_header_cpp (basepath : string) (input_file : string) (inline : string list) (output_file : string) : unit =
  let s = ref (Xfile.get_contents (Filename.concat basepath input_file)) in
  let perform_inline finline =
      let include_instr = "#include \"" ^ finline ^ "\"" in
      if debug_inline_cpp then Printf.printf "Inlined %s\n" include_instr;
      let contents = Xfile.get_contents (Filename.concat basepath finline) in
      s := Tools.string_subst_first include_instr contents !s;
      s := Tools.string_subst include_instr "" !s
    in
  let process_item finline =
    (* let basename = Filename.basename finline in *)
    let extension = Filename.extension finline in
    if extension = ".hc" then begin
      let corename = Filename.chop_extension finline in
      let fheader = corename ^ ".h" in
      let fimplem = corename ^ ".c" in
      let include_instr = "#include \"" ^ fheader ^ "\"" in
      let include_instr_new = "#include \"" ^ fheader ^ "\"\n#include \"" ^ fimplem ^ "\"" in
      if debug_inline_cpp then Printf.printf "Prepare inline of implementation for %s\n" include_instr;
      s := Tools.string_subst include_instr include_instr_new !s;
      perform_inline fimplem;
      perform_inline fheader;
    end else if extension = ".h-" then begin
      let corename = Filename.chop_extension finline in
      let fheader = corename ^ ".h" in
      let include_instr = "#include \"" ^ fheader ^ "\"" in
      s := Tools.string_subst include_instr "" !s;
    end else begin
      perform_inline finline
    end
  in
  List.iter process_item inline;
  Xfile.put_contents (Filename.concat basepath output_file) !s

(* [get_program_basename ()]: returns the basename of the current binary program being used.
    It takes care to remove the leading './' and takes care to remove the "with_lines" suffix. *)
let get_program_basename () : string =
  Flags.process_program_name ();
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

(* [may_report_time msg f]: returns the result of [f()] and, if not in batch mode, reports the time taken by that call on stdout. *)
let may_report_time (msg : string) (f : unit -> 'a) : 'a =
  if not !Flags.report_exectime then
    f ()
  else begin
    let (r, t) = Tools.measure_time f in
    Printf.printf "Time %s: %dms\n" msg t;
    r
  end

(* [script ~filename ~extension ~batching ~check_exit_at_end ~prefix f]:
   serves as "main" function for an Optitrust script. It takes care of parsing
   the command line arguments, handling the errors, and parsing the file that will be processed,
   before running the function [f] provided and outputing the results.

   In transformation scripts you should prefer the language specific variants such as
   [script_cpp].

   It invokes [Trace.init "foo.xxx"] at start, where "foo" is the basename of
   the current script named "foo.ml" and ".xxx" is the [extension]
   (alternatively, this name can be specified using the ~filename argument).
   At the end of the script, it invokes [Trace.dump];
   (the main output file is named "foo_out.xxx").

   Paths are all relative to the script.

   It takes the following parameters:
   - [~check_exit_at_end:false] is an option for deactivating the implicit call to [Trace.check_exit()]
      at the end of the execution of [f] (LATER: will be deprecated)
      This flag only has an effect if a [-exit_line] option was passed on the command line.
   - [~prefix:string] allows providing the basename for the output files produced
   *)
let script ?(filename : string option) ~(extension : string) ?(check_exit_at_end : bool = true) ?(prefix : string option) ?(capture_show_in_batch = false) (f : unit -> unit) : unit =
  Flags.process_cmdline_args ();
  Target.show_next_id_reset ();

  let program_basename = get_program_basename () in
  let dirname = Filename.dirname program_basename in
  let prefix =
    match prefix with
    | Some prefix -> Filename.concat dirname prefix
    | None -> program_basename
  in
  let filename =
    match filename with
    | Some filename -> Filename.concat dirname filename
    | None -> program_basename ^ extension
  in

  let produce_trace () : unit =
    may_report_time "dump-trace" (fun () ->
      Trace.dump_full_trace_to_js ~prefix ());
    if !Flags.trace_as_text then
      may_report_time "dump-trace-as-text" (fun () ->
        Trace.dump_trace_to_textfile ~prefix ());
    in

  (* DEBUG: Printf.printf "script default_basename=%s filename=%s prefix=%s \n" default_basename filename prefix; *)

  let stats_before = Stats.get_cur_stats () in
  let contents_captured_show = ref "" in
  let activate_capture_show = capture_show_in_batch && not (Flags.is_execution_mode_step()) in

  (* Set the input file, execute the function [f], dump the results. *)
  (try
    begin
      try
        Trace.init ~program:program_basename ~prefix filename;
        if !Flags.check_validity then
          Trace.step ~kind:Step_small ~tags:["pre-post-processing"] ~name:"Preprocessing loop contracts" (fun () ->
            Resources.make_strict_loop_contracts [];
          );
        Show.with_captured_show ~activated:activate_capture_show contents_captured_show (fun () ->
          may_report_time "script-exec" f)
      with
      | Stop -> ()
      | e when Flags.is_execution_mode_trace () -> (* FIXME: remove when cond *)
          let backtrace = Printexc.get_backtrace () in
          Trace.finalize_on_error ~exn:e;
          produce_trace();
          Printf.eprintf "========> BACKTRACE:\n%s\n" backtrace;
          exit 1 (* FIXME: don't exit in batch? *)
    end;
    flush stdout;
    (* If we requested a diff for the last line of the script, print it *)
    if check_exit_at_end
      then Trace.check_exit_at_end();
    (* Stores the current ast to the end of the history *)
    Trace.close_smallstep_if_needed();
    Trace.close_bigstep_if_needed();
    (* Optionally, print in comment at the end of the [_out.cpp] file the stdout output produced by the script *)
    let append_comments =
      if !contents_captured_show = ""
        then ""
        else "CAPTURED STDOUT:\n" ^ !contents_captured_show
      in
    (* Collapse the step stack onto the root step *)
    Trace.finalize ();
        (* LATER:
          IF ~expected_ast<>"" then load and unserialized expected ast and
            call trm_cmp and store result in a list ref,
            and batch.ml calls at the very end Run.batch_postlude
            which dumps the list contents on stdout and into a file.
            In this case, we skip trace.dump and other dumps. *)

    (* Dump the final AST produced by the script, using the [cur_style] of the trace *)
    Trace.dump ~store_in_trace:false (Trace.get_style()) ~append_comments ~prefix ();
        (* LATER: in theory, providing the prefix in function "init" should suffice; need to check, however, what happens when the file is not in the current folder *)

    (* Dump full trace if option [-dump-trace] as provided *)
    if !Flags.execution_mode = Execution_mode_full_trace
      then produce_trace();

    (* Dump one file for each big step if option [-dump-big-steps] was provided *)
    begin match !Flags.dump_big_steps with
    | None -> ()
    | Some foldername -> Trace.dump_steps ~onlybig:true ~prefix foldername
    end;
    (* Dump one file for each small step if option [-dump-small-steps] was provided *)
    begin match !Flags.dump_small_steps with
    | None -> ()
    | Some foldername -> Trace.dump_steps ~prefix foldername
    end;
    Trace.close_logs();
  with e ->
    Trace.close_logs();
    Printexc.(raise_with_backtrace e (get_raw_backtrace ()))
  );
  (* Dump statistics if they were requested *)
  let stats_after = Stats.get_cur_stats () in
  if !Flags.analyse_stats
    then
      let stats_str = Stats.stats_diff_str stats_before stats_after in
      Printf.printf "%s\n" stats_str;

  (* Printf.printf "END  %s\n" basename; *)
  ()

(* [script_cpp ~filename ~prepro ~inline ~check_exit_at_end ~prefix f]:
   is a specialized version of [script f] that parses C/C++ files.

   Its specific options are:
   - [~inline:["foo.cpp";"bar.h"]] allows to perform substitution of "#include" directives
     with the contents of the corresponding files; the substitutions are performed one after
     the other, meaning that "bar.h" will be inlined if included from "foo.cpp".
     See the specification of [generate_source_with_inlined_header_cpp] for additional features.
   The rest of the options are the same as [script f] *)
let script_cpp ?(filename : string option) ?(prepro : string list = []) ?(inline : string list = []) ?(check_exit_at_end : bool = true) ?(capture_show_in_batch = false) ?(prefix : string option) (f : unit -> unit) : unit =
  may_report_time "script-cpp" (fun () ->
    (* Handles preprocessor -- FUTURE USE MENHIR PARSER
    Compcert_parser.Clflags.prepro_options := prepro;
    *)

    (* Handles on-the-fly inlining *)
    let filename =
      match inline with
      | [] -> filename
      | _ ->
        let program_basename = get_program_basename () in
        let basepath = Filename.dirname program_basename in
        let filename =
          match filename with
          | Some filename -> filename
          | None -> (Filename.basename program_basename) ^ ".cpp"
        in
        let basename = Filename.chop_extension filename in
        let inlinefilename = basename ^ "_inlined.cpp" in
        generate_source_with_inlined_header_cpp basepath filename inline inlinefilename;
        if debug_inline_cpp then Printf.printf "Generated %s\n" inlinefilename;
        Some inlinefilename
    in

    script ?filename ~capture_show_in_batch ~extension:".cpp" ~check_exit_at_end ?prefix f)

(* [doc_script_cpp ~f src]: is a variant of [script_cpp] that takes as input a piece of source code [src]
    as a string, and stores this contents into [foo_doc.cpp], where [foo.ml] is the name of the current file. It then
    executes the transformation [f] using [script_cpp]  *)
let doc_script_cpp (f : unit -> unit) (src : string) : unit =
  (* Handle names *)
  let basename = get_program_basename () in
  let docbasename = basename ^ "_doc" in
  let docfilename = docbasename ^ ".cpp" in

  (* Special flag to save the diff of the first step *)
  Flags.documentation_save_file_at_first_check := docbasename;
  (* Write the contents of the cpp file; note that it will be overwritten at the first '!!' from the script. *)
  Xfile.put_contents docfilename src;

  (* Invoke [script_cpp] *)
  script_cpp ~prefix:(Filename.basename docbasename) ~filename:(Filename.basename docfilename) ~check_exit_at_end:false f;

  Flags.documentation_save_file_at_first_check := ""

