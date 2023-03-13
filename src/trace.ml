open Ast
open Stats

(* [line_of_last_step]: stores the line number from the source script at which a step
    ('!!' or '!^') was last processed. *)
let line_of_last_step = ref (-1)



(******************************************************************************)
(*                             Debugging tools                                *)
(******************************************************************************)

(* [report "mymessage" t]: can be used for debugging. *)
let report (msg : string) (t : trm) : unit =
  Printf.printf "%s: %s\n" msg (AstC_to_c.ast_to_string t)

(******************************************************************************)
(*                             Logging management                             *)
(******************************************************************************)

(* [timing_log_handle]: is a handle on the channel for writing timing reports. *)
let timing_log_handle = ref None

(* [stats_log_handle]: is a handle on the channel for writing stats reports. *)
let stats_log_handle = ref None

(* [logs]: is a reference on the list of open log channels. *)
let logs : (out_channel list) ref = ref []

(* [close_logs]: closes all open log channels. *)
let close_logs () : unit =
  List.iter (fun log -> close_out log) !logs;
  logs := []

(* [init_logs]: initializes the log files. It closes any existing logs.
   Returns the one log created. *)
let init_logs directory prefix =
  close_logs();
  let clog = open_out (directory ^ prefix ^ ".log") in
  let timing_log = open_out ("timing.log") in
  timing_log_handle := Some timing_log;
  let stats_log = open_out ("stats.log") in
  stats_log_handle := Some stats_log;
  logs := timing_log :: stats_log :: clog :: [];
  clog

(* [write_log clog msg]: writes the string [msg] to the channel [clog]. *)
let write_log (clog : out_channel) (msg : string) : unit =
  output_string clog msg;
  flush clog

(* [trm_to_log clog styp t]: writes in the channel [clog] the term [t],
   and its typing information described by the string [styp]. *)
let trm_to_log (clog : out_channel) (exp_type : string) (t : trm) : unit =
  let sloc =
    match t.loc with
    | None -> ""
    | Some {loc_file = _; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
       Printf.sprintf "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
    in
  let msg = Printf.sprintf (" -expression\n%s\n" ^^ " %s is a %s\n") (AstC_to_c.ast_to_string t) sloc exp_type in
 write_log clog msg

(******************************************************************************)
(*                             Timing logs                                    *)
(******************************************************************************)

(* DEPRECATED *)
(* [write_timing_log msg]: writes a message in the timing log file. *)
let write_timing_log (msg : string) : unit =
  let timing_log = match !timing_log_handle with
    | Some log -> log
    | None -> failwith "Trace.uninitialized timing log"
   in
   write_log timing_log msg


(* DEPRECATED *)
(* [measure_time f]: returns a pair made of the result of [f()] and
   of the number of milliseconds taken by that call. *)
let measure_time (f : unit -> 'a) : 'a * int =
  let t0 = Unix.gettimeofday () in
  let res = f() in
  let t1 = Unix.gettimeofday () in
  res, (Tools.milliseconds_between t0 t1)

(* DEPRECATED *)
(* [timing_nesting]: records the current level of nesting of calls to the
   [timing] function. It is used for printing tabulations in the reports. *)
let timing_nesting : int ref = ref 0

(* [timing ~name f]: writes the execution time of [f] in the timing log file. *)
let timing ?(cond : bool = true) ?(name : string = "") (f : unit -> 'a) : 'a =
  if !Flags.analyse_stats && cond then begin
    incr timing_nesting;
    let res, time = measure_time f in
    decr timing_nesting;
    let msg = Printf.sprintf "%s%d\tms -- %s\n" (Tools.spaces (2 * !timing_nesting)) time name in
    write_timing_log msg;
    res
  end else begin
    f()
  end

(* DEPRECATED *)
(* [time name f]: is a shorthand for [timing ~cond:!Flags.analyse_stats_details ~name]. *)
let time (name : string) (f : unit -> 'a) : 'a =
  timing ~cond:!Flags.analyse_stats_details ~name f

(* [start_time]: stores the date at which the script execution started (before parsing). *)
let start_time = ref (0.)

(* [last_time]: stores the date at which the execution of the current step started. *)
let last_time = ref (0.)

(* DEPRECATED *)
(* [last_time_update()]: updates [last_time] and returns the delay
   since last call -- LATER: find a better name. *)
let last_time_update () : int =
  let t0 = !last_time in
  let t = Unix.gettimeofday() in
  last_time := t;
  Tools.milliseconds_between t0 t

(* DEPRECATED *)
(* [report_full_time ()]: reports the time for the last step, and for the full total. *)
let report_full_time () : unit =
  write_timing_log (Printf.sprintf "------------------------TOTAL TRANSFO TIME: %.3f s\n" (!last_time -. !start_time))

(* [id_big_step]: traces the number of big steps executed. This reference is used only
   when executing a script from the command line, because in this case the line numbers
   from the source script are not provided on calls to the [step] function. *)
let id_big_step = ref 0


(******************************************************************************)
(*                             File input                                     *)
(******************************************************************************)

(* [get_cpp_includes filename]: gets the list of file includes syntactically visible
   on the first lines of a CPP file -- this implementation is quite restrictive. *)
let get_cpp_includes (filename : string) : string =
  (* make sure the include list is clean *)
  let includes = ref "" in
  let c_in = open_in filename in
  try
    while (true) do
      let s = input_line c_in in
      if Str.string_match (Str.regexp "^#include") s 0 then
        includes := !includes ^ s ^ "\n\n";
    done;
    !includes
  with
  | End_of_file -> close_in c_in; !includes

(* [parse filename]: returns (1) a list of filenames corresponding to the '#include',
   and the OptiTrust AST. *)
let parse ?(parser = Parsers.Default) (filename : string) : string * trm =
  let parser = Parsers.get_selected ~parser () in
  if !Flags.debug_reparse
    then Printf.printf "Parsing %s using %s\n" filename (Parsers.string_of_cparser parser);
  print_info None "Parsing %s...\n" filename;
  let includes = get_cpp_includes filename in
  let command_line_include =
    List.map Clang.Command_line.include_directory
      (Clang.default_include_directories ()) in
  let command_line_warnings = ["-Wno-parentheses-equality"; "-Wno-c++11-extensions"] in
  let command_line_args = command_line_warnings @ command_line_include in

  let t =
    stats ~name:"tr_ast" (fun () ->
      let parse_clang () =
        Clang_to_astRawC.tr_ast (Clang.Ast.parse_file ~command_line_args filename) in
      let parse_menhir () =
        CMenhir_to_astRawC.tr_ast (MenhirC.parse_c_file_without_includes filename) in
      let rawAst = match parser with
        | Parsers.Default -> assert false (* see def of parser; !dParsers.default_cparser should not be Default *)
        | Parsers.Clang -> parse_clang()
        | Parsers.Menhir -> parse_menhir()
        | Parsers.All ->
           let rawAstClang = parse_clang() in
           let rawAtMenhir = parse_menhir() in
           let strAstClang = AstC_to_c.ast_to_string rawAstClang in
           let strAstMenhir = AstC_to_c.ast_to_string rawAtMenhir in
           if strAstClang <> strAstMenhir then begin
             (* LATER: we could add a prefix based on the filename, but this is only for debug *)
             Xfile.put_contents "ast_clang.cpp" strAstClang;
             Xfile.put_contents "ast_menhir.cpp" strAstMenhir;
            fail None "Trace.parse: [-cparser all] option detected discrepencies;\n meld ast_clang.cpp ast_menhir.cpp";
           end else
           (* If the two ast match, we can use any one of them (only locations might differ); let's use the one from the default parser. *)
             if Parsers.default_cparser = Parsers.Clang then rawAstClang else rawAtMenhir
          in
        if !Flags.bypass_cfeatures
          then rawAst
          else Ast_fromto_AstC.cfeatures_elim rawAst
    )
  in

  print_info None "Parsing Done.\n";
  print_info None "Translating Done...\n";

  print_info None "Translation done.\n";
  (includes, t)

(******************************************************************************)
(*                             Trace management                               *)
(******************************************************************************)

(* [context]: contains general information about:
   - the source code that was loaded initially using [set_init_file],
   - the prefix of the filenames in which to output the final result using [dump]
   - the log file to report on the transformation performed. *)
type context =
  { extension : string;
    directory : string;
    prefix : string;
    includes : string;
    clog : out_channel; }

(* [contex_dummy]: used for [trace_dummy]. *)
let context_dummy : context =
  { extension = ".cpp";
    directory = "";
    prefix = "";
    includes = "";
    clog = stdout; }

(* [stepdescr]: description of a script step. *)
type stepdescr = {
  mutable isbigstep : string option; (* if the step is the beginning of a big step,
                                        then the value is [Some descr] *)
  mutable script : string; (* excerpt from the transformation script, or "" *)
  mutable exectime : int; } (* number of milliseconds, -1 if unknown *)

(* [stepdescr_for_interactive_step]: dummy stepdescr used for interactive steps such as [show]. *)
let stepdescr_for_interactive_step =
  { isbigstep = None; script = ""; exectime = 0; }

(* [trace]: a record made of a context, a current AST, and a list of ASTs that were
   saved as "interesting intermediate steps", via the [Trace.save] function.
   Any call to the [step] function adds a copy of [cur_ast] into [history]. *)
type trace = {
  mutable context : context;
  mutable cur_ast : trm;
  mutable history : trms;
  mutable stepdescrs : stepdescr list } (* same length as the history field *)
  (* LATER: history will become a tree *)

(* [trm_dummy]: dummy trm. *)
let trm_dummy : trm =
  trm_val (Val_lit Lit_unit)

(* [trace_dummy]: an initial trace made of dummy context and dummy trm. *)
let trace_dummy : trace =
  { context = context_dummy;
    cur_ast = trm_dummy; (* dummy *)
    history = [];
    stepdescrs = []; (* indicate for each step if it was a big step *)
      (* LATER: could improve the storage of bigsteps *)
    }

(* [the_trace]: the trace produced by the current script. *)
let the_trace : trace =
  trace_dummy

(* DEPRECATED?
let set_the_trace (trace : trace) : unit :=
  the_trace.context <- trace.context;
  the_trace.cur_ast <- trace.cur_ast;
  the_trace.history <- trace.history;
  the_trace.stepdescrs <- trace.stepdescrs
  *)

(* [is_trace_dummy()]: returns whether the trace was never initialized. *)
let is_trace_dummy () : bool =
  the_trace.history = []
  (* DEPRECATED? *)

(* [reset()]: restores the global state (object [trace]) in its uninitialized state,
   like at the start of the program. This operation is automatically called by [Trace.init]. *)
let reset () : unit =
  close_logs();
  the_trace.context <- trace_dummy.context;
  the_trace.cur_ast <- trace_dummy.cur_ast;
  the_trace.history <- trace_dummy.history;
  the_trace.stepdescrs <- trace_dummy.stepdescrs

(* [ml_file_excerpts]: maps line numbers to the corresponding sections in-between [!!] marks in
   the source file. Line numbers are counted from 1 in that map. *)
module Int_map = Map.Make(Int)
let ml_file_excerpts = ref Int_map.empty

(* [compute_ml_file_excerpts lines]: is a function for grouping lines according to the [!!] symbols. *)
let compute_ml_file_excerpts (lines : string list) : string Int_map.t =
  let r = ref Int_map.empty in
  let start = ref 0 in
  let acc = Buffer.create 3000 in
  let push () =
    r := Int_map.add (!start+1) (Buffer.contents acc) !r;
    Buffer.clear acc; in
  let regexp_let = Str.regexp "^[ ]*let" in
  let starts_with_let (str : string) : bool =
    Str.string_match regexp_let str 0 in
  (* match a line that starts with '!!' or LATER '!^' or 'bigstep' *)
  let regexp_step = Str.regexp "^[ ]*\\(!!\\|!\\^\\|bigstep\\)" in
  let starts_with_step (str : string) : bool =
    Str.string_match regexp_step str 0 in
  let process_line (iline : int) (line : string) : unit =
    if starts_with_step line then begin
      push();
      start := iline;
    end;
    if not (starts_with_let line) then begin
      Buffer.add_string acc line;
      Buffer.add_string acc "\n";
    end;
    in
  List.iteri process_line lines;
  push();
  !r

(* [get_excerpt line]: returns the piece of transformation script that starts on the given line. Currently returns the ""
    in case [compute_ml_file_excerpts] was never called. LATER: make it fail in that case. *)
let get_excerpt (line : int) : string =
  if line = - 1 then failwith "get_excerpt: requires a valid line number";
  if !ml_file_excerpts = Int_map.empty then "" else begin (* should "" be failure? *)
  match Int_map.find_opt line !ml_file_excerpts with
    | Some txt -> txt
    | None -> (*LATER: failwith? *) Printf.sprintf "<unable to retrieve line %d from script>" line
  end

(* [get_initial_ast ~parser ser_mode ser_file filename]: gets the initial ast before applying any trasformations
     [parser] - choose which parser to use for parsing the source code
     [ser_mode] - serialization mode
     [ser_file] - if serialization is used for the initial ast, the filename of the serialized version
                  of the source code is needed
     [filename] - filename of the source code  *)
let get_initial_ast ?(parser : Parsers.cparser = Parsers.Default) (ser_mode : Flags.serialized_mode) (ser_file : string)
  (filename : string) : (string * trm) =
  (* LATER if ser_mode = Serialized_Make then let _ = Sys.command ("make " ^ ser_file) in (); *)
  let includes = get_cpp_includes filename in
  let ser_file_exists = Sys.file_exists ser_file in
  let ser_file_more_recent = if (not ser_file_exists) then false else Xfile.is_newer_than ser_file filename in
  let auto_use_ser = (ser_mode = Serialized_Auto && ser_file_more_recent) in
  if (ser_mode = Serialized_Use
   || ser_mode = Serialized_Make
   || auto_use_ser) then begin
    if not ser_file_exists
      then fail None "Trace.get_initial_ast: please generate a serialized file first";
    if not ser_file_more_recent
      then fail None (Printf.sprintf "Trace.get_initial_ast: serialized file is out of date with respect to %s\n" filename);
    let ast = unserialize_from_file ser_file in
    if auto_use_ser
      then Printf.printf "Loaded ast from %s.\n" ser_file;
    (includes, ast)
    end
  else
    parse ~parser filename

(* [init f]: initializes the trace with the contents of the file [f].
   This operation should be the first in a transformation script.
   The history is initialized with the initial AST.
   [~prefix:"foo"] allows to use a custom prefix for all output files,
   instead of the basename of [f]. *)
(* LATER for mli: val set_init_source : string -> unit *)
let init ?(prefix : string = "") ?(parser : Parsers.cparser = Parsers.Default) (filename : string) : unit =
  reset ();
  let basename = Filename.basename filename in
  let extension = Filename.extension basename in
  let directory = (Filename.dirname filename) ^ "/" in
  let default_prefix = Filename.remove_extension basename in
  let ml_file_name =
    if Tools.pattern_matches "_inlined" default_prefix
      then List.nth (Str.split (Str.regexp "_inlined") default_prefix) 0
      else default_prefix in
  if !Flags.analyse_stats || !Flags.dump_trace then begin
    let src_file = (ml_file_name ^ ".ml") in
    if Sys.file_exists src_file then begin
      let lines = Xfile.get_lines src_file in
      ml_file_excerpts := compute_ml_file_excerpts lines;
    end;
  end;
  let mode = !Flags.serialized_mode in
  start_stats := get_cur_stats ();
  last_stats := !start_stats;

  let prefix = if prefix = "" then default_prefix else prefix in
  let clog = init_logs directory prefix in
  let ser_file = basename ^ ".ser" in

  let (includes, cur_ast), stats_parse = Stats.measure_stats (fun () -> get_initial_ast ~parser mode ser_file filename) in

  let context = { extension; directory; prefix; includes; clog } in
  let stepdescr = { isbigstep = None;
                    script = "Result of parsing";
                    exectime = int_of_float(stats_parse.stats_time); } in
  the_trace.context <- context;
  the_trace.cur_ast <- cur_ast;
  the_trace.history <- [cur_ast];
  the_trace.stepdescrs <- [stepdescr]; (* TODO: use a function the_trace_set_fields *)
  if mode = Serialized_Build || mode = Serialized_Auto
    then serialize_to_file ser_file cur_ast;
  if mode = Serialized_Build
    then exit 0;
  print_info None "Starting script execution...\n"

(* [finalize()]: should be called at the end of the script, to properly close the log files
    created by the call to [init]. *)
let finalize () : unit =
  close_logs()

(* [alternative f]: executes the script [f] in the original state that
   was available just after the call to [init].
   After the call, all the actions performed are discarded.

  Current usage:
     !! Trace.alternative (fun () ->
        !! Loop.fusion_on_block [cLabel "tofusion"];
        !!());

   LATER: figure out if it is possible to avoid "!!" in front and tail of [Trace.restart].
   LATER: figure out if this implementation could be extended in the presence of [switch]. *)
let alternative (f : unit->unit) : unit =
  let trace = the_trace in
  if trace.history = [] || trace.stepdescrs = []
    then fail None "Trace.alternative: the history is empty";
  let _,init_ast = Xlist.unlast trace.history in
  let _,init_stepdescr = Xlist.unlast trace.stepdescrs in
  let cur_ast = trace.cur_ast in
  let history = trace.history in
  let stepdescrs = trace.stepdescrs in
  the_trace.cur_ast <- init_ast;
  the_trace.history <- [init_ast];
  the_trace.stepdescrs <- [init_stepdescr];
  f();
  the_trace.cur_ast <- cur_ast;
  the_trace.history <- history;
  the_trace.stepdescrs <- stepdescrs
  (* TODO: beautify? *)

(* [switch cases]: allows to introduce a branching point in a script.
   The [cases] argument gives a list of possible continuations (branches).
   Each of the branches can terminate with a [dump] operation to produce
   its output in a specific file. Alternatively, there could be further
   tranformations after the [switch] construct---a diamond construct.
   In such case, the instructions that follow the [switch] are applied
   in parallel on each of the traces, where one trace corresponds to one
   possible path in the script (via the branches).
   The optional argument [only_branch] can be use to temporary disable
   all branches but one. This is currently needed for the interactive mode
   to work. Branches are numbered from 1 (not from zero). *)
(* LATER for mli: switch : ?only_branch:int -> (unit -> unit) list -> unit *)
(* DEPRECATED
let switch ?(only_branch : int = 0) (cases : (unit -> unit) list) : unit =
  (* Close logs: new logs will be opened in every branch. *)
  close_logs ();
  let list_of_traces =
    Xlist.fold_lefti
      (fun i tr f ->
        let branch_id = i + 1 in
        if only_branch = 0 || branch_id = only_branch then
          begin
            let old_traces = !traces in
            let new_traces =
              List.fold_right
                (fun trace acc_traces ->
                  let context = trace.context in
                  (* create an extended prefix for this branch, unless there is a single branch *)
                  let prefix =
                    if List.length cases <= 1 || only_branch <> 0
                      then context.prefix
                      else context.prefix ^ "_" ^ (string_of_int branch_id)
                    in
                  (* create and register new log channel *)
                  let clog = open_out (context.directory ^ prefix ^ ".log") in
                  logs := clog :: !logs;
                  (* execute each branch in a single context *)
                  let branch_trace = { trace with context = { context with prefix; clog } } in
                  traces := [branch_trace];
                  f ();
                  (* store the traces produced by this branch *)
                  (!traces) :: acc_traces;
                )
                old_traces
                []
            in
            traces := old_traces;
            (List.flatten new_traces) :: tr
          end
        else tr
      )
      []
      cases
  in
  traces := List.flatten (List.rev list_of_traces)
 *)

(* [apply f]: applies the transformation [f] to the current AST,
   and updates the current ast with the result of that transformation.
   If there are several active trace (e.g., after a [switch]),
   then [f] is applied to each of the traces. During the execution of [f]
   on a given trace, the set of traces is replaced with a singleton set
   made of only that trace; this allows for safe re-entrant calls
   (i.e., the function [f] itself may call [Trace.apply]. *)
let apply (f : trm -> trm) : unit =
  if is_trace_dummy()
    then fail None "Trace.init must be called prior to any transformation.";
  the_trace.cur_ast <- f the_trace.cur_ast

(* [call f]: is similar to [apply] except that it applies to a function [f]
   with unit return type: [f] is meant to update the [cur_ast] by itself
   through calls to [apply].
   If there are several active trace (e.g., after a [switch]),
   then [f] is applied to each of the traces. During the execution of [f]
   on a given trace, the set of traces is replaced with a singleton set
   made of only that trace; this allows for safe re-entrant calls
   (i.e., the function [f] itself may call [Trace.apply]. *)
   (* TODO: see whether it's not simpler to use Trace.get_ast() ; DEPRECATED? *)
let call (f : trm -> unit) : unit =
  if is_trace_dummy()
    then fail None "Trace.init must be called prior to any transformation.";
  f the_trace.cur_ast

(* [nextstep_isbigstep]: is a reference that stores a [Some descr]
   when the function [bigstep] is called. This reference is reset
   to [None] after the [step] function is called. It is initialized
   to false, for proper reporting in scripts that don't start with
   a [bigstep] instruction. *)
let nextstep_isbigstep : (string option) ref =
  ref (Some "Start of the script")

(* [bigstep s]: announces that the next step is a bigstep, and registers
   a string description for that step. *)
(* LATER: add the line argument *)
let bigstep (s : string) : unit =
  nextstep_isbigstep := Some s

(* [step()]: takes the current AST and adds it to the history.
   If there are several traces, it does so in every branch. *)
let step (stepdescr : stepdescr) : unit =
  the_trace.history <- the_trace.cur_ast :: the_trace.history;
  the_trace.stepdescrs <- stepdescr :: the_trace.stepdescrs

(* [check_recover_original()]: checks that the AST obtained so far
   is identical to the input AST, obtained from parsing. If not,
   it raises an error. *)
let check_recover_original () : unit =
  let check_same ast1 ast2 =
    if AstC_to_c.ast_to_string ast1 <> AstC_to_c.ast_to_string ast2
      then fail None "Trace.check_recover_original: the current AST is not identical to the original one."
      else () (* FOR DEBUG: Printf.printf "check_recover_original: successful" *)
    in
  let h = the_trace.history in
  match h with
  | [] -> failwith "check_recover_original: no history"
  | astLast :: [] -> () (* no operation performed, nothing to check *)
  | astLast :: astsBefore ->
      let _,astInit = Xlist.unlast astsBefore in
      check_same astLast astInit



(******************************************************************************)
(*                                   Output                                   *)
(******************************************************************************)

(* [cleanup_cpp_file_using_clang_format filename]: makes a system call to
   reformat a CPP file using the clang format tool.
   LATER: find a way to remove extra parentheses in ast_to_doc, by using
   priorities to determine when parentheses are required. *)
let cleanup_cpp_file_using_clang_format ?(uncomment_pragma : bool = false) (filename : string) : unit =
  stats ~name:(Printf.sprintf "cleanup_cpp_file_using_clang_format(%s)" filename) (fun () ->
    ignore (Sys.command ("clang-format -style=\"Google\" -i " ^ filename));
    if (* temporary *) false && uncomment_pragma
      then ignore (Sys.command ("sed -i 's@//#pragma@#pragma@' " ^ filename))
  )

(* [language]: choose the programming language of the source code. *)
type language = | Language_cpp | Language_rust | Language_ocaml

(* [language_of_extension extension]: based on the extension of a file choose the language *)
let language_of_extension (extension:string) : language =
  match extension with
  | ".cpp" -> Language_cpp
  | ".rs" -> Language_rust
  | ".ml" -> Language_ocaml
  | _ -> fail None ("Trace.language_of_extension: unknown extension " ^ extension)

(* [get_language ()]: get the language *)
let get_language () : language =
  language_of_extension the_trace.context.extension

(* [get_language ()]: get the includes directive *)
let get_includes () : string =
  the_trace.context.includes

(* [output_prog ctx prefix ast]: writes the program described by the term [ast]
   in several files:
   - one describing the raw AST ("prefix.ast")
   - one describing the internal AST ("prefix_enc.cpp")
   - one describing the CPP code ("prefix.cpp").
   The CPP code is automatically formatted using clang-format. *)
let output_prog ?(beautify:bool=true) ?(ast_and_enc:bool=true) (ctx : context) (prefix : string) (ast : trm) : unit =
  let use_clang_format = beautify && !Flags.use_clang_format in
  let file_prog = prefix ^ ctx.extension in
  let out_prog = open_out file_prog in
  begin try
    (* print C++ code with decoding *)
    (*   DEPRECATED
    Printf.printf "===> %s \n" (ctx.includes); print_newline();*)
    (* LATER: try to find a way to put the includes in the AST so we can do simply ast_to_file *)
    output_string out_prog ctx.includes;
    let beautify_mindex = beautify && !Flags.pretty_matrix_notation in
    if !Flags.bypass_cfeatures
      then AstC_to_c.ast_to_outchannel ~optitrust_syntax:true out_prog ast
      else AstC_to_c.ast_to_outchannel ~beautify_mindex ~comment_pragma:use_clang_format out_prog (Ast_fromto_AstC.cfeatures_intro ast);
    output_string out_prog "\n";
    close_out out_prog;
  with | Failure s ->
    close_out out_prog;
    failwith s
  end;
  (* beautify the C++ code --comment out for debug *)
  if use_clang_format
    then cleanup_cpp_file_using_clang_format ~uncomment_pragma:use_clang_format file_prog;
  (* ast and enc *)
  if ast_and_enc && !Flags.dump_ast_details then begin
    let file_ast = prefix ^ ".ast" in
    let file_enc = prefix ^ "_enc" ^ ctx.extension in
    let out_ast = open_out file_ast in
    let out_enc = open_out file_enc in
    begin try
      (* print the raw ast *)
      begin
        Ast_to_text.print_ast out_ast ast;
        output_string out_ast "\n";
        close_out out_ast;
      end;
      (* print the non-decoded ast *)
      output_string out_enc ctx.includes;
      AstC_to_c.ast_to_outchannel ~optitrust_syntax:true out_enc ast;
      output_string out_enc "\n";
      close_out out_enc;
      if use_clang_format
        then cleanup_cpp_file_using_clang_format file_enc;
    with | Failure s ->
      close_out out_ast;
      close_out out_enc;
      failwith s
    end
  end

(* [output_prog_check_empty ~ast_and_enc ctx prefix ast_opt]: similar to [output_prog], but it
   generates an empty file in case the [ast] is an empty ast. *)
let output_prog_check_empty ?(ast_and_enc : bool = true) (ctx : context) (prefix : string) (ast_opt : trm) : unit =
  match ast_opt.desc with
  | Trm_seq tl when Mlist.length tl <> 0 -> output_prog ~ast_and_enc ctx prefix ast_opt
  | _ ->
      let file_prog = prefix ^ ctx.extension in
      let out_prog = open_out file_prog in
      close_out out_prog


(* [history]: history type used for storing all the trace information about all steps
    prefix, ctx, and a list of pairs made of an ast and a stepdescr *)
type history = string * context * ((trm*stepdescr) list)

(* [get_history]: gets history from trace *)
let get_history ?(prefix : string = "") () : history =
  let extract (trace : trace) =
    let ctx = trace.context in
      let prefix =
        if prefix = "" then ctx.directory ^ ctx.prefix else prefix in
      if List.length trace.history <> List.length trace.stepdescrs
        then failwith "get_history: invariant broken, stepdescrs should have the same length as history";
      let hrev = List.combine trace.history trace.stepdescrs in
      let hist = (* remove the ast associated with the input file *)
        match List.rev hrev with
        | _::hist -> hist
        | _ -> failwith "Trace.get_history: empty history"
        in
      (prefix, ctx, hist)
    in
  extract the_trace (* TODO: inliner cette fonction *)


(* [dump_steps]: writes into files called [`prefix`_$i_out.cpp] the contents of each of the big steps,
    where [$i] denotes the index of a big step. *)
let dump_steps ?(onlybig : bool = false) ?(prefix : string = "") (foldername : string) : unit =
  ignore (Sys.command ("mkdir -p " ^ foldername));
  let (prefix, ctx, hist_and_descr) = get_history ~prefix () in
  let n = List.length hist_and_descr in
  let id = ref 0 in
  List.iteri (fun i (ast,stepdescr) ->
    let isstartofbigstep =
      match stepdescr.isbigstep with
      | None -> false
      | Some _descr -> true
      in
    let should_dump = if onlybig then ((isstartofbigstep) || (i = n-1)) else true in
    if should_dump then begin
      let prefixi = Printf.sprintf "%s/%s_%s%d_out" foldername prefix (if !id < 10 then "0" else "") !id in
      output_prog ctx prefixi ast;
      incr id;
    end;
  ) hist_and_descr


(* [dump_trace_to_js]: writes into a file called [`prefix`.js] the
   contents of each of the steps record by the script, both for
   small steps and big steps, including the diffs and the excerpts
   of proof scripts associated with each step.

   The argument [history_and_isbigstep] takes the history
   with oldest entry first (unliked the history record field).
   It does not include the parsing step. --LATER: include it.

   The JS file is
   structured as follows (up to the order of the definitions):

   var codes = []; // if the script has 3 '!!', the array will have 4 entries (one for the state of the code before each '!!' and one for the final result)
   codes[i] = window.atob("...");

   var smallsteps = []; // smallsteps.length = codes.length - 1
   smallsteps[i] = { diff: window.atob("...");
                     script: window.atob("...");
                     exectime: ... } // for future use
   var bigsteps = []; // bigsteps.length <= smallsteps.length
   bigstep.push ({ diff: window.atob("...");
                  start: idStart;
                  stop: idStop;
                  descr : window.atob("...") });
     // invariant: bigstep[j].stop = bigstep[j+1].start
   *)
let dump_trace_to_js (history : history) : unit =
  let (prefix, ctx, hist_and_descr) = history in
  let file_js = prefix ^ "_trace.js" in
  let out_js = open_out file_js in
  let out = output_string out_js in
  let sprintf = Printf.sprintf in
  let cmd s =
    (* FOR DEBUG Printf.printf "execute: %s\n" s; flush stdout; *)
    ignore (Sys.command s) in
  let compute_command_base64 (s : string) : string =
    cmd (sprintf "%s | base64 -w 0 > tmp.base64" s);
    Xfile.get_contents ~newline_at_end:false "tmp.base64"
    in
  let compute_diff () : string =
    compute_command_base64 "git diff --ignore-all-space --no-index -U10 tmp_before.cpp tmp_after.cpp" in
  let lastbigstepstart = ref (-1) in
  let nextbigstep_descr = ref "<undefined bigstep descr>" in
  (* LATER: catch failures *)
  (* LATER: support other languages than C/C++ *)
  out "var codes = [];\nvar smallsteps = [];\nvar bigsteps = [];\n";
  let n = List.length hist_and_descr in
  List.iteri (fun i (ast,stepdescr) ->
    (* obtain source code *)
    output_prog ctx "tmp_after" ast;
    let src = compute_command_base64 "cat tmp_after.cpp" in
    out (sprintf "codes[%d] = window.atob(\"%s\");\n" i src);
    (* obtain smallstep diff *)
    if i > 0 then begin
      let diff = compute_diff() in
      out (sprintf "smallsteps[%d] = { exectime: %d, script: window.atob(\"%s\"), diff: window.atob(\"%s\") };\n" (i-1) stepdescr.exectime (Base64.encode_exn stepdescr.script) diff);
    end;
    (* obtain bigstep diff *)
    let isstartofbigstep =
      match stepdescr.isbigstep with
      | None -> false
      | Some _descr -> true
      in
    let isendofbigstep = (i > 0 && isstartofbigstep) || i = n-1 in
    if isendofbigstep then begin
      cmd "mv tmp_big.cpp tmp_before.cpp";
      let diff = compute_diff() in
      out (sprintf "bigsteps.push({ start: %d, stop: %d, descr: window.atob(\"%s\"), diff: window.atob(\"%s\") });\n"  !lastbigstepstart i (Base64.encode_exn !nextbigstep_descr) diff);
    end;
    (* shift files to prepare for next step *)
    cmd "mv tmp_after.cpp tmp_before.cpp";
    if isstartofbigstep then begin
      cmd "cp tmp_before.cpp tmp_big.cpp";
      lastbigstepstart := i;
      begin match stepdescr.isbigstep with
      | None -> assert false
      | Some descr -> nextbigstep_descr := descr
      end;
    end
  ) hist_and_descr;
  cmd "rm -f tmp.base64 tmp_after.cpp tmp_before.cpp tmp_big.cpp";
  close_out out_js


(* [dump_traces_to_js]: dump all traces to js.
    LATER: later generalize to multiple traces, currently it would
       probably overwrite the same file over and over again. *)
let dump_traces_to_js ?(prefix : string = "") () : unit =
  let history = get_history ~prefix () in
  dump_trace_to_js history

(*
  filename = prefix ^ "_trace.js"
  let f = open_out filename

  fprintf  f "var trace = {};";
  let print_step i ast =
     fprintf f "traces[%d] = {" i;
     code_to_js f i ast;
     fprintf f "};";
     in
  List.iteri print_step !traces



  var trace = {};
  trace[0] = {`
  trace[1] = {..};
  trace[2] = {..};
 *)

(******************************************************************************)
(*                                   Reparse                                  *)
(******************************************************************************)

(* [reparse_trm ctx ast]: prints [ast] in a temporary file and reparses it using Clang. *)
let reparse_trm ?(info : string = "") ?(parser = Parsers.Default) (ctx : context) (ast : trm) : trm =
  if !Flags.debug_reparse then begin
    let info = if info <> "" then info else "of a term during the step starting at" in
    Printf.printf "Reparse: %s line %d.\n" info !line_of_last_step;
    flush stdout
  end;
  let in_prefix = ctx.directory ^ "tmp_" ^ ctx.prefix in
  output_prog ~beautify:false ctx in_prefix ast;

  let (_, t) = parse ~parser (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

(* [reparse()]: function takes the current AST, prints it to a file, and parses it
   as if it was a fresh input. Doing so ensures in particular that all the type
   information is properly set up. WARNING: reparsing discards all the marks in the AST. *)
let reparse ?(info : string = "") ?(parser = Parsers.Default) () : unit =
  let info = if info <> "" then info else "the code during the step starting at" in
  let parser = Parsers.get_selected ~parser () in
  the_trace.cur_ast <- reparse_trm ~info ~parser the_trace.context the_trace.cur_ast

(* Work-around for a name clash *)
let reparse_alias = reparse


(******************************************************************************)
(*                                   Dump                                     *)
(******************************************************************************)

(* [light_diff astBefore astAfter]: find all the functions that have not change after
    applying a transformation and hides their body for a more robust view diff. *)
let light_diff (astBefore : trm) (astAfter : trm) : trm * trm  =
    let topfun_before = top_level_fun_bindings astBefore in
    let topfun_after = top_level_fun_bindings astAfter in
    let topfun_common = get_common_top_fun topfun_before topfun_after in
    let filter_common ast = fst (hide_function_bodies (fun f -> List.mem f topfun_common) ast) in
    let new_astBefore = filter_common astBefore in
    let new_astAfter = filter_common astAfter in
    (new_astBefore, new_astAfter)

(* [dump_diff_and_exit()]: invokes [output_prog] on the current AST an also on the
   last item from the history, then it interrupts the execution of the script.
   This function is useful for interactively studying the effect of one particular
   transformation from the script.
   If option [-dump-last nb] was provided, output files are produced for the last [nb] step. *)
(* LATER for mli: dump_diff_and_exit : unit -> unit *)
let dump_diff_and_exit () : unit =
  if !Flags.analyse_stats then begin
    report_full_stats();
    write_timing_log (Printf.sprintf "------------START DUMP------------\n")
  end;
  stats ~name:"TOTAL for dump_diff_and_exit" (fun () ->
    print_info None "Exiting script\n";
    let trace = the_trace in
    let ctx = trace.context in
    let prefix = ctx.directory ^ ctx.prefix in
    (* Common printinf function *)
    let output_ast ?(ast_and_enc:bool=true) filename_prefix ast =
      output_prog_check_empty ~ast_and_enc ctx filename_prefix ast;
      print_info None "Generated: %s%s\n" filename_prefix ctx.extension;
      in
    (* CPP and AST output for BEFORE *)
    let astBefore =
      match trace.history with
      | t::_ -> t (* the most recently saved AST *)
      | [] -> Printf.eprintf "Warning: only one step in the history; consider previous step blank.\n"; empty_ast
      in
    let astAfter = trace.cur_ast in

    (* Compute light-diff: hide bodies of functions that are identical in astBefore and astAfter. *)
    let astBefore, astAfter =
      if !Flags.use_light_diff then light_diff astBefore astAfter else astBefore, astAfter in

    output_ast (prefix ^ "_before") astBefore;
    (* CPP and AST for BEFORE_N *)
    if !Flags.dump_last <> Flags.dump_last_default then begin
      let nb_requested = !Flags.dump_last in
      let nb_available = List.length trace.history in
      (* if nb_requested < nb_available
        then Printf.eprintf "Warning: not enought many steps for [dump_last]; completing with blank files.\n"; *)
      for i = 0 to nb_requested-1 do
        let astBeforeI = if i < nb_available then List.nth trace.history i else empty_ast in
        output_ast ~ast_and_enc:false (prefix ^ "_before_" ^ string_of_int i) astBeforeI
      done;
    end;
    (* CPP and AST for AFTER *)
    output_ast (prefix ^ "_after") astAfter;
    print_info None "Writing ast and code into %s.js " prefix;
    (* LATER output_js 0 prefix astAfter; *)
    (* Printf.printf "EXIT   %s\n" prefix; *)
  );
  (* Exit *)
  close_logs ();
  exit 0

(* [check_exit_and_step()]: performs a call to [check_exit], to check whether
   the program execution should be interrupted based on the command line argument
   [-exit-line], then it performas a call to [step], to save the current AST
   in the history, allowing for a visualizing the diff if the next call to
   [check_exit_and_step] triggers a call to [dump_diff_and_exit].
   If the optional argument [~reparse:true] is passed to the function,
   then the [reparse] function is called, replacing the current AST with
   a freshly parsed and typechecked version of it.
   The [~is_small_step] flag indicates whether the current step is small
   and should be ignored when visualizing big steps only. *)
(* The [is_small_step] option WILL PROBABLY BE DEPREACTED IN THE FUTURE *)
let check_exit_and_step ?(line : int = -1) ?(is_small_step : bool = true) ?(reparse : bool = false) () : unit =
  (* Special hack for minimizing diff in documentation *)
  if !Flags.documentation_save_file_at_first_check <> "" then begin
    let trace = the_trace in
    let ctx = trace.context in
    output_prog ctx !Flags.documentation_save_file_at_first_check (trace.cur_ast)
  end else begin
    (* Support for '!^' which simulates an on-the-fly call to [bigstep ""] -- will probably be DEPRECATED *)
    if not is_small_step then begin
      let descr = if line = -1 then "" else Printf.sprintf "Bigstep at line %d" line in
      bigstep descr; (* this call must be performed after reading [!nextstep_isbigstep] above;
        it saves the big-step information to be used at the next call to [check_exit_and_step] *)
    end;
    let isbigstep = !nextstep_isbigstep in (* TODO ARTHUR: rename this variable/fieldname *)
    let is_start_of_bigstep = isbigstep <> None in
    let ignore_step = !Flags.only_big_steps && not is_start_of_bigstep in
    (*Printf.printf "line=%d  istartbigstep=%d  ignore_step=%d\n"
      (match Flags.get_exit_line() with Some line -> line | None -> -1)
      (if is_start_of_bigstep then 1 else 0)
      (if ignore_step then 1 else 0);*)
    if not ignore_step then begin
      (* Processing of a regular step, which is not ignored by the [-only-big-steps flag] *)
      let execstats = last_stats_update() in
      let execstats_str = stats_to_string execstats in
      write_stats_log execstats_str;

      let exectime = int_of_float (execstats.stats_time) in

      (* Handle exit of script *)
      let should_exit =
        match Flags.get_exit_line() with
        | Some li -> (line > li)
        | _ -> false
        in
      if should_exit then begin
        if !Flags.analyse_stats then begin
          write_timing_log (Printf.sprintf "------------------------\n");
        end;
        dump_diff_and_exit();
      end else begin
        (* Handle reparse of code *)
        if reparse || (!Flags.reparse_at_big_steps && is_start_of_bigstep) then begin
          let info = if reparse then "the code on demand at" else "the code just before the big step at" in
          let _, reparse_stats = Stats.measure_stats (fun () -> reparse_alias ~info ()) in
          if !Flags.analyse_stats then
            let reparse_stats_str = Stats.stats_to_string reparse_stats in
            Stats.write_stats_log (Printf.sprintf "------------------------\nREPARSE: %s\n" reparse_stats_str );
        end;
        (* Handle the reporting of the script excerpt associated with the __next__ step, which starts on the line number reported *)
        if !Flags.analyse_stats then begin
          let descr = if line = -1 then "" else get_excerpt line in
          Stats.write_stats_log (Printf.sprintf "------------------------\n[line %d]\n%s\n" line descr);
        end;
        (* Handle progress report *)
        if is_start_of_bigstep && !Flags.report_big_steps then begin
          if line = -1 then begin
            incr id_big_step;
            Printf.printf "Executing bigstep #%d\n" !id_big_step
          end else begin
            Printf.printf "Executing bigstep at line %d\n" line
          end;
          flush stdout;
        end;
      end;
      (* Prepare the step description, handling immediate prior calls to [bigstep] *)
      let script = if !Flags.dump_trace && !line_of_last_step <> -1 then get_excerpt !line_of_last_step else "" in
      let stepdescr = { isbigstep; script; exectime } in
      nextstep_isbigstep := None; (* reset the nextstep_isbigstep field after use *)
      (* Save the step in the trace *)
      step stepdescr;
    end
  end;
  (* Update the line of the last step entered *)
  line_of_last_step := line


(* [!!]: is a prefix notation for the operation [check_exit_and_step].
   By default, it performs only [step]. The preprocessor of the OCaml script file
   can add the [line] argument to the call to [check_exit_and_step], in order
   to allow for checking the exit line. Concretely, if the user has the cursor
   one line N when invoking the Optitrust "view_diff" command, then the tool
   will display the difference between the state of the AST at the first "!!"
   that occurs strictly after line N, and the state at the previous "!!",
   which could be on line N or before (or could correspond to the input AST
   loaded by [Trace.init] if there is no preceeding '!!'.).
   Use [!!();] for a step in front of another language construct, e.g., a let-binding. *)
let (!!) (x:'a) : 'a =
  check_exit_and_step ~is_small_step:true ~reparse:false ();
  x

(* [!^]: is similar to [!!] but indicates the start of a big step in the transformation script. *)
(* WILL PROBABLY BE DEPREACTED IN THE FUTURE *)
let (!^) (x:'a) : 'a =
  check_exit_and_step ~is_small_step:false ~reparse:false ();
  x

(* [!!!]: is similar to [!!] but forces a [reparse] prior to the [step] operation.
   ONLY FOR DEVELOPMENT PURPOSE. *)
let (!!!) (x : 'a) : 'a =
  check_exit_and_step ~is_small_step:true ~reparse:true ();
  x

(* [!!^]: is forces reparse before a big step.
   ONLY FOR DEVELOPMENT PURPOSE. *)
let (!!^) (x : 'a) : 'a =
  check_exit_and_step ~is_small_step:false ~reparse:true ();
  x

(* [dump ~prefix]: invokes [output_prog] to write the contents of the current AST.
   If there are several traces (e.g., due to a [switch]), it writes one file for each.
   If the prefix is not provided, the input file basename is used as prefix,
   and in any case "_out" is appended to the prefix.

   If you use [dump] in your script, make sure to call [!! Trace.dump] with the
   prefix [!!] in order for the diff visualization to work well for the last
   command before the call to dump.

   WILL BE DEPRECATED: If the command line argument [-dump-trace] was provided, then the
   function writes all the ASTs from the history into javascript files. *)
(* LATER for mli: val dump : ?prefix:string -> unit -> unit *)
let dump ?(prefix : string = "") () : unit =
  if !Flags.analyse_stats then begin
      write_timing_log (Printf.sprintf "------------START DUMP------------\n");
  end;
  (* Dump final result, for every [switch] branch *)
  let ctx = the_trace.context in
  let prefix =
    if prefix = "" then ctx.directory ^ ctx.prefix else prefix
  in
  output_prog ctx (prefix ^ "_out") (the_trace.cur_ast)


(* [only_interactive_step line f]: invokes [f] only if the argument [line]
   matches the command line argument [-exit-line]. If so, it calls the
   [step] function to save the current AST, then calls [f] (for example
   to add decorators to the AST in the case of function [show]), then
   calls [dump_diff_and_exit] to visualize the effect of [f]. *)
let only_interactive_step (line : int) ?(reparse : bool = false) (f : unit -> unit) : unit =
  if (Flags.get_exit_line() = Some line) then begin
    if reparse
      then
        reparse_alias ();
    step stepdescr_for_interactive_step;
    f();
    dump_diff_and_exit()
  end
  else
    begin
    check_exit_and_step();
    f()
    end

(* [ast()]: returns the current ast; this function should only be called within the
   scope of a call to [Trace.apply] or [Trace.call]. For example:
   [Trace.call (fun t -> ...  let t = ast() in ...) ].
   Note that in most cases, this function is not needed because the argument of
   the continuation already describes the current AST as the variable [t]. *)
let ast () : trm =
   the_trace.cur_ast

(* [set_ast]: is used for implementing [iteri_on_transformed_targets]. Don't use it elsewhere.
   NOTE: INTERNAL FUNCTION. *)
let set_ast (t:trm) : unit =
  the_trace.cur_ast <- t

(* [get_context ()]: returns the current context. Like [ast()], it should only be called
   within the scope of [Trace.apply] or [Trace.call]. *)
let get_context () : context =
  the_trace.context


(* LATER:  need to reparse to hide spurious parentheses *)
(* LATER: add a mechanism for automatic simplifications after every step *)


(******************************************************************************)
(*                               Target aliases                               *)
(******************************************************************************)

let resolve_target (tg : target) : paths 
val resolve_target : target -> trm -> paths

let resolve_path (dl : path) (t : trm) : trm  =

val path_of_target_mark_one : mark -> trm -> path
