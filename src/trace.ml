open Ast
open Stats
open Tools
open PPrint


let debug = false

(******************************************************************************)
(*                             File excerpts                                  *)
(******************************************************************************)

(* [ml_file_excerpts]: maps line numbers to the corresponding sections in-between [!!] marks in
   the source file. Line numbers are counted from 1 in that map. *)
module Int_map = Map.Make(Int)
let ml_file_excerpts = ref Int_map.empty
let debug_compute_ml_file_excerpts = false

(* [compute_ml_file_excerpts lines]: is a function for grouping lines according to the [!!] symbols. *)
let compute_ml_file_excerpts (lines : string list) : string Int_map.t =
  let r = ref Int_map.empty in
  let start = ref 0 in
  let acc = Buffer.create 3000 in
  let push () =
    let s = Buffer.contents acc in
    let i = !start+1 in
    if debug_compute_ml_file_excerpts
      then printf "Excerpt[%d] = <<<%s>>>\n\n" i s;
    r := Int_map.add i s !r;
    Buffer.clear acc; in
  let regexp_let = Str.regexp "^[ ]*let" in
  let starts_with_let (str : string) : bool =
    Str.string_match regexp_let str 0 in
  (* match a line that starts with '!!' or 'bigstep' *)
  let regexp_step = Str.regexp "^[ ]*\\(!!\\|bigstep\\)" in
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


(******************************************************************************)
(*                             Logging management                             *)
(******************************************************************************)


(* [now()] returns the current time *)
let now () : float =
   Unix.gettimeofday()

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
let init_logs prefix =
  close_logs();
  let clog = open_out (prefix ^ ".log") in
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
  let msg = Printf.sprintf " -expression\n%s\n %s is a %s\n" (AstC_to_c.ast_to_string t) sloc exp_type in
 write_log clog msg

(******************************************************************************)
(*                             File input                                     *)
(******************************************************************************)

(* A parser should read a filename and return:
   - A header to copy in the produced file (typically a list of '#include' for C)
   - The OptiTrust AST of the rest of the file *)
(* TODO: encode header information in the AST *)
type parser = string -> string * trm

(* [parse ~parser filename]:
   call the parser on the given file while recording statistics *)
let parse ~(parser: parser) (filename : string) : string * trm =
  print_info None "Parsing %s...\n" filename;
  let parsed_file = stats ~name:"tr_ast" (fun () -> parser filename) in
  print_info None "Parsing Done.\n";
  parsed_file

(******************************************************************************)
(*                             Trace management                               *)
(******************************************************************************)

(* [context]: contains general information about:
   - the source code that was loaded initially using [set_init_file],
   - the prefix of the filenames in which to output the final result using [dump]
   - the log file to report on the transformation performed. *)
type context =
  { parser : parser;
    (* DEPRECATED?
       directory : string; *)
    mutable prefix : string; (* TODO: needs mutable? *)
    extension : string;
    header : string;
    clog : out_channel; }

(* [contex_dummy]: used for [trace_dummy]. *)
let context_dummy : context =
  { parser = (fun _ -> failwith "context_dummy has no parser");
    (* directory = ""; *)
    prefix = "";
    extension = "";
    header = "";
    clog = stdout; }

(* DEPRECATED [stepdescr]: description of a script step. *)
type stepdescr = {
  mutable isbigstep : string option; (* if the step is the beginning of a big step,
                                        then the value is [Some descr] *)
  mutable script : string; (* excerpt from the transformation script, or "" *)
  mutable exectime : int; } (* number of milliseconds, -1 if unknown *)



(* [step_kind] : classifies the kind of steps *)
type step_kind = Step_root | Step_big | Step_small | Step_transfo | Step_target_resolve | Step_io | Step_scoped | Step_aborted | Step_interactive | Step_error

(* [step_kind_to_string] converts a step-kind into a string *)
let step_kind_to_string (k:step_kind) : string =
  match k with
  | Step_root -> "Root"
  | Step_big -> "Big"
  | Step_small -> "Small"
  | Step_transfo -> "Transfo"
  | Step_target_resolve -> "Target"
  | Step_io -> "IO"
  | Step_scoped -> "Scoped"
  | Step_aborted -> "Aborted"
  | Step_interactive -> "Interactive"
  | Step_error -> "Error"

(* [step_infos] *)
type step_infos = {
  mutable step_script : string;
  mutable step_script_line : int option;
  mutable step_time_start : float; (* seconds since start *)
  mutable step_exectime : float; (* seconds *)
  mutable step_name : string;
  mutable step_args : (string * string) list;
  mutable step_valid : bool;
  mutable step_justif : string list;
  mutable step_tags : string list;
}

(* [step_tree]: history type used for storing all the trace information about all steps, recursively *)
type step_tree = {
  mutable step_kind : step_kind;
  mutable step_ast_before : trm;
  mutable step_ast_after : trm;
  mutable step_sub : step_tree list;
  (* substeps in reverse order during construction (between open and close) *)
  mutable step_infos : step_infos; }
  (* TODO: FOR PARSING STEPS int_of_float(stats_parse.stats_time); } *)


(* A [step_stack] is a stack that contains the currently opened steps,
   with the innermost at the top. The bottom element of the stack is
   always the one that describes the execution of the full transformation
   script. *)
type step_stack = step_tree list


(* [trace]: a record made of a context, a current AST, and a list of ASTs that were
   saved as "interesting intermediate steps", via the [Trace.save] function.
   Any call to the [step] function adds a copy of [cur_ast] into [history]. *)
type trace = {
  mutable context : context;
  mutable cur_ast : trm;
  mutable step_stack : step_stack; } (* stack of open steps *)

(* [trm_dummy]: dummy trm. *)
let trm_dummy : trm =
  trm_val (Val_lit Lit_unit)

(* [trace_dummy]: an trace made of dummy context and dummy trm,
   whose purpose is to enforce that [Trace.init] is called before any transformation *)
let trace_dummy : trace =
  { context = context_dummy;
    cur_ast = trm_dummy; (* dummy *)
    step_stack = []; (* dummy *)
    }

(* [the_trace]: the trace produced by the current script. *)
let the_trace : trace =
  trace_dummy

(* [is_trace_dummy()]: returns whether the trace was never initialized. *)
let is_trace_dummy () : bool =
  the_trace.context == context_dummy

(* [get_decorated_history]: gets history from trace with a few meta information *)
(* TODO: remove this function? *)
let get_decorated_history ?(prefix : string = "") () : string * context * step_tree =
  let ctx = the_trace.context in
  let prefix = (* LATER: figure out why we needed a custom prefix here *)
    if prefix = "" then ctx.prefix else prefix in
  let tree =
    match the_trace.step_stack with
    | [] -> failwith "step stack must never be empty"
    | [tree] -> tree
    | _ -> failwith "step stack contains more than one step; this should not be the case when a transformation script has completed"
    in
  (prefix, ctx, tree)

let dummy_exectime : float = 0.

(* [get_cur_step ()] returns the current step --there should always be one. *)
let get_cur_step ?(error : string = "get_cur_step: empty stack") () : step_tree =
  match the_trace.step_stack with
  | [] -> failwith error
  | step::_ -> step

(* [open_root_step] is called only by [Trace.init], for initializing
   the bottom element of the [step_stack].
   Assumes fields of [the_trace] have already been initialized. *)
let open_root_step ?(source : string = "<unnamed-file>") () : unit =
  assert(the_trace.step_stack = []);
  let step_root_infos = {
    step_script = "Contents of " ^ source;
    step_script_line = None;
    step_time_start = now();
    step_exectime = dummy_exectime;
    step_name = "Full script";
    step_args = [("extension", the_trace.context.extension) ];
    step_justif = [];
    step_valid = false;
    step_tags = [];
  } in
  let step_root = {
    step_kind = Step_root;
    step_ast_before = the_trace.cur_ast;
    step_ast_after = trm_dummy;
    step_sub = [];
    step_infos = step_root_infos; }
    in
  the_trace.step_stack <- [step_root]

(* [step_set_validity s] sets a computation to be valid if all its substeps are valid
   and form a contiguous chain *)
let step_set_validity (s : step_tree) : unit =
  let infos = s.step_infos in
  if not infos.step_valid then begin
    if List.for_all (fun sub -> sub.step_infos.step_valid) s.step_sub then begin
      let asts1: trm list = [s.step_ast_before] @
        (List.map (fun sub -> sub.step_ast_after) s.step_sub);
      in
      let asts2: trm list = (List.map (fun sub -> sub.step_ast_before) s.step_sub) @
        [s.step_ast_after]
      in
      (*printf "%s\n" (Trace_printers.list_arg_printer pointer_to_string asts1);
      printf "%s\n" (Trace_printers.list_arg_printer pointer_to_string asts2);*)
      if List.for_all2 (==) asts1 asts2 then begin
        infos.step_tags <- "valid_by_composition" :: infos.step_tags;
        infos.step_valid <- true
      end
    end
  end

(* [finalize_step] is called by [close_root_step] and [close_step] *)
let finalize_step (step : step_tree) : unit =
  let infos = step.step_infos in
  infos.step_exectime <- now() -. infos.step_time_start;
  infos.step_args <- List.rev infos.step_args;
  step.step_ast_after <- the_trace.cur_ast;
  step.step_sub <- List.rev step.step_sub;
  step_set_validity step

(* [get_root_step()] returns the root step, after close_root_step has been called *)
let get_root_step () : step_tree =
  match the_trace.step_stack with
  | [step] ->
      if step.step_ast_after == trm_dummy
        then failwith "get_root_step: close_root_step has not been called";
      step
  | _ -> failwith "close_root_step: broken invariant, stack must have size one"

(* [get_excerpt line]: returns the piece of transformation script that starts on the given line. Currently returns the ""
    in case [compute_ml_file_excerpts] was never called. LATER: make it fail in that case. *)
let get_excerpt (line : int) : string =
  if line = - 1 then sprintf "<get_excerpt for line -1>" else (*failwith "get_excerpt: requires a valid line number";*)
  if !ml_file_excerpts = Int_map.empty then "<get_excerpt: empty map>" else begin (* should "" be failure? *)
  match Int_map.find_opt line !ml_file_excerpts with
    | Some txt -> if txt <> "" then txt else sprintf "<get_excerpt: empty string mapped to line %d>" line
    | None -> (*LATER: failwith? *) sprintf "<get_excerpt: no binding for line %d>" line
  end

(* [open_step] is called at the start of every big-step, or small-step,
   or combi, or basic transformation. *)
let open_step ?(valid:bool=false) ?(line : int option) ?(step_script:string="") ?(tags:string list=[]) ~(kind:step_kind) ~(name:string) () : step_tree =
  let infos = {
    step_script;
    step_script_line = line;
    step_time_start = now();
    step_exectime = dummy_exectime;
    step_name = name;
    step_args = [];
    step_justif = [];
    step_valid = valid;
    step_tags = tags;
  } in
  let step = {
    step_kind = kind;
    step_ast_before = the_trace.cur_ast;
    step_ast_after = trm_dummy;
    step_sub = [];
    step_infos = infos; }
    in
  the_trace.step_stack <- step :: the_trace.step_stack;
  step

(* [step_justif txt] is called by a transformation after open_step in order
   to store explaination of why it is correct *)
let step_justif (justif:string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_valid <- true;
  infos.step_justif <- justif::infos.step_justif

(* [step_justif_always_correct()] is a specialized version of [step_justif]
   for transformation that are always correct. *)
let step_justif_always_correct () : unit =
  step_justif "always correct"

(* [step_arg] is called by a transformation after open_step in order
   to store the string representations of one argument. *)
let step_arg ~(name:string) ~(value:string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_args <- (name,value)::infos.step_args

(* [tag] is called by a transformation after open_step in order to associate a tag with itself. *)
let tag (s : string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_tags <- s :: infos.step_tags

(* [tag_trivial] is called by a transformation after open_step to indicate that it is trivial, or trivially explained by its substeps. *)
let tag_trivial () : unit =
  tag "trivial"

(* [tag_atomic] is called by a transformation after open_step to indicate that it is atomic, e.g. looking at its substeps does not explain why it is correct. *)
let tag_atomic () : unit =
  tag "atomic"

(* [tag_valid_by_composition] is called by a transformation after open_step to indicate that it should be valid by composition. This can be used for filtering trace display or checking that it is indeed valid by composition. *)
let tag_valid_by_composition () : unit =
  tag "should_be_valid_by_composition"

(* [tag_simpl_arith] is called by a transformation after open_step to indicate that it performs arithmetic simplifications. This can be used for filtering trace display. *)
let tag_simpl_arith () : unit =
  tag "simpl";
  tag "simpl_arith"

(* [close_step] is called at the end of every big-step, or small-step,
   or combi, or basic transformation. The step to close can be passed
   as an optional argument, to check that the exected step is being closed.
   If all substeps are valid and their sequence explains how to go from ast_before to ast_after, the step is valid by the explaination "combination of valid steps" *)
let close_step ?(check:step_tree option) () : unit =
  match the_trace.step_stack with
  | [] -> failwith "close_step: the_trace should not be empty"
  | [root_step] -> failwith "close_step: on the root, should call close_root_step"
  | step :: ((parent_step :: _) as stack_tail)  ->
      (* Checking that swe close the expected step *)
      begin match check with
      | None -> ()
      | Some opened_step ->
          if step != opened_step
            then failwith "close_step: not closing the expected step"
      end;
      (* Finalize the step, by reversing the list of substeps and computing validity *)
      finalize_step step;
      (* Folding step into parent substeps *)
      parent_step.step_sub <- step :: parent_step.step_sub;
      the_trace.step_stack <- stack_tail

(* [close_step_kind_if_needed k] is used by
   [close_smallstep_if_needed] and [close_bigstep_if_needed] *)
let close_step_kind_if_needed (k:step_kind) : unit =
  let step = get_cur_step() in
  if step.step_kind = k then close_step()

(* [close_smallstep_if_needed()] closes a current big-step.
   Because big-steps are not syntactically scoped in the user script,
   we need such an implicit close operation to be called on either
   the opening of a new big-step, or on closing of the root step. *)
let close_smallstep_if_needed () : unit =
  close_step_kind_if_needed Step_small

(* [close_bigstep_if_needed()] closes a current big-step.
   Because big-steps are not syntactically scoped in the user script,
   we need such an implicit close operation to be called on either
   the opening of a new big-step, or on closing of the root step. *)
let close_bigstep_if_needed () : unit =
  close_smallstep_if_needed();
  close_step_kind_if_needed Step_big

(* [close_root_step] is called only by [Run. TODO???] at the end of the script,
   or at the end of the small-step targeted by the user.
   It leaves the root step at the bottom of the stack *)
let close_root_step () : unit =
  close_bigstep_if_needed();
  let step = match the_trace.step_stack with
    | [step] -> step
    | _ -> failwith "close_root_step: broken invariant, stack must have size one" in
  finalize_step step


(* [step] is a function wrapping the body of a transformation *)
let step ?(valid:bool=false) ?(line : int = -1) ?(tags:string list=[]) ~(kind:step_kind) ~(name:string) (body : unit -> 'a) : 'a =
  let s = open_step ~valid ~line ~tags ~kind ~name () in
  let r = body () in
  assert (get_cur_step () == s);
  close_step ~check:s ();
  r

(* [scoped_step] opens a scope to perform transformations in.
  At the end:
  - closes all scope-local steps automatically if not already done
  - restores the current AST to what it was before the scope *)
let scoped_step ~(kind : step_kind) (f : unit -> unit) : unit =
  let ast_bak = the_trace.cur_ast in
  let s = open_step ~kind ~name:"" () in
  f ();
  let error = "Trace.scoped_step: did not find 's'" in
  while (get_cur_step ~error () != s) do
    close_step ()
  done;
  the_trace.cur_ast <- ast_bak;
  close_step ~check:s ()

type backtrack_result =
| Success
| Failure of exn

let backtrack_on_failure (f : unit -> unit) : backtrack_result =
  let ast_bak = the_trace.cur_ast in
  let s = open_step ~kind:Step_scoped ~name:"" () in
  let res =
    try
      f (); Success
    with e -> begin
      let error = "Trace.backtrack_on_failure: did not find 's'" in
      while (get_cur_step ~error () != s) do
        close_step ()
      done;
      s.step_kind <- Step_aborted;
      the_trace.cur_ast <- ast_bak;
      Failure e
    end in
  assert (get_cur_step () == s);
  close_step ~check:s ();
  res

(* [parsing_step f] adds a step accounting for a parsing operation *)
let parsing_step (f : unit -> unit) : unit =
  step ~valid:true ~kind:Step_io ~name:"Parsing" ~tags:["IO"] f

(* [dumping_step f] adds a step accounting for a parsing operation *)
let dumping_step (f : unit -> unit) : unit =
  step ~valid:true ~kind:Step_io ~name:"Dumping" ~tags:["IO"] f

(* [error_step f] adds a step accounting for a fatal error *)
let error_step (error : string) : unit =
  step ~valid:false ~kind:Step_error ~name:error (fun () -> ())

(* [open_target_resolve_step] *)
let open_target_resolve_step () : unit =
  ignore (open_step ~valid:true ~kind:Step_target_resolve ~tags:["target"] ~name:"" ())

(* [close_target_resolve_step] has a special handling because it saves a diff
   between an AST and an AST decorated with marks for targeted paths,
   even though the [cur_ast] is not updated with the marsk *)
let close_target_resolve_step (ps:Path.path list) (t:trm) : unit =
  if !Flags.dump_trace then begin
    let marked_ast, _marks = Path.add_marks_at_paths ps t in
    let cur_ast = the_trace.cur_ast in
    the_trace.cur_ast <- marked_ast;
    close_step();
    the_trace.cur_ast <- cur_ast
  end else
    close_step()

(* [invalidate()]: restores the global state (object [trace]) in its uninitialized state,
   like at the start of the program.  *)
let invalidate () : unit =
  close_logs();
  the_trace.context <- trace_dummy.context;
  the_trace.cur_ast <- trace_dummy.cur_ast;
  the_trace.step_stack <- trace_dummy.step_stack

(* [get_initial_ast ~parser ser_mode ser_file filename]: gets the initial ast before applying any trasformations
     [parser] - choose which parser to use for parsing the source code
     [ser_mode] - serialization mode
     [ser_file] - if serialization is used for the initial ast, the filename of the serialized version
                  of the source code is needed
     [filename] - filename of the source code  *)
let get_initial_ast ~(parser : parser) (ser_mode : Flags.serialization_mode) (ser_file : string)
  (filename : string) : (string * trm) =
  (* LATER if ser_mode = Serialized_Make then let _ = Sys.command ("make " ^ ser_file) in (); *)
  let ser_file_exists = Sys.file_exists ser_file in
  let ser_file_more_recent = if (not ser_file_exists) then false else Xfile.is_newer_than ser_file filename in
  let auto_use_ser = (ser_mode = Serialized_Auto && ser_file_more_recent) in
  if (ser_mode = Serialized_Use (* || ser_mode = Serialized_Make *) || auto_use_ser) then (
    if not ser_file_exists
      then fail None "Trace.get_initial_ast: please generate a serialized file first";
    if not ser_file_more_recent
      then fail None (Printf.sprintf "Trace.get_initial_ast: serialized file is out of date with respect to %s\n" filename);
    let ast = Xfile.unserialize_from ser_file in
    if auto_use_ser
      then Printf.printf "Loaded ast from %s.\n" ser_file;
    ast
  )
  else
    parse ~parser filename

(* [init f]: initializes the trace with the contents of the file [f].
   This operation should be the first in a transformation script.
   The history is initialized with the initial AST.
   [~prefix:"foo"] allows to use a custom prefix for all output files,
   instead of the basename of [f]. *)
(* LATER for mli: val set_init_source : string -> unit *)
let init ?(prefix : string = "") ~(parser: parser) (filename : string) : unit =
  invalidate ();
  let basename = Filename.basename filename in
  let extension = Filename.extension basename in
  let default_prefix = Filename.remove_extension filename in
  let ml_file_name =
    if Tools.pattern_matches "_inlined" default_prefix
      then List.nth (Str.split (Str.regexp "_inlined") default_prefix) 0
      else default_prefix in
  if !Flags.analyse_stats || !Flags.dump_trace then begin
    let src_file = (ml_file_name ^ ".ml") in
    if Sys.file_exists src_file then begin
      let lines = Xfile.get_lines src_file in
      (* printf "%s\n" (Tools.list_to_string ~sep:"\n" lines); *)
      ml_file_excerpts := compute_ml_file_excerpts lines;
    end;
  end;
  let mode = !Flags.serialization_mode in
  start_stats := get_cur_stats ();
  last_stats := !start_stats;

  let prefix = if prefix = "" then default_prefix else prefix in
  let clog = init_logs prefix in
  let ser_file = basename ^ ".ser" in

  let (header, cur_ast), stats_parse = Stats.measure_stats (fun () -> get_initial_ast ~parser mode ser_file filename) in

  let context = { parser; extension; prefix; header; clog } in
  the_trace.context <- context;
  the_trace.cur_ast <- cur_ast;
  the_trace.step_stack <- [];
  open_root_step ~source:ml_file_name ();

  if mode = Serialized_Build || mode = Serialized_Auto
    then Xfile.serialize_to ser_file (header, cur_ast);
  if mode = Serialized_Build
    then exit 0;
  print_info None "Starting script execution...\n"

(* [finalize()]: should be called at the end of the script to close the root step *)
let finalize () : unit =
  close_root_step()

(* [finalize_on_error()]: performs a best effort to close all steps after an error occurred *)
let finalize_on_error ~(error:string) : unit =
  let rec close_all_steps () : unit =
    match the_trace.step_stack with
    | [] -> failwith "close_close_all_stepsstep: the_trace should not be empty"
    | [_root_step] -> error_step error; finalize()
    | _step :: _ -> close_step(); close_all_steps()
    in
  close_all_steps()

(* [get_last_substep] returns the last substep, which corresponds to the
    step to be visualized by a diff *)
let get_last_substep () : step_tree =
  match (get_cur_step ()).step_sub with
  | [] -> failwith "Trace.get_last_substep: expected a previous substep in the current step"
  | last_step :: _ -> last_step

(* [get_original_ast] returns the ast obtained at [Trace.init] *)
let get_original_ast () : trm =
  assert (the_trace.step_stack <> []);
  let (_, root_step) = Xlist.unlast the_trace.step_stack in
  root_step.step_ast_before

(* [alternative f]: executes the script [f] in the original state that
  was available just after the call to [init].
  After the call, all the actions performed are discarded.

  Current usage:
     !! Trace.alternative (fun () ->
        !! Loop.fusion_on_block [cLabel "tofusion"];
     );
  TODO: deprecate this and use trace.reset instead
*)
let alternative (f : unit->unit) : unit =
  let ast = get_original_ast () in
  scoped_step ~kind:Step_aborted (fun () ->
    the_trace.cur_ast <- ast;
    f();
  )

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

(* FIXME: where should [failure_expected] and [alternative] be defined? *)
exception Failure_expected_did_not_fail

(* [failure_expected f]: executes the unit function [f], and checks that
   it raises the exception [Failure_expected_did_not_fail]. If it does
   not, then an error is triggered. *)
let failure_expected (f : unit -> unit) : unit =
  scoped_step ~kind:Step_aborted (fun () ->
    try
      f();
      raise Failure_expected_did_not_fail
    with
      | Failure_expected_did_not_fail -> failwith "failure_expected: the operation was supposed to fail but it didn't"
      |_ -> ()
  )

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

(* [reset] performs a step that sets the current ast to the original ast *)
let reset () : unit =
  let t = get_original_ast () in
  apply (fun _cur_ast -> t)

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


(* [get_header ()]: get the header of the current file (e.g. include directives) *)
let get_header () : string =
  the_trace.context.header

(* [ensure_header]: ensures that the header [h] is included in the header of the current file. *)
(* FIXME: does not show in diff this way *)
let ensure_header (h : string) : unit =
  let ctx = the_trace.context in
  let found = Tools.pattern_matches h (ctx.header) in
  if not found then
    the_trace.context <- { ctx with header = ctx.header ^ h ^ "\n" }

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
    output_string out_prog ctx.header;
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
      output_string out_enc ctx.header;
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

(******************************************************************************)
(*                                   Reparse                                  *)
(******************************************************************************)

(* [reparse_trm ctx ast]: prints [ast] in a temporary file and reparses it using Clang. *)
let reparse_trm ?(info : string = "") ?(parser: parser option) (ctx : context) (ast : trm) : trm =
  if !Flags.debug_reparse then begin
    let info = if info <> "" then info else "of a term during the step starting at" in
    Printf.printf "Reparse: %s.\n" info;
    flush stdout
  end;
  let in_prefix = (Filename.dirname ctx.prefix) ^ "/tmp_" ^ (Filename.basename ctx.prefix) in
  output_prog ~beautify:false ctx in_prefix ast;

  let parser =
    match parser with
    | Some p -> p
    | None -> ctx.parser
  in

  let (_, t) = parse ~parser (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

(* [reparse ()]: function takes the current AST, prints it to a file, and parses it
   as if it was a fresh input. Doing so ensures in particular that all the type
   information is properly set up. WARNING: reparsing discards all the marks in the AST. *)
let reparse ?(update_cur_ast : bool = true) ?(info : string = "") ?(parser: parser option) () : unit =
  parsing_step (fun () ->
    let info = if info <> "" then info else "the code during the step starting at" in
    let tnew = reparse_trm ~info ?parser the_trace.context the_trace.cur_ast in
    if update_cur_ast
      then the_trace.cur_ast <- tnew
  )

(* Work-around for a name clash *)
let reparse_alias = reparse


(******************************************************************************)
(*                                   More dump                                *)
(******************************************************************************)


(* [dump_steps]: writes into files called [`prefix`_$i_out.cpp] the contents of each of the big steps,
    where [$i] denotes the index of a big step. *)
let dump_steps ?(onlybig : bool = false) ?(prefix : string = "") (foldername : string) : unit =
  ()
  (* TODO FIXME
  ignore (Sys.command ("mkdir -p " ^ foldername));
  let (prefix, ctx, hist_and_descr) = get_decorated_history ~prefix () in

  (* TODO: modify this code *)
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
  *)

(* TODO: move  *)
let cmd s =
  (* FOR DEBUG Printf.printf "execute: %s\n" s; flush stdout; *)
  ignore (Sys.command s)

(* [trace_custom_postprocessing] is a function applied to all ast-after that are dumped in the trace;
   for debugging purposes only *)
let trace_custom_postprocessing : (trm -> trm) ref = ref (fun t -> t)

(* EXAMPLE POSTPROCESSING: display the type of every statement;
   place this definition at the top of your script.

      let _ = Trace.trace_custom_postprocessing := (fun t ->
        let tg = [nbAny; cPred (fun ti -> ti.is_statement)] in
        let ps = resolve_target tg t in
        let markof _pi ti =
          match ti.typ with
          | Some ty -> AstC_to_c.typ_to_string ty
          | None -> "-" in
        Target.trm_add_mark_at_paths markof ps t)

  Example: show the address of each AST
  let _ = Trace.trace_custom_postprocessing := (fun t ->
    let markof _pi ti = Tools.pointer_to_string ti in
    Target.trm_add_mark_at_paths markof [[]] t
    )
*)


(* LATER: optimize script to avoid computing twice the same ASTs for step[i].after and step[i+1].after *)
(* [dump_step_tree_to_js] auxiliary function for [dump_trace_to_js] *)
let rec dump_step_tree_to_js ~(is_substep_of_targeted_line:bool) ?(beautify : bool = false) (get_next_id:unit->int) (out:string->unit) (id:int) (s:step_tree) : unit =
  let i = s.step_infos in
  (* Report diff and AST for details *)
  let is_smallstep_of_targeted_line =
    (i.step_script_line <> Some (-1)) && (* LATER: use options *)
    (i.step_script_line = Some !Flags.trace_details_only_for_line) in
  let is_substep_of_targeted_line =
      is_substep_of_targeted_line || is_smallstep_of_targeted_line in
  let details =
        (!Flags.trace_details_only_for_line = -1) (* details for all steps *)
     || s.step_kind = Step_big
     || s.step_kind = Step_small
     || is_substep_of_targeted_line
     in
  (* Recursive calls *)
  let aux = dump_step_tree_to_js ~is_substep_of_targeted_line ~beautify get_next_id out in
  (* LATER: move these functions elsewhere? *)
  let compute_command_base64 (s : string) : string =
    cmd (sprintf "%s | base64 -w 0 > tmp.base64" s);
    Xfile.get_contents ~newline_at_end:false "tmp.base64"
    in
  (* Assign ids to sub-steps *)
  let sub_ids = List.map (fun _ -> get_next_id()) s.step_sub in
  (* Dump Json for this node *)
  let ctx = the_trace.context in
  let sBefore, sAfter, sDiff =
    if details then begin
      let ast_before = !trace_custom_postprocessing s.step_ast_before in
      let ast_after = !trace_custom_postprocessing s.step_ast_after in
      output_prog ~beautify ctx "tmp_before" ast_before;
      output_prog ~beautify ctx "tmp_after" ast_after;
      let sBefore = compute_command_base64 "cat tmp_before.cpp" in
      let sAfter = compute_command_base64 "cat tmp_after.cpp" in
      let sDiff = compute_command_base64 "git diff --ignore-all-space --no-index -U10 tmp_before.cpp tmp_after.cpp" in
      sBefore, sAfter, sDiff
    end else begin
      "", "", ""
    end in
  let json =
    Json.obj_quoted_keys [
      "id", Json.int id;
      "kind", Json.str (step_kind_to_string s.step_kind);
      "exectime", Json.float i.step_exectime;
      "name", Json.str i.step_name;
      "script", Json.base64 (Base64.encode_exn i.step_script);
      "script_line", Json.(optionof int) (if i.step_script_line = Some (-1) then None else i.step_script_line);
        (* TODO: avoid use of -1 for undef line *)
      "args", Json.(listof (fun (k,v) -> Json.obj_quoted_keys ["name", str k; "value",str v])) i.step_args;
      "isvalid", Json.bool i.step_valid;
        (* TODO: at the moment, we assume that a justification item means is-valid *)
      "justif", Json.(listof str) i.step_justif;
      "tags", Json.(listof str) i.step_tags;
      "sub", Json.(listof int) sub_ids;
      "ast_before", Json.base64 sBefore;
      "ast_after", Json.base64 sAfter;
      "diff", Json.base64 sDiff;
    ] in
  out (sprintf "steps[%d] = %s;\n" id (Json.to_string json));
  (* If this step is the targeted step, mention it as such *)
  if is_smallstep_of_targeted_line
    then out (sprintf "var startupOpenStep = %d;\n" id);
  (* Process sub-steps recursively *)
  List.iter2 aux sub_ids s.step_sub


(* [dump_trace_to_js]: writes into a file called [`prefix`_trace.js] the
   contents of the step_tree. The JS file is structured as follows
   (up to the order of the definitions):

   var startupOpenStep = 45; // optional binding
   var steps = [];
   steps[i] = {
      id: i,
      kind: "..",
      exectime: 0.0453;   // in seconds
      name: "..",
      args: [ { name: "..", value: ".."}, { name: "..", value: ".." } ],
      justif: ["..", ".." ],
      script: window.atob("..."),
      scriptline: 23, // possibly undefined
      astBefore: window.atob("..."), // NOT YET IMPLEMENTED; could also an id of an source code stored in a different array, for improved factorization
      astAfter: window.atob("..."), // NOT YET IMPLEMENTED
      diff: window.atob("..."), // could be slow if requested for all!
      sub: [ j1, j2, ... jK ]  // ids of the sub-steps
      }
   *)
let dump_trace_to_js ?(beautify : bool = false) ?(prefix : string = "") () : unit =
  let prefix =
    if prefix = "" then the_trace.context.prefix else prefix in
  let filename = prefix ^ "_trace.js" in
  if debug then printf "Dumping trace to '%s'\n" filename;
  let out_js = open_out filename in
  let out = output_string out_js in
  let step_tree = get_root_step() in
  let next_id = ref (-1) in
  let get_next_id () : int =
    incr next_id;
    !next_id in
  out "var steps = [];\n";
  let idroot = get_next_id() in
  dump_step_tree_to_js ~is_substep_of_targeted_line:false ~beautify get_next_id out idroot step_tree;
  (* Clean up the files generated by the functions located in dump_step_tree_to_js_and_return_id
     LATER: move this elsewhere *)
  cmd "rm -f tmp.base64 tmp_after.cpp tmp_before.cpp";
  close_out out_js

(* [step_tree_to_doc step_tree] takes a step tree and gives a string
   representation of it, using indentation to represent substeps *)
let step_tree_to_doc (step_tree:step_tree) : document =
  let ident_width = 3 in
  let rec aux (depth:int) (s:step_tree) : document =
    let i = s.step_infos in
    let space = blank 1 in
    let tab = blank (depth * ident_width) in
       tab
    ^^ string (step_kind_to_string s.step_kind)
    ^^ space
    ^^ string (sprintf "(%dms)" (int_of_float (i.step_exectime *. 1000.)))
    ^^ space
    ^^ string i.step_name
    ^^ concat_map (fun (k,v) -> space ^^ string (if k = "" then v else sprintf "~%s:%s" k v)) i.step_args
    ^^ (if i.step_justif = [] then empty else concat_map (fun txt -> hardline ^^ tab ^^ string "==> " ^^ string txt) i.step_justif)
    ^^ (if i.step_script = "" then empty else (*hardline ^^ tab ^^*) string ">> " ^^ (string i.step_script))
    ^^ hardline
    ^^ concat_map (aux (depth+1)) s.step_sub
    in
  aux 0 step_tree

(* [step_tree_to_file filename step_tree] takes a step tree and writes
   its string representation into a file. *)
let step_tree_to_file (filename:string) (step_tree:step_tree) =
  let line_width = 500 in
  let out = open_out filename in
  ToChannel.pretty 0.9 line_width out (step_tree_to_doc step_tree);
  close_out out

(* [dump_trace_to_textfile] dumps a trace into a text file *)
let dump_trace_to_textfile ?(prefix : string = "") () : unit =
  let prefix =
    if prefix = "" then the_trace.context.prefix else prefix in
  let filename = prefix ^ "_trace.txt" in
  if debug then printf "Dumping trace to '%s'\n" filename;
  step_tree_to_file filename (get_root_step())


(* [output_prog_check_empty ~ast_and_enc ctx prefix ast_opt]: similar to [output_prog], but it
   generates an empty file in case the [ast] is an empty ast. *)
let output_prog_check_empty ?(ast_and_enc : bool = true) (ctx : context) (prefix : string) (ast_opt : trm) : unit =
  match ast_opt.desc with
  | Trm_seq tl when Mlist.length tl <> 0 -> output_prog ~ast_and_enc ctx prefix ast_opt
  | _ ->
      let file_prog = prefix ^ ctx.extension in
      let out_prog = open_out file_prog in
      close_out out_prog


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
   small-step or big-step from the script. The diff is computed by taking the step
   at the top of the stack [the_trace], and considering the diff of the last sub-step
   the was performed. *)
(* LATER for mli: dump_diff_and_exit : unit -> unit *)
let dump_diff_and_exit () : unit =
  print_info None "Exiting script\n";
  let trace = the_trace in
  let ctx = trace.context in
  let prefix = (* ctx.directory ^ *) ctx.prefix in
  (* Common printinf function *)
  let output_ast ?(ast_and_enc:bool=true) filename_prefix ast =
    output_prog_check_empty ~ast_and_enc ctx filename_prefix ast;
    print_info None "Generated: %s%s\n" filename_prefix ctx.extension;
    in
  (* Extrat the two ASTs that should be used for the diff *)
  let step = get_cur_step() in
  let kind = step.step_kind in
  if kind <> Step_root && kind <> Step_big && kind <> Step_scoped
    then failwith (sprintf "dump_diff_and_exit: expects the current step to be Root, Big or Scoped, found %s" (step_kind_to_string kind));
  if (get_cur_step ()).step_sub = []
    then failwith "dump_diff_and_exit: no step was recorded; the script should start with '!!' or 'bigstep'";
  let last_step = get_last_substep () in
  if !Flags.only_big_steps && last_step.step_kind <> Step_big
    then failwith "dump_diff_and_exit: cannot show a diff for a big-step, no call to bigstep was made";
  let astBefore, astAfter =
    last_step.step_ast_before, last_step.step_ast_after
    in

  (* Option to compute light-diff:
      hide the bodies of functions that are identical in astBefore and astAfter. *)
  let astBefore, astAfter =
    if !Flags.use_light_diff
      then light_diff astBefore astAfter
      else astBefore, astAfter in

  (* Generate files *)
  output_ast (prefix ^ "_before") astBefore;
  output_ast (prefix ^ "_after") astAfter;
  print_info None "Writing ast and code into %s.js " prefix;
  (* Exit *)
  close_logs ();
  exit 0

(* [check_exit ~line] checks whether the program execution should be interrupted based
   on the command line argument [-exit-line]. If so, it exists the program after dumping
   the diff. *)
let check_exit ~(line:int) : unit (* may not return *) =
  let should_exit = match Flags.get_exit_line() with
    | Some li -> (line > li)
    | _ -> false
    in
  if should_exit
     then dump_diff_and_exit()

(* [check_exit_at_end] is called by [run.ml] when reaching the end of the script.
   This special case is needed to display a diff for the last transformation of
   the script. Indeed, this last transformation is not followed by a [!!] or a
   [bigstep] call.
   LATER: we may want to check that the targeted line is before the closing
   parenthesis of the call to script_cpp, but this requires instrumenting the
   call to script_cpp, and obtaining the end position of this construction. *)
let check_exit_at_end () : unit (* may not return *) =
  let should_exit = (Flags.get_exit_line () <> None) in
  if should_exit then begin
    close_smallstep_if_needed();
    if !Flags.only_big_steps
      then close_bigstep_if_needed();
    dump_diff_and_exit()
  end


(******************************************************************************)
(*                                   Steps                                     *)
(******************************************************************************)


(* [open_bigstep s]: announces that the next step is a bigstep, and registers
   a string description for that step. The [close_bigstep] is implicitly handled. *)
let open_bigstep ~(line : int) (title:string) : unit =
  (* The [check_exit] is performed after closing the last small-step or big-step,
    depending on whether the user is interested in a diff over the last big-step. *)
  close_smallstep_if_needed();
  if not !Flags.only_big_steps
    then check_exit ~line;
  close_bigstep_if_needed();
  if !Flags.only_big_steps
    then check_exit ~line;
  (* Reparse if needed *)
  if !Flags.reparse_at_big_steps
    then reparse_alias ();
  ignore (open_step ~kind:Step_big ~name:"" ~step_script:title ~line ());
  (* Handle progress report *)
  if !Flags.report_big_steps then begin
    Printf.printf "Executing bigstep %s%s\n"
      (if line <> -1 then sprintf "at line %d" line else "")
      title
  end

(* [open_smallstep s]: announces that the next step is a smallstep,
   and registers a string description for that step, based on the excerpt
   frmo the file. The [close_smallstep] is implicitly handled. *)
(* LATER: add the line argument in the generation of the _with_lines file *)
let open_smallstep ~(line : int) ?(reparse:bool=false) () : unit =
  close_smallstep_if_needed();
  if not !Flags.only_big_steps
    then check_exit ~line;
  if reparse
    then reparse_alias();
  let step_script =
    if !Flags.dump_trace
      then get_excerpt line
      else ""
    in
  ignore (open_step ~kind:Step_small ~name:"" ~line ~step_script ())

(* [interactive_step] is used to implement functions such as [show_ast] to show a target,
   or [show_encoding] or [show_ast], etc. It takes as argument a function describing
   the action to perform when the user cursor is on the line of the operation.
   It also accept an optional argument for an action to perform in other cases. *)
let interactive_step ~(line:int) ~(ast_before:unit->trm) ~(ast_after:unit->trm) : unit =
  let should_exit = (Flags.get_exit_line() = Some line) in
  if should_exit then begin
    close_smallstep_if_needed ();
    let s = open_step ~line ~kind:Step_interactive ~name:"show" () in
    close_step ~check:s ();
    (* Overwrite the ast_before and ast_after *)
    s.step_ast_before <- ast_before();
    s.step_ast_after <- ast_after();
    dump_diff_and_exit ()
  end

(* [show_step] is used to implement [Target.show].
   It takes as argument a function describing
   the action to perform when the user cursor is on the line of the operation.
   It also accept an optional argument for an action to perform in other cases. *)
let show_step ~(line:int) ~(interactive_action: unit->unit) ?(action_otherwise:unit->unit=(fun()->())) () : unit =
  let should_exit = (Flags.get_exit_line() = Some line) in
  let batch_mode = Flags.is_batch_mode() in
  if should_exit || (!Flags.execute_show_even_in_batch_mode && batch_mode) then begin
    close_smallstep_if_needed ();
    let s = open_step ~name:"show" ~kind:Step_interactive ~line () in
    interactive_action ();
    close_step ~check:s ();
    if should_exit
      then dump_diff_and_exit ()
  end else begin
    action_otherwise();
  end

(* [transfo_step] is the function that is produced by a [let%transfo]. It performs a step,
   and set the name and the arguments of the step. *)
let transfo_step ~(name : string) ~(args : (string * string) list) (f : unit -> unit) : unit =
  step ~kind:Step_transfo ~name (fun () ->
    (* printf "> %s\n" name; *)
    List.iter (fun (k, v) -> step_arg ~name:k ~value:v) args;
    f ();
    (* printf "< %s\n" name; *)
  )

(* [check_recover_original()]: checks that the AST obtained so far
   is identical to the input AST, obtained from parsing. If not,
   it raises an error. *)
let check_recover_original () : unit =
  let check_same ast1 ast2 =
    if AstC_to_c.ast_to_string ast1 <> AstC_to_c.ast_to_string ast2
      then fail None "Trace.check_recover_original: the current AST is not identical to the original one."
      else () (* FOR DEBUG: Printf.printf "check_recover_original: successful" *)
    in
  let orig_ast = get_original_ast () in
  call (fun cur_ast -> check_same cur_ast orig_ast)



(******************************************************************************)
(*                                   User-level fucntions                     *)
(******************************************************************************)

  (* TODO: INTEGRATE Special hack for minimizing diff in documentation
  if !Flags.documentation_save_file_at_first_check <> "" then begin
    let trace = the_trace in
    let ctx = trace.context in
    output_prog ctx !Flags.documentation_save_file_at_first_check (trace.cur_ast)
 *)

(* [!!]: is a prefix notation for the operation [open_smallstep].
   By default, it performs only [step]. The preprocessor of the OCaml script file
   can add the [line] argument to the call to [open_smallstep], in order
   to allow for checking the exit line. Concretely, if the user has the cursor
   one line N when invoking the Optitrust "view_diff" command, then the tool
   will display the difference between the state of the AST at the first "!!"
   that occurs strictly after line N, and the state at the previous "!!",
   which could be on line N or before (or could correspond to the input AST
   loaded by [Trace.init] if there is no preceeding '!!'.).
   Use [!!();] for a step in front of another language construct, e.g., a let-binding. *)
let (!!) (x:'a) : 'a =
  open_smallstep ~line:(-1) ~reparse:false ();
  x

(* [!!!]: is similar to [!!] but forces a [reparse] prior to the [step] operation.
   ONLY FOR DEVELOPMENT PURPOSE. *)
let (!!!) (x : 'a) : 'a =
  open_smallstep ~line:(-1) ~reparse:true ();
  x

(* [bigstep s]: an alias for [open_bigstep s], for usage in user scripts. *)
let bigstep (s : string) : unit =
  open_bigstep ~line:(-1) s

(* [dump ~prefix]: invokes [output_prog] to write the contents of the current AST.
   If there are several traces (e.g., due to a [switch]), it writes one file for each.
   If the prefix is not provided, the input file basename is used as prefix,
   and in any case "_out" is appended to the prefix.

   If you use [dump] in your script, make sure to call [!! Trace.dump] with the
   prefix [!!] in order for the diff visualization to work well for the last
   command before the call to dump. *)
let dump ?(prefix : string = "") () : unit =
  dumping_step (fun () ->
    let ctx = the_trace.context in
    let prefix =
      if prefix = "" then (* ctx.directory ^ *) ctx.prefix else prefix in
    output_prog ctx (prefix ^ "_out") (the_trace.cur_ast)
  )

(* DEPRECATED? [only_interactive_step line f]: invokes [f] only if the argument [line]
   matches the command line argument [-exit-line]. If so, it calls the
   [step] function to save the current AST, then calls [f] (for example
   to add decorators to the AST in the case of function [show]), then
   calls [dump_diff_and_exit] to visualize the effect of [f].
let only_interactive_step (line : int) ?(reparse : bool = false) (f : unit -> unit) : unit =
  let stepdescr_for_interactive_step =
    { isbigstep = None; script = ""; exectime = 0; } in
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
    *)

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
