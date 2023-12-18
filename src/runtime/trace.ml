open Ast
open Trm
open Typ
open Mark
open Stats
open Tools
open PPrint

(** [output_style] describes the mode in which an AST should be pretty-printed *)
type output_style = Style.custom_style


let debug = false

(* [check_trace_at_every_step] can be activated to call the function
   [check_trace] after every step, to check the invariants of the
   trace data structure, which stores the stack of open steps. *)
let check_trace_at_every_step = false


(******************************************************************************)
(*                             File excerpts                                  *)
(******************************************************************************)

(** [ml_file_excerpts]: maps line numbers to the corresponding sections in-between [!!] marks in
   the source file. Line numbers are counted from 1 in that map. *)
module Int_map = Map.Make(Int)
let ml_file_excerpts = ref Int_map.empty
let debug_compute_ml_file_excerpts = false

(** [compute_ml_file_excerpts lines]: is a function for grouping lines according to the [!!] symbols. *)
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


(** [now()] returns the current time *)
let now () : float =
   Unix.gettimeofday()

(** [timing_log_handle]: is a handle on the channel for writing timing reports. *)
let timing_log_handle = ref None

(** [stats_log_handle]: is a handle on the channel for writing stats reports. *)
let stats_log_handle = ref None

(** [logs]: is a reference on the list of open log channels. *)
let logs : (out_channel list) ref = ref []

(** [close_logs]: closes all open log channels. *)
let close_logs () : unit =
  List.iter (fun log -> close_out log) !logs;
  logs := []

(** [init_logs]: initializes the log files. It closes any existing logs.
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

(** [write_log clog msg]: writes the string [msg] to the channel [clog]. *)
let write_log (clog : out_channel) (msg : string) : unit =
  output_string clog msg;
  flush clog

(** [trm_to_log clog styp t]: writes in the channel [clog] the term [t],
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

(** A parser should read a filename and return:
   - A header to copy in the produced file (typically a list of '#include' for C)
   - The OptiTrust AST of the rest of the file *)
(* TODO: encode header information in the AST *)
type parser = string -> string * trm

(** [parse ~parser filename]:
   call the parser on the given file while recording statistics *)
let parse ~(parser: parser) (filename : string) : string * trm =
  print_info None "Parsing %s...\n" filename;
  let parsed_file = stats ~name:"tr_ast" (fun () -> parser filename) in
  print_info None "Parsing Done.\n";
  parsed_file


(******************************************************************************)
(*                             Trace management                               *)
(******************************************************************************)

(** [context]: contains general information about:
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

(** [contex_dummy]: used for [trace_dummy]. *)
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

(** [step_kind] : classifies the kind of steps *)
type step_kind = Step_root | Step_big | Step_small | Step_transfo | Step_target_resolve | Step_io | Step_group | Step_backtrack | Step_show | Step_typing | Step_error

(** [step_kind_to_string] converts a step-kind into a string *)
let step_kind_to_string (k:step_kind) : string =
  match k with
  | Step_root -> "Root"
  | Step_big -> "Big"
  | Step_small -> "Small"
  | Step_transfo -> "Transfo"
  | Step_target_resolve -> "Target"
  | Step_io -> "IO"
  | Step_group -> "Group"
  | Step_backtrack -> "Backtrack"
  | Step_show -> "Show"
  | Step_typing -> "Typing"
  | Step_error -> "Error"

(** [step_infos] *)
type step_infos = {
  mutable step_script : string;
  mutable step_script_line : int option;
  mutable step_time_start : float; (* seconds since start *)
  mutable step_exectime : float; (* seconds *)
  mutable step_name : string;
  mutable step_args : (string * string) list;
  mutable step_valid : bool;
  mutable step_justif : string list; (* accumulated in reverse order during the step *)
  mutable step_tags : string list; (* accumulated in reverse order during the step *)
  mutable step_debug_msgs : string list; (* accumulated in reverse order during the step *)
}

(** [step_tree]: history type used for storing all the trace information about all steps, recursively. *)
type step_tree = {
  mutable step_kind : step_kind;
  mutable step_ast_before : trm; (* possibly [empty_ast], for "show" steps *)
  mutable step_ast_after : trm;
  mutable step_style_before : output_style;
  mutable step_style_after : output_style;
  mutable step_sub : step_tree list; (* accumulated in reverse order during the step *)
  (* substeps in reverse order during construction (between open and close) *)
  mutable step_infos : step_infos; }
  (* TODO: FOR PARSING STEPS int_of_float(stats_parse.stats_time); } *)


(** A [step_stack] is a stack that contains the currently opened steps,
   with the innermost at the top. The bottom element of the stack is
   always the one that describes the execution of the full transformation
   script. *)
type step_stack = step_tree list


(** [trace]: a record made of a context, a current AST, and a list of ASTs that were
   saved as "interesting intermediate steps", via the [Trace.save] function.
   Any call to the [step] function adds a copy of [cur_ast] into [history]. *)
type trace = {
  mutable context : context;
  mutable cur_ast : trm;
  mutable cur_style : output_style;
  mutable step_stack : step_stack; } (* stack of open steps *)

(** [trm_dummy]: dummy trm. *)
let trm_dummy : trm =
  trm_val (Val_lit Lit_unit)

(** [trace_dummy]: an trace made of dummy context and dummy trm,
   whose purpose is to enforce that [Trace.init] is called before any transformation *)
let trace_dummy : trace =
  { context = context_dummy;
    cur_ast = trm_dummy; (* dummy *)
    cur_style = Style.default_custom_style();
    step_stack = []; (* dummy *)
    }

(** [the_trace]: the trace produced by the current script. *)
let the_trace : trace =
  trace_dummy

(** [is_trace_dummy()]: returns whether the trace was never initialized. *)
let is_trace_dummy () : bool =
  the_trace.context == context_dummy

(** [get_decorated_history]: gets history from trace with a few meta information *)
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


(******************************************************************************)
(*                                   Checker                                  *)
(******************************************************************************)

exception Invalid_trace of string

(** [check ~final] verify that [the_trace] is well-formed.
    If the argument [final] is true, all the steps must be properly closed.
    Raises [Invalid_trace] if an invariant is broken. *)
let check_the_trace ~(final:bool) : unit =
  (* Auxiliary function to report a broken invariant *)
  let err (msg:string) : unit =
    raise (Invalid_trace msg) in
  (* Auxiliary function to compute the expected kind of a substep *)
  let kind_for_sub (kind:step_kind) : step_kind =
    match kind with
    | Step_root -> Step_big
    | Step_big -> Step_small
    | Step_small -> Step_transfo
    | _ -> Step_transfo
    in
  (* Recursive checker for a [step_tree].
     [expected_kind] is one of [Step_root], [Step_big], [Step_small], or [Step_transfo]
     it is used to ensure e.g. that small-steps are not nested in other small-steps,
     or big-steps nested inside small-steps, etc.
     Note that big-steps are optional in the hierarchy:
    small steps may be children of the root. *)
  let rec check_tree ~(expected_kind:step_kind) (step:step_tree) : unit =
    let kind = step.step_kind in
    let sub = List.rev step.step_sub in
    (* Check kind, compared with [expected_kind] *)
    begin match kind, expected_kind with
      | Step_root, Step_root -> ()
      | Step_root, _ -> err "A root step should only appear at the top of the step_stack"
      | _, Step_root -> err "A root step was expected to appear at the top of the step_stack"
      | Step_big, Step_big -> ()
      | Step_big, _ -> err "A big step should only appear at depth one in the step_stack"
      | Step_small, (Step_small | Step_big) -> ()
      | Step_small, _ -> err "A small step should only appear at depth one or two in the step_stack"
      | _, (Step_transfo | Step_small | Step_big) -> ()
      | _ -> failwith "Invalid argument for [expected_kind] in [check_tree]"
    end;
    (* A backtrack step always contains a single substep, which must be of kind group.
       Moreover, if the trace is final, the ast_after matches the ast_before *)
    if kind = Step_backtrack then begin
      match sub with
      | [] when not final -> () (* backtrack step should be currently opened;
                    LATER: implement a check that there are more elements in the stack *)
      | [substep] ->
          if substep.step_kind <> Step_group
            then err "A backtrack step should have a group step as child"
      | _ -> err "A backtrack step should have exactly one substep"
    end;
    (* Check substeps, with a lower [expected_kind] *)
    let expected_kind_sub = kind_for_sub kind in
    List.iter (check_tree ~expected_kind:expected_kind_sub) sub;
    in
  ignore (List.fold_right (fun step expected_kind ->
            check_tree ~expected_kind step;
            kind_for_sub step.step_kind)
          the_trace.step_stack Step_root);
  (* If [final], then [trace] must be reduced to a single root step. *)
  if final then begin
    match the_trace.step_stack with
    | [step] -> ()
    | _ -> err "A finalized trace should consist of a single step"
  end


(******************************************************************************)
(*                                   Output                                   *)
(******************************************************************************)

(** [cleanup_cpp_file_using_clang_format filename]: makes a system call to
   reformat a CPP file using the clang format tool.
   LATER: find a way to remove extra parentheses in ast_to_doc, by using
   priorities to determine when parentheses are required. *)
let cleanup_cpp_file_using_clang_format ?(uncomment_pragma : bool = false) (filename : string) : unit =
  stats ~name:(Printf.sprintf "cleanup_cpp_file_using_clang_format(%s)" filename) (fun () ->
    ignore (Sys.command ("clang-format -style=\"Google\" -i " ^ filename));
    if (* false && *) uncomment_pragma
      then ignore (Sys.command ("sed -i 's@//#pragma@#pragma@' " ^ filename))
  )


(** [get_header ()]: get the header of the current file (e.g. include directives) *)
let get_header () : string =
  the_trace.context.header

(** [ensure_header]: ensures that the header [h] is included in the header of the current file. *)
(* FIXME: does not show in diff this way *)
let ensure_header (h : string) : unit =
  let ctx = the_trace.context in
  let found = Tools.pattern_matches h (ctx.header) in
  if not found then
    the_trace.context <- { ctx with header = ctx.header ^ h ^ "\n" }

(** [output_prog style ctx prefix ast]: writes the program described by the term [ast] into file.
   - one describing the CPP code ("prefix.cpp")
   If the flag [-dump-ast-details] is set, also produce:
   - one describing the raw AST ("prefix.ast")
   - one describing the internal AST ("prefix_enc.cpp")
   The CPP code is formatted using clang-format, unless [-disable-clang-format] is passed. *)
let output_prog (style:output_style) ?(beautify:bool=true) (ctx : context) (prefix : string) (ast : trm) : unit =
  let use_clang_format = beautify && !Flags.use_clang_format in
  let file_prog = prefix ^ ctx.extension in
  let out_prog = open_out file_prog in
  begin try
    (* Print the header, in particular the include directives *) (* LATER: include header directives into the AST representation *)
    output_string out_prog ctx.header;
    (* Optionally add typing information such as resources *)
    let ast =
      if Style.is_typing_none style.typing
        then ast
        else Ast_fromto_AstC.computed_resources_intro style.typing ast in
    (* Optionally convert from OptiTrust to C syntax *)
    let ast = if style.decode then Ast_fromto_AstC.cfeatures_intro ast else ast in
    (* Print the code into file, using the specified style *)
    let cstyle = match style.print with
      | Lang_AST _-> failwith "output_prog requires a Lang_C printing mode, not a Lang_AST"
      | Lang_C cstyle -> cstyle
      in
    AstC_to_c.ast_to_outchannel cstyle out_prog ast;
    output_string out_prog "\n";
    close_out out_prog;
  with | Failure s ->
    close_out out_prog;
    failwith s
  end;
  (* Beautify the generated C++ code using clang-format *)
  if use_clang_format
    then cleanup_cpp_file_using_clang_format ~uncomment_pragma:use_clang_format file_prog;
  (* Optionally (flag [-dump-ast-details]), generated output also in OptiTrust syntax and Raw syntax *)
  if !Flags.dump_ast_details then begin
    let file_ast = prefix ^ ".ast" in
    let file_enc = prefix ^ "_enc" ^ ctx.extension in
    let out_ast = open_out file_ast in
    let out_enc = open_out file_enc in
    begin try
      (* Print the raw ast *)
      begin
        let style = Ast_to_text.default_style() in
        Ast_to_text.print_ast style out_ast ast;
        output_string out_ast "\n";
        close_out out_ast;
      end;
      (* Print the non-decoded ast *)
      output_string out_enc ctx.header;
      let style = AstC_to_c.default_style() in
      AstC_to_c.ast_to_outchannel { style with optitrust_syntax = true } out_enc ast;
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

(** [reparse_trm ctx ast]: prints [ast] in a temporary file and reparses it using Clang. *)
let reparse_trm ?(info : string = "") ?(parser: parser option) (ctx : context) (ast : trm) : trm =
  if !Flags.debug_reparse then begin
    let info = if info <> "" then info else "of a term during the step starting at" in
    Printf.printf "Reparse: %s.\n" info;
    flush stdout
  end;
  let in_prefix = (Filename.dirname ctx.prefix) ^ "/tmp_" ^ (Filename.basename ctx.prefix) in
  output_prog (Style.custom_style_for_reparse()) ~beautify:false ctx in_prefix ast;

  let parser =
    match parser with
    | Some p -> p
    | None -> ctx.parser
  in

  let (_, t) = parse ~parser (in_prefix ^ ctx.extension) in
  (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
  t

let reparse_ast ?(update_cur_ast : bool = true) ?(info : string = "the code during the step starting at") ?(parser: parser option) () =
  let tnew = reparse_trm ~info ?parser the_trace.context the_trace.cur_ast in
  if update_cur_ast
    then the_trace.cur_ast <- tnew


let recompute_resources_on_ast () : unit =
  let t = Scope_computation.infer_var_ids the_trace.cur_ast in (* Resource computation needs var_ids to be calculated *)
  let t = Resource_computation.trm_recompute_resources Resource_set.empty t in
  the_trace.cur_ast <- t

(******************************************************************************)
(*                               Step management                              *)
(******************************************************************************)

(** [get_cur_step ()] returns the current step --there should always be one. *)
let get_cur_step ?(error : string = "get_cur_step: empty stack") () : step_tree =
  match the_trace.step_stack with
  | [] -> failwith error
  | step::_ -> step

(** [open_root_step] is called only by [Trace.init], for initializing
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
    step_debug_msgs = [];
  } in
  let step_root = {
    step_kind = Step_root;
    step_ast_before = the_trace.cur_ast;
    step_style_before = the_trace.cur_style;
    step_ast_after = trm_dummy;
    step_style_after = the_trace.cur_style;
    step_sub = [];
    step_infos = step_root_infos; }
    in
  the_trace.step_stack <- [step_root]

(** [get_root_step()] returns the root step, after close_root_step has been called *)
let get_root_step () : step_tree =
  match the_trace.step_stack with
  | [step] ->
      if step.step_ast_after == trm_dummy
        then failwith "get_root_step: close_root_step has not been called";
      step
  | _ -> failwith "close_root_step: broken invariant, stack must have size one"

(** [get_excerpt line]: returns the piece of transformation script that starts on the given line. Currently returns the ""
    in case [compute_ml_file_excerpts] was never called. LATER: make it fail in that case. *)
let get_excerpt (line : int) : string =
  if line = - 1 then sprintf "<get_excerpt for line -1>" else (*failwith "get_excerpt: requires a valid line number";*)
  if !ml_file_excerpts = Int_map.empty then "<get_excerpt: empty map>" else begin (* should "" be failure? *)
  match Int_map.find_opt line !ml_file_excerpts with
    | Some txt -> if txt <> "" then txt else sprintf "<get_excerpt: empty string mapped to line %d>" line
    | None -> (*LATER: failwith? *) sprintf "<get_excerpt: no binding for line %d>" line
  end

(** [open_step] is called at the start of every big-step, or small-step,
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
    step_debug_msgs = [];
  } in
  let step = {
    (* fields set at the start of the step *)
    step_kind = kind;
    step_ast_before = the_trace.cur_ast;
    step_style_before = the_trace.cur_style;
    (* mutated during the lifetime of the step: *)
    step_sub = [];
    step_infos = infos;
    (* mutated at the finalization of the step: *)
    step_ast_after = trm_dummy;
    step_style_after = the_trace.cur_style; }
    in
  the_trace.step_stack <- step :: the_trace.step_stack;
  step

(** [justif txt] is called by a transformation after open_step in order
   to store explaination of why it is correct *)
let justif (justif:string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_valid <- true;
  infos.step_justif <- justif::infos.step_justif

(** [justif_always_correct()] is a specialized version of [step_justif]
   for transformation that are always correct. *)
let justif_always_correct () : unit =
  justif "always correct"
  (* TODO: subcases:
     - correct because the ast is not modified
     - correct because the code has not changed (only contracts and decorations have) *)

(** [step_arg] is called by a transformation after open_step in order
   to store the string representations of one argument. *)
let step_arg ~(name:string) ~(value:string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_args <- (name,value)::infos.step_args

(** [tag] is called by a transformation after open_step in order to associate a tag with itself. *)
let tag (s : string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_tags <- s :: infos.step_tags

(** [msg] is a function to add a debug message to the current step. *)
let msg (s : string) : unit =
  let step = get_cur_step () in
  let infos = step.step_infos in
  infos.step_debug_msgs <- s :: infos.step_debug_msgs

(** [tag_trivial] is called by a transformation after open_step to indicate that it is trivial, or trivially explained by its substeps. *)
let tag_trivial () : unit =
  tag "trivial"

(** [tag_atomic] is called by a transformation after open_step to indicate that it is atomic, e.g. looking at its substeps does not explain why it is correct. *)
let tag_atomic () : unit =
  tag "atomic"

(** [tag_valid_by_composition] is called by a transformation after open_step to indicate that it should be valid by composition. This can be used for filtering trace display or checking that it is indeed valid by composition. *)
let tag_valid_by_composition () : unit =
  tag "should_be_valid_by_composition"

(** [tag_simpl_arith] is called by a transformation after open_step to indicate that it performs arithmetic simplifications. This can be used for filtering trace display. *)
let tag_simpl_arith () : unit =
  tag "simpl";
  tag "simpl_arith"

(** [tag_simpl_access] is called by a transformation after open_step to indicate that it performs array/matrix access simplificatoins.
  *)
let tag_simpl_access () : unit =
  tag "simpl.access"

let without_substep_validity_checks (f: unit -> 'a): 'a =
  Flags.with_flag Flags.check_validity false f

(** [try_validate_step_by_compostion s] sets a computation to be valid if all its substeps are valid
   and form a contiguous chain *)
let try_validate_step_by_compostion (s : step_tree) : unit =
  let infos = s.step_infos in
  if not infos.step_valid then begin
    let kinds_excluded = [Step_target_resolve; Step_io; Step_backtrack] in
    let subs = List.filter (fun si -> not (List.mem si.step_kind kinds_excluded)) s.step_sub in
    if List.for_all (fun sub -> sub.step_infos.step_valid) subs then begin
      let asts1: trm list = [s.step_ast_before] @
        (List.map (fun sub -> sub.step_ast_after) subs);
      in
      let asts2: trm list = (List.map (fun sub -> sub.step_ast_before) subs) @
        [s.step_ast_after]
      in
      if List.for_all2 (==) asts1 asts2 then begin
        infos.step_tags <- "valid_by_composition" :: infos.step_tags;
        infos.step_valid <- true
      end (* else begin
        printf "%s\n" (infos.step_name);
        printf "%s\n" (Trace_printers.list_arg_printer pointer_to_string asts1);
        printf "%s\n" (Trace_printers.list_arg_printer pointer_to_string asts2);
      end *)
    end
  end

let is_saved_step step =
  match step.step_kind with
  | Step_root | Step_big | Step_small | Step_transfo | Step_group | Step_typing | Step_io -> true
  | Step_target_resolve | Step_backtrack | Step_error | Step_show -> false

let last_recorded_ast step: trm =
  let rec browse_steps steps =
    match steps with
    | [] -> step.step_ast_before
    | last_step :: _ when is_saved_step last_step -> last_step.step_ast_after
    | _ :: previous_steps -> browse_steps previous_steps
  in
  browse_steps step.step_sub

(** [finalize_step] is called by [close_root_step] and [close_step] *)
let rec finalize_step (step : step_tree) : unit =
  let infos = step.step_infos in
  (* Handle retyping and reparse operations at the end of every step,
     except for steps that do not modify the current ast *)
  let same_as_last_step =
    match step.step_sub with
    | [] -> step.step_ast_before == the_trace.cur_ast
    | last_step :: _ -> last_step.step_ast_after == the_trace.cur_ast
  in
  if not same_as_last_step then begin match step.step_kind with
    | Step_typing | Step_io | Step_target_resolve
    | Step_backtrack | Step_error | Step_show -> ()
    | Step_root | Step_big | Step_small | Step_transfo | Step_group ->
        (* TODO: Wrap error message without losing trace *)
        if !Flags.reparse_between_steps
          then reparse ();
        if !Flags.recompute_resources_between_steps
          then recompute_resources ()
  end;
  (* Flip lists that have been accumulated in reverse order during the step *)
  infos.step_args <- List.rev infos.step_args;
  infos.step_tags <- List.rev infos.step_tags;
  infos.step_debug_msgs <- List.rev infos.step_debug_msgs;
  step.step_sub <- List.rev step.step_sub;
  (* Save the ast_after and its style, and the time *)
  step.step_ast_after <- the_trace.cur_ast;
  step.step_style_after <- the_trace.cur_style;
  infos.step_exectime <- now() -. infos.step_time_start;
  (* Set the validity flag *)
  if !Flags.check_validity
    then try_validate_step_by_compostion step
    else step.step_infos.step_valid <- false;
  (* If the step is a small-step and contains a unique substep tagged
     "show", then the current small-step is also tagged "show".
     If the small-step contains multiple show step, we print a warning. *)
  if step.step_kind = Step_small then begin
    let has_show_tag (substep:step_tree) : bool =
      List.mem "show" substep.step_infos.step_tags in
    match step.step_sub with
    | [ substep ] when has_show_tag substep ->
      infos.step_tags <- "show"::infos.step_tags;
    | steps ->
        if List.length (List.filter has_show_tag steps) > 1
          then Tools.warn "Should have only one show function after '!!'."
  end;


and without_reparsing_between_steps (f: unit -> unit): unit =
  Flags.with_flag Flags.reparse_between_steps false f;
  if !Flags.reparse_between_steps then reparse ()

and without_resource_computation_between_steps (f: unit -> 'a): 'a =
  Flags.with_flag Flags.recompute_resources_between_steps false f;
  if !Flags.recompute_resources_between_steps then recompute_resources ()

(** [close_step] is called at the end of every big-step, or small-step,
   or combi, or basic transformation. The step to close can be passed
   as an optional argument, to check that the exected step is being closed.
   If all substeps are valid and their sequence explains how to go from ast_before to ast_after, the step is valid by the explaination "combination of valid steps" *)
and close_step ?(discard = false) ?(check:step_tree option) () : unit =
  match the_trace.step_stack with
  | [] -> failwith "close_step: the_trace should not be empty"
  | [root_step] -> failwith "close_step: on the root, should call close_root_step"
  | step :: ((parent_step :: _) as stack_tail)  ->
      (* Checking that we close the expected step *)
      begin match check with
      | None -> ()
      | Some opened_step ->
          if step != opened_step
            then failwith "close_step: not closing the expected step"
      end;
      if not discard then begin
        (* Finalize the step, by reversing the list of substeps and computing validity *)
        finalize_step step;
        (* Folding step into parent substeps *)
        parent_step.step_sub <- step :: parent_step.step_sub;
      end;
      the_trace.step_stack <- stack_tail;
      (* In debug mode, check the trace invariant *)
      if check_trace_at_every_step
        then check_the_trace ~final:false

(** [step] is a function wrapping the body of a transformation *)
and step ?(valid:bool=false) ?(line : int = -1) ?(tags:string list=[]) ~(kind:step_kind) ~(name:string) (body : unit -> 'a) : 'a =
  let s = open_step ~valid ~line ~tags ~kind ~name () in
  let r = body () in
  assert (get_cur_step () == s);
  close_step ~check:s ();
  r

(** [parsing_step f] adds a step accounting for a parsing operation *)
and parsing_step (f : unit -> unit) : unit =
  step ~valid:true ~kind:Step_io ~name:"Parsing" ~tags:["IO"] f

(** [dumping_step f] adds a step accounting for a parsing operation *)
and dumping_step (f : unit -> unit) : unit =
  step ~valid:true ~kind:Step_io ~name:"Dumping" ~tags:["IO"] f

(** [error_step f] adds a step accounting for a fatal error *)
and error_step (exn : exn): unit =
  let preprend_to_step_name (prefix: string) : unit =
    let step = get_cur_step () in
    let infos = step.step_infos in
    infos.step_name <- prefix ^ infos.step_name
  in
  let process_context (contexts : error_context list) : unit =
    List.iter (fun c ->
      Option.iter (fun p ->
        let mark = Mark.next () in
        let prefix = ref p in
        let prefix_invalid = ref true in
        (* TODO: factorize this code in Path. module *)
        while !prefix_invalid do
          try
            the_trace.cur_ast <- Path.apply_on_path (trm_add_mark mark) the_trace.cur_ast !prefix;
            prefix_invalid := false;
          with
          | _ -> (* TODO: more precise catch ? *)
            prefix := Path.parent !prefix
        done;
        let prefix_len = List.length !prefix in
        if prefix_len == List.length p
          then preprend_to_step_name (" @ path " ^ mark)
          else preprend_to_step_name (" @ path " ^ mark ^ "+" ^ (Path.path_to_string (Xlist.drop prefix_len p)));
      ) c.path;
      Option.iter (fun trm ->
        let mark = Mark.next () in
        let marked = ref false in
        let rec apply_mark t =
          if t == trm then begin
            marked := true;
            trm_add_mark mark t
          end else
            trm_map apply_mark t
        in
        the_trace.cur_ast <- apply_mark the_trace.cur_ast;
        preprend_to_step_name (" @ term " ^ mark);
      ) c.trm;
      (* TODO: c.loc *)
      if c.msg <> "" then
        preprend_to_step_name (" " ^ c.msg);
    ) (List.rev contexts)
  in
  let rec process (exn : exn) : unit =
    match exn with
    | Contextualized_error (contexts, exn) ->
      process_context contexts;
      process exn;
    | Failure msg ->
      preprend_to_step_name msg
    | _ ->
      preprend_to_step_name (Printexc.to_string exn)
  in
  step ~valid:false ~kind:Step_error ~name:"" (fun () -> process exn)

(** [typing_step f] adds a step accounting for a typing recomputation *)
and typing_step ~name (f : unit -> unit) : unit =
  step ~valid:true ~kind:Step_typing ~name ~tags:["typing"] f

(** [reparse ()]: function takes the current AST, prints it to a file, and parses it
   as if it was a fresh input. Doing so ensures in particular that all the type
   information is properly set up.
   WARNING: reparsing discards all the marks in the AST. *)
and reparse ?(update_cur_ast = true) ?(info : string option) ?(parser: parser option) () : unit =
  parsing_step (reparse_ast ~update_cur_ast ?info ?parser)

and recompute_resources (): unit =
  typing_step ~name:"Resource recomputation" recompute_resources_on_ast

(** [retypecheck] is currently implemented as [reparse], but in the future it
   would use a dedicated typechecker. *)
let retypecheck ?(info : string option) ?(parser: parser option) () =
  typing_step ~name:"Retypecheck" (reparse_ast ?info ?parser)

(** [close_step_kind_if_needed k] is used by
   [close_smallstep_if_needed] and [close_bigstep_if_needed] *)
let close_step_kind_if_needed (k:step_kind) : unit =
  let step = get_cur_step() in
  if step.step_kind = k then close_step()

(** [close_smallstep_if_needed()] closes a current small-step.
   Because small-steps are not syntactically scoped in the user script,
   we need such an implicit close operation to be called on either
   the opening of a new big-step, or on closing of the root step. *)
let close_smallstep_if_needed () : unit =
  close_step_kind_if_needed Step_small

(** [close_bigstep_if_needed()] closes a current big-step.
   Because big-steps are not syntactically scoped in the user script,
   we need such an implicit close operation to be called on either
   the opening of a new big-step, or on closing of the root step. *)
let close_bigstep_if_needed () : unit =
  close_smallstep_if_needed();
  close_step_kind_if_needed Step_big

(** [close_root_step] is called only by [finalize] at the end of the
   [Run.script] function. It finalizes the root step, and leaves the
   root step at the bottom of the stack. *)
let close_root_step () : unit =
  close_bigstep_if_needed();
  let step = match the_trace.step_stack with
    | [step] -> step
    | _ -> failwith "close_root_step: broken invariant, stack must have size one" in
  finalize_step step

(** [step_backtrack f] executes [f] wrapped in a step of kind [Step_backtrack],
   and a nested step of kind [Step_group]. At the end of [f], the current ast is
   restored to its original value. The value returned is the result produced by [f].
   The backtrack-step will have the current ast as [ast_before] and [ast_after].
   The group-step will have the current ast as [ast_before], and saves into
   [ast_after] the state of the ast just before it is rolled back to its original state.
   Note: all the substeps that f might have opened but not closed are
   automatically closed.
   If the option [~discard_after:true] is provided, then the steps
   performed by [f] are completely erased from the trace. *)
let step_backtrack ?(tags:string list=[]) ?(discard_after = false) (f : unit -> 'a) : 'a =
  let ast_snapshot = the_trace.cur_ast in
  (* Open backtrack step and group step, then execute [f] *)
  let step_backtrack = open_step ~kind:Step_backtrack ~name:"step-backtrack" ~tags () in
  let step_group = open_step ~kind:Step_group ~name:"step-backtrack-group" () in
  let res = f () in
  (* Close the group step -- LATER: document why a while-loop may be useful here *)
  let error = "Trace.step_backtrack: unable to close the group" in
  while (get_cur_step ~error () != step_group) do
    close_step ()
  done;
  close_step ~check:step_group ();
  (* Restore the ast, then close the backtrack step;
    This step is always correct because it corresponds to a noop. *)
  the_trace.cur_ast <- ast_snapshot;
  justif "step-backtrack restores the ast";
  close_step ~discard:discard_after ~check:step_backtrack ();
  res

type 'a backtrack_result =
| Success of 'a
| Failure of exn

(** [step_backtrack_on_failure f] executes [f].
   If [f] succeeds, the step terminates, and [Success] is returned.
   The operations performed by [f] are wrapped in an outer step of kind
   [Step_backtrack] and an inner step of kind [Step_group].
   -- LATER: we could attempt to eliminate the [Step_backtrack] but it's tricky.
   If [f] fails, the operation returns [Failure e]. In that case, the ast
   is restored to its original value. The operations performed by [f]
   are wrapped as if [step_backtrack] had been called, that is, with
   an outer [Step_backtrack] step, and an inner [Step_group] step.
   If the option [~discard_on_failure:true] is provided, and if [f] raises an
   exception, then the steps performed by [f] are completely erased from the trace.
   Implementation note: initially, the backtrack step is created; if [f]
   succeeds, this step is discarded, and only the group step is kept. *)
let step_backtrack_on_failure ?(discard_on_failure = false) (f : unit -> 'a) : 'a backtrack_result =
  let ast_snapshot = the_trace.cur_ast in
  let step_backtrack = open_step ~kind:Step_backtrack ~name:"step-backtrack-on-failure" () in
  let step_group = open_step ~kind:Step_group ~name:"step-backtrack-on-failure-group" () in
  let res =
    try
      let x = f() in
      (* Close the group step *)
      close_step ~check:step_group ();
      Success x
    with e -> begin
      (* Close all the steps that have been interrupted by the exception *)
      let error = "Trace.step_backtrack_on_failure: unable to close the group" in
      while (get_cur_step ~error () != step_group) do
        close_step ()
      done;
      (* Close the group step *)
      close_step ~check:step_group ();
      Failure e
    end in
  (* Close the backtrack step *)
  let is_success = match res with Success _ -> true | Failure _ -> false in
  if is_success then begin
    (* If success, we close the backtrack step. LATER: collapse it?
       The changes performed on the ast remain. *)
    close_step ~discard:false ~check:step_backtrack ();
  end else begin
    (* If failure, restore the ast, and close the backtrack step;
       discard the whole step if [~discard_on_failure:true] is provided. *)
    the_trace.cur_ast <- ast_snapshot;
    close_step ~discard:discard_on_failure ~check:step_backtrack ();
  end;
  (* Return a description of the result of [f] *)
  res

(** [target_resolve_step] has a special handling because it saves a diff
   between an AST and an AST decorated with marks for targeted paths,
   even though the [cur_ast] is not updated with the marsk *)
let target_resolve_step (f: trm-> Path.path list) (t:trm) : Path.path list =
  ignore (open_step ~valid:true ~kind:Step_target_resolve ~tags:["target"] ~name:"" ());
  let ps = f t in
  if Flags.is_execution_mode_trace() then begin
    let marked_ast, _marks = Path.add_marks_at_paths ps t in
    let cur_ast = the_trace.cur_ast in
    the_trace.cur_ast <- marked_ast;
    close_step();
    the_trace.cur_ast <- cur_ast
  end else begin
    close_step();
  end;
  ps

(** [invalidate()]: restores the global state (object [trace]) in its uninitialized state,
   like at the start of the program.  *)
let invalidate () : unit =
  close_logs();
  the_trace.context <- trace_dummy.context;
  the_trace.cur_ast <- trace_dummy.cur_ast;
  the_trace.step_stack <- trace_dummy.step_stack

(** [get_initial_ast ~parser ser_mode ser_file filename]: gets the initial ast before applying any trasformations
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
      then failwith "Trace.get_initial_ast: please generate a serialized file first";
    if not ser_file_more_recent
      then failwith (Printf.sprintf "Trace.get_initial_ast: serialized file is out of date with respect to %s\n" filename);
    let ast = Xfile.unserialize_from ser_file in
    if auto_use_ser
      then Printf.printf "Loaded ast from %s.\n" ser_file;
    ast
  )
  else
    parse ~parser filename

(** [init f]: initializes the trace with the contents of the file [f].
   This operation should be the first in a transformation script.
   The history is initialized with the initial AST.
   [~prefix:"foo"] allows to use a custom prefix for all output files,
   instead of the basename of [f].
   [~style] allows to specify a printing style for ASTs; the default
   style is computed based on the global flags.   *)
(* LATER for mli: val set_init_source : string -> unit *)
let init ?(prefix : string = "") ?(style:output_style option) ~(parser: parser) (filename : string) : unit =
  invalidate ();
  let basename = Filename.basename filename in
  let extension = Filename.extension basename in
  let default_prefix = Filename.remove_extension filename in
  let ml_file_name =
    if Tools.pattern_matches "_inlined" default_prefix
      then List.nth (Str.split (Str.regexp "_inlined") default_prefix) 0
      else default_prefix in
  if !Flags.analyse_stats || Flags.is_execution_mode_trace() then begin
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
  the_trace.cur_style <- begin match style with Some s -> s | None -> Style.default_custom_style() end;
  the_trace.step_stack <- [];
  open_root_step ~source:ml_file_name ();

  if mode = Serialized_Build || mode = Serialized_Auto
    then Xfile.serialize_to ser_file (header, cur_ast);
  if mode = Serialized_Build
    then exit 0;

  (* If recompute resources between steps is enabled, we need resources to be computed after the initial parsing as well *)
  if !Flags.recompute_resources_between_steps then recompute_resources ();

  print_info None "Starting script execution...\n"

(** [get_last_substep] returns the last substep, which corresponds to the
    step to be visualized when interactively targeting a given line *)
let get_last_substep () : step_tree =
  match (get_cur_step ()).step_sub with
  | [] -> failwith "Trace.get_last_substep: expected a previous substep in the current step"
  | last_step :: _ -> last_step

(** [get_original_ast] returns the ast obtained at [Trace.init] *)
let get_original_ast () : trm =
  assert (the_trace.step_stack <> []);
  let (_, root_step) = Xlist.unlast the_trace.step_stack in
  root_step.step_ast_before

(** [Failure_expected_did_not_fail] is an exception produced by [failure_expected] *)
exception Failure_expected_did_not_fail

(** [failure_expected h f]: executes the unit function [f], and checks that
   it raises an exception satisfying the boolean function [h].
   If [f] raises an exception [e] and the call [h e] evaluates to [true],
   then the function terminates normally.
   Otherwise, if [f] raises an exception that does not satisfy [h],
   this exception is propagated.
   Otherwise, if [f] does not raise an exception, then the exception
   [Failure_expected_did_not_fail] is raised. *)
let failure_expected (h : exn -> bool) (f : unit -> unit) : unit =
  step_backtrack (fun () ->
    try
      f();
      raise Failure_expected_did_not_fail
    with
      | Failure_expected_did_not_fail as e -> raise e
      | e -> if h e then () else raise e
  )

(** [apply f]: applies the transformation [f] to the current AST,
   and updates the current ast with the result of that transformation.
   If there are several active trace (e.g., after a [switch]),
   then [f] is applied to each of the traces. During the execution of [f]
   on a given trace, the set of traces is replaced with a singleton set
   made of only that trace; this allows for safe re-entrant calls
   (i.e., the function [f] itself may call [Trace.apply]. *)
let apply (f : trm -> trm) : unit =
  if is_trace_dummy()
    then failwith "Trace.init must be called prior to any transformation.";
  the_trace.cur_ast <- f the_trace.cur_ast

(** [reset] performs a step that sets the current ast to the original ast *)
let reset () : unit =
  let t = get_original_ast () in
  apply (fun _cur_ast -> t)

(** [call f]: is similar to [apply] except that it applies to a function [f]
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
    then failwith "Trace.init must be called prior to any transformation.";
  f the_trace.cur_ast



(******************************************************************************)
(*                                   More dump                                *)
(******************************************************************************)

(** [dump_steps]: writes into files called [`prefix`_$i_out.cpp] the contents of each of the
    small_steps or big steps, where [$i] denotes the index of a step.
    TODO: revive and adjust interface to allow a custom filter on which steps to select. *)
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

(* Only for debugging purposes.
  [trace_custom_postprocessing] is a function applied to all ast-after that
  are dumped in the trace *)
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
(** [dump_step_tree_to_js] auxiliary function for [dump_trace_to_js] *)
let rec dump_step_tree_to_js ~(is_substep_of_targeted_line:bool) (get_next_id:unit->int) (out:string->unit) (id:int) (s:step_tree) : unit =
  (* LATER: flags_trace_details_only_for_line could be revived in the future if we need more details for a specific step *)
  let flags_trace_details_only_for_line = -1 in (* [-1] means requesting details for all steps *)

  let i = s.step_infos in
  (* Report diff and AST for details *)
  let is_smallstep_of_targeted_line =
    (i.step_script_line <> Some (-1)) && (* LATER: use options *)
    (i.step_script_line = Some flags_trace_details_only_for_line) in
  let is_substep_of_targeted_line =
      is_substep_of_targeted_line || is_smallstep_of_targeted_line in
  let details =
        (flags_trace_details_only_for_line = -1)
     || s.step_kind = Step_big
     || s.step_kind = Step_small
     || is_substep_of_targeted_line
     in
  (* Recursive calls *)
  let aux = dump_step_tree_to_js ~is_substep_of_targeted_line get_next_id out in
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
      (* custom processing for debugging *)
      let disp_ast_before = !trace_custom_postprocessing s.step_ast_before in
      let disp_ast_after = !trace_custom_postprocessing s.step_ast_after in
      begin try
        output_prog s.step_style_before ctx "tmp_before" disp_ast_before;
        output_prog s.step_style_after ctx "tmp_after" disp_ast_after;
      with e ->
        (* Prevent any exception during printing to corrupt the entire trace *)
        let exn = Printexc.to_string e in
        Printf.eprintf "Error while saving trace:\n%s\n" exn
      end;
      let sBefore = compute_command_base64 "cat tmp_before.cpp" in
      let sAfter = compute_command_base64 "cat tmp_after.cpp" in
      let sDiff = compute_command_base64 "git diff --ignore-all-space --no-index -U10 tmp_before.cpp tmp_after.cpp" in
      sBefore, sAfter, sDiff
    end else begin
      "", "", ""
    end in
  let json = (* TODO: check if ~html_newlines:true is needed for certain calls to [Json.str] *)
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
      "debug_msgs", Json.(listof str) i.step_debug_msgs;
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


(** [dump_trace_to_js step]: writes into a file called [`prefix`_trace.js] the
   contents of the step tree [step]. The JS file is structured as follows
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
let dump_trace_to_js ?(prefix : string = "") (step:step_tree) : unit =
  let prefix =
    if prefix = "" then the_trace.context.prefix else prefix in
  let filename = prefix ^ "_trace.js" in
  if debug then printf "Dumping trace to '%s'\n" filename;
  let out_js = open_out filename in
  let out = output_string out_js in
  let next_id = ref (-1) in
  let get_next_id () : int =
    incr next_id;
    !next_id in
  out "var steps = [];\n";
  let idroot = get_next_id() in
  dump_step_tree_to_js ~is_substep_of_targeted_line:false get_next_id out idroot step;
  (* Clean up the files generated by the functions located in dump_step_tree_to_js_and_return_id
     LATER: move this elsewhere *)
  cmd "rm -f tmp.base64 tmp_after.cpp tmp_before.cpp";
  close_out out_js

(** [dump_full_trace_to_js ()] invokes [dump_trace_to_js] on the root step *)
let dump_full_trace_to_js ?(prefix : string = "") () : unit =
  dump_trace_to_js ~prefix (get_root_step())

(** [step_tree_to_doc step_tree] takes a step tree and gives a string
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

(** [step_tree_to_file filename step_tree] takes a step tree and writes
   its string representation into a file. *)
let step_tree_to_file (filename:string) (step_tree:step_tree) =
  let line_width = 500 in
  let out = open_out filename in
  ToChannel.pretty 0.9 line_width out (step_tree_to_doc step_tree);
  close_out out

(** [dump_trace_to_textfile] dumps a trace into a text file *)
let dump_trace_to_textfile ?(prefix : string = "") () : unit =
  let prefix =
    if prefix = "" then the_trace.context.prefix else prefix in
  let filename = prefix ^ "_trace.txt" in
  if debug then printf "Dumping trace to '%s'\n" filename;
  step_tree_to_file filename (get_root_step())

(** [output_prog_check_empty style ctx prefix ast_opt]: similar to [output_prog], but it
   generates an empty file in case the [ast] is an empty ast.
   LATER: this function could be inlined at its unique call site. *)
let output_prog_check_empty (style : output_style) (ctx : context) (prefix : string) (ast_opt : trm) : unit =
  match ast_opt.desc with (* TODO: add and use a function [trm_empty_inv] *)
  | Trm_seq tl when Mlist.length tl <> 0 ->
      output_prog style ctx prefix ast_opt
  | _ ->
      let file_prog = prefix ^ ctx.extension in
      let out_prog = open_out file_prog in (* TODO: use a function [file_put_contents] *)
      close_out out_prog

(** [light_diff astBefore astAfter]: find all the functions that have not change after
    applying a transformation and hides their body for a more robust view diff. *)
let light_diff (astBefore : trm) (astAfter : trm) : trm * trm  =
    let topfun_before = top_level_fun_bindings astBefore in
    let topfun_after = top_level_fun_bindings astAfter in
    let topfun_common = get_common_top_fun topfun_before topfun_after in
    let filter_common ast = fst (hide_function_bodies (fun f -> List.mem f topfun_common) ast) in
    let new_astBefore = filter_common astBefore in
    let new_astAfter = filter_common astAfter in
    (new_astBefore, new_astAfter)

(** [produce_diff_output_internal step] is an auxiliary function for [produce_diff_output]. *)
let produce_diff_output_internal (step:step_tree) : unit =
  let trace = the_trace in
  let ctx = trace.context in
  let prefix = (* ctx.directory ^ *) ctx.prefix in
  (* Extract the two ASTs that should be used for the diff *)
  let ast_before, ast_after = step.step_ast_before, step.step_ast_after in
  let style_before, style_after = step.step_style_before, step.step_style_after in
  (* Option to compute light-diff:
      hide the bodies of functions that are identical in astBefore and astAfter. *)
  let ast_before, ast_after =
    if !Flags.use_light_diff
      then light_diff ast_before ast_after
      else ast_before, ast_after in
  (* Common printing function *)
  let output_ast style filename_prefix ast =
    output_prog_check_empty style ctx filename_prefix ast;
    print_info None "Generated: %s%s\n" filename_prefix ctx.extension;
    in
  (* Generate files. *)
  output_ast style_before (prefix ^ "_before") ast_before;
  output_ast style_after (prefix ^ "_after") ast_after;
  print_info None "Writing ast and code into %s.js " prefix

(** [produce_trace_output step] is an auxiliary function for [produce_output_and_exit] *)
let produce_trace_output (step:step_tree) : unit =
  let trace = the_trace in (* LATER: cleanup and factorize next 3 lines *)
  let ctx = trace.context in
  let prefix = (* ctx.directory ^*) ctx.prefix in
  dump_trace_to_js ~prefix step

(** [extract_show_step] extracts a [Step_show] nested as unique substep in depth
    of a step. *)
let rec extract_show_step (step:step_tree) : step_tree =
  if step.step_kind = Step_show then step else begin
    match step.step_sub with
    | [ substep ] -> extract_show_step substep
    | _ -> failwith "Trace.extract_show_step: did not find a Step_show in depth"
  end

(** [produce_diff_output step] is an auxiliary function for [produce_output_and_exit].
   If the step targeted is a step with [ast_after == ast_before], then the diff
   would be empty. In the particular case this empty diff comes from a step
   tagged "show", we report the diff for the subset of kind [Step_show]
   (which is wrapped inside steps of kind [Step_backtract] and [Step_group]).
   -- LATER: generalize: take the ast_before of the first
   substep, and the ast_after of the last substep. *)
let produce_diff_output (step:step_tree) : unit =
  if step.step_ast_before == step.step_ast_after
    && List.mem "show" step.step_infos.step_tags then begin
    produce_diff_output_internal (extract_show_step step);
  end else begin
    produce_diff_output_internal step
  end

(** [produce_output_and_exit()]: invokes [output_prog] on the current AST an also on the
   last item from the history, then it interrupts the execution of the script.
   This function is useful for interactively studying the effect of one particular
   small-step or big-step from the script. The diff is computed by taking the step
   at the top of the stack [the_trace], and considering the diff of the last sub-step
   the was performed. *)
let produce_output_and_exit () : unit =
  print_info None "Exiting script\n";
  (* Extract the step that should be used for the diff *)
  let container_step = get_cur_step() in
  if container_step.step_sub = []
    then failwith "produce_output_and_exit: make sure you cursor is on a line starting with '!!' or 'bigstep'";
  let step = get_last_substep () in
  if !Flags.only_big_steps && step.step_kind <> Step_big
    then failwith "produce_output_and_exit: cannot show a diff for a big-step, no call to bigstep was made";
  (* Output the step description *)
  begin match !Flags.execution_mode with
  | Execution_mode_step_diff -> produce_diff_output step
  | Execution_mode_step_trace -> produce_trace_output step
  | Execution_mode_exec
  | Execution_mode_full_trace -> failwith "produce_output_and_exit should be in a 'step' execution mode"
  end;
  (* Print debug messages of the current step *)
  List.iter (fun s -> printf "%s\n" s) step.step_infos.step_debug_msgs;
  (* Exit *)
  close_logs ();
  exit 0

(** [check_exit ~line] checks whether the program execution should be interrupted based
   on the command line argument [-exit-line]. If so, it exists the program after dumping
   the diff. *)
let check_exit ~(line:int) : unit (* may not return *) =
  if Flags.is_execution_mode_step() && line > Flags.get_target_line()
     then produce_output_and_exit()

(** [check_exit_at_end] is called by [run.ml] when reaching the end of the script.
   This special case is needed to display a diff for the last transformation of
   the script. Indeed, this last transformation is not followed by a [!!] or a
   [bigstep] call.
   LATER: we may want to check that the targeted line is before the closing
   parenthesis of the call to script_cpp, but this requires instrumenting the
   call to script_cpp, and obtaining the end position of this construction. *)
let check_exit_at_end () : unit (* may not return *) =
  if  Flags.is_execution_mode_step() then begin
    close_smallstep_if_needed();
    if !Flags.only_big_steps
      then close_bigstep_if_needed();
    produce_output_and_exit()
  end


(******************************************************************************)
(*                                   Steps                                     *)
(******************************************************************************)


(** [open_bigstep s]: announces that the next step is a bigstep, and registers
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
    then reparse ();
  ignore (open_step ~kind:Step_big ~name:"" ~step_script:title ~line ());
  (* Handle progress report *)
  if !Flags.report_big_steps then begin
    Printf.printf "Executing bigstep %s%s\n"
      (if line <> -1 then sprintf "at line %d" line else "")
      title
  end

(** [open_smallstep s]: announces that the next step is a smallstep,
   and registers a string description for that step, based on the excerpt
   frmo the file. The [close_smallstep] is implicitly handled. *)
(** LATER: add the line argument in the generation of the _with_lines file *)
let open_smallstep ~(line : int) ?reparse:(need_reparse:bool=false) () : unit =
  close_smallstep_if_needed();
  if not !Flags.only_big_steps
    then check_exit ~line;
  if need_reparse
    then reparse ();
  let step_script =
    if Flags.is_execution_mode_trace()
      then get_excerpt line
      else ""
    in
  ignore (open_step ~kind:Step_small ~name:"" ~line ~step_script ())

(** [show_step ~ast_left ~style_left ~ast_right ~style_right]
   is used to implement the visualization operations.
   A optional [~name] argument can be used to customize the step
   label, e.g. [~name:"show-target"]. See module [Show.ml] for examples.
   A vizualization step is always wrapped in a [Step_backtrack], whose
   [ast_after] is equal to its [ast_before]. Inside of this step
   is contained a [Step_group], which itselft contains a [Step_show].
   This inner [Step_show] carries an [ast_before] and an [ast_after] that
   correspond to the material displayed on the left and right panels
   of the diff. These two [ast] may be arbitrary, and use custom styles
   for their display. When visualizing a step in interactive mode,
   if the step is a Step_backtrack, then instead of showing an empty diff,
   the diff displayed corresponds to the ast of the (unique) step inside.
   The backtract step containing the show step is tagged with the tag "show",
   to easily identify it as such. *)
let show_step ?(name:string="show") ~(ast_left:trm) ~(style_left:output_style) ~(ast_right:trm) ~(style_right:output_style) () =
  step_backtrack ~tags:["show"] (fun () ->
    (* Create the show step *)
    let s = open_step ~kind:Step_show ~name () in
    close_step ~check:s ();
    (* Customize the ast before and after, and their styles *)
    s.step_ast_before <- ast_left;
    s.step_ast_after <- ast_right;
    s.step_style_before <- style_left;
    s.step_style_after <- style_right;
  )

(** [transfo_step] is the function that is produced by a [let%transfo]. It performs a step,
   and set the name and the arguments of the step. *)
let transfo_step ~(name : string) ~(args : (string * string) list) (f : unit -> unit) : unit =
  step ~kind:Step_transfo ~name (fun () ->
    (* printf "> %s\n" name; *)
    List.iter (fun (k, v) -> step_arg ~name:k ~value:v) args;
    f ();
    (* printf "< %s\n" name; *)
  )

(** [check_recover_original()]: checks that the AST obtained so far
   is identical to the input AST, obtained from parsing. If not,
   it raises an error. *)
let check_recover_original () : unit =
  let check_same ast1 ast2 =
    if AstC_to_c.ast_to_string ast1 <> AstC_to_c.ast_to_string ast2
      then failwith "Trace.check_recover_original: the current AST is not identical to the original one."
      else () (* FOR DEBUG: Printf.printf "check_recover_original: successful" *)
    in
  let orig_ast = get_original_ast () in
  call (fun cur_ast -> check_same cur_ast orig_ast)


(******************************************************************************)
(*                                   FINALIZE                                 *)
(******************************************************************************)

(* TEMPORARY HACK *)
let ast_just_before_first_call_to_restore_original : trm option ref = ref None
(** [restore_original ()] sets as current ast the original ast obtained
    after parsing, i.e. the [ast_before] of the root step. *)
let restore_original () : unit =
  if !ast_just_before_first_call_to_restore_original = None
    then ast_just_before_first_call_to_restore_original := Some the_trace.cur_ast;
  transfo_step ~name:"restore-original" ~args:[] (fun () ->
    the_trace.cur_ast <- get_original_ast();
  )

(** [finalize()]: should be called at the end of the script to close the root step *)
let finalize () : unit =
  (* TEMPORARY HACK for handling effects after a call to restore_original *)
  begin match !ast_just_before_first_call_to_restore_original with
  | None -> ()
  | Some ast ->
      transfo_step ~name:"restore-ast-before-first-restore-original" ~args:[] (fun () ->
        the_trace.cur_ast <- ast)
  end;
  (* END *)
  close_root_step();
  (* Check the trace invariant (optional) *)
  try check_the_trace ~final:true
  with Invalid_trace msg -> Printf.eprintf "NON-FATAL ERROR: Trace.check_the_trace reports: %s\n" msg

(** [finalize_on_error()]: performs a best effort to close all steps after an error occurred *)
let finalize_on_error ~(exn: exn) : unit =
  Printf.eprintf "%s\n" (Printexc.to_string exn); (* FIXME: not here? *)
  error_step exn;
  let rec close_all_steps () : unit =
    match the_trace.step_stack with
    | [] -> failwith "close_close_all_stepsstep: the_trace should not be empty"
    | [_root_step] -> finalize()
    | _step :: _ -> close_step(); close_all_steps()
    in
  close_all_steps()


(******************************************************************************)
(*                                   User-level functions                     *)
(******************************************************************************)

  (* TODO: INTEGRATE Special hack for minimizing diff in documentation
  if !Flags.documentation_save_file_at_first_check <> "" then begin
    let trace = the_trace in
    let ctx = trace.context in
    output_prog ctx !Flags. (trace.cur_ast)
 *)

(** [!!]: is a prefix notation for the operation [open_smallstep].
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

(** [!!!]: is similar to [!!] but forces a [reparse] prior to the [step] operation.
   ONLY FOR DEVELOPMENT PURPOSE. *)
let (!!!) (x : 'a) : 'a =
  open_smallstep ~line:(-1) ~reparse:true ();
  x

(** [bigstep s]: an alias for [open_bigstep s], for usage in user scripts. *)
let bigstep (s : string) : unit =
  open_bigstep ~line:(-1) s

(** [dump style ()]: invokes [output_prog] to write the contents of the current AST.
   - If [~prefix] is provided, it is used as basename for the output file;
     otherwise the default prefix is [basename_out].
    the input file is used as prefix,
   - If [~append_comments] is provided, the corresponding string will be added
     as comments near the end of the output file *)
let dump (style : output_style) ?(prefix : string = "") ?(append_comments : string = "") () : unit =
  dumping_step (fun () ->
    let ctx = the_trace.context in
    let prefix =
      if prefix = "" then (* ctx.directory ^ *) ctx.prefix else prefix in
    output_prog style ctx (prefix ^ "_out") (the_trace.cur_ast);
    if append_comments <> "" then begin
      let filename = prefix ^ "_out.cpp" in
      (* open in append mode *)
      let c = open_out_gen [Open_append; Open_creat] 0o666 filename in
      output_string c "/*\n";
      output_string c append_comments;
      output_string c "\n*/\n";
      close_out c
    end
  )

(* DEPRECATED? [only_interactive_step line f]: invokes [f] only if the argument [line]
   matches the command line argument [-exit-line]. If so, it calls the
   [step] function to save the current AST, then calls [f] (for example
   to add decorators to the AST in the case of function [show]), then
   calls [produce_output_and_exit] to visualize the effect of [f].
let only_interactive_step (line : int) ?(reparse : bool = false) (f : unit -> unit) : unit =
  let stepdescr_for_interactive_step =
    { isbigstep = None; script = ""; exectime = 0; } in
  if should_exit = Flags.is_execution_mode_step() && Flags.get_target_line() = line then
    if reparse
      then
        reparse_alias ();
    step stepdescr_for_interactive_step;
    f();
    produce_output_and_exit()
  end
  else
    begin
    check_exit_and_step();
    f()
    end
    *)

(** [ast()]: returns the current ast; this function should only be called within the
   scope of a call to [Trace.apply] or [Trace.call]. For example:
   [Trace.call (fun t -> ...  let t = ast() in ...) ].
   Note that in most cases, this function is not needed because the argument of
   the continuation already describes the current AST as the variable [t]. *)
let ast () : trm =
   the_trace.cur_ast

(** [set_ast]: is used for implementing [iteri_on_transformed_targets]. Don't use it elsewhere.
   NOTE: INTERNAL FUNCTION. *)
let set_ast (t:trm) : unit =
  the_trace.cur_ast <- t

(** [get_context ()]: returns the current context. Like [ast()], it should only be called
   within the scope of [Trace.apply] or [Trace.call]. *)
let get_context () : context =
  the_trace.context

(** [get_style ()]: read the current style using for printing ASTs in the trace. *)
let get_style () : output_style =
  the_trace.cur_style

(** [set_style ()]: change the current style using for printing ASTs in the trace. *)
let set_style (style:output_style) : unit =
  the_trace.cur_style <- style

(** [update_style ()]: updates the current style using for printing ASTs in the trace
   by reading the flags. Call this function after modifying global flags. *)
let update_style () : unit =
  the_trace.cur_style <- Style.default_custom_style()


(* LATER:  need to reparse to hide spurious parentheses *)
(* LATER: add a mechanism for automatic simplifications after every step *)
