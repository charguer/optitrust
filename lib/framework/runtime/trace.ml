open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Stats
open Tools
open PPrint

(** [output_style] describes the mode in which an AST should be pretty-printed *)
type output_style = Style.output_style

(** Exceptions raised by this module when the user does not respect
    the interaction rules, or when internal invariants are broken *)
exception TraceFailure of string

(* TEMPORARY HACK *)
let ast_just_before_first_call_to_restore_original : trm option ref = ref None

(* TODO: lack of uniformity: show_step  vs step_backtrack *)

(******************************************************************************)
(*                             Debug flags                                    *)
(******************************************************************************)

(** [check_trace_at_every_step] can be activated to call the function
   [check_trace] after every step, to check the invariants of the
   trace data structure, which stores the stack of open steps. *)
let check_trace_at_every_step = ref false

(* For debugging, insert this line in your code:
   let _ = Trace.debug_open_close_step := true
*)

(* Other debug flags *)

let debug_open_close_step = ref false

let debug_notify_dump_trace = ref false

let debug_compute_ml_file_excerpts = ref false


(******************************************************************************)
(*                             File excerpts                                  *)
(******************************************************************************)

(** [ml_file_excerpts]: maps line numbers to the corresponding sections in-between [!!] marks in
   the source file. Line numbers are counted from 1 in that map. *)
module Int_map = Map.Make(Int)
let ml_file_excerpts = ref Int_map.empty

(** [compute_ml_file_excerpts lines]: is a function for grouping lines according to the [!!] symbols. *)
(* FIXME: The user will write scripts that make this approach produce weird results. Replace !! operators by a PPX for a robust alternative. *)
let compute_ml_file_excerpts (lines : string list) : string Int_map.t =
  let r = ref Int_map.empty in
  let start = ref 0 in
  let acc = Buffer.create 3000 in
  let push () =
    let s = Buffer.contents acc in
    let i = !start+1 in
    if !debug_compute_ml_file_excerpts
      then Tools.debug "Excerpt[%d] = <<<%s>>>" i s;
    r := Int_map.add i s !r;
    Buffer.clear acc; in
  (* match a line that starts with '!!', 'bigstep' or 'let' *)
  let regexp_step = Str.regexp "^[ ]*\\(!!\\|bigstep\\|let\\)" in
  let starts_with_step (str : string) : bool =
    Str.string_match regexp_step str 0 in
  let process_line (iline : int) (line : string) : unit =
    if starts_with_step line then begin
      push();
      start := iline;
    end;
    Buffer.add_string acc line;
    Buffer.add_string acc "\n";
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
  logs := timing_log :: stats_log :: clog :: []

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
  let msg = Printf.sprintf " -expression\n%s\n %s is a %s\n" (Ast_to_c.ast_to_string t) sloc exp_type in
 write_log clog msg

(******************************************************************************)
(*                             File input                                     *)
(******************************************************************************)

(** A parser should read a filename and return:
   - A header to copy in the produced file (typically a list of '#include' for C)
   - The OptiTrust AST of the rest of the file *)
(* TODO: encode header information in the AST *)
type parser = string -> string * trm

let c_parser ~(persistant:bool) (filename: string) : string * trm =
  (* "ser" means serialized *)
  let ser_filename = filename ^ ".ser" in

  let exitcode =
    Sys.command (Printf.sprintf "cd \"%s\" && dune exec --no-build tools/c_parser/c_parser.exe -- %s %s %s"
      !Flags.optitrust_root
      (if !Flags.ignore_serialized || persistant then "" else "-f")
      (if !Flags.debug_parsing_serialization then "-v" else "")
      (Unix.realpath filename))
  in
  if exitcode <> 0 then failwith "C parser returned with error code %d" exitcode;

  if not (Sys.file_exists ser_filename) then failwith "C parser did not produce the expected file: %s" ser_filename;

  let header, ast =
    try
      let ser_file = open_in_bin ser_filename in
      let _deps = Marshal.from_channel ser_file in (* Used by C parser to store header deps *)
      Marshal.from_channel ser_file
    with _ ->
      failwith "Deserialization failure on file: %s" ser_filename
  in

  if not persistant then Unix.unlink ser_filename;

  (* Possibly perform the decoding *)
  let ast = if !Flags.bypass_cfeatures then Scope_computation.infer_var_ids ast else C_encoding.cfeatures_elim ast in
  (* Return the header and the ast *)
  (header, ast)

(** [parse filename]:
   call the parser on the given file while recording statistics *)
let parse ?(persistant=true) (filename : string) : string * trm =
  Flags.verbose_info "Parsing %s..." filename;
  let parsed_file = stats ~name:"tr_ast" (fun () -> c_parser ~persistant filename) in
  Flags.verbose_info "Parsing Done.";
  parsed_file


(******************************************************************************)
(*                             Light diffs                                    *)
(******************************************************************************)

let debug_light_diff = ref false

(** [top_level_fun_bindings t]: returns a map with keys the names of toplevel function names and values being their bodies *)
let top_level_fun_bindings (t : trm) : tmap =
  let tmap = ref Var_map.empty in
    let aux (t : trm) : unit =
      match t.desc with
      | Trm_seq tl ->
        Mlist.iter (fun t1 ->
          match t1.desc with
          | Trm_let_fun (f, _, _, body, _) -> tmap := Var_map.add f body !tmap
          | _ -> ()
        ) tl
      | _ -> failwith "Ast.top_level_fun_bindings: expected the global sequence that contains all the toplevel declarations"
   in
  aux t;
  !tmap

(** [get_common_top_fun tm1 tm2]: takes two maps, binding function names to terms describing the function bodies,
    and returns the list of function names that are bound to the same terms in the two maps. *)
let get_common_top_fun (tm1 : tmap) (tm2 : tmap) : vars =
  let common = ref [] in
  Var_map.iter (fun f1 b1 ->
    match Var_map.find_opt f1 tm2 with
    | Some b2 when b1 == b2 -> common := f1 :: !common
    | _ -> ()
  ) tm1;
  !common

(** [light_diff astBefore astAfter]: find all the functions that have not change after
    applying a transformation and hides their body for a more robust view diff. *)
let light_diff (astBefore : trm) (astAfter : trm) : trm * trm  =
    let topfun_before = top_level_fun_bindings astBefore in
    let topfun_after = top_level_fun_bindings astAfter in
    let topfun_common = get_common_top_fun topfun_before topfun_after in
    if !debug_light_diff then begin
       Tools.debug "light_diff removes common functions: %s" (String.concat " " (List.map (fun f -> f.name) topfun_common))
    end;
    let filter_common ast = fst (hide_function_bodies (fun f -> List.mem f topfun_common) ast) in
    let new_astBefore = filter_common astBefore in
    let new_astAfter = filter_common astAfter in
    (new_astBefore, new_astAfter)

(** [process_ast_before_after_for_diff astBefore astAfter] massages the arguments,
    in case the flag [use_light_diff] is set, to remove the bodies of functions that
    are identical in astBefore and astAfter. Bodies are not removed if the output
    styles are not the same for both ASTs. *)
let process_ast_before_after_for_diff (style_before : output_style) (style_after : output_style) (ast_before : trm) (ast_after : trm) : trm * trm  =
  if !Flags.use_light_diff && (style_before = style_after)
      then light_diff ast_before ast_after
      else ast_before, ast_after


(******************************************************************************)
(*                             Trace management                               *)
(******************************************************************************)

(** [context]: contains general information about:
   - the source code that was loaded initially using [set_init_file],
   - the prefix of the filenames in which to output the final result using [dump] *)
type context = {
  prefix : string;
  extension : string;
  header : string;
}

(** [contex_dummy]: used for [trace_dummy]. *)
let context_dummy : context = {
  prefix = "";
  extension = "";
  header = ""
}

(** [step_kind] : classifies the kind of steps.*)
type step_kind =
  | Step_root (* root step created by [init] *)
  | Step_big (* produced by a [bigstep] call, via [open_big_step] *)
  | Step_small (* produced by a small-step [!!], via [open_small_step] *)
  | Step_transfo (* produced by [transfo_step] *)
  | Step_target_resolve (* produced by [target_resolve_step], for target_iter *)
  | Step_mark_manip (* produced during target_iter *)
  | Step_io (* produced by [io_step] *)
  | Step_group (* produced in particular by [backtrack] steps, or [target_iter] steps *)
  | Step_backtrack (* [backtrack] or [backtrack_on_failure] *)
  | Step_show (* produced by [show_step] *)
  | Step_typing (* produced by [step] *)
  | Step_trustme (* produced by [trustme] *)
  | Step_change (* change step introduced by [finalize_step] -- TODO: add this feature *)
  | Step_error (* fatal error caught *)


(** [step_kind_to_string] converts a step-kind into a string *)
let step_kind_to_string (k:step_kind) : string =
  match k with
  | Step_root -> "Root"
  | Step_big -> "Big"
  | Step_small -> "Small"
  | Step_transfo -> "Transfo"
  | Step_target_resolve -> "Target"
  | Step_mark_manip -> "Mark-manip"
  | Step_io -> "IO"
  | Step_group -> "Group"
  | Step_backtrack -> "Backtrack"
  | Step_show -> "Show"
  | Step_typing -> "Typing"
  | Step_trustme -> "Trustme"
  | Step_change -> "Change"
  | Step_error -> "Error"

(** [step_infos] *)
type step_infos = {
  mutable step_id : int;
  mutable step_script : string;
  mutable step_script_line : int option;
  mutable step_time_start : float; (* seconds since start *)
  mutable step_exectime : float; (* seconds *)
  mutable step_name : string;
  mutable step_args : (string * string) list;
  mutable step_flag_check_validity : bool; (* state of flag check_validity at start; must be the same at end *)
  mutable step_valid : bool;
  mutable step_justif : string list; (* accumulated in reverse order during the step *)
  mutable step_tags : string list; (* accumulated in reverse order during the step *)
  mutable step_debug_msgs : string list; (* accumulated in reverse order during the step *)
}

(** [step_tree]: history type used for storing all the trace information about all steps, recursively. *)
type step_tree = {
  mutable step_kind : step_kind;
  mutable step_context : context;
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
   Any call to the [step] function adds a copy of [cur_ast] into [history].
  TODO: Rename to open_trace? *)
type trace = {
  mutable next_step_id : int;
  mutable cur_context : context;
  mutable cur_ast : trm;
  mutable cur_style : output_style;
  mutable cur_ast_typed : bool;
  mutable step_stack : step_stack; (* stack of open steps *)
}

(** [trm_dummy]: dummy trm. *)
let trm_dummy : trm =
  trm_lit Lit_unit

(** [the_trace]: the trace produced by the current script. *)
let the_trace : trace = {
  next_step_id = 0;
  cur_context = context_dummy;
  cur_ast = trm_dummy; (* dummy *)
  cur_style = Style.default_style();
  cur_ast_typed = true;
  step_stack = []; (* dummy *)
}

let next_step_id () =
  let id = the_trace.next_step_id in
  the_trace.next_step_id <- id + 1;
  id

(** [is_trace_dummy()]: returns whether the trace was never initialized. *)
let is_trace_dummy () : bool =
  the_trace.cur_context == context_dummy

(** [get_decorated_history]: gets history from trace with a few meta information *)
(* TODO: remove this function? *)
let get_decorated_history ?(prefix : string = "") () : string * context * step_tree =
  let ctx = the_trace.cur_context in
  let prefix = (* LATER: figure out why we needed a custom prefix here *)
    if prefix = "" then ctx.prefix else prefix in
  let tree =
    match the_trace.step_stack with
    | [] -> raise (TraceFailure "step stack must never be empty")
    | [tree] -> tree
    | _ -> raise (TraceFailure "step stack contains more than one step; this should not be the case when a transformation script has completed")
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
    (* beware that the sublist might be in normal order or reverse order
       depending on whether the step has already been closed or not
       -- LATER: we should have the information on whether it is closed... *)
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
      | _ -> raise (TraceFailure "Invalid argument for [expected_kind] in [check_tree]")
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
      | [changestep; substep] when changestep.step_kind = Step_change -> ()
      | [substep; changestep] when changestep.step_kind = Step_change -> ()
          (* the end of the backtrack step restores the previous ast *)
      | _ -> err (sprintf "A backtrack step should have exactly one substep, %d found for %d" (List.length sub) (step.step_infos.step_id))
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


(** [filename_before_clang_format filename] takes as input a string
    such as "foo.cpp" and returns "foo_notfmt.cpp". This filename
    is meant to store the file before it is reformated using clang-format. *)
let filename_before_clang_format (filename:string) : string =
  let base =
    try Filename.chop_suffix filename ".cpp"
    with _ -> failwith "filename_before_clang_format: expects a .cpp file, provided: %s." filename in
  base ^ "_notfmt.cpp"

(* LATER: document and factorize *)

let style_normal_code () =
  Style.default_style ()

let style_resources ?(print_var_id : bool option) () = (*TODO factorize with Show.res *)
  let cstyle = Ast_to_c.(default_style()) in
  let typing_style = if !Flags.detailed_resources_in_trace then Style.typing_all else Style.typing_ctx in
  Style.({ decode = false;
    typing = { typing_style with print_generated_res_ids = true } ;
    print = Lang_C { cstyle with
      print_var_id = Option.value ~default:cstyle.print_var_id print_var_id;
      optitrust_syntax = true; } })


(** [cleanup_cpp_file_using_clang_format filename]: makes a system call to
   reformat a CPP file using the clang format tool.
   LATER: find a way to remove extra parentheses in ast_to_doc, by using
   priorities to determine when parentheses are required. *)
let cleanup_cpp_file_using_clang_format ?(uncomment_pragma : bool = false) (filename : string) : unit =
  stats ~name:(Printf.sprintf "cleanup_cpp_file_using_clang_format(%s)" filename) (fun () ->
    (* If requested, save a copy of the file "foo.cpp" into "foo_notfmt.cpp" before reformating "foo.cpp" *)
    if !Flags.keep_file_before_clang_format then begin
      let orig_filename = filename_before_clang_format filename in
      ignore (Sys.command (sprintf "cp %s %s" filename orig_filename));
    end;
    (*DEPRECATED ignore (Sys.command ("clang-format -style=\"Google\" -i " ^ filename));*)
    ignore (Sys.command (sprintf "clang-format -style=\"{BasedOnStyle: Google, ColumnLimit: %d}\" -i %s" !Flags.clang_format_nb_columns filename));
    if uncomment_pragma
      then ignore (Sys.command ("sed -i 's@//#pragma@#pragma@' " ^ filename))
  )

(** [get_header ()]: get the header of the current file (e.g. include directives) *)
let get_header () : string =
  the_trace.cur_context.header

(** [ensure_header]: ensures that the header [h] is included in the header of the current file. *)
(* FIXME: does not show in diff this way *)
let ensure_header (h : string) : unit =
  let ctx = the_trace.cur_context in
  let found = Tools.pattern_matches h (ctx.header) in
  if not found then
    the_trace.cur_context <- { ctx with header = ctx.header ^ h ^ "\n" }

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
    (* Convert contracts into code *)
    let fromto_style = C_encoding.style_of_output_style style in
    let ast = C_encoding.computed_resources_intro fromto_style ast in
    (* Optionally convert from OptiTrust to C syntax *)
    let ast =
      if style.decode then begin
        try
          C_encoding.cfeatures_intro fromto_style ast
        with
        | Scope_computation.InvalidVarId msg ->
          Tools.warn "output_prog could not decode due do invalid var ids: %s" msg;
          (* TODO: add comment in code or in trace by returning info to callers *)
          C_encoding.meta_intro fromto_style ast
      end else
        C_encoding.meta_intro fromto_style ast
      in
    (* Print the code into file, using the specified style *)
    let cstyle = match style.print with
      | Lang_AST _-> raise (TraceFailure "output_prog requires a Lang_C printing mode, not a Lang_AST")
      | Lang_C cstyle -> cstyle
      in
    Ast_to_c.ast_to_outchannel cstyle out_prog ast;
    output_string out_prog "\n";
    close_out out_prog;
  with | Failure _ as exn ->
    close_out out_prog;
    Printexc.(raise_with_backtrace exn (get_raw_backtrace ()))
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
        let style = Ast_to_text.{ style_full with print_var_id = !Flags.debug_var_id } in
        Ast_to_text.print_ast style out_ast ast;
        output_string out_ast "\n";
        close_out out_ast;
      end;
      (* Print the non-decoded ast *)
      output_string out_enc ctx.header;
      let style = Ast_to_c.default_style () in
      Ast_to_c.ast_to_outchannel { style with optitrust_syntax = true } out_enc ast;
      output_string out_enc "\n";
      close_out out_enc;
      if use_clang_format
        then cleanup_cpp_file_using_clang_format file_enc;
    with | Failure s ->
      close_out out_ast;
      close_out out_enc;
      failwith "%s" s
    end
  end

(******************************************************************************)
(*                                   Reparse                                  *)
(******************************************************************************)

(** [reparse_trm ctx ast]: prints [ast] in a temporary file and reparses it using Clang. *)
let reparse_trm ?(info : string = "") (ctx : context) (ast : trm) : trm =
  (* Disable caching for reparsing *)
  Flags.with_flag Flags.debug_parsing_serialization false (fun () ->

    if !Flags.debug_reparse then begin
      let info = if info <> "" then info else "of a term during the step starting at" in
      Tools.debug "Reparse: %s." info;
      flush stdout
    end;
    let in_prefix = (Filename.dirname ctx.prefix) ^ "/tmp_" ^ (Filename.basename ctx.prefix) in
    output_prog (Style.style_for_reparse()) ~beautify:false ctx in_prefix ast;

    let (_, t) = parse ~persistant:false (in_prefix ^ ctx.extension) in
    (*let _ = Sys.command ("rm " ^ in_prefix ^ "*") in*)
    t
  )

let reparse_ast ?(update_cur_ast : bool = true) ?(info : string = "the code during the step starting at") () =
  let tnew = reparse_trm ~info the_trace.cur_context the_trace.cur_ast in
  if update_cur_ast then begin
    the_trace.cur_ast <- tnew;
    the_trace.cur_ast_typed <- false;
  end



(******************************************************************************)
(*                               Step management                              *)
(******************************************************************************)

(** [get_cur_step ()] returns the current step --there should always be one. *)
let get_cur_step ?(error : string = "get_cur_step: empty stack") () : step_tree =
  match the_trace.step_stack with
  | [] -> failwith "%s" error
  | step::_ -> step

(** [open_root_step] is called only by [Trace.init], for initializing
   the bottom element of the [step_stack].
   Assumes fields of [the_trace] have already been initialized. *)
let open_root_step ?(source : string = "<unnamed-file>") () : unit =
  assert(the_trace.step_stack = [] && the_trace.next_step_id = 0);
  let step_root_infos = {
    step_id = next_step_id();
    step_script = "Trace for " ^ source;
    step_script_line = None;
    step_time_start = now();
    step_exectime = dummy_exectime;
    step_name = "";
    step_args = [("extension", the_trace.cur_context.extension) ];
    step_justif = [];
    step_flag_check_validity = !Flags.check_validity;
    step_valid = false;
    step_tags = [];
    step_debug_msgs = [];
  } in
  let step_root = {
    step_kind = Step_root;
    step_context = the_trace.cur_context;
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
        then raise (TraceFailure "get_root_step: close_root_step has not been called");
      step
  | _ -> raise (TraceFailure "close_root_step: broken invariant, stack must have size one")

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
    step_id = next_step_id();
    step_script = step_script;
    step_script_line = line;
    step_time_start = now();
    step_exectime = dummy_exectime;
    step_name = name;
    step_args = [];
    step_justif = [];
    step_flag_check_validity = !Flags.check_validity;
    step_valid = valid;
    step_tags = tags;
    step_debug_msgs = [];
  } in
  let step = {
    (* fields set at the start of the step *)
    step_kind = kind;
    step_context = the_trace.cur_context;
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
  if !debug_open_close_step
    then Tools.debug "%sTrace.open_step [%d]: %s (%s)" (String.make (List.length the_trace.step_stack) ' ') step.step_infos.step_id (step_kind_to_string kind) name;
  step

(** [change_step] helps creating a [Step_change] during [finalize]. *)
let change_step ~(ast_before:trm) ~(style:output_style) ~(ast_after:trm) ~(time_start : float) ~(step_exectime : float) ~(flag_check_validity:bool) : step_tree =
  let infos = {
    step_id = next_step_id();
    step_script = "";
    step_script_line = None;
    step_time_start = time_start;
    step_exectime = step_exectime;
    step_name = "Changed AST directly";
    step_args = [];
    step_justif = [];
    step_flag_check_validity = flag_check_validity;
    step_valid = false;
    step_tags = [];
    step_debug_msgs = [];
  } in
  { step_kind = Step_change;
    step_context = the_trace.cur_context;
    step_ast_before = ast_before;
    step_style_before = style;
    step_sub = [];
    step_infos = infos;
    step_ast_after = ast_after;
    step_style_after = style; }

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
let step_arg ?(name:string="") (value:string) : unit =
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
  () (* DEPRECATED: tag "should_be_valid_by_composition" *)

(** [tag_simpl_arith] is called by a transformation after open_step to indicate that it performs arithmetic simplifications. This can be used for filtering trace display. *)
let tag_simpl_arith () : unit =
  tag "simpl";
  tag "simpl_arith"

(** [tag_simpl_access] is called by a transformation after open_step to indicate that it performs array/matrix access simplificatoins.
  *)
let tag_simpl_access () : unit =
  tag "simpl";
  tag "simpl_access"

(** [without_substep_validity_checks f] executes [f] with
    the flag [check_validity] temporarily set to false.
    Only for internal use; user scripts should use the [trustme] function. *)
let without_substep_validity_checks (f: unit -> 'a): 'a =
  Flags.with_flag Flags.check_validity false f

(** [make_substeps_chained step] Finalize the list of substeps of [step],
    by inserting [Step_change] steps where the ast was modified directly
    in-between steps, to ensure that from [ast_before] we reach [ast_after]
    by applying the series of substep, each substep starting from the same
    physical ast as the one produced by the previous step. *)
let make_substeps_chained (step:step_tree) : unit =
  let flag_check_validity = step.step_infos.step_flag_check_validity in
  let style = step.step_style_before in
  let before (s:step_tree) : trm =
    s.step_ast_before in
  let after (s:step_tree) : trm =
    s.step_ast_after in
  let time_start (s:step_tree) : float =
    s.step_infos.step_time_start in
  let time_stop (s:step_tree) : float =
    s.step_infos.step_time_start +. s.step_infos.step_exectime in
  let newsubrev = ref [] in
  let cur_ast = ref step.step_ast_before in
  let cur_time = ref (time_start step) in
  let process (substep:step_tree) : unit =
    if before substep != !cur_ast then begin
      let changestep = change_step ~ast_before:(!cur_ast) ~ast_after:(before substep)
        ~time_start:(!cur_time) ~step_exectime:(time_start substep -. !cur_time)
        ~flag_check_validity ~style in
        (* or style:(Style.default_custom_style()) *)
      Tools.ref_list_add newsubrev changestep;
    end;
    Tools.ref_list_add newsubrev substep;
    (* Recall that target_resolve step have their "ast_after" hacked for display *)
    if substep.step_kind <> Step_target_resolve
      then cur_ast := after substep;
    cur_time := time_stop substep;
    in
  List.iter process step.step_sub; (* [step_sub] assumed already in final order *)
  (* If there are no substeps at all, it would be redundant to create a direct AST change *)
  if step.step_sub <> [] && !cur_ast != step.step_ast_after then begin
    let changestep = change_step ~ast_before:(!cur_ast) ~ast_after:step.step_ast_after
        ~time_start:(!cur_time) ~step_exectime:(time_stop step -. !cur_time)
        ~flag_check_validity ~style in
    Tools.ref_list_add newsubrev changestep;
  end;
  step.step_sub <- List.rev !newsubrev

(* DEPRECATED? *)
let is_saved_step step =
  match step.step_kind with
  | Step_root | Step_big | Step_small | Step_transfo | Step_group | Step_typing | Step_io | Step_trustme | Step_change -> true
  | Step_target_resolve | Step_mark_manip | Step_backtrack | Step_error | Step_show -> false

(* DEPRECATED? *)
let last_recorded_ast step: trm =
  let rec browse_steps steps =
    match steps with
    | [] -> step.step_ast_before
    | last_step :: _ when is_saved_step last_step -> last_step.step_ast_after
    | _ :: previous_steps -> browse_steps previous_steps
  in
  browse_steps step.step_sub

(** [is_kind_preserving_code kind] returns a boolean indicating whether
    the steps of this [kind] may modify the underlying source code.
    Beware that a [Step_backtrack] is preserving. *)
let is_kind_preserving_code (kind:step_kind) : bool =
  match kind with
  | Step_typing | Step_io | Step_target_resolve | Step_mark_manip
  | Step_backtrack | Step_error | Step_show ->
      true
  | Step_root | Step_big | Step_small | Step_transfo
  | Step_group | Step_trustme | Step_change ->
      false

(** [finalize_step] is called by [close_root_step] and [close_step] *)
let rec finalize_step ~(on_error: bool) (step : step_tree) : unit =
  let infos = step.step_infos in
  (* Handle retyping and reparse operations at the end of every step,
     except for steps that do not modify the current ast *)
  let same_ast_as_last_step =
    match step.step_sub with
    | [] -> step.step_ast_before == the_trace.cur_ast
    | last_step :: _ -> last_step.step_ast_after == the_trace.cur_ast
  in
  if not same_ast_as_last_step
    && not (is_kind_preserving_code step.step_kind) then begin
        (* TODO: Wrap error message without losing trace *)
        if !Flags.reparse_between_steps
          then reparse ();
        if !Flags.recompute_resources_between_steps
          then recompute_resources ()
  end;
  (* Save the ast_after and its style *)
  step.step_ast_after <- the_trace.cur_ast;
  step.step_style_after <- the_trace.cur_style;
  (* Flip lists that have been accumulated in reverse order during the step *)
  infos.step_args <- List.rev infos.step_args;
  infos.step_tags <- List.rev infos.step_tags;
  infos.step_debug_msgs <- List.rev infos.step_debug_msgs;
  infos.step_justif <- List.rev infos.step_justif;
  (* Finalize the list of substeps, by inserting [Step_change] steps
     to ensure a well-chained list of asts; unless it is a [Step_backtrack],
     or another kind of step that does not change the underlying code,
     in which case we do nothing. *)
  step.step_sub <- List.rev step.step_sub;
  if not (is_kind_preserving_code step.step_kind)
    then make_substeps_chained step;
  (* Check that [Flags.check_validity] is like at the start of the step *)
  if not on_error && !Flags.check_validity <> infos.step_flag_check_validity
    then raise (TraceFailure "At finalize_step, Flags.check_validity is not same as when step was opened.");
  (* Set the validity flag if it is not already set, in particular
     if the step is an identity step, or if all substeps are valid.
     (they have previously been ensured to form a chain).
     A [Step_trustme] is always considered invalid. *)
  if !Flags.check_validity then begin
    if step.step_kind = Step_trustme
      then step.step_infos.step_valid <- false
    else if not infos.step_valid
    && (   step.step_ast_before == step.step_ast_after
        || List.for_all (fun substep -> substep.step_infos.step_valid) step.step_sub)
    then step.step_infos.step_valid <- true;
  end;
  (* Set the tag "same-code" if the kind of the step is one that preserves the underlying code *)
  if is_kind_preserving_code step.step_kind
    then infos.step_tags <- "same-code"::infos.step_tags;
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
  (* Save the time *)
  infos.step_exectime <- now() -. infos.step_time_start


and without_reparsing_between_steps (f: unit -> unit): unit =
  Flags.with_flag Flags.reparse_between_steps false f;
  if !Flags.reparse_between_steps then reparse ()

and without_resource_computation_between_steps (f: unit -> 'a): 'a =
  Flags.with_flag Flags.recompute_resources_between_steps false f;
  if !Flags.recompute_resources_between_steps then recompute_resources ()

and preserves_resource_typing (f: unit -> 'a): 'a =
  let cur_ast_typed_snapshot = the_trace.cur_ast_typed in
  Flags.with_flag Flags.recompute_resources_between_steps false f;
  the_trace.cur_ast_typed <- cur_ast_typed_snapshot

(** [close_step] is called at the end of every big-step, or small-step,
   or combi, or basic transformation. The step to close can be passed
   as an optional argument, to check that the exected step is being closed.
   If all substeps are valid and their sequence explains how to go from ast_before to ast_after, the step is valid by the explaination "combination of valid steps" *)
and close_step ?(discard = false) ?(on_error = false) ?(check:step_tree option) () : unit =
  match the_trace.step_stack with
  | [] -> raise (TraceFailure "close_step: the_trace should not be empty")
  | [root_step] -> raise (TraceFailure "close_step: on the root, should call close_root_step")
  | step :: ((parent_step :: _) as stack_tail)  ->
      (* Checking that we close the expected step *)
      begin match check with
      | None -> ()
      | Some opened_step ->
          if step != opened_step
            then raise (TraceFailure "close_step: not closing the expected step")
      end;
      if !debug_open_close_step
        then Tools.debug "%sTrace.close_step[%d]: %s" (String.make (List.length the_trace.step_stack) ' ') step.step_infos.step_id (step_kind_to_string step.step_kind);
      if discard then
        (* Discarded step id should be reused to prevent creating holes in recorded step ids.
           Non contiguous step ids break the JS displaying the trace. *)
        the_trace.next_step_id <- step.step_infos.step_id
      else begin
        (* Finalize the step, by reversing the list of substeps and computing validity *)
        finalize_step ~on_error step;
        (* Folding step into parent substeps *)
        parent_step.step_sub <- step :: parent_step.step_sub;
      end;
      the_trace.step_stack <- stack_tail;
      (* In debug mode, check the trace invariant *)
      if !check_trace_at_every_step
        then check_the_trace ~final:false

(** [step_and_get_handle] is a function wrapping the body of a transformation,
    like [step] but also returns the object describing the step *)
and step_and_get_handle ?(valid:bool=false) ?(line : int = -1) ?(tags:string list=[]) ~(kind:step_kind) ~(name:string) (body : unit -> 'a) : 'a * step_tree =
  let s = open_step ~valid ~line ~tags ~kind ~name () in
  let r = body () in
  assert (get_cur_step () == s);
  close_step ~check:s ();
  r, s

(** [step] is a function wrapping the body of a transformation *)
and step ?(valid:bool option) ?(line : int option) ?(tags:string list option) ~(kind:step_kind) ~(name:string) (body : unit -> 'a) : 'a =
  let r, _s = step_and_get_handle ?valid ?line ?tags ~kind ~name body in
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
    infos.step_name <- prefix ^ " " ^ infos.step_name
  in
  let process_context (contexts : error_context list) : unit =
    List.iter (fun c ->
      Option.iter (fun p ->
        let mark = Mark.next () in
        let (p, add_mark) = match Path.extract_last_dir p with
        | p, Path.Span s -> (p, ref (trm_add_mark_span s mark))
        | p, Path.Before i -> (p, ref (trm_add_mark_between i mark))
        | _ -> (p, ref (trm_add_mark mark))
        in
        let prefix = ref p in
        let prefix_invalid = ref true in
        (* TODO: factorize this code in Path. module *)
        while !prefix_invalid do
          try
            the_trace.cur_ast <- Path.apply_on_path !add_mark the_trace.cur_ast !prefix;
            prefix_invalid := false;
          with
          | _ -> (* TODO: more precise catch ? *)
            add_mark := trm_add_mark mark;
            prefix := Path.parent !prefix
        done;
        let prefix_len = List.length !prefix in
        if prefix_len == List.length p
          then preprend_to_step_name ("@ path " ^ mark)
          else preprend_to_step_name ("@ path " ^ mark ^ "+" ^ (Path.path_to_string (List.drop prefix_len p)));
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
        preprend_to_step_name ("@ term " ^ mark);
      ) c.trm;
      (* TODO: c.loc *)
      if c.ctx_desc <> "" then
        preprend_to_step_name (c.ctx_desc);
    ) (List.rev contexts)
  in
  let print_var_id = ref false in
  let rec process (exn : exn) : unit =
    match exn with
    | Scope_computation.InvalidVarId _ ->
      print_var_id := true
    | Contextualized_error (context, exn) ->
      process exn;
      process_context context;
    | Failure msg ->
      preprend_to_step_name msg
    | _ ->
      preprend_to_step_name (Printexc.to_string exn)
  in
  let (), s =
    step_and_get_handle ~valid:false ~kind:Step_error ~name:"" (fun () -> process exn) in
  s.step_style_before <- style_normal_code();
  s.step_style_after <- style_resources ~print_var_id:!print_var_id ()

(** [typing_step f] adds a step accounting for a typing recomputation *)
and typing_step ~name (f : unit -> unit) : unit =
  let (), s =
    step_and_get_handle ~valid:true ~kind:Step_typing ~name ~tags:["typing"] f in
  s.step_style_before <- style_normal_code();
  s.step_style_after <- style_resources()

(** [reparse ()]: function takes the current AST, prints it to a file, and parses it
   as if it was a fresh input. Doing so ensures in particular that all the type
   information is properly set up.
   WARNING: reparsing discards all the marks in the AST. *)
and reparse ?(update_cur_ast = true) ?(info : string option) ?(parser: parser option) () : unit =
  parsing_step (reparse_ast ~update_cur_ast ?info)

and recompute_resources (): unit =
  if not the_trace.cur_ast_typed then
    typing_step ~name:"Resource recomputation" recompute_resources_on_ast

and recompute_resources_on_ast () : unit =
  if not !Flags.resource_typing_enabled then failwith "Cannot compute resources when resource typing is disabled";
  let t = Scope_computation.infer_var_ids the_trace.cur_ast in (* Resource computation needs var_ids to be calculated *)
  (* Compute a typed AST *)

  (* Printf.printf "%s\n" (AstC_to_c.ast_to_string ~style (Ast_fromto_AstC.(meta_intro ~skip_var_ids:true (style_of_custom_style custom_style)) t)); *)
  try
    the_trace.cur_ast <- Resource_computation.trm_recompute_resources Resource_set.empty t;
    the_trace.cur_ast_typed <- true;
  with (Resource_computation.ResourceError (t_with_error, _phase, _exn)) as e ->
    (* TODO: Resources computation warning when failing in non critical contexts:
    let (), s = step_and_get_handle ~valid:true ~kind:Step_error ~name:"Typing error" (fun () -> the_trace.cur_ast <- t) in
    s.step_style_before <- style_normal_code();
    s.step_style_after <- style_resources();
    *)
    the_trace.cur_ast <- t_with_error;
    Printexc.(raise_with_backtrace e (get_raw_backtrace ()))

(** [retypecheck] is currently implemented as [reparse], but in the future it
   would use a dedicated typechecker. *)
let retypecheck ?(info : string option) ?(parser: parser option) () =
  typing_step ~name:"Retypecheck" (reparse_ast ?info)

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
let close_root_step ~(on_error: bool) () : unit =
  close_bigstep_if_needed();
  let step = match the_trace.step_stack with
    | [step] -> step
    | _ -> raise (TraceFailure  "close_root_step: broken invariant, stack must have size one") in
  finalize_step ~on_error step

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
  let ast_snapshot_typed = the_trace.cur_ast_typed in
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
  the_trace.cur_ast_typed <- ast_snapshot_typed;
  justif "step-backtrack restored the ast";
  close_step ~discard:discard_after ~check:step_backtrack ();
  res

type 'a backtrack_result =
| Success of 'a
| Failure of exn * Printexc.raw_backtrace

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
  let ast_snapshot_typed = the_trace.cur_ast_typed in
  let step_backtrack = open_step ~kind:Step_backtrack ~name:"step-backtrack-on-failure" () in
  let step_group = open_step ~kind:Step_group ~name:"step-backtrack-on-failure-group" () in
  let res =
    try
      let x = f() in
      (* Close the group step *)
      close_step ~check:step_group ();
      Success x
    with e -> begin
      let bt = Printexc.get_raw_backtrace () in
      (* Close all the steps that have been interrupted by the exception *)
      let error = "Trace.step_backtrack_on_failure: unable to close the group" in
      while (get_cur_step ~error () != step_group) do
        close_step ()
      done;
      (* Close the group step *)
      close_step ~check:step_group ();
      Failure (e, bt)
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
    the_trace.cur_ast_typed <- ast_snapshot_typed;
    justif "step-backtrack-on-failure has backtracked and restored the ast";
    close_step ~discard:discard_on_failure ~check:step_backtrack ();
  end;
  (* Return a description of the result of [f] *)
  res

(** [target_resolve_step] has a special handling because it saves a diff
   between an AST and an AST decorated with marks for targeted paths,
   even though the [cur_ast] is not updated with the marks. *)
let target_resolve_step ?(prefix : Path.path = []) (f: unit -> Path.path list) : Path.path list =
  ignore (open_step ~valid:true ~kind:Step_target_resolve ~tags:["target"] ~name:"Target-resolve" ());
  let ps = f () in
  if Flags.is_execution_mode_trace() then begin
    let cur_ast = the_trace.cur_ast in
    let marked_ast, _marks = Path.add_marks_at_paths ~prefix ps cur_ast in
    the_trace.cur_ast <- marked_ast;
    close_step();
    the_trace.cur_ast <- cur_ast
  end else begin
    close_step();
  end;
  ps

(** [target_iter_step] is for wrapping substeps that are together. *)
let step_group (name: string) (f: unit -> unit): unit =
  step ~kind:Step_group ~name f

(** [target_iter_step] is for wrapping the processing of one among several
    targets. *)
let target_iter_step (istep : int) (f: unit->unit) : unit =
  step_group (sprintf "Target #%d" istep) f


(** [invalidate()]: restores the global state (object [trace]) in its uninitialized state,
   like at the start of the program.  *)
let invalidate () : unit =
  close_logs();
  the_trace.next_step_id <- 0;
  the_trace.cur_context <- context_dummy;
  the_trace.cur_ast <- trm_dummy;
  the_trace.cur_style <- Style.default_style();
  the_trace.cur_ast_typed <- true;
  the_trace.step_stack <- []

(** [get_initial_ast filename]: gets the initial ast before applying any trasformations
     [filename] - filename of the source code
     returns header and ast. *)
let get_initial_ast (filename : string) : (string * trm) =
  parse filename

(** [init f]: initializes the trace with the contents of the file [f].
   This operation should be the first in a transformation script.
   The history is initialized with the initial AST.
   [~prefix:"foo"] allows to use a custom prefix for all output files,
   instead of the basename of [f].
   style is computed based on the global flags.   *)
let init ~(prefix : string) ~(program : string) (filename : string) : unit =
  ast_just_before_first_call_to_restore_original := None; (* TEMPORARY HACK *)
  invalidate ();
  let basename = Filename.basename filename in
  let extension = Filename.extension basename in
  let script_filename = program ^ ".ml" in
  (* TODO: could optimize the setting of the flag only_big_step by
     testing the target line only for "bigstep", without computing
     all of compute_ml_file_excerpts *)
  if !Flags.analyse_stats || Flags.is_execution_mode_trace() then begin
    if Sys.file_exists script_filename then begin
      let lines = File.get_lines script_filename in
      (* printf "%s\n" (Tools.list_to_string ~sep:"\n" lines); *)
      ml_file_excerpts := compute_ml_file_excerpts lines;
      (* Automatically set the flag [only_big_steps] if targeted line  *)
      if Flags.is_execution_mode_step() then begin
        let line_num = Flags.get_target_line() in
        let line_str = get_excerpt line_num in
        let regexp_bigstep = Str.regexp "^[ ]*\\(bigstep\\)" in
        let starts_with_bigstep = Str.string_match regexp_bigstep line_str 0 in
        if starts_with_bigstep then begin
          Tools.debug "Reporting diff for big-step";
          Flags.only_big_steps := true;
        end
      end;
    end;
  end;
  start_stats := get_cur_stats ();
  last_stats := !start_stats;

  init_logs prefix;

  let (header, cur_ast), stats_parse = Stats.measure_stats (fun () -> get_initial_ast filename) in

  let context = { extension; prefix; header } in
  the_trace.next_step_id <- 0;
  the_trace.cur_context <- context;
  the_trace.cur_ast <- cur_ast;
  the_trace.cur_ast_typed <- false;
  the_trace.cur_style <- Style.default_style();
  the_trace.step_stack <- [];
  open_root_step ~source:program ();

  (* If recompute resources between steps is enabled, we need resources to be computed after the initial parsing as well *)
  if !Flags.recompute_resources_between_steps then recompute_resources ();

  Flags.verbose_info "Starting script execution..."

(** [get_last_substep] returns the last substep, which corresponds to the
    step to be visualized when interactively targeting a given line *)
let get_last_substep () : step_tree =
  match (get_cur_step ()).step_sub with
  | [] -> raise (TraceFailure ("Trace.get_last_substep: expected a previous substep in the current step"))
  | last_step :: _ -> last_step

(** [get_original_ast] returns the ast obtained at [Trace.init] *)
let get_original_ast () : trm =
  assert (the_trace.step_stack <> []);
  let (_, root_step) = List.unlast the_trace.step_stack in
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
      | e ->
        let bt = Printexc.get_raw_backtrace () in
        if h e then () else Printexc.raise_with_backtrace e bt
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
    then raise (TraceFailure "Trace.init must be called prior to any transformation.");
  let ast_snapshot = the_trace.cur_ast in
  the_trace.cur_ast <- f the_trace.cur_ast;
  if ast_snapshot != the_trace.cur_ast then
    the_trace.cur_ast_typed <- false

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

(* Only for debugging purposes.
  [trace_custom_postprocessing] is a function applied to all ast-after that
  are dumped in the trace *)
let trace_custom_postprocessing : (trm -> trm) ref = ref (fun t -> t)

(* EXAMPLE POSTPROCESSING: display the type of every statement;
   place this definition at the top of your script.

      let _ = Trace.trace_custom_postprocessing := (fun t ->
        let tg = [nbAny; cPred trm_is_statement] in
        let ps = resolve_target tg t in
        let markof _pi ti =
          match ti.typ with
          | Some ty -> Ast_to_c.typ_to_string ty
          | None -> "-" in
        Target.trm_add_mark_at_paths markof ps t)

  Example: show the address of each AST
  let _ = Trace.trace_custom_postprocessing := (fun t ->
    let markof _pi ti = Tools.pointer_to_string ti in
    Target.trm_add_mark_at_paths markof [[]] t
    )
*)

let code_to_temp_file ?(temp_prefix="code") (style:output_style) (ctx: context) (ast: trm): string =
  (* Patch the AST with custom processing, only for debugging purposes *)
  let ast = !trace_custom_postprocessing ast in
  let filename = Filename.temp_file temp_prefix ctx.extension in
  let prefix = Filename.remove_extension filename in
  output_prog style ctx prefix ast;
  filename

let get_code ?(temp_prefix) (style:output_style) (ctx: context) (ast: trm): string =
  let filename = code_to_temp_file ?temp_prefix style ctx ast in
  let prog = File.get_contents filename in
  Sys.remove filename;
  prog

(** [get_code_before s] returns the code before the step [s] starts as a string. *)
let get_code_before ?(style:output_style option) (s:step_tree) : string =
  let style = Option.value ~default:s.step_style_before style in
  get_code ~temp_prefix:"before" style s.step_context s.step_ast_before

(** [get_code_after s] returns the code after the step [s] ends as a string. *)
let get_code_after ?(style:output_style option) (s:step_tree) : string =
  let style = Option.value ~default:s.step_style_after style in
  get_code ~temp_prefix:"after" style s.step_context s.step_ast_after

let compute_before_after_and_diff ?(style:output_style option) ~(drop_before_after: bool) (s:step_tree) : string * string * string =
  (* Handle the light-diff feature, which eliminates top-level functions that
     are physically identical in the AST before and after *)
  let ast_before, ast_after = process_ast_before_after_for_diff s.step_style_before s.step_style_after s.step_ast_before s.step_ast_after in

  (* Dump the two files, and evaluate the diff command *)
  let style_before = Option.value ~default:s.step_style_before style in
  let before_file = code_to_temp_file ~temp_prefix:"before" style_before s.step_context ast_before in
  let style_after = Option.value ~default:s.step_style_after style in
  let after_file = code_to_temp_file ~temp_prefix:"after" style_after s.step_context ast_after in
  let sDiff = Tools.get_process_output (sprintf "git diff --ignore-all-space --no-index -U10 %s %s" before_file after_file) in
  let sBefore = if drop_before_after then "" else File.get_contents before_file in
  let sAfter = if drop_before_after then "" else File.get_contents after_file in
  Sys.remove before_file;
  Sys.remove after_file;
  sBefore, sAfter, sDiff

(** [compute_diff s] returns the string describing the diff associated with the step [s].
    The AST are printed using the style_before and style_after of [s]. *)
let compute_diff ?(style:output_style option) (s:step_tree) : string =
  let _, _, diff = compute_before_after_and_diff ?style ~drop_before_after:true s in
  diff

(** [compute_before_after_and_diff s] returns not just the diff, but also the contents
    of the files that correspond to the AST-before and AST-after for the step [s]. *)
let compute_before_after_and_diff ?(style:output_style option) (s:step_tree) : string * string * string =
  compute_before_after_and_diff ?style ~drop_before_after:false s

(* LATER: optimize script to avoid computing twice the same ASTs for step[i].after and step[i+1].after *)
(** [dump_step_tree_to_js] auxiliary function for [dump_trace_to_js] *)
let rec dump_step_tree_to_js ~(is_substep_of_targeted_line:bool) (root_id:int)(out:string->unit)  (s:step_tree) : unit =
  let i = s.step_infos in
  (* Determine whether diff needs to be computed for this step *)
  let is_mode_step_trace = Flags.is_execution_mode_step () in
  let is_smallstep_of_targeted_line =
       is_mode_step_trace
    && i.step_script_line = Some (Flags.get_target_line())
    in
  let is_substep_of_targeted_line =
      is_substep_of_targeted_line || is_smallstep_of_targeted_line in
    (* FOR FUTURE USE WITH LIGHT MODE
    || s.step_kind = Step_big
    || s.step_kind = Step_small *)
  let should_compute_diff =
      (not !Flags.serialize_trace)
      && ((not is_mode_step_trace) || is_substep_of_targeted_line)
      && (match !Flags.substeps_including_ast with
          | SubstepsAST_all -> true
          | SubstepsAST_small ->
              begin match s.step_kind with
              | Step_root | Step_big | Step_small -> true
              | _ -> false
              end
          | SubstepsAST_all_important ->
              begin match s.step_kind with (* select steps that deserve a diff in non-detailed mode, based on their kind *)
              | Step_root | Step_big | Step_small | Step_transfo | Step_trustme | Step_error | Step_show -> true
              | Step_typing | Step_mark_manip | Step_target_resolve | Step_change | Step_backtrack | Step_group | Step_io -> false
             end)
    in
  (* Recursive calls *)
  let aux = dump_step_tree_to_js ~is_substep_of_targeted_line root_id out in
  (* Dump Json for this node *)
  let sBefore, sAfter, sDiff =
    if should_compute_diff then begin
      try
        let sBefore, sAfter, sDiff = compute_before_after_and_diff s in
        Some sBefore, Some sAfter, Some sDiff
      with e ->
        (* Prevent any exception during printing to corrupt the entire trace *)
        let exn = Printexc.to_string e in
        Tools.warn "Error while saving trace:\n%s" exn;
        None, None, None
    end else
      None, None, None
    in
  let id = s.step_infos.step_id - root_id in
  let json = (* TODO: check if ~html_newlines:true is needed for certain calls to [Json.str] *)
    Json.obj_quoted_keys [
      "id", Json.int id;
      "kind", Json.str (step_kind_to_string s.step_kind);
      "exectime", Json.float i.step_exectime;
      "name", Json.str i.step_name;
      "script", Json.base64 (i.step_script);
      "script_line", Json.(optionof int) (if i.step_script_line = Some (-1) then None else i.step_script_line);
        (* TODO: avoid use of -1 for undef line *)
      "args", Json.(listof (fun (k,v) -> Json.obj_quoted_keys ["name", str k; "value",str v])) i.step_args;
      "check_validity", Json.bool i.step_flag_check_validity;
      "isvalid", Json.bool i.step_valid;
        (* TODO: at the moment, we assume that a justification item means is-valid *)
      "justif", Json.(listof str) i.step_justif;
      "tags", Json.(listof str) i.step_tags;
      "debug_msgs", Json.(listof str) i.step_debug_msgs;
      "sub", Json.(listof int) (List.map (fun sub -> sub.step_infos.step_id - root_id) s.step_sub);
      "code_before", Json.(optionof base64) sBefore;
      "code_after", Json.(optionof base64) sAfter;
      "diff", Json.(optionof base64) sDiff;
    ] in
  out (sprintf "steps[%d] = %s;\n" id (Json.to_string json));
  (* If this step is the targeted step, mention it as such *)
  if is_smallstep_of_targeted_line
    then out (sprintf "var startupOpenStep = %d;\n" id);
  (* Process sub-steps recursively *)
  List.iter aux s.step_sub


(** [dump_trace_to_js step]: writes into a file called [`prefix`_trace.js] the
   contents of the step tree [step]. The JS file is structured as follows
   (up to the order of the definitions):
{@js[
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
      astBefore: window.atob("..."), // could also an id of an source code stored in a different array, for improved factorization
      astAfter: window.atob("..."),
      diff: window.atob("..."), // could be slow if requested for all!
      sub: [ j1, j2, ... jK ]  // ids of the sub-steps
      }
]}
   *)
let dump_trace_to_js ?(prefix : string = "") ?(serialized_trace_timestamp : string option) (step:step_tree) : unit =
  let prefix =
    if prefix = "" then step.step_context.prefix else prefix in
  let filename = prefix ^ "_trace.js" in
  if !debug_notify_dump_trace then Tools.debug "Dumping trace to '%s'" filename;
  let out_js = open_out filename in
  let out = output_string out_js in
  (* Print additional information *)
  begin match serialized_trace_timestamp with
  | None ->
    out "var serialized_trace = null;\n"
  | Some timestamp ->
    let ser_filename = prefix ^ ".trace" in
    out (sprintf "\
      var serialized_trace = \"%s\";\n\
      var serialized_trace_timestamp = \"%s\";\n"
      ser_filename timestamp);
  end;
  (* Since step_ids are attributed in execution order they always correspond to depth first traversal.
    We also use depth first traversal step id in the JS trace.
    No matter which step is the JS root, there is a constant offset between serialized trace and JS step ids. *)
  let root_id = step.step_infos.step_id in
  out (sprintf "var root_serialized_step_id = %d;\n" root_id);
  (* Print the trace, each step produces a line of the form "step[i] = ...;" *)
  out "var steps = [];\n";
  dump_step_tree_to_js ~is_substep_of_targeted_line:false root_id out step;
  close_out out_js

(** [serialize_full_trace_and_get_timestamp] serializes the current trace object
    into a file named "prefix.trace", and return the timestamp of that file
    as a string. *)
let serialize_full_trace_and_get_timestamp ~(prefix : string) : string =
  let ser_filename = prefix ^ ".trace" in
  let out_file = open_out_bin ser_filename in
  Marshal.to_channel out_file (get_root_step()) [];
  close_out out_file;
  let timestamp = string_of_float ((Unix.stat ser_filename).st_mtime) in
  timestamp

(** [dump_full_trace_to_js ()] invokes [dump_trace_to_js] on the root step,
     and serialize the trace object into "prefix.trace" if the flag
     [serialized_trace] is set.
     In case of serialization, the JS file contains an additional line of the form:
     [var serialized_trace_timestamp = "...";] storing the timestamp of the
     serialized trace (as a string). *)
let dump_full_trace_to_js ~(prefix : string) : unit =
  let serialized_trace_timestamp =
    if !Flags.serialize_trace
      then Some (serialize_full_trace_and_get_timestamp ~prefix)
      else None
    in
  dump_trace_to_js ~prefix ?serialized_trace_timestamp (get_root_step())

(** [step_tree_to_doc step_tree] takes a step tree and gives a string
   representation of it, using indentation to represent substeps *)
let step_tree_to_doc (step_tree:step_tree) : document =
  let ident_width = 3 in
  let rec aux (depth:int) (s:step_tree) : document =
    let i = s.step_infos in
    let space = blank 1 in
    let tab = blank (depth * ident_width) in
       tab
    ^^ string (sprintf "%d:" i.step_id) ^^ space
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
let dump_trace_to_textfile ~(prefix : string) : unit =
  let prefix =
    if prefix = "" then the_trace.cur_context.prefix else prefix in
  let filename = prefix ^ "_trace.txt" in
  if !debug_notify_dump_trace then Tools.debug "Dumping trace to '%s'" filename;
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

(** [produce_diff_output_internal step] is an auxiliary function for [produce_diff_output]. *)
let produce_diff_output_internal (step:step_tree) : unit =
  let ctx = step.step_context in
  let prefix = ctx.prefix in
  (* Extract the two ASTs and the styles that should be used for the diff *)
  let ast_before, ast_after = step.step_ast_before, step.step_ast_after in
  let style_before, style_after = step.step_style_before, step.step_style_after in
  (* Handle light diffs *)
  let ast_before, ast_after = process_ast_before_after_for_diff style_before style_after ast_before ast_after in
  (* Common printing function *)
  let output_ast style filename_prefix ast =
    let style =
      if not !Flags.print_only_code then style else
        Style.{ style with typing = typing_none }
      in
    output_prog_check_empty style ctx filename_prefix ast;
    Flags.verbose_info "Generated: %s%s" filename_prefix ctx.extension;
    in
  (* Generate files. *)
  output_ast style_before (prefix ^ "_before") ast_before;
  output_ast style_after (prefix ^ "_after") ast_after;
  Flags.verbose_info "Writing ast and code into %s.js" prefix

(** [produce_trace_output step] is an auxiliary function for [produce_output_and_exit] *)
let produce_trace_output (step:step_tree) : unit =
  let prefix = step.step_context.prefix in
  dump_trace_to_js ~prefix step

(** [extract_show_step] extracts a [Step_show] nested as unique substep in depth
    of a step. *)
let rec extract_show_step (step:step_tree) : step_tree =
  if step.step_kind = Step_show then step else begin
    match step.step_sub with
    | [ substep ] -> extract_show_step substep
    | _ -> raise (TraceFailure "Trace.extract_show_step: did not find a Step_show in depth")
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
  Flags.verbose_info "Exiting script";
  (* Extract the step that should be used for the diff *)
  let container_step = get_cur_step() in
  if container_step.step_sub = []
    then raise (TraceFailure "produce_output_and_exit: make sure you cursor is on a line starting with '!!' or 'bigstep'");
  let step = get_last_substep () in
  if !Flags.only_big_steps && step.step_kind <> Step_big
    then raise (TraceFailure "produce_output_and_exit: cannot show a diff for a big-step, no call to bigstep was made");
  (* Output the step description *)
  begin match !Flags.execution_mode with
  | Execution_mode_step_diff -> produce_diff_output step
  | Execution_mode_step_trace -> produce_trace_output step
  | Execution_mode_exec
  | Execution_mode_full_trace -> raise (TraceFailure "produce_output_and_exit should be in a 'step' execution mode")
  end;
  (* Print debug messages of the current step *)
  List.iter (fun s -> Tools.debug "%s" s) step.step_infos.step_debug_msgs;
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
    Tools.debug "Executing bigstep %s%s"
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
   The backtrack step containing the show step is tagged with the tag "show",
   to easily identify it as such. *)
let show_step ?(name:string="show") ~(ast_left:trm) ~(style_left:output_style) ~(ast_right:trm) ~(style_right:output_style) () =
  step_backtrack ~tags:["show"] (fun () ->
    (* Create the show step *) (* LATER: use a "step_and_get_handle" function *)
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
    List.iter (fun (k, v) -> step_arg ~name:k v) args;
    f ();
    (* printf "< %s\n" name; *)
  )

(** [check_recover_original()]: checks that the AST obtained so far
   is identical to the input AST, obtained from parsing. If not,
   it raises an error. *)
let check_recover_original () : unit =
  let check_same cur orig =
    (* TODO: Do not convert to string to make the comparison. *)
    let orig_str = Ast_to_c.ast_to_string orig in
    let cur_str = Ast_to_c.ast_to_string cur in
    if cur_str <> orig_str then begin
      raise (TraceFailure "Trace.check_recover_original: the current AST is not identical to the original one")
    end else () (* FOR DEBUG: Printf.printf "check_recover_original: successful" *)
  in
  let orig_ast = get_original_ast () in
  call (fun cur_ast -> check_same cur_ast orig_ast)

(** [trustme msg f] executes [f] with the flag [check_validity]
    temporarily set to false. The string [msg] is stored in the
    [name] field of the step. It is intended to be a human-readable
    summary of what the transformation [f] intends to perform,
    and why it is preserves the semantics of the program. *)
let trustme (name : string) (f: unit -> 'a): 'a =
  (* TODO: figure out whether the call to [step] should be inside or outside
     of the call to [without_substep_validity_checks] *)
  step ~valid:false ~kind:Step_trustme ~name:("TRUSTME: " ^ name) (fun () ->
    without_substep_validity_checks f)


(******************************************************************************)
(*                                   FINALIZE                                 *)
(******************************************************************************)

(* TEMPORARY HACK *)
(** [restore_original ()] sets as current ast the original ast obtained
    after parsing, i.e. the [ast_before] of the root step. *)
let restore_original () : unit =
  if !ast_just_before_first_call_to_restore_original = None
    then ast_just_before_first_call_to_restore_original := Some the_trace.cur_ast;
  transfo_step ~name:"restore-original" ~args:[] (fun () ->
    the_trace.cur_ast <- get_original_ast();
    the_trace.cur_ast_typed <- false;
  )

(** [finalize()]: should be called at the end of the script to close the root step *)
let finalize ?(on_error = false) () : unit =
  (* TEMPORARY HACK for handling effects after a call to restore_original *)
  begin match !ast_just_before_first_call_to_restore_original with
  | None -> ()
  | Some ast ->
      transfo_step ~name:"restore-ast-before-first-restore-original" ~args:[] (fun () ->
        the_trace.cur_ast <- ast)
  end;
  (* END *)
  close_root_step ~on_error ();
  (* Check the trace invariant (optional) *)
  try check_the_trace ~final:true
  with Invalid_trace msg ->
    Tools.error "Trace.check_the_trace reports: %s" msg

(** [finalize_on_error()]: performs a best effort to close all steps after an error occurred *)
let finalize_on_error ~(exn: exn) : unit =
  Tools.error "%s" (Printexc.to_string exn); (* FIXME: not here? *)
  error_step exn;
  let rec close_all_steps () : unit =
    match the_trace.step_stack with
    | [] -> raise (TraceFailure "close_close_all_stepsstep: the_trace should not be empty")
    | [_root_step] -> finalize ~on_error:true ()
    | _step :: _ -> close_step ~on_error:true (); close_all_steps()
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
let dump (style : output_style) ?(store_in_trace = true) ?(prefix : string = "") ?(append_comments : string = "") () : unit =
  let action () =
    let ctx = the_trace.cur_context in
    let prefix =
      if prefix = "" then (* ctx.directory ^ *) ctx.prefix else prefix in
    output_prog style ctx (prefix ^ "_out") (the_trace.cur_ast);
    if append_comments <> "" then begin
      let filename = prefix ^ "_out.cpp" in
      File.append_contents filename ("/*\n" ^ append_comments ^ "\n*/\n");
    end
    in
  if store_in_trace
    then dumping_step action
    else action()

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
  if t != the_trace.cur_ast then
    the_trace.cur_ast_typed <- false;
  the_trace.cur_ast <- t

(** [set_ast_for_target_iter t] is used by [Target.iteri] when updating the marks
    using low-level mechanisms. *)
let set_ast_for_target_iter (t:trm) : unit =
  step ~valid:true ~kind:Step_mark_manip ~name:"Target-iter-mark-manipulation" ~tags:["target"]
    (fun () -> set_ast t)

(** [get_context ()]: returns the current context. Like [ast()], it should only be called
   within the scope of [Trace.apply] or [Trace.call]. *)
let get_context () : context =
  the_trace.cur_context

(** [get_style ()]: read the current style using for printing ASTs in the trace. *)
let get_style () : output_style =
  the_trace.cur_style

(** [set_style ()]: change the current style using for printing ASTs in the trace. *)
let set_style (style:output_style) : unit =
  the_trace.cur_style <- style

(** [update_style ()]: updates the current style using for printing ASTs in the trace
   by reading the flags. Call this function after modifying global flags. *)
let update_style () : unit =
  the_trace.cur_style <- Style.default_style()


(* LATER:  need to reparse to hide spurious parentheses *)
(* LATER: add a mechanism for automatic simplifications after every step *)


