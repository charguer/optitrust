
(******************************************************************************)
(*                        Debug                                               *)
(******************************************************************************)

(* include Tools.Debug *)
include Tools

let set_exn_backtrace (b : bool) : unit =
  Printexc.record_backtrace b

(* By default, we want backtrace for exceptions *)
let _ = set_exn_backtrace true



(******************************************************************************)
(*                              Run                                           *)
(******************************************************************************)

(* [script f] serves as "main" function for an Optitrust script. It cakes care
   of parsing the command line arguments and handling the errors, in addition
   to running the function [f] provided. *)
let script (f : unit -> unit) : unit =
  Arg.parse
    Flags.spec
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  try
    let t0 = Unix.gettimeofday() in
    f ();
    let t1 = Unix.gettimeofday() in
    if !Flags.analyse_time
      then Tools.printf "Script execution time: %d ms\n" (Tools.milliseconds_between t0 t1);
  with | Failure s | Ast.TransfoError s ->
    Trace.finalize();
    (* failwith s *)
    let sbt = Printexc.get_backtrace() in
    Printf.eprintf "%s\n" sbt;
    Printf.eprintf "=======\nFailure: %s\n" s;
    exit 1


(* [get_basename ()] is  function to get the name of the file being executed
     by chopping the extension.
*)
let get_basename () : string =
  let basename = Filename.chop_extension Sys.argv.(0) in
  let suffix = "_with_lines" in
  let nsuffix = String.length suffix in
  let nbasename = String.length basename in
  if nbasename >= nsuffix && (String.sub basename (nbasename - nsuffix) nsuffix) = suffix
    then String.sub basename 0 (nbasename - nsuffix)
    else basename


let debug_inline_cpp = false

(* [generated_source_with_inlined_header_cpp input_file inline output_file]
   takes a file [input_file] and produces a file [output_file] obtained by
   inlining in the source the "#include" corresponding to the files listed
   in the list of filenames [inline]. *)
let generated_source_with_inlined_header_cpp (input_file:string) (inline:string list) (output_file:string) : unit =
  let s = ref (Xfile.get_contents input_file) in
  List.iter (fun file_to_inline ->
      let include_instr = "#include \"" ^ file_to_inline ^ "\"" in
      if debug_inline_cpp then Printf.printf "Inlined %s\n" include_instr;
      let contents = Xfile.get_contents file_to_inline in
      s := Str.global_replace (Str.regexp_string include_instr) contents !s)
    inline;
  Xfile.put_contents output_file !s

(* [script_cpp f] is a specialized version of [script f] that:
   - automatically invokes [Trace.init "foo.cpp"] at start,
     where "foo" is the basename of the current script named "foo.ml"
     (alternatively, this name can be specified using the ~filename argument).
   - automatically invokes [Trace.dump] at the end of the script;
     (the main output file is named "foo_out.cpp").
   - [~check_exit_at_end:false] is an option for deactivating the implicit call to [Trace.check_exit_and_step()]
    at the end of the execution of [f] (LATER: will be deprecated)
   - [~prefix:string] allows providing the basename for the output files produced
   - [~inline:["foo.cpp";"bar.h"]] allows to perform substitution of "#include" directives
     with the contents of the corresponding files; the substitutions are performed one after
     the other, meaning that "bar.h" will be inlined if included from "foo.cpp". *)
let script_cpp ?(filename : string = "") ?(inline : string list = []) ?(check_exit_at_end : bool = true) ?(prefix : string = "") (f : unit -> unit) : unit =
  (* Extract the basename. We remove "_with_lines" suffix if the basename ends with that suffix. *)
  (* LATER: see what happens of the directory... *)
  let basename =
    if filename <> "" then Filename.chop_extension filename else begin
      let b = if filename <> "" then filename else get_basename() in
      if String.length b > 2 && b.[0] = '.' && b.[1] = '/'
        then String.sub b 2 (String.length b - 2)
        else b
    end in
  let default_prefix = Filename.remove_extension basename in
  let prefix = if prefix = "" then default_prefix else prefix in
  let default_input_file = basename ^ ".cpp" in
  let input_file =
    match inline with
    | [] -> default_input_file
    | _ ->
        let file = basename ^ "_inlined.cpp" in
        generated_source_with_inlined_header_cpp default_input_file inline file;
        if debug_inline_cpp then Printf.printf "Generated %s\n" file;
        file
    in

  (* Set the input file, execute the function [f], dump the results. *)
  script (fun () ->
    Trace.init ~prefix input_file;
    f();
    flush stdout;
    if check_exit_at_end && Flags.get_exit_line() <> None
      then Trace.dump_diff_and_exit ();
    Trace.dump ~prefix (); (* LATER: in theory, providing the prefix in function "init" should suffice; need to check, however, what happens when the file is not in the current folder *)
    Trace.finalize();
  )


(* LATER:   add  script_rust  following script_cpp *)




