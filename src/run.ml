
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
(*                        Perf                                                *)
(******************************************************************************)

(* Use higher default value for the GC parameters *)
let _ =
  Gc.set { (Gc.get()) with
              Gc.minor_heap_size = 524288; (* 512k *)
              Gc.major_heap_increment = 4194304 (* 4M *) }


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
  Flags.fix_flags();
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
      s := Tools.string_subst include_instr contents !s)
    inline;
  Xfile.put_contents output_file !s

(* [get_basename filename] is  function to get the name of the file being executed
     by chopping the extension. If filename is the empty string, then the name of
     current binary program is used.
*)
let get_basename (filename : string) =
  if filename <> "" then Filename.chop_extension filename else begin
    let basename = Filename.chop_extension Sys.argv.(0) in
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
    end

(* [script_cpp f] is a specialized version of [script f] that:
   - automatically invokes [Trace.init "foo.cpp"] at start,
     where "foo" is the basename of the current script named "foo.ml"
     (alternatively, this name can be specified using the ~filename argument).
   - automatically invokes [Trace.dump] at the end of the script;
     (the main output file is named "foo_out.cpp").
   - [~check_exit_at_end:false] is an option for deactivating the implicit call to [Trace.check_exit_and_step()]
    at the end of the execution of [f] (LATER: will be deprecated)
      This flag only has an effect if a [-exit_line] option was passed on the command line.
   - [~prefix:string] allows providing the basename for the output files produced
   - [~inline:["foo.cpp";"bar.h"]] allows to perform substitution of "#include" directives
     with the contents of the corresponding files; the substitutions are performed one after
     the other, meaning that "bar.h" will be inlined if included from "foo.cpp". *)
let script_cpp ?(filename : string = "") ?(inline : string list = []) ?(check_exit_at_end : bool = true) ?(prefix : string = "") (f : unit -> unit) : unit =
  (* Extract the basename. We remove "_with_lines" suffix if the basename ends with that suffix. *)
  (* LATER: see what happens of the directory... *)
  let basename = get_basename filename in
  (* Printf.printf "START   %s\n" basename; *)
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
  (* Printf.printf "END  %s\n" basename *)

(* [doc_script_cpp f src] is a variant of [script_cpp] that takes as input a
    piece of source code [src] as a string, and stores this contents into
    [foo_doc.cpp], where [foo.ml] is the name of the current file. It then
    executes the transformation [f] using [script_cpp ~filename:"foo_doc.cpp"]  *)
let doc_script_cpp ?(filename : string = "") ?(prefix : string = "") (f : unit -> unit) (src : string) : unit =
  let basename = get_basename filename in
  let doc_basename = basename ^ "_doc" in
  Flags.documentation_save_file_at_first_check := doc_basename;
  let src_filename = doc_basename ^ ".ml" in
  (* Write the contents of the cpp file; not that it will be overwritten at the first '!!' from the script. *)
  Xfile.put_contents (doc_basename ^ ".cpp") src;
  script_cpp ~filename:src_filename ~check_exit_at_end:false ~prefix f;
  Flags.documentation_save_file_at_first_check := ""

(* LATER:   add  script_rust  following script_cpp *)




