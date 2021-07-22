
(******************************************************************************)
(*                        Debug                                               *)
(******************************************************************************)

(* include Tools.Debug *)
include Tools (* TODO: make it better *)

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
    f ();
    Trace.close_logs()
  with | Failure s ->
    Trace.close_logs();
    (* failwith s *)
    Printf.eprintf "Failure: %s\n" s;
    let s = Printexc.get_backtrace() in
    Printf.eprintf "%s\n" s;
    exit 1


(* [script_cpp f] is a specialized version of [script f] that:
   - automatically invokes [Trace.init "foo.cpp"] at start,
     where "foo" is the basename of the current script named "foo.ml";
   - automatically invokes [Trace.dump] at the end of the script;
     (the main output file is named "foo_out.cpp"). *)
let script_cpp ?(prefix : string = "") (f : unit -> unit) : unit =
  (* Extract the basename. We remove "_with_lines" suffix if the basename ends with that suffix. *)
  (* TODO: move these lines to a get_basename function *)
  let basename = Filename.chop_extension Sys.argv.(0) in
  let basename =
    let suffix = "_with_lines" in
    let nsuffix = String.length suffix in
    let nbasename = String.length basename in
    if nbasename >= nsuffix && (String.sub basename (nbasename - nsuffix) nsuffix) = suffix
      then String.sub basename 0 (nbasename - nsuffix)
      else basename
      in
  (* Set the input file, execute the function [f], dump the results. *)
  script (fun () ->
    Trace.init (basename ^ ".cpp");
    f();
    flush stdout;
    Trace.dump ~prefix ();
  )


(* TODO: Anton :   add  script_rust  following script_cpp *)




