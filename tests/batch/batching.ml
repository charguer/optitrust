open Optitrust

(******************************************************************************)
(*                               Batching Options                             *)
(******************************************************************************)

type result = {
  result_basename : string;
  result_exec_success : bool;
  result_diff_success : bool;
  (* result_error_msg : string *)
}

type results = result list

let batch_results : results ref = ref []

(* TODO: call in script_cpp depending on following options
   if AST diff fails, need to dump cpp output
    *)
let save_batch_result ~(basename : string) ~(exec_success : bool) ~(diff_success : bool) : unit =
  let result = {
    result_basename = basename;
    result_exec_success = exec_success;
    result_diff_success = diff_success;
  } in
  batch_results := result :: !batch_results

let compare_expected_serialized : bool ref = ref false
let stop_on_error : bool ref = ref false

(* Function to report progress, can be overwritten by tester.ml *)
(* TODO: not sure if eprintf should be used by default ;
         should be using a flag to control verbosity *)
let report_progress script_name =
  if !Flags.hide_stdout
    then (Printf.eprintf "Batch test executing: %s\n" script_name; flush stderr)
    else (Printf.printf "Batch test executing: %s\n" script_name; flush stdout)


(******************************************************************************)
(*                               Batch execution                              *)
(******************************************************************************)

(* Batch tests are modules that do not contain anything specific *)
module type TEST = sig end

let run_test ~(script_name:string) (test: unit -> (module TEST)) =
  let toplevel_vars = !Trm.toplevel_vars in
  Flags.reset_flags_to_default ();
  Flags.process_program_name ();
  let program_name = !Flags.program_name in
  let program_path = Filename.dirname program_name in
  Flags.program_name := program_path ^ "/" ^ script_name;
  report_progress script_name;
  begin try
    let _ = test () in
    ()
  with e ->
    Printf.eprintf "===> Script failed: %s\n" script_name;
    if !Flags.print_backtrace_on_error then begin
      (* LATER: flip backtrace *)
      (* LATER: in the backtrace we want to extract and copy the last line
          that contains [in file "tests/target/target_one.ml"], for the
          current test file *)
      Printexc.print_backtrace stderr;
      Printf.eprintf "--------------\n"
    end;
    Printf.eprintf "%s\n" (Printexc.to_string e)
  end;
  Trm.toplevel_vars := toplevel_vars;
  Flags.program_name := program_name


(* [batch_prelude]: called once at the top of 'batch.ml' generated by 'tester.ml'.
   *)
let batch_prelude ~(compare_expected_serialized : bool)
  ~(stop_on_error : bool) : unit = ()
  (* TODO: store in refs *)

(* [batch_postlude]: called once at the bottom of 'batch.ml' generated by 'tester.ml'.*)
let batch_postlude ~(serialized_output_file : string) : unit =
  Xfile.serialize_to serialized_output_file !batch_results
