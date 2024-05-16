open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.serialize_trace := true

let _ = Run.script_cpp ~filename:"interact_traceview.cpp" (fun _ ->
  !! Label.add "lab1" [cVarDef "a"];
  !! Label.add "lab2" [cVarDef "a"];
)

let _ =
  let open Trace_query in
  let prefix = Run.get_program_basename () in
  Trace.dump_full_trace_to_js ~prefix ();
  Trace.dump_trace_to_textfile ~prefix ();
  let path = prefix ^ ".trace" in
  (* TODO: More robust step_id for this test *)
  let step_id = 7 in
  let trace = read_trace_tree path in
  let step = get_step_with_id step_id trace in
  let comments_to_app =
    Printf.sprintf "step_name: %s\nbefore: %s\ndiff: %s\n"
      step.step_infos.step_name
      (get_code_before step)
      (compute_diff step)
  in
  Xfile.append_contents (prefix ^ "_out.cpp") ("/*\n" ^ comments_to_app ^ "\n*/\n")
