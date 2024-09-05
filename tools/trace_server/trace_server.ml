open Optitrust
open Trace_query

exception Invalid_query of string
exception Step_not_found of string * int

let handle_exn_response sub_handler request =
  try
    sub_handler request
  with
  | Invalid_query msg -> Dream.respond ~status:`Bad_Request msg
  | Trace_not_found path -> Dream.respond ~status:`Not_Found ("Trace " ^ path ^ " not found")
  | Trace_deserialization_error path -> Dream.respond ~status:`Internal_Server_Error ("Trace deserialization failed for " ^ path)
  | Trace_out_of_date path -> Dream.respond ~status:(`Status 419) ("Wrong timestamp for " ^ path)
  | Step_not_found (path, step_id) -> Dream.respond ~status:`Not_Found ("Could not find step " ^ string_of_int step_id ^ " in trace " ^ path)

let get_query request query_name =
  match Dream.query request query_name with
  | Some query -> query
  | None -> raise (Invalid_query ("Missing query parameter " ^ query_name))

let get_query_as_int request query_name =
  let query = get_query request query_name in
  match int_of_string_opt query with
  | Some query -> query
  | None -> raise (Invalid_query ("Query parameter " ^ query_name ^ " should be an integer"))

let get_query_as_bool request query_name =
  let query = get_query request query_name in
  match bool_of_string_opt query with
  | Some query -> query
  | None -> raise (Invalid_query ("Query parameter " ^ query_name ^ " should be a boolean"))

let style_of_query request : Trace.output_style =
  let open Style in
  let typing =
    match (get_query request "typing_style") with
    | "hide" -> typing_none
    | "annot" -> typing_annot
    | "ctx" -> typing_ctx
    | "usage" -> typing_usage
    | "full" -> typing_all_but_frame
    | style -> raise (Invalid_query ("Invalid typing style " ^ style))
    in
  let c_style = Ast_to_c.default_style () in
  { decode = get_query_as_bool request "decode";
    typing;
    print = Lang_C { c_style with
      optitrust_syntax = get_query_as_bool request "optitrust_syntax";
      print_types = get_query_as_bool request "print_types";
    } }

type trace_cache_entry = {
  trace_path: string;
  trace_timestamp: string;
  trace_tree: Trace.step_tree;
}
let trace_cache = ref None

let read_trace_tree ~(timestamp:string) (path: string): Trace.step_tree =
  Trace_query.check_trace_file ~timestamp path;
  let trace = match !trace_cache with
    | Some { trace_path; trace_timestamp; trace_tree } when
        path = trace_path && timestamp = trace_timestamp ->
      trace_tree
    | _ ->
      Tools.info "Loading trace file %s" path;
      let trace_tree = Trace_query.deserialize_trace_tree path in
      trace_cache := Some { trace_path = path; trace_timestamp = timestamp; trace_tree };
      trace_tree
  in
  trace

let handle_get_request request =
  let path, _ = Dream.split_target (Dream.target request) in
  let path = String.sub path 1 (String.length path - 1) in
  let filename = Filename.basename path in
  let dirname = Filename.dirname path in
  let extension = Filename.extension path in
  if extension = ".trace" then begin
    let timestamp = get_query request "timestamp" in
    let trace = read_trace_tree ~timestamp path in
    let step_id = get_query_as_int request "step" in
    let view_mode = get_query request "view" in
    let style = style_of_query request in
    let step =
      try
        get_step_with_id step_id trace
      with Not_found -> raise (Step_not_found (path, step_id))
    in
    begin match view_mode with
    | "diff" -> Dream.respond (Optitrust.Trace.compute_diff ~style step)
    | "code_before" -> Dream.respond (Optitrust.Trace.get_code_before ~style step)
    | "code_after" -> Dream.respond (Optitrust.Trace.get_code_after ~style step)
    | _ -> Dream.respond ~status:`Bad_Request ("Invalid view mode " ^ view_mode)
    end
  end else
    Dream.from_filesystem dirname filename request

let () =
  Tools.error_fun := (fun msg -> Dream.error (fun log -> log "%s" msg));
  Tools.warn_fun := (fun msg -> Dream.warning (fun log -> log "%s" msg));
  Tools.info_fun := (fun msg -> Dream.info (fun log -> log "%s" msg));
  Tools.debug_fun := (fun msg -> Dream.debug (fun log -> log "%s" msg));

  Dream.run ~port:6775 ~adjust_terminal:false
  @@ Dream.logger
  @@ handle_exn_response
  @@ Dream.router [
    Dream.get "**" handle_get_request;
  ]
