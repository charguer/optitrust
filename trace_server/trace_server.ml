open Optitrust
open Trace_query

exception Invalid_query of string

let handle_exn_response sub_handler request =
  try
    sub_handler request
  with
  | Invalid_query msg -> Dream.respond ~status:`Bad_Request msg
  | Trace_not_found path -> Dream.respond ~status:`Not_Found ("Trace " ^ path ^ " not found")
  | Trace_invalid path -> Dream.respond ~status:`Internal_Server_Error ("Trace " ^ path ^ " is invalid")
  | Trace_out_of_date path -> Dream.respond ~status:(`Status 419) ("Wrong timestamp for " ^ path)

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
  { decode = get_query_as_bool request "decode";
    typing;
    print = Lang_C (AstC_to_c.default_style ()) }

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
    begin match view_mode with
    | "diff" ->
      begin try
        let style = style_of_query request in
        let step = get_step_with_id step_id trace in
        Dream.respond (Optitrust.Trace.compute_diff ~style step)
      with Not_found ->
        Dream.respond ~status:`Not_Found ("Could not find step " ^ string_of_int step_id)
      end
    | _ -> Dream.respond ~status:`Bad_Request ("Invalid view method " ^ view_mode)
      end
  end else
    Dream.from_filesystem dirname filename request

let () =
  Dream.run ~port:6775
  (*@@ Dream.logger*)
  @@ handle_exn_response
  @@ Dream.router [
    Dream.get "**" handle_get_request;
  ]
