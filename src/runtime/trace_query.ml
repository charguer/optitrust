open Trace

(******************************************************************************)
(*                                   Tree accessors                            *)
(******************************************************************************)

(** [iter_step_tree f s] calls the function [f] on every subnode of [s], in DFS order. *)
let iter_step_tree (f:step_tree->unit) (s:step_tree) : unit =
  let rec aux (s:step_tree) : unit =
    f s;
    List.iter aux s.step_sub
    in
  aux s

(** [get_step_with_id id s] finds in the tree [s] the node with identifier [id].
    Raises [Not_found] otherwise. *)
let get_step_with_id (id:int) (s:step_tree) : step_tree =
  let exception StepFound of step_tree in
  try
    iter_step_tree (fun si ->
      if si.step_infos.step_id = id then raise (StepFound si)) s;
    raise Not_found
  with StepFound si -> si

(******************************************************************************)
(*                                  Trace loading                  *)
(******************************************************************************)

exception Trace_not_found of string
exception Trace_invalid of string
exception Trace_out_of_date of string

let read_trace_tree ?(timestamp: string option) (path: string) : step_tree =
  if not (Sys.file_exists path) then raise (Trace_not_found path);
  begin match timestamp with
    | Some timestamp ->
      let file_timestamp = string_of_float ((Unix.stat path).st_mtime) in
      if file_timestamp <> timestamp then raise (Trace_out_of_date path)
    | None -> ()
  end;
  try
    let file = open_in_bin path in
    let tree = Marshal.from_channel file in
    close_in file;
    tree
  with _ -> raise (Trace_invalid path)

(******************************************************************************)
(*                                  Views                     *)
(******************************************************************************)

type user_view = View_code | View_annot | View_resources | View_usage | View_full

(* TODO: move *)
(* The view_mode corresponds to that used in optitrust_trace.js *)
let user_view_of_string (mode : string) : user_view =
  match mode with
    | "view_code" -> View_code
    | "view_annot" -> View_annot
    | "view_resources" -> View_resources
    | "view_usage" -> View_usage
    | "view_full" -> View_full
    | _ -> failwith (Printf.sprintf "user_view_of_string: unknown mode '%s'" mode)

(* [custom_style_of_view_mode decode view] computes the style
  associated with a user-level specified "view mode" and a boolean
  indicating whether the syntax should be C or OptiTrust. *)
let custom_style_of_view_mode (decode : bool) (user_view : user_view) : output_style =
  let open Style in
  let typing =
    match user_view with
    | View_code -> typing_none
    | View_annot -> typing_annot
    | View_resources -> typing_ctx
    | View_usage -> typing_usage
    | View_full -> typing_all_but_frame
    in
  { decode;
    typing;
    print = Lang_C (AstC_to_c.default_style ()) }

