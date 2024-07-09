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
exception Trace_out_of_date of string

(** [check_trace_file ?timestamp path]: Checks that a trace file exists and have the correct timestamp *)
let check_trace_file ?(timestamp: string option) (path: string): unit =
  if not (Sys.file_exists path) then raise (Trace_not_found path);
  begin match timestamp with
    | Some timestamp ->
      let file_timestamp = string_of_float ((Unix.stat path).st_mtime) in
      if file_timestamp <> timestamp then raise (Trace_out_of_date path)
    | None -> ()
  end

exception Trace_deserialization_error of string

(** [deserialize_trace_tree path]: Reads a serialized trace tree *)
let deserialize_trace_tree (path: string) : step_tree =
  try
    let file = open_in_bin path in
    let tree = Marshal.from_channel file in
    close_in file;
    tree
  with _ -> raise (Trace_deserialization_error path)
