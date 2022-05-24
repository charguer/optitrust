

(******************************************************************************)
(*                                 Stats                                      *)
(******************************************************************************)
(* [write_stats_log msg]: writes a message in the timing log file. *)
let write_stats_log (msg : string) : unit =
  let stats_log = open_out ("stats.log") in
  output_string stats_log msg;
  flush stats_log

(* [stats_nesting]: records the current level of nesting of calls to the [stats] function. It is used
     for printing tabulations in the reports. *)
let stats_nesting : int ref = ref 0

(* [stats]: a record for storing statistics about timing, nodes allocated and target resolution steps. *)
type stats = {
  mutable stats_time : float;
  mutable stats_trm_alloc : int;
  mutable stats_target_resolution_steps : int}

(* [cur_stats]: initialization of stats record. *)
let cur_stats = {
  stats_time = 0.;
  stats_trm_alloc = 0;
  stats_target_resolution_steps = 0;
}

(* [incr_trm_alloc ()]: function to count the current number of allocated nodes. *)
let incr_trm_alloc () : unit =
  cur_stats.stats_trm_alloc <- cur_stats.stats_trm_alloc + 1

(* [incr_target_resolution_steps ()]: function to count the number of target resolution steps. *)
let incr_target_resolution_steps () : unit =
  cur_stats.stats_target_resolution_steps <- cur_stats.stats_target_resolution_steps + 1

(* [get_cur_stats ()]: return the current statistics. *)
let get_cur_stats () : stats =
  { cur_stats with stats_time = Unix.gettimeofday ()}

(* [stats_diff stats_start stats_stop]: return the difference between [stats_start] and [stats_stop]. *)
let stats_diff (stats_start : stats) (stats_stop : stats) : stats = {
    stats_time = stats_stop.stats_time -. stats_start.stats_time;
    stats_trm_alloc = stats_stop.stats_trm_alloc - stats_start.stats_trm_alloc;
    stats_target_resolution_steps = stats_stop.stats_target_resolution_steps;
}


(* [start_stats]: stores the stats before starting the script execution (before parsing). *)
let start_stats = ref cur_stats

(* [last_stats]: stores the stats after the execution of the current step. *)
let last_stats = ref cur_stats


(* [last_stats_update ()]: updates [last_stats] and returns the difference *)
let last_stats_update () : stats =
  let stats0 = !last_stats in
  let stats1 = get_cur_stats () in
  last_stats := stats1;
  stats_diff stats0 stats1

(* [stats_to_string stats]: prints [stats]. *)
let stats_to_string (stats : stats) : string =
  Printf.sprintf "%dms;\t%d trm_allocations;\t%d target_resolution_steps"
     (int_of_float (1000. *. stats.stats_time))
     stats.stats_trm_alloc
     stats.stats_target_resolution_steps

(* [stats_diff_str start_stats end_stats]: similar to [stats_diff] but this one returns diff as string. *)
let stats_diff_str (start_stats : stats) (end_stats : stats) : string =
  let stats_dff = stats_diff start_stats end_stats in
  stats_to_string stats_dff

(* [report_full_stats ()]: reports the stats for the last step, and for the full total *)
let report_full_stats () : unit =
  let full_stats_str = stats_diff_str !start_stats !last_stats in
  write_stats_log (Printf.sprintf "------------------------FULL STATS: %s\n" full_stats_str)

(* [measure_stats f]: computes the difference of stats before and after applying function [f].
    Then it returns the result of [f] and the computed stats. *)
let measure_stats (f : unit -> 'a) : 'a * stats =
  let stats_start = get_cur_stats () in
  let res = f () in
  let stats_stop = get_cur_stats () in
  let stats = stats_diff stats_start stats_stop in
  res, stats


(* [stats ~cond ~name f]: computes and prints stats only if [Flags.analyse_stats] is set to true. *)
let stats ?(cond : bool = true) ?(name : string = "") (f : unit -> 'a) : 'a =
  if !Flags.analyse_stats && cond
    then begin
      incr stats_nesting;
      let res, stats = measure_stats f in
      decr stats_nesting;
      let msg = Printf.sprintf "%s%s\t -- %s\n" (Tools.spaces (2 * !stats_nesting)) (stats_to_string stats) name in
      write_stats_log msg;
      res
    end
    else f()

(* [comp_stats name f ]: an alias to [stats] with the default condition being [!Flags.analyse_stats_details] *)
let comp_stats (name : string) (f : unit -> 'a) : 'a =
  stats ~cond:!Flags.analyse_stats_details ~name f
