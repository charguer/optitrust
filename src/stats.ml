

(******************************************************************************)
(*                                 Stats                                      *)
(******************************************************************************)
(* [write_timing_log msg]: writes a message in the timing log file. *)
let write_timing_log (msg : string) : unit =
  let timing_log = open_out ("timing.log") in
  output_string timing_log msg;
  flush timing_log



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
    stats_target_resolution_steps = stats_stop.stats_target_resolution_steps - stats_start.stats_target_resolution_steps;
}

(* [stats_to_string stats]: prints [stats]. *)
let stats_to_string (stats : stats) : string =
  Printf.sprintf "%dms;\t%d trm_allocations;\t%d target_resolution_steps"
     (int_of_float (1000. *. stats.stats_time))
     stats.stats_trm_alloc
     stats.stats_target_resolution_steps

(* [measure_stats f]: computes the difference of stats before and after applying function [f]. 
    Then it returns the result of [f] and the computed stats. *)
let measure_stats (f : unit -> 'a) : 'a * stats =
  let stats_start = get_cur_stats () in 
  let res = f () in
  let stats_stop = get_cur_stats () in 
  let stats = stats_diff stats_start stats_stop in 
  res, stats

(* TODO: Change Flags.analyse_time to Flags.compute_stats. *)
(* [stats ~cond ~name f]: computes and prints stats only if [Flags.analyse_time] is set to true. *)
let stats ?(cond : bool = true) ?(name : string = "") (f : unit -> 'a) : 'a =
  if !Flags.analyse_time && cond 
    then begin 
      incr stats_nesting;
      let res, stats = measure_stats f in 
      decr stats_nesting;
      let msg = Printf.sprintf "%s%s\t -- %s\n" (Tools.spaces (2 * !stats_nesting)) (stats_to_string stats) name in
      (* let msg = Printf.sprintf "%s -- %s\n" (stats_to_string stats) name in  *)
      write_timing_log msg;
      res
    end 
    else f()
