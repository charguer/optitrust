

(******************************************************************************)
(*                             Timing logs                                    *)
(******************************************************************************)
(* [write_timing_log msg]: writes a message in the timing log file. *)
let write_timing_log (msg : string) : unit =
  let timing_log = open_out ("timing.log") in
  output_string timing_log msg;
  flush timing_log

(* [measure_time f]: returns a pair made of the result of [f()] and
   of the number of milliseconds taken by that call. *)
let measure_time (f : unit -> 'a) : 'a * int =
  let t0 = Unix.gettimeofday () in
  let res = f() in
  let t1 = Unix.gettimeofday () in
  res, (Tools.milliseconds_between t0 t1)

(* [timing_nesting]: records the current level of nesting of calls to the
   [timing] function. It is used for printing tabulations in the reports. *)
let timing_nesting : int ref = ref 0

(* [timing ~name f]: writes the execution time of [f] in the timing log file. *)
let timing ?(cond : bool = true) ?(name : string = "") (f : unit -> 'a) : 'a =
  if !Flags.analyse_time && cond then begin
    incr timing_nesting;
    let res, time = measure_time f in
    decr timing_nesting;
    let msg = Printf.sprintf "%s%d\tms -- %s\n" (Tools.spaces (2 * !timing_nesting)) time name in
    write_timing_log msg;
    res
  end else begin
    f()
  end

(* [time name f]: is a shorthand for [timing ~cond:!Flags.analyse_time_details ~name]. *)
let time (name : string) (f : unit -> 'a) : 'a =
  timing ~cond:!Flags.analyse_time_details ~name f

(* [start_time]: stores the date at which the script execution started (before parsing). *)
let start_time = ref (0.)

(* [last_time]: stores the date at which the execution of the current step started. *)
let last_time = ref (0.)

(* [last_time_update()]: updates [last_time] and returns the delay
   since last call -- LATER: find a better name. *)
let last_time_update () : int =
  let t0 = !last_time in
  let t = Unix.gettimeofday() in
  last_time := t;
  Tools.milliseconds_between t0 t

(* DEPRECATED *)
(* [report_time_of_step()]: reports the total duration of the last step.
   As bonus, reports the number of steps in target resolution. *)
(* let report_time_of_step (timing : int) : unit =
  if !Flags.analyse_time then begin
    write_timing_log (Printf.sprintf "===> TOTAL: %d\tms\n" timing);
    write_timing_log (Printf.sprintf "     TARGETS: %d nodes visited for target resolution\n" (Constr.resolve_target_steps()));
  end *)



(* DEPRECATED *)
(* [report_full_time ()]: reports the time for the last step, and for the full total. *)
(* let report_full_time () : unit =
  write_timing_log (Printf.sprintf "------------------------TOTAL TRANSFO TIME: %.3f s\n" (!last_time -. !start_time)) *)

(* [id_big_step]: traces the number of big steps executed. This reference is used only
   when executing a script from the command line, because in this case the line numbers
   from the source script are not provided on calls to the [step] function. *)
let id_big_step = ref 0


type stats = {
  mutable stats_time : float;
  mutable stats_trm_alloc : int;
  mutable stats_target_resolution_steps : int}

let cur_stats = {
  stats_time = 0.;
  stats_trm_alloc = 0;
  stats_target_resolution_steps = 0;
}

let incr_trm_alloc () : unit =
  cur_stats.stats_trm_alloc <- cur_stats.stats_trm_alloc + 1
  
let incr_target_resolution_steps () : unit =
  cur_stats.stats_target_resolution_steps <- cur_stats.stats_target_resolution_steps + 1

let get_cur_stats () : stats =
  { cur_stats with stats_time = Unix.gettimeofday ()}

let stats_diff (stats_start : stats) (stats_stop : stats) : stats = {
    stats_time = stats_stop.stats_time -. stats_start.stats_time;
    stats_trm_alloc = stats_stop.stats_trm_alloc - stats_start.stats_trm_alloc;
    stats_target_resolution_steps = stats_stop.stats_target_resolution_steps - stats_start.stats_target_resolution_steps;
}

let stats_to_string (stats : stats) : string =
  Printf.sprintf "%dms;\t%d trm_allocations;\t%d target_resolution_steps"
     (int_of_float (1000. *. stats.stats_time))
     stats.stats_trm_alloc
     stats.stats_target_resolution_steps

let measure_stats (f : unit -> 'a) : 'a * stats =
  let stats_start = get_cur_stats () in 
  let res = f () in
  let stats_stop = get_cur_stats () in 
  let stats = stats_diff stats_start stats_stop in 
  res, stats

let stats ?(cond : bool = true) ?(name : string = "") (f : unit -> 'a) : 'a =
  if !Flags.analyse_time && cond 
    then begin 
      incr timing_nesting;
      let res, stats = measure_stats f in 
      decr timing_nesting;
      let msg = Printf.sprintf "%s -- %s\n" (stats_to_string stats) (* time *) name in 
      write_timing_log msg;
      res
    end 
    else f()

