(** Main functions for Optitrust scripts *)

val script : (unit -> unit) -> unit

val script_cpp : ?filename:string -> ?inline:(string list) -> ?check_exit_at_end:bool -> ?prefix:string -> (unit -> unit) -> unit


(** Debugging *)

val set_exn_backtrace : bool -> unit

module type DebugSig = sig

  exception Breakpoint

  val counter : int ref

  val backtrace : (unit -> unit) -> unit

end

module Debug : DebugSig
