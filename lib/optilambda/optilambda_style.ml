(** Printing options for the OptiLambda textual language. *)

type loop_mode_style =
  | Short (** Print loop modes as [seq], [par], [gpu_thread], [magic_thread]. *)
  | Full  (** Print loop modes using their OCaml constructor names. *)

type style = {
  (* Include internal variable ids, e.g. [x#12], when available. *)
  print_var_ids : bool;
  (* Print type annotations on variables and function return types. *)
  print_types : bool;
  (* Print function and loop contract summaries/clauses. *)
  print_contracts : bool;
  (* Print ghost instructions using [ghost], [ghost_begin], and [ghost_end]. *)
  print_ghosts : bool;
  (* Print OptiTrust marks as [@marks[...]] prefixes. *)
  print_marks : bool;
  (* Include loop directions in [for<mode, direction>] headers. *)
  print_loop_direction : bool;
  (* Print [(i = start, stop)] instead of [(i = start, stop, 1)]. *)
  omit_default_loop_step : bool;
  (* Choose compact or constructor-style loop mode names. *)
  loop_mode_style : loop_mode_style;
}

let default = {
  print_var_ids = false;
  print_types = true;
  print_contracts = true;
  print_ghosts = true;
  print_marks = true;
  print_loop_direction = true;
  omit_default_loop_step = false;
  loop_mode_style = Short;
}
