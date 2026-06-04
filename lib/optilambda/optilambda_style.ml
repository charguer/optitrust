(** Printing options for the OptiLambda textual language. *)

type loop_mode_style =
  | Short  (** Print loop modes as [seq], [par], [gpu_thread], [magic_thread]. *)
  | Full  (** Print loop modes using their OCaml constructor names. *)

(** OptiLambda can display the same internal AST at several levels of explicitness. *)
type representation =
  | Surface  (** Human-oriented syntax, currently the default OptiLambda printer output. *)
  | Internal  (** Explicit internal operations without full type parameters. *)
  | FullyTypedInternal  (** Explicit internal operations with type parameters when available. *)

type style = {
  (* Select which OptiLambda representation to print. *)
  representation : representation;
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
  (* Deprecated: loop directions are no longer printed in simple loop headers. *)
  print_loop_direction : bool;
  (* Omit [:1] from [for<mode> i in start..stop:1]. *)
  omit_default_loop_step : bool;
  (* Choose compact or constructor-style loop mode names. *)
  loop_mode_style : loop_mode_style;
}

let default =
  {
    representation = Surface;
    print_var_ids = false;
    print_types = true;
    print_contracts = true;
    print_ghosts = true;
    print_marks = true;
    print_loop_direction = false;
    omit_default_loop_step = true;
    loop_mode_style = Short;
  }

let representation_to_string = function
  | Surface -> "surface"
  | Internal -> "internal"
  | FullyTypedInternal -> "typed"

let representation_to_label = function
  | Surface -> "Surface"
  | Internal -> "Internal"
  | FullyTypedInternal -> "Fully-Typed Internal"

let representation_of_string = function
  | "surface" -> Some Surface
  | "internal" -> Some Internal
  | "typed" -> Some FullyTypedInternal
  | _ -> None

let parse_representation s =
  match representation_of_string s with
  | Some representation -> representation
  | None -> failwith "OptiLambda representation should be one of: surface, internal, typed"
