(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)

val generate_static_func_names : bool ref
  (* This option allows to deactivate the generation of __func__ entries *)

val generate_implicit_return_on_main : bool ref
  (* This option allows to deactivate the generation of [return 0] at end of main *)

val allow_variables_as_array_size : bool ref
  (* This option adds support for referring to const variables in array sizes *)

val allow_compound_initializer_in_return : bool ref
  (* This option adds support for return statements applied to compound initializers *)

val keep_for_loops_untransformed : bool ref
  (* This option disables on-the-fly transformations on for-loops *)

val elab_file : Cabs.definition list -> C.program
  (* This is the main entry point.  It transforms a list of toplevel
     definitions as produced by the parser into a program in C abstract
     syntax. *)

val elab_int_constant : Cabs.loc -> string -> int64 * C.ikind
val elab_float_constant : Cabs.floatInfo -> C.float_cst * C.fkind
val elab_char_constant : Cabs.loc -> bool -> int64 list -> int64
  (* These auxiliary functions are exported so that they can be reused
     in other projects that deal with C-style source languages. *)
