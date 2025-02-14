open Ast

(** [tnt_blast]: clears and resets all the data structures and flags. *)
let tnt_blast () : unit =
  reset_fresh_var_int ();
  Var_Hashtbl.clear Apac_records.functions;
  Apac_flags.constify := false;
  Apac_flags.cutoff_count := false;
  Apac_flags.cutoff_depth := false;
  Apac_flags.profile := false;
  Apac_flags.keep_graphs := false;
  Apac_flags.verbose := false;
  Apac_flags.main := "main";
  Flags.code_print_width := 80

