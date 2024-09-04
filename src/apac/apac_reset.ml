open Ast

(** [tnt_blast]: clears and resets all the data structures and flags. *)
let tnt_blast () : unit =
  Var_Hashtbl.clear Apac_records.functions;
  Apac_records.mutables := Apac_dep.Dep_map.empty;
  Apac_macros.instrument_code := false;
  Apac_macros.keep_graphs := false;
  Apac_macros.verbose := false;
  Apac_macros.apac_main := "main";
  Flags.code_print_width := 80

