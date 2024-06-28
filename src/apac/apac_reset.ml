open Ast

(** [tnt_blast]: clears and resets all the data structures. *)
let tnt_blast () : unit =
  Var_Hashtbl.reset Apac_records.const_records;
  Var_Hashtbl.reset Apac_records.const_candidates;
  Apac_records.mutables := Apac_dep.Dep_map.empty;
  Stack.clear Apac_constify.to_unconst;
  Stack.clear Apac_constify.to_unconst_objects;
  Hashtbl.reset Apac_constify.to_const_mult 
