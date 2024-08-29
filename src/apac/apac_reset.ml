open Ast

(** [tnt_blast]: clears and resets all the data structures. *)
let tnt_blast () : unit =
  Apac_records.mutables := Apac_dep.Dep_map.empty;

