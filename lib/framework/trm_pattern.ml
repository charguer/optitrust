open Ast
open Trm
open Tools

(* TODO: think about the relationship between this, trm_unify, trm_matching.ml, and string matching *)

(** Patterns are terms that may contain pattern variables. *)
type pattern = trm

type compiled_pattern = {
  trm : trm;
  evars : unit unification_ctx;
}

let pattern_namespace = "__pattern"

let pvar name = name_to_var ~namespaces:[pattern_namespace] name

let trm_pattern_var ?typ name = trm_var ?typ (pvar name)

(** Compiles a pattern, i.e. assigns var ids to pattern variables and constructs a set of evars for unification. *)
let pattern_compile (pattern : trm) : compiled_pattern =
  let evars = ref Var_map.empty in
  let pvar_ids = ref String_map.empty in
  let map_var _ (annot, loc, typ, ctx) v =
    let var =
      if v.namespaces = [pattern_namespace]
      then begin
        (* pattern variable cases *)
        let var = if v.id = unset_var_id
          then begin
            match String_map.find_opt v.name !pvar_ids with
            | Some id -> { namespaces = v.namespaces; name = v.name; id }
            | None ->
              let new_v = new_var ~namespaces:v.namespaces v.name in
              pvar_ids := String_map.add v.name new_v.id !pvar_ids;
              new_v
          end
          else begin
            if String_map.mem v.name !pvar_ids
              then failwith "pattern variable appears with and without id %s" v.name;
            v
          end
        in
        evars := Var_map.add var (Unknown ()) !evars;
        var
      end else begin
        (* not a pattern variable *)
        v
      end
    in
    trm_var ~annot ?loc ?typ ~ctx var
  in
  let trm = trm_map_vars map_var Var_set.empty pattern in
  { trm; evars = !evars }

let trm_matches_pattern (trm : trm) (pattern : compiled_pattern) : bool =
  Option.is_some (unify_trm trm pattern.trm pattern.evars (fun _ () ctx -> ctx))
