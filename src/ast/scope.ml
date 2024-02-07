open Ast
open Trm
module String_map = Tools.String_map

let infer_var_ids () = Trace.apply Scope_computation.infer_var_ids

let ends_with_num_suffix_rexp = Str.regexp ".*_[0-9]+"
let ends_with_num_suffix name =
  Str.string_match ends_with_num_suffix_rexp name 0

(* first binding of 'x' keeps the name 'x',
   second one takes name 'x_2',
   third one takes name 'x_3', ...

   note: if name already ends with '_N', first binding ends with `_N_1`.
   *)
(* NOTE: using unqualified names to detect conflict because
  namespace management is language-dependant.contents
  We keep qualifiers from original AST. *)
(* LATER: also add language-specific ability to correct qualifiers?
    *)
let unique_alpha_rename (t : trm) : trm =
  (* map from name to next suffix number *)
  (* use deterministic hash table ? *)
  let nb_uses_of_name = ref String_map.empty in
  (* map from var to new name *)
  (* could be map from id to names? *)
  let name_of_id = ref Var_map.empty in
  let map_var () v =
    let v' = match Var_map.find_opt v !name_of_id with
    | Some name' -> { v with name = name' }
    | None ->
      let (v', nb_uses) = begin
        match String_map.find_opt v.name !nb_uses_of_name with
        | Some prev_nb_uses ->
          let nb_uses = prev_nb_uses + 1 in
          ({ v with name = v.name ^ "_" ^ (string_of_int nb_uses) },
           nb_uses)
        | None ->
          ((if ends_with_num_suffix v.name
            then { v with name = v.name ^ "_1" }
            else v),
           1)
      end in
      (* TODO: check that add overrides previous entry *)
      nb_uses_of_name := String_map.add v.name nb_uses !nb_uses_of_name;
      name_of_id := Var_map.add v v'.name !name_of_id;
      v'
    in
    (* DEBUG:
    Printf.printf "v: %s\n" (var_to_string v);
    Printf.printf "v': %s\n" (var_to_string v'); *)
    v'
  in
  (* initialize entries for toplevel variables. *)
  Qualified_map.iter (fun (q, n) id ->
    let var = { qualifier = q; name = n; id = id } in
    ignore (map_var () var)
  ) !toplevel_vars;
  trm_rename_vars map_var () t


let trm_let_or_let_fun_inv t =
  match trm_let_inv t with
  | Some (_, x, _, _) -> Some x
  | _ ->
  begin match trm_let_fun_inv t with
  | Some (x, _, _, _) -> Some x
  | _ -> None
  end

(** Lists all the let-bindings inside [tl_new_scope] interfering with the instructions in [tl_after].
    A let-binding interferes if it appears as a free variable in [tl_after]. *)
let find_interference tl_new_scope tl_after : var list =
  let fv_after = trm_free_vars (trm_seq tl_after) in
  let fv_after = Qualified_set.of_seq (Seq.map (fun v -> (v.qualifier, v.name)) (Var_set.to_seq fv_after)) in
  let find_toplevel_bind t =
    match trm_let_or_let_fun_inv t with
    | Some x when Qualified_set.mem (x.qualifier, x.name) fv_after -> Some x
    | _ -> None
  in
  List.filter_map find_toplevel_bind (Mlist.to_list tl_new_scope)

(** Like [find_interference], but fails if there are interferences. *)
let assert_no_interference ~(after_what : string) ~(on_interference : string) tl_new_scope tl_after : unit =
  match find_interference tl_new_scope tl_after with
  | [] -> ()
  | [x] -> failwith (sprintf "local variable '%s' is used after %s, but will now be %s" (var_to_string x) after_what on_interference)
  | xs -> failwith (sprintf "local variables %s are used after %s, but will now be %s" (Tools.list_to_string ~sep:"', '" ~bounds:["'";"'"] (List.map var_to_string xs)) after_what on_interference)

(** If [x] is used in [instrs], traces a justification, otherwise fails. *)
let justif_unused_in (x : var) (instrs : trm mlist) : unit =
  let fv = trm_free_vars (trm_seq instrs) in
  if Var_set.mem x fv then
    failwith (sprintf "'%s' is used" (var_to_string x));
  Trace.justif (sprintf "'%s' is unused" (var_to_string x))

(** Given a path to a variable definition, assert that it is unused using [justif_unused_in]. *)
let justif_unused (p : Path.path) : unit =
  let t = Path.resolve_path p (Trace.ast ()) in
  let error = "expected a variable or function definition within a sequence" in
  let x = trm_inv ~error trm_let_or_let_fun_inv t in
  let (index, pseq) = Path.index_in_seq p in
  let instrs = trm_inv ~error trm_seq_inv (Path.resolve_path pseq (Trace.ast ())) in
  let _, instrs_after = Mlist.split (index + 1) instrs in
  justif_unused_in x instrs_after
