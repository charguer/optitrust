open Ast
open Trm
module String_map = Tools.String_map

let unique_alpha_rename (t : trm) : trm =
  (* NOTE: using unqualified names to detect conflict because
     namespace management is language-dependant.contents

     LATER: also add language-specific ability to correct qualifiers?
    *)
  let in_use = ref (
    String_map.of_seq (
      Seq.map (fun ((_q, n), v) -> (n, 0)) (
        Qualified_map.to_seq (
          !toplevel_free_vars)))) in
  let var_map = ref (
    Var_map.of_seq (
      Seq.map (fun ((q, n), id) ->
        let var = { qualifier = q; name = n; id = id } in
        (var, n)) (
        Qualified_map.to_seq (
          !toplevel_free_vars)))) in
  let common_map_var v =
    let v' = match Var_map.find_opt v !var_map with
    | Some name' -> { v with name = name' }
    | None ->
      let (v', i) = begin
        match String_map.find_opt v.name !in_use with
        | Some i -> ({ v with name = v.name ^ "__" ^ (string_of_int i) }, i)
        | None -> (v, -1)
      end in
      in_use := String_map.add v.name (i + 1) !in_use;
      var_map := Var_map.add v v'.name !var_map;
      v'
    in
    (* DEBUG:
    Printf.printf "v: %s\n" (var_to_string v);
    Printf.printf "v': %s\n" (var_to_string v'); *)
    v'
  in
  let map_binder () var _t =
    ((), common_map_var var)
  in
  let map_var () (annot, loc, typ, ctx, kind) var =
    let var' = common_map_var var in
    trm_var ~annot ?loc ?typ ~ctx ~kind var'
  in
  trm_map_vars ~map_binder map_var () t

(* LATER: #var-id, flag to disable check for performance *)
let check_unique_var_ids (t : trm) : unit =
  (* LATER: refactor with function mapping over bindings? *)
  let vars = ref Var_set.empty in
  let add_var v =
    if Var_set.mem v !vars then
      failwith (sprintf "variable '%s' is not declared with a unique id" (var_to_string v));
    vars := Var_set.add v !vars
  in
  let rec aux t =
    begin match t.desc with
    | Trm_let (_, (x, _), _, _) ->
      add_var x
    | Trm_let_mult (_, tvs, _) ->
      List.iter (fun (x, _) -> add_var x) tvs
    | Trm_let_fun (x, _, _, _, _)
      (* FIXME: kind of C-specific? *)
      when not (Trm.is_fun_with_empty_body t) ->
      add_var x
    | Trm_for ((x, _, _, _, _, _), _, _) ->
      add_var x
    (* | Trm_typedef td -> *)
    | _ -> ()
    end;
    trm_iter aux t
  in
  aux t

(* TODO: deal with OCaml and other languages *)
let infer_var_ids t =
 let t2 = C_scope.infer_var_ids t in
 check_unique_var_ids t2;
 t2

let check_var_ids t =
  C_scope.check_var_ids t;
  check_unique_var_ids t

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
  let t = Path.get_trm_at_path p (Trace.ast ()) in
  let error = "expected a variable or function definition within a sequence" in
  let x = trm_inv ~error trm_let_or_let_fun_inv t in
  let (index, pseq) = Path.index_in_seq p in
  let instrs = trm_inv ~error trm_seq_inv (Path.get_trm_at_path pseq (Trace.ast ())) in
  let _, instrs_after = Mlist.split (index + 1) instrs in
  justif_unused_in x instrs_after
