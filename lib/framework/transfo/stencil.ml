open Prelude

(* TODO: unit tests + document *)

type nd_tile = Matrix_core.nd_tile

let%transfo loop_align_stop_extend_start ~(start : trm) ~(stop : trm) ?(simpl : target -> unit = Arith.default_simpl) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  Target.iter (fun p ->
    let loop_t = Target.resolve_path p in
    let error = "Stencil.loop_align_stop_extend_start: expected simple loop" in
    let ({ start = start'; stop = stop' }, _body, _contract) = trm_inv ~error trm_for_inv loop_t in
    if (are_same_trm start start') && (are_same_trm stop stop') then
      ()
    else begin
      Loop.shift ~reparse:false ~simpl (StopAt stop) (target_of_path p);
      Loop.extend_range ~simpl ~start:(ExtendTo start) (target_of_path p)
    end
  ) tg
  (* TODO: remove following *)
  (* Trace.reparse ();
  simpl tg *)

let%transfo loop_align_stop_extend_start_like ~(orig:target) ?(nest_of : int = 1) ?(simpl : target -> unit = Arith.default_simpl) (tg:target) : unit =
  Trace.tag_valid_by_composition ();
  let orig_p = resolve_target_exactly_one orig in
  let ps = resolve_target tg in
  let rec aux (nest_of : int) (orig_p : path) (ps : paths) (map_vars : tmap list) =
    if nest_of > 0 then begin
      (* is this a good idea? simplify original loop range before using it. *)
      Loop.simpl_range ~simpl (target_of_path orig_p);
      let t = Path.resolve_path orig_p (Trace.ast ()) in
      let error = "Stencil.loop_align_stop_extend_start_like: expected simple loop" in
      let ({ index; start; stop }, _body, _contract) = trm_inv ~error trm_for_inv t in
      let map_vars' = List.map2 (fun p map_var ->
        let start = trm_subst map_var start in
        let stop = trm_subst map_var stop in
        let ({ index = tg_index }, _, _) = trm_inv ~error trm_for_inv (Path.resolve_path p (Trace.ast ())) in
        loop_align_stop_extend_start ~start ~stop ~simpl (target_of_path p);
        Var_map.add index (trm_var tg_index) map_var
      ) ps map_vars in
      aux (nest_of - 1) (Path.to_inner_loop orig_p)
        (List.map Path.to_inner_loop ps) map_vars'
    end
  in aux nest_of orig_p ps (List.map (fun _ -> Var_map.empty) ps)

(* TODO: inline ~delete:IfOnlyCall *)
let rec pry_loop_nest (nest_of: int) (simpl : target -> unit) (p : path) : unit =
  if nest_of > 0 then begin
    let t = Path.resolve_path p (Trace.ast ()) in
    match trm_for_inv t with
    | Some _ -> pry_loop_nest (nest_of - 1) simpl (p @ [Dir_body; Dir_seq_nth 0]) (* (p @ [Dir_body]) *)
    | None ->
      begin match trm_apps_inv t with
      | Some (f, _) ->
        Function.inline ~simpl (target_of_path p);
        pry_loop_nest nest_of simpl p
      | None ->
        (* TODO: sequence with other things inside?
        begin match trm_seq_inv t with
        | Some
        | None -> *) trm_fail t "Stencil.pry_loop_nest: expected nested for loops, potentially hidden by function calls."
      end
  end

(** [may_slide]: slides a stencil that writes to [written] with outer loop at path [p], so that tiles of [sizes] values are produced by inner loops, within outer loops progressing by [steps].

  Keeps outer loop index names and uses [written] as suffix for inner loop index names.
  Returns the list of created inner loop index names.
  *)
let may_slide (written : var list) (sizes : trm list) (steps : trm list) ~(simpl : target -> unit) (p : path) : string list =
  let written = List.map (fun v -> v.name) written in
  let outer_loop_count = List.length sizes in
  let outer_loop_indices = List.map (fun v -> v.name) (Loop.get_indices outer_loop_count p) in
  let size_steps = List.map2 (fun size step ->
    if (is_trm_int 1 size) && (is_trm_int 1 step) then
      None
    else
      Some (size, step)
  ) sizes steps in
  let inner_loop_indices = List.filter_map (fun (base_index, szst) ->
    Option.map (fun _ -> Tools.list_to_string ~sep:"_" ~bounds:("","") (base_index :: written)) szst
  ) (List.combine outer_loop_indices size_steps) in
  Loop.slides ~iter:TileIterLocal ~size_steps ~simpl (target_of_path p);
  Loop.set_indices (outer_loop_indices @ inner_loop_indices) p;
  inner_loop_indices

(* TODO: move elsewhere? *)
let var_of_access (access_t : trm) : var =
  let error = "Stencil.var_of_access: expected array write on base variable" in
  let (base, index) = trm_inv ~error trm_array_access_inv access_t in
  let var = begin match trm_var_inv base with
  | Some x -> x
  | None -> trm_inv ~error trm_var_get_inv base
  end in
  var
let var_of_def (def_t : trm) : var =
  let error = "Stencil.var_of_def: expected variable declaration" in
  let (var, _, _) = trm_inv ~error trm_let_inv def_t in
  var

let collect_writes (p : path) : Var_set.t =
  let writes = ref Var_set.empty in
  (* 1. collect all array writes *)
  Target.iter (fun p ->
    let waccess_t = Target.resolve_path p in
    writes := Var_set.add (var_of_access waccess_t) !writes;
  ) ((target_of_path p) @ [nbAny; cArrayWriteAccess ""]);
  (* 2. filter out all writes to locally defined arrays *)
  Target.iter (fun p ->
    let vdef_t = Target.resolve_path p in
    writes := Var_set.remove (var_of_def vdef_t) !writes;
  ) ((target_of_path p) @ [nbAny; cVarDef ""]);
  !writes

(*
 [tile]: allows fusing the stencils by chaining tiled computations, rather than chaining individual computations.
 [overlaps]: list of [var, overlap] pairs, where [var] is a variable being written to by a loop, that needs to be produced in tiles of [tile_size + overlap] due to following dependencies.
 [outputs]: list of variables to keep alive after the stencil chain is fused.
 *)
let%transfo fusion_targets_tile (tile : trm list) ?(overlaps : (string * (trm list)) list = []) ~(outputs : string list) ?(simpl : target -> unit = Arith.default_simpl) ?(fuse_inner_loops : bool = true) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  let outer_loop_count = List.length tile in
  let surrounding_sequence = ref None in
  let overlaps': (var * (trm list)) list ref = ref [] in
  let outputs': Var_set.t ref = ref Var_set.empty in
  let must_be_in_surrounding_sequence p =
    let (_, p_seq) = Path.index_in_seq p in
    match !surrounding_sequence with
    | None ->
      surrounding_sequence := Some p_seq;
      let find_var n = find_var n (target_of_path p_seq) in
      overlaps' := List.map (fun (v_name, l) -> (find_var v_name, l)) overlaps;
      outputs' := Var_set.of_list (List.map find_var outputs);
    | Some p_seq' -> assert (p_seq = p_seq')
  in
  Marks.with_fresh_mark (fun to_fuse ->
    let all_writes = ref Var_map.empty in
    (* 1. prepare loop nests for fusion *)
    Trace.step ~kind:Step_group ~name:("1. prepare loop nests for fusion") (fun () ->
    Target.iteri (fun loop_i p ->
      must_be_in_surrounding_sequence p;
      (* 1.1 pry out each target to reveal loop nests *)
      pry_loop_nest outer_loop_count simpl p;
      let writes = collect_writes p in
      (* 1.2. slide each loop nests as necessary *)
      let overlap_tile =
        begin match List.find_opt (fun (w, _) ->
          Var_set.mem w writes
        ) !overlaps' with
        | Some (w, ot) -> ot
        | None -> List.map (fun _ -> trm_int 0) tile
        end
      in
      let sizes = List.map2 (fun tile_size overlap ->
        if is_trm_int 0 overlap then tile_size
        else trm_add tile_size overlap
      ) tile overlap_tile in
      let steps = tile in
      let inner_loop_indices = may_slide (Var_set.elements writes) sizes steps ~simpl p in
      Var_set.iter (fun w ->
        assert (not (Var_map.mem w !all_writes));
        all_writes := Var_map.add w (sizes, inner_loop_indices) !all_writes
      ) writes;
      (* Show.current_ast_at_path "slided" p; *)
      Marks.add to_fuse (target_of_path p);
    ) tg;
    );
    (* 2. fuse loop nests *)
    let to_fuse_paths = Target.resolve_target [nbMulti; cMark to_fuse] in
    let nest_to_fuse = if fuse_inner_loops
      then outer_loop_count + (List.length tile) else outer_loop_count in
    Trace.step ~kind:Step_group ~name:("2. fuse loop nests") (fun () ->
    if fuse_inner_loops then begin
      match to_fuse_paths with
      | first :: others ->
        loop_align_stop_extend_start_like ~nest_of:nest_to_fuse ~simpl ~orig:(target_of_path first) (target_of_paths others)
      | _ -> ()
    end;
    let rename loop_p =
      let writes = List.map (fun v -> v.name) (Var_set.elements (collect_writes loop_p)) in
      Some (Variable.Rename.AddSuffix (Tools.list_to_string ~sep:"_" ~bounds:("_","") writes))
    in
    (* Show.current_ast_at_target "before fusion" [nbMulti; cMark to_fuse]; *)
    (* DEPRECATED: ~into: (target_of_path (snd (List.unlast to_fuse_paths))) *)
    Loop.fusion_targets ~nest_of:nest_to_fuse ~rename ~into:FuseIntoLast (target_of_paths to_fuse_paths);
    );
    (* Show.current_ast_at_target "after fusion" [nbMulti; cMark to_fuse]; *)
    (* 3. reduce temporary storage *)
    let surrounding_seq = Option.unsome !surrounding_sequence in
    let local_memory = Var_map.filter (fun v _ -> not (Var_set.mem v !outputs')) !all_writes in
    let fused_p = resolve_mark_exactly_one to_fuse in
    let outer_loop_indices = Loop.get_indices outer_loop_count fused_p in
    let reduce_local_memory var (sizes, _) =
      let fused_p = resolve_mark_exactly_one to_fuse in
      let inner_p = Path.to_inner_loop_n outer_loop_count fused_p in
      let alloc_instr = (target_of_path surrounding_seq) @ [cVarDef var.name] in
      let alloc_trm = get_trm_at_exn (alloc_instr @ [dInit]) in
      let error = "Stencil.fusion_targets_tile: expected allocation instruction" in
      let (dims, _ty, _size) = trm_inv ~error Matrix_core.alloc_inv_with_ty alloc_trm in
      let may_eliminate = (List.length sizes) = (List.length dims) && (List.for_all (is_trm_int 1) sizes) in
      if may_eliminate then
        Matrix.elim alloc_instr
      else begin
        let touched_nd_tiles = List.map2 (fun size idx -> (trm_var idx, trm_add (trm_var idx) size)) sizes outer_loop_indices in
        let untouched_nd_tiles = List.map (fun size -> (trm_int 0, size)) (List.drop (List.length sizes) dims) in
        let nd_tiles = touched_nd_tiles @ untouched_nd_tiles in
        Matrix.local_name_tile_after ~var:var.name ~alloc_instr ~simpl ~tile:nd_tiles (target_of_path inner_p);
      end;
    in
    (* TODO: iter in reverse order of code appearance. *)
    Trace.step ~kind:Step_group ~name:("3. reduce temporary storage") (fun () ->
    Var_map.iter reduce_local_memory local_memory
    )
  )

let fusion_targets ~(nest_of : int) ?(overlaps : (string * (trm list)) list = []) ~(outputs : string list) ?(simpl : target -> unit = Arith.default_simpl) ?(fuse_inner_loops : bool = false) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  fusion_targets_tile (List.init nest_of (fun _ -> trm_int 1)) ~overlaps ~outputs ~simpl ~fuse_inner_loops tg
