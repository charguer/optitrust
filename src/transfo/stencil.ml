open Target
open Ast

(* TODO: unit tests + document *)

type nd_tile = Matrix_core.nd_tile

let%transfo loop_align_stop_extend_start ~(start : trm) ~(stop : trm) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Target.iter (fun t p ->
    let loop_t = Path.resolve_path p t in
    let error = "Stencil.loop_align_stop_extend_start: expected simple loop" in
    let ((_index, start', _dir, stop', _step, _par), _body) = trm_inv ~error trm_for_inv loop_t in
    if (Internal.same_trm start start') && (Internal.same_trm stop stop') then
      ()
    else begin
      Loop.shift ~reparse:false ~simpl (StopAt stop) (target_of_path p);
      Loop.extend_range ~simpl ~start:(ExtendTo start) (target_of_path p)
    end
  ) tg
  (* TODO: remove following *)
  (* Trace.reparse ();
  simpl tg *)

(* TODO: ~nest_of *)
let%transfo loop_align_stop_extend_start_like ~(orig:target) ?(simpl : Transfo.t = Arith.default_simpl) (tg:target) : unit =
  let t = get_trm_at_exn orig in
  let error = "Stencil.loop_align_stop_extend_start_like: expected simple loop" in
  let ((_index, start, _dir, stop, _step, _par), _body) = trm_inv ~error trm_for_inv t in
  loop_align_stop_extend_start ~start ~stop ~simpl tg

(* TODO: inline ~delete:IfOnlyCall *)
let rec pry_loop_nest (nest_of: int) (simpl : Transfo.t) (p : path) : unit =
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
        | None -> *) fail t.loc "Stencil.pry_loop_nest: expected nested for loops, potentially hidden by function calls."
      end
  end

(*
 [tile]: allows fusing the stencils by chaining tiled computations, rather than chaining individual computations.
 [overlaps]: list of [var, overlap] pairs, where [var] is a variable being written to by a loop, that needs to be produced in tiles of [tile_size + overlap] due to following dependencies.
 [outputs]: list of variables to keep alive after the stencil chain is fused.
 *)
let%transfo fusion_targets_tile (tile : trm list) ?(overlaps : (var * (trm list)) list = []) ~(outputs : var list) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  let nest_of = List.length tile in
  let var_of_access (access_t : trm) : var =
    let error = "Stencil.fusion_targets: expected array write on base variable" in
    let (base, index) = trm_inv ~error array_access_inv access_t in
    let (_, var) = begin match trm_var_inv base with
    | Some x -> x
    | None -> trm_inv ~error trm_var_get_inv base
    end in
    var
  in
  let surrounding_sequence = ref None in
  let must_be_in_surrounding_sequence p =
    let (_, p_seq) = Path.index_in_seq p in
    match !surrounding_sequence with
    | None -> surrounding_sequence := Some p_seq
    | Some p_seq' -> assert (p_seq = p_seq')
  in
  Marks.with_fresh_mark (fun to_fuse ->
    let all_writes = ref [] in
    (* 1. prepare loop nests to fuse by prying them out and sliding them as necessary *)
    Target.iteri (fun loop_i _ p ->
      must_be_in_surrounding_sequence p;
      pry_loop_nest nest_of simpl p;
      let writes = ref [] in
      Target.iter (fun t p ->
        let waccess_t = Path.get_trm_at_path p t in
        writes := (var_of_access waccess_t) :: !writes;
      ) ((target_of_path p) @ [cArrayWriteAccess ""]);
      all_writes := !writes @ !all_writes;
      List.iter (fun (written, overlap_tile) ->
        (* if List.mem written !writes then begin *)
          let loop_indices = Loop.get_indices nest_of p in
          let waccess_t = get_trm_at_exn ((target_of_path p) @ [cArrayWriteAccess ""]) in
          let written = var_of_access waccess_t in
          let size_steps = List.map2 (fun tile_size overlap ->
            if (is_trm_int 0 overlap) && (is_trm_int 1 tile_size) then
              None
            else
              Some (trm_add tile_size overlap, tile_size)
          ) tile overlap_tile in
          (* FIXME: not correct: *)
          printf "size_steps: %s %s\n" written (Trace_printers.(list_arg_printer (option_arg_printer (fun (a, b) ->
            (AstC_to_c.ast_to_string a) ^ ", " ^(AstC_to_c.ast_to_string b)
          )) size_steps));
          Loop.slides ~iter:TileIterLocal ~size_steps ~simpl (target_of_path p);
          let slide_indices_rev = ref [] in
          List.iteri (fun i overlap ->
            if not (is_trm_int 0 overlap) then begin
              let base_index = List.nth loop_indices i in
              let new_index = base_index ^ "_" ^ written in
              slide_indices_rev := new_index :: !slide_indices_rev
            end
          ) overlap_tile;
          Loop.set_indices (loop_indices @ (List.rev !slide_indices_rev)) p;
        (* end *)
      ) overlaps;
      Debug_transfo.current_ast_at_path "slided" p;
      Marks.add to_fuse (target_of_path p);
    ) tg;
    let rename loop_p =
      let waccess_t = get_trm_at_exn ((target_of_path loop_p) @ [cArrayWriteAccess ""]) in
      let written = var_of_access waccess_t in
      Some (Variable.Rename.AddSuffix ("_" ^ written))
    in
    (* 2. fuse loop nests *)
    Debug_transfo.current_ast_at_target "before align" [nbMulti; cMark to_fuse];
    let to_fuse_paths = Target.resolve_target [nbMulti; cMark to_fuse] (Trace.ast ()) in
    (* begin match to_fuse_paths with
    | first :: others ->
      (* TODO: ~nest_of *)
      loop_align_stop_extend_start_like ~simpl ~orig:(target_of_path first) (target_of_paths others)
    | _ -> ()
    end; *)
    Debug_transfo.current_ast_at_target "before fusion" [nbMulti; cMark to_fuse];
    Loop.fusion_targets ~nest_of ~rename (target_of_paths to_fuse_paths);
    Debug_transfo.current_ast_at_target "after fusion" [nbMulti; cMark to_fuse];
    (* 3. reduce temporary storage *)
    let surrounding_seq = Tools.unsome !surrounding_sequence in
    let local_memory = Xlist.diff !all_writes outputs in
    let fused_p = path_of_target_mark_one_current_ast to_fuse in
    let loop_indices = Loop.get_indices nest_of fused_p in
    let reduce_local_memory var =
      let alloc_instr = (target_of_path surrounding_seq) @ [cVarDef var] in
      begin match List.find_opt (fun (v, _) -> var = v) overlaps with
      | Some (_, overlap_tile) ->
        let should_eliminate = false in
        (* LATER
         List.for_all (fun szst ->
          szst |> Option.map (fun (_size, step) ->
            match trm_lit_inv step with
            | Some (Lit_int 1) -> true
            | _ -> false
          ) |> Option.value ~default:true
        ) size_steps in *)
        if should_eliminate then
          Matrix.elim alloc_instr
        else begin
          let nd_tiles = List.map2 (fun overlap idx ->
            (trm_var idx, trm_add overlap (trm_int 1))
          ) overlap_tile loop_indices in
          let fused_p = path_of_target_mark_one_current_ast to_fuse in
          let inner_p = Path.to_inner_loop_n nest_of fused_p in
          Sequence.intro_after (target_of_path inner_p);
          Debug_transfo.current_ast_at_path (sprintf "before local_name_tile %d\n" (List.length nd_tiles)) inner_p;
          (* FIXME: nd_tiles *)
          Matrix.local_name_tile var ~alloc_instr ~simpl nd_tiles (target_of_path inner_p);
          let fused_p = path_of_target_mark_one_current_ast to_fuse in
          let inner_p = (Path.to_inner_loop_n (nest_of - 1) fused_p) @ [Dir_body] in
          Debug_transfo.current_ast_at_path "after local_name_tile\n" inner_p;
          Sequence.elim (target_of_path (inner_p @ [Dir_seq_nth 1]));
        end;
      | None -> Matrix.elim alloc_instr
      end
    in
    List.iter reduce_local_memory local_memory
  )

let fusion_targets ~(nest_of : int) ?(overlaps : (var * (trm list)) list = []) ~(outputs : var list) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  fusion_targets_tile (List.init nest_of (fun _ -> trm_int 1)) ~overlaps ~outputs ~simpl tg