open Prelude
open Target
include Instr_basic

let insert = Sequence.insert
let delete = Sequence.delete

(** [read_last_write ~write tg]: similar to [Instr_basic.read_last_write] except that this transformation
     tries to find the last write instead of asking explicitly for a target to that write,
     [write_mark]: a mark used by the transformation [inline_last_write] to mark the instruction that needs to
                   be deleted
     [write]: optional explicit target to the last write instruction
     Note: This transformation will fail in case it doesn't find a write to to the address that it reads from. *)
let%transfo read_last_write ?(write_mark : mark = no_mark) ?(write : target = []) (tg : target) : unit =
  if write <>  []
    then Instr_basic.read_last_write ~write tg
    else begin
      Target.iter (fun p ->
        let tg_trm = Target.resolve_path p in
        let arg = get_operation_arg tg_trm in
          begin match write with
          | [] ->
            let path_to_seq, _, index = Internal.get_instruction_in_surrounding_sequence p in
            let seq_trm = Target.resolve_path path_to_seq in
            let write_index = ref None in
            begin match seq_trm.desc with
            | Trm_seq (tl, _) ->
              Mlist.iteri (fun i t1 ->
                if i >= index
                  then ()
                  else begin match t1.desc with
                       | Trm_apps (_, [ls; _rs], _, _) when is_set_operation t1 ->
                         if Trm_unify.are_same_trm ls arg then write_index := Some i else ()
                       | Trm_let ((x, _), _) when Trm_unify.are_same_trm (trm_var x) arg ->
                          write_index := Some i
                       | _ -> ()
                       end
              ) tl
            | _ -> trm_fail seq_trm (Printf.sprintf "Instr.read_last_write: expected the sequence which contains the targeted get operation got %s" (Ast_to_c.ast_to_string seq_trm))
            end;
            begin match !write_index with
            | Some index ->
              let write =  (target_of_path path_to_seq) @ [dSeqNth index] in
              Instr_basic.read_last_write ~write  (target_of_path p);
              Marks.add write_mark write

            | None -> trm_fail tg_trm "Instr.read_last_write: couuldn't find a write operation for your targeted read operation"
            end
          | _ ->
              Instr_basic.read_last_write ~write (target_of_path p)
          end
    ) tg end

(** [inline_last_write ~write ~write_mark tg]: similar to [read_last_write] except that this one
    deletes the last write operation. *)
let%transfo inline_last_write ?(write : target = []) ?(write_mark : mark = "__todelete__") (tg : target) : unit =
  let write_mark = if write = [] then Some write_mark else None in
  read_last_write ~write ?write_mark tg;
  if write <> [] then  Instr_basic.delete write else Instr_basic.delete [nbMulti;cMark "__todelete__"]

(** [accumulate tg]: similar to [Instr_basic.accumulate], however this transformation requires the user to use a
    different target. For this transformation the user needs to provide a target to the first instruction
    and the number [nb] of consecutive instructions that can be accumulated into a single one. *)
let%transfo accumulate ?(nb : int option) (tg : target) : unit =
  Target.iter (fun p ->
    let tg_trm = Target.resolve_path p in
    begin match tg_trm.desc with
    | Trm_seq _ -> Instr_basic.accumulate (target_of_path p)
    | _  when is_set_operation tg_trm ->
      begin match nb with
      | Some n ->
        Sequence_basic.intro ~mark:"temp_MARK" n (target_of_path p);
        Instr_basic.accumulate [cMark "temp_MARK"]
      | _ -> trm_fail tg_trm "Instr.accumulate: if the given target is a write operation please
           provide me the number [nb] of instructions to consider in the accumulation"
      end
    | _ -> trm_fail tg_trm "Instr.accumulate: expected a target to a sequence or a target to an instruction and the number of instructions to consider too"
    end
  ) tg

(** [accumulate_targets tg]: similar to [Instr_basic.accumulate], the main difference is that this transformation
     expects the target [tg] to point to multiple instructions instead of a sequence that consists of multiple
     instructions that can be reduced to a single one. Here the regrouping is done automatically. *)
let%transfo accumulate_targets (tg : target) : unit =
  let mark = Mark.next() in
  Sequence.intro_targets ~mark tg;
  Instr_basic.accumulate [cMark mark]

(** [move_in_seq ~dest tg] perform the same actions as {!Instr_basic.move},
   but move the instructions with the ghost pairs they need around them. *)
let%transfo move_in_seq ~(dest: target) (tg: target) : unit =
  if !Flags.resource_typing_enabled then
    Target.iter (fun p ->
      let seq_path, span = Path.extract_last_dir_span p in
      let t_seq = Target.resolve_path seq_path in
      let dest_index = match Constr.resolve_target_between_children dest t_seq with
        | [i] -> i
        | [] -> trm_fail t_seq "Instr.move_in_seq: could not find the destination target";
        | _ -> trm_fail t_seq "Instr.move_in_seq: the destination target should be unique";
      in

      let mark = Mark.next () in
      let (mark_begin, mark_end) = span_marks mark in
      let dest_mark = Mark.next () in
      Marks.add dest_mark (target_of_path (seq_path @ [Dir_before dest_index]));

      Ghost_pair.fission ~mark_between:mark_end (target_of_path (seq_path @ [Dir_before span.stop]));
      Ghost_pair.fission ~mark_between:mark_begin (target_of_path (seq_path @ [Dir_before span.start]));

      (* TODO: Make a better API for this kind of mark in sequence *)
      let seq_dest_mark = Mark.next () in
      Marks.add_fake_instr seq_dest_mark [cMark dest_mark];
      Ghost_pure.move_all_upwards (target_of_path seq_path);
      Ghost_pair.minimize_all_in_seq (target_of_path seq_path);

      Instr_basic.move ~dest:[cMark seq_dest_mark; tBefore] [cMarkSpan mark];

      Marks.remove_fake_instr [cMark seq_dest_mark];
      Marks.remove_st (fun m -> m = mark_begin || m = mark_end || m = dest_mark) (target_of_path seq_path);
    ) tg
  else
    Instr_basic.move ~dest tg

(* DEPRECATED: Use immediate children targets instead *)
type gather_dest = GatherAtFirst | GatherAtLast | GatherAt of target


(** [gather_targets ~dest tg]: expects the target [tg] to point to one or more instructions, than
    it will move this instructions just before the instruction targeted by [dest].

    DEPRECATED with this interface:
    It should be replaced by a more robust equivalent using relative immediate children targets.

    Note: This transformation assumes that [tg]  is going to be pointing at multiple instructions,
    hence no need to provide the occurrecen target [nbMulti] before the main target.

    TODO: add flags to choose:
    - failing if the instructions cannot be put strictly side-to-side
    - allowing ghosts to be moved automatically if there is a dependence
    - allowing non-ghosts to be moved
   *)
let%transfo gather_targets ?(dest : gather_dest = GatherAtLast) (tg : target) : unit =
  Marks.with_marks (fun next_mark ->
    let paths = resolve_target (enable_multi_targets tg) in
    (* TODO: factorize with move_in_seq and Loop.fusion_targets *)
    let seq_p = ref None in
    let assert_seq_p seq_p' =
      match !seq_p with
      | None -> seq_p := Some seq_p';
      | Some sp ->
        if sp <> seq_p' then path_fail seq_p' "paths are not in the same sequence";
    in
    let indices = List.map (fun p ->
      let (i, seq_p) = Path.index_in_seq p in
      assert_seq_p seq_p;
      (* let m = Marks.add_next_mark_on next_mark p in
      printf "m = %s\n" m;
      (i, m) *)
      i
    ) paths in
    let sorted_indices = List.sort compare indices in

    let move_index_marks dest to_move =
      (* let dest_p = resolve_target_exactly_one dest in
      let (seq_p, dest_index) = Path.extract_last_dir_before dest_p in
      assert_seq_p seq_p; *)
      let seq_p = Option.unsome !seq_p in
      let tg_span span = target_of_path (seq_p @ [Dir_span span]) in

      (*
      let dest_mark = Marks.add_next_mark next_mark (target_of_path dest_p) in
      let instr_dest_mark = next_mark () in
      Marks.add_fake_instr instr_dest_mark [cMark dest_mark]; *)

      (* move a span of instrs to a destination,
         along with necessary dependencies collected on the way *)
      let rec move_downwards_with_deps (span : Dir.span) (dest : int) : Dir.span =
        (* printf "move downwards (%d, %d) - %d\n" span.start span.stop dest; *)
        if span.stop >= dest then
          (* the span of instruction has reached the destination *)
          span
        else
          match Trace.step_backtrack_on_failure (fun () ->
            Instr_basic.move ~dest:[dAfter span.stop] (tg_span span)
          ) with
          | Success () ->
            move_downwards_with_deps { start = span.start + 1; stop = span.stop + 1 } dest
          | Failure _ ->
            move_downwards_with_deps { start = span.start; stop = span.stop + 1 } dest
      in

      let rec move_upwards_with_deps (span : Dir.span) (dest : int) : Dir.span =
        (* printf "move upwards (%d, %d) - %d\n" span.start span.stop dest; *)
        if span.start <= (dest + 1) then
          (* the span of instruction has reached the destination *)
          span
        else
          match Trace.step_backtrack_on_failure (fun () ->
            Instr_basic.move ~dest:[dBefore (span.start - 1)] (tg_span span)
          ) with
          | Success () ->
            move_upwards_with_deps { start = span.start - 1; stop = span.stop - 1 } dest
          | Failure _ ->
            move_upwards_with_deps { start = span.start - 1; stop = span.stop } dest
      in

      (* printf "dest = %d\n" dest;
      printf "to_move = %s\n" (Tools.list_to_string (List.map string_of_int to_move)); *)
      let (above, below) = match List.find_index (fun i -> i > dest) to_move with
      | Some i -> (*Tools.debug "split_at %i\n" i;*) List.split_at i to_move
      | None -> (to_move, [])
      in

      if above <> [] then begin
        let above_span = ref None in
        let above_dests = (List.drop 1 above) @ [dest] in
        List.iter2 (fun i dest ->
          (* printf "above: i = %d, dest = %d\n" i dest; *)
          above_span := Some (match !above_span with
          | None -> move_downwards_with_deps { start = i; stop = i + 1 } dest
          | Some span -> move_downwards_with_deps { span with stop = i + 1 } dest);
        ) above above_dests;
      end;

      if below <> [] then begin
        let below_rev = List.rev below in
        let below_span = ref None in
        let below_dests = (List.drop 1 below_rev) @ [dest] in
        List.iter2 (fun i dest ->
          (* printf "below: i = %d, dest = %d\n" i dest; *)
          below_span := Some (match !below_span with
          | None -> move_upwards_with_deps { start = i; stop = i + 1 } dest
          | Some span -> move_upwards_with_deps { span with start = i } dest);
        ) below_rev below_dests;
      end;

      (* Marks.remove_fake_instr [cMark instr_dest_mark]; *)
    in
    begin match dest with
    | GatherAtFirst ->
      let (first, rest) = List.uncons sorted_indices in
      move_index_marks first rest
    | GatherAtLast ->
      let (rest, last) = List.unlast sorted_indices in
      move_index_marks last rest
    | GatherAt tg_dest ->
      failwith "GatherAt not yet implemented, need to move instrs before and after target in right order, maybe Instr.move should already behave like GatherAt?"
    end)

(** [move dest tg]: move the instructions pointed by [tg] to destination [dest]
   Note: If checking validity, dest must be in the same sequence as the moved instructions.
*)
let%transfo move ~(dest : target) (tg : target) : unit =
  Trace.tag_atomic ();
  if !Flags.check_validity then
    (* TODO: handle move out of loop, conditions, etc. *)
    Target.iter (fun p ->
      let seq_path, span = Path.extract_last_dir_span p in
      let dest_path, i = Target.resolve_target_between_exactly_one dest in
      if seq_path <> dest_path then
        path_fail dest_path "Instr.move: Unsupported move outside the sequence when checking validity";
      move_in_seq ~dest:[dBefore i] (target_of_path p)
    ) tg
  else begin
    Target.iter (fun p ->
      let tg_trm = Target.resolve_path p in
      Marks.add "instr_move_out" (target_of_path p);
      Sequence_basic.insert tg_trm dest;
      Instr_basic.delete [cMark "instr_move_out"]) tg
  end

(** [move_out tg]: moves the instruction targeted by [tg], just before its surrounding sequence. *)
let%transfo move_out (tg : target) : unit =
  Target.iter (fun p ->
    let (seq, _) = try Internal.isolate_last_dir_in_seq p with | Contextualized_error _ ->
      path_fail p "Instr.move_out: only instructions can be targeted" in
    let tg_instr = target_of_path p in
    move ~dest:[cPath seq; tBefore] tg_instr
  ) tg

(** [move_out_of_fun tg]: moves the instruction targeted by [tg] just befor the toplevel declaration function
    that it belongs to. *)
let%transfo move_out_of_fun (tg : target) : unit =
  Target.iteri (fun i p ->
    let path_to_topfun = Internal.get_ascendant_topfun_path p in
    let mark = "move_out_fun" ^ (string_of_int i) in
    match path_to_topfun with
    | Some pf -> begin
      Marks.add mark (target_of_path pf);
      let tg_instr = target_of_path p in
      move ~dest:[cMark mark; tBefore] tg_instr;
      Marks.remove mark [cMark mark]
      end
    | None -> path_fail p "Instr.move_out_of_fun: can't move toplevel instructions"

  ) tg

(* TODO: %transfo ? *)
(** [set_atomic tg]: just an alias to Omp.atomic tg, please refer to omp_basic.ml  line 9 *)
let set_atomic : target -> unit =
  Omp_basic.atomic

(* TODO: %transfo ? *)
(** [unset_atomic ty]: the opposite of [set_atomic]. *)
let unset_atomic : target -> unit =
  apply_at_target_paths (trm_filter_pragma (function | Atomic _ -> false | _ -> true))
