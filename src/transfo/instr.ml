open Prelude
open Target
include Instr_basic

let insert = Sequence.insert
let delete = Sequence.delete

(* [read_last_write ~write tg]: similar to [Instr_basic.read_last_write] except that this transformation
     tries to find the last write instead of asking explicitly for a target to that write,
     [write_mark]: a mark used by the transformation [inline_last_write] to mark the instruction that needs to
                   be deleted
     [write]: optional explicit target to the last write instruction
     Note: This transformation will fail in case it doesn't find a write to to the address that it reads from. *)
let%transfo read_last_write ?(write_mark : mark = no_mark) ?(write : target = []) (tg : target) : unit =
  if write <>  []
    then Instr_basic.read_last_write ~write tg
    else begin
      iter_on_targets (fun t p ->
        let tg_trm = Path.resolve_path p t in
        let arg = get_operation_arg tg_trm in
          begin match write with
          | [] ->
            let path_to_seq, _, index = Internal.get_instruction_in_surrounding_sequence p in
            let seq_trm = Path.resolve_path path_to_seq t in
            let write_index = ref None in
            begin match seq_trm.desc with
            | Trm_seq tl ->
              Mlist.iteri (fun i t1 ->
                if i >= index
                  then ()
                  else begin match t1.desc with
                       | Trm_apps (_, [ls; _rs], _) when is_set_operation t1 ->
                         if Internal.same_trm ls arg then write_index := Some i else ()
                       | Trm_let (_, (x, _), _) when Internal.same_trm (trm_var x) arg ->
                          write_index := Some i
                       | _ -> ()
                       end
              ) tl
            | _ -> trm_fail seq_trm (Printf.sprintf "Instr.read_last_write: expected the sequence which contains the targeted get operation got %s" (AstC_to_c.ast_to_string seq_trm))
            end;
            begin match !write_index with
            | Some index ->
              let write =  (target_of_path path_to_seq) @ [dSeqNth index] in
              Instr_basic.read_last_write ~write  (target_of_path p);
              Marks.add write_mark write

            | None -> trm_fail tg_trm "Instr.read_last_write: couuldn't find a write operation for your targeted read operation"
            end
          | _ ->
              Instr_basic.read_last_write  ~write (target_of_path p)
          end
    ) tg end

(* [inline_last_write ~write ~write_mark tg]: similar to [read_last_write] except that this one
    deletes the last write operation. *)
let%transfo inline_last_write ?(write : target = []) ?(write_mark : mark = "__todelete__") (tg : target) : unit =
  let write_mark = if write = [] then Some write_mark else None in
  read_last_write ~write ?write_mark tg;
  if write <> [] then  Instr_basic.delete write else Instr_basic.delete [nbMulti;cMark "__todelete__"]

(* [accumulate tg]: similar to [Instr_basic.accumulate], however this transformation requires the user to use a
    different target. For this transformation the user needs to provide a target to the first instruction
    and the number [nb] of consecutive instructions that can be accumulated into a single one. *)
let%transfo accumulate ?(nb : int option) (tg : target) : unit =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    begin match tg_trm.desc with
    | Trm_seq _ -> Instr_basic.accumulate (target_of_path p)
    | _  when is_set_operation tg_trm ->
      begin match nb with
      | Some n ->
        Sequence_basic.intro ~mark:"temp_MARK" n (target_of_path p);
        Instr_basic.accumulate [cMark "temp_MARK"]
      | _ -> trm_fail t "Instr.accumulate: if the given target is a write operation please
           provide me the number [nb] of instructions to consider in the accumulation"
      end
    | _ -> trm_fail t "Instr.accumulate: expected a target to a sequence or a target to an instruction and the number of instructions to consider too"
    end
  ) tg

(* [accumulate_targets tg]: similar to [Instr_basic.accumulate], the main difference is that this transformation
     expects the target [tg] to point to multiple instructions instead of a sequence that consists of multiple
     instructions that can be reduced to a single one. Here the regrouping is done automatically. *)
let%transfo accumulate_targets (tg : target) : unit =
  let mark = Mark.next() in
  Sequence.intro_targets ~mark tg;
  Instr_basic.accumulate [cMark mark]

(* DEPRECATED: Use immediate children targets instead *)
type gather_dest = GatherAtFirst | GatherAtLast | GatherAt of target


(* [gather_targets ~dest tg]: expects the target [tg] to point to one or more instructions, than
    it will move this instructions just before the instruction targeted by [dest].

    DEPRECATED with this interface:
    It should be replaced by a more robust equivalent using relative immediate children targets.

    Note: This transformation assumes that [tg]  is going to be pointing at multiple instructions,
    hence no need to provide the occurrecen target [nbMulti] before the main target. *)
let%transfo gather_targets ?(dest : gather_dest = GatherAtLast) (tg : target) : unit =
  (* FIXME: This is heavily broken with the new Instr_basic.move.
     Moreover, the approach taken here cannot be validated most of the time.
     Use instead a mark that moves with instructions *)
  let tg = filter_constr_occurrence tg in
  let tg_dest = ref [] in
  let reverse = ref true in
  begin match dest with
  | GatherAtFirst ->
    tg_dest := [tAfter; occFirst] @ tg;
    reverse := true
  | GatherAtLast ->
    tg_dest := [tBefore; occLast] @ tg;
    reverse := false
  | GatherAt tg_dest1 ->
    tg_dest := tg_dest1;
    match get_relative_type tg_dest1 with
    | Some tg_rel ->
      begin match tg_rel with
      | TargetBefore ->
        reverse := false
      | TargetAfter ->
        reverse := true
      | TargetBetweenAll ->
        failwith "Instr.gather_targets: need to support TargetBetweenAll"
      | TargetFirst ->
        reverse := true
      | TargetLast ->
        reverse := false
      | TargetAt -> failwith "Instr.gather_targets: if you used GatherAt you should provide a valid relative target"
      end
    | None -> failwith "Instr.gather_targets: if you used GatherAt you should provide a valid relative target"
  end;
  let tg = enable_multi_targets tg in
  Instr_basic.move ~rev:!reverse ~dest:!tg_dest tg

(* [move dest tg]: move the invariant [tg] to destination [dest]
   Note: The transformation does not check if [tg] points to some invariant code or not
   LATER: Check if [tg] is dependent on other instructions of the same scope *)
let%transfo move ~dest:(dest : target) (tg : target) : unit =
  Trace.tag_atomic ();
  if !Flags.check_validity then
    (* TODO: handle move out of loop, conditions, etc. *)
    (* TODO: handle moving together with ghost scopes. *)
    Instr_basic.move ~dest tg
  else begin
    iter_on_targets (fun t p ->
      let tg_trm = Path.resolve_path p t in
      Marks.add "instr_move_out" (target_of_path p);
      Sequence_basic.insert tg_trm dest;
      Instr_basic.delete [cMark "instr_move_out"]) tg
  end

(* [move_out tg]: moves the instruction targeted by [tg], just before its surrounding sequence. *)
let%transfo move_out (tg : target) : unit =
  iter_on_targets (fun t p ->
    let (seq, _) = try Internal.isolate_last_dir_in_seq p with | Contextualized_error _ ->
      path_fail p "Instr.move_out: only instructions can be targeted" in
    let tg_seq = target_of_path seq in
    let tg_instr = target_of_path p in
    move ~dest:tg_seq tg_instr
  ) tg

(* [move_out_of_fun tg]: moves the instruction targeted by [tg] just befor the toplevel declaration function
    that it belongs to. *)
let%transfo move_out_of_fun (tg : target) : unit =
  iteri_on_targets (fun i t p ->
    let path_to_topfun = Internal.get_ascendant_topfun_path p in
    let mark = "move_out_fun" ^ (string_of_int i) in
    match path_to_topfun with
    | Some pf -> begin
      Marks.add mark (target_of_path pf);
      let tg_instr = target_of_path p in
      move ~dest:[cMark mark] tg_instr;
      Marks.remove mark [cMark mark]
      end
    | None -> path_fail p "Instr.move_out_of_fun: can't move toplevel instructions"

  ) tg

(* TODO: %transfo ? *)
(* [set_atomic tg]: just an alias to Omp.atomic tg, please refer to omp_basic.ml  line 9 *)
let set_atomic : Transfo.t =
  Omp_basic.atomic

(* TODO: %transfo ? *)
(* [unset_atomic ty]: the opposite of [set_atomic]. *)
let unset_atomic : Transfo.t =
  apply_on_targets (apply_on_path (trm_filter_pragma (function | Atomic _ -> false | _ -> true)))
