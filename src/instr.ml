open Ast
include Instr_basic




(* [read_last_write ~write tg] expects the target [tg] to be pointing at a read operation
    then it will take the value of the write operation [write] and replace the current read operation
    with that value, if [tg] doesn't point to a read operation then the transformation will fail
*)

let read_last_write ?(write : Target.target = []) : Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_apps (_, [arg]) when is_get_operation tg_trm ->
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
                   | Trm_apps (_, [ls; _rs]) when is_set_operation t1 ->
                     if Internal.same_trm ls arg then write_index := Some i else ()
                   | Trm_let (_, (x, _), _ ) when Internal.same_trm (trm_var x) arg ->
                      write_index := Some i
                   | _ -> ()
                   end
          ) tl
        | _ -> fail seq_trm.loc (Printf.sprintf "read_last_write: expected the sequence which contains the targeted get operation got %s" (Ast_to_c.ast_to_string seq_trm))
        end;
        begin match !write_index with
        | Some index ->
          let write =  (Target.target_of_path path_to_seq) @ [Target.dSeqNth index] in
          Instr_basic.read_last_write ~write (Target.target_of_path p)
        | None -> fail tg_trm.loc "read_last_write: couuldn't find a write operation for your targeted read operation"
        end
      | _ -> Instr_basic.read_last_write  ~write (Target.target_of_path p)
      end
    | _ -> fail tg_trm.loc "read_last_write: the main target should be a get operation"
  )

(* [inline_last_write ~write ~delete tg] this tranformation is a version of read last write, in fact it will call
    the basic read_last_write transformation and then it will delete the write operation. So the main difference between
    these two transformations is that the later one deletes the write operation
*)
let inline_last_write ?(write : Target.target = []) (tg : Target.target) : unit =
  Instr_basic.read_last_write ~write tg;
  if write <> [] then  Instr_basic.delete write


(* [accumulate tg] expects the target [tg] to point to a block of write operations in the same memory location
    or to a single instruction and [nb] the number of the instructions after the targetd instruction that needs to be
    considered.
    Ex.
    int x;
    {
      x += 1;
      x += 2;
      x += 3;
    }
    is transformed to x += 1+2+3
*)
let accumulate ?(nb : int option) : Target.Transfo.t =
  Target.iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    begin match tg_trm.desc with
    | Trm_seq _ -> Instr_basic.accumulate (Target.target_of_path p)
    | _  when is_set_operation tg_trm ->
      begin match nb with
      | Some n ->
        Sequence_basic.intro ~mark:"temp_MARK" n (Target.target_of_path p);
        Instr_basic.accumulate [Target.cMark "temp_MARK"]
      | _ -> fail t.loc "accumulate: if the given target is a write operation please provide me the number [nb] of instructions to consider in the accumulation"
      end
    | _ -> fail t.loc "accumulate: expected a target to a sequence or a target to an instruction and the number of instructions to consider too"
    end
  )


(* [accummulate_targets tg] similar to accummulate but here one can target multiple consecutive targets at the same time, and the number of
    targets is not needed.
*)
let accumulate_targets (tg : Target.target) : unit =
  let mark = Mark.next() in
  Sequence.intro_targets ~mark tg;
  Instr_basic.accumulate  [Target.cMark mark]

type gather_dest = GatherAtFirst | GatherAtLast | GatherAt of Target.target



(* [gather_targets ~dest tg] expects the target [tg] pointing to a list to multiple nodes belonging to the
    same sequence. Then it will move all those targets to the given destination.
    NOTE: No need to write explicitly nbMulti before the main target
*)
let gather_targets ?(dest : gather_dest = GatherAtLast) (tg : Target.target) : unit =
  let tg = Target.filter_constr_occurrence tg in
  let tg_dest = ref [] in
  let reverse = ref true in
  begin match dest with
  | GatherAtFirst ->
    tg_dest := [Target.tAfter; Target.occFirst] @ tg;
    reverse := true
  | GatherAtLast ->
    tg_dest := [Target.tBefore; Target.occLast] @ tg;
    reverse := false
  | GatherAt tg_dest1 ->
    tg_dest := tg_dest1;
    match Target.get_relative_type tg_dest1 with
    | Some tg_rel ->
      begin match tg_rel with
      | TargetBefore ->
        reverse := false
      | TargetAfter ->
        reverse := true
      | TargetFirst ->
        reverse := true
      | TargetLast ->
        reverse := false
      | TargetAt -> fail None "gather_targets: if you used GatherAt you should provide a valid relative target"
      end
    | None -> fail None "gather_targets: if you used GatherAt you should provide a valid relative target"
  end;
  let tg = Target.enable_multi_targets tg in
  Instr_basic.move ~rev:!reverse ~dest:!tg_dest tg


(* [move_multiple ~targets tgs] expects a list of destinations and a list of targets to be movet at those
    destinations, the map is based on the indices, ex target and index 1 will be move at the destination 1 and so on.
*)
let move_multiple ~destinations:(destinations : Target.target list)  ~targets:(targets : Target.target list ) : unit =
  if List.length destinations <> List.length targets then fail None "move_multiple: each destination corresponds to a single target and vice-versa";
  List.iter2(fun dest tg1 -> Instr_basic.move ~dest tg1) destinations targets

(* [move_out dest tg] move the invariant [tg] to destination [dest]
   Note: The transformation does not check if [tg] points to some invariant code or not
   LATER: Check if [tg] is dependent on other instructions of the same scope
*)
let move_out ?(rev : bool = false) ~dest:(dest : Target.target) : Target.Transfo.t =
  Target.iter_on_targets ~rev (fun t p ->
    let tg_trm = Path.resolve_path p t in
    Marks.add "instr_move_out" (Target.target_of_path p);
    Sequence_basic.insert ~reparse:false tg_trm dest;
    Instr_basic.delete [Target.cMark "instr_move_out"]
)
