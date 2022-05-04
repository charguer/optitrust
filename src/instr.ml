open Ast
open Target
include Instr_basic


(* [read_last_write ~write tg]: similar to [Instr_basic.read_last_write] except that this transformation 
     tries to find the last write instead of asking explicitly for a target to that write,
     [write_mark]: a mark used by the transformation [inline_last_write] to mark the instruction that needs to 
                   be deleted
     [write]: optional explicit target to the last write instruction 
     Note: This transformation will fail in case it doesn't find a write to to the address that it reads from. *)
let read_last_write ?(write_mark : mark option = None) ?(write : target = []) (tg : target) : unit =
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
                       | Trm_apps (_, [ls; _rs]) when is_set_operation t1 ->
                         if Internal.same_trm ls arg then write_index := Some i else ()
                       | Trm_let (_, (x, _), _ ) when Internal.same_trm (trm_var x) arg ->
                          write_index := Some i
                       | _ -> ()
                       end
              ) tl
            | _ -> fail seq_trm.loc (Printf.sprintf "Instr.read_last_write: expected the sequence which contains the targeted get operation got %s" (AstC_to_c.ast_to_string seq_trm))
            end;
            begin match !write_index with
            | Some index ->
              let write =  (target_of_path path_to_seq) @ [dSeqNth index] in
              Instr_basic.read_last_write ~write  (target_of_path p);
              begin match write_mark with | Some m -> Marks.add m write | _ -> () end

            | None -> fail tg_trm.loc "Instr.read_last_write: couuldn't find a write operation for your targeted read operation"
            end
          | _ -> 
              Instr_basic.read_last_write  ~write (target_of_path p)
          end
    ) tg end

(* [inline_last_write ~write ~write_mark tg]: similar to [read_last_write] except that this one 
    deletes the last write operation. *)
let inline_last_write ?(write : target = []) ?(write_mark : mark = "__todelete__") (tg : target) : unit =
  let write_mark = if write = [] then Some write_mark else None in 
  read_last_write ~write ~write_mark  tg;
  if write <> [] then  Instr_basic.delete write else Instr_basic.delete [nbMulti;cMark "__todelete__"]
  
(* [accumulate tg]: similar to [Instr_basic.accumulate], however this transformation requires the user to use a 
    different target. For this transformation the user needs to provide a target to the first instruction 
    and the number [nb] of consecutive instructions that can be accumulated into a single one. *)
let accumulate ?(nb : int option) : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    begin match tg_trm.desc with
    | Trm_seq _ -> Instr_basic.accumulate (target_of_path p)
    | _  when is_set_operation tg_trm ->
      begin match nb with
      | Some n ->
        Sequence_basic.intro ~mark:"temp_MARK" n (target_of_path p);
        Instr_basic.accumulate [cMark "temp_MARK"]
      | _ -> fail t.loc "Instr.accumulate: if the given target is a write operation please
           provide me the number [nb] of instructions to consider in the accumulation"
      end
    | _ -> fail t.loc "Instr.accumulate: expected a target to a sequence or a target to an instruction and the number of instructions to consider too"
    end
  )

(* [accumulate_targets tg]: similar to [Instr_basic.accumulate], the main difference is that this transformation
     expects the target [tg] to point to multiple instructions instead of a sequence that consists of multiple
     instructions that can be reduced to a single one. Here the regrouping is done automatically. *)
let accumulate_targets (tg : target) : unit =
  let mark = Mark.next() in
  Sequence.intro_targets ~mark tg;
  Instr_basic.accumulate [cMark mark]

type gather_dest = GatherAtFirst | GatherAtLast | GatherAt of target


(* [gather_targets ~dest tg]: expects the target [tg] to point to one or more instructions, than 
    it will move this instructions just before the instruction targeted by [dest].
    
    Note: This transformation assumes that [tg]  is going to be pointing at multiple instructions,
    hence no need to provide the occurrecen target [nbMulti] before the main target. *)
let gather_targets ?(dest : gather_dest = GatherAtLast) (tg : target) : unit =
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
      | TargetFirst ->
        reverse := true
      | TargetLast ->
        reverse := false
      | TargetAt -> fail None "Instr.gather_targets: if you used GatherAt you should provide a valid relative target"
      end
    | None -> fail None "Instr.gather_targets: if you used GatherAt you should provide a valid relative target"
  end;
  let tg = enable_multi_targets tg in
  Instr_basic.move ~rev:!reverse ~dest:!tg_dest tg


(* [move_multiple ~destinations ~targets]: expects [targets] and definitions both to be a list of targets, such th at
     to each target there exists a unique destination where it's going to be moved. *)
let move_multiple ~destinations:(destinations : target list) ~targets:(targets : target list) : unit =
  if List.length destinations <> List.length targets then fail None "Instr.move_multiple: each destination corresponds to a single target and vice-versa";
  List.iter2(fun dest tg1 -> Instr_basic.move ~dest tg1) destinations targets

(* [move dest tg]: move the invariant [tg] to destination [dest] 
   Note: The transformation does not check if [tg] points to some invariant code or not
   LATER: Check if [tg] is dependent on other instructions of the same scope *)
let move ~dest:(dest : target) : Transfo.t =
  iter_on_targets (fun t p ->
    let tg_trm = Path.resolve_path p t in
    Marks.add "instr_move_out" (target_of_path p);
    Sequence_basic.insert ~reparse:false tg_trm dest;
    Instr_basic.delete [cMark "instr_move_out"])

(* [move_out ~dest tg]: this is just an alias for transformation [move] *)
let move_out ~dest:(dest : target) : Transfo.t = 
  move ~dest

(* TODO: Debug *)
(* [move_out_of_fun tg]: moves the instruction targeted by [tg] just befor the toplevel declaration function 
    that it belongs to. *)
let move_out_of_fun (tg : target) : unit =
  let mark = "move_out_of_fun" in 
  Marks.add mark [cTopFunDef ~body:tg ""];
  iter_on_targets 
  ( fun t p -> 
     let tg_instr = target_of_path p in 
     move_out ~dest:[cMark mark] tg_instr
  ) tg;
  Marks.remove mark [cMark mark]

(* [set_atomic tg]: just an alias to Omp.atomic tg, please refer to omp_basic.ml  line 9 *)
let set_atomic : Transfo.t = 
  Omp_basic.atomic 
