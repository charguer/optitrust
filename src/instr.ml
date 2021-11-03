open Ast 
include Instr_basic

let inline_last_write ~write:(write : Target.target) ?(delete : bool = true) (tg : Target.target) : unit =
  Instr_basic.read_last_write ~write tg;
  if delete then Instr_basic.delete write 

(* [accumulate tg] expects the target [tg] to point to a block of write operations in the same memory location 
    or to a single instruction and [nb] the number of the instructions after the targetd instruction that need to be
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
    let tg_trm, _ = Path.resolve_path p t in
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

(* [move_multiple ~targets tgs] expects a list of destinations and a list of targets to be movet at those 
    destinations, the map is based on the indices, ex target and index 1 will be move at the destination 1 and so on.
*)
let move_multiple ~destinations:(destinations : Target.target list)  ~targets:(targets : Target.target list ) : unit = 
  if List.length destinations <> List.length targets then fail None "move_multiple: each destination corresponds to a single target and vice-versa";
  List.iter2(fun dest tg1 -> Instr_basic.move ~dest tg1) destinations targets 