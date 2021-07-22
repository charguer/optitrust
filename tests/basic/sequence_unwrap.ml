open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence.unwrap [cSeq ~args:[cVarDef "x"] ()];
    !! Sequence.unwrap [cSeq ~args:[cVarDef "y"] ()];

    (* TODO: rename inline to elim
             rename unwrap to elim_around_instr
      TODO: think about better names

    Sequence.elim_around_instr tg =
      resolve tg (fun p ->
         let pseq = get_surrounding_seq p
         Sequence.elim pseq)

     symmetry/specialization suggested by the names
         intro   intro_instr
         elim    elim_instr
    *)
)
