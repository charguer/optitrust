open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
    !! Sequence_basic.elim_around_instr [cVarDef "x"];
    !! Sequence_basic.elim_around_instr [cVarDef "y"];

    (* FIXED: rename inline to elim
             rename unwrap to elim_around_instr
      LATER: think about better names
    FIXED:
    Sequence.elim_around_instr tg =
      resolve tg (fun p ->
         let pseq = get_surrounding_seq p
         Sequence.elim pseq)

     symmetry/specialization suggested by the names
         intro   intro_instr
         elim    elim_instr
    *)
)
