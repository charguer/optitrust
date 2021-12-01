open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Instr.accumulate ~nb:6 [occFirst; cWriteVar "x"];
  !! Instr.accumulate ~nb:6 [occFirst; sInstr "result.x +="];

  (* TODO:
      Instr.accumulate_targets [sInstr "result.x +="]

    which is implemented as:

      let Instr.accumulate_targets tg =
        let m = fresh_mark() in
        Sequence.intro_targets ~mark:m tg;
        Instr_basic.accumulate [cMark m]


    Sequence.intro_targets is adding nbMulti to the target, resolving the targets,
    checking that the paths reach consecutive instructions within a same sequence,
    then perfom the Sequence.intro on those instructions.
   *)
)
