open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence.intro 1 [cVarDef "x"];
  !! Sequence.intro_between [tBefore; cVarDef "y"] [tAfter; cVarDef "t"];
  !! Sequence.intro_between [tAfter; sInstr "int u"] [tLast; cFunDef "main"; dBody];

  !! Tools.failure_expected (fun () ->
       Sequence.intro_between [tAfter; cVarDef "z"] [tBefore; cVarDef "z"]);
  !! Tools.failure_expected (fun () ->
       Sequence.intro_between [tAfter; cVarDef "z"] [tAfter; cVarDef "z"]);
)


(*
    { t1 ; t2 }   ->  { { t1 } ; t2 }

    else t1 ->   else {t1}

    low-level:
    - Sequence.intro_in_seq targetSeq indexStart nb
        // make a sub sequence
    - Sequence.intro_on_instr targetInstr
        // this is a normal target on an instruction
        // t -> { t }

    high-level:
    - Sequence.intro ~start:target_between ~stop:target_between ~nb:int ~on:target

       ~on => use intro_on_instr   (and must have no other arguments)

       ~nb -> must have start or stop
       not ~nb -> use have start and stop
         => use :  intro_in_seq targetSeq indexStart nb
*)