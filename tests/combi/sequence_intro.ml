open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->
  !! Sequence.intro ~on:[cVarDef "a"] ();
  !! Sequence.intro ~start:[tBefore; cVarDef "b"] ~stop:[tAfter; cVarDef "c"] ();
  !! Sequence.intro ~start:[cVarDef "d"] ~nb:2 ();
  !! Sequence.intro ~stop:[cVarDef "g"] ~nb:2 ();

  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tBefore; cVarDef "z"] ());
  !! Tools.failure_expected (fun () ->
       Sequence.intro ~start:[tAfter; cVarDef "z"] ~stop:[tAfter; cVarDef "z"] ());
)

(* TODO: Add more tests *)
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