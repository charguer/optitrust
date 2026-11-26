open Optitrust
open Target
open Prelude

let _ = Flags.typechecking_mode := Flags.ProofPreserving
let _ = Flags.recompute_resources_between_steps := true

let _ = Run.script_cpp (fun () ->
  !! Matrix.pad ~var:"a" 2 2 1 [cFunDef "main1"(* ; cVarDef "a" *)];
  !! Matrix.pad ~var:"a" 2 1 10 [cFunDef "main2"(* ; cVarDef "a" *)];

  !! Matrix.pad ~var:"a" 3 1 2 [cFunDef "main3"(* ; cVarDef "a" *)];
  !! Matrix.pad ~var:"b" 4 4 32 [cFunDef "main3"(* ; cVarDef "a" *)];
  Trace.recompute_resources ();
  )
