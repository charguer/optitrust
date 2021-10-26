open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  (*
  !! Loop.fold "i" [cLabelled "iterations"];

  *) (* NOTE: nbMulti is added by default *)

  !! Loop.fold  ~index:"k"  ~start:"0" ~stop:"3" ~step:"1" 3 [cCellWrite ~base:[cVar "values"] ~index:[cInt 0]];

  (* TODO:
  !! Loop.fold takes as target a sequence containing the list of instructions to fold
      string ?(start:string="0") ?(stop:string) ?(step:string="1")
      by default "stop" is the number of items in that sequence

      Loop.fold "k" [cLabelled "foo"]

      TODO: let cLabelled x = cChain [cConstrLabel x; dBody ]

  !! Loop.fold_instr takes a target that resolves to a list of consecutive instructions;
     these instructions are first folded into a sequence with a mark "m",
     then Loop.fold is called on the [cMark "m"]

    !! Loop.fold_instr ?(start:string="0") ?(stop:string) ?(step:string="1")  target

    if start <> ="0" then fail "not yet supported";
  *)
)


(* LATER: Loop.fold will compute automatically the difference between the instructions
   in order to guess what should be the index *)