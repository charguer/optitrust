open Optitrust
open Run

let _ =
  run
  ( fun _ ->
    set_init_source"inline_seq.cpp";
    (** Does not work correctly *)
    Sequence.inline  1 [cSeq ~args:[cVarDef "y"]] ();

    dump()
  )