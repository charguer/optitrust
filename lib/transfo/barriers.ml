open Prelude
open Target
open Flags
open Barrier_trm

include Barriers_basic

let%transfo insert_barrier (tg: target) =
  Sequence_basic.insert ~reparse:false (magic_barrier ()) tg
