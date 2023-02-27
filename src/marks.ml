include Marks_basic

open Target
open Ast

let with_fresh_mark (f : mark -> unit) : unit =
  let m = Mark.next () in
  f m;
  remove m [nbAny; cMark m]