open Optitrust
open Prelude

let _ = Run.script_cpp (fun _ ->
  !! Loop.collapse [cFunDef "consts"; cFor "i"];
  !! Loop.collapse [cFunDef "from_zero"; cFor "i"];
  !! Loop.collapse [cFunDef "incr"; cFor "i"];
)
