open Optitrust
open Target

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.rename_index "i2" [cFor "i"];
  !! Loop_basic.rename_index "j2" [cFor "j"];
  !! Loop_basic.rename_index "foo" [cFor "k"];
  !! Loop_basic.rename_index "bar" [cFor "l"];
)
