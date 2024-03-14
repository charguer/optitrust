open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp(fun _ ->
  !! Loop_basic.rename_index "i2" [cFunDef "main"; cFor "i"];
  !! Loop_basic.rename_index "j2" [cFunDef "main"; cFor "j"];
  !! Loop_basic.rename_index "foo" [cFunDef "main"; cFor "k"];
  !! Loop_basic.rename_index "bar" [cFunDef "main"; cFor "l"];

  !! Loop_basic.rename_index "j" [cFunDef "contract"; cFor "i"];
)
