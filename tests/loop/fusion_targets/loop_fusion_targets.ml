open Optitrust
open Target


let _ = Run.script_cpp ( fun _ ->
  !! Loop.fusion_targets [multi cFor ["i"; "j"]];
  !! Loop.fusion_targets ~nest_of:2 ~rename:(fun p -> Some (Variable.Rename.AddSuffix (string_of_int (fst (Path.index_in_seq p))))) [nbMulti; cFor "k0"];
)
