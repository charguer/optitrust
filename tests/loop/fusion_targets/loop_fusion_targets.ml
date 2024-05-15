open Optitrust
open Target

let _ = Flags.check_validity := true

let _ = Run.script_cpp ( fun _ ->
  !! Loop.fusion_targets [cFunBody "f"; multi cFor ["i"; "j"]];
  !! Loop.fusion_targets [cFunBody "with_deps1"; multi cFor ["i"; "j"]];
  !! Loop.fusion_targets ~into:FuseIntoLast [cFunBody "with_deps2"; multi cFor ["i"; "j"]];
  (* TODO:
  !! Loop.fusion_targets ~nest_of:2 ~rename:(fun p -> Some (Variable.Rename.AddSuffix (string_of_int (fst (Path.index_in_seq p))))) [nbMulti; cFor "k0"]; *)
)
