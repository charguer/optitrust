open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "sort" in
  Apac_macros.skip ["sort_init"; "sort_verify"];
  Apac_flags.main := "sort";
  Run.apac input output
