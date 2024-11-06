open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "knapsack" in
  Apac_macros.skip ["read_input"];
  Apac_flags.main := "knapsack_main";
  Run.apac input output
