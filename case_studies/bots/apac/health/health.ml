open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "health" in
  Apac_macros.skip ["my_print"; "read_input_data"; "check_village"];
  Apac_flags.main := "sim_village_main";
  Run.apac input output
