open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "uts" in
  Apac_macros.skip ["uts_read_file"; "uts_show_stats"; "uts_check_result"];
  Apac_flags.main := "serial_uts";
  Run.apac input output
