open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "floorplan" in
  Apac_macros.skip ["floorplan_init"; "floorplan_end"; "floorplan_verify"];
  Apac_flags.main := "compute_floorplan";
  Run.apac input output
