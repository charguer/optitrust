open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "sparselu" in
  Apac_macros.skip ["sparselu_init"; "sparselu_fini"];
  Apac_flags.main := "sparselu";
  Run.apac input output
