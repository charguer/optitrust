open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "fft" in
  Apac_macros.skip ["test_correctness"];
  Apac_flags.main := "fft";
  Run.apac input output
