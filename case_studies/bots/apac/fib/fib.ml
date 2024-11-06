open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "fib" in
  Apac_flags.main := "fib0";
  Run.apac input output
