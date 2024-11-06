open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "nqueens" in
  Apac_macros.skip ["verify_queens"];
  Apac_flags.main := "find_queens";
  Run.apac input output
