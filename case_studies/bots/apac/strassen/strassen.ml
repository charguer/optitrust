open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "strassen" in
  Apac_macros.skip ["init_matrix"; "compare_matrix"; "alloc_matrix"];
  Apac_flags.main := "strassen_main";
  Run.apac input output
