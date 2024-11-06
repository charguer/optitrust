open Optitrust

let () =
  let input, output = Apac_macros.preset_bots "alignment" in
  Apac_macros.skip ["init_matrix"; "pairalign_init"; "align_init"; "align_end"];
  Apac_flags.main := "align";
  Run.apac input output
