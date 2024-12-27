open Optitrust

let bots = Apac_macros.cwd () ^ "/case_studies/bots"

let includes = [
    bots ^ "/common";
    bots ^ "/omp-apac/alignment"
  ]

let skip = [
    "init_matrix";
    "pairalign_init";
    "align_init";
    "align_end";
    "align_verify"
  ]

let main = "align"

let () =
  Flags.c_parser_includes := includes;
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.omit := ".*_se[qr]$";
  Apac_macros.skip skip;
  Apac_flags.main := main;
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
