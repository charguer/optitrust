open Optitrust

let bots = Apac_macros.cwd () ^ "/case_studies/bots"

let includes = [
    bots ^ "/common";
    bots ^ "/omp-apac/uts"
  ]

let skip = [
    "uts_read_file";
    "uts_show_stats";
    "uts_check_result"
  ]

let main = "parallel_uts"

let () =
  Flags.c_parser_includes := includes;
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.omit := ".*_se[qr]$";
  Apac_macros.skip skip;
  Apac_flags.main := main;
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
