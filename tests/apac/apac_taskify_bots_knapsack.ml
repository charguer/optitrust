open Optitrust

let bots = Apac_macros.cwd () ^ "/case_studies/bots"

let includes = [
    bots ^ "/common";
    bots ^ "/omp-apac/knapsack"
  ]

let skip = [
    "read_input";
    "knapsack_check"
  ]

let main = "knapsack_main_par"

let () =
  Flags.c_parser_includes := includes;
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.omit := ".*_se[qr]$";
  Apac_macros.skip skip;
  Apac_flags.main := main;
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile;
  Apac_reset.tnt_blast ()
