open Optitrust

let bots = Sys.getenv "OPTITRUST" ^ "/case_studies/bots"

let includes = [
    bots ^ "/common";
    bots ^ "/omp-apac/moldyn"
  ]

let skip = [
    "check";
    "check_symb";
    "check_force"
  ]

let main = "compute"

let () =
  Apac_reset.tnt_blast ();
  Flags.c_parser_includes := includes;
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.omit := ".*_se[qr]$";
  Apac_macros.skip skip;
  Apac_flags.main := main;
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile
