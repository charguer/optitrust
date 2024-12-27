open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.cutoff_count_and_depth := true                         

let () =
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile;
  Apac_reset.tnt_blast ()
