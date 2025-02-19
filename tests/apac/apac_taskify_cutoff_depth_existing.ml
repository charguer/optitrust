open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.cutoff_depth := true;
  Apac_flags.omit := ".*_se[qr]$";
  Apac_flags.sequential := "@f_se[qr]$";
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile;
  Apac_reset.tnt_blast ()
