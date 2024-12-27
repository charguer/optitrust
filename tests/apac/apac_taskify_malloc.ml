open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true

let () =
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile;
  Apac_reset.tnt_blast ()
