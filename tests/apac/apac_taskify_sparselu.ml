open Optitrust

let () =
  Flags.code_print_width := 1024;
  Apac_flags.constify := true;
  Apac_flags.verbose := false;
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
