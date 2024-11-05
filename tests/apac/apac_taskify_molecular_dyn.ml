open Optitrust

let () =
  Flags.code_print_width := 80;
  Apac_flags.verbose := false;
  Apac_flags.constify := true;
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
