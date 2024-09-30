open Optitrust

let () =
  Apac_flags.verbose := true;
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
