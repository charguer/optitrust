open Optitrust

let () =
  Run.script_cpp Apac_main.compile;
  Apac_reset.tnt_blast ()
