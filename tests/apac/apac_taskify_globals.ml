open Optitrust

let () =
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile;
  Apac_reset.tnt_blast ()
