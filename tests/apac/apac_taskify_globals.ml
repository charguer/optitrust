open Optitrust

let () =
  Apac_reset.tnt_blast ();
  Run.script_cpp ~check_syntax_at_end:true Apac_main.compile
