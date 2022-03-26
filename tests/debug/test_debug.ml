open Optitrust
open Target

let _ = Flags.dump_ast_details := true

let _ = Run.script_cpp (fun _ ->

   
   !! Instr.read_last_write ~write:[cWriteVar "N"] [cFor "a"; cReadVar "N"];
   
)
 