open Optitrust
open Run

(* Works *)
let _ = run_unit_test ( fun _ ->

      Label.remove [cLabel "start"];
      (* List.iter (fun l -> Label.remove [cLabel l]) ["loop";"cond";"incr_1";"incr_2"]; (* TODO: try *) *)

      (* NOT A PRIORITY: *)
      Label.remove_multiple [[cLabel "loop"]; [cLabel "cond"]; [cLabel"incr_1"];[cLabel "incr_2"];[cLabel "stop"]];
      (* LATER:  extend cLabel  with a regexp
        Label.remove [cLabel ~rexexp:true (regexp_or ["loop";"cond";"incr_1";"incr_2"])];
        where regexp_or builds the string "(loop)|(cond)|..."
      *)

   )