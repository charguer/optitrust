open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

      !! Label.remove [cLabel "start"];
      !! Label.remove [cLabel "unit"];
      !! List.iter (fun l -> Label.remove [cLabel l]) ["floop";"cond";"incr_1";"incr_2"];
      (* LATER:  extend cLabel  with a regexp
        Label.remove [cLabel ~rexexp:true (regexp_or ["loop";"cond";"incr_1";"incr_2"])];
        where regexp_or builds the string "(loop)|(cond)|..."
      *)
)
