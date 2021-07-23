open Optitrust
open Target

let _ = Run.script_cpp ( fun _ ->

      !! Label_basic.remove [cLabel "start"];
      !! Label_basic.remove [cLabel "unit"];
      !! List.iter (fun l -> Label_basic.remove [cLabel l]) ["loop";"cond";"incr_1";"incr_2"];

      !! Tools.failure_expected (fun () ->
            Label_basic.remove [cLabel "foo"];)

      (* LATER:  extend cLabel  with a regexp
        Label_basic.remove [cLabel ~rexexp:true (regexp_or ["loop";"cond";"incr_1";"incr_2"])];
        where regexp_or builds the string "(loop)|(cond)|..."
      *)
)
