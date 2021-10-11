open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

    show [cInclude "test_include.cpp" ];
    (* Marks.add "stupid_mark" [cInclude "test_include.cpp"]; *)
  
)