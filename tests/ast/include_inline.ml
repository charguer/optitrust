open Optitrust
open Target

let _ = Run.script_cpp ~filename:"include_inline.cpp" ~inline:["test_include.cpp"; "test_header.h"] (fun _ ->

	    show [cFunDef "main"];
	    (* Marks.add "stupid_mark" [cInclude "test_include.cpp"]; *)

)

