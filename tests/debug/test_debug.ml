open Optitrust
open Target

let _ = Flags.dump_ast_details := true

(* let _ = Parsers.(selected_cparser := Menhir)*)

let _ = Run.script_cpp (* ~parser:Parsers.Menhir  *) (fun _ ->

    show [cVarDef "v"];
)
