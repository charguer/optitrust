open Optitrust
open Target
open Ast


let _ = Flags.dump_ast_details := true

let _ = Flags.use_light_diff := true

let _ =
  Run.script_cpp  ~parser:Parsers.Clang (fun _ ->
    
    !! Sequence_basic.insert ~reparse:true (stmt "typedef struct { int x; int y; } vect;") [tBefore; cTopFunDef "main"];
    !! Sequence_basic.insert (stmt "typedef vect myvect;") [tAfter; cTypDef "vect"];
    !! Sequence_basic.insert (stmt "int test () {return 0;}") [tAfter; cTypDef "vect"];
)
