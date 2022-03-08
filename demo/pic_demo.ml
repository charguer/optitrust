open Optitrust
open Target
open Ast


let _ = Run.script_cpp ~parser:Parsers.Menhir (*~inline:["bag.h";"particle_chunk_alloc.h";"particle.h"]*) (fun () ->

  show [cTopFunDef "main"];

)
