open Optitrust
open Target
open Ast

let main = cFunDef "main"
let dims = ["X";"Y";"Z"]
let nb_dims = List.length dims
let iter_dims f = List.iter f dims
let map_dims f = List.map f dims
let idims = map_dims (fun d -> "i" ^ d)
let delocalize_double_add = Delocalize_arith (Lit_double 0., Binop_add)

let _ = Run.script_cpp (*~inline:["particle_chunk.h";"particle_chunk_alloc.h";"particle.h"]*) (fun () ->



)




(*
!! Instr.read_last_write [nbMulti; main; cWrite ~lhs:[sExpr "p2.pos"](); dRHS; cRead ~addr:[sExpr "(c->items)[i].pos"]()];
*)