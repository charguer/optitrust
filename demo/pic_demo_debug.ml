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

  (* TODO: ARTHUR: simplify mybij calls in the sum


    mybij(nbCells, nbCorners, indicesOfCorners(idCell2).val[k], k)
  with
    MINDEX2(nbCells, nbCorners, idCell2, k)
 *)



(*
!! Instr.read_last_write [nbMulti; main; cWrite ~lhs:[sExpr "p2.pos"](); dRHS; cRead ~addr:[sExpr "(c->items)[i].pos"]()];
*)