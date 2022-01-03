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





  (* Part: duplicate the charge of a corner for each of the threads *)
  !^ Matrix.delocalize "nextChargeCorners" ~into:"nextChargeThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var "nbThreads") ~index:"k" ~acc:"sum" ~ops:delocalize_double_add [cLabel "core"];
  !! Specialize.any "idThread" [nbMulti; main; cAny]; (* TODO: why nbMulti here? *) (* TODO: exploit the ~use argument in delocalize *)


)




(*
!! Instr.read_last_write [nbMulti; main; cWrite ~lhs:[sExpr "p2.pos"](); dRHS; cRead ~addr:[sExpr "(c->items)[i].pos"]()];
*)