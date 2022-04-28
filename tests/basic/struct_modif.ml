open Optitrust
open Target
open Ast

let factor = trm_int 1 
let get t = Arith_core.apply_aux Binop_add factor t
let set t = Arith_core.apply_aux Binop_mul factor t 

let _ = Run.script_cpp (fun _ -> 

   !! Struct_basic.struct_modif ~f_get:get ~f_set:Fun.id [cTypDef "vect"];
)
