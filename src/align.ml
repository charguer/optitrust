open Ast
open Target
include Align_basic

(* [alloc vec_align tg] expects the target [tg] to be pointing at a call to OptiTrust MALLOC macros, 
     then it will convert it to an aligned one *)
let alloc (vec_align : trm) : Target.Transfo.t =
  Target.iter_on_targets (fun t p -> 
    let tg_trm = Path.get_trm_at_path p t in 
    begin match Matrix_core.alloc_inv tg_trm with 
    | Some (dims, sz, zero_init) -> 
      if zero_init then fail tg_trm.loc "alloc: can't align a calloc call";
      let num_dims = List.length dims in 
      let new_fun_name = "MALLOC_ALIGNED" ^ (string_of_int num_dims) in 
      Function_basic.replace_with_change_args new_fun_name (fun tl -> tl @ [vec_align]) (target_of_path p)
    | None -> fail tg_trm.loc "alloc: expected a call to MALLOC function "
    end
)
  