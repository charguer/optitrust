
open Ast

(* [intro_mcalloc tg] expects the target [tg] pointing to a call to funciton alloc  
      then it will replace this call with a call to MCALLOC
*)
let intro_mcalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mcalloc)

(* [intro_mmalloc tg] expects the target [tg] pointing to a call to funciton malloc  
      then it will replace this call with a call to MMALLOC
*)
let intro_mmalloc : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mmalloc)

(* [intro_mindex dim tg] expects the target [tg] pointing to an array access 
    then it will replace that access to let say index i with an access at
    MINDEX (dim,i)
*)
let intro_mindex (dim : trm) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.intro_mindex dim)

(* [reorder_dims order tg]: expects the target [tg] pointing to a call to ALLOC functions, or MINDEX 
      then it will reorder their args based on [order], where [order] is a list of indices which the 
      current args should follow
*)
let reorder_dims (order : int list) : Target.Transfo.t = 
  Target.apply_on_targets (Matrix_core.reorder_dims order)

(* [insert_alloc_dim new_dim]: expects the target [tg] pointing to call to ALLOC functions, then it will 
      add a new arg at the begining of the list of args in the targetd call
 *)
let insert_alloc_dim (new_dim : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_alloc_dim new_dim)

(* [insert_access_dim new_dim new_index tg] expects the target [tg] pointing to an array access, then it will
    add two new args in the call to MINDEX function inside that array access *)

let insert_access_dim_index (new_dim : trm) (new_index : trm) : Target.Transfo.t =
  Target.apply_on_targets (Matrix_core.insert_access_dim_index new_dim new_index)

(* [local_name ~mark var local_var tg] expects the target pointing to an instruction that contains 
      an occurrence of [var] then it will define a matrix [local_var] whose dimensions will be the same
      as the one of [var]. Then we copy the contents of the matrix [var] into [local_var] and finaly we 
      free up the memory.
 *)
let local_name ?(mark : mark option) ~var:(var : var) ~local_var:(local_var : var) (tg : Target.target) : unit =
  let vardef_trm = Target.get_trm_at [Target.cVarDef var] in
  let var_type = match trm_var_def_inv vardef_trm with
  | Some (_, _, ty, _) -> ty
  | _ -> fail vardef_trm.loc "local_name: make sure the name of the current var is entered correctly" in
  let malloc_trm = Target.get_trm_at [Target.cVarDef var; Target.cFun ~regexp:true "MCALLOC."] in
  let malloc_trms = match Matrix_core.alloc_inv malloc_trm with 
  | Some (dims, sz, _) -> (dims, sz)
  | _ -> fail None "local_name: could not get the dimensions and the size of the matrix" in
  Internal.nobrace_remove_after (fun _ -> 
    Target.apply_on_targets (Matrix_core.local_name mark var local_var malloc_trms var_type ) tg
  )
  


let delocalize ?(init_zero : bool = false) ?(acc_in_place : bool = false) ?(acc : string option) ~dim:(dim : trm)  ~index:(index : string) (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Target.apply_on_targets (Matrix_core.delocalize dim init_zero acc_in_place acc index) tg)