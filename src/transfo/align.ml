open Prelude
open Target
include Align_basic

(** [alloc vec_align tg]: expects the target [tg] to point at a call at a OptiTrust MALLOC macro,
    then them will convert it to an aligned one with alignment size [vec_align]. *)
let alloc (vec_align : trm) : target -> unit =
  Target.iter (fun p ->
    let tg_trm = Target.resolve_path p in
    begin match Matrix_trm.alloc_inv tg_trm with
    | Some (dims, sz, zero_init) ->
      if zero_init
        then trm_fail tg_trm "Align.alloc: can't align calloc macros";
      let num_dims = List.length dims in
      let new_fun_name = toplevel_var ("MALLOC_ALIGNED" ^ (string_of_int num_dims)) in
      Function_basic.replace_with_change_args new_fun_name (fun tl ->
        tl @ [vec_align]) (target_of_path p)
    | None -> trm_fail tg_trm "Align.alloc: expected a call to MALLOC function "
    end
  )
