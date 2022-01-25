open Archi
open BinInt
open BinNums
open Bounds
open Coqlib

(** val fe_ofs_arg : coq_Z **)

let fe_ofs_arg =
  if win64
  then Zpos (Coq_xO (Coq_xO (Coq_xO (Coq_xO (Coq_xO Coq_xH)))))
  else Z0

(** val make_env : bounds -> frame_env **)

let make_env b =
  let w =
    if ptr64
    then Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))
    else Zpos (Coq_xO (Coq_xO Coq_xH))
  in
  let olink =
    align
      (Z.add fe_ofs_arg
        (Z.mul (Zpos (Coq_xO (Coq_xO Coq_xH))) b.bound_outgoing)) w
  in
  let ocs = Z.add olink w in
  let ol =
    align (size_callee_save_area b ocs) (Zpos (Coq_xO (Coq_xO (Coq_xO
      Coq_xH))))
  in
  let ostkdata =
    align (Z.add ol (Z.mul (Zpos (Coq_xO (Coq_xO Coq_xH))) b.bound_local))
      (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
  in
  let oretaddr = align (Z.add ostkdata b.bound_stack_data) w in
  let sz = Z.add oretaddr w in
  { fe_size = sz; fe_ofs_link = olink; fe_ofs_retaddr = oretaddr;
  fe_ofs_local = ol; fe_ofs_callee_save = ocs; fe_stack_data = ostkdata;
  fe_used_callee_save = b.used_callee_save }
