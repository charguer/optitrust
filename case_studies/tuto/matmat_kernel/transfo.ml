open Optitrust
open Prelude

let memset_apply_on ~(depth: int) ?(typ:typ option) (t : trm) :trm =
  let for_error = "Matrix_basic.memset_apply: expected for loop"  in
  let (ranges_and_contracts, body) = trm_inv ~error:for_error (trm_fors_inv depth) t in
  let ranges = List.map fst ranges_and_contracts in
  let error = "Matrix_basic.memset_apply: expected exactly one instr in loop body" in
  let instr  = trm_inv ~error trm_seq_single_inv body in
  let error = "Matrix_basic.memset_apply: expected a set operation" in
  let (lhs,rhs) = trm_inv ~error trm_set_inv instr in
  let error = "Matrix_basic.memset_apply: expected an array affectation" in
  let (array,dims,indices) = trm_inv ~error Matrix_trm.access_inv lhs in
  let array_typ =
    match typ with
    | Some ty -> ty
    | None ->
      match array.typ with
      | Some ty -> get_inner_array_type ty
      | None -> trm_fail t "Cannot find array type for memset"
    in
  if not (List.length ranges = List.length dims) then trm_fail t "Matrix_basic.memset_apply: expect nestedness to match array dimensions";
  let check (range,(dim,indice)) : unit =
    let { index; start; direction; stop; step } = range in
    if not (direction = DirUp) then  trm_fail t  "Matrix_basic.memset_apply: expect up direction";
    if not (trm_is_zero start && trm_is_one step) then trm_fail t "Matrix_basic.memset_apply: expect start =0 and step = 1";
    if not (Trm_unify.are_same_trm stop dim) then  trm_fail t "Matrix_basic.memset_apply: expect stop to match matrix dimension";
    in
  List.iter check (List.combine ranges (List.combine dims indices));
  Matrix_core.matrix_set ~typ:array_typ rhs array dims

let memset ?(depth :int option) ?(typ:typ option) (tg:target) : unit =
  apply_at_target_paths (fun t ->
    let depth =
      match depth with
      | Some nest -> nest
      | _ -> trm_fors_depth t
      in
    memset_apply_on ~depth ?typ t) tg

let _ = Run.script_cpp( fun() ->
  !! memset [nbMulti; cFor "i"];
  (** Check the size of the array matches the loop size *)(
 !! Trace.failure_expected (fun _e -> true) (fun _ ->
     memset [cFor "k"];);
!! Trace.reparse ()))
