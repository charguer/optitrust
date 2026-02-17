open Prelude
open Target
open Flags
open Barrier_trm

let remove_loop_around_barrier (tg: target): unit =
  Target.iter (fun p ->
    let barrier_error = "Barriers_basic.remove_loop_around_barrier: expected magic barrier at target"  in
    let barrier = (Path.resolve_path p (Trace.ast ())) in
    trm_inv ~error:barrier_error (magic_barrier_inv) barrier;
    let ind,loop_p = Path.index_in_surrounding_loop p in
    let for_error = "Barriers_basic.remove_loop_around_barrier: expected for loop surrounding barrier"  in
    let _,_,instrs,_ = (trm_inv ~error:for_error (trm_for_inv_instrs) (Path.resolve_path loop_p (Trace.ast ()))) in
    if (ind != 0 || Mlist.length instrs != 1) then
      failwith "Barriers_basic.remove_loop_around_barrier: expected single instruction inside loop";
    Target.apply_at_path (fun loop -> barrier) loop_p;
    ()) tg


(* TODO: support more than kernel teardown sync *)

(* let%transfo convert_magic_sync (tg: target): unit =
  Target.apply_at_target_paths (fun t ->
    match t.desc with
    | Trm_apps ({desc = Trm_var var_magic_sync}, tl, ga, gb) ->
      trm_add_attribute GhostInstr (trm_alter ~desc:(Trm_apps (trm_var ghost_var_kernel_teardown_sync, tl, ga, gb)) t)
    | _ -> failwith "Gpu_basic.convert_magic_sync: expected target to point to a magic sync instruction.") tg *)
