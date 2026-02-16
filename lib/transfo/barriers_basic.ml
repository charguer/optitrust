open Prelude
open Target
open Flags
open Barrier_trm

let%transfo remove_loop_around_barrier (tg: target): unit =
  Target.iter (fun p ->
    let barrier_error = "Barriers_basic.remove_loop_around_barrier: expected magic barrier at target"  in
    trm_inv ~error:barrier_error (magic_barrier_inv) (Path.resolve_path p (Trace.ast ()));
    let ind,loop_p = Path.index_in_surrounding_loop p in
    let for_error = "Barriers_basic.remove_loop_around_barrier: expected for loop surrounding barrier"  in
    let _,_,instrs,_ = (trm_inv ~error:for_error (trm_for_inv_instrs) (Path.resolve_path loop_p (Trace.ast ()))) in
    if (ind != 0 || Mlist.length instrs != 1) then
      failwith "Barriers_basic.remove_loop_around_barrier: expected single instruction inside loop";
    Target.apply_at_path (fun loop -> magic_barrier ()) loop_p;
    ()) tg
