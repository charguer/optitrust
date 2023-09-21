open Syntax
open Target

(** <private>
  [swap_on t]: swaps the order of two nested loops, the targeted loop with the immediate inner loop,
       [t] - ast of the targeted loop. *)
let swap_on (t : trm) : trm =
  match Internal.extract_loop t with
  | Some (loop1, body1) ->
    begin match body1.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let loop2 = Mlist.nth tl 0 in
      begin match Internal.extract_loop loop2 with
      | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
      | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested loop^inside"
      end
    | _ -> begin match Internal.extract_loop body1 with
           | Some (loop2, body2) -> loop2 (trm_seq_nomarks [loop1 body2])
           | None -> fail body1.loc "Loop_core..swap_aux: should target a loop with nested inner loops"
           end
    end
  | None -> fail t.loc "Loop_core.swap_aux: should target a loop"

(** [swap tg]: expects the target [tg] to point at a loop that contains an
   immediately-nested loop. The transformation swaps the two loops. *)
let%transfo swap (tg : target) : unit =
  apply_at_target_paths swap_on tg;
  if !Flags.check_validity then begin
    C_scope.check_var_ids (Trace.ast ());
    (* TODO: check parallelisable as well
       Trace.justif ""; *)
  end

let f = swap
