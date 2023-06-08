open Target
open Ast

(* TODO: unit tests + document *)

let%transfo loop_align_stop_extend_start ~(start : trm) ~(stop : trm) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Loop.shift ~simpl (StopAt stop) tg;
  Loop.extend_range ~simpl ~start:(ExtendTo start) tg;
  (* TODO: remove following *)
  Trace.reparse ();
  simpl tg

let%transfo loop_align_stop_extend_start_like ~(orig:target) ?(simpl : Transfo.t = Arith.default_simpl) (tg:target) : unit =
  let t = get_trm_at_exn orig in
  let error = "Stencil.loop_align_stop_extend_start_like: expected simple loop" in
  let ((_index, start, _dir, stop, _step, _par), _body) = trm_inv ~error trm_for_inv t in
  loop_align_stop_extend_start ~start ~stop ~simpl tg

(* TODO: inline ~delete:IfOnlyCall *)
let rec pry_loop_nest (nest_of: int) (simpl : Transfo.t) (p : path) : unit =
  if nest_of > 0 then begin
    let t = Path.resolve_path p (Trace.ast ()) in
    match trm_for_inv t with
    | Some _ -> pry_loop_nest (nest_of - 1) simpl (p @ [Dir_body; Dir_seq_nth 0]) (* (p @ [Dir_body]) *)
    | None ->
      begin match trm_apps_inv t with
      | Some (f, _) ->
        Function.inline ~simpl (target_of_path p);
        pry_loop_nest nest_of simpl p
      | None ->
        (* TODO: sequence with other things inside?
        begin match trm_seq_inv t with
        | Some
        | None -> *) fail t.loc "Stencil.pry_loop_nest: expected nested for loops, potentially hidden by function calls."
      end
  end

(* / fuse_recompute? / fuse_inlined? *)
let%transfo fusion_targets ?(nest_of : int = 1) ~(outputs : var list) ?(simpl : Transfo.t = Arith.default_simpl) (tg : target) : unit =
  Marks.with_fresh_mark (fun to_fuse ->
    Target.iter (fun _ p ->
      pry_loop_nest nest_of simpl p;
      Marks.add to_fuse (target_of_path p)
    ) tg;
    Loop.fusion_targets ~nest_of [cMark to_fuse];
    let writes = [] in (* TODO: collect_array_writes *)
    Matrix.elim [cDiff [[multi cVarDef writes]] [[multi cVarDef outputs]]]
  )