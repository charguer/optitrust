open Optitrust
open Prelude

let _ = Flags.check_validity := true
let _ = Flags.recompute_resources_between_steps := true

let _ = Flags.report_exectime := true

(** Attach a mark showing the typing exectime wherever available
    (requires [Flag.report_exectime]). Unit is microseconds *)
let%transfo show_typing_exectime (_u : unit) : unit =
  let total = ref 0 in
  let rec aux ?(name = "") (t:trm) : trm =
    let exectime = t.ctx.ctx_resources_exectime in
    let microsec = int_of_float (1000000. *. exectime) in
    total := !total + microsec;
    let mark = string_of_int microsec in
    let t2 = if exectime <> 0. then begin Tools.info "typing time %s: %s" name mark; trm_add_mark mark t end else t in
    let name = match trm_let_inv t with Some (x, _tx, _init) -> x.name | None -> "" in
    trm_map (aux ~name) t2
    in
  Trace.apply aux;
  Tools.info "=====> typing time total: %d\n" !total

(* Special command to avoid GC during typechecking *)
let _ = let open Gc in set { (get ()) with minor_heap_size = 1073741824 }


let _ = Run.script_cpp (fun () ->
  !! show_typing_exectime();

  !! for i = 0 to 10 do
     Typedef_basic.insert ("T" ^ string_of_int i) (Typedef_alias typ_int) [tBefore; cFunDef "main"];
  done
)

