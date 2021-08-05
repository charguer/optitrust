open Optitrust
open Target


let coloring (ds : string list) (tg : target) : unit =
  let bs = List.map (fun s -> "b" ^ s) ds in
  let cs = List.map (fun s -> "c" ^ s) ds in
  List.iter2 (fun d b -> Loop_basic.tile "2" ~index:b (tg @ [cFor d])) ds bs;
  List.iter2 (fun b c -> Loop_basic.color "2" ~index:c (tg @ [cFor b])) bs cs
  

let _ = Run.script_cpp (fun () ->
   !! Loop_basic.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [cFor "idCell"];
   !! Function.bind_args ["z0"; ""] [sExpr "+ z"];
   
   !! coloring ["x";"y";"z"] [cFor "step"];
  
   (*  LATER:
       Loop.reorder_nest ["x"; "y"; "z"; "bx"; "by"; "bz" "cx"; "cy"; "cz" ] [cFor "cx"]
       compute a small number of swaps that achieve the order --like insertion sert
       each step uses Loop.move to take a loop indepth up to the current level
   *)

  (* LATER: nice to show the high-level function
      Trace.switch [
        (fun () ->
            !! coloring ["x";"y";"z"] [cFor "cStep"];);
        (fun () -> ... details)
        ];
  *)

  Ast.(Trace.apply (fun _ t ->
    let tg_path = Target.resolve_target [cIf ] in
    Target.apply_on_path (fun t tg_path ->
      trm_if cond t t
    )))
)

