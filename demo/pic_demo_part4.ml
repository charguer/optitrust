open Optitrust
open Target



(* ds is a list of the form ["x";"y";"z"] *)
(* let coloring (ds : string list) (tg : target) : unit =
  let bs = List.map (fun s -> "b" ^ s) ds in
  let cs = List.map (fun s -> "c" ^ s) ds in
  List.iter2 (fun d b -> Loop_basic.tile "2" b ((*tg @*) [cFor d])) ds bs *)
  (* TODO: complete the demo to do all the coloring and reordering *)


let _ = Run.script_cpp (fun () ->
   !! Loop_basic.grid_enumerate [("x", "gridSize"); ("y", "gridSize"); ("z", "gridSize")] [cFor "idCell"];
   !! Function.bind_args ["z0"; ""] [sExpr "+ z"];
   (* TODO: Loop_basic.hoist_const [cVarDef "z0"]
        --> give a targetBetween for the destination
        --> if no target given, use  "before" the immediately surrounding loop
    *)

   !! Loop_basic.tile "2" [cFor "x"];
   !! Loop_basic.tile "2" ~index:"by" [cFor "y"];
   !! Loop_basic.tile "2" ~index:"bz" [cFor "z"];
   !! Loop_basic.color "2" ~index:"cx" [cFor "bx"];
   !! Loop_basic.color "2" ~index:"cy" [cFor "by"];
   !! Loop_basic.color "2" ~index:"cz" [cFor "bz"];
   !! Loop.move "x" ~after:"bz";
   !! Loop.move "y" ~after:"x";
   !! Loop.move "cy" ~before:"bx";
   !! Loop.move "cz" ~before:"bx";
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

  (* TODO: show how to write AST-manipulating functions at the user-script level
    Ast.(Trace.apply (fun
      resovle_target .. (fun t ->
          trm_if cond t t
        )) [cIf])
  *)

)


