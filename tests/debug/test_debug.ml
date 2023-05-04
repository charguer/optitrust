open Optitrust
open Target
open Path
open Ast

(*
let _ = Flags.dump_ast_details := true
let _ = Flags.debug_stringreprs := true
*)

let _ = Run.script_cpp (fun () ->
   !! Function.inline ~delete:true [nbMulti; cFun "test"];

   !! Loop.tile (trm_int 4) ~bound:TileDivides [cLabel "a"; cFor "j"];
   !! Loop.tile (trm_int 6) [cLabel "a"; cFor "i"];
   !! Loop.shift (StopAt (trm_int 32)) [cLabel "a"; cFor "bj"];
   !! Loop.extend_range ~start:ExtendToZero [cLabel "a"; cFor "bj"];

   !! Loop.shift (StopAt (trm_int 32)) [cLabel "b"; cFor "j"];
   !! Loop.extend_range ~start:ExtendToZero [cLabel "b"; cFor "j"];
   !! Loop.fusion [cLabel "b"; cFor "i"];
)
