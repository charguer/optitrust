open Optitrust
open Prelude

(* TODO: Cover more cases *)

let _ = Run.script_cpp (fun _ ->

  !! Specialize_basic.fundefs "f1" [None; None] [cTopFunDef "f"];

  !! Specialize_basic.fundefs "f2" [None; Some (lit "3")] [cTopFunDef "f"];

  !! Specialize_basic.fundefs "f3" [Some (lit "3"); None] [cTopFunDef "f"];

  !! Specialize_basic.fundefs "f4" [Some (lit "2"); Some (lit "3")] [cTopFunDef "f"];

)
