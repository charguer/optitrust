open Optitrust
open Target
open Ast

let _ = Flags.pretty_matrix_notation := true

(* TODO: il faut commencer par montrer un échauffement, avec un script de trois étapes
   simples, par exemple : un tile, un swap loop, une fission,
   illustrés parmi des transformations qui seront effectivement faites dans les deux premiers steps
   du vrai script; voici une proposition *)

let _ = Run.script_cpp ~filename:"matmul.cpp" (fun () ->

    (* !! Loop.tile (trm_int 32) ~index:"bi" [cFor "i"]; --> TODO: optionnel, montrer le tiling quand ça tombe pas juste; voir si c'est facile à faire marcher, là ça donne une erreur *)

    !! Loop.tile (trm_int 32) ~index:"bi" ~bound:TileDivides [cFor "i"];

    !! Loop.reorder ~order:["i"; "j"; "bi"] [cFor "bi"];

    !! Loop.hoist [cVarDef "sum"];

    !! Loop.fission_all_instrs [cFor "bi"];
  )
