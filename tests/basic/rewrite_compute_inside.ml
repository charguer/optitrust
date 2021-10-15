open Optitrust
open Target
let _ = Run.script_cpp (fun _ ->

  !! Rewrite_basic.compute_inside [ cLabel "block" ];
  (*

     apply_on_targets tg (fun proot ->
        let cPrimFunAny =  cPrimPredFun (function (Prim_binop _) -> true | _ -> false in
        apply_on_targets (nbMulti ++ (target_of_path proot) ++ [cPrimFunAny]) (fun p ->
            compute p)
  *)
  !! Rewrite_basic.compute_inside [nbMulti;cPrimPredFun (function (Prim_binop _) -> true | _ -> false) ];
)