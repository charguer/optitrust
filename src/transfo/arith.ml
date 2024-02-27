open Prelude
open Path
open Target
include Arith_basic


(* [simpl_surrounding_expr] first goes to the outside of the targeted expression,
   then applies [simpl] *)
let%transfo simpl_surrounding_expr ?(indepth : bool = true) (f : (expr -> expr)) (tg : target) : unit =
  Trace.tag_valid_by_composition ();
  let paths_to_simpl = ref Path_set.empty in
  let t = Trace.ast () in
  Trace.without_resource_computation_between_steps (fun () ->
    Target.iter (fun p ->
      paths_to_simpl := Path_set.add (Path.find_surrounding_expr p t) !paths_to_simpl;
    ) tg;
    Path_set.iter (fun p ->
      Arith_basic.simpl ~indepth f (target_of_path p);
    ) !paths_to_simpl
  )

(* TODO?
let default_simpl tg = simpl_surrounding_expr (fun x -> compute (gather x)) (nbAny :: tg)
*)
let default_simpl tg = simpl_surrounding_expr gather (nbAny :: tg)
