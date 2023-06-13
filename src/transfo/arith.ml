open Ast
open Path
open Target
include Arith_basic

(* private *)
let find_surrounding_path (p : path) (t : trm) : path =
  let rec aux p =
    let pp = Path.parent p in
    let pp_t = Path.resolve_path pp t in
    (* not pp_t.is_statement *)
    (* is_prim_arith_call pp_t *)
    if not pp_t.is_statement then aux pp else p
  in
  (* can be useful for user to directly target statement
  assert (not (Path.resolve_path p t).is_statement); *)
  aux p

(* [simpl_surrounding_expr] first goes to the outside of the targeted expression,
   then applies [simpl] *)
let%transfo simpl_surrounding_expr ?(indepth : bool = true) (f : (expr -> expr)) (tg : target) : unit =
  let paths_to_simpl = ref Path_set.empty in
  Target.iter (fun t p ->
    paths_to_simpl := Path_set.add (find_surrounding_path p t) !paths_to_simpl;
  ) tg;
  Path_set.iter (fun p ->
    Arith_basic.simpl ~indepth f (target_of_path p);
  ) !paths_to_simpl

(* TODO?
let default_simpl tg = simpl_surrounding_expr (fun x -> compute (gather x)) (nbAny :: tg)
*)
let default_simpl tg = simpl_surrounding_expr gather (nbAny :: tg)