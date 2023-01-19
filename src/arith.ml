open Ast
open Path
include Arith_basic

(* private *)
let find_surrounding_path (p : path) (t : trm) : path =
  let rec aux p =
    let pp = Path.parent p in
    let pp_t = Path.resolve_path pp t in
    if pp_t.is_statement then p else aux pp
  in
  assert (not (Path.resolve_path p t).is_statement);
  aux p

(* [simpl_surrounding_expr] first goes to outside of the targeted expression,
   then applies [simpl] *)
let simpl_surrounding_expr ?(indepth : bool = false) (f: (expr -> expr)) : Target.Transfo.t =
  Target.iter (fun t p ->
    let p' = find_surrounding_path p t in
    (* Printf.printf "path: %s\n" (Path.path_to_string p'); *)
    Arith_basic.simpl ~indepth f (Target.target_of_path p')
  )