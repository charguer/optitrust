open Optitrust
open Ast
open Trm
open Trm_unify

type range = trm * trm * trm
type index = trm
type starindex = Star of range * index | Index of index
type group_repr = starindex list * trm
type focus = (group_repr * group_repr) list

let is_focusable star index : bool = true
let are_same_group_repr ((stars1, t1) : group_repr) ((stars2, t2) : group_repr) : bool =
  are_same_trm t1 t2 &&
  List.length stars1 = List.length stars2 &&
  List.for_all2
    (fun s1 s2 ->
      match s1, s2 with
      | Index i1, Index i2 -> are_same_trm i1 i2
      | Star ((a1,b1,c1), i1), Star ((a2,b2,c2), i2) ->
          are_same_trm a1 a2 &&
          are_same_trm b1 b2 &&
          are_same_trm c1 c2 &&
          are_same_trm i1 i2
      | _, _ -> false)
    stars1 stars2

let are_same (result : focus) (expected : focus) : bool =
  List.length result = List.length expected &&
  List.for_all2
    (fun (g1a, g1b) (g2a, g2b) ->
      are_same_group_repr g1a g2a &&
      are_same_group_repr g1b g2b)
    result expected

let build_focus (from_group : group_repr) (to_group : group_repr) : focus option =
  let stars_from, trm1 = from_group in
  let stars_to, trm2 = to_group in
  if List.length stars_from <> List.length stars_to || not (are_same_trm trm1 trm2) then None
  else
    let folder (state_opt : (starindex list * focus) option) (si_from : starindex) (si_to : starindex) =
      match state_opt with
      | None -> None
      | Some (current_group, acc_focus) -> (
          match (si_from, si_to) with
          | Index _, Star _ -> None
          | Star _, Star _
          | Index _, Index _ ->
            (* Add some checks here ? unification ?  *)
              Some (current_group, acc_focus)
          | Star (range, i1), Index i2 ->
              if is_focusable (range,i1) i2 then
                let new_group = List.map (fun si -> if si = si_from then Index i2 else si) current_group in
                Some (new_group, ((current_group, trm1), (new_group, trm2)) :: acc_focus)
              else None)
    in
    List.fold_left2 folder (Some (stars_from, [])) stars_from stars_to |> Option.map snd

let print_trm_string (t : trm) : string =
  let doc = Ast_to_text.print_trm Ast_to_text.default_style t in
  Tools.document_to_string doc

let print_range ((a, b, c) : range) : string =
  Printf.sprintf "(%s, %s, %s)"
    (print_trm_string a)
    (print_trm_string b)
    (print_trm_string c)

let print_starindex (s : starindex) : string =
  match s with
  | Index i ->
      Printf.sprintf "Index(%s)" (print_trm_string i)
  | Star (r, i) ->
      Printf.sprintf "Star(%s, %s)"
        (print_range r)
        (print_trm_string i)

let print_group_repr ((stars, t) : group_repr) : string =
  let stars_str =
    stars
    |> List.map print_starindex
    |> String.concat "; "
  in
  Printf.sprintf "([%s], %s)" stars_str (print_trm_string t)

let print_focus (f : focus) : unit =
  let content =
    f
    |> List.map (fun (g1, g2) ->
         Printf.sprintf "(%s, %s)"
           (print_group_repr g1)
           (print_group_repr g2))
    |> String.concat " ; "
  in
  Printf.printf "[%s]\n" content
;;
let tests = []

let run_tests f =
  List.iter (fun ((x, y), expected) ->
    match f x y with
    | None ->
        Printf.printf "FAIL: result is None for input (%s, %s)\n"
          (print_group_repr x)
          (print_group_repr y)
    | Some result ->
        if are_same result expected then (
          Printf.printf "OK for:\n";
          print_focus expected
        ) else (
          Printf.printf "FAIL for:\n";
          Printf.printf "Result:\n";
          print_focus result;
          Printf.printf "Expected:\n";
          print_focus expected;
          Printf.printf "\n"
        )
  ) tests
;;

let () = run_tests build_focus

