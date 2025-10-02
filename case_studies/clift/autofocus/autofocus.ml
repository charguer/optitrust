open Optitrust
open Ast
open Trm
open Trm_unify

type starindex = Star of trm * trm * trm | Index of trm
type group_repr = starindex list * trm
type focus = (group_repr * group_repr) list

let build_focus (from_group : group_repr) (to_group : group_repr) : focus option =
  let stars_from, trm1 = from_group in
  let stars_to, trm2 = to_group in
  if List.length stars_from <> List.length stars_to || not (are_same_trm trm1 trm2) then None
  else
    let folder (state_opt : (starindex list * focus) option) (si_from : starindex) (si_to : starindex) =
      match state_opt with
      | None -> None
      | Some (current_group, acc_focus) -> (
          match si_from, si_to with
          | Index _, Star _ -> None
          | Star _, Star _
          | Index _, Index _ ->
              Some (current_group, acc_focus)
          | Star (_, _, _), Index i2 ->
              let new_group =
                List.map (fun si -> if si = si_from then Index i2 else si) current_group
              in
              Some (new_group, ((current_group, trm1), (new_group, trm2)) :: acc_focus))
    in
    List.fold_left2 folder (Some (stars_from, [])) stars_from stars_to
    |> Option.map snd
