open Prelude

(* [may_merge]: if [t] corresponds to two nested ifs, merge them using '&&'.
   *)
let may_merge (t : trm) : trm option =
  match trm_if_inv t with
  | Some (cond1, then1, else1) ->
    if is_trm_unit else1 then
    let then1' = begin match trm_seq_inv then1 with
    | Some instrs when (Mlist.length instrs) = 1 ->
      Mlist.nth instrs 0
    | _ -> then1
    end in
    begin match trm_if_inv then1' with
    | Some (cond2, then2, else2) ->
      if is_trm_unit else2 then
        Some (trm_if (trm_and cond1 cond2) then2 (trm_unit ()))
      else None
    | None -> None
    end
    else None
  | None -> None
