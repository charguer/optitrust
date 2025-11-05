open Optitrust
open Ast
open Trm
open Prelude

let elaborate_on (t:trm) :trm =
  let aux t1 =
  match t1.ctx.elaborate with
  | Some {pre_ghost = tl_pre; post_ghost = tl_post} ->
    Printf.printf "we are here\n";
    trm_seq (Mlist.of_list (tl_pre @[t1] @ tl_post))
  | _ -> t1
  in
  trm_bottom_up aux t

let elaborate (tg:target) : unit =
  Printf.printf "here\n";
  apply_at_target_paths elaborate_on tg

let _ = Flags.check_validity := true; Flags.recompute_resources_between_steps := false; Flags.resource_typing_enabled :=true
let _ = Run.script_cpp (fun _ -> !!();
ShowAt.trm ~style:(Style.internal_ast()) [cFunDef "RO_simple_focus_caller"]
);
