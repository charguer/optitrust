open Ast



(* internal *)
(* [clean_all_seq id t]: remove recursively all the sequences from ast with annotation [No_braces id]

  [check_scoping] is DEPRECATED, use var ids instead.
  LATER: Simplify clean_all_seq with a new implementation of Mlist
  *)
let clean_all_seq ?(check_scoping : bool = true) (id : int) (t : trm) : trm =
  let check_scoping = if (check_scoping && !Flags.check_validity) then
      Scope.assert_no_interference ~after_what:"the inlined sequence" ~on_interference:"shadowed"
    else (fun _ _ -> ())
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let tl = Mlist.map aux tl in
      let tl = Nobrace.flatten_seq ~check_scoping id tl in
      Trm.trm_replace (Trm_seq tl) t
    | _ -> Trm.trm_map aux t
  in aux t

(* [remove_and_exit ?remove ()]: apply function clean_no_brace over the curren ast.
  By default, scope is checked: inlining nobrace sequences should not change variable usage. *)
let remove_and_exit ?(check_scoping = true) ?(remove:bool=true) () =
  assert (check_scoping = true);
  let id = Nobrace.exit () in
  if remove
    then Trace.apply (fun ast ->
      clean_all_seq ~check_scoping:false id ast |>
      Scope.infer_var_ids)

(* [remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence *)
let remove_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> unit) : unit =
  Nobrace.enter ();
  f ();
  remove_and_exit ~check_scoping ~remove ()

(* FIXME: unused? *)
(* [clean_nobraces tg]: remove all the hidden sequence starting from target [ŧg] *)
(* let clean_nobraces (new_target_apply : bool): Transfo.t =
  (if new_target_apply then Target.apply else apply_on_targets) (
    apply_on_path (fun t -> clean_no_brace_seq ~all:true (-1) t))
    *)
