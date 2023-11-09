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

(* [remove_and_exit_on ?remove ?check_scoping t]: apply function clean_nobrace over the trm t.
  By default, scope is checked: inlining nobrace sequences should not change variable usage. *)
let remove_and_exit_on ?(check_scoping = true) ?(remove:bool=true) (t : trm) : trm =
  let id = Nobrace.exit () in
  if remove
    then begin
      let t = clean_all_seq ~check_scoping:false id t in
      if check_scoping then Scope.infer_var_ids t else t
    end else
      t

(* [remove_on_after ?remove ?check_scoping t]: wrapper for creating and deleting a nobrace sequence over trm t.
   Options are the same as [remove_and_exit_on] *)
let remove_on_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> trm) : trm =
  Nobrace.enter ();
  let t = f () in
  remove_and_exit_on ~check_scoping ~remove t

(* [remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence *)
let remove_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> unit) : unit =
  Nobrace.enter ();
  f ();
  Trace.apply (remove_and_exit_on ~check_scoping ~remove)

(* FIXME: unused? *)
(* [clean_nobraces tg]: remove all the hidden sequence starting from target [ŧg] *)
(* let clean_nobraces (new_target_apply : bool): Transfo.t =
  (if new_target_apply then Target.apply else apply_on_targets) (
    apply_on_path (fun t -> clean_nobrace_seq ~all:true (-1) t))
    *)
