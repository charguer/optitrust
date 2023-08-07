open Ast
open Nobrace

(* [inline_seq_at index ml]: in the case of nested sequence,
    this function can be used to inline the sublist at [index] into the main list.

    If [check_scoping], also checks for variable scope interference.
  *)
let inline_seq_at ~(check_scoping : bool) (index : int) (ml : trm mlist) : trm mlist =
  let lfront, st, lback  = Mlist.get_item_and_its_relatives index ml in
  match st.desc with
  | Trm_seq tl ->
    if (check_scoping && !Flags.check_validity) then begin
      (* NOTE: assumes that nobrace elimination is called by a tranformation that introduces nobraces. *)
      (* FIXME: will mark the transformation as valid even if this criteria is not sufficient. *)
      Scope.assert_no_interference ~after_what:"the inlined sequence" ~on_interference:"shadowed" tl lback
    end;
    Mlist.merge (Mlist.merge lfront tl) lback
  | _ -> fail st.loc "Internal.inline_seq_at: expected an ast node which taks a mlist as parameter"


(* [clean_seq ~all id t]: remove all the sequences from ast with annotation No_braces if [all] is set to true
    otherwise remove only those sequence with id [id].

  If [check_scoping = true]: Everytime an inner sequence is inlined into an outer sequence, this checks that there is no scope interefence between the variables bound in the inner sequence and the ones used in the outer sequence continuation.
  This assumes that shadowing is allowed.
  *)
let clean_seq ~(check_scoping : bool) ?(all : bool = false) (id : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let indices_list = List.flatten (List.mapi (fun i t1 ->
        let current_seq_id = get_id t1 in
        begin match current_seq_id with
        | Some c_i when (all || (c_i = id)) -> [i]
        | _ -> []
        end
      ) (Mlist.to_list tl)) in
      let new_tl = Mlist.map aux tl in

      let new_tl =
        if indices_list <> [] then
          List.fold_left (fun acc x_i -> inline_seq_at ~check_scoping x_i acc) tl (List.rev indices_list)
        else new_tl in
      Trm.trm_replace (Trm_seq new_tl) t
    | _ -> Trm.trm_map aux t
  in aux t

(* [remove_and_exit ?remove ()]: apply function clean_no_brace over the curren ast.
  By default, scope is checked: inlining nobrace sequences should not change variable usage. *)
let remove_and_exit ?(check_scoping = true) ?(remove:bool=true) () =
  let id = exit () in
  if remove
    then Trace.apply (fun ast -> clean_seq ~check_scoping id ast)

(* [remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence *)
let remove_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> unit) : unit =
  enter ();
  f ();
  remove_and_exit ~check_scoping ~remove ()

(* FIXME: unused? *)
(* [clean_nobraces tg]: remove all the hidden sequence starting from target [ŧg] *)
(* let clean_nobraces (new_target_apply : bool): Transfo.t =
  (if new_target_apply then Target.apply else apply_on_targets) (
    apply_on_path (fun t -> clean_no_brace_seq ~all:true (-1) t))
    *)