open Ast


(* internal *)
(* [clean_all_seq id t]: remove recursively all the sequences from ast with annotation [No_braces id] *)
let clean_all_seq (id : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let tl = Mlist.map aux tl in
      let tl = Nobrace.flatten_seq id tl in
      Trm.trm_replace (Trm_seq tl) t
    | _ -> Trm.trm_map aux t
  in aux t


(* [remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence.
  Internally it applies the function clean_nobrace over the trm t.
  By default, scope is checked: inlining nobrace sequences should not change variable usage. *)
let remove_after ?(check_scoping = true) ?(remove : bool = true) (f : unit -> unit) : unit =
  Nobrace.enter ();
  f ();
  let id = Nobrace.exit () in
  if remove then begin
    Trace.apply (clean_all_seq id);
    if check_scoping then Scope.infer_var_ids ()
  end
