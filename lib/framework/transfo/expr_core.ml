open Prelude

(** [replace_fun_aux name t]: changes the current function call to another function call where the name has
      been changed to [name],
      [name] - name of the function replacing the targeted one,
      [t] - ast of the function call trm. *)
let replace_fun_on (name : var) (t : trm) : trm =
  let error = "Expr_core.replace_fun: expected a function call" in
  let (f, args) = trm_inv ~error trm_apps_inv t in
  trm_apps ~annot:t.annot ?typ:t.typ (trm_var name) args

(** [view_subterms_on stringreprs ro]: prints the string representations of all the subterms of [t]?
   assumes terms to carry [Annot_stringreprid], using the ids from the table [stringreprs].
   See [Expr_basic.view_subterms] for details on how this is achieved. *)
let view_subterms_on (stringreprs : Ast_to_c.stringreprs) (ro : Constr.rexp option) (t : trm) : trm =
  let sprintf = Printf.sprintf in
  let rec aux (t : trm) : trm =
    let sloc =
      match t.loc with
      | None -> "<noloc>"
      | Some {loc_file = _filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
        (* Format:  is 18(4-7)  or 18(4)-20(7)  for multilines *)
        if start_row = end_row
          then sprintf "%d(%d-%d)" start_row start_column end_column
          else sprintf "%d(%d)-%d(%d)" start_row start_column end_row end_column
      in
    let strm =
      match Trm.get_stringreprid t with
      | Some id ->
        begin match Hashtbl.find_opt stringreprs id with
        | None -> sprintf "<no_stringrepr_for:%d>" id
        | Some d -> Tools.document_to_string ~width:PPrint.infinity d
        end
      | None -> "<no_stringreprid>"
      in
    let styp =
      match t.typ with
      | Some ty -> Ast_to_c.typ_to_string ty
      | None -> "<notyp>"
      in
    let styp_trimmed =
      (* If styp > 15 chars, cut the end *)
      let sz = 15 in
      let s = if String.length styp <= sz then styp else (String.sub strm 0 (sz-3)) ^ "..." in
      s ^ String.make (sz - (String.length s)) ' '
      in
    let strm_trimmed =
      (* If strm > 50 chars, keep only 30 first and 20 last. and make it a single line *)
      let n = String.length strm in
      let s =
        if n <= 50
          then strm
          else (String.sub strm 0 30) ^ " ... " ^ (String.sub strm (n-20) 20)  (* ARTHUR: use a wrapper function *)
        in
      Str.global_replace (Str.regexp "\n") " " s
      in
    let sannot = Tools.document_to_string (Ast_to_text.(print_trm_annot default_style) t) in
    let tkind = Constr.get_trm_kind t in
    let skind = Constr.trm_kind_to_string tkind in
    let sreg =
      match ro with
      | None -> ""
      | Some r ->
        (if r.rexp_trm_kind <> tkind then
          sprintf "-"
        else if Constr.match_regexp_str r strm then
          sprintf "1"
        else
          sprintf "0")
      in
    let spacing1 =
      let nloc = String.length sloc in
      if nloc > 15 then "" else String.make (15 - nloc) ' ' in  (* ARTHUR: use a wrapper function *)
    let spacing2=
      let nkind = String.length skind in
      if nkind > 5 then "" else String.make (5 - nkind) ' ' in (* ARTHUR: use a wrapper function *)
    Tools.debug "%s%s: %s : %s%s : %s : %s : %s" sloc spacing1 sreg skind spacing2 styp_trimmed strm_trimmed sannot;
    trm_map aux t
    in
  aux t
