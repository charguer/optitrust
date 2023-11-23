open Ast
open Trm

(* [Nobrace]: module for managing nobrace sequences(hidden sequences), these sequence are visible only at the AST level *)

let ids = ref []

let current_id = ref 0

let init () =
  ids := !current_id :: !ids

let enter () =
  current_id := !current_id + 1;
  ids := !current_id :: !ids

let current () =
  match !ids with
  | [] ->  failwith "current:empty list"
  | id :: _rest -> id

let exit () =
  match !ids with
  | [] -> failwith "exit: empty list"
  | id :: rest ->
      ids := rest;
      id

(** [trm_add_style]: adds [No_braces] style to term [t] with the current id. *)
let trm_add_style (t : trm) : trm =
  trm_add_cstyle (No_braces (current())) t

(* [trm_seq tl]: generates a nobrace sequence with the current id *)
let trm_seq (tl : trm mlist) : trm =
  trm_add_style (Trm.trm_seq tl)

(* [trm_seq_nomarks tl]: generates a nobrace sequence with the current id *)
let trm_seq_nomarks (tl : trms) : trm =
  trm_seq (Mlist.of_list tl)



(* [get_id t]: gets the id of the sequence annotated as No_braces *)
let get_id (t : trm) : int option =
  let rec aux l = match l with
  | [] -> None
  | hd :: tl ->
  begin match hd with
  | No_braces i -> Some i
  | _ -> aux tl
  end in
  aux t.annot.trm_annot_cstyle

(* [set_if_sequence t]: convert a normal sequence into a hidden sequence *)
let set_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 -> trm_seq tl1
  | _-> t

(* [is_nobrace t]: check if the current sequence is a hidden sequence or not *)
let is_nobrace (t : trm) : bool =
  match t.desc with
  | Trm_seq _ ->
    List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle
  | _ -> false

(* [remove_if_sequence t]: converts a hidden sequence to a normal one *)
let remove_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq _ ->
    if is_nobrace t then trm_filter_cstyle (function No_braces _ -> true | _ -> false) t else t
  | _ -> t

(* [inline_seq_at index ml]: in the case of nested sequence,
    this function can be used to inline the sublist at [index] into the main list.

    If [check_scoping] is set, also checks for variable scope interference.
    [check_scoping] is DEPRECATED, use var ids instead.
  *)
let inline_seq_at ?(check_scoping = fun _ _ -> ()) (index : int) (ml : trm mlist) : trm mlist =
  let lfront, st, lback  = Mlist.get_item_and_its_relatives index ml in
  match st.desc with
  | Trm_seq tl ->
    check_scoping tl lback;
    Mlist.merge (Mlist.merge lfront tl) lback
  | _ -> trm_fail st "Internal.inline_seq_at: expected an ast node which taks a mlist as parameter"

(* [flatten_seq]: flatten inside [tl] the sequences with annotation [No_braces id] *)
let flatten_seq ?(check_scoping = fun _ _ -> ()) (id: int) (tl: trm Mlist.t): trm Mlist.t =
  let indices_list = List.flatten (List.mapi (fun i ti ->
    begin match get_id ti with
    | Some c_i when c_i = id -> [i]
    | _ -> []
    end
  ) (Mlist.to_list tl)) in

  if indices_list <> [] then
    List.fold_left (fun acc x_i -> inline_seq_at ~check_scoping x_i acc) tl (List.rev indices_list)
  else tl
