open Ast
open Trm

(** [Nobrace]: module for managing nobrace sequences(hidden sequences), these sequence are visible only at the AST level *)

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

(** [trm_seq tl]: generates a nobrace sequence with the current id *)
let trm_seq (tl : trm mlist) : trm =
  trm_add_style (Trm.trm_seq tl)

(** [trm_seq_nomarks tl]: generates a nobrace sequence with the current id *)
let trm_seq_nomarks (tl : trms) : trm =
  trm_seq (Mlist.of_list tl)



(** [get_id t]: gets the id of the sequence annotated as No_braces *)
let get_id (t : trm) : int option =
  let rec aux l = match l with
  | [] -> None
  | hd :: tl ->
  begin match hd with
  | No_braces i -> Some i
  | _ -> aux tl
  end in
  aux t.annot.trm_annot_cstyle

(** [set_if_sequence t]: convert a normal sequence into a hidden sequence *)
let set_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 -> trm_seq tl1
  | _-> t

(** [is_nobrace t]: check if the current sequence is a hidden sequence or not *)
let is_nobrace (t : trm) : bool =
  List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle

(** [remove_if_sequence t]: converts a hidden sequence to a normal one *)
let remove_if_sequence (t : trm) : trm =
  trm_filter_cstyle (function No_braces _ -> false | _ -> true) t

(** [flatten_seq]: flatten inside [tl] the sequences with annotation [No_braces id] *)
let flatten_seq (id: int) (tl: trm Mlist.t): trm Mlist.t =
  Mlist.concat_map (fun ti ->
    match get_id ti with
    | Some c_i when c_i = id ->
      let tl = trm_inv ~error:"expected an ast node which taks a mlist as parameter" trm_seq_inv ti in
      tl
    | _ -> Mlist.of_list [ti]
  ) tl

(* internal *)
(** [clean_all_seq id t]: remove recursively all the sequences from ast with annotation [No_braces id] *)
let clean_all_seq (id : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let tl = Mlist.map aux tl in
      let tl = flatten_seq id tl in
      trm_replace (Trm_seq tl) t
    | _ -> Trm.trm_map aux t
  in aux t

(** [remove_after_trm_op f t]: computes [f t] and removes in the result the nobrace-sequences that were inserted during that computation. *)
let remove_after_trm_op (f : trm -> trm) (t : trm) : trm =
  enter ();
  let t2 = f t in
  let id = exit () in
  clean_all_seq id t2
