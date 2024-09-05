
(******************************************************************************)
(*                                  Grammar of paths                          *)
(******************************************************************************)


(** [dir]: direction type *)
type dir =
  (* [Dir_before] is used by target_between, to aim for a position in a sequence
  TODO:
    Dir_around of int * diraround
    type diraround = Diraround_before | Diraround_after
        OR
    Dir_between of int * between_anchor
    type between_anchor = Anchor_prev | Anchor_next
          *)
  | Dir_before of int
  (* [Dir_span] is used by target_span, to get a sequence of contiguous instruction in a sequence.
     TODO: between_anchor for start and stop *)
  | Dir_span of span
  (* TODO: Introduce type nary_node for choosing between  Struct, Array, Seq. .. and generalize before, span, nth *)
  (* nth: direction to nth element in a struct initialization *)
  | Dir_struct_nth of int
  (* nth: direction to nth element in a array initialization *)
  | Dir_array_nth of int
  (* nth: direction to nth element in sequence *)
  | Dir_seq_nth of int
  (* cond: direction to condition of an if, for_c, while and do while loop, or switch statement *)
  | Dir_cond
  (* then: direction to then branch of an if statement *)
  | Dir_then
  (* else: direction to else branch of an if statement *)
  | Dir_else
  (* body: direction to the body of a function definition, loop, then or else branche, or switch case *)
  | Dir_body
  (* direction to the body of a let binding, i.e. the bound value *)
  | Dir_let_body
  (* var_body: direction to the body of a variable, similar to Dir_body but this one bypasses the encoded new operation  *)
  | Dir_var_body
  (* for start: direction to initialization trm of a simple for loop *)
  | Dir_for_start
  (* for stop: direction to bound trm of a simple loop *)
  | Dir_for_stop
  (* for step: direction to step trm of a simple loop *)
  | Dir_for_step
  (* for_c init: direction to initialization trm of a for_c loop  *)
  | Dir_for_c_init
  (* for_c step: direction to step trm of a for_c loop  *)
  | Dir_for_c_step
  (* app_fun: direction to function call  *)
  | Dir_app_fun
  (* arg: direction to nth function argument, both on calls and declarations *)
  | Dir_arg_nth of int
  (* name: direction to name of declared var/fun or label *)
  | Dir_name
  (* case: direction to case group in switch, Dir_case (n, d) = follow d in nth case group *)
  | Dir_case of int * case_dir
  (* enum_const: direction to constant in enum declaration *)
  | Dir_enum_const of int * enum_const_dir
  (* struct, class methods *)
  | Dir_record_field of int
  (* namespace *)
  | Dir_namespace
  (* contracts inside let_fun, for, for_c *)
  | Dir_contract of contract_dir * resource_set_dir * int
  (* ghost argument in apps *)
  | Dir_ghost_arg_nth of int

(** [span]: Represent a set of contiguous positions inside a sequence.
   [start] is the postion before the first element, and [stop] is the postion after the last element. *)
and span = { start: int; stop: int; }

(** [case_dir]: direction to a switch case *)
and case_dir =
  | Case_name of int
  | Case_body

(** [enum_const_dir]: direction to a const enum declaration *)
and enum_const_dir =
  | Enum_const_name
  | Enum_const_val

and contract_dir =
  | Contract_pre
  | Contract_post
  | Contract_loop_ghosts
  | Contract_parallel_reads
  | Contract_invariant

and resource_set_dir =
  | Resource_set_pure
  | Resource_set_linear
  | Resource_set_fun_contracts

[@@deriving show { with_path = false }, ord]

(** [dir_to_string d]: print direction [d]*)
let dir_to_string = show_dir

(* A [path] is a "fully explicit path" describing a point in the AST as a list
    of directions through the nodes from that AST. *)
type path = dir list
[@@deriving show]

(** [paths]: target resolutions produces a list of paths(explicit list of directions),
            we let [paths] be a shorthand for such type. *)
type paths = path list
[@@deriving show]

(** [path_to_string dl]: print the path [dl] *)
let path_to_string = show_path

(** [paths_to_string ~sep dls]: print the list of paths [dls] *)
let paths_to_string ?(sep:string="; ") (dls : paths) : string =
  Tools.list_to_string ~sep (List.map path_to_string dls)

(** [compare_path dl dl']: compare paths [dl] and [dl'] based on the generated function compare_dir.
  When one path is the prefix of the other, it must be considered "greater"
*)
let rec compare_path (dl : path) (dl' : path) : int =
  match dl, dl' with
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | d :: dl, d' :: dl' ->
      let cd = compare_dir d d' in
      if cd = 0 then compare_path dl dl' else cd

(** [Path_set]: a set module used for storing paths *)
module Path_set = Set.Make(
  struct
  let compare = compare_path
  type t = path
  end
)

(** [set_of_paths p1]: create a set of paths *)
let set_of_paths (p1 : paths) : Path_set.t =
  let set_of_p1 = Path_set.empty in
  List.fold_left (fun acc x -> Path_set.add x acc) set_of_p1 p1

(** [filter_duplicates p1]: remove all the duplicate paths from p1 *)
let filter_duplicates (ps : paths) : paths =
  let sp = set_of_paths ps in
  Path_set.elements sp

(** [intersect p1 p2] compute the intersection of two resolved paths *)
let intersect (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = set_of_paths p1 in
  let set_of_p2 = set_of_paths p2 in
  let inter_p1_p2 = Path_set.inter set_of_p1 set_of_p2 in
  Path_set.elements inter_p1_p2

(** [union p1 p2]: compute the union of two resolved paths and remove duplicates *)
let union (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = List.fold_left (fun acc x -> Path_set.add x acc) Path_set.empty p1 in
  let set_of_p1_p2 = List.fold_left (fun acc x -> Path_set.add x acc) set_of_p1 p2 in
  Path_set.elements set_of_p1_p2

(** [diff p1 p2]: compute the the diff of two resolved paths and remove duplicates *)
let diff (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = set_of_paths p1 in
  let set_of_p2 = set_of_paths p2 in
  let diff_p1_p2 = Path_set.diff set_of_p1 set_of_p2 in
  Path_set.elements diff_p1_p2
