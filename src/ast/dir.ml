
(******************************************************************************)
(*                                  Grammar of paths                          *)
(******************************************************************************)

(* A [path] is a "fully explicit path" describing a point in the AST as a list
    of directions through the nodes from that AST. *)
type path = dir list

(* [dir]: direction type *)
and dir =
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

(* [span]: Represent a set of contiguous positions inside a sequence.
   [start] is the postion before the first element, and [stop] is the postion after the last element. *)
and span = { start: int; stop: int; }

(* [case_dir]: direction to a switch case *)
and case_dir =
  | Case_name of int
  | Case_body

(* [enum_const_dir]: direction to a const enum declaration *)
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

(* [paths]: target resolutions produces a list of paths(explicit list of directions),
            we let [paths] be a shorthand for such type. *)
type paths = path list


(* [dir_to_string d]: print direction [d]*)
let dir_to_string (d : dir) : string =
  match d with
  | Dir_before n -> "Dir_before " ^ (string_of_int n)
  | Dir_span { start; stop } -> "Dir_span " ^ (string_of_int start) ^ " " ^ (string_of_int stop)
  | Dir_array_nth n -> "Dir_array_nth " ^ (string_of_int n)
  | Dir_struct_nth n -> "Dir_struct_nth " ^ (string_of_int n)
  | Dir_seq_nth n-> "Dir_seq_nth " ^ (string_of_int n)
  | Dir_cond -> "Dir_cond"
  | Dir_then -> "Dir_then"
  | Dir_else -> "Dir_else"
  | Dir_body -> "Dir_body"
  | Dir_var_body -> "Dir_var_body"
  | Dir_for_start -> "Dir_for_start"
  | Dir_for_stop -> "Dir_for_stop"
  | Dir_for_step -> "Dir_for_step"
  | Dir_for_c_init -> "Dir_for_c_init"
  | Dir_for_c_step -> "Dir_for_c_step"
  | Dir_app_fun -> "Dir_app_fun"
  | Dir_arg_nth n -> "Dir_arg_nth " ^ (string_of_int n)
  | Dir_name -> "Dir_name"
  | Dir_case (n, cd) ->
      let s_cd =
        match cd with
        | Case_name n -> "Case_name " ^ (string_of_int n)
        | Case_body -> "Case_body"
      in
      "Dir_case (" ^ (string_of_int n) ^ ", " ^ s_cd ^ ")"
  | Dir_enum_const (n, ecd) ->
      let s_ecd =
        match ecd with
        | Enum_const_name -> "Enum_const_name"
        | Enum_const_val -> "Enum_const_val"
      in
      "Dir_enum_const (" ^ (string_of_int n) ^ ", " ^ s_ecd ^ ")"
  | Dir_record_field i ->
    "Dir_record_field " ^ string_of_int i
  | Dir_namespace -> "Dir_namespace"
  | Dir_contract (cdir, rdir, i) -> Printf.sprintf "Dir_contract(%s, %s, %d)"
    (match cdir with
    | Contract_pre -> "Contract_pre"
    | Contract_post -> "Contract_post"
    | Contract_loop_ghosts -> "Contract_loop_ghosts"
    | Contract_parallel_reads -> "Contract_parallel_reads"
    | Contract_invariant -> "Contract_invariant")
    (match rdir with
    | Resource_set_pure -> "Resource_set_pure"
    | Resource_set_linear -> "Resource_set_linear"
    | Resource_set_fun_contracts -> "Resource_set_fun_contracts")
    i
  | Dir_ghost_arg_nth n -> "Dir_ghost_arg_nth " ^ (string_of_int n)

(* [path_to_string dl]: print the path [dl] *)
let path_to_string (dl : path) : string =
  "Dir." ^ Tools.list_to_string (List.map (fun d ->
    let s = dir_to_string d in
    String.sub s 4 ((String.length s) - 4)
  ) dl)


(* [paths_to_string ~sep dls]: print the list of paths [dls] *)
let paths_to_string ?(sep:string="; ") (dls : paths) : string =
  Tools.list_to_string ~sep (List.map path_to_string dls)

(******************************************************************************)
(*                                  Compare path                              *)
(******************************************************************************)

(* [compare_dir d d']: comparison functions for path sorting the order between direction does not matter.
    When one path is the prefix of the other, it must be considered "greater" *)
let compare_dir (d : dir) (d' : dir) : int =
  match d, d' with
  | Dir_before n, Dir_before m -> compare n m
  | Dir_span { start; stop }, Dir_span { start = start'; stop = stop' } ->
    let cmp_indices = compare start start' in
    if cmp_indices = 0 then compare stop stop' else cmp_indices
  | Dir_array_nth n, Dir_array_nth m -> compare n m
  | Dir_seq_nth n, Dir_seq_nth m -> compare n m
  | Dir_struct_nth n, Dir_struct_nth m -> compare n m
  | Dir_arg_nth n, Dir_arg_nth m -> compare n m
  | Dir_case (n, cd), Dir_case (m, cd') ->
      let cn = compare n m in
      if cn <> 0 then cn else
        begin match cd, cd' with
        | Case_name i, Case_name j -> compare i j
        | Case_body, Case_body -> 0
        | Case_name _, _ -> -1
        | _, Case_name _ -> 1
        end
  | Dir_enum_const (n, ecd), Dir_enum_const (m, ecd') ->
      let cn = compare n m in
      if cn <> 0 then cn else
        begin match ecd, ecd' with
        | d, d' when d = d' -> 0
        | Enum_const_name, _ -> -1
        | Enum_const_val, _ -> 1
        end
  | Dir_ghost_arg_nth n, Dir_ghost_arg_nth m -> compare n m
  (* FIXME: There are a lot of missing cases including Dir_contract *)
  | d, d' when d = d' -> 0
  | Dir_before _, _ -> -1
  | _, Dir_before _ -> 1
  | Dir_span _, _ -> -1
  | _, Dir_span _ -> 1
  | Dir_array_nth _, _ -> -1
  | _, Dir_array_nth _ -> 1
  | Dir_struct_nth _, _ -> -1
  | _, Dir_struct_nth _ -> 1
  | Dir_seq_nth _, _ -> -1
  | _, Dir_seq_nth _ -> 1
  | Dir_cond, _ -> -1
  | _, Dir_cond -> 1
  | Dir_then, _ -> -1
  | _, Dir_then -> 1
  | Dir_else, _ -> -1
  | _, Dir_else -> 1
  | Dir_body, _ -> -1
  | _, Dir_body -> 1
  | Dir_var_body, _ -> -1
  | _, Dir_var_body -> 1
  | Dir_for_start, _ -> -1
  | _, Dir_for_start -> 1
  | Dir_for_stop, _ -> -1
  | _, Dir_for_stop -> 1
  | Dir_for_step, _ -> -1
  | _, Dir_for_step -> 1
  | Dir_for_c_init, _ -> -1
  | _, Dir_for_c_init -> 1
  | Dir_for_c_step, _ -> -1
  | _, Dir_for_c_step -> 1
  | Dir_app_fun, _ -> -1
  | _, Dir_app_fun -> 1
  | Dir_arg_nth _, _ -> -1
  | _, Dir_arg_nth _ -> 1
  | Dir_name, _ -> -1
  | _, Dir_name -> 1
  | Dir_case _, _ -> -1
  | _, Dir_case _ -> 1
  | Dir_record_field _, _ -> -1
  | _ , Dir_record_field _ -> 1
  | Dir_namespace, _ -> -1
  | _, Dir_namespace -> 1
  | Dir_contract _, _ -> -1
  | _, Dir_contract _ -> 1
  | Dir_ghost_arg_nth _, _ -> -1
  | _, Dir_ghost_arg_nth _ -> 1

(* [compare_path dl dl']: compare paths [dl] and [dl'] based on function compare_dir *)
let rec compare_path (dl : path) (dl' : path) : int =
  match dl, dl' with
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | d :: dl, d' :: dl' ->
      let cd = compare_dir d d' in
      if cd = 0 then compare_path dl dl' else cd

(* [Path_set]: a set module used for storing paths *)
module Path_set = Set.Make(
  struct
  let compare = compare_path
  type t = path
  end
)

(* [set_of_paths p1]: create a set of paths *)
let set_of_paths (p1 : paths) : Path_set.t =
  let set_of_p1 = Path_set.empty in
  List.fold_left (fun acc x -> Path_set.add x acc) set_of_p1 p1

(* [filter_duplicates p1]: remove all the duplicate paths from p1 *)
let filter_duplicates (ps : paths) : paths =
  let sp = set_of_paths ps in
  Path_set.elements sp

(* [intersect p1 p2] compute the intersection of two resolved paths *)
let intersect (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = set_of_paths p1 in
  let set_of_p2 = set_of_paths p2 in
  let inter_p1_p2 = Path_set.inter set_of_p1 set_of_p2 in
  Path_set.elements inter_p1_p2

(* [union p1 p2]: compute the union of two resolved paths and remove duplicates *)
let union (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = List.fold_left (fun acc x -> Path_set.add x acc) Path_set.empty p1 in
  let set_of_p1_p2 = List.fold_left (fun acc x -> Path_set.add x acc) set_of_p1 p2 in
  Path_set.elements set_of_p1_p2

(* [diff p1 p2]: compute the the diff of two resolved paths and remove duplicates *)
let diff (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = set_of_paths p1 in
  let set_of_p2 = set_of_paths p2 in
  let diff_p1_p2 = Path_set.diff set_of_p1 set_of_p2 in
  Path_set.elements diff_p1_p2
