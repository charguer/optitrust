open Ast
open Tools

(******************************************************************************)
(*                                  Grammar of paths                          *)
(******************************************************************************)

(* A [path] is a "fully explicit path" describing a point in the AST as a list
    of directions through the nodes from that AST. *)
type path = dir list

(* [dir]: direction type *)
and dir =
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

(* [case_dir]: direction to a switch case *)
and case_dir =
  | Case_name of int
  | Case_body

(* [enum_const_dir]: direction to a const enum declaration *)
and enum_const_dir =
  | Enum_const_name
  | Enum_const_val

(* [paths]: target resolutions produces a list of paths(explicit list of directions),
            we let [paths] be a shorthand for such type. *)
type paths = path list


(* [dir_to_string d]: print direction [d]*)
let dir_to_string (d : dir) : string =
  match d with
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

(* [path_to_string dl]: print the path [dl] *)
let path_to_string (dl : path) : string =
  list_to_string (List.map dir_to_string dl)


(* [paths_to_string ~sep dls]: print the list of paths [dls] *)
let paths_to_string ?(sep:string="; ") (dls : paths) : string =
  list_to_string ~sep (List.map path_to_string dls)


(******************************************************************************)
(*                                  Compare path                              *)
(******************************************************************************)

(* [compare_dir d d']: comparison functions for path sorting the order between direction does not matter.
    When one path is the prefix of the other, it must be considered "greater" *)
let compare_dir (d : dir) (d' : dir) : int =
  match d, d' with
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
  | d, d' when d = d' -> 0
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
  let set_of_p1 = Path_set.empty in
  let set_of_p2 = Path_set.empty in
  let set_of_p1 = List.fold_left (fun acc x -> Path_set.add x acc) set_of_p1 p1 in
  let set_of_p2 = List.fold_left (fun acc x -> Path_set.add x acc) set_of_p2 p2 in
  let union_p1_p2 = Path_set.union set_of_p1 set_of_p2 in
  Path_set.elements union_p1_p2

(* [diff p1 p2]: compute the the diff of two resolved paths and remove duplicates *)
let diff (p1 : paths) (p2 : paths) : paths =
  let set_of_p1 = set_of_paths p1 in
  let set_of_p2 = set_of_paths p2 in
  let diff_p1_p2 = Path_set.diff set_of_p1 set_of_p2 in
  Path_set.elements diff_p1_p2

(***********************************************************************************)
(*                               Auxiliary functions                               *)
(***********************************************************************************)

(* [app_to_nth loc l n cont]: apply a continuation to the nth element of [l] if it exists *)
let app_to_nth (loc : location) (l : 'a list) (n : int) (cont : 'a -> 'b) : 'b =
  try
    match List.nth_opt l n with
    | None ->
       fail loc ("Path.app_to_nth: not enough elements (>= " ^ (string_of_int (n + 1)) ^ " expected)")
        (* LATER: report a better error message when using dArg 1 on a function with only 1 argument, for example *)
    | Some a -> cont a
  with
  | Invalid_argument _ ->
     fail loc "Path.app_to_nth: index must be non-negative"

(* [app_to_nth_dflt]: similar to [app_to_nth] except that this function returns an empty list in case there
   is an exception raised*)
let app_to_nth_dflt (loc : location) (l : 'a list) (n : int) (cont : 'a -> 'b list) : 'b list =
  try app_to_nth loc l n cont with | Failure s -> print_info loc "%s\n" s; []



(***********************************************************************************)
(*                                 Apply on path                                   *)
(***********************************************************************************)

(* [apply_on_path transfo t dl]: follow an explicit path to apply a function on the corresponding subterm *)
let apply_on_path (transfo : trm -> trm) (t : trm) (dl : path) : trm =
  let rec aux_on_path_rec (dl : path) (t : trm) : trm =
    match dl with
    | [] -> transfo t
    | d :: dl ->
       let aux t = aux_on_path_rec dl t in
       let newt = begin match d, t.desc with
       | Dir_array_nth n, Trm_array tl ->
          { t with desc = Trm_array (Mlist.update_nth n aux tl)}
       | Dir_seq_nth n, Trm_seq tl ->
          { t with desc = Trm_seq (Mlist.update_nth n aux tl) }
       | Dir_struct_nth n, Trm_record tl ->
          let aux (lb, t1) = (lb, aux t1) in
          { t with desc = Trm_record (Mlist.update_nth n aux tl)}
       | Dir_cond, Trm_if (cond, then_t, else_t) ->
          { t with desc = Trm_if (aux cond, then_t, else_t)}
       | Dir_cond, Trm_while (cond, body) ->
          { t with desc = Trm_while (aux cond, body)}
       | Dir_cond, Trm_do_while (body, cond) ->
          { t with desc = Trm_do_while (body, aux cond)}
       | Dir_cond, Trm_for_c (init, cond, step, body) ->
          { t with desc = Trm_for_c (init, aux cond, step, body)}
       | Dir_cond, Trm_switch (cond, cases) ->
          { t with desc = Trm_switch (aux cond, cases)}
       | Dir_then, Trm_if (cond, then_t, else_t) ->
          { t with desc = Trm_if (cond, aux then_t, else_t)}
       | Dir_else, Trm_if (cond, then_t, else_t) ->
          { t with desc = Trm_if (cond, then_t, aux else_t) }
       | Dir_var_body, Trm_let (vk,tx,body) ->
          let body =
          begin match new_operation_inv body with
          | Some (ty, arg) -> trm_new ty (aux arg)
          | None -> aux body
          end in
          { t with desc = Trm_let (vk, tx, body)}
       | Dir_body, Trm_let (vk, tx, body) ->
          trm_replace (Trm_let (vk, tx, aux body)) t
       | Dir_body, Trm_let_fun (x, tx, txl, body) ->
          { t with desc = Trm_let_fun (x, tx, txl, aux body)}
       | Dir_body, Trm_for (l_range, body) ->
          { t with desc = Trm_for (l_range, aux body) }
       | Dir_body, Trm_for_c (init, cond, step, body) ->
          { t with desc = Trm_for_c (init, cond, step, aux body) }
       | Dir_body, Trm_while (cond, body) ->
          { t with desc = Trm_while (cond, aux body)}
       | Dir_body, Trm_do_while (body, cond) ->
          trm_replace (Trm_do_while (aux body, cond)) t
       | Dir_body, Trm_abort (Ret (Some body)) ->
          { t with desc = Trm_abort (Ret (Some (aux body)))}
       | Dir_for_start, Trm_for ((index, start, direction, stop, step,is_parallel), body) ->
          { t with desc = Trm_for ((index, aux start, direction, stop, step, is_parallel), body)}
       | Dir_for_stop, Trm_for ((index, start, direction, stop, step, is_parallel), body) ->
          { t with desc = Trm_for ((index, start, direction, aux stop, step, is_parallel), body)}
       | Dir_for_step, Trm_for ((index, start, direction, stop, step, is_parallel), body) ->
          { t with desc = Trm_for ((index, start, direction, stop, apply_on_loop_step aux step, is_parallel), body)}
       | Dir_for_c_init, Trm_for_c (init, cond, step, body) ->
          { t with desc = Trm_for_c (aux init, cond, step, body)}
       | Dir_for_c_step, Trm_for_c (init, cond, step, body) ->
          { t with desc = Trm_for_c (init, cond, aux step, body)}
       | Dir_app_fun, Trm_apps (f, tl) ->
          (*
            warning: the type of f may change
            -> print and reparse to have the right type
           *)
          { t with desc = Trm_apps (aux f, tl)}
       | Dir_arg_nth n, Trm_apps (f, tl) ->
          { t with desc = Trm_apps (f, Xlist.update_nth n aux tl)}
       | Dir_arg_nth n, Trm_let_fun (x, tx, txl, body) ->
          let txl' =
            Xlist.update_nth n
              (fun (x1, tx) ->
                let t' = aux (trm_var ~loc:t.loc x1) in
                match t'.desc with
                | Trm_var (_,  x') -> (x'.qvar_var, tx)
                | _ ->
                   fail t.loc ("Path.apply_on_path: transformation must preserve fun arguments")
              )
              txl
          in
          trm_replace (Trm_let_fun (x, tx, txl', body)) t
        | Dir_name , Trm_let (vk,(x,tx),body) ->
          let t' = aux (trm_var ~loc:t.loc x) in
          begin match t'.desc with
          | Trm_var (_, x') -> { t with desc = Trm_let (vk, (x'.qvar_var, tx), body)}
          | _ -> fail t.loc "Path.apply_on_path: transformation must preserve variable names"
          end
       | Dir_name, Trm_let_fun (x, tx, txl, body) ->
          let t' = aux (trm_var ~loc:t.loc ~qvar:x "") in
          begin match t'.desc with
          | Trm_var (_, x') -> { t with desc = Trm_let_fun (x', tx, txl, body)}
          | _ ->
             fail t.loc "Path.apply_on_path: transformation must preserve names(function)"
          end
       | Dir_case (n, cd), Trm_switch (cond, cases) ->
          let updated_cases =
            (Xlist.update_nth n
               (fun (tl, body) ->
                 match cd with
                 | Case_body -> (tl, aux body)
                 | Case_name i ->
                    (Xlist.update_nth i (fun ith_t -> aux ith_t) tl , body)
               )
               cases
            ) in trm_replace (Trm_switch (cond, updated_cases)) t
        | _, _ ->
           let s = dir_to_string d in
           fail t.loc (Printf.sprintf "Path.apply_on_path: direction %s does not match with trm %s" s (AstC_to_c.ast_to_string t))

       end in
        { newt with typ = None; ctx = None }


  in
  aux_on_path_rec dl t

(***********************************************************************************)
(*                           Explicit path resolution                              *)
(***********************************************************************************)

(* [resolve_path_and_ctx dl t]: follow the explicit path and return the corresponding subterm and its context *)
let resolve_path_and_ctx (dl : path) (t : trm) : trm * (trm list) =
  let rec aux_on_path_rec (dl : path) (t : trm) (ctx : trm list) : trm * (trm list) =
    match dl with
    | [] -> (t, List.rev ctx)
    | d :: dl ->
       let aux t ctx = aux_on_path_rec dl t ctx in
       let loc = t.loc in
       begin match d, t.desc with
       | Dir_seq_nth n, Trm_seq tl ->
          let tl = Mlist.to_list tl in
          let decl_before (n : int) (tl : trm list) =
            Xlist.fold_lefti
              (fun i acc (t : trm) ->
                if i >= n
                  then acc
                  else
                    match t.desc with
                    | Trm_let _ -> t :: acc
                    | Trm_let_fun _ -> t :: acc
                    | Trm_typedef _ -> t :: acc
                    | _ -> acc) [] tl
            in
          app_to_nth loc tl n
            (fun nth_t -> aux nth_t ((decl_before n tl)@ctx))
       | Dir_array_nth n, Trm_array tl ->
          app_to_nth loc (Mlist.to_list tl) n (fun nth_t -> aux nth_t ctx)
       | Dir_struct_nth n, Trm_record tl ->
          app_to_nth loc (Xlist.split_pairs_snd (Mlist.to_list tl)) n (fun nth_t -> aux nth_t ctx)
       | Dir_cond, Trm_if (cond, _, _)
         | Dir_cond, Trm_while (cond, _)
         | Dir_cond, Trm_do_while (_, cond)
         | Dir_cond, Trm_switch (cond, _) ->
          aux cond ctx
       | Dir_cond, Trm_for_c (init, cond, _, _) ->
          begin match init.desc with
          | Trm_let _  -> aux cond (init :: ctx)
          | _ -> aux cond ctx
          end
       | Dir_then, Trm_if (_, then_t, _) ->
          aux then_t ctx
       | Dir_else, Trm_if (_, _, else_t) ->
          aux else_t ctx
       | Dir_body, Trm_let_fun (_, _, args, body) ->
          (* do as if fun args were heap allocated *)
          let args_decl =
            List.rev_map
              (fun (x, tx) ->
                trm_let_mut (x, tx) (trm_lit Lit_uninitialized)
              )
              args
          in
          aux body (args_decl@ctx)
       | Dir_body, Trm_for_c (init, _, _, body) ->
          begin match init.desc with
          | Trm_let _ ->
             aux body (init :: ctx)
          | _ -> aux body ctx
          end
       | Dir_body, Trm_for (_, body) ->
          aux body ctx
       | Dir_var_body, Trm_let (_, _, body) ->
          let new_op_arg = new_operation_arg body in
          if is_new_operation body then aux new_op_arg (body :: ctx) else aux body ctx
       | Dir_body, Trm_let (_, _, body)
         | Dir_body, Trm_while (_, body)
         | Dir_body, Trm_do_while (body, _)
         | Dir_body, Trm_abort (Ret (Some body)) ->
          aux body ctx
       | Dir_for_start, Trm_for (l_range, _) ->
          let (_, start, _, _, _, _) = l_range in
          aux start ctx
       | Dir_for_stop, Trm_for (l_range, _) ->
          let (_,  _, _, stop, _, _) = l_range in
          aux stop ctx

       | Dir_for_step, Trm_for (l_range, _) ->
          let (_, _, _, _, step, _) = l_range in
          let step_trm = loop_step_to_trm step in
          aux step_trm ctx
       | Dir_for_c_init, Trm_for_c (init, _, _, _) ->
          aux init ctx
       | Dir_for_c_step, Trm_for_c (init, _, step, _) ->
          begin match init.desc with
          | Trm_let _ ->
             aux step (init :: ctx)
          | _ -> aux step ctx
          end
       | Dir_app_fun, Trm_apps (f, _) -> aux f ctx
       | Dir_arg_nth n, Trm_apps (_, tl) ->
          app_to_nth loc tl n (fun nth_t -> aux nth_t ctx)
       | Dir_arg_nth n, Trm_let_fun (_, _, arg, _) ->
          app_to_nth loc arg n
            (fun (x, _) -> aux (trm_var ~loc x) ctx)
       | Dir_name, Trm_let_fun (x, _, _, _) ->
          aux (trm_var ~loc ~qvar:x "") ctx
       | Dir_name , Trm_let (_,(x,_),_)
         | Dir_name, Trm_goto x ->
          aux (trm_var ~loc x) ctx
       | Dir_name, Trm_typedef td ->
        aux (trm_var ~loc td.typdef_tconstr) ctx
       | Dir_case (n, cd), Trm_switch (_, cases) ->
          app_to_nth loc cases n
            (fun (tl, body) ->
              match cd with
              | Case_body -> aux body ctx
              | Case_name i ->
                 app_to_nth loc tl i (fun ith_t -> aux ith_t ctx)
            )
       | Dir_enum_const (n, ecd), Trm_typedef td ->
          begin match td.typdef_body with
          | Typdef_enum xto_l ->
            app_to_nth loc xto_l n
             (fun (x, t_o) ->
               match ecd with
               | Enum_const_name -> aux (trm_var ~loc x) ctx
               | Enum_const_val ->
                  begin match t_o with
                  | None ->
                     fail loc
                       "Path.resolve_path_and_ctx: no value for enum constant"
                  | Some t ->
                     aux t ctx
                  end
             )
          | _ -> fail loc "Path.resolving_path: direction"
          end

       | _, _ ->
          let s = dir_to_string d in
          let s_t = AstC_to_c.ast_to_string t in
          fail loc (Printf.sprintf "Path.resolve_path_and_ctx: direction  %s does not match with the following term %s" s s_t )
       end
  in
  aux_on_path_rec dl t []

(* [resolve_path dl t]: resolve get the trm that corresponds to path [dl] *)
let resolve_path (dl : path) (t : trm) : trm  =
  fst (resolve_path_and_ctx dl t )

(* [get_trm_at_path dl]: alias for resolve_path *)
let get_trm_at_path (dl : path) (t : trm) : trm =
  resolve_path dl t


(***********************************************************************************)
(*                           Smart constructors for paths                          *)
(***********************************************************************************)

(* [parent]: returns the parent of a path. *)
let parent (p : path) : path =
   match List.rev p with
   | _ :: p' -> List.rev p'
   | _ -> p

(* [to_inner_loop] takes the path to a loop that contains 1 nested loop,
   and returns the path the inner loop *)
let to_inner_loop (p : path) : path =
  p @ [Dir_body; Dir_seq_nth 0]

(* [to_inner_loops] takes the path to a loop that contains N nested loops,
   and returns the path the inner loop *)
let rec to_inner_loop_n (n : int) (p : path) : path =
   if n > 0 then to_inner_loop_n (n - 1) (to_inner_loop p) else p

(* [index_in_surrounding_loop]: takes the path to a term inside a loop,
   and returns the index of that term in the sequence of the loop body,
   as well as the path to the loop itself. *)
   let index_in_surrounding_loop (dl : path) : int * path =
      match List.rev dl with
      | Dir_seq_nth i :: Dir_body :: dl' -> (i, List.rev dl')
      | _ -> fail None "Path.index_in_surrounding_loop: unexpected path"
   
(* [to_outer_loop]: takes the path to a loop surrounded by another loop,
   and returns the path to the outer loop *)
let to_outer_loop (p : path) : path =
   match index_in_surrounding_loop p with
   | (0, p') -> p'
   | _ -> fail None "Path.to_outer_loop: unexpected path"
