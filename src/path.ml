open Ast
open Tools

(******************************************************************************)
(*                                  Grammar of paths                          *)
(******************************************************************************)

(* A [path] is a "fully explicit path" describing a point in the AST as a list
    of directions through the nodes from that AST. *)
type path = dir list

and dir =
  (* nth: go to nth element in seq, array, struct *)
  | Dir_nth of int
  (* cond: used for if, loops and switch *)
  | Dir_cond
  (* if *)
  | Dir_then
  | Dir_else
  (*
    body: used for loops, definitions, return, labelled terms or switch case
      -> directions for while loop: cond and body
   *)
  | Dir_body
  (* for *)
  | Dir_for_init
  | Dir_for_step
  (* app *)
  | Dir_app_fun
  (* arg for fun application and declaration *)
  | Dir_arg of int
  (* name of declared var/fun or label *)
  | Dir_name
  (*
    case group in switch
    Dir_case (n, d) = follow d in nth case group
   *)
  | Dir_case of int * case_dir
  (* constant in enum declaration *)
  | Dir_enum_const of int * enum_const_dir

and case_dir =
  | Case_name of int
  | Case_body

and enum_const_dir =
  | Enum_const_name
  | Enum_const_val

(* The resolution of a target produces a list of [path] (explicit list of directions),
   we let [paths] be a shorthand for such type. *)
type paths = path list

let dir_to_string (d : dir) : string =
  match d with
  | Dir_nth n -> "Dir_nth " ^ (string_of_int n)
  | Dir_cond -> "Dir_cond"
  | Dir_then -> "Dir_then"
  | Dir_else -> "Dir_else"
  | Dir_body -> "Dir_body"
  | Dir_for_init -> "Dir_for_init"
  | Dir_for_step -> "Dir_for_step"
  | Dir_app_fun -> "Dir_app_fun"
  | Dir_arg n -> "Dir_arg " ^ (string_of_int n)
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

let path_to_string (dl : path) : string =
  list_to_string (List.map dir_to_string dl)

let paths_to_string ?(sep:string="; ") (dls : paths) : string =
  list_to_string ~sep (List.map path_to_string dls)


(******************************************************************************)
(*                                  Compare path                              *)
(******************************************************************************)
(*
  comparison functions for path sorting
  the order between directions does not matter
  only constraint: when one path is the prefix of the other, it must be
  considered "greater"
 *)
let compare_dir (d : dir) (d' : dir) : int =
  match d, d' with
  | Dir_nth n, Dir_nth m -> compare n m
  | Dir_arg n, Dir_arg m -> compare n m
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
  | Dir_nth _, _ -> -1
  | _, Dir_nth _ -> 1
  | Dir_cond, _ -> -1
  | _, Dir_cond -> 1
  | Dir_then, _ -> -1
  | _, Dir_then -> 1
  | Dir_else, _ -> -1
  | _, Dir_else -> 1
  | Dir_body, _ -> -1
  | _, Dir_body -> 1
  | Dir_for_init, _ -> -1
  | _, Dir_for_init -> 1
  | Dir_for_step, _ -> -1
  | _, Dir_for_step -> 1
  | Dir_app_fun, _ -> -1
  | _, Dir_app_fun -> 1
  | Dir_arg _, _ -> -1
  | _, Dir_arg _ -> 1
  | Dir_name, _ -> -1
  | _, Dir_name -> 1
  | Dir_case _, _ -> -1
  | _, Dir_case _ -> 1

let rec compare_path (dl : path) (dl' : path) : int =
  match dl, dl' with
  | [], [] -> 0
  | [], _ -> 1
  | _, [] -> -1
  | d :: dl, d' :: dl' ->
     let cd = compare_dir d d' in
     if cd = 0 then compare_path dl dl' else cd





(******************************************************************************)
(*                                  Auxiliary functions                       *)
(******************************************************************************)

(* applies a continuation to the nth element of l if it exists *)
let app_to_nth (loc : location) (l : 'a list) (n : int) (cont : 'a -> 'b) : 'b =
  try
    match List.nth_opt l n with
    | None ->
       fail loc
         ("app_to_nth: not enough elements (>= " ^ (string_of_int (n + 1)) ^
            " expected)")
    | Some a -> cont a
  with
  | Invalid_argument _ ->
     fail loc "app_to_nth: index must be non-negative"

let app_to_nth_dflt (loc : location) (l : 'a list) (n : int)
  (cont : 'a -> 'b list) : 'b list =
  try app_to_nth loc l n cont with
  | Failure s ->
     print_info loc "%s\n" s;
     []



(******************************************************************************)
(*                                  Apply on path                             *)
(******************************************************************************)


(* follow an explicit target to apply a function on the corresponding subterm *)
let apply_on_path (transfo : trm -> trm) (t : trm) (dl : path) : trm =
  let rec aux (dl : path) (t : trm) : trm =
    match dl with
    | [] -> transfo t
    | d :: dl ->
       let annot = t.annot in
       let loc = t.loc in
       let is_statement = t.is_statement in
       let add = t.add in
       let typ = t.typ in
       let attributes = t.attributes in
       begin match d, t.desc with
       | Dir_nth n, Trm_seq tl ->
          trm_seq ~annot ~loc ~add ~attributes (Tools.list_update_nth (aux dl) tl n)
       | Dir_nth n, Trm_array tl ->
          trm_array ~annot ~loc ~add ~typ ~attributes (Tools.list_update_nth (aux dl) tl n)
       | Dir_nth n, Trm_struct tl ->
          trm_struct ~annot ~loc ~add ~typ ~attributes(Tools.list_update_nth (aux dl) tl n)
       | Dir_nth _, Trm_val (Val_array _) ->
          fail loc "apply_on_path: val_array should not appear"
       | Dir_nth _, Trm_val (Val_struct _) ->
          fail loc "apply_on_path: val_struct should not appear"
       | Dir_cond, Trm_if (cond, then_t, else_t) ->
          trm_if ~annot ~loc ~add ~attributes (aux dl cond) then_t else_t
       | Dir_cond, Trm_while (cond, body) ->
          trm_while ~annot ~loc ~add ~attributes (aux dl cond) body
       | Dir_cond, Trm_for (init, cond, step, body) ->
          trm_for ~annot ~loc ~add ~attributes init (aux dl cond) step body
       | Dir_cond, Trm_switch (cond, cases) ->
          trm_switch ~annot ~loc ~add ~attributes (aux dl cond) cases
       | Dir_then, Trm_if (cond, then_t, else_t) ->
          trm_if ~annot ~loc ~add ~attributes cond (aux dl then_t) else_t
       | Dir_else, Trm_if (cond, then_t, else_t) ->
          trm_if ~annot ~loc ~add ~attributes cond then_t (aux dl else_t)
        | Dir_body, Trm_let (vk,tx,body) ->
          trm_let ~annot ~loc ~is_statement ~add ~attributes vk tx  (aux dl body)
       | Dir_body, Trm_let_fun (x, tx, txl, body) ->
          trm_let_fun ~annot ~loc ~is_statement ~add ~attributes x tx txl (aux dl body)
       | Dir_body, Trm_for (init, cond, step, body) ->
          trm_for ~annot ~loc ~add ~attributes init cond step (aux dl body)
       | Dir_body, Trm_while (cond, body) ->
          trm_while ~annot ~loc ~add ~attributes cond (aux dl body)
       | Dir_body, Trm_abort (Ret (Some body)) ->
          trm_abort ~annot ~loc ~add ~attributes (Ret (Some (aux dl body)))
       | Dir_body, Trm_labelled (l, body) ->
          trm_labelled ~annot ~loc ~add ~attributes l (aux dl body)
       | Dir_body, Trm_decoration(left, body, right) ->
          trm_decoration ~annot ~loc ~add ~attributes left right (aux dl body)
       | Dir_for_init, Trm_for (init, cond, step, body) ->
          trm_for ~annot ~loc ~add ~attributes (aux dl init) cond step body
       | Dir_for_step, Trm_for (init, cond, step, body) ->
          trm_for ~annot ~loc ~add ~attributes init cond (aux dl step) body
       | Dir_app_fun, Trm_apps (f, tl) ->
          (*
            warning: the type of f may change
            -> print and reparse to have the right type
           *)
          trm_apps ~annot ~loc ~is_statement ~add ~typ ~attributes (aux dl f) tl
       | Dir_arg n, Trm_apps (f, tl) ->
          trm_apps ~annot ~loc ~is_statement ~add ~typ ~attributes f
            (Tools.list_update_nth (aux dl) tl n)
       | Dir_arg n, Trm_let_fun (x, tx, txl, body) ->
          let txl' =
            Tools.list_update_nth
              (fun (x, tx) ->
                let t' = aux dl (trm_var ~loc x) in
                match t'.desc with
                | Trm_var x' -> (x', tx)
                | _ ->
                   fail loc ("apply_on_path: transformation " ^
                               "must preserve fun arguments")
              )
              txl
              n
          in
          trm_let_fun ~annot ~loc ~is_statement ~add ~attributes x tx txl' body
        | Dir_name , Trm_let (vk,(x,tx),body) ->
          let t' = aux dl (trm_var ~loc x) in
          (* DEBUG: Ast_to_text.print_ast  ~only_desc:true stdout t'; *)
          begin match t'.desc with
          | Trm_var x' -> trm_let ~annot ~loc ~is_statement ~add ~attributes  vk (x',tx) body
          (* | Trm_decoration(ls,{desc=Trm_var x';_},rs) ->
              trm_decoration ls rs
              (trm_let ~annot ~loc ~is_statement ~add ~attributes vk (x',tx) body) *)

          | _ -> fail loc ("apply_on_path: transformation " ^ "must preserve names(variable)")
          end
       | Dir_name, Trm_let_fun (x, tx, txl, body) ->
          let t' = aux dl (trm_var ~loc x) in
          begin match t'.desc with
          | Trm_var x' ->
            trm_let_fun ~annot ~loc ~is_statement ~add ~attributes x' tx txl body
          | _ ->
             fail loc ("apply_on_path: transformation " ^
                         "must preserve names(function)")
          end
       | Dir_name, Trm_labelled (l, body) ->
          let t' = aux dl (trm_var ~loc l) in
          begin match t'.desc with
          | Trm_var l' ->
             trm_labelled ~annot ~loc ~add ~attributes l' body
          | _ ->
             fail loc ("apply_on_path: transformation " ^
                         "must preserve names(label)")
          end

       | Dir_case (n, cd), Trm_switch (cond, cases) ->
          trm_switch ~annot ~loc ~add ~attributes cond
            (Tools.list_update_nth
               (fun (tl, body) ->
                 match cd with
                 | Case_body -> (tl, aux dl body)
                 | Case_name i ->
                    (Tools.list_update_nth (fun ith_t -> aux dl ith_t) tl i, body)
               )
               cases
               n
            )
        | _, _ ->
           let s = dir_to_string d in
           fail loc ("apply_on_path: direction " ^ s ^
                       " does not match")
       end

  in
  aux dl t



(******************************************************************************)
(*                         Explicit path resolution                           *)
(******************************************************************************)

(*
  follow the explicit path and return the corresponding subterm and its context
 *)
let resolve_path (dl : path) (t : trm) : trm * (trm list) =
  let rec aux (dl : path) (t : trm) (ctx : trm list) : trm * (trm list) =
    match dl with
    | [] -> (t, List.rev ctx)
    | d :: dl ->
       let loc = t.loc in
       begin match d, t.desc with
       | Dir_nth n, Trm_seq tl ->
          let decl_before (n : int) (tl : trm list) =
            foldi
              (fun i acc (t : trm) ->
                if i >= n then acc
                else
                  match t.desc with
                  | Trm_let _ -> t :: acc
                  | Trm_let_fun _ -> t :: acc
                  | Trm_typedef _ -> t :: acc
                  | _ -> acc
              )
              []
              tl
          in
          app_to_nth loc tl n
            (fun nth_t -> aux dl nth_t ((decl_before n tl) ++ ctx))
       | Dir_nth n, Trm_array tl
         | Dir_nth n, Trm_struct tl ->
          app_to_nth loc tl n (fun nth_t -> aux dl nth_t ctx)
       | Dir_nth n, Trm_val (Val_array vl)
         | Dir_nth n, Trm_val (Val_struct vl) ->
          app_to_nth loc vl n
            (fun v -> aux dl (trm_val ~loc v) ctx)
       | Dir_cond, Trm_if (cond, _, _)
         | Dir_cond, Trm_while (cond, _)
         | Dir_cond, Trm_switch (cond, _) ->
          aux dl cond ctx
       | Dir_cond, Trm_for (init, cond, _, _) ->
          begin match init.desc with
          | Trm_let _  -> aux dl cond (init :: ctx)
          | _ -> aux dl cond ctx
          end
       | Dir_then, Trm_if (_, then_t, _) ->
          aux dl then_t ctx
       | Dir_else, Trm_if (_, _, else_t) ->
          aux dl else_t ctx
       | Dir_body, Trm_let_fun (_, _, args, body) ->
          (* do as if fun args were heap allocated *)
          let args_decl =
            List.rev_map
              (fun (x, tx) ->
                trm_let Var_mutable (x, typ_ptr tx) (trm_lit Lit_uninitialized)
              )
              args
          in
          aux dl body (args_decl ++ ctx)
       | Dir_body, Trm_for (init, _, _, body) ->
          begin match init.desc with
          (* | Trm_seq _ when init.annot = Some Heap_allocated -> *)
          | Trm_let _ ->
             aux dl body (init :: ctx)
          | _ -> aux dl body ctx
          end
       | Dir_body, Trm_let (_,(_,_), body)
         | Dir_body, Trm_while (_, body)
         | Dir_body, Trm_abort (Ret (Some body))
         | Dir_body, Trm_labelled (_, body) ->
          aux dl body ctx
       | Dir_for_init, Trm_for (init, _, _, _) ->
          aux dl init ctx
       | Dir_for_step, Trm_for (init, _, step, _) ->
          begin match init.desc with
          | Trm_let _ ->
          (* | Trm_seq _ when init.annot = Some Heap_allocated -> *)
             aux dl step (init :: ctx)
          | _ -> aux dl step ctx
          end
       | Dir_app_fun, Trm_apps (f, _) -> aux dl f ctx
       | Dir_arg n, Trm_apps (_, tl) ->
          app_to_nth loc tl n (fun nth_t -> aux dl nth_t ctx)
       | Dir_arg n, Trm_let_fun (_, _, arg, _) ->
          app_to_nth loc arg n
            (fun (x, _) -> aux dl (trm_var ~loc x) ctx)
       | Dir_name , Trm_let (_,(x,_),_)
         | Dir_name, Trm_let_fun (x, _, _, _)
         | Dir_name, Trm_labelled (x, _)
         | Dir_name, Trm_goto x ->
          aux dl (trm_var ~loc x) ctx
       | Dir_name, Trm_typedef td ->
        aux dl (trm_var ~loc td.typdef_tconstr) ctx
       | Dir_case (n, cd), Trm_switch (_, cases) ->
          app_to_nth loc cases n
            (fun (tl, body) ->
              match cd with
              | Case_body -> aux dl body ctx
              | Case_name i ->
                 app_to_nth loc tl i (fun ith_t -> aux dl ith_t ctx)
            )
       (* TODO: Uncoment this when enabling enums *)
       (* | Dir_enum_const (n, ecd), Trm_typedef td ->
        let xto_l = begin match
        | T
        end

       (Typedef_enum (_, xto_l)) ->
          app_to_nth loc xto_l n
             (fun (x, t_o) ->
               match ecd with
               | Enum_const_name -> aux dl (trm_var ~loc x) ctx
               | Enum_const_val ->
                  begin match t_o with
                  | None ->
                     fail loc
                       "resolve_path: no value for enum constant"
                  | Some t ->
                     aux dl t ctx
                  end
             ) *)
       | _, _ ->
          let s = dir_to_string d in
          fail loc ("resolve_path: direction " ^ s ^ " does not match")
       end
  in
  aux dl t []

