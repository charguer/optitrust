open Ast
open Target
(* TODO: Add docs for all the internal functions *)
(* LATER: reimplement a function change_trm that operations on explicit paths
   and thus does not need to do resolution again. *)
let change_trm ?(change_at : target list = [[]]) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  (* change all occurences of t_before in t' *)
  let rec apply_change (t' : trm) : trm=
    (* necessary because of annotations that may be different *)
    (* Tools.printf "change %s with %s\n" (Ast_to_c.ast_to_string t') (Ast_to_c.ast_to_string t_after); *)

    if Ast_to_c.ast_to_string t' = Ast_to_c.ast_to_string t_before then t_after
    else trm_map apply_change t'
  in
  List.fold_left
    (fun t' tr ->
      let tr = if not (List.mem nbAny tr) 
        then [nbAny] @ tr 
        else tr in
      let epl = resolve_target tr t' in
      match epl with
      | [] ->
         print_info t'.loc "change_trm: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at




(* same as change_trm but for types *)
let change_typ ?(change_at : target list = [[]]) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  (* change all occurences of ty_before in ty *)
  let rec change_typ (ty : typ) : typ =

    if same_types ~match_generated_star:false ty ty_before then ty_after
      else typ_map change_typ ty

    (* if Ast_to_c.typ_to_string ty = Ast_to_c.typ_to_string ty_before then ty_after
    else Ast.typ_map change_typ ty *)
  in
  (* change all occurrences of ty_before in type annotations in t *)
  let rec replace_type_annot (t : trm) : trm =
    let t =
      {t with typ = match t.typ with
                    | None -> None
                    | Some ty' -> Some (change_typ ty')
      }
    in
    trm_map replace_type_annot t
  in
  (* change all occurences of ty_before in t *)
  let apply_change (t : trm) : trm =
    let rec aux (t : trm) : trm =
      (* only match nodes where typs occur *)
      match t.desc with
      | Trm_val (Val_prim (Prim_new ty)) ->
         trm_prim ~annot:t.annot ~loc:t.loc ~add:t.add
           (Prim_new (change_typ ty))
      | Trm_val (Val_prim (Prim_unop (Unop_cast ty))) ->
         trm_unop ~annot:t.annot ~loc:t.loc ~add:t.add
           (Unop_cast (change_typ ty))
      | Trm_let (vk,(y,ty),init) ->
        trm_let ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add vk (y,change_typ ty) (aux init)
      | Trm_let_fun (f, ty, args, body) ->
         trm_let_fun ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
           ~attributes:t.attributes f (change_typ ty)
                     (List.map (fun (y, ty) -> (y, change_typ ty)) args)
                     (aux body)
      | Trm_typedef td ->
       begin match td.typdef_body with
       | Typdef_alias ty ->
         trm_typedef  ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~attributes:t.attributes
          { td with typdef_body = Typdef_alias (change_typ ty)}
       | Typdef_prod (b, s) ->
          let s = List.map (fun (lb, x) -> (lb, change_typ x)) s in
          trm_typedef ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add ~attributes:t.attributes
          { td with typdef_body = Typdef_prod (b, s)}
       | _ -> trm_map aux t
       end

      | _ -> trm_map aux t
    in
    replace_type_annot (aux t)
  in
  List.fold_left
    (fun t' tr ->
      Flags.verbose := false;
      let tr = if not (List.mem nbAny tr) 
        then [nbAny] @ tr 
        else tr in
      let epl = resolve_target tr t' in
      match epl with
      | [] ->
         print_info t'.loc "change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at




(* [isolate_last_dir_in_seq dl]:
    params:
      dl: explicit path to the targeted trm
    return:
      a pair of the explicit path to the outer sequence and the index of the term inside that sequence
*)
let isolate_last_dir_in_seq (dl : path) : path * int =
  match List.rev dl with
  | Dir_seq_nth i :: dl' -> (List.rev dl',i)
  | _ -> fail None "isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)

let get_call_in_surrounding_sequence (dl : path) : path * path * int =
  let rec aux (acc : path) (dl : path) =
    match dl with
    | [] -> fail None "get_call_in_surrounding_sequence: empty path"
    | Dir_seq_nth i :: dl'-> (List.rev dl', acc, i)
    | dir :: dl' -> aux (dir :: acc) dl'
  in
  aux [] (List.rev dl)

let get_trm_in_surrounding_loop(dl : path) : path * int =
    match List.rev dl with
    | Dir_seq_nth i :: Dir_body :: dl' -> (List.rev dl', i)
    | _ -> fail None "get_trm_in_surrounding_loop: empty path"


(* compute a fresh variable (w.r.t. t) based on x *)
let fresh_in (t : trm) (x : var) : var =
  if not (is_used_var_in t x) then x
  else
    begin
      let n = ref 0 in
      while is_used_var_in t (x ^ "_" ^ string_of_int !n) do
        incr n
      done;
      x ^ "_" ^ string_of_int !n
    end

let fresh_args (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_var x -> trm_var ("_" ^ x)
    | _ -> trm_map (aux global_trm) t
  in aux t t

let get_context(ctx : Trace.context) (t : trm) : string =
   ctx.includes ^ Ast_to_c.ast_to_string t

let parse_cstring (context : string) (is_expression : bool) (s : string) (ctx : Trace.context): trm list =
 let context = if context = "" then ctx.includes else context in
 let command_line_args =
  List.map Clang.Command_line.include_directory
    (ctx.directory :: Clang.default_include_directories())
  in
 let ast =
    Clang.Ast.parse_string ~command_line_args
      (Printf.sprintf
         {|
          %s
          void f(void){
            #pragma clang diagnostic ignored "-Wunused-value"
            %s
          }
          |}
         context
         (if is_expression then s ^ ";" else s)
      )
  in

  let t = Clang_to_ast.translate_ast ast in
  match t.desc with
  | Trm_seq [t] ->
     begin match t.desc with
     | Trm_seq tl  ->
        let fun_def = List.hd (List.rev tl) in
        begin match fun_def.desc with
        | Trm_let_fun (_, _, _, fun_body) ->
          begin match fun_body.desc with
          | Trm_seq tl -> tl
          | _ -> fail fun_body.loc "parse_cstring: expcted a sequence of terms"
          end
        | _ -> fail fun_def.loc "parse_cstring: expected a function definition"
        end
     | _ -> fail t.loc "parse_cstring: expected another sequence"
     end
  | _-> fail t.loc "parse_cstring: exptected with only one trm"


(* Get the sat of a C/C++ trm entered as a string *)
let term ?(context : string = "")(ctx : Trace.context) (s : string) : trm =
  let tl = parse_cstring context true s ctx  in
  match tl with
  | [expr] -> expr
  | _ -> fail None "term: expcted a list with only one element"

let get_field_list (td : typedef) : (var * typ) list =
  begin match td.typdef_body with
  | Typdef_prod (_, s) -> List.rev s
  (* | Typdef_prod (_, s) -> List.rev (fst (List.split s)) *)
  | _ -> fail None "get_field_lists: expected a Typedef_prod"
  end


let rec get_typid_from_typ (t : typ) : int =
  match t.typ_desc with 
  | Typ_constr (_, id, _) -> id
  | Typ_const ty -> get_typid_from_typ ty
  | Typ_var (_, id) -> id
  | Typ_ptr {inner_typ = ty;_} -> get_typid_from_typ ty
  | Typ_array (ty, _) -> get_typid_from_typ ty
  | Typ_fun (_, ty) -> get_typid_from_typ ty
  | _ -> -1


let rec get_typid_from_trm (t : trm) : int = 
  match t.desc with 
  | Trm_apps (_,[base]) ->
    begin match t.typ with
    | Some typ ->
      begin match typ.typ_desc with
      | Typ_constr (_,id,_) -> id
      | _ -> get_typid_from_trm base
      end
    | None -> get_typid_from_trm base
    end
  | Trm_struct _ | Trm_var _ ->
    begin match t.typ with 
    | Some typ -> 
      begin match typ.typ_desc with 
      | Typ_constr(_,id,_) -> id
      | _ -> -1
      end
    | None -> -1
    end
  | Trm_let (_,(_,tx),_) ->
    get_typid_from_typ tx
  | _ -> -1
  

let rec toplevel_decl (x : var) (t : trm) : trm option =
  match t.desc with
  | Trm_typedef td when td.typdef_tconstr = x -> Some t
  | Trm_let (_, (y, _),_ ) when y = x -> Some t
  | Trm_let_fun (y, _, _, _) when y = x -> Some t
  | Trm_seq tl ->
    List.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ ->
        let t2 = toplevel_decl x t1 in
        begin match t2 with
        | Some _->  t2
        | _ -> None
        end
    ) None tl
  | _ -> None

let rec get_loop_nest_indices (t : trm) : 'a list =
  match t.desc with
  | Trm_for (index, _, _, _, _, body) ->
    begin match body.desc with
    | Trm_seq [f_loop] ->
      index :: get_loop_nest_indices f_loop

    | _ ->
      (* Ast_to_text.print_ast ~only_desc:true stdout body; *)
      index :: []

    end
  | Trm_for_c (_, _, _, body) ->
    let index = for_loop_index t in
    begin match body.desc with
    | Trm_seq [f_loop] ->
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | _ -> []

let extract_loop (t : trm) : ((trm -> trm) * trm) option =
  match t.desc with
  | Trm_for_c (init, cond, step, body) ->
    Some ((fun b -> trm_for_c init cond step b), body)
  | Trm_for (index, direction, start, stop, step, body) ->
    Some ((fun b -> trm_for index direction start stop step b), body)
  | _ -> fail t.loc "extract_loop: expected a loop"

let get_field_index (field : field) (fields : (var * typ) list) : int =
  let rec aux field fields c = match fields with
    | [] -> failwith "get_field_index: empty list"
    | (f, _) :: tl ->
      if (f = field) then c else aux field tl (c+1)
    in
  aux field fields 0


(* Auxiliary functions for reorder transformation *)

let get_pair x xs = List.fold_left(fun acc (y,ty) -> if y = x then (y,ty) :: acc else acc) [] xs

let get_pairs ys xs = List.fold_left(fun acc y -> (get_pair y xs) :: acc) [] ys

let remove_pair x xs = List.filter (fun (y,_) -> y <> x) xs

let remove_pairs (ys : var list) (xs : (var * typ) list) = List.fold_left (fun acc y -> remove_pair y acc) xs ys


let move_fields_after (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ ) list=
  let fins = List.flatten (get_pairs local_l l )in
  let l = remove_pairs local_l l in
  let rec aux = function
    | [] -> failwith "move_fields_after: empty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then fins @ [hd, ty] @ tl (* local_l @ hd :: acc @ tl *)
        else (hd,ty) :: aux tl
      in
    aux l

let move_fields_before (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ) list =
  let fins = List.flatten (get_pairs local_l l) in
  let l = remove_pairs local_l l in
  let rec aux = function
    | [] -> failwith "move_fields_after: empty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then [hd, ty] @ fins @ tl (* local_l @ hd :: acc @ tl *)
        else (hd,ty) :: aux tl
      in
    aux l

(* Get the index for a given field of struct inside its list of fields *)
let get_pos (x : typvar) (t : trm) : int =
  begin match t.desc with
    | Trm_typedef {typdef_body = Typdef_prod (_, fs); _} ->
        let rec find x lst =
        match lst with
        | [] -> raise (Failure "Not Found")
        | (hd, _) :: tl -> if hd = x then 0 else 1 + find x tl
        in
        find x fs
    | _ -> fail t.loc "get_pos_and_element: expected a struct type"
    end

let get_trm_and_its_relatives (index : int) (trms : trm list) : (trm list * trm * trm list) =
  let lfront, lback = Tools.split_list_at index trms in
  let element, lback = Tools.split_list_at 1 lback in
  let element = match element with 
  | [el] -> el
  | _ -> fail None "get_element_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)
  

let clean_no_brace_seq (id : int) (t : trm) : trm =
  let rec clean_up_in_list (tl : trm list) : trm list =
    match tl with 
    | [] -> []
    | t :: tl ->
      begin match t.desc with
      | Trm_seq tl' ->
        let current_seq_id = get_nobrace_id t in
        if  current_seq_id = id then
          tl' @ (clean_up_in_list tl)
        else t :: (clean_up_in_list tl)
      | _ -> t :: (clean_up_in_list tl)
      end in
  let rec aux (t : trm) : trm =
    match t.desc with 
    | Trm_seq tl ->
      trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (clean_up_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in aux t


let clean_lit_unit_seq (t : trm) : trm =
  let rec clean_up_in_list (tl : trm list) : trm list =
    match tl with 
    | [] -> []
    | t :: tl ->
      begin match t.desc with 
      | Trm_seq tl' ->
        (clean_up_in_list tl) @ (clean_up_in_list tl')
      | Trm_val (Val_lit (Lit_unit)) ->
        clean_up_in_list tl
      | _ -> t :: (clean_up_in_list tl)
      end in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
        (clean_up_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in aux t


let nobrace_remove_and_exit () =
    let id = Nobrace.exit () in
    Trace.apply (fun _ctx ast -> clean_no_brace_seq id ast)

let nobrace_enter () =
  Nobrace.enter()

let set_no_brace_if_sequence (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl1 -> trm_seq_no_brace tl1
  | _-> t


let change_loop_body (loop : trm) (body : trm) : trm = 
  match loop.desc with 
  | Trm_for (index , direction, start, stop, step, _) ->
    trm_for index direction start stop step body
  | Trm_for_c (init, cond, step, _) ->
    trm_for_c init cond step body
  | _-> fail loop.loc "change_loop_body: expected for loop"

let process_for_loops (process : (trm -> trm) -> trm) (t : trm) : trm =
  match t.desc with
  | Trm_for (index , direction, start, stop, step, _) ->
    process (fun t -> trm_for index direction start stop step t)
  | Trm_for_c (init, cond, step, _) ->
    process (fun t -> trm_for_c init cond step t)
  | _ -> fail t.loc "process_for_loops: expected a for loop"

let get_constr_from_target (tg : target) : constr =
  match tg with 
  | [cnst] -> cnst
  | _ -> cChain tg
