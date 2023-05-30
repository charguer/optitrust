open Ast
open Target

(* [same_kind t1 t2]: check if two ast nodes are of the same kind or not *)
let same_kind (t1 : trm) (t2 : trm) : bool =
  match t1.desc, t2 .desc with
  | Trm_val _, Trm_val _ -> true
  | Trm_var _, Trm_var _ -> true
  | Trm_var _, Trm_apps _  when is_get_operation t2 -> true
  | Trm_array _, Trm_array _ ->  true
  | Trm_record _, Trm_record _ -> true
  | Trm_let _, Trm_let _ -> true
  | Trm_let_fun _, Trm_let_fun _ -> true
  | Trm_typedef _, Trm_typedef _ -> true
  | Trm_if _, Trm_if _ -> true
  | Trm_seq _, Trm_seq _ -> true
  | Trm_apps _, Trm_apps _-> true
  | Trm_while _, Trm_while  _ -> true
  | Trm_for _, Trm_for _ -> true
  | Trm_for_c _, Trm_for_c _ -> true
  | Trm_do_while _, Trm_do_while _ -> true
  | Trm_switch _, Trm_switch _ -> true
  | Trm_abort _, Trm_abort _ -> true
  | Trm_goto _, Trm_goto _ -> true
  | Trm_arbitrary _, Trm_arbitrary _ -> true
  | Trm_omp_routine _ , Trm_omp_routine _ -> true
  | Trm_extern _, Trm_extern _ -> true
  | Trm_namespace _, Trm_namespace _ -> true
  | Trm_template _, Trm_template _ -> true
  | _ , _ -> false

(* [same_trm ~ast_decode t1 t2]: check if [t1] and [t2] have the same string representation *)
let same_trm ?(ast_decode:bool=false) (t1 : trm) (t2 : trm) : bool =
  if same_kind t1 t2 then
    AstC_to_c.ast_to_string t1 = AstC_to_c.ast_to_string  t2
   else false

(* [same_val v1 v2]: check if [v1] and [v2] give the same value *)
let same_val (v1 : value) (v2 : value) : bool =
  same_trm (trm_val v1) (trm_val v2)


(* [change_trm ~change_at t_before t_after t]: replace all the occurrences of [t_before] with [t_after]
   If [change_at] is not equal to [[]] then this function is applied only to descendants of the trm corresponding to
   the targets [change_at] *)
let change_trm ?(change_at : target list = [[]]) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  let rec apply_change (t' : trm) : trm=
    if same_trm t' t_before then
      t_after
      else trm_map apply_change t'
      in
  if change_at = [[]] then
    begin
    let res = apply_change t in
    res
    end
  else
    let res = List.fold_left
    (fun t' tr ->
      let tr = if not (List.mem nbAny tr)
        then [nbAny] @ tr
        else tr in
      let epl = resolve_target_with_stringreprs_available tr t' in
      match epl with
      | [] ->
         print_info t'.loc "Internal.change_trm: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at in
    res

(* [change_typ ~change_at ty_before ty_after t]: similar to [change_trm] but for types *)
let change_typ ?(change_at : target list = [[]]) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  (* change all occurences of ty_before in ty *)
  let rec change_typ (ty : typ) : typ =
    if same_types ~match_generated_star:false ty ty_before then
      ty_after
      else
        typ_map change_typ ty
  in
  let rec replace_type_annot (t : trm) : trm =
    let t =
      let typ = match t.typ with
      | None -> None
      | Some ty' -> Some (change_typ ty') in
      trm_alter ?typ t
    in
    trm_map replace_type_annot t
  in
  let apply_change (t : trm) : trm =
    let rec aux (t : trm) : trm =
      match t.desc with
      | Trm_val (Val_prim (Prim_new ty)) ->
         trm_prim ~annot:t.annot ?loc:t.loc
           (Prim_new (change_typ ty))
      | Trm_val (Val_prim (Prim_unop (Unop_cast ty))) ->
         trm_unop ~annot:t.annot ?loc:t.loc
           (Unop_cast (change_typ ty))
      | Trm_let (vk,(y,ty),init) ->
        trm_let ~annot:t.annot ?loc:t.loc vk (y,change_typ ty) (aux init)
      | Trm_let_fun (f, ty, args, body) ->
         trm_let_fun ~annot:t.annot ?loc:t.loc ~qvar:f "" (change_typ ty)
            (List.map (fun (y, ty) -> (y, change_typ ty)) args)
            (aux body)
      | Trm_typedef td ->
        begin match td.typdef_body with
        | Typdef_alias ty ->
          trm_typedef  ~annot:t.annot ?loc:t.loc
           { td with typdef_body = Typdef_alias (change_typ ty)}
        | Typdef_record rf ->
           let rf = List.map (fun (rf1, rf_annot) ->
            match rf1 with
            | Record_field_member (lb, ty) -> (Record_field_member (lb, change_typ ty), rf_annot)
            | Record_field_method t -> (Record_field_method (aux t), rf_annot)
           ) rf in
           trm_typedef ~annot:t.annot ?loc:t.loc { td with typdef_body = Typdef_record rf}
        | _ -> trm_map aux t
        end
       | Trm_var (_, x) ->
          let ty = begin match t.typ with
                   | Some ty -> ty
                   | None -> fail t.loc "Internal.apply_change: all variable occurrences should have a type"
                   end in
        trm_var ~annot:t.annot ?loc:t.loc ~typ:(change_typ ty) ~qvar:x ""
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
      let epl = resolve_target_with_stringreprs_available tr t' in
      match epl with
      | [] ->
         print_info t'.loc "Internal.change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at


(* [isolate_last_dir_in_seq dl]: for a trm with path [dl] return the path to the surrouding sequence of the
    instruction that contains that trm, and the index of that instruction on that sequence *)
let isolate_last_dir_in_seq (dl : path) : path * int =
    match List.rev dl with
    | Dir_seq_nth i :: dl' -> (List.rev dl',i)
    | Dir_record_field _ :: Dir_seq_nth i :: dl'  -> (List.rev dl', i)
      (* Printf.printf "Path: %s\n" (Path.path_to_string dl); *)
    | _ ->
      fail None "Internal.isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)

(* [get_instruction_in_surrounding_sequence dl]: for a trm with path [dl] return the path to the surrouding sequence
     of the instruction that contains that trm, the path from that instruction to the trm itself and the index of that
     instruction on that sequence. *)
let get_instruction_in_surrounding_sequence (dl : path) : path * path * int =
  let rec aux (acc : path) (dl : path) =
    match dl with
    | [] -> fail None "Internal.get_instruction_in_surrounding_sequence: empty path"
    | Dir_seq_nth i :: dl'-> (List.rev dl', acc, i)
    | dir :: dl' -> aux (dir :: acc) dl'
  in aux [] (List.rev dl)

(* [get_ascendant_path checker dl t]: for a trm with path [dl] return the path to the first ascendant that satisfies
    that satisfies the predicate [checker]. *)
let get_ascendant_path (checker : trm -> bool) (dl : path) (t : trm) : path =
  let rec aux (dl1 : path) : path =
    match dl1 with
    | [] -> []
    | hd_p :: tl_p ->
      let res = Path.resolve_path (List.rev dl1) t in
        if checker res then (List.rev dl1) else aux tl_p
    in
  aux (List.rev dl)

(* [get_surrouding_access dl t]: specialization of get_ascendant_path for accesses*)
let get_ascendant_access_path (dl : path) (t : trm) : path =
  get_ascendant_path is_access dl t

(* [get_ascendant_read_path dl t] specialization of get_ascendant_path for read operations*)
let get_ascendant_read_path (dl : path) (t : trm) : path =
  get_ascendant_path is_get_operation dl t

(* [get_ascendant_write_path dl t] specialization of get_ascendant_path for write operations*)
let get_ascendant_write_path (dl : path) (t : trm) : path =
  get_ascendant_path is_set_operation dl t

(* [get_ascendant_topfun_path dl]: returns the path to the toplevel function that contains
     the trm where the path [dl] points to. *)
let get_ascendant_topfun_path (dl : path) : path option =
  match dl with
  | Dir_seq_nth i :: Dir_body :: _ -> Some [Dir_seq_nth i]
  | _ -> None

(* [is_decl_body dl]: check if [dl] points to a declaration body *)
let is_decl_body (dl : path) : bool =
  match List.rev dl with
  | Dir_body :: _ -> true
  | _ -> false

(* [fresh_args t]: rename all the occurrences of a variable by adding an underscore as prefix *)
let fresh_args (t : trm) : trm =
  match t.desc with
  | Trm_var (kind, x) -> trm_var ~kind ("_" ^ x.qvar_var)
  | _ -> t

(* [get_field_list td]: in the case of typedef struct give back the list of struct fields *)
let get_field_list (td : typedef) : (var * typ) list =
  match td.typdef_body with
  | Typdef_record rfl ->
    List.map (fun (rf, _) ->
      match rf with
      | Record_field_member (lb, ty) -> (lb, ty)
      | _ -> fail None "Internal.get_field_list: expected a struct without methods"
    ) rfl
  | _ -> fail None "Internal.get_field_list: expected a Typedef_prod"


(* [get_typid_from_typ t]: check if typ is a constructed type or a composed type
    In case it is constructed type then return its id.
    In case it is a composed type go in depth and check if it contains a constructed type and return its id.
    Otherwise return -1 meaning that the type [t] is not a constructed type. *)
let rec get_typid_from_typ (t : typ) : int =
  match t.typ_desc with
  | Typ_constr (_, id, _) -> id
  | Typ_const ty -> get_typid_from_typ ty
  | Typ_var (_, id) -> id
  | Typ_ptr {inner_typ = ty;_} -> get_typid_from_typ ty
  | Typ_array (ty, _) -> get_typid_from_typ ty
  | Typ_fun (_, ty) -> get_typid_from_typ ty
  | _ -> -1

(* [get_typid_from_trm ~first_martch t]: for trm [t] check if its type is a constructed type.
   If that's the case then return its id, otherwise return -1, meaning that trm [t] has a different typ. *)
let rec get_typid_from_trm ?(first_match : bool = true) (t : trm) : int =
  match t.desc with
  | Trm_apps (_,[base]) ->
    begin match t.typ with
    | Some typ ->
      begin match typ.typ_desc with
      | Typ_constr (_,id,_) -> id
      | _ -> if first_match then -1 else get_typid_from_trm base
      end
    | None -> get_typid_from_trm base
    end
  | Trm_record _ ->
    begin match t.typ with
    | Some typ ->
      begin match typ.typ_desc with
      | Typ_constr(_,id,_) -> id
      | _ -> -1
      end
    | None -> -1
    end
  | Trm_let (_,(_,tx),_) ->
    get_typid_from_typ (get_inner_ptr_type tx)
  | Trm_var _ ->
      begin match t.typ with
      | Some ty ->  get_typid_from_typ ty
      | _ -> -1
      end
  | _ -> -1


(* [toplevel_decl ~require_body x]: finds the toplevel declaration of variable x, x may be a function, variable, typedef or a class method.
      If [require_body] is set to true, then only definitions are considered.*)
let toplevel_decl ?(require_body:bool=false) (x : var) : trm option =
  let full_ast = Target.get_ast () in
  let rec aux(t1 : trm) : trm option =
    match t1.desc with
    | Trm_typedef td ->
        if td.typdef_tconstr = x
          then Some t1
          else begin match td.typdef_body with
               | Typdef_record rfs ->
                 List.fold_left (fun acc (rf, _) ->
                  begin match acc with
                  | Some _ -> acc
                  | _ ->
                    begin match rf with
                    | Record_field_method t2 ->
                      aux t2
                    | _ -> None
                    end
                  end) None rfs
               | _ -> None
               end
    | Trm_let (_, (y, _),_ ) when y = x -> Some t1
    | Trm_let_fun (y, _, _, body) when (is_qvar_var y x) ->
      if require_body then begin
        match body.desc with
        | Trm_seq _ -> Some t1 (* LATER: we might want to test insted if body.desc <> trm_uninitialized or something like that *)
        | _ -> None
      end else begin
        Some t1
      end
    | _ -> None
   in
  match full_ast.desc with
  | Trm_seq tl ->
    Mlist.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ -> aux t1
  ) None tl
  | _ -> fail full_ast.loc "Internal.top_level_decl: the full ast starts with the main sequence which contains all the toplevel declarations"


(* [local_decl x t]: check if [t] is a declaration with name [x], if that's the case the return that declaration *)
let rec local_decl (x : var) (t : trm) : trm option =
  match t.desc with
  | Trm_typedef td when td.typdef_tconstr = x -> Some t
  | Trm_let (_, (y, _),_ ) when y = x -> Some t
  | Trm_let_fun (y, _, _, body) ->
    if (is_qvar_var y x) then Some t else local_decl x body
  | Trm_seq tl ->
    Mlist.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ ->
        let t2 = local_decl x t1 in
        begin match t2 with
        | Some _->  t2
        | _ -> None
        end
    ) None tl
  | _ -> None

(* LATER: Use trm_fors_inv instead *)
(* [get_loop_nest_indices t]: if node [t] represents a loop nest then go through all of them an return an
    ordered list of their indices where the order is the depth order *)
let rec get_loop_nest_indices (t : trm) : 'a list =
  match t.desc with
  | Trm_for (l_range, body) ->
    let (index, _, _, _, _, _) = l_range in
    begin match body.desc with
    | Trm_seq tl when Mlist.length tl = 1  ->
      let f_loop = Mlist.nth tl 0 in
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | Trm_for_c (_, _, _, body) ->
    let index = for_loop_index t in
    begin match body.desc with
    | Trm_seq tl when Mlist.length tl = 1 ->
      let f_loop = Mlist.nth tl 0 in
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | _ -> []

(* [extract_loop t]: for a loop [t] return a pair of function and trm. Where the function takes trm b
    and gives a loop with the same components as loop [t] but with body b. And the trm is the body of the loop [t]. *)
let extract_loop (t : trm) : ((trm -> trm) * trm) option =
  match t.desc with
  | Trm_for_c (init, cond, step, body) ->
    Some ((fun b -> trm_for_c init cond step b), body)
  | Trm_for (l_range, body) ->
    Some ((fun b -> trm_for l_range b), body)
  | _ ->
    fail t.loc "Internal.extract_loop: expected a loop"

(* [get_field_index field fields]: for a struct field with name [field] and [fields] being the list of fields of the
    same struct, return back the index of field [field] in the list of fields [fields]. *)
let get_field_index (field : field) (fields : record_fields) : int =
  let rec aux field fields c = match fields with
    | [] -> failwith "Internal.get_field_index: empty list"
    | (rf, _) :: tl ->
      begin match rf with
      | Record_field_member (f, _) ->
        if f = field then c else aux field tl (c + 1)
      | _ -> aux field tl (c+1)
      end
    in
  aux field fields 0


(* [apply_on_record_fields app_fun rfs]: applies [app_fun] on all the elements of [rfs]. *)
let apply_on_record_fields (app_fun : record_field -> record_field ) (rfs : record_fields) : record_fields =
  List.map (fun (rf, rf_annot) -> (app_fun rf, rf_annot)) rfs


(* [rename_record_fields]: renames all the fields [rfs] by applying function [rename_fun]. *)
let rename_record_fields (rename_fun : string -> string ) (rfs : record_fields) : record_fields =
  let app_fun (rf : record_field) : record_field =
    match rf with
    | Record_field_member (f, ty) -> Record_field_member (rename_fun f, ty)
    | Record_field_method t ->
      begin match t.desc with
      | Trm_let_fun (fn, ret_ty, args, body) ->
        let new_fn = qvar_update ~var:(rename_fun fn.qvar_var) fn in
        (* let new_fn = {fn with qvar_str = rename_fun fn.qvar_var} in  *)
        let new_t = trm_alter  ~desc:(Trm_let_fun (new_fn, ret_ty, args, body)) t in
        Record_field_method new_t
      | _ -> fail t.loc "Internal.rename_record_fields: record member not supported."
      end
      in
    apply_on_record_fields app_fun rfs

(* [update_record_fields_type typ_update rfs]: updates the type of [rfs] based on [typ_update] function. *)
let update_record_fields_type ?(pattern : string = "")(typ_update : typ -> typ ) (rfs : record_fields) : record_fields =
  let app_fun (rf : record_field) : record_field =
    match rf with
    | Record_field_member (f, ty) ->
      let ty = if Tools.pattern_matches pattern f then typ_update ty else ty in
      Record_field_member (f, ty)
    | Record_field_method t -> fail None "Internal.update_record_fields_type: can't update the type of a method."
      in
    apply_on_record_fields app_fun rfs


(* [get_item_and_its_relatives index trms]: for an  item [t] with index [index] in the mlist its belongs to,
    returns the list of items before [t], [t] itself and the list of items that come after [t]. *)
let get_item_and_its_relatives (index : int) (items : 'a mlist) : ('a mlist * 'a * 'a mlist) =
  let lfront, lback = Mlist.split index items in
  let element, lback = Mlist.split 1 lback in
  let element =
    if Mlist.length element = 1
      then Mlist.nth element 0
      else fail None "Internal.get_item_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)

(* [inline_sublist_at index ml]: in the case of nested sequence, nested initialization lists for arrays and structs,
    this function can be used to inline the sublist at [index] into the main list. *)
let inline_sublist_at (index : int) (ml : trm mlist) : trm mlist =
  let lfront, st, lback  = get_item_and_its_relatives index ml in
  match st.desc with
  | Trm_seq tl -> Mlist.merge (Mlist.merge lfront tl) lback
  | _ -> fail st.loc "Internal.inline_sublist_at: expected an ast node which taks a mlist as parameter"


(* [clean_no_brace_seq ~all id t]: remove all the sequences from ast with annotation No_braces if [all] is set to true
    otherwise remove only those sequence with id [id]. *)
let clean_no_brace_seq ?(all : bool = false) (id : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
      let indices_list = List.flatten (List.mapi (fun i t1 ->
        let current_seq_id = get_nobrace_id t1 in
        begin match current_seq_id with
        | Some c_i when  (all || (c_i = id)) -> [i]
        | _ -> []
        end
      ) (Mlist.to_list tl)) in
      let new_tl = Mlist.map aux tl in

      let new_tl =
        if indices_list <> [] then
          List.fold_left (fun acc x_i -> inline_sublist_at x_i acc) tl (List.rev indices_list)
        else new_tl in
      trm_replace (Trm_seq new_tl) t
    | _ -> trm_map aux t
   in aux t

(* [nobrace_remove_and_exit ?remove ()]: apply function clean_no_brace over the curren ast *)
let nobrace_remove_and_exit ?(remove:bool=true) () =
  let id = Nobrace.exit () in
  if remove
    then Trace.apply (fun ast -> clean_no_brace_seq id ast)



(* [nobrace_enter ()]: generates a new id and assigns it to the hidden sequence that is going to be introduced *)
let nobrace_enter () =
  Nobrace.enter()

(* [set_nobrace_if_sequence t]: convert a normal sequence into a hidden sequence *)
let set_nobrace_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 -> trm_seq_no_brace (Mlist.to_list tl1)
  | _-> t

(* [is_nobrace t]: check if the current sequence is a hidden sequence or not*)
let is_nobrace (t : trm) : bool =
  match t.desc with
  | Trm_seq _ ->
    List.exists (function No_braces _ -> true | _ -> false) t.annot.trm_annot_cstyle
  | _ -> false


(* [remove_nobrace_if_sequence t]: converts a hidden sequence to a normal one *)
let remove_nobrace_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq _ ->
    if is_nobrace t then trm_filter_cstyle (function No_braces _ -> true | _ -> false) t else t
  | _ -> t


(* [change_loop_body loop body]: change the current body of loop [loop] with [body] *)
let change_loop_body (loop : trm) (body : trm) : trm =
  match loop.desc with
  | Trm_for (l_range, _) ->
    trm_for l_range body
  | Trm_for_c (init, cond, step, _) ->
    trm_for_c init cond step body
  | _-> fail loop.loc "Internal.change_loop_body: expected for loop"

(* [is_trm_loop t] check if [t] is a loop or not *)
let is_trm_loop (t : trm) : bool =
  match t.desc with
  | Trm_for _ | Trm_for_c _ -> true
  | _ -> false

(* [is_struct_type t]: check if [t] is type struct or not

    Note: The current infrastructure of Optitrust supports only
      struct declared via typedefs, later we will add support for
      struct types not declared via a typedef. *)
let is_struct_type (t : typ) : bool =
  match t.typ_desc with
  | Typ_constr (_tv, tid, _) ->
    begin match Context.typid_to_typedef tid with
    | Some td ->
      begin match td.typdef_body with
      | Typdef_record _ -> true
      | _ -> false
      end
    | _ -> false
    end
  | Typ_record _ -> false (* LATER: All the transformations that work with typedefs should also work with structs *)
  | _ -> false



(* [get_constr_from_target tg]: get the constraint from a list of constraints(targets) *)
let get_constr_from_target (tg : target) : constr =
  match tg with
  | [cnst] -> cnst
  | _ -> cTarget tg

(* [nobrace_remove_after ~remove f]: wrapper for creating and deleting a nobrace sequence *)
let nobrace_remove_after ?(remove : bool = true) (f : unit -> unit) : unit =
  nobrace_enter();
  f();
  nobrace_remove_and_exit ~remove ()


(* [repalce_type_with x y]: replace the current type of variable [y] to [typ_constr x] *)
let rec replace_type_with (x : typvar) (y : var) (t : trm) : trm =
  match t.desc with
  | Trm_var (_, y') when (is_qvar_var y' y) ->
    trm_var ~annot:t.annot ?loc:t.loc ~typ:(typ_constr x) y
  | _ -> trm_map (replace_type_with x y) t

(* [subst tm t]: find all the occurrences of variables in [t] and check if they belong to map [tm]
    if yes then assign its values otherwise do nothing *)
(* LATER: open question: can this be implemented using onscope? *)
let rec subst (tm : tmap) (t : trm) : trm =
  let aux (t : trm) : trm =
    subst tm t in
  (* make a recursive call by removing from the map
    the keys that satisfy [f] *)
  let aux_filter (f : var -> bool)  (t : trm) : trm =
    let tm2 = Trm_map.filter (fun k v -> not (f k)) tm in
    subst tm2 t in
  match t.desc with
  (* Hack to avoid unnecessary get operations when we substitute a variable occurrence with arbitrary code *)
  | Trm_var (vk, x) ->
    begin match Trm_map.find_opt x.qvar_var tm with
    | Some t1 ->
      let t1 = {t1 with annot = t1.annot} in
      if (is_trm_arbit t1 && vk = Var_mutable) then trm_address_of t1 else t1
    | _ -> t
    end
  | Trm_seq ts ->
    let cur_tm = ref tm in
    let subst_item ti =
      begin match ti.desc with
      | Trm_let (_, (x, ty), tbody) ->
        let ti2 = subst !cur_tm ti in
        cur_tm := Trm_map.filter (fun k _v -> k <> x) tm;
        ti2
      | Trm_let_fun (f, __retty, targs, tbody) ->
        cur_tm := Trm_map.filter (fun k _v -> k <> f.qvar_var) tm;
        subst !cur_tm ti
      | _ -> subst !cur_tm ti
      end
      in
      let ts2 = Mlist.map subst_item ts in
      { t with desc = Trm_seq ts2}
  | Trm_for (l_range, _) ->
    let (index, _, _, _, _, _) = l_range in
    trm_map (aux_filter (fun x -> x = index)) t
  | Trm_for_c (init, _, _, _) ->
    let vs = vars_bound_in_trm_init init in
    trm_map (aux_filter (fun x -> List.mem x vs)) t
  | _ -> trm_map aux t


(* [subst x u t]: replace all the occurences of x with t *)
let subst_var (x : var) (u : trm) (t : trm) =
  let empty_tmap =  Trm_map.empty  in
  let tmap = Trm_map.add x u empty_tmap  in
  subst tmap t

(* FIXME: unused? *)
(* [clean_nobraces tg]: remove all the hidden sequence starting from target [ŧg] *)
(* let clean_nobraces (new_target_apply : bool): Transfo.t =
  (if new_target_apply then Target.apply else apply_on_targets) (
    apply_on_path (fun t -> clean_no_brace_seq ~all:true (-1) t))
    *)

(* [replace_return_with_assign exit_label r t]: removes all the return statements from the body of a function declaration,
      [exit_label] - generated only if [t] is there is a sequence that contains not terminal instructions,
      [r] - the name of the variable replacing the return statement,
      [t] - ast of the body of the function. *)
let replace_return_with_assign ?(check_terminal : bool = true) ?(exit_label : label = "") (r : var) (t : trm) : (trm * int) =
  let nb_gotos = ref 0 in
  let rec aux (is_terminal : bool) (t : trm) : trm =
    match t.desc with
    | Trm_abort ab ->
      begin match ab with
      | Ret t1 ->
        begin match t1 with
        | Some t2 ->
          let t1' = (aux false t2) in
          let t_assign = if r = "" then t2 else trm_set (trm_var r) t1' in
          if is_terminal
            then t_assign
            else begin
                 incr nb_gotos;
                 if exit_label = "" then t_assign else trm_seq_no_brace [t_assign; trm_goto exit_label]
                 end
        | _ ->
            incr nb_gotos;
            if exit_label = "" then trm_unit () else trm_goto exit_label
        end
      | _ ->
          incr nb_gotos;
          if exit_label = "" then trm_unit () else trm_goto exit_label
      end
    | Trm_let_fun _ -> t (* do not recurse through local function definitions *)
    | _-> trm_map_with_terminal is_terminal aux t
  in
  let t = aux check_terminal t in
  (t, !nb_gotos)


(* [get_field_name rf]: returns the name of the field [rf]. *)
let get_field_name (rf : record_field) : var option =
  match rf with
  | Record_field_member (n, _) -> Some n
  | Record_field_method t1 ->
    begin match t1.desc with
    | Trm_let (_, (n, _), _) -> Some n
    | Trm_let_fun (qn, _, _, _) -> Some qn.qvar_var
    | _ -> None
    end

(* [fix_class_member_accesses class_name t]: when class methods are inlined fixes all the direct accesses to class members
      on method definitions. *)
let fix_class_member_accesses (class_name : var) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match struct_get_inv t with
    | Some (base, field) ->
      begin match base.desc with
      | Trm_var (_, qn) when qn.qvar_var = "this" ->
        trm_struct_get ~annot:t.annot (trm_var_get class_name) field
      | _ -> trm_map aux t
      end
    | _ -> trm_map aux t
   in aux t