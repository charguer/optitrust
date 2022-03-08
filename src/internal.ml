
open Ast
open Target

(* [same_kind t1 t2] check if two ast nodes are of the same kind or not *)
let same_kind (t1 : trm) (t2 : trm) : bool =
  match t1.desc, t2 .desc with
  | Trm_val _, Trm_val _ -> true
  | Trm_var _, Trm_var _ -> true
  | Trm_var _, Trm_apps _  when is_get_operation t2 -> true
  | Trm_array _, Trm_array _ ->  true
  | Trm_struct _, Trm_struct _ -> true
  | Trm_let _, Trm_let _ -> true
  | Trm_let_fun _, Trm_let_fun _ -> true
  | Trm_let_record _, Trm_let_record _ -> true
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
  | Trm_labelled _, Trm_labelled _ -> true
  | Trm_goto _, Trm_goto _ -> true
  | Trm_arbitrary _, Trm_arbitrary _ -> true
  | Trm_omp_directive  _, Trm_omp_directive _ -> true
  | Trm_omp_routine _ , Trm_omp_routine _ -> true
  | Trm_extern _, Trm_extern _ -> true
  | Trm_namespace _, Trm_namespace _ -> true
  | Trm_template _, Trm_template _ -> true
  | _ , _ -> false

(* check if two ast nodes when translated give the same code *)
let same_trm ?(ast_decode:bool=false) (t1 : trm) (t2 : trm) : bool =
  if same_kind t1 t2 then
    AstC_to_c.ast_to_string t1 = AstC_to_c.ast_to_string  t2
   else false

(* check if two values are equal *)
let same_val (v1 : value) (v2 : value) : bool =
  same_trm (trm_val v1) (trm_val v2)


(* Replaces all the occurrences of t_before in the ast [t] with t_after.
    If the user does not want to target the full ast but just some specific locations,
    then he can enter the targeted locations in [change_at].
*)
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
         print_info t'.loc "change_trm: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at in
    res



(* Replaces all the occurrences of types ty_before in the ast [t] with ty_after.
    If the user does not want to target the full ast but just some specific locations,
    then he can enter the targeted locations in [change_at].
*)
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
      {t with typ = match t.typ with
                    | None -> None
                    | Some ty' -> Some (change_typ ty')
      }
    in
    trm_map replace_type_annot t
  in
  let apply_change (t : trm) : trm =
    let rec aux (t : trm) : trm =
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
       | Trm_var (_, x) ->
          let ty = begin match t.typ with
                   | Some ty -> ty
                   | None -> fail t.loc "apply_change: all variable occurrences should have a type"
                   end in
        trm_var ~annot:t.annot ~loc:t.loc ~typ:(Some (change_typ ty)) x
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
         print_info t'.loc "change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t
    change_at


(* For an ast node with path [dl] this function returns back the path ot the sequence
     containing that trm together with the index of that trm in the surrounding sequence.
*)
let isolate_last_dir_in_seq (dl : path) : path * int =
  match List.rev dl with
  | Dir_seq_nth i :: dl' -> (List.rev dl',i)
  | _ -> fail None "isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)




(* For nodes whose parent node is not a sequence, return the path to the sequence containing the instructions
    which contains the node at path [dl], the index of that instuction into the surronding sequence and the
    local path from the instruction at index [i] to the node with the initial path [dl]
*)
let get_instruction_in_surrounding_sequence (dl : path) : path * path * int =
  let rec aux (acc : path) (dl : path) =
    match dl with
    | [] -> fail None "get_instruction_in_surrounding_sequence: empty path"
    | Dir_seq_nth i :: dl'-> (List.rev dl', acc, i)
    | dir :: dl' -> aux (dir :: acc) dl'
  in aux [] (List.rev dl)


(* For an ast node with path [dl] where the node is a children of the body of a for loop, this function returns
    the path to the for loop contining that node together with the index of the instruction in the body sequence
    of the for loop.
*)
let get_trm_in_surrounding_loop (dl : path) : path * int =
    match List.rev dl with
    | Dir_seq_nth i :: Dir_body :: dl' -> (List.rev dl', i)
    | _ -> fail None "get_trm_in_surrounding_loop: empty path"


(* [get_surrounding_trm checker dl t] given the path [dl] that resolves to trm res find a predecessor of that trm
    that satisfies the predicate [checker]*)
let get_surrounding_trm (checker : trm -> bool) (dl : path) (t : trm) : path = 
  let rec aux (dl1 : path) : path = 
    match dl1 with 
    | [] -> []
    | hd_p :: tl_p -> 
      let res = Path.resolve_path (List.rev dl1) t in 
        if checker res then (List.rev dl1) else aux tl_p
    in 
  aux (List.rev dl)

(* Maybe we will need this later on *)
(* [get_surrouding_access dl t] specialization of get_surrouding_trm for accesses*)
let get_surrouding_access (dl : path) (t : trm) : path = 
  get_surrounding_trm is_access dl t

(* [get_surrouding_access dl t] specialization of get_surrouding_trm for read operations*)
let get_surrouding_read (dl : path) (t : trm) : path = 
  get_surrounding_trm is_get_operation dl t

(* [get_surrouding_access dl t] specialization of get_surrouding_trm for write operations*)
let get_surrouding_write (dl : path) (t : trm) : path = 
  get_surrounding_trm is_set_operation dl t

(* [is_decl_body dl] checks if the full path points to a declaration body *)
let is_decl_body (dl : path) : bool =
  match List.rev dl with
  | Dir_body :: _ -> true
  | _ -> false



(* Rename all the occurrences of a variable by adding an underscore as prefix*)
let fresh_args (t : trm) : trm =
  match t.desc with
  | Trm_var (kind, x) -> trm_var ~kind ("_" ^ x)
  | _ -> t

(* In the case of typedef struct give back the list of struct fields *)
let get_field_list (td : typedef) : (var * typ) list =
  begin match td.typdef_body with
  | Typdef_prod (_, s) -> List.rev s
  | _ -> fail None "get_field_lists: expected a Typedef_prod"
  end

(* Checks if typ is a constructed type or it is a composed type, if it is a constructed type then return its id.
    If it is a composed type go in depth and check if it contains a constructed type and return it id.
    Else return -1 meaning that the type [t] is not a constructed type.
*)
let rec get_typid_from_typ (t : typ) : int =
  match t.typ_desc with
  | Typ_constr (_, id, _) -> id
  | Typ_const ty -> get_typid_from_typ ty
  | Typ_var (_, id) -> id
  | Typ_ptr {inner_typ = ty;_} -> get_typid_from_typ ty
  | Typ_array (ty, _) -> get_typid_from_typ ty
  | Typ_fun (_, ty) -> get_typid_from_typ ty
  | _ -> -1

(* For an ast node [t] check if its type is a constructed type. If this is the case then return its id
    Else return -1. Meaning that node [t] has a different type.
 *)
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
  | Trm_struct _ ->
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


let nb_inits (x : var) (t : trm) : int =
  let counter = ref 0 in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_apps (_,[ls; _]) ->
      begin match ls.desc with
      | Trm_var (_, y) when y = x -> incr counter; ls
      | _ -> ls
      end
    | _ -> trm_map aux t
    in
    let _t = aux t in !counter

(* Find the declaration of variable [x] if it exists in [t] where t usually is the full ast.
   If [~require_body:true] is provided, then only definitions with a body are selected. *)
let toplevel_decl ?(require_body:bool=false) (x : var) : trm option =
  let full_ast = Target.get_ast () in
  match full_ast.desc with
  | Trm_seq tl ->
    Mlist.fold_left(
      fun acc t1 ->
      match acc with
      | Some _ -> acc
      | _ -> match t1.desc with
            | Trm_typedef td when td.typdef_tconstr = x -> Some t1
            | Trm_let (_, (y, _),_ ) when y = x -> Some t1
            | Trm_let_fun (y, _, _, body) when y = x ->
              if require_body then begin
                match body.desc with
                | Trm_seq _ -> Some t1 (* LATER: we might want to test insted if body.desc <> trm_uninitialized or something like that *)
                | _ -> None
              end else begin
                Some t1
              end
            | _ -> None
  ) None tl
  | _ -> fail full_ast.loc "top_level_decl: the full ast starts with the main sequence which contains all the toplevel declarations"


(* [local_decl x t] search for a declaration with name [x] in node [t] *)
let rec local_decl (x : var) (t : trm) : trm option =
  match t.desc with
  | Trm_typedef td when td.typdef_tconstr = x -> Some t
  | Trm_let (_, (y, _),_ ) when y = x -> Some t
  | Trm_let_fun (y, _, _, body) ->
    if y = x then Some t else local_decl x body
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


(* If node [t] represents a loop nest then go through all of them an return an
    ordered list of their indices where the order is the depth order
*)
let rec get_loop_nest_indices (t : trm) : 'a list =
  match t.desc with
  | Trm_for (index, _, _, _, _, body) ->
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

(* For loop [t] return a pair of function and trm. Where the function takes trm b
    and gives a loop with the same components as loop [t] but with body b. And the trm
    is the body of the loop [t].
*)
let extract_loop (t : trm) : ((trm -> trm) * trm) option =
  match t.desc with
  | Trm_for_c (init, cond, step, body) ->
    Some ((fun b -> trm_for_c init cond step b), body)
  | Trm_for (index, start, direction, stop, step, body) ->
    Some ((fun b -> trm_for index start direction stop step b), body)
  | _ ->
    fail t.loc "extract_loop: expected a loop"

(* For a struct field with name [field] and  [fields] being the list of fields of the
    same struct return back the index of field [field] in the list of fields [fields].
*)
let get_field_index (field : field) (fields : (var * typ) list) : int =
  let rec aux field fields c = match fields with
    | [] -> failwith "get_field_index: empty list"
    | (f, _) :: tl ->
      if (f = field) then c else aux field tl (c+1)
    in
  aux field fields 0

(*********************Auxiliary functions for reorder transformation ******************************************************)
(* *) let get_pair x xs = List.fold_left(fun acc (y,ty) -> if y = x then (y,ty) :: acc else acc) [] xs                 (* *)
(* *) let get_pairs ys xs = List.fold_left(fun acc y -> (get_pair y xs) :: acc) [] ys                                  (* *)
(* *) let remove_pair x xs = List.filter (fun (y,_) -> y <> x) xs                                                      (* *)
(* *) let remove_pairs (ys : vars) (xs : (var * typ) list) = List.fold_left (fun acc y -> remove_pair y acc) xs ys (* *)
(* ************************************************************************************************************************)

(* Move struct fields with names [local_l] after field [x] *)
let move_fields_after (x : var) (local_l : vars) (l : (var * typ) list) : (var * typ ) list=
  let fins = List.flatten (get_pairs local_l l )in
  let l = remove_pairs local_l l in
  let rec aux = function
    | [] -> failwith "move_fields_after: empty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then fins @ [hd, ty] @ tl
        else (hd,ty) :: aux tl
      in
    aux l

(* Move struct fields with names [local_l] before field [x] *)
let move_fields_before (x : var) (local_l : vars) (l : (var * typ) list) : (var * typ) list =
  let fins = List.flatten (get_pairs local_l l) in
  let l = remove_pairs local_l l in
  let rec aux = function
    | [] -> failwith "move_fields_after: empty list" (* raise an error x not part of the list *)
    | (hd, ty) :: tl ->
      if hd = x
        then [hd, ty] @ fins @ tl
        else (hd,ty) :: aux tl
      in
    aux l

let reorder_fields (reorder_kind : reorder) (local_l : vars) (sf : (var * typ) list) : (var * typ) list =
  match reorder_kind with
  | Reorder_after around -> move_fields_after around local_l sf
  | Reorder_before around -> move_fields_before around local_l sf
  | Reorder_all -> let check = (List.length (Tools.list_remove_duplicates local_l) = List.length sf) in
    begin match check with
    | false -> fail None "reorder_fields: list of fields entered contains duplicates"
    | true -> List.map (fun x ->
        match List.assoc_opt x sf with
      | Some d -> (x,d)
      | None -> fail None (Tools.sprintf "reorder_fields: field %s doest not exist" x)
        ) (List.rev local_l)
    end

(* For a trm t with index [index] in its surrounding sequence return that the list of trms before t,
    t itself and the list of trms behind t.
*)

let get_trm_and_its_relatives (index : int) (trms : trm mlist) : (trm mlist * trm * trm mlist) =
  let lfront, lback = Mlist.split index trms in
  let element, lback = Mlist.split 1 lback in
  let element =
    if Mlist.length element = 1
      then Mlist.nth element 0
      else fail None "get_element_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)

(* In the case of nested sequence, nested initialization lists for arrays and structs, this function
    can be used to inline the sublist at index [index] into the main list
*)
let inline_sublist_at (index : int) (ml : trm mlist) : trm mlist =
  let lfront, st, lback  = get_trm_and_its_relatives index ml in
  match st.desc with
  | Trm_seq tl | Trm_array tl | Trm_struct tl ->
    Mlist.merge (Mlist.merge lfront tl) lback
  | _ -> fail st.loc "inline_sublist_at: expected an ast node which taks a mlist as parameter"


(* Remove all the sequences from ast with annotation No_braces if [all] is equal to true
    otherwise remove only those sequence with id [id].
*)
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
      {t with desc = Trm_seq new_tl}
    | _ -> trm_map aux t
   in aux t

(* Apply function clean_no_brace over the curren ast *)
let nobrace_remove_and_exit () =
  let id = Nobrace.exit () in
  Trace.apply (fun ast -> clean_no_brace_seq id ast)


(* Called when there is generated a no brace sequence from a transformation, this is needed
    to generate a unique id for that nobrace sequence.
*)
let nobrace_enter () =
  Nobrace.enter()

(* Transform a normal sequence into a nobrace sequence *)
let set_nobrace_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq tl1 -> trm_seq_no_brace (Mlist.to_list tl1)
  | _-> t

(* Check if the current sequence is visible or not or not *)
let is_nobrace (t : trm) : bool =
  match t.desc with
  | Trm_seq _ ->
    List.exists (function No_braces _ -> true | _ -> false) t.annot
  | _ -> false


(*  *)
let remove_nobrace_if_sequence (t : trm) : trm =
  match t.desc with
  | Trm_seq _ ->
    if is_nobrace t then trm_annot_filter (function No_braces _ -> true | _ -> false) t else t
  | _ -> t


(* Change the current body of loop [loop] with [body]*)
let change_loop_body (loop : trm) (body : trm) : trm =
  match loop.desc with
  | Trm_for (index , start, direction, stop, step, _) ->
    trm_for index start direction stop step body
  | Trm_for_c (init, cond, step, _) ->
    trm_for_c init cond step body
  | _-> fail loop.loc "change_loop_body: expected for loop"

(* [is_trm_loop t] check if [t] is a loop or not *)
let is_trm_loop (t : trm) : bool =
  match t.desc with
  | Trm_for _ | Trm_for_c _ -> true
  | _ -> false

(* [is_struct_type t] check if t is type struct or not
    Note: The current infrastructure of Optitrust supports only
      struct declared via typedefs, later we will add support for
      struct types not declared via a typedef.
*)
let is_struct_type (t : typ) : bool =
  match t.typ_desc with
  | Typ_constr (_tv, tid, _) ->
    begin match Context.typid_to_typedef tid with
    | Some td ->
      begin match td.typdef_body with
      | Typdef_prod _ -> true
      | _ -> false
      end
    | _ -> false
    end
  | Typ_record _ -> false (* LATER: All the transformations that work with typedefs should also work with structs *)
  | _ -> false



(* Get the constraint from a list of constraints(targets) *)
let get_constr_from_target (tg : target) : constr =
  match tg with
  | [cnst] -> cnst
  | _ -> cChain tg

(* A wrapper for creating and deleting a nobrace sequence *)
let nobrace_remove_after ?(remove : bool = true) (f : unit -> unit) : unit =
  if remove then begin nobrace_enter();
  f();
  nobrace_remove_and_exit() end

(* In the cases when targeted sequences are labelled, this wrapper targets directly the sequence instead of the labeeld ast node *)
let apply_on_path_targeting_a_sequence ?(keep_label:bool = true) (tr:trm->trm) (op_name:string) : trm->trm =
  fun (t:trm) ->
    match t.desc with
    | Trm_seq _ -> tr t
    | Trm_labelled (l, t1) ->
        begin match t1.desc with
        | Trm_seq _ ->
          if keep_label
          then trm_labelled l (tr t1)
          else tr t1
        | _ -> fail t.loc (op_name ^ ": expected a labelled sequence")
        end

    | _ -> fail t.loc (op_name ^ ": expected a sequence or a labelled sequence")

(* make sure each occurrence of y in t is marked with type variable x *)
let rec replace_type_with (x : typvar) (y : var) (t : trm) : trm =
  match t.desc with
  | Trm_var (_, y') when y' = y ->
    trm_var ~annot:t.annot ~loc:t.loc ~add:t.add ~typ:(Some (typ_constr  x )) y
  | _ -> trm_map (replace_type_with x y) t

(* find all the occurrences of variables in [t] and check if they are key in map [tm]
    if yes then assign its values otherwise do nothing
*)
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
    begin match Trm_map.find_opt x tm with
    | Some t1 ->
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
        cur_tm := Trm_map.filter (fun k _v -> k <> f) tm;
        subst !cur_tm ti
      | _ -> subst !cur_tm ti
      end
      in
      let ts2 = Mlist.map subst_item ts in
      { t with desc = Trm_seq ts2}
  | Trm_for (index, _, _, _, _, _) ->
    trm_map (aux_filter (fun x -> x = index)) t
  | Trm_for_c (init, _, _, _) ->
    let vs = vars_bound_in_trm_init init in
    trm_map (aux_filter (fun x -> List.mem x vs)) t
  | _ -> trm_map aux t


(* [subst x u t] replace all the occurences of x with t *)
let subst_var (x : var) (u : trm) (t : trm) =
  let empty_tmap =  Trm_map.empty  in
  let tmap = Trm_map.add x u empty_tmap  in
  subst tmap t

(* [clean_nobraces tg] Remove all the hidden sequence starting from target [ŧg] *)
let clean_nobraces : Transfo.t =
  apply_on_targets (apply_on_path (fun t -> clean_no_brace_seq ~all:true (-1) t))




(* replace with x the types of the variables given by their index
  assumption: t is a fun body whose argument are given by tvl
*)
(* let replace_arg_types_with (x : typvar) (il : int list) (tvl : typed_vars) (t : trm) : trm =
  List.fold_left (fun t' i ->
    let (y, _) = List.nth tvl i in
    replace_type_with x y t'
  )
  t il
let rec functions_with_arg_type ?(outer_trm : trm option = None) (x : typvar) (t : trm) : ilset funmap =
  let rec aux (t : trm) : ilset funmap =
    match t.desc with
    | Trm_let (_, _, body) -> aux body
    | Trm_let_fun (_, _, _, body) -> aux body
    | Trm_if (cond, then_, else_) -> aux cond +@ aux then_ +@ aux else_
    | Trm_seq tl ->
      Mlist.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty tl
    | Trm_apps (f, tl) ->
      let ilsm =
        Mlist.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty tl
      in
      begin match f.desc with
      (* If f is a variable, we have to add f to ilsm if an argument has type x
        ignore the free function
      *)
      | Trm_var (_, f) when f <> "free" ->
          let il =
            fold_lefti
              (fun i il (t' : trm) ->
                match t'.typ with
                (* note: also works for heap allocated variables *)
                | Some {typ_desc = Typ_constr (x', _, _);_} when x' = x -> i :: il
                | _ -> il
              )
              []
              tl
          in
          begin match il with
          | [] -> ilsm
          | _ ->
             let ils = IntListSet.singleton (List.rev il) in
             Fun_map.update f
               (function
                | None -> Some ils
                | Some ils' -> Some (IntListSet.union ils ils')
               )
               ilsm
          end
       (* in other cases, do a recursive call *)
       | _ -> ilsm +@ aux f
       end
      | Trm_while (cond, body) -> aux cond +@ aux body
      | Trm_for_c (init, cond, step, body) ->
        aux init +@ aux cond +@ aux step +@ aux body
      | Trm_for (_, _, _, _, _, body) -> aux body
      | Trm_switch (cond, cases) ->
        aux cond +@
          List.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty
            (List.map (fun (_, t') -> t') cases)
      | Trm_abort (Ret (Some t'))
        | Trm_labelled (_, t') ->
          aux t'
      (* val, var, array, struct, type decl, aborts with no argument *)
      | _ -> Fun_map.empty
    in
    let ilsm = aux t in
    (*
      For each function, do a recursive call on its declaration where the type of arguments is replaced with x
    *)
    Fun_map.fold (
      fun f ils res ->
        IntListSet.fold (
          fun il res ->
            (*
              First compute the body of f where the arguments at position in il have type x
            *)
            let global_trm =
              match outer_trm with
              | None -> t
              | Some t' -> t'
            in
            match (toplevel_decl f global_trm) with
            | None ->
              print_info global_trm.loc
               ("functions_with_arg_type: cannot find declaration of " ^^
                  "function %s, ignoring it.\n") f;
               Fun_map.remove f res
            | Some dl ->
              begin match dl.desc with
              | Trm_let_fun (_, _, args, body) ->
                let b = replace_arg_types_with x il args body in
                (* then do a recursive call on the body *)
                res +@ functions_with_arg_type ~outer_trm:(Some global_trm) x b
              | _ -> fail t.loc "function_with_arg_type: expected a function declaration"
              end

        )
        ils res
    ) ilsm ilsm *)


(*
  add copies of the provided functions given by their name
  each function f is mapped to the set of lists of indices corresponding to uses
  of f where the arguments at those indices have x for type
  the name of the copies are indexed with "new_name_i", where "new_name" is the
  result of name, and similarly for labels in these copies
 *)
(* let rec insert_fun_copies (name : var -> var) (ilsm : ilset funmap) (x : typvar) (t : trm) : trm =
  (* also chang the labels in the body of fun copies for unquennes *)
  let rec label_aux (i : int) (t : trm) : trm =
    match t.desc with
    | Trm_labelled (l, body) ->
       trm_labelled ~annot:t.annot ~loc:t.loc ~add:t.add
         ~attributes:t.attributes (name l ^ "_" ^ string_of_int i)
         (label_aux i body)
    | _ -> trm_map (label_aux i) t
  in
   (Fun_map.fold
       (fun f ils t' ->
         match toplevel_decl f t' with
         | None ->
            fail t'.loc
              ("insert_fun_copies: cannot find declaration of function " ^ f)
         | Some fdecl ->
            begin match fdecl.desc with
            | Trm_let_fun (f', r, tvl, b) when f = f' ->
               (* for each element of ils, create a copy *)
               let tl =
                 intl_set_fold_lefti
                   (fun i il tl ->
                     (*
                       for each argument whose index is in il, use x as
                       (possibly new) type in the declaration
                      *)
                     let tvl' =
                       List.fold_left
                         (map_at (fun (y, _) -> (y, typ_var x))) tvl il
                     in
                     (* add index to labels in the body of the function *)
                     let b' =
                       label_aux i (replace_arg_types_with x il tvl' b)
                     in
                     (* create the copy of f corresponding to il *)
                     (trm_let_fun (name f ^ "_" ^ string_of_int i) r tvl' b') :: tl
                   )
                   ils
                   []
               in
               (* insert the copies of f *)
               (* insert_trm_after dl *)
                 (trm_seq_no_brace  (List.rev tl)) t'
            | _ -> fail t'.loc "insert_fun_copies: bad target to fun decl"
            end
       )
       ilsm
       t
       )
     *)

