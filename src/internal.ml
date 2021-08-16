open Ast
open Target
(* TODO: reimplement a function change_trm that operations on explicit paths
   and thus does not need to do resolution again. *)

(* Replaces all the occurrences of t_before in the ast [t] with t_after.
    If the user does not want to target the full ast but just some specific locations,
    then he can enter the targeted locations in [change_at].
*)
let change_trm ?(change_at : target list = [[]]) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  let rec apply_change (t' : trm) : trm=
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



(* Replaces all the occurrences of types ty_before in the ast [t] with ty_after.
    If the user does not want to target the full ast but just some specific locations,
    then he can enter the targeted locations in [change_at].
*)
let change_typ ?(change_at : target list = [[]]) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  (* change all occurences of ty_before in ty *)
  let rec change_typ (ty : typ) : typ =

    if same_types ~match_generated_star:false ty ty_before then ty_after
      else typ_map change_typ ty
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

(* For an ast node with path [dl] this function returns back the path ot the sequence
     containing that trm together with the index of that trm in the surrounding sequence.
*)
let isolate_last_dir_in_seq (dl : path) : path * int =
  match List.rev dl with
  | Dir_seq_nth i :: dl' -> (List.rev dl',i)
  | _ -> fail None "isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)

(* For an ast node with path [dl] where the node represents a function call, this function returns
    the path to the sequence containing the instruction which contains the function call, the local path
    from that instruction to the call and the index of that instruction on its surrounding sequence.
*)
let get_call_in_surrounding_sequence (dl : path) : path * path * int =
  let rec aux (acc : path) (dl : path) =
    match dl with
    | [] -> fail None "get_call_in_surrounding_sequence: empty path"
    | Dir_seq_nth i :: dl'-> (List.rev dl', acc, i)
    | dir :: dl' -> aux (dir :: acc) dl'
  in
  aux [] (List.rev dl)

(* For an ast node with path [dl] where the node is a children of the body of a for loop, this function returns
    the path to the for loop contining that node together with the index of the instruction in the body sequence
    of the for loop.
*)
let get_trm_in_surrounding_loop(dl : path) : path * int =
    match List.rev dl with
    | Dir_seq_nth i :: Dir_body :: dl' -> (List.rev dl', i)
    | _ -> fail None "get_trm_in_surrounding_loop: empty path"

(* Rename all the occurrences of a variable by adding an underscore as prefix*)
let fresh_args (t : trm) : trm =
  let rec aux (global_trm : trm) (t : trm) : trm =
    match t.desc with
    | Trm_var x -> trm_var ("_" ^ x)
    | _ -> trm_map (aux global_trm) t
  in aux t t

(* Transform code entered as string into ast, this function returns a list of ast nodes because, the user can enter
    code which could be a list of instructions, for ex: int x; x = 1; x = 5;
    [context] - denotes specific entered by the user
    [is_expression] - a flag for telling if the entered code is an expression or not, this is needed to decide
      if we should add a semicolon at the end or not.
    [s] - denotes the code entered as a string.
    [ctx] - check Trace.context
*)
let parse_cstring (context : string) (is_expression : bool) (s : string) (ctx : Trace.context) : trm list =
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


(* For a single instruction s return its ast *)
let term ?(context : string = "")(ctx : Trace.context) (s : string) : trm =
  let tl = parse_cstring context true s ctx  in
  match tl with
  | [expr] -> expr
  | _ -> fail None "term: expcted a list with only one element"

(* In the case of typedef struct give back the list of struct fields *)
let get_field_list (td : typedef) : (var * typ) list =
  begin match td.typdef_body with
  | Typdef_prod (_, s) -> List.rev s
  (* | Typdef_prod (_, s) -> List.rev (fst (List.split s)) *)
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
  
(* Find the declaration of variable [x] if it exists in [t] where t usually is the full ast.*)
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


(* If node [t] represents a loop nest then go through all of them an return an 
    ordered list of their indices where the order is the depth order
*)
let rec get_loop_nest_indices (t : trm) : 'a list =
  match t.desc with
  | Trm_for (index, _, _, _, _, body) ->
    begin match body.desc with
    | Trm_seq [f_loop] ->
      index :: get_loop_nest_indices f_loop
    | _ -> index :: []
    end
  | Trm_for_c (_, _, _, body) ->
    let index = for_loop_index t in
    begin match body.desc with
    | Trm_seq [f_loop] -> index :: get_loop_nest_indices f_loop
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
  | Trm_for (index, direction, start, stop, step, body) ->
    Some ((fun b -> trm_for index direction start stop step b), body)
  | _ -> fail t.loc "extract_loop: expected a loop"

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
(* *) let remove_pairs (ys : var list) (xs : (var * typ) list) = List.fold_left (fun acc y -> remove_pair y acc) xs ys (* *)
(* ************************************************************************************************************************)

(* Move struct fields with names [local_l] after field [x] *)
let move_fields_after (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ ) list=
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
let move_fields_before (x : var) (local_l : var list) (l : (var * typ) list) : (var * typ) list =
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

(* For a trm t with index [index] in its surrounding sequence return that the list of trms before t,
    t itself and the list of trms behind t.
*)
let get_trm_and_its_relatives (index : int) (trms : trm list) : (trm list * trm * trm list) =
  let lfront, lback = Tools.split_list_at index trms in
  let element, lback = Tools.split_list_at 1 lback in
  let element = match element with 
  | [el] -> el
  | _ -> fail None "get_element_and_its_relatives: expected a list with a single element"
  in
  (lfront, element, lback)
  
(* Remove all the sequences from ast with annotation No_braces if [all] is equal to true
    otherwise remove only those sequence with id [id].
*)
let clean_no_brace_seq ?(all : bool = false) (id : int) (t : trm) : trm =
  let rec clean_up_in_list (tl : trm list) : trm list =
    match tl with 
    | [] -> []
    | t :: tl ->
      begin match t.desc with
      | Trm_seq tl' ->
        begin match all with 
        | false ->
          let current_seq_id = get_nobrace_id t in
          if current_seq_id = id 
            then tl' @ (clean_up_in_list tl)
            else t :: (clean_up_in_list tl) 
        | true ->
          tl' @ (clean_up_in_list tl)
        end
      | _ -> t :: (clean_up_in_list tl)
      end in
  let rec aux (t : trm) : trm =
    match t.desc with 
    | Trm_seq tl ->
      trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (clean_up_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in aux t

(* Apply function clean_no_brace over the curren ast *)
let nobrace_remove_and_exit ?(all : bool = false) () =
    match all with 
    | true -> 
      Trace.apply (fun _ctx ast -> clean_no_brace_seq ~all (-1) ast)
    | false ->
      let id = Nobrace.exit () in
      Trace.apply (fun _ctx ast -> clean_no_brace_seq ~all id ast)
    
    
(* Called when there is generated a no brace sequence from a transformation, this is needed
    to generate a unique id for that nobrace sequence.
*)
let nobrace_enter () =
  Nobrace.enter()

(* Transform a normal sequence into a nobrace sequence *)
let set_no_brace_if_sequence (t : trm) : trm = 
  match t.desc with 
  | Trm_seq tl1 -> trm_seq_no_brace tl1
  | _-> t

(* Change the current body of loop [loop] with [body]*)
let change_loop_body (loop : trm) (body : trm) : trm = 
  match loop.desc with 
  | Trm_for (index , direction, start, stop, step, _) ->
    trm_for index direction start stop step body
  | Trm_for_c (init, cond, step, _) ->
    trm_for_c init cond step body
  | _-> fail loop.loc "change_loop_body: expected for loop"

(* Get the constraint from a list of constraints(targets) *)
let get_constr_from_target (tg : target) : constr =
  match tg with 
  | [cnst] -> cnst
  | _ -> cChain tg

