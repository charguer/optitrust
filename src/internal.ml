
open Ast
open Target

(* check if two ast nodes give the same code *)
let same_trm (t1 : trm) (t2 : trm) : bool = 
  Ast_to_c.ast_to_string t1 = Ast_to_c.ast_to_string t2 

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
    (* DEBUG: *)
    (* Tools.printf "Trying to match %s with %s\n" (Ast_to_c.ast_to_string t') (Ast_to_c.ast_to_string t_before);
    Tools.printf "--------------------------------------\n"; *)
    if same_trm t' t_before then t_after
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
       | Trm_var x ->
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
let parse_cstring (context : string) (is_expression : bool) (s : string) (ctx : Trace.context) : trms =
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
  | Trm_seq tl1 when Mlist.length tl1 = 1 ->
    let t = Mlist.nth tl1 0 in
     begin match t.desc with
     | Trm_seq tl  ->
        let fun_def = List.nth (List.rev (Mlist.to_list tl)) 0 in
        begin match fun_def.desc with
        | Trm_let_fun (_, _, _, fun_body) ->
          begin match fun_body.desc with
          | Trm_seq tl -> Mlist.to_list tl
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
      | Trm_var y when y = x -> incr counter; ls
      | _ -> ls
      end
    | _ -> trm_map aux t
    in
    let _t = aux t in !counter

(* Find the declaration of variable [x] if it exists in [t] where t usually is the full ast.*)
let rec toplevel_decl (x : var) (t : trm) : trm option =
  match t.desc with
  | Trm_typedef td when td.typdef_tconstr = x -> Some t
  | Trm_let (_, (y, _),_ ) when y = x -> Some t
  | Trm_let_fun (y, _, _, body) ->
    if y = x then Some t else toplevel_decl x body 
  | Trm_seq tl ->
    Mlist.fold_left(
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
  | Trm_for (index, direction, start, stop, step, body) ->
    Some ((fun b -> trm_for index direction start stop step b), body)
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
let clean_no_brace_seq (id : int) (t : trm) : trm =
  let rec aux (t : trm) : trm =
    match t.desc with 
    | Trm_seq tl ->
      let indices_list = List.flatten (List.mapi (fun i t1 -> 
        let current_seq_id = get_nobrace_id t1 in
        begin match current_seq_id with 
        | Some c_i when c_i = id -> [i] 
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
  | Trm_for (index , direction, start, stop, step, _) ->
    trm_for index direction start stop step body
  | Trm_for_c (init, cond, step, _) ->
    trm_for_c init cond step body
  | _-> fail loop.loc "change_loop_body: expected for loop"


let is_trm_loop (t : trm) : bool = 
  match t.desc with 
  | Trm_for _ | Trm_for_c _ -> true 
  | _ -> false


(* Get the constraint from a list of constraints(targets) *)
let get_constr_from_target (tg : target) : constr =
  match tg with 
  | [cnst] -> cnst
  | _ -> cChain tg

(* A wrapper for creating and deleting a nobrace sequence *)
let nobrace_remove_after (f : unit -> unit) : unit =
  nobrace_enter();
  f();
  nobrace_remove_and_exit()

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
  | Trm_var y' when y' = y ->
    trm_var ~annot:t.annot ~loc:t.loc ~add:t.add ~typ:(Some (typ_constr  x )) y
  | _ -> trm_map (replace_type_with x y) t


(* Check if a regexp matches a given string or not *)
let pattern_matches (pattern : string) (s : string) : bool = 
  try let _ = Str.search_forward (Str.regexp pattern) s 0 in true 
  with Not_found -> false 


(* find all the occurrences of variables in [t] and check if they are key in map [tm] 
    if yes then assign its values otherwise do nothing
*)
let variable_substitute (tm : tmap) (t : trm) : trm = 
  let rec function_to_apply (t : trm) : trm = 
    match t.desc with 
    | Trm_var x -> 
      begin match Trm_map.find_opt x tm with
      | Some t1 -> t1
      | _ -> t
      end
    | _ -> trm_map function_to_apply t
  in
  trm_map function_to_apply t

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
      | Trm_var f when f <> "free" ->
          let il =
            foldi
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
                 intl_set_foldi
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



(* TOOD: Optimize this function later *)
(* [trm_fors_inv nb t] got into a node of nested loops and return all the components
    of all the loops up to the depth of [nb]
 *)
let trm_fors_inv (nb : int) (t : trm) : (loop_range list * trm) option = 
  let nb_loops = ref 0 in
  let body_to_return  = ref (trm_int 0) in
  let rec aux (t : trm) : loop_range list = 
    match t.desc with 
    | Trm_for (index, direction, start, stop, step, body) ->
      incr nb_loops;
      begin match body.desc with 
      | Trm_seq tl when Mlist.length tl = 1 -> 
        if !nb_loops = nb 
          then begin
            body_to_return := body;
            (index, direction, start, stop, step) :: []
            end
          else 
            (index, direction, start, stop, step) :: aux (Mlist.nth tl 0)
      | _ -> 
        (index, direction, start, stop, step) :: []
      end
      
    | _ -> []
    in
  
  let loop_range_list = aux t in
  if List.length loop_range_list <> nb then None else Some (loop_range_list, !body_to_return)
    
