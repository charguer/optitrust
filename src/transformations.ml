open Ast
open Target
open Path_constructors
open Ast_to_c
open Ast_to_text
open Tools

let failure_expected f =
  begin try f(); failwith "should have failed"
  with TransfoError _ -> () end

let write_log (clog : out_channel) (log : string) : unit =
  output_string clog log; flush clog

(* return the list where the nth element is transformed *)
let change_nth (transfo : 'a -> 'a) (al : 'a list) (n : int) : 'a list =
  List.mapi (fun i a -> if i = n then transfo a else a) al

(* follow an explicit target to apply a function on the corresponding subterm *)
let apply_local_transformation (transfo : trm -> trm) (t : trm)
  (dl : path) : trm =
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
          trm_seq ~annot ~loc ~add ~attributes (change_nth (aux dl) tl n)
       | Dir_nth n, Trm_array tl ->
          trm_array ~annot ~loc ~add ~typ ~attributes (change_nth (aux dl) tl n)
       | Dir_nth n, Trm_struct tl ->
          trm_struct ~annot ~loc ~add ~typ ~attributes(change_nth (aux dl) tl n)
       | Dir_nth _, Trm_val (Val_array _) ->
          fail loc "apply_local_transformation: val_array should not appear"
       | Dir_nth _, Trm_val (Val_struct _) ->
          fail loc "apply_local_transformation: val_struct should not appear"
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
       | Dir_body, Trm_decl (Def_var (tx, body)) ->
          trm_decl ~annot ~loc ~is_statement ~add ~attributes (Def_var (tx, aux dl body))
       | Dir_body, Trm_decl (Def_fun (x, tx, txl, body)) ->
          trm_decl ~annot ~loc ~is_statement ~add ~attributes (Def_fun (x, tx, txl, aux dl body))
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
            (change_nth (aux dl) tl n)
       | Dir_arg n, Trm_decl (Def_fun (x, tx, txl, body)) ->
          let txl' =
            change_nth
              (fun (x, tx) ->
                let t' = aux dl (trm_var ~loc x) in
                match t'.desc with
                | Trm_var x' -> (x', tx)
                | _ ->
                   fail loc ("apply_local_transformation: transformation " ^
                               "must preserve fun arguments")
              )
              txl
              n
          in
          trm_decl ~annot ~loc ~is_statement ~add ~attributes
            (Def_fun (x, tx, txl', body))
       | Dir_name, Trm_decl (Def_var ((x, tx), body)) ->
          let t' = aux dl (trm_var ~loc x) in
          (* print_ast  stdout t'; *)
          begin match t'.desc with
          | Trm_var x' ->
             trm_decl ~annot ~loc ~is_statement ~add ~attributes
               (Def_var ((x', tx), body))
          | Trm_decoration(ls,{desc=Trm_var x';_},rs) -> trm_decoration ls rs (trm_decl ~annot ~loc ~is_statement ~add ~attributes
               (Def_var ((x', tx), body)))
          | _ ->
             fail loc ("apply_local_transformation: transformation " ^
                         "must preserve names(var)")
          end
       | Dir_name, Trm_decl (Def_fun (x, tx, txl, body)) ->
          let t' = aux dl (trm_var ~loc x) in
          begin match t'.desc with
          | Trm_var x' ->
             trm_decl ~annot ~loc ~is_statement ~add ~attributes
               (Def_fun (x', tx, txl, body))
          | _ ->
             fail loc ("apply_local_transformation: transformation " ^
                         "must preserve names(function)")
          end
       | Dir_name, Trm_labelled (l, body) ->
          let t' = aux dl (trm_var ~loc l) in
          begin match t'.desc with
          | Trm_var l' ->
             trm_labelled ~annot ~loc ~add ~attributes l' body
          | _ ->
             fail loc ("apply_local_transformation: transformation " ^
                         "must preserve names(label)")
          end

       | Dir_case (n, cd), Trm_switch (cond, cases) ->
          trm_switch ~annot ~loc ~add ~attributes cond
            (change_nth
               (fun (tl, body) ->
                 match cd with
                 | Case_body -> (tl, aux dl body)
                 | Case_name i ->
                    (change_nth (fun ith_t -> aux dl ith_t) tl i, body)
               )
               cases
               n
            )
        | _, _ ->
           let s = dir_to_string d in
           fail loc ("apply_local_transformation: direction " ^ s ^
                       " does not match")
       end

  in
  aux dl t


(*
  insert inert after the subterm pointed at by dl in t
  assumption: dl points at a seq element, thus ends with Dir_nth n
  if the inserted element must be first in the seq, use n < 0
 *)
let insert_trm_after (dl : path) (insert : trm) (t : trm) : trm =
  let dl' = List.rev dl in
  match List.hd dl' with
  | Dir_nth n ->
     apply_local_transformation
       (fun t' ->
         match t'.desc with
         | Trm_seq tl ->
            trm_seq ~annot:t'.annot ~loc:t'.loc ~add:t'.add
              ~attributes:t'.attributes (list_insert n insert tl)
         | _ -> fail t'.loc "insert_trm_after: path points at wrong term"
       )
       t
       (List.rev (List.tl dl'))
  | _ -> fail t.loc "insert_trm_after: bad path"

let left_decoration (index:int):string  = "/*@" ^ string_of_int index ^ "<*/"

let right_decoration (index:int):string  = "/*>" ^ string_of_int index ^ "@*/"


let remove_instruction_aux (clog : out_channel) (t : trm) : trm =
  let log : string =
    let loc : string =
    match t.loc with
    | None -> ""
    | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf "at start_location %d %d end location %d %d" start_row start_column end_row end_column
    in Printf.sprintf
    (" -expression\n%s\n" ^^
    "  %s is an instruction to be deleted \n"
    )
    (ast_to_string t) loc
    in write_log clog log;
    trm_seq ~annot:(Some No_braces) []



let remove_instruction (clog : out_channel) (tr : target) (t : trm) : trm =
  let epl = resolve_target tr t in
  match epl with
  | [] ->
    print_info t.loc "remove_instruction: no matching subterm";
    t
  | _ -> List.fold_left
        ( fun t dl ->
          apply_local_transformation (remove_instruction_aux clog ) t dl )
          t epl

let remove_instructions (clog : out_channel) (instruction_list : (target) list) (t : trm) : trm =
  let t = List.fold_left (fun t instruction -> (remove_instruction clog) instruction t)
  t instruction_list
  in t

(* TODO: debug_path : bool = false
  as argument,
   when turned on, you should do List.iter (fun p -> printf (path_to_string p)) epl
  *)
let show_target ?(debug_ast : bool = false) (tr : target) (t : trm) : trm =
  let epl = resolve_target tr t in
  match epl with
  | [] ->
    print_info t.loc "show_target: no matching subterm\n";
    t
  | [dl] -> if debug_ast then Ast_to_text.print_ast ~only_desc:true stdout t;
            apply_local_transformation (trm_decoration (left_decoration 0) (right_decoration 0) ) t dl

  | _ -> foldi
          (fun i -> if debug_ast then Ast_to_text.print_ast ~only_desc:true stdout t;
                    apply_local_transformation
                   (trm_decoration (left_decoration i) (right_decoration i )))

          t epl


let show_ast ?(file:string="_ast.txt") ?(to_stdout:bool=true) (tr : target) (t : trm) : trm =
  let epl = resolve_target tr t in
  match epl with
  | [] ->
    print_info t.loc "show_ast: no matching subterm\n";
    t
  | _ ->
    let out_ast = open_out file in
    foldi
      (
        fun i -> apply_local_transformation(fun t ->
            if to_stdout then begin
              print_ast ~only_desc:true stdout t;
              output_string stdout "\n\n";
            end;
            output_string out_ast (Printf.sprintf "=========================Occurence %i======================\n" i);
            print_ast ~only_desc:true out_ast t;
            output_string out_ast "\n\n";
            output_string out_ast (Printf.sprintf "------------------------Occurence %i details---------------\n" i);
            print_ast ~only_desc:false out_ast t;
            output_string out_ast "\n\n";
            t)
      )
      t epl
      (* close_out out_ast; *)

(* Change one declaration form const to heap allocated and vice versa*)
let const_non_const_aux (clog : out_channel) (trm_index : int) (t : trm) : trm =
  let rec list_replace_el (el : trm) (i : int) (list : trm list) : 'a list = match list with
    | [] -> failwith "Empty list"
    | x :: xs -> if i = 0 then el :: xs else x :: list_replace_el el (i-1) xs
  in
  let log : string =
      let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      (" -expression\n%s\n" ^^
      " %s section of interest \n"
      )
      (ast_to_string t) loc
    in write_log clog log;
    match t.desc with
    | Trm_seq tl ->
      (* Find the declaration trm, using the index inside the sequence *)
      let t_decl = List.nth tl trm_index in
      (*Check if the delcaration is const or non-const  *)
      begin match t_decl.desc with
      | Trm_seq[decl;assgn] ->
        let var_name,var_type = begin match decl.desc with
        | Trm_decl(Def_var ((x, tx), _)) ->
          let y = match tx.ty_desc with
          | Typ_ptr vt -> vt
          | _ -> fail t.loc "const_non_const_aux: expected a pointer type"
          in x,y
        | _ -> fail t.loc "const_non_const_aux: exepected a declaration"
        end
        in
        let var_value = begin match assgn.desc with
        | Trm_apps (_,[_;v]) ->  v
        | _ -> fail t.loc "const_non_const_aux: expected an assignment"
        end
        in
        let new_trm = trm_decl ~is_statement:true (Def_var ((var_name,var_type),var_value)) in
        let tl = list_replace_el new_trm trm_index tl in
        trm_seq ~annot:t.annot ~loc:t.loc tl
      | Trm_decl(Def_var ((x, tx),dx)) ->
          let new_trm = trm_seq ~annot:(Some Heap_allocated) [
            trm_decl (Def_var ((x,typ_ptr tx),trm_prim (Prim_new tx)));trm_set ~annot:(Some Initialisation_instruction) (trm_var x) dx]

          in
          let tl = list_replace_el new_trm trm_index tl in
          trm_seq ~annot:t.annot tl
      | _ -> fail t.loc "const_non_const_aux: exepected a declaration"
      end

    | _ -> fail t.loc "const_non_const_aux: expected the sequence which contains the declaration"



let const_non_const (clog : out_channel) (tr : target) (t : trm) : trm =
  let epl = resolve_target tr t in
  let app_transfo (t : trm) (dl : path) : trm =
    match List.rev dl with
    | Dir_nth n :: dl' ->
      let dl = List.rev dl' in
      apply_local_transformation (const_non_const_aux clog n )  t dl
    | _ -> fail t.loc "const_non_const: expected a dir_nth inside the sequence"
  in
  match epl with
  | [] -> print_info t.loc "const_non_const: no matching subterm";
    t
  | _ -> List.fold_left (fun t dl -> app_transfo t dl)
    t epl


let rec delete_target_decorators (t : trm) : trm =
  match t.desc with
  | Trm_decoration (_,t',_) -> t'
  | _ -> trm_map (delete_target_decorators ) t

(* make sure each occurence of y in t is marked with type variable x *)
let rec replace_type_with (x : typvar) (y : var) (t : trm) : trm =
  match t.desc with
  | Trm_var y' when y' = y ->
     trm_var ~annot:t.annot ~loc:t.loc ~add:t.add ~typ:(Some (typ_var x))
       ~attributes:t.attributes y
  | _ -> trm_map (replace_type_with x y) t

(*
  replace with x the types of the variables given by their index
  assumption: t is a fun body whose arguments are given by tvl
 *)
let replace_arg_types_with (x : typvar) (il : int list) (tvl : typed_var list)
  (t : trm) : trm =
  List.fold_left
    (fun t' i ->
      let (y, _) = List.nth tvl i in
      replace_type_with x y t'
    )
    t
    il

let rec functions_with_arg_type ?(outer_trm : trm option = None) (x : typvar)
  (t : trm) : ilset funmap =
  let rec aux (t : trm) : ilset funmap =
    match t.desc with
    | Trm_decl (Def_var (_, body)) -> aux body
    | Trm_decl (Def_fun (_, _, _, body)) -> aux body
    | Trm_if (cond, then_, else_) -> aux cond +@ aux then_ +@ aux else_
    | Trm_seq tl ->
       List.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty tl
    | Trm_apps (f, tl) ->
       (* functions may be applied to arguments of type x in terms of tl *)
       let ilsm =
         List.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty tl
       in
       begin match f.desc with
       (*
         if f is a variable, we have to add f to ilsm if an argument has type x
         ignore the free function
        *)
       | Trm_var f when f <> "free" ->
          let il =
            foldi
              (fun i il (t' : trm) ->
                match t'.typ with
                (* note: also works for heap allocated variables *)
                | Some {ty_desc = Typ_var x'; _} when x' = x -> i :: il
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
    | Trm_for (init, cond, step, body) ->
       aux init +@ aux cond +@ aux step +@ aux body
    | Trm_switch (cond, cases) ->
       aux cond +@
         List.fold_left (fun ilsm t' -> ilsm +@ aux t') Fun_map.empty
           (* no function applications in case values *)
           (List.map (fun (_, t') -> t') cases)
    | Trm_abort (Ret (Some t'))
      | Trm_labelled (_, t') ->
       aux t'
    (* val, var, array, struct, type decl, aborts with no argument *)
    | _ -> Fun_map.empty
  in
  let ilsm = aux t in
  (*
    for each function, do a recursive call on its declaration where the type of
    arguments is replaced with x
   *)
  Fun_map.fold
    (fun f ils res ->
      IntListSet.fold
        (fun il res ->
          (*
            first compute the body of f where the arguments at positions in il
            have type x
           *)
          let global_trm =
            match outer_trm with
            | None -> t
            | Some t' -> t'
          in
          match target_to_decl f global_trm with
          (* if the declaration cannot be found, ignore this function *)
          | None ->
             print_info global_trm.loc
               ("functions_with_arg_type: cannot find declaration of " ^^
                  "function %s, ignoring it.\n") f;
             Fun_map.remove f res
          | Some dl ->
             let (def, _) = resolve_path dl global_trm in
             begin match def.desc with
             | Trm_decl (Def_fun (_, _, args, body)) ->
                let b = replace_arg_types_with x il args body in
                (* then do a recursive call on the new body *)
                res +@ functions_with_arg_type ~outer_trm:(Some global_trm) x b
             | _ ->
                fail t.loc
                  ("functions_with_arg_type: wrong target to declaration of " ^ f)
             end
        )
        ils
        res
    )
    ilsm
    ilsm

let clean_up_no_brace_seq (t : trm) : trm =
  let rec clean_up_in_list (tl : trm list) : trm list =
    match tl with
    | [] -> []
    | t :: tl ->
       begin match t.desc with
       | Trm_seq tl' when t.annot = Some No_braces ->
          tl' ++ (clean_up_in_list tl)
       | _ -> t :: (clean_up_in_list tl)
       end
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    (*
      condition on annotation: toplevel insdeclarations might contain a heap
      allocated variable and hence we can find a no_brace seq inside a
      delete_instructions seq, which we do not want to inline
     *)
    | Trm_seq tl when t.annot <> Some Delete_instructions ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (clean_up_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t

(*
  add copies of the provided functions given by their name
  each function f is mapped to the set of lists of indices corresponding to uses
  of f where the arguments at those indices have x for type
  the name of the copies are indexed with "new_name_i", where "new_name" is the
  result of name, and similarly for labels in these copies
 *)
let rec insert_fun_copies (name : var -> var) (ilsm : ilset funmap) (x : typvar)
  (t : trm) : trm =
  (* also change the labels in the body of fun copies for uniqueness *)
  let rec label_aux (i : int) (t : trm) : trm =
    match t.desc with
    | Trm_labelled (l, body) ->
       trm_labelled ~annot:t.annot ~loc:t.loc ~add:t.add
         ~attributes:t.attributes (name l ^ "_" ^ string_of_int i)
         (label_aux i body)
    | _ -> trm_map (label_aux i) t
  in
  clean_up_no_brace_seq
    (Fun_map.fold
       (fun f ils t' ->
         match target_to_decl f t' with
         | None ->
            fail t'.loc
              ("insert_fun_copies: cannot find declaration of function " ^ f)
         | Some dl ->
            let (fdecl, _) = resolve_path dl t' in
            begin match fdecl.desc with
            | Trm_decl (Def_fun (f', r, tvl, b)) when f = f' ->
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
                         (change_nth (fun (y, _) -> (y, typ_var x))) tvl il
                     in
                     (* add index to labels in the body of the function *)
                     let b' =
                       label_aux i (replace_arg_types_with x il tvl' b)
                     in
                     (* create the copy of f corresponding to il *)
                     (trm_decl
                        (Def_fun (name f ^ "_" ^ string_of_int i, r, tvl', b'))
                     ) ::
                     tl
                   )
                   ils
                   []
               in
               (* insert the copies of f *)
               insert_trm_after dl
                 (trm_seq ~annot:(Some No_braces) (List.rev tl)) t'
            | _ -> fail t'.loc "insert_fun_copies: bad target to fun decl"
            end
       )
       ilsm
       t
    )

(*
  replace the applications of the given functions to arguments of type x with
  the application of their copies whose name is given by name
 *)
and replace_fun_names (name : var -> var) (ilsm : ilset funmap) (x : typvar)
  (t : trm) : trm =
  let annot = t.annot in
  let loc = t.loc in
  let is_statement = t.is_statement in
  let add = t.add in
  let typ = t.typ in
  let attributes = t.attributes in
  match t.desc with
  | Trm_apps (fun_, args) ->
     begin match fun_.desc with
     | Trm_var f ->
        (* first check if f is one of the functions that required copies *)
        begin match Fun_map.find_opt f ilsm with
        | None -> (* if not, just do a recursive call on args *)
           trm_map (replace_fun_names name ilsm x) t
        | Some ils ->
           (*
             if f required copies, compute the indices of arguments of type x
            *)
           let il =
             List.rev
               (foldi
                  (fun i il (ti : trm) ->
                    match ti.typ with
                    (* note: also works for heap allocated variables *)
                    | Some {ty_desc = Typ_var x'; _} when x' = x -> i :: il
                    | _ -> il
                  )
                  []
                  args
               )
           in
           begin match il with
           (* if il = [] then no argument is of type x so do a recursive call *)
           | [] -> trm_map (replace_fun_names name ilsm x) t
           | _ -> (* otherwise, find the appropriate name *)
              let io =
                intl_set_foldi
                  (fun i il' io ->
                    match io with
                    | Some _ -> io
                    | None ->
                       if IntList.compare il il' = 0 then Some i else None
                  )
                  ils
                  None
              in
              let f' =
                match io with
                | None -> fail loc "replace_fun_names: unmatched call"
                | Some i -> name f ^ "_" ^ string_of_int i
              in
              (* also do a recursive call on args *)
              let args' = List.map (replace_fun_names name ilsm x) args in
              trm_apps ~annot ~loc ~is_statement ~add ~typ ~attributes
                (trm_var ~annot:fun_.annot ~loc:fun_.loc ~add:fun_.add
                   ~attributes:fun_.attributes f') args'
           end
        end
     | _ -> trm_map (replace_fun_names name ilsm x) t
     end
  | _ -> trm_map (replace_fun_names name ilsm x) t

(*
  insert t_inserted either before the position pointed at by insert_before or
  after the position pointed at by insert_after in t
  both must be resolved as paths to a seq element
 *)
let insert_trm ?(insert_before : target = [])
  ?(insert_after : target = []) (t_inserted : trm) (t : trm) : trm =
  let p =
    match insert_before, insert_after with
    | [], _ :: _ -> insert_after
    | _ :: _, [] -> insert_before
    | [], [] -> fail t.loc "insert_trm: please specify an insertion point"
    | _ -> fail t.loc "insert_trm: cannot insert both before and after"
  in
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target p t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "insert_trm: no matching subterm\n";
     t
  | _ ->
     List.fold_left
       (fun t' dl ->
         match List.rev dl with
         | Dir_nth n :: dl' ->
            begin match insert_before, insert_after with
            (* insert after *)
            | [], _ :: _ -> insert_trm_after dl t_inserted t'
            (* insert before: replace n with n - 1 *)
            | _ :: _, [] ->
               insert_trm_after (List.rev (Dir_nth (n - 1) :: dl')) t_inserted
                 t'
            | [], [] ->
               fail t'.loc "insert_trm: please specify an insertion point"
            | _ -> fail t'.loc "insert_trm: cannot insert both before and after"
            end
         | _ -> fail t'.loc "insert_trm: bad insertion target"
       )
       t
       epl

(*
  replace occurrences of t_before with t_after in t
  paths point at subterms in which all occurences will be replaced
  the empty path means all occurences will be replaced (default behaviour)
  assumption: t_before and t_after are equivalent (in terms of value and of side
  effects)
 *)
let change_trm ?(change_at : target list = [[]]) (t_before : trm)
  (t_after : trm) (t : trm) : trm =
  (* change all occurences of t_before in t' *)
  let rec apply_change (t' : trm) =
    (* necessary because of annotations that may be different *)
    if ast_to_string t' = ast_to_string t_before then t_after
    else
      match t'.desc with
      (*
        particular case for heap allocation: do not change the lhs of the
        initialisation
       *)
      | Trm_seq [t_decl; {desc = Trm_apps (_, [lhs; init]); loc; _}]
           when t'.annot = Some Heap_allocated ->
         trm_seq ~annot:t'.annot ~loc:t'.loc ~add:t'.add
           ~attributes:t'.attributes
           [
             t_decl;
             trm_set ~annot:(Some Initialisation_instruction) ~loc lhs
               (apply_change init)
           ]
      | _ -> trm_map apply_change t'
  in
  List.fold_left
    (fun t' tr ->
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t'.loc "change_trm: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_local_transformation apply_change) t' epl
    )
    t
    change_at



(* same as change_trm but for types *)
let change_typ ?(change_at : target list = [[]]) (ty_before : typ)
  (ty_after : typ) (t : trm) : trm =
  (* change all occurences of ty_before in ty *)
  let rec change_typ (ty : typ) : typ =
    (* necessary because of annotations in trms that may be different *)
    if typ_to_string ty = typ_to_string ty_before then ty_after
    else typ_map change_typ ty
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
      | Trm_decl (Def_var ((y, ty), init)) ->
         trm_decl ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
           ~attributes:t.attributes (Def_var ((y, change_typ ty), aux init))
      | Trm_decl (Def_fun (f, ty, args, body)) ->
         trm_decl ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
           ~attributes:t.attributes
           (Def_fun (f, change_typ ty,
                     List.map (fun (y, ty) -> (y, change_typ ty)) args,
                     aux body)
           )
      | Trm_decl (Def_typ (y, ty)) ->
         trm_decl ~annot:t.annot ~loc:t.loc ~is_statement:t.is_statement ~add:t.add
            ~attributes:t.attributes (Def_typ (y, change_typ ty))
      | _ -> trm_map aux t
    in
    replace_type_annot (aux t)
  in
  List.fold_left
    (fun t' tr ->
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t'.loc "change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_local_transformation apply_change) t' epl
    )
    t

    change_at




let local_other_name_aux (clog : out_channel) (var_type : typvar) (old_var : var) (new_var : var) (t : trm) : trm =
    let log : string =
      let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      (" -expression\n%s\n" ^^
      " %s is a for lopp \n"
      )
      (ast_to_string t) loc
      in write_log clog log;
      match t.desc with
      | Trm_seq [no_braces] ->
        begin match no_braces.desc with
          | Trm_seq [f_loop;del_inst] ->
            begin match f_loop.desc with
            | Trm_for (init, cond, step, body) ->

              let new_decl = trm_seq ~annot:(Some Heap_allocated) [
                trm_decl (Def_var ((new_var, typ_ptr (typ_var var_type)), trm_prim (Prim_new (typ_var var_type))));
                trm_set ~annot:(Some Initialisation_instruction) (trm_var new_var) (trm_apps ~annot:(Some Heap_allocated) (trm_unop (Unop_get)) [trm_var old_var] )
              ]
              in
              let new_set_old = trm_set (trm_var old_var) (trm_var new_var) in
              let new_del_inst = trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ())) ~is_statement:true (trm_unop (Unop_delete false)) [trm_var new_var] in


              let new_loop = trm_seq ~annot:(Some Delete_instructions) [trm_for init cond step (change_trm (trm_var old_var)(trm_var new_var) body);del_inst] in
              trm_seq ~annot:(Some Delete_instructions) [
                trm_seq ~annot:(Some No_braces) [
                  new_decl;new_loop;new_set_old
                ]; new_del_inst
              ]

            | _ -> fail t.loc "local_other_name_aux: expected a for loop"
            end
        | _ -> fail t.loc "local_other_name_aux: expected the sequnece which contains the for loop"
        end
      | _ -> fail t.loc "local_other_name_aux: expected the no brace sequence"


let local_other_name (clog : out_channel) (sec_of_int : label) (var_type : typvar) (old_var) (new_var : var) (t : trm) =
    let tr = [cLabel sec_of_int; cBody] in
    let b = !Flags.verbose in
    Flags.verbose := false;
    let epl = resolve_target tr t in
    Flags.verbose := b;
    match epl with
    | []->
      print_info t.loc "local_other_name: no matching subterm";
      t
    | _ -> List.fold_left
            (fun t dl ->
              apply_local_transformation (local_other_name_aux clog var_type old_var new_var) t dl)
              t
              epl


let delocalize_aux (clog : out_channel) (array_size : string) (neutral_element : int) (fold_operation : string) (t : trm) : trm =
  let rec list_replace_el (el : trm) (i : int) (list : trm list) : 'a list = match list with
| [] -> failwith "Empty list"
| x :: xs -> if i = 0 then el :: xs else x :: list_replace_el el (i-1) xs
  in
  let log : string =
      let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      (" -expression\n%s\n" ^^
      " %s section of interest \n"
      )
      (ast_to_string t) loc
      in write_log clog log;
      Ast_to_text.print_ast ~only_desc:true stdout t;
      match t.desc with
      | Trm_seq [no_brace;del_inst] ->
        begin match no_brace.desc with
        | Trm_seq tl ->
          let new_var = List.nth tl 0 in
            let old_var,new_var = match new_var.desc with
            | Trm_seq [_;t_assign] ->
              begin match t_assign.desc with
              | Trm_apps (_,[o_v;n_v]) ->
                let ov = match o_v.desc with
                | Trm_var x -> x
                | _ -> fail t.loc "delocalize_aux: expected a var"
                in
                let nv = begin match n_v.desc with
                  | Trm_apps (_,[base]) ->
                    begin match base.desc with
                    | Trm_var x -> x
                    | _ -> fail t.loc "delocalize_aux: expected a var"
                    end
                  | _ -> fail t.loc "delocalize_aux: expected a get operation"
                  end
                in nv ,ov
              | _ -> fail t.loc "delocalize_aux: expected a declaration with its initialisaition"
              end
            | _ -> fail t.loc "delocalize_aux"
          in
          let new_decl = [
            trm_seq ~annot:(Some Heap_allocated) [trm_decl (Def_var ((new_var,typ_ptr (typ_array (typ_var "T") (Trm (trm_var array_size)))),trm_prim (Prim_new (typ_int()))))];
            trm_set (trm_apps (trm_binop Binop_array_access) [trm_var new_var; trm_lit (Lit_int 0)]) (trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var old_var]);
            (* trm_set (trm_apps (trm_binop Binop_array_access)[trm_var new_var;trm_lit (Lit_int 0)]); *)
            trm_seq ~annot:(Some Delete_instructions)[
              trm_for
                (* init *)
                  (trm_seq ~annot:(Some Heap_allocated) [
                    trm_decl (Def_var (("k", typ_ptr (typ_int ()) ),
                          trm_prim (Prim_new (typ_int()))));
                    trm_set ~annot:(Some Initialisation_instruction) (trm_var "k") (trm_lit (Lit_int 0))
                  ])
                (* cond *)
                  (trm_apps (trm_binop Binop_lt)
                    [
                      trm_apps ~annot:(Some Heap_allocated)
                      (trm_unop Unop_get) [trm_var "k"];
                      (trm_var array_size)
                    ]
                  )
                (* step *)
                  ( trm_apps ( trm_unop Unop_inc) [trm_var "k"])
                (* body *)
                (trm_seq ~annot:(None)[
                  trm_set (trm_var old_var) (trm_lit (Lit_int 0))
                ]
                )

                ;
              trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit()))
                (trm_unop (Unop_delete false)) [trm_var "k"];
            ]
          ]
        in
        let for_loop = List.nth tl 1 in
        let new_for_loop = match for_loop.desc with
        | Trm_seq[f_loop;del_inst_f] ->
          begin match f_loop.desc with
          | Trm_for (init,cond,step,body) -> trm_seq ~annot:(Some Delete_instructions)
            [ trm_for init cond step
              (change_trm (trm_var new_var) (trm_apps (trm_binop Binop_array_access) [trm_var new_var;trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_any(trm_var "my_core_id")]]) body);
              del_inst_f
            ]
          | _ -> fail t.loc "delocalize_aux: expected a for loop"
          end
        | _ -> fail t.loc "delocalize_aux: expected a sequence which contains the for loop"
        in
        let operation = match fold_operation with
                | "+" -> Binop_add
                | "-" -> Binop_sub
                | "*" -> Binop_mul
                | "/" -> Binop_div
                | _ -> fail t.loc "delocalize_aux: this operation is not suported"
        in
        let accum = trm_seq ~annot:(Some No_braces) [
          trm_set (trm_var old_var) (trm_lit (Lit_int neutral_element));
          trm_seq ~annot:(Some Delete_instructions)[
            trm_for
              (* init *)
                (trm_seq ~annot:(Some Heap_allocated) [
                  trm_decl (Def_var (("k", typ_ptr (typ_int ()) ),
                        trm_prim (Prim_new (typ_int()))));
                  trm_set ~annot:(Some Initialisation_instruction) (trm_var "k") (trm_lit (Lit_int 0))
                ])
              (* cond *)
                (trm_apps (trm_binop Binop_lt)
                  [
                    trm_apps ~annot:(Some Heap_allocated)
                    (trm_unop Unop_get) [trm_var "k"];
                    (trm_var array_size)
                  ]
                )
              (* step *)
                ( trm_apps ( trm_unop Unop_inc) [trm_var "k"])
              (* body *)
              (trm_seq ~annot:(None)[

                trm_set ~annot:(Some App_and_set) (trm_var old_var)

                (trm_apps (trm_binop operation)
                  [
                    trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var old_var];
                    trm_apps (trm_binop Binop_array_access)[trm_var new_var;trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var "k"]]
                  ]
                )            ]
              )

              ;
            trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var "k"]
          ]
        ] (* TODO: discuss how to generate this using parsing of C code + substitution
             TODO: add smarter constructors for for-loops :  for_int_range i a b tbody *)
        in
        (* let tl = list_replace_el new_decl 0 tl in *)
        let tl = list_replace_el new_for_loop 1 tl in
        let tl = list_replace_el accum 2 tl in
        let tl = insert_sublist_in_list new_decl 0 tl in

        trm_seq  ~annot:(Some Delete_instructions) [trm_seq ~annot:(Some No_braces) tl; del_inst]
    | _ -> fail t.loc "delocalize_aux: expected the inner sequence which contains all the necessary terms"
    end
  | _ -> fail t.loc "delocalize_aux: expected the body of the section of interest"




let delocalize (clog : out_channel) (sec_of_int : label) (array_size : string) (neutral_element : int) (fold_operation : string) (t : trm) : trm =
  let tr = [cLabel sec_of_int;cBody] in
  let b = !Flags.verbose in
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
    print_info t.loc "delocalize: no matching subterm";
    t
  | _ -> List.fold_left
        (fun t dl ->
          apply_local_transformation (delocalize_aux clog array_size neutral_element fold_operation) t dl)
          t
          epl


let add_attribute (clog : out_channel) (a : attribute) (tr : target)
  (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  match epl with
  | [] ->
     print_info t.loc "add_attribute: no matching subterm";
     t
  | _ ->
     List.fold_left
       (apply_local_transformation
          (fun t ->
            let log : string =
              let loc : string =
                match t.loc with
                | None -> ""
                | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
              in
              Printf.sprintf
                ("  - expression\n%s\n" ^^
                 "    %sis a variable/type declaration\n"
                )
                (ast_to_string t) loc
            in
            write_log clog log;
            match t.desc with
            | Trm_decl (Def_var ((x, tx), init)) ->
               let ty_attributes = a :: tx.ty_attributes in
               {t with
                 desc = Trm_decl (Def_var ((x, {tx with ty_attributes}), init))}
            | Trm_decl (Def_typ (x, tx)) ->
               let ty_attributes = a :: tx.ty_attributes in
               {t with desc = Trm_decl (Def_typ (x, {tx with ty_attributes}))}
            | Trm_seq (t_decl :: tl) when t.annot = Some Heap_allocated ->
               begin match t_decl.desc with
               | Trm_decl (Def_var ((x, tx), init)) ->
                  begin match tx.ty_desc with
                  | Typ_ptr ty ->
                     let tx =
                       {tx with ty_desc =
                        Typ_ptr {ty with ty_attributes = a :: ty.ty_attributes}}
                     in
                     let t_decl =
                       {t_decl with desc = Trm_decl (Def_var ((x, tx), init))}
                     in
                     {t with desc = Trm_seq (t_decl :: tl)}
                  | _ -> assert false
                  end
               | _ -> assert false
               end
            | _ -> {t with attributes = a :: t.attributes}
          )
       )
       t epl

  (*
    transforma loop of the shape
    optional label:
    for(int i = 0; i < N;i += D)
      body
    into a loop of the form
    optional label:
    for (int c = 0; c < C; c++)
      for (int i = c*D; i < N; i += C*D)
      body
  *)
let undetach_expression_aux(clog : out_channel) (trm_index : int) (t : trm) : trm =
  let rec list_replace_el (el : trm) (i : int) (list : trm list) : 'a list = match list with
    | [] -> failwith "Empty list"
    | x :: xs -> if i = 0 then el :: xs else x :: list_replace_el el (i-1) xs
  in

  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      (" -expresssion\n%s\n" ^^
      "  %sis an assignment \n"
      )
      (ast_to_string t) loc
      in write_log clog log;
      match t.desc with
      | Trm_seq tl ->
        let t_decl = List.nth tl trm_index in
        let t_assgn = List.nth tl (trm_index + 1) in
        let t_assgn = {t_assgn with annot=(Some Initialisation_instruction)} in
        let t_decl = begin match t_decl.desc with
        | Trm_seq [var_decl] -> var_decl
        | _ -> fail t.loc "undelocalize_aux: expected the sequence which contain the declaration"
        end
        in
        let new_trm = trm_seq ~annot:(Some Heap_allocated) [t_decl;t_assgn] in
        let tl = list_remove_at (trm_index + 1) tl in
        let tl = list_replace_el new_trm trm_index tl in
        trm_seq ~annot:t.annot tl
      | _ -> fail t.loc "undelocalize_aux: expected the outer sequence"




let undetach_expression (clog :out_channel) (tr :target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  let app_transfo  (t : trm) (dl : path) : trm =
    match List.rev dl with
    | Dir_nth n :: dl' ->
      let dl = List.rev dl' in
      apply_local_transformation (undetach_expression_aux clog n ) t dl
    | _ -> fail t.loc "app_transfo: expected a dir_th inside the sequence"

  in
  match epl with
  | [] ->
    (* TODO: decide later whether the empty results should be treated as error *)
    print_info t.loc "detach_expression: no matching subterm";
    t
  | _ -> List.fold_left ( fun t dl -> app_transfo t dl)
    t epl


let detach_expression_aux (clog : out_channel) ?(keep_label : bool = false) (label : string) (trm_index : int) (expression_trm : trm)(t : trm) : trm =
  let log : string =
    let loc : string =
      match t.loc with
      | None -> ""
      | Some (_,start_row,end_row,start_column,end_column) -> Printf.sprintf  "at start_location %d  %d end location %d %d" start_row start_column end_row end_column
      in Printf.sprintf
      (" -expresssion\n%s\n" ^^
      "  %sis an assignment \n"
      )
      (ast_to_string t) loc
      in write_log clog log;
      match t.desc with
      | Trm_seq tl ->
        begin match expression_trm.desc with
        | Trm_seq[t_decl;t_assign] ->
          let t_decl = trm_seq ~annot:(Some Heap_allocated) [t_decl] in
          let t_assign = {t_assign with annot = None} in

          if keep_label then
            let trm_labelled = trm_labelled label t_assign in
            trm_seq ~annot:t.annot (insert_sublist_in_list [t_decl;trm_labelled] trm_index tl)

          else
            trm_seq ~annot:t.annot (insert_sublist_in_list [t_decl; t_assign] trm_index tl)

        | _ -> fail t.loc "detach_expression_aux:No trm was matched, please give the correct target "
        end
      | _ -> fail t.loc "detach_expression_aux: the outer sequence was not matched"




let detach_expression (clog :out_channel) ?(label : string = "detached") ?(keep_label : bool = false) (tr :target) (t : trm) : trm =
  let b = !Flags.verbose in
  Flags.verbose := false;
  let epl = resolve_target tr t in
  Flags.verbose := b;
  let app_transfo ?(keep_label : bool = false) (label : string) (t : trm) (dl : path) : trm =
    match List.rev dl with
    | Dir_nth n :: dl' ->
      let (t',_) = resolve_path dl t in
      let dl = List.rev dl' in
      apply_local_transformation (detach_expression_aux clog ~keep_label label n t') t dl
    | _ -> fail t.loc "app_transfo: expected a dir_th inside the sequence"

  in
  match epl with
  | [] ->
    (* TODO: decide later whether the empty results should be treated as error *)
    print_info t.loc "detach_expression: no matching subterm";
    t
  | _ -> List.fold_left ( fun t dl -> app_transfo ~keep_label label t dl)
    t epl
  (*|_ ->
    List.fold_left
      (fun t dl ->
        let index, prefix_sequence_trm =
        begin match List.rev dl with
        | Dir_nth n :: dl' ->
          let dl = List.rev dl' in
          let (t',_) = resolve_target dl t in n, t'
        | _ -> fail t.loc " detach_expression: expected a dir_nth inside the sequence"
        end in
        apply_local_transformation (detach_expression_aux clog ~keep_label label index prefix_sequence_trm) t dl)
        t
        epl  *)





(* Create an instance of the pattern *)
(* let pattern_instantiate (t : trm) (p : pat) : instatiation option =
  let rec aux p t =
      match p, t with
      | var x, _ ->
         if Fmap.mem x !inst then begin
            if same_trm (Fmap.get x !inst) t
               then ()
               else raise Mismatch
         end else
            inst := Fmap.add x t !inst
         end
      | trm_if p0 p1 p2, trm_if t0 t1 t2 ->
         aux p0 t0;
         aux p1 t1;
         aux p2 t2
      | trm_for (int i = ..)   | trm_for (int j = ...) => LATER: support this, not now
      | _, _ -> (* different constructors *)
  in
  try Some(aux p t) with Mismatch -> None
 *)



(* Check if rule is applicable *)
(* let is_rule_applicable (t : trm) (p : pat) : bool =
 let rec aux (t : trm) : bool =
  match t.desc with
  | Trm *)


(* Rewrite rule transformation  *)
(* let rewrite (pl : target) (rule : base)  *)
