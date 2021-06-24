open Ast
open Target
open Tools
(* INTERNAL FUNCTIONS *)
(* *********************************************** *)
(*replace occurrences of t_before with t_after in t.
  paths point at subterms in which all occurences will be replaced.
  the empty path means all occurences will be replaced (default behaviour).
  assumption: t_before and t_after are equivalent (in terms of value and of side
  effects)
 *)

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
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
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
    (* necessary because of annotations in trms that may be different *)
    (* TODO: replace
        Ast_to_c.typ_to_string ty = Ast_to_c.typ_to_string ty_before
      with a call to the comparison funciton --> to move to ast.ml
        let rec same_types (mustMatchGeneratedStar : bool) (tya : typ) (tyb : typ) : bool =
          let aux = same_types mustMatchGeneratedStar in
          (typa.typ_annot = typb.typ_annot) &&
          match tya, tyb with
          | Typ_ptr tya1, Typ_ptr tyb1 ->
              (if mustMatchGeneratedStar
                then (is_generated_star tya = is_generated_star tyb)
                else true)
              && (aux tya1 tyb1)
          | Typ_int, Typ_int -> true
          | ...
          | _,_ -> false (* if different constructors *)

    *)
    if Ast_to_c.typ_to_string ty = Ast_to_c.typ_to_string ty_before then ty_after
    else Ast.typ_map change_typ ty
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
      let b = !Flags.verbose in
      Flags.verbose := false;
      let epl = resolve_target tr t' in
      Flags.verbose := b;
      match epl with
      | [] ->
         print_info t'.loc "change_typ: no matching subterm for target %s\n"
           (target_to_string tr);
         t'
      | _ -> List.fold_left (apply_on_path apply_change) t' epl
    )
    t

    change_at

(*
  insert inert after the subterm pointed at by dl in t
  assumption: dl points at a seq element, thus ends with Dir_nth n
  if the inserted element must be first in the seq, use n < 0
 *)
let insert_trm_after (dl : path) (insert : trm) (t : trm) : trm =
  let dl' = List.rev dl in
  match List.hd dl' with
  | Dir_nth n ->
     apply_on_path
       (fun t' ->
         match t'.desc with
         | Trm_seq tl ->
            trm_seq ~annot:t'.annot ~loc:t'.loc ~add:t'.add
              ~attributes:t'.attributes (Tools.list_insert n insert tl)
         | _ -> fail t'.loc "insert_trm_after: path points at wrong term"
       )
       t
       (List.rev (List.tl dl'))
  | _ -> fail t.loc "insert_trm_after: bad path"

(* make sure each occurence of y in t is marked with type variable x *)
let rec replace_type_with (x : typvar) (y : var) (t : trm) : trm =
  match t.desc with
  | Trm_var y' when y' = y ->
     trm_var ~annot:t.annot ~loc:t.loc ~add:t.add ~typ:(Some (typ_var x))
       ~attributes:t.attributes y
  | _ -> trm_map (replace_type_with x y) t


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
    | Trm_seq tl (* when t.annot <> Some Delete_instructions *) ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (clean_up_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t


(* [isolate_last_dir_in_seq dl]:
    params:
      dl: explicit path to the targeted trm
    return:
      a pair of the explicit path to the outer sequence and the index of the term inside that sequence
*)
let isolate_last_dir_in_seq (dl : path) : path * int =
  match List.rev dl with
  | Dir_nth i :: dl' -> (List.rev dl',i)
  | _ -> fail None "isolate_last_dir_in_seq: the transformation expects a target on an element that belongs to a sequence"
  (* LATER: raise an exception that each transformation could catch OR take as argument a custom error message *)

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

let eliminate_goto_next (t : trm) : trm =
  let rec elim_in_list (tl : trm list) : trm list =
    match tl with
    | t1 :: t2 :: tl ->
       begin match t1.desc, t2.desc with
       | Trm_goto l1, Trm_labelled (l2, _) when l1 = l2 ->
          elim_in_list (t2 :: tl)
       | _ -> t1 :: (elim_in_list (t2 :: tl))
       end
    | _ -> tl
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (elim_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t
(* TODO: Change this based on Arthurs'idea *)
let group_decl_init (t : trm) : trm =
  let rec group_in_list (tl : trm list) : trm list =
    match tl with
    | t1 :: t2 :: tl ->
       begin match t1.desc, t2.desc with
       | Trm_seq [{desc = Trm_let (Var_mutable,(x, tx), dx); _}],
         Trm_apps ({desc = Trm_val (Val_prim (Prim_binop Binop_set)); _},
                   [{desc = Trm_var _; _}; _])
             (* when y = x && t1.annot = Some Heap_allocated *) ->
          let t =
            trm_let ~loc:t1.loc Var_mutable (x, tx) dx
          in
          group_in_list (t :: tl)
       | _ -> t1 :: (group_in_list (t2 :: tl))
       end
    | _ -> tl
  in
  let rec aux (t : trm) : trm =
    match t.desc with
    | Trm_seq tl ->
       trm_seq ~annot:t.annot ~loc:t.loc ~add:t.add ~attributes:t.attributes
         (group_in_list (List.map aux tl))
    | _ -> trm_map aux t
  in
  aux t


let parse_cstring (is_expression : bool) (s : string) : trm list =
 let ast =
    Clang.Ast.parse_string
      (Printf.sprintf
         {|
          void f(void){
            #pragma clang diagnostic ignored "-Wunused-value"
            %s
          }
          |}
         (if is_expression then s ^ ";" else s)
      )
  in
  let t = Clang_to_ast.translate_ast ast in
  Ast_to_text.print_ast ~only_desc:true stdout t;
  match t.desc with
  | Trm_seq [t] ->
     begin match t.desc with
     | Trm_seq[fun_def] ->
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

(*
  let parse_cstring (is_expression:bool) (s:string) : trm =
  let ast =
    Clang.Ast.parse_string
      (Printf.sprintf
         {|
          void f(void){
            #pragma clang diagnostic ignored "-Wunused-value"
            %s
          }
          |}
         (if is_expression then s ^ ";" else s)
      )
    in
    match ast with
    | trm_seq main
      | trm_let_fun
        | trm_seq ts ->
            if not is_expression then ts else
            match ts with
            | [t] -> t


  --> [term s] takes a C expression without semi-column at the end,
      and returns the corresponding term (an expression)
  let term (s : string) : trm =
    parse_cstring true s

  --> [stats s] takes a string, e.g. "int x = 5; x++; while(true){x++;}"
  and it should return a list of terms, each of them being an instruction.

  let stats (s : string) : trm list =
    parse_cstring false s

*)

(* Get the sat of a C/C++ trm entered as a string *)
let term (s : string) : trm =
  let tl = parse_cstring true s in
  match tl with
  | [expr] -> expr
  | _ -> fail None "term: expcted a list with only one element"

let stats (s : string) : trm list =
  parse_cstring false s

(*
  aliased_type X takes as argument the description of a file
  (that is a toplevel sequence), and it returns the type ty
  associated via a "typedef ty X" if there is one such definition
  LATER: check if this is subsumed by the environments carried by type variables
 *)
let rec aliased_type (x : typvar) (t : trm) : typ option =
  match t.desc with
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_alias ty when td.typdef_tconstr = x -> Some ty
    | _ -> None
    end
  | Trm_seq tl ->
     List.fold_left
       (fun tyo t ->
         match tyo with
         | Some _ -> tyo
         | None -> aliased_type x t
       )
       None
       tl
  | _ -> None


let get_field_list (td : typedef) : var list =
  begin match td.typdef_body with
  | Typdef_prod (_, s) -> fst (List.split s)
  | _ -> fail None "get_field_lists: expected a Typedef_prod"
  end



let get_typid (t : trm) : int =
  let trm_typ =
  begin match t.typ with
  | Some typ ->
      typ
  | None -> fail t.loc "get_typid: no type was found"
  end
  in
  match t.desc with
  | Trm_apps (_,[_])
  | Trm_struct _ | Trm_var _ ->
    begin match trm_typ.typ_desc with
    | Typ_constr(_,id,_) -> id
    | _ -> fail t.loc "get_typid: expected a user defined type"
    end

  | _ -> -1
(* ********************************************** *)






(* [var_init_detach_aux t]: This is an auxiliary function for var_init_detach
    params:
      t: an ast subterm
    return:
      a sequence which contains the declaration of the variable and a set operations for that variable
*)
let var_init_detach_aux (t : trm) : trm =
  match t.desc with
  | Trm_let(vk,(x, tx), init) ->
    begin match vk with
    | Var_immutable -> fail t.loc "var_init_detach_aux: const declarations cannot be detached"
    | _ ->
      let init =
        begin match init.desc with
        | Trm_apps(_,[init]) -> init
        | _ -> fail t.loc "var_init_detach_aux: expected a heap allocated variable declaration"
        end in
      trm_seq [
        trm_let vk (x, tx) (trm_prim (Prim_new tx));
        trm_set (trm_var x) init
      ]
    end
  | _ -> fail t.loc "var_init_detach_aux: variable could not be matched, make sure your path is correct"


let var_init_detach : Target.Transfo.local =
  Target.apply_on_path(var_init_detach_aux )

(* [var_init_attach_aux t]: This is an auxiliary function for var_init_attach
    params:
      t: an ast subterm
    return
      the updated
*)
let var_init_attach_aux (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    (* Assumption: The sequence is of the form
      {
        int x;
        x = 5;
      }
    *)
    let var_decl = List.nth tl 0 in
    let var_set = List.nth tl 1 in
    let var_kind, var_name, var_type = begin match var_decl.desc with
    | Trm_let (vk,(x,tx),_) -> vk, x, tx
    | _ -> fail t.loc "var_init_attach_aux: sequence does not satisfy the assumption described above"
    end
    in
    let var_init = begin match var_set.desc with
    | Trm_apps(_, [_;init]) -> init
    | _ -> fail t.loc "var_init_attach_aux: sequence does not satisfy the assumtion that the second term of the sequence is the set operations"
    end
    in
    trm_let ~loc:t.loc var_kind (var_name, var_type) (trm_apps (trm_prim ~loc:t.loc (Prim_new var_type)) [var_init])
  | _ -> fail t.loc "var_init_attach_aux: sequence was not matched, make sure the path is correct"

(* [var_init_attach t]: Change a sequence of the form {int x; x = 5;} to int x = 5
    params:
      path_to_seq: path to the sequence which satisfy the assumtion above
      t: ast
    return
      the updated ast
*)
let var_init_attach : Target.Transfo.local =
  Target.apply_on_path(var_init_attach_aux)


(* [const_non_const_aux t]: This is an auxiliary function for const_non_const
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let const_non_const_aux (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x,tx), init) ->
    begin match vk with
     (* If variable is a constant than whe remove the const and we perform the heap allocation  *)
    | Var_immutable ->
      trm_let Var_mutable (x, typ_ptr ~typ_attributes:[GeneratedStar] tx) (trm_apps (trm_prim ~loc: t.loc (Prim_new tx)) [init])
    | _ ->
      let var_type = begin match tx.typ_desc with
      | Typ_ptr t -> t
      | _ -> fail t.loc "const_non_const_aux: expected a pointer type"
      end
      in
      let var_init = begin match init.desc with
      | Trm_apps(_, [_; init]) -> init
      | _ -> fail t.loc "const_non_const_aux: expected a something of the form 'new ()'"
      end
      in
      trm_let Var_immutable (x,var_type) var_init
    end
  | _ -> fail t.loc "const_non_const_aux: variable declaration was not matched, make sure the path is correct"

let const_non_const : Target.Transfo.local =
  apply_on_path (const_non_const_aux)


(* [remove_instruction_aux t]: This is an auxiliary function for remove_instruction
    params:
      t: an ast subterm
    return:
      the updated ast
*)
let remove_instruction_aux (_t : trm) : trm =
  (* Replace the current t with an empty sequence *)
  trm_seq ~annot:(Some No_braces) []

let remove_instruction : Target.Transfo.local=
  apply_on_path (remove_instruction_aux)

(* [local_other_name var_type old_var new_var t]: This is an auxiliary function for local_other_name
    params:
      var_type: type of the var
      old_var: old variable for which we want to chang the local name
      new_var: new_variable
      t: an ast subterm
    return:
      the updated ast
*)
let local_other_name_aux (var_type : typvar) (old_var : var) (new_var : var) (t : trm) : trm =
    begin match t.desc with
    | Trm_seq [no_braces] ->
      begin match no_braces.desc with
        | Trm_seq [f_loop] ->
          begin match f_loop.desc with
          | Trm_for (init, cond, step, body) ->
            let new_type = typ_var var_type  in
            let new_decl = trm_let Var_mutable (new_var, new_type) (trm_apps (trm_prim (Prim_new new_type)) [trm_var old_var])

            in
            let new_set_old = trm_set (trm_var old_var) (trm_var new_var) in
            (* let new_del_inst = trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ())) ~is_statement:true (trm_unop (Unop_delete false)) [trm_var new_var] in *)
            let new_loop = trm_for init cond step (change_trm (trm_var old_var)(trm_var new_var) body) in


              trm_seq (* ~annot:(Some No_braces) *) [
                new_decl;new_loop;new_set_old
              ]
          | _ -> fail t.loc "local_other_name_aux: expected a for loop"
          end
      | _ -> fail t.loc "local_other_name_aux: expected the sequnece which contains the for loop"
      end
    | _ -> fail t.loc "local_other_name_aux: expected the no brace sequence"
    end

let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.local =
  Target.apply_on_path(local_other_name_aux var_type old_var new_var)

(* [delocalize_aux array_size neutral_element fold_operation t]: This is an auxiliary function for deloclize
    params:
      array_size: the size of the array we want to create
      neutral_element: nutral element for reduction phase
      fold_operation: fold_operation for reduction phase
      t: ast subterm
    return:
      the updated ast
*)

(* [insert_trm_aux insert_where t_insert t]: This is an auxiliary function for insert_trm
    params:
      insert_where: a string equal to before or after describing the location where shoudl we insert the trm
      t_insert: the trm to be inserted
      t: the trm around which the trm is going to be inserted
    return:
      the updated ast
*)
let insert_trm_aux (t_insert : trm) (index : int) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let tl = Tools.list_insert (index) t_insert tl in
    trm_seq ~annot:t.annot tl
  | _ -> fail t.loc "insert_trm_aux: expected the outer sequence"

let insert_trm (t_insert : trm) (index : int) : Target.Transfo.local =
  Target.apply_on_path(insert_trm_aux t_insert index)


(* [delocalize_aux array_size neutral_element fold_operation t]: This is an auxiliary function for delocalize
    params:
      array_size: size of the array
      neutral_element: neutral element needed for the final reduction
      fold_operation: a string representing the fold operation needed for the final reduction
    return:
      the updated ast
*)
let delocalize_aux (array_size : string) (neutral_element : int) (fold_operation : string) (t : trm) : trm =
  match t.desc with
  | Trm_seq tl ->
    let def = List.hd tl in
    let vk,new_var,old_var =
    begin match def.desc with
    | Trm_let (vk,(x, _),init) ->
      begin match init.desc with
      | Trm_apps(_, [base]) ->
        begin match base.desc with
        | Trm_apps (_, [base1]) ->
          begin match base1.desc with
          | Trm_var y -> (vk,y, x)
          | _ -> fail t.loc "delocalize_aux: expected a variable"
          end
        | Trm_var y -> (vk,y,x)
        | _ -> fail t.loc "delocalize_aux: expected a get or a simple variable"
        end

      | Trm_var y -> (vk,y, x)
      | _ -> fail t.loc "delocalize_aux: expected something"
      end
    | _ -> fail t.loc "delocalize_aux: expected a varaible declaration"
    end
    in
    let new_decl = trm_seq ~annot:(Some No_braces)[
      trm_let vk (new_var, typ_ptr ~typ_attributes:[GeneratedStar] (typ_array (typ_var "T" ) (Trm (trm_var array_size)))) (trm_prim (Prim_new (typ_array (typ_var "T") (Trm (trm_var array_size)))));
      trm_for
      (* init *)
        (trm_let Var_mutable ("k",typ_ptr ~typ_attributes:[GeneratedStar] (typ_int ())) (trm_apps (trm_prim  (Prim_new (typ_int ()))) [trm_lit (Lit_int 0)]))
      (* cond *)
        (trm_apps (trm_binop Binop_lt)
          [

            trm_var "k";
            trm_apps ~annot:(Some Mutable_var_get)
            (trm_unop Unop_get) [trm_var "k"]
          ]
        )
      (* step *)
        ( trm_apps ( trm_unop Unop_inc) [trm_var "k"])
      (* body *)
      (trm_seq ~annot:(None)[
        trm_set (trm_var old_var) (trm_lit (Lit_int 0))
      ]
      )]
    in
    let for_loop = List.nth tl 1 in
    let parallel_for =  begin match for_loop.desc  with
    | Trm_for(init, cond, step, body) ->
      trm_for init cond step
        (
          change_trm (trm_var new_var) (trm_apps (trm_binop Binop_array_access) [trm_var new_var; trm_apps ~annot:(Some Mutable_var_get) (trm_unop Unop_get) [trm_any (trm_var "my_core_id")]]) body
        )
    | _ -> fail t.loc "delocalize_aux: expected a for loop"
    end
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
      trm_for
        (* init *)
        (trm_let Var_mutable ("k", typ_ptr ~typ_attributes:[GeneratedStar] (typ_int ())) (trm_apps (trm_prim (Prim_new (typ_int ()))) [trm_lit (Lit_int 0)]))
        (* cond *)
        (trm_apps (trm_binop Binop_lt) [
          trm_apps ~annot:(Some Mutable_var_get)
          (trm_unop Unop_get) [trm_var "k"];
          (trm_var array_size)
        ])
        (* step *)
        ( trm_apps ( trm_unop Unop_inc) [trm_var "k"])
        (* body *)
        (trm_seq [
          trm_set ~annot:(Some App_and_set) (trm_var old_var)
          (trm_apps (trm_binop operation)
            [
              (* trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var old_var]; *)
              trm_var old_var;
              (* trm_apps (trm_binop Binop_array_access)[trm_var new_var;trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var "k"]] *)
              trm_apps (trm_binop Binop_array_access)[trm_var new_var; trm_var "k"]
            ]
          ) ])

    ]
    in trm_seq ([new_decl] @ [parallel_for] @ [accum])

  | _ -> fail t.loc "delocalize_aux: expected the nobrace sequence"

  let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.local =
    Target.apply_on_path (delocalize_aux array_size neutral_element fold_operation)



(* [add_attribute_aux a t]: This is an auxiliary function for add_attribute
    params:
      a: attribute  which is going to be added
      t: an ast subterm
    return:
      the updated ast
*)
let add_attribute_aux (a : attribute) (t : trm) : trm =
  match t.desc with
  | Trm_let (vk, (x, tx), init) ->
    let typ_attributes = a :: tx.typ_attributes in
    trm_let vk (x, {tx with typ_attributes}) init
  | Trm_typedef td ->
    begin match td.typdef_body with
    | Typdef_alias tx ->
      let typ_attributes = a :: tx.typ_attributes in
      trm_typedef {td with typdef_body = Typdef_alias {tx with typ_attributes}}
    | _ -> fail t.loc "add_attribute_aux: expected a typdef_alias"
    end

  | _ ->  {t with attributes = a :: t.attributes}


let add_attribute (a : attribute) : Target.Transfo.local =
  Target.apply_on_path(add_attribute_aux a)


let ast_show_aux (file : string) (to_stdout:bool) (index : int) (t : trm) : trm =
  let out_ast = open_out file in
  if to_stdout then begin
    Ast_to_text.print_ast ~only_desc:true stdout t;
    output_string stdout "\n\n ";
    end
  else
    output_string out_ast (Printf.sprintf "=========================Occurence %i======================\n" index);
    Ast_to_text.print_ast ~only_desc:true out_ast t;
    output_string out_ast "\n\n";
    output_string out_ast (Printf.sprintf "------------------------Occurence %i details---------------\n" index);
    Ast_to_text.print_ast ~only_desc:false out_ast t;
    output_string out_ast "\n\n";
    t





let ast_show (file : string) (to_stdout : bool) (index : int): Target.Transfo.local =
  Target.apply_on_path (ast_show_aux file to_stdout index)