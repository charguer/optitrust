open Ast
open Ast_to_text
open Output
open Target
open Tools
open Path_constructors




let var_init_detach : Target.Transfo.t =
  Target.apply_on_target ( Generic_core.var_init_detach)

let var_init_atttach : Target.Transfo.t =
  Target.apply_on_target (Generic_core.var_init_attach)

let const_non_const : Target.Transfo.t =
  Target.apply_on_target (Generic_core.const_non_const)

let remove_instruction : Target.Transfo.t =
  Target.apply_on_target (Generic_core.remove_instruction)

let remove_instructions (tgs : target list) : unit =
  List.fold_left(fun () x ->
      remove_instruction x
    ) () tgs
  
let local_other_name (var_type : typvar) (old_var : var) (new_var : var) : Target.Transfo.t = 
  Target.apply_on_target (Generic_core.local_other_name var_type old_var new_var)


(* This one used special smart constructors like cBefore and cAfter instead of giving target_befoer or target_after *)
let insert_trm_new  (t_insert : trm) (tg : target) : unit =
  Target.apply_on_transformed_targets(Generic_core.isolate_last_dir_in_seq)
    (fun (p,i) t -> Generic_core.insert_trm t_insert i t p) tg











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
            
            (* TODO: Fix this later *)
            
            (* trm_seq ~annot:(Some Heap_allocated) [trm_decl (Def_var ((new_var,typ_ptr (typ_array (typ_var "T") (Trm (trm_var array_size)))),trm_prim (Prim_new (typ_int()))))]; *)
            (* trm_set (trm_apps (trm_binop Binop_array_access) [trm_var new_var; trm_lit (Lit_int 0)]) (trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var old_var]); *)
            (* trm_set (trm_apps (trm_binop Binop_array_access)[trm_var new_var;trm_lit (Lit_int 0)]); *)
            trm_seq (* ~annot:(Some Delete_instructions) *)[
              trm_for
                (* init *)
                  (trm_let Var_mutable ("k",typ_ptr (typ_int ())) (trm_apps (trm_prim  (Prim_new (typ_int ()))) [trm_lit (Lit_int 0)]))
                (* cond *)
                  (trm_apps (trm_binop Binop_lt)
                    [
                      (* trm_apps ~annot:(Some Heap_allocated)
                      (trm_unop Unop_get) [trm_var "k"]; *)
                      trm_var "k";
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
              (* trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit()))
                (trm_unop (Unop_delete false)) [trm_var "k"] *)
            ]
          ]
        in
        let for_loop = List.nth tl 1 in
        let new_for_loop = match for_loop.desc with
        | Trm_seq[f_loop;del_inst_f] ->
          begin match f_loop.desc with
          | Trm_for (init,cond,step,body) -> trm_seq (* ~annot:(Some Delete_instructions) *)
            [ trm_for init cond step
              (Generic_core.change_trm (trm_var new_var) (trm_apps (trm_binop Binop_array_access) [trm_var new_var;trm_apps (* ~annot:(Some Heap_allocated) *) (trm_unop Unop_get) [trm_any(trm_var "my_core_id")]]) body);
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
          trm_seq (* ~annot:(Some Delete_instructions) *)[
            trm_for
              (* init *)
                (trm_let Var_mutable ("k", typ_int()) (trm_lit (Lit_int 0)))
                
              (* cond *)
                (trm_apps (trm_binop Binop_lt)
                  [
                    (* trm_apps ~annot:(Some Heap_allocated)
                    (trm_unop Unop_get) [trm_var "k"]; *)
                    trm_var "k";
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
                    (* trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var old_var]; *)
                    trm_var old_var;
                    (* trm_apps (trm_binop Binop_array_access)[trm_var new_var;trm_apps ~annot:(Some Heap_allocated) (trm_unop Unop_get) [trm_var "k"]] *)
                    trm_apps (trm_binop Binop_array_access)[trm_var new_var; trm_var "k"]
                  ]
                )            ]
              )

              ;
            (* trm_apps ~annot:(Some Heap_allocated) ~typ:(Some (typ_unit ()))
                (trm_unop (Unop_delete false)) [trm_var "k"] *)
          ]
        ] (* TODO: discuss how to generate this using parsing of C code + substitution
             TODO: add smarter constructors for for-loops :  for_int_range i a b tbody *)
        in
        (* let tl = list_replace_el new_decl 0 tl in *)
        let tl = list_replace_el new_for_loop 1 tl in
        let tl = list_replace_el accum 2 tl in
        let tl = insert_sublist_in_list new_decl 0 tl in

        trm_seq (*  ~annot:(Some Delete_instructions) *) [trm_seq ~annot:(Some No_braces) tl; del_inst]
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
          apply_on_path (delocalize_aux clog array_size neutral_element fold_operation) t dl)
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
       (apply_on_path
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
             | Trm_let (vk, (x, tx), init) ->
               let ty_attributes = a :: tx.ty_attributes in
               {t with
                 desc = Trm_let (vk,(x, {tx with ty_attributes}), init)}
            | Trm_typedef (Typedef_abbrev (x, tx)) ->
               let ty_attributes = a :: tx.ty_attributes in
               {t with desc = Trm_typedef (Typedef_abbrev (x, {tx with ty_attributes}))}
            (* | Trm_seq (t_decl :: tl) when t.annot = Some Heap_allocated ->
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
               end *)
            | _ -> {t with attributes = a :: t.attributes}
          )
       )
       t epl


(* TODO: Remove this function after dealing with all the transformations which use this function *)
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
            | [], _ :: _ -> Generic_core.insert_trm_after dl t_inserted t'
            (* insert before: replace n with n - 1 *)
            | _ :: _, [] ->
               Generic_core.insert_trm_after (List.rev (Dir_nth (n - 1) :: dl')) t_inserted
                 t'
            | [], [] ->
               fail t'.loc "insert_trm: please specify an insertion point"
            | _ -> fail t'.loc "insert_trm: cannot insert both before and after"
            end
         | _ -> fail t'.loc "insert_trm: bad insertion target"
       )
       t
       epl
(* This transformations is used only for debugging purposes *)
(* ********************************************************* *)
let left_decoration (index:int):string  = "/*@" ^ string_of_int index ^ "<*/"

let right_decoration (index:int):string  = "/*>" ^ string_of_int index ^ "@*/"


(* TODO: debug_path : bool = false
  as argument,
   when turned on, you should do List.iter (fun p -> printf (path_to_string p)) epl
  *)
let show_target ?(debug_ast : bool = false) (tr : target) (t : trm) : trm =
  let epl = resolve_target tr t in
   (* DEBUG *)(*  printf "%s\n" (list_to_string (List.map path_to_string epl));*)
  match epl with
  | [] -> (* TODO: remove this warning *)
    print_info t.loc "show_target: no matching subterm\n";
    t
  | [dl] -> if debug_ast then Ast_to_text.print_ast ~only_desc:true stdout t;
            apply_on_path (trm_decoration (left_decoration 0) (right_decoration 0) ) t dl

  | _ -> foldi
          (fun i -> if debug_ast then Ast_to_text.print_ast ~only_desc:true stdout t;
                    apply_on_path
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
        fun i -> apply_on_path(fun t ->
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

let rec delete_target_decorators (t : trm) : trm =
  match t.desc with
  | Trm_decoration (_,t',_) -> t'
  | _ -> trm_map (delete_target_decorators ) t

(* ********************************************************* *)
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