open Ast
open Ast_to_text
open Output
open Target
open Tools




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


let delocalize (array_size : string) (neutral_element : int) (fold_operation : string) : Target.Transfo.t =
  Target.apply_on_target (Generic_core.delocalize array_size neutral_element fold_operation)


let add_atribute(a : attribute) : Transfo.t =
  Target.apply_on_target (Generic_core.add_attribute a)

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