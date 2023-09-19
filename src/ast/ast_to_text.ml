open PPrint
open Ast
open Trm
open Typ
open Mark
open Tools


(*  Note: This module is used mainly for debugging purposes.
    The ast is printed in such a way so that it mimics its actual OCaml structure.
    All the ast components are first converted to pprint document.
    [only_desc] to print only the description field an avoid the other ones *)

(* [print_typ_desc only_desc t]: converts type descriptions to pprint document *)
let rec print_typ_desc ?(only_desc : bool = false) (t : typ_desc) : document =
  match t with
  | Typ_const t ->
    let dt = print_typ ~only_desc t in
    print_node "Typ_const" ^^ dt
  | Typ_var (name, tid) ->
    print_node "Typ_var" ^^ parens (separate (comma ^^ break 1) [string name; string (string_of_int tid)])
  | Typ_constr (tc, tid, tl) ->
    let tv_d = print_typconstr tc in
    let tl = List.map (print_typ ~only_desc) tl in
    print_node "Typ_constr" ^^ parens ( separate (comma ^^ break 1)
      [tv_d; string (string_of_int tid); print_list tl])
  | Typ_auto -> string "Typ_auto"
  | Typ_unit -> string "Typ_unit"
  | Typ_int -> string "Typ_int"
  | Typ_float -> string "Typ_float"
  | Typ_double -> string "Typ_double"
  | Typ_bool -> string "Typ_bool"
  | Typ_char -> string "Typ_char"
  | Typ_string -> string "Typ_string"
  | Typ_ptr {ptr_kind = pk; inner_typ = ty} ->
     let dpk = begin match pk with
               | Ptr_kind_mut -> string "pointer"
               | Ptr_kind_ref -> string "reference"
               end
      in
     let dt = print_typ ~only_desc ty in
     print_node "Typ_ptr" ^^ parens (dpk) ^^dt
  | Typ_array (t, s) ->
     let dt = print_typ ~only_desc t in
     let ds =
       begin match s with
       | Undefined -> underscore
       | Const n -> string (string_of_int n)
       | Trm t' -> print_trm ~only_desc t'
       end
     in
     print_node "Typ_array" ^^ print_pair dt ds
  | Typ_fun (tl, t) ->
     let dtl = List.map (print_typ ~only_desc) tl in
     let dt = print_typ ~only_desc t in
     print_node "Typ_fun" ^^ parens (print_list dtl ^^ comma ^/^ dt)
  | Typ_record (rt, name) ->
    let dt = print_typ ~only_desc name in
    let drt = print_record_type rt in
    print_node "Typ_record" ^^ parens (drt ^^ comma ^^ blank 1 ^^ dt)
  | Typ_template_param name ->
    print_node "Typ_template_param" ^^ parens (string name)
  | Typ_arbitrary s -> print_node "Typ_arbitrary " ^^ parens (string (code_to_str s))
  | Typ_decl e -> print_node "Typ_decl " ^^ parens (print_trm ~only_desc e)

(* [print_typ_annot a]: converts type annotations to pprint document *)
and print_typ_annot (a : typ_annot) : document =
  match a with
  | Unsigned -> string "Unsigned"
  | Long -> string "Long"
  | Short -> string "Short"

(* [print_typ ~only_desc t]: converts type records to pprint document *)
and print_typ ?(only_desc : bool = false) (t : typ) : document =
  let ddesc = print_typ_desc ~only_desc t.typ_desc in
  if only_desc then ddesc
  else
    let dannot =
      List.fold_left (fun d a -> print_typ_annot a ^^ blank 1 ^^ d) underscore
        t.typ_annot
    in
    let dattr =
      print_list (List.map (print_attribute ~only_desc) t.typ_attributes)
    in
    braces (separate (blank 1) [string "annot"; equals;
                                dannot ^^ semi ^//^ string "desc"; equals;
                                ddesc ^^ semi ^//^ string "attributes"; equals;
                                dattr])
(* [print_unop ~only_desc op]: converts unary operators to pprint document *)
and print_unop ?(only_desc : bool = false) (op : unary_op) : document =
  match op with
  | Unop_get -> string "Unop_get"
  | Unop_address -> string "Unop_address"
  | Unop_neg -> string "Unop_neg"
  | Unop_bitwise_neg -> string "Unop_bitwise_neg"
  | Unop_minus -> string "Unop_minus"
  | Unop_plus -> string "Unop_plus"
  | Unop_post_inc -> string "Unop_post_inc"
  | Unop_post_dec -> string "Unop_post_dec"
  | Unop_pre_inc -> string "Unop_pre_inc"
  | Unop_pre_dec -> string "Unop_pre_dec"
  | Unop_struct_access f -> print_node "Unop_struct_access" ^^ string f
  | Unop_struct_get f -> print_node "Unop_struct_get" ^^ string f
  | Unop_cast t ->
     let dt = print_typ ~only_desc t in
     print_node "Unop_cast" ^^ dt

(* [print_binop]: converts binary operators to pprint document *)
and print_binop (op : binary_op) : document =
  match op with
  | Binop_set -> string "Binop_set"
  | Binop_array_access -> string "Binop_array_access"
  | Binop_array_get -> string "Binop_array_get"
  | Binop_eq -> string "Binop_eq"
  | Binop_neq -> string "Binop_neq"
  | Binop_sub -> string "Binop_sub"
  | Binop_add -> string "Binop_add"
  | Binop_mul -> string "Binop_mul"
  | Binop_mod -> string "Binop_mod"
  | Binop_div -> string "Binop_div"
  | Binop_exact_div -> string "Binop_exact_div"
  | Binop_le -> string "Binop_le"
  | Binop_lt -> string "Binop_lt"
  | Binop_ge -> string "Binop_ge"
  | Binop_gt -> string "Binop_gt"
  | Binop_and -> string "Binop_and"
  | Binop_bitwise_and -> string "Binop_bitwise_and"
  | Binop_or -> string "Binop_or"
  | Binop_bitwise_or -> string "Binop_bitwise_or"
  | Binop_shiftl -> string "Binop_shiftl"
  | Binop_shiftr -> string "Binop_shiftr"
  | Binop_xor -> string "Binop_xor"
  (*| Binop_fmod -> string "Binop_fmod"*)

(* [print_consistency cm]: converts OpenMP memory consistency model to pprint document *)
and print_consistency (cm : consistency_mode) : document =
  match cm with
  | Sequentially_consistent -> string "Sequentially_consistent"
  | Release -> string "Release"
  | Acquire -> string "Acquire"

(* [print_primt ~only_desc p]: converts primitive operatios to pprint document *)
and print_prim ?(only_desc : bool = false) (p : prim) : document =
  match p with
  | Prim_unop op ->
     let dop = print_unop ~only_desc op in
     print_node "Prim_unop" ^^ dop
  | Prim_binop op ->
     let dop = print_binop op in
     print_node "Prim_binop" ^^ dop
  | Prim_compound_assgn_op op ->
    let dop = print_binop op in
    print_node "Prim_compound_assgn_op" ^^ dop
  | Prim_overloaded_op p ->
    let dp = print_prim p in
    print_node "Prim_overloaded_op" ^^ dp
  | Prim_new t ->
     let dt = print_typ ~only_desc t in
     print_node "Prim_new" ^^ dt
  | Prim_conditional_op -> print_node "Prim_conditional_op"

(* [print_lit l]: converts literals to pprint document *)
and print_lit (l : lit) : document =
  match l with
  | Lit_unit -> string "Lit_unit"
  | Lit_uninitialized -> string "Lit_uninitialized"
  | Lit_bool b -> print_node "Lit_bool" ^^ string (string_of_bool b)
  | Lit_int n -> print_node "Lit_int" ^^ string (string_of_int n)
  | Lit_double f -> print_node "Lit_double" ^^ string (string_of_float f)
  | Lit_string s ->
     print_node "Lit_string" ^^ dquotes (separate (backslash ^^ string "n") (lines s))
  | Lit_nullptr -> print_node "Lit_nullptr"

(* [print_val ~only_desc v]: converts values to pprint document *)
and print_val ?(only_desc : bool = false) (v : value) : document =
  match v with
  | Val_lit l ->
     let dl = print_lit l in
     print_node "Val_lit" ^^ parens dl
  | Val_prim p ->
     let dp = print_prim ~only_desc p in
     print_node "Val_prim" ^^ parens dp


(* [print_attribute ~only_desc a]: converts attribute [a] to pprint document *)
and print_attribute ?(only_desc : bool = false) (a : attribute) : document =
  match a with
  | Alignas t ->
     string "Alignas" ^^ blank 1 ^^ print_trm ~only_desc t
  | GeneratedTyp ->
    string "GeneratedTyp" ^^ blank 1
  | Injected -> string "Injected class type"  ^^ blank 1
  | Others -> empty

(* [print_var]: converts [v] into a docuemnt. *)
and print_var (v : var) : document =
  (concat_map (fun q -> string q ^^ string "::") v.qualifier) ^^
  string v.name ^^ string "#" ^^ string (string_of_int v.id)

and print_typconstr ((qualifier, name) : typconstr) : document =
  (concat_map (fun q -> string q ^^ string "::") qualifier) ^^
  string name

(* [print_trm_desc ~only_desc t]: converts the description of trm [t] to pprint document *)
and print_trm_desc ?(only_desc : bool = false) (t : trm_desc) : document =
  match t with
  | Trm_val v ->
     let dv = print_val ~only_desc v in
     print_node "Trm_val" ^^ parens dv
  | Trm_var (vk, v) ->
    let var_kind_str = match vk with | Var_immutable -> string "Var_immutable" | Var_mutable -> string "Var_mutable" in
    let v_d = print_var v in
    string "Trm_var(" ^^ blank 1 ^^ var_kind_str ^^ comma ^^ v_d ^^ rparen
  | Trm_array tl ->
     let tl = Mlist.to_list tl in
     let dtl = List.map (print_trm ~only_desc) tl in
     print_node "Trm_array" ^^ print_list dtl
  | Trm_record tl ->
     let tl = Mlist.to_list tl in
     let dtl = List.map (fun (lb, t) ->
      let td = print_trm ~only_desc t in
      match lb with Some lb -> parens (string lb ^^ comma ^^blank 1 ^^ td) | None -> td) tl in
     print_node "Trm_record" ^^ print_list dtl
  | Trm_let (vk,(x,tx),t) ->
    let dvk = match vk with
    | Var_immutable -> string "Var_immutable"
    | Var_mutable ->  string "Var_mutable"

    in
    let dtx = print_typ ~only_desc tx in
    let dt = print_trm ~only_desc t in
    print_node "Trm_let" ^^
      parens (separate (comma ^^ break 1) [dvk; print_var x; dtx; dt])
  | Trm_let_mult (vk, tvl, tl) ->
    let dvk = match vk with
    | Var_mutable -> string "Var_mutable"
    | Var_immutable -> string "Var_immutable"
      in
    let dtx = List.map (fun (x, ty) -> parens (print_var x ^^ comma ^^ print_typ ~only_desc:true ty)) tvl in
    let dts = List.map (fun t -> print_trm ~only_desc t) tl in
    let dtl = List.map2 (fun v t ->  parens (v ^^ comma ^^ t)) dtx dts in
    let dtx = Tools.list_to_doc ~sep:comma dtx in
    print_node "Trm_let_mult" ^^
      parens (separate (comma ^^ break 1)
        [dvk; dtx; print_list dtl;])
  | Trm_let_fun (f, r, tvl, b, _) ->
    let dout = print_typ ~only_desc r in
    let dtvl = List.map(function (x,tx) ->
          let dtx = print_typ ~only_desc tx in
          print_pair (print_var x) dtx) tvl in
    let dt = print_trm ~only_desc b in
    let fd = print_var f in
    print_node "Trm_let_fun" ^^
      parens (separate (comma ^^ break 1)
        [fd; dout; print_list dtvl; dt])
  | Trm_typedef td -> print_typedef ~only_desc td
  | Trm_if (c, t, e) ->
     let dc = print_trm ~only_desc c in
     let dt = print_trm ~only_desc t in
     let de = print_trm ~only_desc e in
     print_node "Trm_if" ^^ parens (separate (comma ^^ break 1) [dc; dt; de])
  | Trm_seq tl ->
     let dtl = List.map (print_trm ~only_desc) (Mlist.to_list tl) in
     print_node "Trm_seq" ^^ print_list dtl
  | Trm_apps (f, tl, _) ->
     let df = print_trm ~only_desc f in
     let dtl = List.map (print_trm ~only_desc) tl in
     print_node "Trm_apps" ^^ parens (df ^^ comma ^/^ print_list dtl)
  | Trm_while (c, b) ->
     let dc = print_trm ~only_desc c in
     let db = print_trm ~only_desc b in
     print_node "Trm_while" ^^ parens (dc ^^ comma ^/^ db)
  | Trm_do_while (b, c) ->
     let db = print_trm ~only_desc b in
     let dc = print_trm ~only_desc c in
     print_node "Trm_do_while" ^^ parens (db ^^ comma ^/^ dc)

  | Trm_for_c (init, cond, step, body, _) ->
     let dinit = print_trm ~only_desc init in
     let dcond = print_trm ~only_desc cond in
     let dstep = print_trm ~only_desc step in
     let dbody = print_trm ~only_desc body in
     print_node "Trm_for" ^^ parens (separate (comma ^^ break 1)
       [dinit; dcond; dstep; dbody])
  | Trm_for (l_range, body, _) ->
    let (index, start, direction, stop, step, is_parallel) = l_range in
    let dstart = print_trm ~only_desc start in
    let dstop = print_trm ~only_desc stop in
    let ddir  = match direction with
    | DirUp -> string "Up"
    | DirDown -> string "Down"
    | DirUpEq -> string "UpEq"
    | DirDownEq -> string "DownEq"
    in
    let dstep = match step with
    | Post_inc -> string "Post_inc"
    | Post_dec -> string "Post_dec"
    | Pre_inc -> string "Pre_inc"
    | Pre_dec -> string "Pre_dec"
    | Step st -> string "Step " ^^ parens (print_trm ~only_desc st)
    in
    let dparallel = string (string_of_bool is_parallel) in
    let dbody = print_trm ~only_desc body in
    print_node "Trm_for" ^^ parens (separate (comma ^^ break 1)
      [print_var index; dstart; ddir; dstop; dstep; dparallel; dbody])
  | Trm_switch (cond, cases) ->
     let dcond = print_trm ~only_desc cond in
     let dcases =
       List.map
         (fun (tl, body) ->
           let dtl = List.map (print_trm ~only_desc) tl in
           let dbody = print_trm ~only_desc body in
           print_pair (print_list dtl) dbody
         )
         cases
     in
     print_node "Trm_switch" ^^ print_pair dcond (print_list dcases)
  | Trm_abort a ->
     let da =
       begin match a with
       | Ret t_o ->
          begin match t_o with
          | None -> print_node "Ret" ^^ underscore
          | Some t ->
             let dt = print_trm ~only_desc t in
             print_node "Ret" ^^ dt
          end
       | Break lb_opt ->
          begin match lb_opt with
          | Some lb -> string "Break" ^^ blank 1 ^^ string lb
          | None -> string "Break"
          end
       | Continue lb_opt->
          begin match lb_opt with
          | Some lb -> string "Continue" ^^ blank 1 ^^ string lb
          | None -> string "Continue"
          end
       end in
     print_node "Trm_abort" ^^ parens da
  | Trm_goto l ->
     print_node "Trm_goto" ^^ string l
  | Trm_arbitrary s ->
    let code_str = code_to_str s in
    print_node "Trm_arbitrary" ^^ parens (string code_str)
  | Trm_omp_routine routine->
    print_node "Trm_omp_routine" ^^ parens (print_routine routine)
  | Trm_extern (lang, decls) ->
    let dtl = List.map (print_trm ~only_desc) decls in
    print_node "Trm_extern" ^^ parens (string lang ^^ comma ^^ print_list dtl)
  | Trm_namespace (name, dcls, inline) ->
    let dt = print_trm ~only_desc dcls in
    print_node "Trm_namespace" ^^ parens (separate (comma ^^ break 1)
      [string name; string (string_of_bool inline); dt])
  | Trm_template _ ->  print_node "Trm_template _"
  | Trm_using_directive str -> print_node "Trm_using_directive " ^^ string str
  | Trm_fun (tvl , ty_opt, b, _) ->
    let dtout = begin match ty_opt with | Some ty -> string "Some " ^^ print_typ ~only_desc ty | None -> string "None" end in
    let dtvl = List.map(function (x,tx) ->
          let dtx = print_typ ~only_desc tx in
          print_pair (print_var x) dtx) tvl in
    let dt = print_trm ~only_desc b in
    print_node "Trm_fun" ^^
      parens (separate (comma ^^ break 1)
        [print_list dtvl; dtout; dt])
  | Trm_delete (b, t1) ->
    let bd = string (string_of_bool b) in
    let td = print_trm ~only_desc t1  in
    print_node "Trm_delete"  ^^
      parens (separate (comma ^^ break 1)
        [bd; td])

(* [print_record_type rt]: converts record types to pprint document *)
and print_record_type (rt : record_type) : document =
  match rt with
  | Struct -> string "struct"
  | Union -> string "union"
  | Class -> string "class"

(* [print_typedef ~only_desc td]: converts typedef to pprint document *)
and print_typedef ?(only_desc : bool = false) (td : typedef) : document =
  let tconstr = td.typdef_tconstr in
  let tbody = td.typdef_body in

  match tbody with
  | Typdef_alias t ->
    let dt = print_typ ~only_desc t in
    print_node "Typedef_alias" ^^ parens ( separate (comma ^^ break 1)
     [string tconstr; dt ])
  | Typdef_record rfl ->
    let get_document_list (rfl : record_fields) : document list =
      let rec aux acc = function
      | [] -> acc
      | (rf, _) :: tl ->
        begin match rf with
        | Record_field_member (lb, ty) ->
          let dt = print_typ ~only_desc ty in
          aux (print_pair (string lb) dt :: acc) tl
        | Record_field_method t1 ->
          let dt = print_trm ~only_desc t1 in
          aux (dt :: acc) tl
        end
         in aux [] rfl
      in
      let dtl = get_document_list rfl in
     print_node "Typedef_prod" ^^ parens ( separate (comma ^^ break 1)
      [string tconstr; print_list dtl ])
  | Typdef_sum _ ->
    fail None "Ast_to_text.print_typedef: sum types are not supported in C/C++"
  | Typdef_enum enum_const_l ->
     let denum_const_l =
       print_list
         (List.map
            (fun (y, t_o) ->
              match t_o with
              | None -> print_pair (print_var y) underscore
              | Some t -> print_pair (print_var y) (print_trm ~only_desc t)
            )
            enum_const_l
         )
     in
     print_node "Typedef_enum" ^^ print_pair (string tconstr) denum_const_l

and print_trm_annot (t : trm) : document =

  let t_attributes = trm_get_attr t in
  let dattr = print_list (List.map (print_attribute ) t_attributes) in

  let t_marks = trm_get_marks t in
  let dmarks = print_list (List.map string t_marks) in

  let dstringrepr = begin match trm_get_stringreprid t with | Some id -> string "Some " ^^ string (string_of_int id) | None -> string "None" end in
  let t_pragmas = trm_get_pragmas t in
  let t_pragmas_str = List.map print_directive t_pragmas in
  let dpragmas = print_list t_pragmas_str in

  let cstyle_annot = trm_get_cstyles t in
  let cstyle_annot_str = List.map print_cstyle_annot cstyle_annot in
  let dcstyle = print_list cstyle_annot_str in

  let files_annot = trm_get_files_annot t in
  let dfiles_str = List.map print_files_annot files_annot in
  let dfiles = print_list dfiles_str in
  braces (separate (blank 1) [string "trm_annot_attributes"; equals;
    dattr ^^ semi ^//^ string "trm_annot_marks"; equals;
    dmarks ^^ semi ^//^ string "trm_annot_stringrepr"; equals;
    dstringrepr ^^ semi ^//^ string "trm_annot_pragma"; equals;
    dpragmas ^^ semi ^//^ string "trm_annot_cstyle"; equals;
    dcstyle ^^ semi ^//^ string "trm_annot_files"; equals;
    dfiles])


(* [print_trm ~only_desc t]: converts trm [t] to pprint document *)
and print_trm ?(only_desc : bool = false) (t : trm) : document =
  let ddesc = print_trm_desc ~only_desc t.desc in
  if only_desc then ddesc
    else
      let dannot = print_trm_annot t in

      let dloc =
        begin match t.loc with
        | None -> underscore
        | Some {loc_file = filename; loc_start = {pos_line = start_row; pos_col = start_column}; loc_end = {pos_line = end_row; pos_col = end_column}} ->
           print_pair (string filename) (string (string_of_int start_row ^ "," ^ string_of_int start_column ^ ": " ^ string_of_int end_row ^ "," ^ string_of_int end_column) )
        end
      in
      let dinstr = string (string_of_bool t.is_statement) in

      let dtyp =
        match t.typ with
        | None -> underscore
        | Some ty -> print_typ ~only_desc ty
      in

      let opt_str c o = if o = None then "-" else c in
      let dctx =
        String.concat ""
            [ opt_str "t" t.ctx.ctx_types;
              opt_str "b" t.ctx.ctx_resources_before;
              opt_str "u" t.ctx.ctx_resources_usage;
              opt_str "c" t.ctx.ctx_resources_contract_invoc;
              opt_str "a" t.ctx.ctx_resources_after;
              opt_str "p" t.ctx.ctx_resources_post_inst] in

      braces (separate (blank 1)
        [string "annot"; equals; dannot ^^ semi ^//^
         string "loc"; equals; dloc ^^ semi ^//^
         string "is_statement"; equals; dinstr ^^ semi ^//^
         (*LATER: string "add"; equals;*)
         string "typ"; equals; dtyp ^//^
         string "ctx"; equals; string dctx ^^ semi ^//^
         string "desc"; equals; ddesc ])

(* [print_files_annot ann]: prints as string files annotation [ann] *)
and print_files_annot (ann : files_annot) : document =
  match ann with
  | Include s -> string ("Include " ^ s)
  | Main_file -> string "Main_file"


(* [print_constructor_kind ck]: prints constructor kinds. *)
and print_constructor_kind (ck : constructor_kind) : document =
  match ck with
  | Constructor_implicit -> string "Constructor_implicit"
  | Constructor_explicit -> string "Constructor_explicit"
  | Constructor_default -> string "Constructor_default"
  | Constructor_simpl -> string "Constructor_simpl"


(* [print_destructor_kind dk]: prints destructor kinds. *)
and print_destructor_kind (dk : destructor_kind) : document =
  match dk with
  | Destructor_default -> string "Destructor_default"
  | Destructor_delete -> string "Destructor_delete"
  | Destructor_simpl -> string "Destructor_simpld"

(* [print_cstyle_annot ann]: prints as string cstyle annotation [ann]. *)
and print_cstyle_annot (ann : cstyle_annot) : document =
 match ann with
 | Display_no_arrow -> string "Display_no_arrow"
 | Empty_cond -> string "Empty_cond"
 | Fun_inline -> string "Fun_inline"
 | No_braces id -> string ("No_braces " ^ string_of_int id)
 | Multi_decl -> string "Multi_decl"
 | Postfix_set -> string "Postfix_set"
 | Reference -> string "Reference"
 | Stackvar -> string "Stackvar"
 | Is_struct -> string "Is_struct"
 | Is_rec_struct -> string "Is_rec_struct"
 | Is_class -> string "Is_class"
 | Static_fun -> string "Static"
 | Method_call -> string "Method_call"
 | Implicit_this -> string "Implicit_this"
 | Typ_arguments tyl -> string "Typ_arguments " ^^ list_to_doc ~bounds:[langle; rangle] (List.map print_typ tyl)
 | Implicit_constructor -> string "Implicit_constructor"
 | Explicit_constructor -> string "Explicit_constructor"
 | Default_constructor -> string "Default_constructor"
 | Const_method -> string "Const_method"
 | Method -> string "Method"
 | Constructed_init -> string "Constructed_init"
 | Class_constructor ck -> print_constructor_kind ck
 | Class_destructor dk -> print_destructor_kind dk
 | Member_initializer -> string "Member_initializer"
 | Redundant_decl -> string "Redundant_decl"
 | Brace_init -> string "Brace_init"
 | Clang_cursor _ -> string "Clang_cursor"
 | Display_null_uppercase -> string "Display_null_uppercase"
 | GhostCall -> string "GhostCall"
 | ResourceFormula -> string "ResourceFormula"

(* [print_atomic_operation ao]: converts OpenMP atomic operations to pprint document *)
and print_atomic_operation (ao : atomic_operation option) : document =
  match ao with
  | None -> empty
  | Some ao1 ->
    begin match ao1 with
    | Read -> string "Read"
    | Write -> string "Write"
    | Update -> string "Update"
    | Capture -> string "Capture"
    end

(* [print_directive directive]: converts OpenMP atomic directives to pprint document *)
and print_directive (directive : directive) : document =
  match directive with
  | Atomic ao -> string "Atomic" ^^ blank 1 ^^ print_atomic_operation ao
  | Atomic_capture -> string "Atomic_capture"
  | Barrier -> string "Barrier"
  | Cancel _ -> string "Cancel"
  | Cancellation_point _-> string "Cancellation_point"
  | Critical _ -> string "Critical"
  | Declare_simd _ -> string "Declare_simd"
  | Declare_reduction _ -> string "Declare_reduction"
  | Declare_target _ -> string "Declare_target"
  | Distribute _ -> string "Distribute"
  | Distribute_parallel_for _ -> string "Distribute_parallel_for"
  | Distribute_parallel_for_simd _ -> string "Distribute_parallel_for_simd"
  | Distribute_simd -> string "Distribute_simd"
  | End_declare_target -> string "End_declare_target"
  | Flush _ -> string "Flush"
  | For _ -> string "For"
  | For_simd _ -> string "For_simd"
  | Master -> string "Master"
  | Ordered _ -> string "Ordered"
  | Parallel _ -> string "Parallel"
  | Parallel_for _ -> string "Parallel_for"
  | Parallel_for_simd _ -> string "Parallel_for_simd"
  | Parallel_sections _ -> string "Parallel_sections"
  | Section -> string "Section"
  | Sections _ -> string "Sections"
  | Simd _ -> string "Simd"
  | Single _ -> string "Single"
  | Target _ -> string "Target"
  | Target_data _ -> string "Target_data"
  | Target_enter_data _ -> string "Target_enter_data"
  | Target_exit_data _ -> string "Target_exit_data"
  | Target_teams _ -> string "Target_teams"
  | Target_teams_distribute _ -> string "Target_teams_distribute"
  | Target_teams_distribute_parallel_for _ -> string "Target_teams_distribute_parallel_for"
  | Target_teams_distribute_parallel_for_simd _ -> string "Target_teams_distribute_parallel_for_simd"
  | Target_teams_distribute_simd _ -> string "Target_teams_distribute_simd"
  | Target_update _ -> string "Target_update"
  | Task _ -> string "Task"
  | Taskgroup -> string "Taskgroup"
  | Taskloop _ -> string "Taskloop"
  | Taskloop_simd _ -> string "Taskloop_simd"
  | Taskwait _ -> string "Taskwait"
  | Taskyield -> string "Taskyield"
  | Teams _ -> string "Teams"
  | Teams_distribute _ -> string "Teams_distribute"
  | Teams_distribute_end _ -> string "Teams_distribute_end"
  | Teams_distribute_parallel_for _ -> string "Teams_distribute_parallel_for"
  | Teams_distribute_parallel_for_simd _ -> string "Teams_distribute_parallel_for_simd"
  | Threadprivate _ -> string "Threadprivate"

(* [print_routine routine]: converts OpenMP routine to pprint document *)
and print_routine (routine : omp_routine) : document =
  match routine with
  | Set_num_threads _ -> string "Set_num_threads"
  | Get_num_threads -> string "Get_num_threads"
  | Get_max_threads -> string "Get_max_threads"
  | Get_thread_num -> string "Get_thread_num"
  | Get_num_procs -> string "Get_num_procs"
  | In_parallel -> string "In_parallel"
  | Set_dynamic _ -> string "Set_dynamic"
  | Get_dynamic -> string "Get_dynamic"
  | Get_cancellation -> string "Get_cancellation"
  | Set_nested _ -> string "Set_nested"
  | Get_nested -> string "Get_nested"
  | Set_schedule _ -> string "Set_schedule"
  | Get_schedule _ -> string "Get_schedule"
  | Get_thread_limit -> string "Get_thread_limit"
  | Set_max_active_levels _ -> string "Set_max_active_levels"
  | Get_max_active_levels -> string "Get_max_active_levels"
  | Get_level -> string "Get_level"
  | Get_ancestor_thread_num -> string "Get_ancestor_thread_num"
  | Get_team_size _ -> string "Get_team_size"
  | Get_active_level -> string "Get_active_level"
  | In_final -> string "In_final"
  | Get_proc_bind -> string "Get_proc_bind"
  | Set_default_device _ -> string "Set_default_device"
  | Get_default_device -> string "Get_default_device"
  | Get_num_devices -> string "Get_num_devices"
  | Get_num_teams -> string "Get_num_teams"
  | Get_team_num -> string "Get_team_num"
  | Is_initial_device -> string "Is_initial_device"
  | Init_lock _ -> string "Init_lock"
  | Init_nest_lock _ -> string "Init_nest_lock"
  | Destroy_lock _ -> string "Destroy_lock"
  | Destroy_nest_lock _ -> string "Destroy_nest_lock"
  | Set_lock _ -> string "Set_lock"
  | Set_nest_lock _ -> string "Set_nest_lock"
  | Unset_lock _ -> string "Unset_lock"
  | Unset_nest_lock _ -> string "Unset_nest_lock"
  | Test_lock _ -> string "Test_lock"
  | Test_nest_lock _ -> string "Test_nest_lock"
  | Get_wtime -> string "Get_wtime"
  | Get_wtick -> string "Get_wtick"


(* [print_ast ~only_desc out t]: prints the ast [t] with the full information or not depending on [only_desc] flag,
      on an outer channel [out] *)
let print_ast ?(only_desc : bool = false) (out : out_channel) (t : trm) : unit =
  let d = print_trm ~only_desc t in
  ToChannel.pretty 0.9 80 out d

(* [ast_to_file filename t]: prints ast [t] into file [filename] *)
let ast_to_file (filename : string) (t : trm) : unit =
  let out = open_out filename in
  print_ast out t;
  close_out out

(* [ast_to_string ~only_desc t]: converts ast [t] to a string *)
let ast_to_string ?(only_desc : bool = false) (t : trm) : string =
  let d = print_trm ~only_desc t in
  document_to_string d

(* [typedef_to_string ~only_desc td]: converts typdef [td] to a string *)
let typedef_to_string ?(only_desc : bool = false) (td : typedef) : string =
  let d = print_typedef ~only_desc td in
  document_to_string d

(* [typ_to_string ~only_desc t]: converts type [t] to a string  *)
let typ_to_string ?(only_desc : bool = false) (t : typ) : string =
  let d = print_typ ~only_desc t in
  document_to_string d

(* [typ_option_to_string ~only_desc t]: converts an optional type [t] to a string  *)
let typ_option_to_string ?(only_desc : bool = false) (t : typ option) : string =
  match t with
  | None -> "<notype>"
  | Some ty -> typ_to_string ~only_desc ty
