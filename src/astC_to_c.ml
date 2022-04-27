open PPrint
open Ast
open Precedence


(* only for internal use *)
let print_optitrust_syntax = ref false

(* only for internal use *)
let print_commented_pragma = ref false

(* only for debugging purposes *)
let print_stringreprids = ref false

(*----------------------------------------------------------------------------------*)
(* An optional memoization table that maps a [stringreprid] of a term
   to the [document] obtained by executing [trm_to_doc] on it.
   If [stringreprs] is not [None], the stringreprs table is filled in
   during the calls to [trm_to_doc], for terms that bear a [Annot_stringreprid].
   Make sure to invoke [label_subterms_with_fresh_stringreprids] on the
   AST being printed, and preferably to reinitialize the stringreprs
   table before starting. *)

type stringreprs = (Ast.stringreprid, document) Hashtbl.t

let stringreprs : stringreprs option ref = ref None

let clear_stringreprs () =
  stringreprs := None

let get_stringreprs () : stringreprs =
  match !stringreprs with
  | Some m -> m
  | None -> fail None "get_stringreprs: must call init_stringreprs or set_stringreprs first"

let get_and_clear_stringreprs () : stringreprs =
  let m = get_stringreprs() in
  stringreprs := None;
  m

let set_stringreprs (t : stringreprs) : unit =
  stringreprs := Some t

let init_stringreprs () =
  stringreprs := Some (Hashtbl.create 100)

let add_stringreprs_entry (t : trm) (d : document) : unit =
  match !stringreprs with
  | None -> ()
  | Some m ->
      match trm_get_stringreprid t with
      | None -> ()
      | Some id -> Hashtbl.add m id d

(* for debugging purpose *)
let print_stringreprs (m : stringreprs) : unit =
  let pr id d =
    Printf.printf "stringreprs[%d] = %s\n----\n" id (Tools.document_to_string d) in
  Printf.printf "====<stringreprs>====\n";
  Hashtbl.iter pr m;
  Printf.printf "====</stringreprs>====\n"

(*----------------------------------------------------------------------------------*)

(* translate an ast to a C/C++ document *)
let rec typ_desc_to_doc (t : typ_desc) : document =
  match t with
  (* LATER: probably more general
      | Typ_const t when is_base_type t -> print const before t
      | Typ_const t _ -> print const after t

      where is_base_type is for int float etc... *)
  | Typ_const t when is_typ_ptr t ->
    typ_to_doc t ^^ string " const"
  | Typ_const t -> string " const "  ^^ typ_to_doc t
  | Typ_constr (tv, _, _) -> string tv
  | Typ_auto  -> string "auto"
  | Typ_unit -> string "void"
  | Typ_int -> string "int"
  | Typ_float -> string "float"
  | Typ_double -> string "double"
  | Typ_bool -> string "bool"
  | Typ_char -> string "char"
  | Typ_string -> string "string"
  | Typ_ptr { ptr_kind = pk; inner_typ = t} ->
    begin match pk with
    | Ptr_kind_mut ->
      typ_to_doc t ^^ star
    | Ptr_kind_ref -> typ_to_doc t ^^ ampersand
    end
  | Typ_array (t, s) ->
     let d = typ_to_doc t in
     begin match s with
     | Undefined -> d ^^ brackets empty
     | Const n -> d ^^ brackets (string (string_of_int n))
     | Trm t' -> d ^^ brackets (decorate_trm t')
     end
  | Typ_fun _ ->
     print_info None "typ_desc_to_doc: typ_fun not implemented\n";
     at
  | Typ_var (t, _) -> string t
  | Typ_record (rt, n) ->
    let d = typ_to_doc n in
    let drt = record_type_to_doc rt in
    drt ^^ blank 1 ^^ d
  | Typ_template_param n ->
    string n
  | Typ_arbitrary a_kind ->
        begin match a_kind with
        | Atyp ty -> string ty
        | _ -> fail None "typ_to_doc: arbitrary types entered as string should be entered by using Atyp"
        end

and typ_annot_to_doc (a : typ_annot) : document =
  match a with
  | Unsigned -> string "unsigned"
  | Long -> string "long"
  | Short -> string "short"

and trm_annot_to_doc (t_annot : trm_annot list) : document =
  let aux t_annot = match t_annot with
  | Multi_decl -> string "Multi_dec"
  | App_and_set -> string "App_and_set"
  | Main_file -> string "Main_file"
  | Postfix_set -> string "Postfix_set"
  | Annot_stringreprid id -> string "Annot_stringreprid" ^^ string (string_of_int id)
  | _ -> empty
  in
  if t_annot = [] then empty else
  Tools.list_to_doc ~sep:comma (List.map aux t_annot)

and typ_to_doc (t : typ) : document =
  let d = typ_desc_to_doc t.typ_desc in
  let dannot =
    List.fold_left (fun d' a -> typ_annot_to_doc a ^^ blank 1 ^^ d') empty
      t.typ_annot
  in
  let dattr =
    match t.typ_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  dattr ^^ dannot ^^ d

and typed_var_to_doc (tx : typed_var) : document =
  let (x, ty) = tx in
  let is_const = is_typ_const ty in
  let const_string = if is_const then blank 1 ^^ string " const " ^^ blank 1 else empty in
  let rec aux (t : typ) (s : size) : document * document list =
    let ds =
      match s with
      | Undefined -> brackets empty
      | Const n -> brackets (string (string_of_int n))
      | Trm t' -> brackets (decorate_trm t')
    in
    match t.typ_desc with
    | Typ_array (t, s') ->
       let (base, bracketl) = aux t s' in
       (base, ds :: bracketl)
    | _ ->
       (typ_to_doc t, [ds])
  in
  let dattr =
    match ty.typ_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  let ty = get_inner_const_type ty in
  match ty.typ_desc with
  | Typ_array (t, s) ->
     let (base, bracketl) = aux t s in
     dattr ^^ const_string ^^ base ^^ blank 1 ^^ string x ^^ concat bracketl
  | Typ_fun (tyl, ty) ->
    let ret_type = typ_to_doc ty in
    let arg_types = List.map typ_to_doc tyl in
    dattr ^^ ret_type ^^ blank 1 ^^ string x ^^ (Tools.list_to_doc ~sep:comma ~bounds:[lparen; rparen] arg_types)
  | Typ_ptr _ ->
     dattr ^^ typ_to_doc ty ^^ blank 1 ^^ const_string ^^ string x
  | _ -> const_string ^^ typ_to_doc ty ^^ blank 1 ^^ string x

and lit_to_doc (l : lit) : document =
  match l with
  | Lit_unit -> semi
  | Lit_uninitialized -> empty
  | Lit_bool b -> string (string_of_bool b)
  | Lit_int i -> string (string_of_int i)
  | Lit_double f -> string (string_of_float f)
  | Lit_string s -> dquotes (separate (backslash ^^ string "n") (lines s))

and unop_to_doc (op : unary_op) : document =
  match op with
  | Unop_get -> star
  | Unop_address -> ampersand
  | Unop_neg -> bang
  | Unop_bitwise_neg -> tilde
  | Unop_minus -> minus
  | Unop_plus -> plus
  | Unop_post_inc | Unop_pre_inc -> twice plus
  | Unop_post_dec | Unop_pre_dec -> twice minus
  | Unop_struct_access s -> dot ^^ string s
  | Unop_struct_get s -> dot ^^ string s
  | Unop_cast t ->
     let dt = typ_to_doc t in
     string "static_cast" ^^ langle ^^ dt ^^ rangle

and binop_to_doc (op : binary_op) : document =
  match op with
  | Binop_set -> equals
  | Binop_array_access -> lbracket ^^ rbracket
  | Binop_array_get -> lbracket ^^ rbracket
  | Binop_eq -> twice equals
  | Binop_neq -> bang ^^ equals
  | Binop_sub -> minus
  | Binop_add -> plus
  | Binop_mul -> star
  | Binop_mod -> percent
  | Binop_div -> slash
  | Binop_le -> langle ^^ equals
  | Binop_lt -> langle
  | Binop_ge -> rangle ^^ equals
  | Binop_gt -> rangle
  | Binop_and -> twice ampersand
  | Binop_bitwise_and -> ampersand
  | Binop_or -> twice bar
  | Binop_bitwise_or -> bar
  | Binop_shiftl -> twice langle
  | Binop_shiftr -> twice rangle
  | Binop_xor -> caret

and prim_to_doc (p : prim) : document =
  match p with
  | Prim_unop op -> unop_to_doc op
  | Prim_binop op -> binop_to_doc op
  | Prim_compound_assgn_op op -> (binop_to_doc op) ^^ equals
  | Prim_overloaded_op p -> prim_to_doc p
  | Prim_new t -> string "new" ^^ blank 1 ^^ typ_to_doc t
  | Prim_conditional_op ->
     (* put holes to display the operator *)
     separate (blank 1) [underscore; qmark; underscore; colon; underscore]

and val_to_doc (v : value) : document =
  match v with
  | Val_lit l -> lit_to_doc l
  | Val_ptr l ->
     if l = 0 then string "NULL"
     else
       begin
         print_info None "val_to_doc: pointers not implemented\n";
         at
       end
  | Val_prim p -> prim_to_doc p

and attr_to_doc (a : attribute) : document =
  match a with
  (* | Alignas t -> string "_Alignas" ^^ parens (decorate_trm t) *)
  | Alignas t -> string "alignas" ^^ parens (decorate_trm t)
  | GeneratedTyp -> blank 1
  | Others -> empty

and decorate_trm ?(semicolon : bool = false) ?(prec : int = 0) ?(print_struct_init_type : bool = true) (t : trm) : document =
  let parentheses = parentheses_needed ~prec t in
  let dt = trm_to_doc ~semicolon ~prec ~print_struct_init_type t in
  (* LATER: if Flags.print_trm_addresses then (string (sprintf "%p" t) ^ dt) else dt *)
  let dt = if parentheses then parens (dt) else dt in
  if t.marks = [] && not !print_stringreprids
    then dt
    else
      begin
      let sid =
        if not !print_stringreprids then "" else begin
        match Ast.trm_get_stringreprid t with
        | None -> "[-]"
        | Some id -> Printf.sprintf "[%d]" id
        end in
      let m = Tools.list_to_string ~sep:"," ~bounds:["";""] t.marks in
      let sleft = string ("/*@" ^ sid ^ m ^ "*/") in
      let sright =  string ("/*" ^ sid ^ m ^ "@*/") in
      sleft ^^ dt ^^ sright
      end

and trm_to_doc ?(semicolon=false) ?(prec : int = 0) ?(print_struct_init_type : bool = false)  (t : trm) : document =
  let loc = t.loc in
  let dsemi = if semicolon then semi else empty in
  let dattr =
    match t.attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
    in
  let d =
    begin match t.desc with
    | Trm_val v ->
       if List.mem Empty_cond t.annot then empty
         else dattr ^^ val_to_doc v
    | Trm_var (_, x) ->
           dattr ^^ string x
    | Trm_array tl -> let tl = Mlist.to_list tl in
       let dl = List.map (decorate_trm ~semicolon ~print_struct_init_type:false) tl in
       dattr ^^ braces (separate (comma ^^ blank 1) dl)
    | Trm_struct tl ->
       let tl = Mlist.to_list tl in
       let dl = List.map (decorate_trm ~print_struct_init_type:false ~semicolon) tl in
       let init_type = if not print_struct_init_type then empty else
        begin match t.typ with
        | Some ty ->
          begin match ty.typ_desc with
          | Typ_constr (_, id, _) when id <> -1 -> parens(typ_to_doc ty)
          | _ -> empty
          end
        | None -> empty
        end in
       dattr ^^ init_type ^^ blank 1 ^^  braces (separate (comma ^^ blank 1) dl)
    | Trm_let (_,tx,t) -> dattr ^^ trm_let_to_doc ~semicolon tx t
    | Trm_let_mult (_, ty, tv, tl) -> dattr ^^ trm_let_mult_to_doc ~semicolon ty tv tl
    | Trm_let_fun (f, r, tvl, b) ->
        let inline = trm_annot_has Fun_inline t in
        dattr ^^ trm_let_fun_to_doc ~semicolon inline f r tvl b
    | Trm_typedef t -> dattr ^^ typedef_to_doc ~semicolon t
    | Trm_if (b, then_, else_) ->
       let db = decorate_trm ~semicolon:false b in
       let dt = decorate_trm ~semicolon:true then_ in
       begin match else_.desc with
       | Trm_val (Val_lit Lit_unit) ->
          dattr ^^ separate (blank 1) [string "if"; parens db; dt]
       | _ ->
          let de = decorate_trm ~semicolon:true else_ in
          dattr ^^ separate (blank 1) [string "if"; parens db; dt] ^^
            hardline ^^ string "else" ^^ blank 1 ^^ de
       end
    | Trm_seq tl ->
       let tl_m = tl.marks in
       let tl = Mlist.to_list tl in

       if List.mem Multi_decl t.annot
         then dattr ^^ multi_decl_to_doc loc tl
       else if List.exists (function No_braces _ -> true | _ -> false) t.annot
         then
          let dl = List.map (decorate_trm ~semicolon:true) tl in
          dattr ^^ separate hardline dl
       else if List.exists (function Include _ -> true | _ -> false) t.annot then
         (* LATER: restore printing of includes
            sharp ^^ string "include" ^^ blank 1 ^^ dquote ^^ string (get_include_filename t) ^^ dquote *)
         empty
       else
          let counter = ref (-1) in
          let dl = List.map (decorate_trm ~semicolon:true) tl in
          let dl = Tools.fold_lefti (fun i acc m ->
           if m <> [] then begin
             incr counter;
             let m = Tools.list_to_string ~sep:"," m in
             let s = string ("/*@" ^ m ^ "@*/") in
             Tools.insert_at (i + !counter) s acc end
           else acc
          ) dl tl_m in
          counter := -1;
          let res = if trm_annot_has Main_file t then (separate (twice hardline) dl) else surround 2 1 lbrace (separate hardline dl) rbrace in
          dattr ^^ res
    | Trm_apps (f, tl) ->
           dattr ^^ apps_to_doc ~prec f tl ^^ dsemi
     | Trm_while (b, t) ->
        let db = decorate_trm b in
        let dt = decorate_trm ~semicolon:true t in
        dattr ^^ separate (blank 1) [string "while"; parens db; dt]
     | Trm_do_while (t, b) ->
      let dt = decorate_trm t in
      let db = decorate_trm b in
      dattr ^^ string "do " ^^ dt ^^ blank 1 ^^ string "while " ^^ parens db ^^ semi
     | Trm_for_c (init, cond, step, body) ->
        let dinit = decorate_trm init in
        let dcond = decorate_trm cond in
        let dstep = decorate_trm step in
        let dbody = decorate_trm ~semicolon:true body in
        dattr ^^ string "for" ^^ blank 1 ^^
          parens (separate (semi ^^ blank 1) [dinit; dcond; dstep]) ^^
            blank 1 ^^ dbody
     | Trm_for (index, start, direction, stop, step, body) ->
       let full_loop = unpack_trm_for ~loc:t.loc index start direction stop step body in
       decorate_trm full_loop
     | Trm_switch (cond, cases) ->
        let dcond = decorate_trm cond in
        let dcases =
          separate hardline
            (List.map
               (fun (tl, body) ->
                 (match tl with
                  | [] -> string "default" ^^ colon
                  | _ ->
                     (separate hardline
                        (List.map (fun t ->
                         string "case" ^^ blank 1 ^^ decorate_trm t ^^ colon) tl)
                     )
                 ) ^^
                 nest 2 (hardline ^^ decorate_trm ~semicolon:true body ^^
                           hardline ^^ string "break" ^^ semi)
               )
               cases
            )
        in
        dattr ^^ string "switch" ^^ blank 1 ^^ parens dcond ^^ blank 1 ^^
          surround 2 1 lbrace dcases rbrace
     | Trm_abort a ->
        begin match a with
        | Ret t_o ->
           begin match t_o with
           | None -> dattr ^^ string "return" ^^ dsemi
           | Some t -> dattr ^^ string "return " ^^ decorate_trm ~print_struct_init_type:true t ^^ dsemi
           end
        | Break _ -> dattr ^^ string "break" ^^ dsemi
        | Continue _ -> dattr ^^ string "continue" ^^ dsemi
        end
     | Trm_labelled (l, t) ->
        let dt = decorate_trm  ~semicolon t in
        dattr ^^ string l ^^ colon ^^ nest 2 (hardline ^^ dt)
     | Trm_goto l -> dattr ^^ string "goto" ^^ blank 1 ^^ string l ^^ dsemi
     | Trm_arbitrary a_kind  ->
        let code_str =
        begin match a_kind with
        | Lit l -> string l
        | Expr e -> string e
        | Stmt s -> string s
        | Instr s -> string s ^^ semi
        | _ -> fail t.loc "trm_to_doc: arbitrary code should be entered by using Lit, Expr and Stmt only"
        end  in
        dattr ^^ code_str
     | Trm_omp_directive d ->
          let intro = if !print_commented_pragma then string "//" else empty in
          dattr ^^ intro ^^ sharp ^^ string "pragma" ^^ blank 1 ^^ string "omp" ^^ blank 1 ^^ directive_to_doc d
     | Trm_omp_routine  r -> dattr ^^ routine_to_doc r
     | Trm_extern (lang, tl) ->
        begin match tl with
        | [t1] ->
          let dt = decorate_trm ~semicolon:true t1 in
          dattr ^^ string "extern " ^^ string lang ^^ blank 1 ^^ dt
        | _ ->
          let dl = List.map (decorate_trm ~semicolon:true) tl in
          dattr ^^ string "extern " ^^ string lang ^^ blank 1^^surround 2 1 lbrace (separate hardline dl) rbrace
        end
     | Trm_namespace (name, t1, inline) ->
      let inline = if inline then string "inline" else empty in
      let dt = decorate_trm ~semicolon:true t1 in
      dattr ^^ inline ^^ string "namespace" ^^ blank 1 ^^ string name ^^  blank 1 ^^ dt
     | Trm_let_record (name, rt, s, t1) ->
      let get_document_list s =
        let rec aux acc = function
          | [] -> acc
          | (lb, t) :: tl ->
            aux ((typed_var_to_doc (lb, t) ^^ semi) :: acc) tl in
            aux [] s in
        let dl = get_document_list s in
        let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
        let dname = if name = "" then empty else blank 1 ^^ string name in
        let drt = record_type_to_doc rt in
        let dt = decorate_trm t1 in
        dattr ^^ drt ^^ dname ^^ blank 1 ^^ sbody  ^^ blank 1 ^^ dt ^^ semi
     | Trm_template (tpl, t1) ->
        let dl = decorate_trm t1 in
        let dtpl = List.map (fun (n, tpk, _) ->
          match tpk with
          | Type_name typ_opt ->
            begin match typ_opt with
            | Some ty -> string "class " ^^ string n ^^ equals ^^ typ_to_doc ty
            | None -> string "class " ^^ string n
            end
          | NonType (ty, t_opt) ->
            begin match t_opt with
            | Some t1 -> typ_to_doc ty ^^ blank 1 ^^ string n ^^ equals ^^ trm_to_doc t1
            | None -> typ_to_doc ty ^^ blank 1 ^^ string n
            end
          | Template _ -> fail None "template_param_kind_to_doc: nested templates are not supported"

        ) tpl in
        string "template" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~bounds:[langle;rangle] dtpl) ^^ dl ^^ semi
     end in
  (* Save the result in the optional stringreprs table, before returning the document *)
  add_stringreprs_entry t d; (* LATER/ might be also in decorate_trm *)
  d

and record_type_to_doc (rt : record_type) : document =
  match rt with
  | Struct -> string "struct"
  | Union -> string "union"
  | Class -> string "class"


and trm_let_to_doc ?(semicolon : bool = true) (tv : typed_var) (init : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dtx = typed_var_to_doc tv in
  let dinit = begin match init.desc with
  | Trm_val (Val_lit Lit_uninitialized) -> dsemi
  | _ -> equals ^^ blank 1 ^^ decorate_trm ~print_struct_init_type:false init ^^ dsemi
  end in
    dtx ^^ blank 1 ^^ dinit

and trm_let_mult_to_doc ?(semicolon : bool = true) (ty : typ) (vl : var list) (tl : trm list) : document =
  let dsemi = if semicolon then semi else empty in
  let dtx = typ_to_doc ty in
  let dtl = List.map2 (fun v t1 ->
    if is_trm_uninitialized t1
      then string v
      else string v ^^ equals ^^ trm_to_doc t1
  ) vl tl in
  dtx  ^^ blank 1 ^^ Tools.list_to_doc ~sep:comma dtl ~bounds:[empty; empty] ^^ dsemi


and trm_let_fun_to_doc ?(semicolon : bool = true)(inline : bool) (f : var) (r : typ) (tvl : typed_vars) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dinline = if inline then [string "inline"] else [] in
  let f = Tools.string_subst "overloaded" "operator" f in (* LATER: this is problematic if the user has a function name that contains "overloaded" as substring *)
  let argd = if List.length tvl = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc tv) tvl) in
  let dr = typ_to_doc r in
  begin match b.desc with
  | Trm_val (Val_lit Lit_uninitialized) ->
     separate (blank 1) (dinline @ [dr; string f; parens argd]) ^^ dsemi
  | _ -> separate (blank 1) (dinline @ [dr; string f; parens argd; decorate_trm b])
  end

and typedef_to_doc ?(semicolon : bool = true) (td : typedef) : document =
  let dsemi = if semicolon then semi else empty in
  let tname = td.typdef_tconstr in
  let tbody = td.typdef_body in

  match tbody with
  | Typdef_alias t ->
      begin match t.typ_desc with
      | Typ_array _ ->
         string "typedef" ^^ blank 1 ^^ typed_var_to_doc (tname, t) ^^ dsemi
      | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = {typ_desc = Typ_fun (tyl, r); _}} ->
         let dl = List.map typ_to_doc tyl in
         let dr = typ_to_doc r in
         separate (blank 1)
         [
            string "typedef"; dr; parens (star ^^ string tname) ^^ parens (separate (comma ^^ blank 1) dl)
         ] ^^ dsemi
      | _ ->
         separate (blank 1) [string "typedef"; typ_to_doc t; string tname] ^^ dsemi
      end
  | Typdef_prod (tn, s) ->
      let get_document_list s =
      let rec aux acc = function
         | [] -> acc
         | (lb, t) :: tl ->
            aux ((typed_var_to_doc (lb, t) ^^ semi) :: acc) tl in
            aux [] s in
      let dl = get_document_list s in
      let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
      let second_name = if tn then td.typdef_tconstr else "" in
      string "typedef " ^^ string "struct" ^^ blank 1 ^^ string second_name ^^ blank 1 ^^ sbody ^^ blank 1 ^^ string td.typdef_tconstr ^^ semi
  | Typdef_sum _ ->
      fail None "typedef_to_doc: sum types are not supported in C/C++"
  | Typdef_enum enum_const_l ->
      let const_doc_l =
        List.map
         (fun (y, t_o) ->
           match t_o with
           | None -> string y
           | Some t -> separate (blank 1) [string y; equals; decorate_trm t]
         )
        enum_const_l in
      separate (blank 1) [string "enum"; string tname;
      braces (separate (comma ^^ blank 1) const_doc_l)] ^^ dsemi

and multi_decl_to_doc (loc : location) (tl : trms) : document =
 let get_info (t : trm) : document =
  begin match t.desc with
  | Trm_let (vk, (x, _), init) ->
    begin match vk with
    | Var_immutable ->
      begin match init.desc with
      | Trm_val(Val_lit Lit_uninitialized) -> string x
      | _ -> string x ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm init
      end
    | _ ->
      begin match init.desc with
      | Trm_apps (_, [base])-> string x ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm base
      | _ -> string x
      end
    end
  | Trm_typedef _ -> string ""
  | _ -> fail loc "multi_decl_to_doc: only variables declarations allowed"
  end
 in
 let dnames = separate (comma ^^ blank 1) (List.map get_info tl) in
  begin match tl with
  | [] -> fail loc "multi_deco_to_doc: empty multiple declaration"
  | [d] -> begin match d.desc with
           | Trm_typedef td -> typedef_to_doc td
           | _ -> fail loc "multi_decl_to_doc: expected a typedef"
           end
  | hd :: _ ->
    match hd.desc with
    | Trm_let (vk, (_, ty), _) ->
      begin match vk with
      | Var_immutable -> string " " ^^ blank 1 ^^ typ_to_doc ty ^^ blank 1 ^^ dnames ^^ semi
      | _ -> begin match ty.typ_desc with
            | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty1} when is_generated_typ ty -> typ_to_doc ty1 ^^ blank 1 ^^ dnames ^^ semi
            | _ -> typ_to_doc ty ^^ blank 1 ^^ dnames ^^ semi
            end
       end
  | _ -> fail loc "multi_decl_to_doc: expected a trm_let"
  end

and apps_to_doc ?(prec : int = 0) (f : trm) (tl : trms) : document =
  let (prec, assoc) = precedence_trm f in
  let aux_arguments f_as_doc =
      f_as_doc ^^ Tools.list_to_doc ~empty ~sep:comma ~bounds:[lparen; rparen]  (List.map (decorate_trm) tl)
      in

  match f.desc with
  (* Case of function pointers *)
  | Trm_apps ({ desc = (Trm_val (Val_prim (Prim_unop Unop_get))); _ }, [ { desc = Trm_var (_, x); _ } ]) ->
      aux_arguments (string x)
  (* Case of function by name *)
  | Trm_var (_, x) -> aux_arguments (string x)

  (* Case of inlined function *)
  | Trm_let_fun _ ->
        parens (decorate_trm f) ^^ Tools.list_to_doc ~sep:comma ~bounds:[lparen; rparen] (List.map decorate_trm tl)
  (* Case of primitive operations *)
  | Trm_val v ->
     begin match v with
     | Val_prim p ->
        begin match p with
        | Prim_unop op ->
           begin match tl with
           | [t] ->
              let d = decorate_trm ~prec t in
              begin match op with
              | Unop_get when !print_optitrust_syntax ->
                  string "get(" ^^ d ^^ string ")"
              | Unop_get -> star ^^ d
              | Unop_address ->ampersand ^^ d
              | Unop_neg -> bang ^^ d
              | Unop_bitwise_neg -> tilde ^^ d
              | Unop_minus -> minus ^^ blank 1 ^^ d
              | Unop_plus -> plus ^^ blank 1 ^^ d
              | Unop_post_inc -> d ^^ twice plus
              | Unop_post_dec -> d ^^ twice minus
              | Unop_pre_inc -> twice plus ^^ d
              | Unop_pre_dec -> twice minus ^^ d
              | Unop_struct_access f1 when !print_optitrust_syntax ->
                  string "struct_access(" ^^ d ^^ comma ^^ string " " ^^ dquotes (string f1) ^^ string ")"
              | (Unop_struct_get f1 | Unop_struct_access f1) ->
                 if is_get_operation t then
                    if List.mem Display_no_arrow f.annot
                      then
                        d ^^ dot ^^ string f1
                      else
                        let d = decorate_trm ~prec (get_operation_arg t) in
                        d ^^ minus ^^ rangle ^^ string f1
                 else
                    d ^^ dot ^^ string f1
              | Unop_cast ty ->
                 let dty = typ_to_doc ty in
                 parens dty ^^ blank 1 ^^ d
              end
           | _ ->
              fail f.loc "apps_to_doc: unary operators must have one argument"
           end
        | Prim_binop op ->
          let (prec1, prec2) =
            if assoc = LtoR
              then (prec, prec + 1)
              else (prec + 1, prec)
            in
          let op_d = binop_to_doc op in
          begin match tl with
          | [t1; t2] ->
            let d1 = decorate_trm ~prec:prec1 t1 in
            let d2 = decorate_trm ~prec:prec2 t2 in
            begin match op with
             | Binop_set when !print_optitrust_syntax ->
                (* equals ^^ parens (d1 ^^ comma ^^ d2) *)
                string "set(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
             | Binop_array_access when !print_optitrust_syntax ->
                string "array_access(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
             (* LATER:
               | Binop_array_access ->
                fail t.loc "Binop_array_access should not appear in C code " *)
             | Binop_array_access | Binop_array_get ->
                let d2 = decorate_trm ~prec:0 t2 in
                d1 ^^ brackets (d2)
             | _ -> separate (blank 1) [d1; op_d; d2]
             end
          | _ -> fail f.loc "apps_to_doc: binary_operators must have two arguments"
          end

        | (Prim_compound_assgn_op _ | Prim_overloaded_op _) as p_b ->
           begin match tl with
           | [t1; t2] ->
              let d1 = decorate_trm ~prec t1 in
              let d2 = decorate_trm ~prec t2 in
              let op_d = prim_to_doc p_b in
              if !print_optitrust_syntax
                then op_d ^^ parens (d1 ^^ comma ^^ d2)
                else separate (blank 1) [d1; op_d; d2]
          | _ -> fail f.loc "apps_to_doc: binary operators must have two arguments"
          end
        | Prim_conditional_op ->
           begin match tl with
           | [t1; t2; t3] ->
              let d1 = decorate_trm ~prec:4 t1 in
              let d2 = decorate_trm ~prec:4 t2 in
              let d3 = decorate_trm ~prec:4 t3 in
              parens (separate (blank 1) [d1; qmark; d2; colon; d3])
           | _ ->
              fail f.loc
                "apps_to_doc: conditional operator must have three arguments"
           end
        | Prim_new t ->
          (* Here we assume that trm_apps has only one trm as argument *)
          let value = List.hd tl in
          string "new" ^^ blank 1 ^^ typ_to_doc t ^^ parens (decorate_trm value)
        end
     | _ -> fail f.loc (Printf.sprintf "apps_to_doc: only primitive values may be applied %s\n" (Ast_to_text.ast_to_string f))
     end
   | _ ->
      Ast_to_text.print_ast ~only_desc:true stdout f;
      fail f.loc "apps_to_doc: only functions may be applied"

and mode_to_doc (m : mode) : document =
  match m with
  | Shared_m -> string "shared"
  | None_ -> string "none"

and sched_type_to_doc (st : sched_type) : document =
  match st with
  | Static -> string "static"
  | Dynamic -> string "dynamic"
  | Guided -> string "guided"
  | Runtime -> string "runtime"

and reduction_identifier_to_doc (ri : reduction_identifier) : document =
  match ri with
  | Plus -> plus
  | Minus -> minus
  | Prod -> star
  | And -> ampersand ^^ ampersand
  | Or -> bar ^^ bar
  | Power -> caret
  | BitAnd -> ampersand
  | BitOr -> bar
  | Min -> string "min"
  | Max -> string "max"

and map_type_to_doc (mt : map_type) : document =
  match mt with
  | Alloc -> string "alloc" ^^ colon
  | To -> string "to" ^^ colon
  | From -> string "from" ^^ colon
  | ToFrom -> string "tofrom" ^^ colon
  | No_map -> empty

and proc_bind_to_doc (pb : proc_bind) : document =
  match pb with
  | Master_pb -> string "master"
  | Close -> string "close"
  | Spread -> string "spread"

and dependece_type_to_doc (dp : dependence_type) : document =
  match dp with
  | In vl -> string "in" ^^ colon ^^ blank 1 ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)
  | Out vl -> string "out" ^^ colon ^^ blank 1 ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)
  | Inout vl -> string "inout" ^^ colon ^^ blank 1 ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)
  | Outin vl -> string "outin" ^^ colon ^^ blank 1 ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)
  | Sink vl -> string "sink" ^^ colon ^^ blank 1 ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)
  | Source -> string "source"

and clause_to_doc (cl : clause) : document =
  match cl with
  | Default m -> string "default" ^^ parens (mode_to_doc m)
  | Shared vl -> string "shared" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Private vl -> string "private" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | FirstPrivate vl -> string "firstprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | LastPrivate vl -> string "lastprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Linear (vl, step) -> string "linear" ^^ parens (string ( Tools.list_to_string ~sep:"," ~bounds: ["";""] vl)  ^^ if step = 0 then empty else blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int step))
  | Reduction (ri, vl) -> string "reduction" ^^ parens (reduction_identifier_to_doc ri ^^ colon ^^ string (Tools.list_to_string ~sep:"," ~bounds:["";""] vl))
  | Copyin vl -> string "copyin" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | CopyPrivate vl -> string "copyprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Map_c (mt, vl) -> string "map" ^^ parens (map_type_to_doc mt ^^  blank 1 ^^ string (Tools.list_to_string ~sep:"," ~bounds: ["";""] vl))
  | Defaultmap (mt, vl) -> string "defaultmap" ^^ parens (map_type_to_doc mt ^^  blank 1 ^^ string (Tools.list_to_string ~sep:"," ~bounds: ["";""] vl))
  | Safelen i -> string "safelen" ^^ parens (string (string_of_int i))
  | Collapse i -> string "collapse" ^^ parens (string (string_of_int i))
  | Simdlen i -> string "simdlen" ^^ parens (string (string_of_int i))
  | Aligned (vl, i) -> string "aligned" ^^ parens (string (Tools.list_to_string ~sep:"," ~bounds:["";""] vl) ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int i))
  | Uniform vl -> string "uniform" ^^ string (Tools.list_to_string ~sep:"," ~bounds:["(";")"] vl)
  | Inbranch -> string "inbranch"
  | NotInbranch -> string "notinbranch"
  | Nowait -> string "nowait"
  | Ordered_c i -> string "ordered" ^^ (if i = 0 then empty else parens (string (string_of_int i)))
  | If e-> string "if" ^^ parens (string e)
  | Device i -> string "device" ^^ parens (string i)
  | Num_threads i -> string "num_threads" ^^ parens (string i)
  | Schedule (st, i) -> string "schedule" ^^ parens (sched_type_to_doc st ^^ (if i = "" then empty else comma ^^ blank 1 ^^ string i))
  | Dist_schedule (st, i) -> string "dist_schedule" ^^ parens (sched_type_to_doc st ^^ (if i = "" then empty else comma ^^ blank 1 ^^ string i))
  | Parallel_c -> string "parallel"
  | Sections_c -> string "sections"
  | For_c -> string "for"
  | Taskgroup_c -> string "taskgroup"
  | Proc_bind pb -> string "proc_bind" ^^ parens (proc_bind_to_doc pb)
  | Priority i -> string "priority" ^^ parens (string i)
  | Depend dp -> string "depend" ^^ parens (dependece_type_to_doc dp)
  | Grainsize i -> string "grainsize" ^^ parens (string (string_of_int i))
  | Mergeable -> string "mergeable"
  | Nogroup -> string "nogroup"
  | Num_tasks i -> string "num_tasks" ^^ parens (string (string_of_int i))
  | Untied -> string "untied"
  | Final e -> string "final" ^^ parens (string e)
  | To_c vl -> string "to" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | From_c vl -> string "from" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Link vl -> string "link" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Num_teams n -> string "num_teams" ^^ parens (string n)
  | Thread_limit n -> string "thread_limit" ^^ parens (string n)

and atomic_operation_to_doc (ao : atomic_operation option) : document =
  match ao with
  | None -> empty
  | Some ao1 ->
    begin match ao1 with
    | Read -> string "read"
    | Write -> string "write"
    | Update -> string "update"
    | Capture -> string "capture"
    end

and directive_to_doc (d : directive) : document =
  match d with
  | Atomic ao -> string "atomic" ^^ blank 1 ^^ (atomic_operation_to_doc ao)
  | Atomic_capture -> string "atomic" ^^ blank 1 ^^ string "capture"
  | Barrier -> string "barrier"
  | Cancel (c, cl) -> string "cancel" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ Tools.list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Cancellation_point (c, cl) -> string "cancellation" ^^ blank 1 ^^ string "point" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ Tools.list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Critical (name, hint) -> string "critical" ^^ if name = "" then empty else parens (string name) ^^ if hint = "" then empty else (string "hint" ^^ parens (string hint))
  | Declare_simd cl -> string "declare" ^^ blank 1 ^^ string "simd " ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Declare_reduction (ri, tvl, e, c) ->  string "declare" ^^ blank 1 ^^ string "simd" ^^ parens (
    reduction_identifier_to_doc ri ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (Tools.list_to_string ~sep:"," ~bounds:["";""] tvl) ^^
    string e ^^ clause_to_doc c)
  | Declare_target cl -> string "declare" ^^ blank 1 ^^ string "target " ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Distribute cl -> string "distribute" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Distribute_parallel_for cl -> string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Distribute_parallel_for_simd cl -> string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Distribute_simd -> string "distribute" ^^ blank 1 ^^ string "simd"
  | End_declare_target -> string "end" ^^ blank 1 ^^ string "declare " ^^ string "target"
  | Flush vl -> string "flush" ^^ string (Tools.list_to_string ~sep:"," ~bounds:["(";")"] vl)
  | For cl -> string "for" ^^ blank 1 ^^ (Tools.list_to_doc ~empty ~sep:(blank 1) ~bounds:[empty; empty](List.map clause_to_doc cl))
  | For_simd cl -> string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Master -> string "master"
  | Ordered cl -> string "ordered" ^^ blank 1 ^^ (Tools.list_to_doc ~empty ~sep:(blank 1) ~bounds:[empty; empty](List.map clause_to_doc cl))
  | Parallel  cl -> string "parallel" ^^ blank 1 ^^ (Tools.list_to_doc ~empty ~sep:(blank 1) ~bounds:[empty; empty](List.map clause_to_doc cl))
  | Parallel_for cl -> string "parallel" ^^ blank 1 ^^ string "for " ^^ (Tools.list_to_doc ~empty ~sep:(blank 1) ~bounds:[empty; empty](List.map clause_to_doc cl))
  | Parallel_for_simd  cl -> string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Parallel_sections  cl -> string "parallel" ^^ blank 1 ^^ string "sections" ^^ blank 1  ^^ (Tools.list_to_doc ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Section -> string "section"
  | Sections cl -> string "sections" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Simd cl -> string "simd" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Single cl -> string "single" ^^ blank 1 ^^ (Tools.list_to_doc ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Target cl -> string "target" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Target_data cl -> string "target" ^^ blank 1 ^^ string "data"  ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_enter_data  cl -> string "target" ^^ blank 1 ^^ string "enter" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_exit_data  cl -> string "target" ^^ blank 1 ^^ string "exit" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_teams cl -> string "target" ^^ blank 1 ^^ string "teams"  ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Target_teams_distribute cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Target_update cl -> string "target" ^^ blank 1 ^^ string "update" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:comma ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Task cl -> string "task" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Taskgroup -> string "taskgroup"
  | Taskloop cl -> string "taskloop" ^^ blank 1 ^^ (Tools.list_to_doc ~sep:(blank 1) ~empty ~bounds:[empty;empty] (List.map clause_to_doc cl))
  | Taskloop_simd cl -> string "taskloop" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Taskwait -> string "taskwait"
  | Taskyield -> string "taskyield"
  | Teams cl -> string "teams" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_end cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "end" ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for_simd cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^(Tools.list_to_doc (List.map clause_to_doc cl))
  | Threadprivate vl -> string "threadprivate" ^^ parens(string (Tools.list_to_string ~sep:"," ~bounds:["";""] vl))

and routine_to_doc (r : omp_routine) : document =
  match r with
  | Set_num_threads i -> string "omp_set_num_threads" ^^ parens (string (string_of_int i)) ^^ semi
  | Get_num_threads -> string "omp_get_num_threads" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_max_threads -> string "omp_get_max_threads" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_thread_num  -> string "omp_get_thread_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_procs  -> string "omp_get_num_procs" ^^ lparen ^^ blank 1 ^^ rparen
  | In_parallel  -> string "omp_in_parallel" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_dynamic i -> string "omp_set_dynamic" ^^ parens (string (string_of_int i))
  | Get_dynamic  -> string "omp_get_dynamic" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_cancellation  -> string "omp_get_cancellation"
  | Set_nested i -> string "omp_set_nested" ^^ parens (string (string_of_int i))
  | Get_nested  -> string "omp_get_nested" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_schedule (s_type, md) -> string "omp_set_schedule" ^^ parens (string "omp_sched" ^^ sched_type_to_doc s_type ^^ comma ^^ blank 1 ^^ string (string_of_int md))
  | Get_schedule (s_type, md) -> string "omp_get_schedule" ^^ parens (string "omp_sched" ^^ sched_type_to_doc s_type ^^ comma ^^ blank 1 ^^ string (string_of_int md))
  | Get_thread_limit  -> string "omp_get_thread_limit" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_max_active_levels i -> string "omp_set_max_active_levels" ^^ parens (string (string_of_int i))
  | Get_max_active_levels -> string "omp_get_max_active_levels" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_level -> string "omp_get_level" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_ancestor_thread_num -> string "omp_get_ancestor_thread_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_team_size level -> string "omp_get_team_size" ^^ lparen ^^ string (string_of_int level) ^^ rparen
  | Get_active_level  -> string "omp_get_active_level" ^^ lparen ^^ blank 1 ^^ rparen
  | In_final  -> string "omp_in_final" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_proc_bind  -> string "omp_get_proc_bind" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_default_device i -> string "omp_set_default_device" ^^ parens (string i)
  | Get_default_device -> string "omp_get_default_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_devices  -> string "omp_get_num_devices" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_teams -> string "omp_get_num_teams" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_team_num  -> string "omp_team_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Is_initial_device -> string "omp_is_initial_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Init_lock lck -> string "omp_init_lock" ^^ parens (ampersand ^^ string lck)
  | Init_nest_lock lck -> string "omp_init_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Destroy_lock lck -> string "omp_destroy_lock" ^^ parens (ampersand ^^ string lck)
  | Destroy_nest_lock lck -> string "omp_destroy_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Set_lock lck-> string "omp_set_lock" ^^ parens (ampersand ^^ string lck)
  | Set_nest_lock lck -> string "omp_set_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Unset_lock lck ->  string "omp_unset_lock" ^^ parens (ampersand ^^ string lck)
  | Unset_nest_lock lck -> string "omp_unset_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Test_lock lck -> string "omp_test_lock" ^^ parens (ampersand ^^ string lck)
  | Test_nest_lock lck -> string "omp_test_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Get_wtime -> string "get_wtime" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_wtick -> string "get_wtich" ^^ lparen ^^ blank 1 ^^ rparen

and unpack_trm_for ?(loc = None) (index : var) (start : trm) (direction : loop_dir) (stop : trm) (step : loop_step) (body : trm) : trm =
  let init = trm_let Var_mutable (index, typ_int()) start  in
  let cond = begin match direction with
    | DirUp -> trm_apps (trm_binop Binop_lt) [trm_var index;stop]
    | DirUpEq -> trm_apps (trm_binop Binop_le) [trm_var index;stop]
    | DirDown ->
      trm_apps (trm_binop Binop_gt) [trm_var index;stop]
    | DirDownEq ->
      trm_apps (trm_binop Binop_ge) [trm_var index;stop]
   end in
  let step =
    begin match direction with
    | DirUp | DirUpEq ->
      begin match step with
      | Pre_inc ->
        trm_apps (trm_unop Unop_pre_inc) [trm_var index]
      | Post_inc ->
        trm_apps (trm_unop Unop_post_inc) [trm_var index]
      | Step st ->
        trm_apps (trm_prim (Prim_compound_assgn_op Binop_add) ) [trm_var index; st]
      | _ -> fail body.loc "unpack_trm_for: can't use decrementing operators for upper bounded for loops"
      end
    | DirDown | DirDownEq ->
      begin match step with
      | Pre_dec ->
        trm_apps (trm_unop Unop_pre_dec) [trm_var index]
      | Post_dec ->
        trm_apps (trm_unop Unop_post_dec) [trm_var index]
      | Step st ->
        trm_apps (trm_prim (Prim_compound_assgn_op Binop_sub) ) [trm_var index; st]
      | _ -> fail body.loc "unpack_trm_for: can't use decrementing operators for upper bounded for loops"
      end

    end in
    trm_for_c  ~loc init cond step body

let ast_to_doc ?(comment_pragma : bool = false) ?(optitrust_syntax:bool=false) (t : trm) : document =
  let comment_pragma = false in (* temporary *)
  if comment_pragma then print_commented_pragma := true;
  if optitrust_syntax then print_optitrust_syntax := true;
  let d = decorate_trm t in
  if optitrust_syntax then print_optitrust_syntax := false;
  if comment_pragma then print_commented_pragma := false;
  d

let ast_to_outchannel ?(comment_pragma : bool = false) ?(optitrust_syntax:bool=false) (out : out_channel) (t : trm) : unit =
  ToChannel.pretty 0.9 (!Flags.code_print_width) out (ast_to_doc ~comment_pragma ~optitrust_syntax t)

let ast_to_file ?(optitrust_syntax:bool=false) (filename : string) (t : trm) : unit =
  let out = open_out filename in
  ast_to_outchannel ~optitrust_syntax out t;
  close_out out

let ast_to_string ?(optitrust_syntax : bool = false) (t : trm) : string =
  Tools.document_to_string (ast_to_doc t)

let typ_to_string (ty : typ) : string =
  let b = Buffer.create 80 in
  ToBuffer.pretty 0.9 (!Flags.code_print_width) b (typ_to_doc ty);
  Buffer.contents b

let trm_print_debug (t : trm) : unit =
  print_stringreprids := true;
  Printf.printf "==\n%s\n===\n" (ast_to_string t);
  print_stringreprids := false


