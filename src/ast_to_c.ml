open PPrint
open Ast

(* Flag to control whether we should "decode" or not the encodings
   that were performed when converting from Clang AST to our AST.
   This global reference is to be accessed only from this file. *)
let decode = ref true

(* translate an ast to a C/C++ document *)
let rec typ_desc_to_doc ?(const : bool = false) (t : typ_desc) : document =
  match t with
  | Typ_const t when (is_atomic_typ t)-> typ_to_doc t ^^ string " const "
  | Typ_const t -> string " const "  ^^ typ_to_doc t
  | Typ_constr (tv, _, _) -> string tv
  | Typ_auto  -> string "auto"
  | Typ_unit -> string "void"
  | Typ_int -> string "int"
  | Typ_float -> string "float"
  | Typ_double -> string "double"
  | Typ_bool -> string "bool"
  | Typ_char -> string "char"
  | Typ_ptr { ptr_kind = pk; inner_typ = t} ->
    begin match pk with
    | Ptr_kind_mut -> 
      typ_to_doc t ^^ star
    | Ptr_kind_ref ->
      if not !decode then 
        begin match const with 
        | true -> typ_to_doc t ^^ string "<annotation:&>"
        | false -> typ_to_doc t ^^ star ^^ string "<annotation:&>"
        end
      else typ_to_doc t ^^ ampersand 
    end
  | Typ_array (t, s) ->
     let d = typ_to_doc t in
     begin match s with
     | Undefined -> d ^^ brackets empty
     | Const n -> d ^^ brackets (string (string_of_int n))
     | Trm t' -> d ^^ brackets (decorate_trm t')
     end
  | Typ_fun (_, _) ->
     print_info None "typ_desc_to_doc: typ_fun not implemented\n";
     at
  | Typ_var t -> string t

and typ_annot_to_doc (a : typ_annot) : document =
  match a with
  | Unsigned -> string "unsigned"
  | Long -> string "long"
  | Short -> string "short"

and typ_to_doc ?(const : bool = false) (t : typ) : document =
  let d = typ_desc_to_doc ~const t.typ_desc in
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

and typed_var_to_doc ?(const:bool=false) (tx : typed_var) : document =
  let const_string = if false then blank 1 ^^ string " const " ^^ blank 1 else empty in
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
  let (x, t) = tx in
  let dattr =
    match t.typ_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  match t.typ_desc with
  | Typ_array (t, s) ->
     let (base, bracketl) = aux t s in
     dattr ^^ base ^^ blank 1 ^^ const_string ^^ string x ^^ concat bracketl
  | _ -> const_string ^^ typ_to_doc ~const t ^^ blank 1 ^^ string x

and lit_to_doc (l : lit) : document =
  match l with
  | Lit_unit -> empty
  | Lit_uninitialized ->
     print_info None "lit_to_doc: uninitialized literal should not occur\n";
     at
  | Lit_bool b -> string (string_of_bool b)
  | Lit_int i -> string (string_of_int i)
  | Lit_double f -> string (string_of_float f)
  | Lit_string s -> dquotes (separate (backslash ^^ string "n") (lines s))

and unop_to_doc (op : unary_op) : document =
  match op with
  | Unop_get -> star
  | Unop_neg -> bang
  | Unop_bitwise_neg -> tilde
  | Unop_opp -> minus
  | Unop_inc -> twice plus
  | Unop_dec -> twice minus
  | Unop_struct_field_addr s -> dot ^^ string s
  | Unop_struct_field_get s -> dot ^^ string s
  | Unop_cast t ->
     let dt = typ_to_doc t in
     string "static_cast" ^^ langle ^^ dt ^^ rangle

and binop_to_doc (op : binary_op) : document =
  match op with
  | Binop_set -> equals
  | Binop_array_cell_addr -> lbracket ^^ rbracket
  | Binop_array_cell_get -> lbracket ^^ rbracket
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
  | Prim_new t -> string "new" ^^ blank 1 ^^ typ_to_doc t
  | Prim_conditional_op ->
     (* put holes to display the operator *)
     separate (blank 1) [underscore; qmark; underscore; colon; underscore]
  | Prim_fetch_and_add ->
    string "fetch_and_add"
  | Prim_atomic_get _ -> string "atomic_get"
  | Prim_atomic_set _ -> string "atomic_set"
  | Prim_compare_and_swap -> string "compare_and_swap"

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
  | Val_array vl | Val_struct vl ->
     let dl = List.map val_to_doc vl in
     braces (separate (comma ^^ blank 1) dl)
  | Val_prim p -> prim_to_doc p

and attr_to_doc (a : attribute) : document =
  match a with
  | Identifier x -> string x
  | Aligned t -> underscore ^^ string "Alignas" ^^ parens (decorate_trm t)
  | GeneratedStar -> blank 1

and decorate_trm ?(semicolon : bool = false) (t : trm) : document = 
  if (List.exists (function Highlight _-> true | _ -> false) t.annot) then
      let (l, r) = get_decorators t in
      let dt = trm_to_doc ~semicolon t in
      string l ^^ dt ^^ string r
    else trm_to_doc ~semicolon t

and trm_to_doc ?(semicolon=false) (t : trm) : document =
  let loc = t.loc in
  let dsemi = if semicolon then semi else empty in
  let dattr =
    match t.attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  (* For printing C code, we have (see explanations in [clang_to_ast.ml],
     search for [Add_address_of_operator] and [Add_star_operator]. *)
  match t.add with
  | Add_address_of_operator :: addl ->
     let d =
       decorate_trm ~semicolon  {desc = t.desc; annot = t.annot; loc = t.loc;
                   is_statement = t.is_statement; add = addl; ctx = t.ctx; typ = t.typ;
                   attributes = []}
     in
     let body = if !decode then parens (ampersand ^^ d) 
                else string "<annotation:addressof>" ^^ d in
     dattr ^^ body ^^ dsemi
  | Add_star_operator :: addl when !decode ->
     let d =
       decorate_trm ~semicolon  {desc = t.desc; annot = t.annot; loc = t.loc;
                   is_statement = t.is_statement; add = addl; ctx = t.ctx; typ = t.typ;
                   attributes = []}
     in
     let body = if !decode then parens (star ^^ d) else d in
     dattr ^^ body ^^ dsemi
  | _ ->
     begin match t.desc with
     | Trm_val v ->
        if List.mem Empty_cond t.annot then empty
          else dattr ^^ val_to_doc v
        
     | Trm_var x -> dattr ^^ string x
     | Trm_array tl | Trm_struct tl ->
        let dl = List.map (decorate_trm ~semicolon) tl in
        dattr ^^ braces (separate (comma ^^ blank 1) dl)
     | Trm_let (vk,tx,t) -> dattr ^^ trm_let_to_doc ~semicolon vk tx t
     | Trm_let_fun (f, r, tvl, b) -> dattr ^^ trm_let_fun_to_doc ~semicolon f r tvl b
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
        if List.mem Multi_decl t.annot 
          then dattr ^^ multi_decl_to_doc loc tl
        else if List.mem (No_braces (Nobrace.current())) t.annot
          then 
           let dl = List.map (decorate_trm ~semicolon:true) tl in
           dattr ^^ separate hardline dl
        else if List.mem Main_file t.annot then
           let dl = List.map (decorate_trm ~semicolon:true) tl in
           dattr ^^ separate (twice hardline) dl
        else if List.exists (function Include _ -> true | _ -> false) t.annot then empty 
        (* else if List.mem (Include h) t.annot then empty *)
        else 
           let dl = List.map (decorate_trm ~semicolon:true) tl in
           dattr ^^ surround 2 1 lbrace (separate hardline dl) rbrace
     | Trm_apps (f, tl) ->
        if List.mem App_and_set t.annot then
           dattr ^^ apps_to_doc ~is_app_and_set:true f tl ^^ dsemi
        else if  List.mem As_left_value t.annot then
          dattr ^^ apps_to_doc ~as_left_value:true f tl 
        else 
           (*
             do not display * operator if the operand is a heap allocated
             variable or a succession of accesses
            *)
          let display_star =

             if (List.mem Mutable_var_get t.annot || List.mem Access t.annot) && !decode then false
             else true
           in
           dattr ^^ apps_to_doc ~display_star f tl ^^ dsemi
     | Trm_while (b, t) ->
        let db = decorate_trm b in
        let dt = decorate_trm ~semicolon:true t in
        dattr ^^ separate (blank 1) [string "while"; parens db; dt]
     | Trm_for_c (init, cond, step, body) ->
        let dinit = decorate_trm init in
        let dcond = decorate_trm cond in
        let dstep = decorate_trm step in
        let dbody = decorate_trm ~semicolon:true body in
        dattr ^^ string "for" ^^ blank 1 ^^
          parens (separate (semi ^^ blank 1) [dinit; dcond; dstep]) ^^
            blank 1 ^^ dbody
     | Trm_for (index, direction, start, stop, step, body) ->
           (* for (int i = 10; i >= 0; i--)       dir=dDirDownTo   step=1 *)
       let full_loop = trm_for_to_trm_for_c index direction start stop step body in
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
           | Some t -> dattr ^^ string "return " ^^ decorate_trm t ^^ dsemi
           end
        | Break -> dattr ^^ string "break" ^^ dsemi
        | Continue -> dattr ^^ string "continue" ^^ dsemi
        end
     | Trm_labelled (l, t) ->
        let dt = decorate_trm ~semicolon t in
        dattr ^^ string l ^^ colon ^^ nest 2 (hardline ^^ dt)
     | Trm_goto l -> dattr ^^ string "goto" ^^ blank 1 ^^ string l ^^ dsemi
     | Trm_any t ->
        let dt = decorate_trm ~semicolon t in
        dattr ^^ string "ANY" ^^ parens (dt)
     | Trm_arbitrary code ->
        dattr ^^ string code ^^ hardline
     | Trm_omp_directive d -> sharp ^^ string "pragma" ^^ blank 1 ^^ string "omp" ^^ directive_to_doc d 
     | Trm_omp_routine _-> fail None "trm_to_doc: still on development" 
     end

and trm_let_to_doc ?(semicolon : bool = true) (varkind : varkind) (tv : typed_var) (init : trm) : document = 
  let dsemi = if semicolon then semi else empty in
  match varkind with 
  | Var_immutable ->
    let dtx = if not !decode then typ_to_doc ~const:true (snd tv) 
                else typed_var_to_doc tv in
    let initialisation = blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm init ^^ dsemi in 
    if not !decode then string "let" ^^blank 1 ^^ string (fst tv) ^^ blank 1 ^^ colon ^^ blank 1 ^^ dtx ^^ initialisation
      else dtx ^^ initialisation
  | Var_mutable ->
    let dtx = begin match (snd tv).typ_desc with 
              | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = tx} when is_generated_star (snd tv) ->
                  begin match tx.typ_desc with 
                  | Typ_ptr {ptr_kind = Ptr_kind_ref; _} ->
                    if not !decode then (typ_to_doc tx)
                     else typed_var_to_doc (fst tv,tx)
                  | _-> 
                    if not !decode then typ_to_doc (snd tv) 
                    else typed_var_to_doc (fst tv, tx)
                  end
              | _ -> typed_var_to_doc tv
              end in
    let d_init, is_initialized  = 
    if not !decode  then init, true
      else begin match init.desc with 
           | Trm_apps (_, [value]) -> value, true
           | Trm_val(Val_prim(Prim_new _)) -> trm_var "", false
           | _-> init, true 
           end in
    let initialisation = blank 1 ^^ (if is_initialized then equals else empty) ^^ blank 1 ^^ decorate_trm d_init ^^ dsemi in
    if not !decode then string "let" ^^ blank 1 ^^ string (fst tv) ^^ blank 1 ^^ colon ^^  blank 1 ^^ dtx ^^ initialisation 
      else dtx ^^ initialisation      

and trm_let_fun_to_doc ?(semicolon : bool = true) (f : var) (r : typ) (tvl : typed_var list) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let f = Str.global_replace (Str.regexp "overloaded") "operator" f in
  let argd = separate (comma ^^ blank 1) (List.map (fun tv -> 
    if is_typ_const (snd tv) then typed_var_to_doc ~const:true tv else typed_var_to_doc tv) tvl) in
  let dr = typ_to_doc r in
  begin match b.desc with
  | Trm_val (Val_lit Lit_uninitialized) ->
     separate (blank 1) [dr; string f; parens argd] ^^ dsemi
  | _ -> separate (blank 1) [dr; string f; parens argd; decorate_trm b]
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
  | Typdef_prod  (tn, s) ->
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

and multi_decl_to_doc (loc : location) (tl : trm list) : document =
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
            | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = ty1} when is_generated_star ty -> typ_to_doc ty1 ^^ blank 1 ^^ dnames ^^ semi
            | _ -> typ_to_doc ty ^^ blank 1 ^^ dnames ^^ semi
            end
       end
  | _ -> fail loc "multi_decl_to_doc: expected a trm_let"
  end

(* display_star: true if f is get and we should display it *)
and apps_to_doc ?(display_star : bool = true) ?(is_app_and_set : bool = false) ?(as_left_value : bool = false)
  (f : trm) (tl : trm list) : document =
  match f.desc with
  (* NOTE: in C, we don't apply arbitrary terms to terms, functions can only
     be variables or primitive functions. *)
  | Trm_var x ->
     if !decode && Str.string_match (Str.regexp "overloaded\\(.*\\)") x 0 then
        (* Note x is for example "overloaded=" *)
       let (d1, d2) =
         begin match List.map decorate_trm tl with
         | [d1; d2] -> (d1, d2)
         | _ ->
            fail f.loc "apps_to_doc: overloaded operators have two arguments"
         end
       in
       let s = Str.string_after x (String.length "overloaded") in
       if (s = "+") then
         parens (separate (blank 1) [d1; plus; d2])
       else if (s = "-") then
         parens (separate (blank 1) [d1; minus; d2])
       else if (s = "*") then
         parens (separate (blank 1) [d1; star; d2])
       else if (s = "=") then
         separate (blank 1) [d1; equals; d2]
       else if (s = "+=") then
         separate (blank 1) [d1; plus ^^ equals; d2]
       else if (s = "-=") then
         separate (blank 1) [d1; minus ^^ equals; d2]
       else if (s = "*=") then
         separate (blank 1) [d1; star ^^ equals; d2]
       else
         fail f.loc "apps_to_doc: unsupported operator"
     else
       (* The generic case of a function being applied *)
       let rec aux d = function
         | [] -> d
         | [t] -> d ^^ decorate_trm t
         | t1 :: t2 :: tl ->
            aux (d ^^ decorate_trm t1 ^^ comma ^^ blank 1) (t2 :: tl)
       in
       string x ^^ parens (aux empty tl)
  | Trm_val v ->
     begin match v with
     | Val_prim p ->
        begin match p with
        | Prim_unop op ->
           begin match tl with
           | [t] ->
              let d = decorate_trm t in
              begin match op with
              | Unop_get when as_left_value -> d
              | Unop_get ->
                 if not !decode then
                   string "read(" ^^ d ^^ string ")"
                 else begin
                   if display_star then parens (star ^^ d) else d
                 end
              | Unop_neg -> parens (bang ^^ d)
              | Unop_bitwise_neg -> parens (tilde ^^ d)
              | Unop_opp -> parens (minus ^^ blank 1 ^^ d)
              | Unop_inc when !decode -> d ^^ twice plus
              | Unop_inc (* when not !decode *) -> string "operator++(" ^^ d ^^ string ")"
              | Unop_dec when !decode -> d ^^ twice minus
              | Unop_dec (* when not !decode *) -> string "operator--(" ^^ d ^^ string ")"
              | (Unop_struct_field_get f | Unop_struct_field_addr f) when !decode ->

                 begin match t.desc with
                 (* if t is get t' we can simplify the display *)
                 | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));
                              _}, [t']) ->
                    let d' = decorate_trm t' in
                    (* if t' was a stack-allocated variable, use t'.f *)
                    if List.mem  Mutable_var_get t.annot then parens (d' ^^ dot ^^ string f)
                    (* otherwise use t'->f instead of *t'.f *) 
                    else  (* parens (d' ^^ minus ^^ rangle ^^ string f) *)  parens (d' ^^ dot ^^ string f)
                 (* in the other cases, we simply display t.f *)
                 | _ -> (* TODO: crossing fingers *)
                     (*parens (d ^^ dot ^^ string f)*)
                    d ^^ dot ^^ string f  
                    (* TODO: line above par (d ^^ ... )

                      and at top of decorate_trm, define   let par d = optional_parens ~_avoid_parens d in

                      where let optional_parens ~_avoid_parens d = (* this one is common to the entire file *)
                         if _avoid_parens then d else parens d *)
                 end(* TODO ( *f).x  *(f.x)     is C interpreting *f.x  as *(f.x) then good else if   ( *f).x then bad
                       *)
              | Unop_struct_field_get f (* when not !decode *) ->
                  parens (d ^^ dot ^^ string f)
              | Unop_struct_field_addr f (* when not !decode *) ->
                  string "struct_access(" ^^ d ^^ comma ^^ string " " ^^ string f ^^ string ")"
              | Unop_cast ty ->
                 let dty = typ_to_doc ty in
                 parens dty ^^ blank 1 ^^ d
              end
           | _ ->
              fail f.loc "apps_to_doc: unary operators must have one argument"
           end
        | Prim_binop op ->
           begin match tl with
           | [t1; t2] ->
              let d1 = decorate_trm t1 in
              let d2 = decorate_trm t2 in
              begin match op with
              | Binop_set ->
                 if not !decode then
                   string "write(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
                 else if not is_app_and_set then
                   separate (blank 1) [d1; equals; d2]
                 else
                   begin match t2.desc with
                   | Trm_apps (f', [_; t2']) ->
                      let d2 = decorate_trm t2' in
                      begin match f'.desc with
                      | Trm_val (Val_prim (Prim_binop Binop_add)) ->
                         separate (blank 1) [d1; plus ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_sub)) ->
                         separate (blank 1) [d1; minus ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_mul)) ->
                         separate (blank 1) [d1; star ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_div)) ->
                         separate (blank 1) [d1; slash ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_mod)) ->
                         separate (blank 1) [d1; percent ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_shiftl)) ->
                         separate (blank 1) [d1; (twice langle) ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_shiftr)) ->
                         separate (blank 1) [d1; (twice rangle) ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_and)) ->
                         separate (blank 1) [d1; ampersand ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_or)) ->
                         separate (blank 1) [d1; bar ^^ equals; d2]
                      | Trm_val (Val_prim (Prim_binop Binop_xor)) ->
                         separate (blank 1) [d1; caret ^^ equals; d2]
                      | _ -> fail f.loc "apps_to_doc: bad app_and_set operator"
                      end
                   | _ -> fail f.loc "apps_to_doc: bad app_and_set annotation"
                   end
              | Binop_eq -> parens (separate (blank 1) [d1; twice equals; d2])
              | Binop_neq ->
                 parens (separate (blank 1) [d1; bang ^^ equals; d2])
              | Binop_sub -> parens (separate (blank 1) [d1; minus; d2])
              | Binop_add -> parens (separate (blank 1) [d1; plus; d2])
              | Binop_mul -> parens (separate (blank 1) [d1; star; d2])
              | Binop_mod -> parens (separate (blank 1) [d1; percent; d2])
              | Binop_div -> parens (separate (blank 1) [d1; slash; d2])
              | Binop_le ->
                 parens (separate (blank 1) [d1; langle ^^ equals; d2])
              | Binop_lt -> parens (separate (blank 1) [d1; langle; d2])
              | Binop_ge ->
                 parens (separate (blank 1) [d1; rangle ^^ equals; d2])
              | Binop_gt -> parens (separate (blank 1) [d1; rangle; d2])
              | Binop_and ->
                 parens (separate (blank 1) [d1; twice ampersand; d2])
              | Binop_bitwise_and ->
                 parens (separate (blank 1) [d1; ampersand; d2])
              | Binop_or -> parens (separate (blank 1) [d1; twice bar; d2])
              | Binop_bitwise_or -> parens (separate (blank 1) [d1; bar; d2])
              | Binop_array_cell_addr when !decode ->
                  d1 ^^ brackets (d2)
              | Binop_array_cell_addr (* when not !decode *) ->
                  string "array_access(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
              | Binop_array_cell_get ->
                 d1 ^^ brackets (d2)
              | Binop_shiftl ->
                 parens (separate (blank 1) [d1; twice langle; d2])
              | Binop_shiftr ->
                 parens (separate (blank 1) [d1; twice rangle; d2])
              | Binop_xor -> parens (separate (blank 1) [d1; caret; d2])
              end
           | _ ->
              fail f.loc "apps_to_doc: binary operators must have two arguments"
           end
        | Prim_conditional_op ->
           begin match tl with
           | [t1; t2; t3] ->
              let d1 = decorate_trm t1 in
              let d2 = decorate_trm t2 in
              let d3 = decorate_trm t3 in
              parens (separate (blank 1) [d1; qmark; d2; colon; d3])
           | _ ->
              fail f.loc
                "apps_to_doc: conditional operator must have three arguments"
           end
        | Prim_new t ->
          (* Here we assume that trm_apps has only one trm as argument *)
          let value = List.hd tl in
          string "new" ^^ blank 1 ^^ typ_to_doc t ^^ parens (decorate_trm value)
        | Prim_fetch_and_add ->
          begin match tl with 
          | [d1;d2] ->
            string "fetch_and_add" ^^ parens ((decorate_trm d1) ^^ comma ^^ blank 1 ^^ decorate_trm d2)
          | _ -> fail f.loc "apps_to_doc: fetch_and_add expects two arguments"
          end
        (* TODO: FIX ME! *)
        | Prim_atomic_get _cm -> string "atomic_get"
        | Prim_atomic_set _cm -> string "atomic_set"
        | Prim_compare_and_swap -> string "compare_and_swap"
        (* | _ -> fail f.loc "apps_to_doc: only op primitives may be applied" *)
        end
     | _ -> fail f.loc "apps_to_doc: only primitive values may be applied"
     end
   | _ -> 
      Ast_to_text.print_ast ~only_desc:true stdout f;
      fail f.loc "apps_to_doc: only functions may be applied"
and mode_to_doc (m : mode) : document = 
  match m with 
  | Shared -> string "shared"
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

and clause_to_doc (cl : clause) : document =
  match cl with
  | Default m -> string "default" ^^ parens (mode_to_doc m)
  | Shared_c vl -> string "shared" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Private vl -> string "private" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | FirstPrivate vl -> string "firstprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | LastPrivate vl -> string "lastprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  (* TODO: Fix ME! *)
  | Linear (vl, _) -> string "linear" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Reduction (ri, vl) -> string "reduction" ^^ parens (reduction_identifier_to_doc ri ^^ blank 1 ^^ colon ^^ string (Tools.list_to_string ~sep:"," ~bounds:["";""] vl))
  | Copyin vl -> string "copyin" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | CopyPrivate vl -> string "copyprivate" ^^ string ( Tools.list_to_string ~sep:"," ~bounds: ["(";")"] vl)
  | Safelen i -> string "safelen" ^^ parens (string (string_of_int i))
  | Collapse i -> string "collapse" ^^ parens (string (string_of_int i))
  | Simdlen i -> string "simdlen" ^^ parens (string (string_of_int i))
  | Aligned_c (vl, i) -> string "aligned" ^^ parens (string (Tools.list_to_string ~sep:"," ~bounds:["";""] vl) ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int i))
  | Uniform vl -> string "uniform" ^^ string (Tools.list_to_string ~sep:"," ~bounds:["(";")"] vl)
  | Inbranch -> string "inbranch"
  | NotInbranch -> string "notinbranch"
  | Nowait -> string "nowait"
  | Ordered_c -> string "ordered"
  | If e-> string "if" ^^ parens (string e)
  | Device i -> string "device" ^^ parens (string (string_of_int i))
  | NumThreads i -> string "numthreads" ^^ parens (string (string_of_int i))
  | Schedule (st, i) -> string "schedule" ^^ parens (sched_type_to_doc st ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int (i)))
  | Parallel_c -> string "parallel"
  | Sections_c -> string "sections"
  | For_c -> string "for"
  | Taskgroup_c -> string "taskgroup"

and atomic_operation_to_doc (ao : atomic_operation) : document = 
  match ao with 
  | Read -> string "read"
  | Write -> string "write"
  | Update -> string "update"
  | Capture -> string "capture"


and directive_to_doc (d : directive) : document =
  match d with 
  | Atomic ao -> string "atomic" ^^ parens (atomic_operation_to_doc ao)
  | Atomic_capture -> string "atomic" ^^ blank 1 ^^ string "capture"
  | Barrier -> string "barrier"
  | Cancel (c, cl) -> string "cancel" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ Tools.doc_list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Cancellation_point (c, cl) -> string "cancellation" ^^ blank 1 ^^ string "point" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ Tools.doc_list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Critical -> string "critical"
  | Declare_simd cl -> string "declare" ^^ blank 1 ^^ string "simd" ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Declare_reduction (ri, tvl, e, c) ->  string "declare" ^^ blank 1 ^^ string "simd" ^^ parens (
    reduction_identifier_to_doc ri ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (Tools.list_to_string ~sep:"," ~bounds:["";""] tvl) ^^
    string e ^^ clause_to_doc c)
  | Declare_target -> string "declare" ^^ blank 1 ^^ string "target"  
  | Distribute cl -> string "distribute" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Distribute_parallel_for cl -> string "distribute" ^^ string "parallel" ^^ string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl)) 
  | Distribute_parallel_for_simd cl -> string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl)) 
  | Distribute_simd -> string "distribute" ^^ blank 1 ^^ string "simd"
  | End_declare_target -> string "end" ^^ blank 1 ^^ string "declare" ^^ string "target"
  | Flush vl -> string "flush" ^^ string (Tools.list_to_string ~sep:"," vl)
  | For cl -> string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | For_simd cl -> string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Master -> string "master"
  | Ordered -> string "ordered"
  | Parallel  cl -> string "parallel" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Parallel_for -> string "parallel" ^^ blank 1 ^^ string "for"
  | Parallel_for_simd  cl -> string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Parallel_sections  cl -> string "parallel" ^^ blank 1 ^^ string "sections" ^^ blank 1  ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Section -> string "section"
  | Sections cl -> string "sections" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Simd cl -> string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Single cl -> string "single" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target cl -> string "target" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_data cl -> string "target" ^^ blank 1 ^^ string "data"  ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_enter_data  cl -> string "target" ^^ blank 1 ^^ string "enter" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_exit_data  cl -> string "target" ^^ blank 1 ^^ string "exit" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_teams cl -> string "target" ^^ blank 1 ^^ string "teams"  ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl)) 
  | Target_update cl -> string "target" ^^ blank 1 ^^ string "update" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Task cl -> string "task" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Taskgroup -> string "taskgroup"
  | Taskloop cl -> string "taskloop" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Taskloop_simd cl -> string "taskloop" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Taskwait -> string "taskwait"
  | Taskyield -> string "taskyield"
  | Teams cl -> string "teams" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_end cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "end" ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for_simd cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^(Tools.doc_list_to_doc (List.map clause_to_doc cl))
  | Threadprivate vl -> string "threadprivate" ^^ string (Tools.list_to_string vl)

and routine_to_doc (r : omp_routine) : document = 
  match r with 
  | Set_num_threads i -> string "omp_set_num_threads" ^^ parens (string (string_of_int i))
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
  (* TODO: FIX ME! *)
  | Set_schedule (_, md) -> string "omp_set_schedule" ^^ parens (string (string_of_int md))  
  | Get_schedule -> string "omp_get_schedule" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_thread_limit  -> string "omp_get_thread_limit" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_max_active_levels i -> string "omp_set_max_active_levels" ^^ parens (string (string_of_int i))
  | Get_max_active_levels -> string "omp_get_max_active_levels" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_level -> string "omp_get_level" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_ancestor_thread_num -> string "omp_get_ancestor_thread_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_team_size  -> string "omp_get_team_size" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_active_level  -> string "omp_get_active_level" ^^ lparen ^^ blank 1 ^^ rparen
  | In_final  -> string "omp_in_final" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_proc_bind  -> string "omp_get_proc_bind" ^^ lparen ^^ blank 1 ^^ rparen
  | Set_default_device i -> string "omp_set_default_device" ^^ parens (string (string_of_int i))  
  | Get_default_device -> string "omp_get_default_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_devices  -> string "omp_get_num_devices" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_teams -> string "omp_get_num_teams" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_team_num  -> string "omp_team_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Is_initial_device -> string "omp_is_initial_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Initialize_lock lck -> string "omp_initialize_lock" ^^ parens (ampersand ^^ string lck)
  | Destroy_lock lck -> string "omp_destroy_lock" ^^ parens (ampersand ^^ string lck)
  | Set_lock lck-> string "omp_set_lock" ^^ parens (ampersand ^^ string lck)
  | Set_nest_lock lck -> string "omp_set_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Unset_lock lck ->  string "omp_unset_lock" ^^ parens (ampersand ^^ string lck)
  | Unset_nest_lock lck -> string "omp_unset_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Test_lock lck -> string "omp_test_lock" ^^ parens (ampersand ^^ string lck)
  | Test_nest_lock lck -> string "omp_test_nest_lock" ^^ parens (ampersand ^^ string lck)
  | Get_wtime -> string "get_wtime" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_wtick -> string "get_wtich" ^^ lparen ^^ blank 1 ^^ rparen

let ast_to_doc (out : out_channel) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (decorate_trm t)

(* To obtain the C++ code without decoding, we temporary set the flag
   "decode" (defined at the top of this file) to false. *)
let ast_to_undecoded_doc (out : out_channel) (t : trm) : unit =
  decode := false;
  ast_to_doc out t;
  decode := true

let ast_to_string ?(ast_decode:bool=true) (t : trm) : string =
  let old_decode = !decode in
  decode := ast_decode;
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b (decorate_trm t);
  decode := old_decode;
  Buffer.contents b

let typ_to_string (ty : typ) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b (typ_to_doc ty);
  Buffer.contents b


