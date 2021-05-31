open PPrint
open Ast

(* Flag to control whether we should "decode" or not the encodings
   that were performed when converting from Clang AST to our AST.
   This global reference is to be accessed only from this file. *)
let decode = ref true

(* translate an ast to a C/C++ document *)
(* todo: option to print heap allocation patterns *)


let rec typ_desc_to_doc (t : typ_desc) : document =
  match t with
  | Typ_const t -> typ_to_doc t ^^ string " " 
  | Typ_unit -> string "void"
  | Typ_int -> string "int"
  | Typ_float -> string "float"
  | Typ_double -> string "double"
  | Typ_bool -> string "bool"
  | Typ_char -> string "char"
  | Typ_ptr t -> typ_to_doc t ^^ star
  | Typ_array (t, s) ->
     let d = typ_to_doc t in
     begin match s with
     | Undefined -> d ^^ brackets empty
     | Const n -> d ^^ brackets (string (string_of_int n))
     | Trm t' -> d ^^ brackets (trm_to_doc t')
     end
  | Typ_struct (l,m, n) ->
     let get_typ x = Field_map.find x m in
     let get_document_list l =
      let rec aux acc = function
      | [] -> acc
      | hd :: tl -> let t = get_typ hd in
      aux((typed_var_to_doc (hd,t) ^^ semi) :: acc) tl in
      aux [] l
     in
     let dl = get_document_list l
     in
     string "struct" ^^ blank 1 ^^ string n ^^
       surround 2 1 lbrace (separate hardline dl) rbrace
  | Typ_fun (_, _) ->
     print_info None "typ_desc_to_doc: typ_fun not implemented\n";
     at
  | Typ_var t -> string t

and typ_annot_to_doc (a : typ_annot) : document =
  match a with
  | Unsigned -> string "unsigned"
  | Long -> string "long"
  | Short -> string "short"

and typ_to_doc (t : typ) : document =
  let d = typ_desc_to_doc t.ty_desc in
  let dannot =
    List.fold_left (fun d' a -> typ_annot_to_doc a ^^ blank 1 ^^ d') empty
      t.ty_annot
  in
  let dattr =
    match t.ty_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  dattr ^^ dannot ^^ d

and typed_var_to_doc ?(const:bool=false) (tx : typed_var) : document =
  let const_string = if const then blank 1 ^^ string "const " ^^ blank 1 else empty in
  let rec aux (t : typ) (s : size) : document * document list =
    let ds =
      match s with
      | Undefined -> brackets empty
      | Const n -> brackets (string (string_of_int n))
      | Trm t' -> brackets (trm_to_doc t')
    in
    match t.ty_desc with
    | Typ_array (t, s') ->
       let (base, bracketl) = aux t s' in
       (base, ds :: bracketl)
    | _ ->
       (typ_to_doc t, [ds])
  in
  let (x, t) = tx in
  let dattr =
    match t.ty_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  match t.ty_desc with
  | Typ_array (t, s) ->
     let (base, bracketl) = aux t s in
     dattr ^^ base ^^ blank 1 ^^ const_string ^^ string x ^^ concat bracketl
  | _ -> const_string ^^ typ_to_doc t ^^ blank 1 ^^ string x

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
  | Prim_new t -> string "new" ^^ blank 1 ^^ typ_to_doc t
  | Prim_conditional_op ->
     (* put holes to display the operator *)
     separate (blank 1) [underscore; qmark; underscore; colon; underscore]

and val_to_doc (v : value) : document =
  match v with
  | Val_lit l -> lit_to_doc l
  | Val_ptr (l, _) ->
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
  | Aligned t -> underscore ^^ string "Alignas" ^^ parens (trm_to_doc t)

(*
  semicolon = true if we need to print a semicolon after the statement
*)
and trm_to_doc ?(semicolon=false) (t : trm) : document =
  let loc = t.loc in
  let dsemi = if semicolon then semi else empty in
  let dattr =
    match t.attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
  in
  (* For printing C code, we have (see explanations in [clang_to_ast.ml],
     search for [Add_address_of_operator] and [Add_star_operator].
     TODO: figure out whether we sometimes need to introduce these
     annotation during transformations. *)
  match t.add with
  | Add_address_of_operator :: addl ->
     let d =
       trm_to_doc {desc = t.desc; annot = t.annot; loc = t.loc;
                   is_statement = t.is_statement; add = addl; typ = t.typ;
                   attributes = []}
     in
     let body = if !decode then parens (ampersand ^^ d) else d in
     dattr ^^ body ^^ dsemi
  | Add_star_operator :: addl when !decode ->
     let d =
       trm_to_doc {desc = t.desc; annot = t.annot; loc = t.loc;
                   is_statement = t.is_statement; add = addl; typ = t.typ;
                   attributes = []}
     in
     let body = if !decode then parens (star ^^ d) else d in
     dattr ^^ body ^^ dsemi
  | _ ->
     begin match t.desc with
     | Trm_val v ->
        begin match t.annot with
        (* empty condition in for loop is the value true *)
        | Some Empty_cond -> empty
        | _ -> dattr ^^ val_to_doc v
        end
     | Trm_var x -> dattr ^^ string x
     | Trm_array tl | Trm_struct tl ->
        let dl = List.map trm_to_doc tl in
        dattr ^^ braces (separate (comma ^^ blank 1) dl)
     | Trm_let (vk,tx,t) -> dattr ^^ trm_let_to_doc ~semicolon vk tx t
     | Trm_let_fun (f, r, tvl, b) -> dattr ^^ trm_let_fun_to_doc ~semicolon f r tvl b
     | Trm_typedef t -> dattr ^^ typedef_to_doc ~semicolon t
     | Trm_if (b, then_, else_) ->
        let db = trm_to_doc b in
        let dt = trm_to_doc ~semicolon:true then_ in
        begin match else_.desc with
        | Trm_val (Val_lit Lit_unit) ->
           dattr ^^ separate (blank 1) [string "if"; parens db; dt]
        | _ ->
           let de = trm_to_doc ~semicolon:true else_ in
           dattr ^^ separate (blank 1) [string "if"; parens db; dt] ^^
             hardline ^^ string "else" ^^ blank 1 ^^ de
        end
     | Trm_seq tl ->
        begin match t.annot with
        | Some Multi_decl -> dattr ^^ multi_decl_to_doc loc tl
        | Some No_braces -> (* TODO: printf stdout "warning no braces left" *)
           (* Print  NOBRACES{ t1; t2 }   *)
           let dl = List.map (trm_to_doc ~semicolon:true) tl in
           dattr ^^ separate hardline dl
        | Some Main_file ->
           let dl = List.map (trm_to_doc ~semicolon:true) tl in
           dattr ^^ separate (twice hardline) dl
        (* do not print content of included files *)
        | Some (Include _) -> empty
        | _ ->
           let dl = List.map (trm_to_doc ~semicolon:true) tl in
           dattr ^^ surround 2 1 lbrace (separate hardline dl) rbrace
        end
     | Trm_apps (f, tl) ->
        begin match t.annot with
        | Some App_and_set ->
           dattr ^^ apps_to_doc ~is_app_and_set:true f tl ^^ dsemi
        | _ ->
           (*
             do not display * operator if the operand is a heap allocated
             variable or a succession of accesses
            *)
          let display_star =
             match t.annot with
             | (Some Mutable_var_get | Some Access) when !decode -> false
             | _ -> true
           in
           dattr ^^ apps_to_doc ~display_star f tl ^^ dsemi
        end
     | Trm_while (b, t) ->
        let db = trm_to_doc b in
        let dt = trm_to_doc ~semicolon:true t in
        dattr ^^ separate (blank 1) [string "while"; parens db; dt]
     | Trm_for (init, cond, step, body) ->
        let dinit = trm_to_doc init in
        let dcond = trm_to_doc cond in
        let dstep = trm_to_doc step in
        let dbody = trm_to_doc ~semicolon:true body in
        dattr ^^ string "for" ^^ blank 1 ^^
          parens (separate (semi ^^ blank 1) [dinit; dcond; dstep]) ^^
            blank 1 ^^ dbody
     | Trm_switch (cond, cases) ->
        let dcond = trm_to_doc cond in
        let dcases =
          separate hardline
            (List.map
               (fun (tl, body) ->
                 (match tl with
                  | [] -> string "default" ^^ colon
                  | _ ->
                     (separate hardline
                        (List.map (fun t ->
                         string "case" ^^ blank 1 ^^ trm_to_doc t ^^ colon) tl)
                     )
                 ) ^^
                 nest 2 (hardline ^^ trm_to_doc ~semicolon:true body ^^
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
           | Some t -> dattr ^^ string "return " ^^ trm_to_doc t ^^ dsemi
           end
        | Break -> dattr ^^ string "break" ^^ dsemi
        | Continue -> dattr ^^ string "continue" ^^ dsemi
        end
     | Trm_labelled (l, t) ->
        let dt = trm_to_doc ~semicolon t in
        dattr ^^ string l ^^ colon ^^ nest 2 (hardline ^^ dt)
     | Trm_goto l -> dattr ^^ string "goto" ^^ blank 1 ^^ string l ^^ dsemi
     | Trm_decoration(l,t,r) ->
        let dt = trm_to_doc ~semicolon t in
        dattr ^^ string l ^^ dt ^^ string r
     | Trm_any t ->
        let dt = trm_to_doc ~semicolon t in
        dattr ^^ string "ANY" ^^ parens (dt)
     end

and trm_let_to_doc ?(semicolon : bool = true) (varkind : varkind) (tv : typed_var) (init : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dtx,d_init = match varkind with
  | Var_immutable -> typed_var_to_doc ~const:true tv, init
  | Var_mutable ->
    let (x, typ) = tv in
    let tv = 
      if not !decode then (x,typ) 
      else 
        begin match typ.ty_desc with
          | Typ_ptr tx -> (x, tx)
          | _ -> fail None "trm_let_to_doc: expected a type ptr"
        end
    in
    let init = 
      if not !decode then init  
      else begin match init.desc with
        | Trm_apps(_, [value]) -> value
        | _ -> init
      end
    in
    if not !decode
      then typed_var_to_doc tv, init (* LATER: factorize with Var_immutable *)
    else typed_var_to_doc  ~const:true tv, init
    in
  let initialisation =
    match init.desc with
    | Trm_val (Val_lit Lit_uninitialized) -> dsemi
    | _ -> blank 1 ^^ equals ^^ blank 1 ^^ trm_to_doc d_init ^^ dsemi
  in
  dtx ^^ initialisation

and trm_let_fun_to_doc ?(semicolon : bool = true) (f : var) (r : typ) (tvl : typed_var list) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let f = Str.global_replace (Str.regexp "overloaded") "operator" f in
  let argd = separate (comma ^^ blank 1) (List.map typed_var_to_doc tvl) in
  let dr = typ_to_doc r in
  begin match b.desc with
  | Trm_val (Val_lit Lit_uninitialized) ->
     separate (blank 1) [dr; string f; parens argd] ^^ dsemi
  | _ -> separate (blank 1) [dr; string f; parens argd; trm_to_doc b]
  end

and typedef_to_doc ?(semicolon : bool = true) (t : typedef) : document =
  let dsemi = if semicolon then semi else empty in
  match t with
  | Typedef_abbrev (x,t) ->
    begin match t.ty_desc with
     (* particular case for array types aliases *)
     | Typ_array _ ->
        string "typedef" ^^ blank 1 ^^ typed_var_to_doc (x, t) ^^ dsemi
     (* particular case for function pointers *)
     | Typ_ptr {ty_desc = Typ_fun (tyl, r); _} ->
        let dl = List.map typ_to_doc tyl in
        let dr = typ_to_doc r in
        separate (blank 1)
          [string "typedef"; dr;
           parens (star ^^ string x) ^^
             parens (separate (comma ^^ blank 1) dl)] ^^
        dsemi
     | _ ->
        separate (blank 1) [string "typedef"; typ_to_doc t; string x] ^^ dsemi
     end
  | Typedef_enum (x, enum_const_l) ->
      let const_doc_l =
       List.map
         (fun (y, t_o) ->
           match t_o with
           | None -> string y
           | Some t -> separate (blank 1) [string y; equals; trm_to_doc t]
         )
         enum_const_l
     in
     separate (blank 1) [string "enum"; string x;
                         braces (separate (comma ^^ blank 1) const_doc_l)] ^^
     dsemi

and multi_decl_to_doc (loc : location) (tl : trm list) : document =
  let rec get_names = function
    | [] -> []
    (* const variables *)
    | {desc = Trm_let (_,(x,_),_);_} :: tl -> x :: get_names tl
    | _ -> fail loc "multi_decl_to_doc: only variables declarations allowed"
  in
  let dtype =
    match tl with
    | [] -> fail loc "multi_decl_to_doc: empty multiple declaration"
    | {desc = Trm_let (vk,(_,ty),_);_} :: _ ->
      begin match vk with
      | Var_immutable -> string "const " ^^ blank 1 ^^ typ_to_doc ty
      | _ -> typ_to_doc ty
      end
    | _ -> fail loc "multi_decl_to_doc: only variables declarations allowed"
  in
  let dnames = separate (comma ^^ blank 1) (List.map string (get_names tl)) in
    dtype ^^ blank 1 ^^ dnames ^^ semi

(* display_star: true if f is get and we should display it *)
and apps_to_doc ?(display_star : bool = true) ?(is_app_and_set : bool = false)
  (f : trm) (tl : trm list) : document =
  match f.desc with
  | Trm_var x ->
     if !decode && Str.string_match (Str.regexp "overloaded\\(.*\\)") x 0 then
        (* Note x is for example "overloaded=" *)
       let (d1, d2) =
         begin match List.map trm_to_doc tl with
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
         | [t] -> d ^^ trm_to_doc t
         | t1 :: t2 :: tl ->
            aux (d ^^ trm_to_doc t1 ^^ comma ^^ blank 1) (t2 :: tl)
       in
       string x ^^ parens (aux empty tl)
  | Trm_val v ->
     begin match v with
     | Val_prim p ->
        begin match p with
        | Prim_unop op ->
           begin match tl with
           | [t] ->
              let d = trm_to_doc t in
              begin match op with
              | Unop_get ->
                 if not !decode then
                   string "get(" ^^ d ^^ string ")"
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
              | (Unop_struct_get f | Unop_struct_access f) when !decode ->

                 begin match t.desc with
                 (* if t is get t' we can simplify the display *)
                 | Trm_apps ({desc = Trm_val (Val_prim (Prim_unop Unop_get));
                              _}, [t']) ->
                    let d' = trm_to_doc t' in
                    begin match t.annot with
                    (* if t' was a stack-allocated variable, use t'.f *)
                    | Some Mutable_var_get -> parens (d' ^^ dot ^^ string f)
                    (* otherwise use t'->f instead of *t'.f *)
                    | _ -> parens (d' ^^ minus ^^ rangle ^^ string f)
                    end
                 (* in the other cases, we simply display t.f *)
                 | _ -> (* TODO: crossing fingers *)
                     (*parens (d ^^ dot ^^ string f)*)
                    d ^^ dot ^^ string f
                    (* TODO: line above par (d ^^ ... )

                      and at top of trm_to_doc, define   let par d = optional_parens ~_avoid_parens d in

                      where let optional_parens ~_avoid_parens d = (* this one is common to the entire file *)
                         if _avoid_parens then d else parens d *)
                 end(* TODO ( *f).x  *(f.x)     is C interpreting *f.x  as *(f.x) then good else if   ( *f).x then bad
                       *)
              | Unop_struct_get f (* when not !decode *) ->
                  parens (d ^^ dot ^^ string f)
              | Unop_struct_access f (* when not !decode *) ->
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
              let d1 = trm_to_doc t1 in
              let d2 = trm_to_doc t2 in
              begin match op with
              | Binop_set ->
                 if not !decode then
                   string "set(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
                 else if not is_app_and_set then
                   separate (blank 1) [d1; equals; d2]
                 else
                   begin match t2.desc with
                   | Trm_apps (f', [_; t2']) ->
                      let d2 = trm_to_doc t2' in
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
              | Binop_array_access when !decode ->
                  d1 ^^ brackets (d2)
              | Binop_array_access (* when not !decode *) ->
                  string "array_access(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
              | Binop_array_get ->
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
              let d1 = trm_to_doc t1 in
              let d2 = trm_to_doc t2 in
              let d3 = trm_to_doc t3 in
              parens (separate (blank 1) [d1; qmark; d2; colon; d3])
           | _ ->
              fail f.loc
                "apps_to_doc: conditional operator must have three arguments"
           end
        | Prim_new t ->
          (* Here we assume that trm_apps has only one trm as argument *)
          let value = List.hd tl in
          string "new" ^^ blank 1 ^^ typ_to_doc t ^^ parens (trm_to_doc value)
        (* | _ -> fail f.loc "apps_to_doc: only op primitives may be applied" *)
        end
     | _ -> fail f.loc "apps_to_doc: only primitive values may be applied"
     end
  | _ -> fail f.loc "apps_to_doc: only functions may be applied"

let ast_to_doc (out : out_channel) (t : trm) : unit =
  PPrintEngine.ToChannel.pretty 0.9 80 out (trm_to_doc t)

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
  PPrintEngine.ToBuffer.pretty 0.9 80 b (trm_to_doc t);
  decode := old_decode;
  Buffer.contents b

let typ_to_string (ty : typ) : string =
  let b = Buffer.create 80 in
  PPrintEngine.ToBuffer.pretty 0.9 80 b (typ_to_doc ty);
  Buffer.contents b


