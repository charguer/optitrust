open PPrint
open Ast
open Tools


let rec print_typ_desc ?(only_desc : bool = false) (t : typ_desc) : document =
  match t with
  | Typ_var x -> node "Typ_var" ^^ string x
  | Typ_unit -> string "Typ_unit"
  | Typ_int -> string "Typ_int"
  | Typ_float -> string "Typ_float"
  | Typ_double -> string "Typ_double"
  | Typ_bool -> string "Typ_bool"
  | Typ_char -> string "Typ_char"
  | Typ_ptr t ->
     let dt = print_typ ~only_desc t in
     node "Typ_ptr" ^^ dt
  | Typ_array (t, s) ->
     let dt = print_typ ~only_desc t in
     let ds =
       begin match s with
       | Undefined -> underscore
       | Const n -> string (string_of_int n)
       | Trm t' -> print_trm ~only_desc t'
       end
     in
     node "Typ_array" ^^ print_pair dt ds
  | Typ_struct (tl, tm, x) ->
     let tl = List.rev tl in
     let get_typ x = Field_map.find x tm in
     let get_document_list l =
      let rec aux acc = function
      | [] -> acc
      | hd :: tl -> let t = get_typ hd in
       let dt = print_typ ~only_desc t in
       aux(print_pair (string hd) dt :: acc) tl in
       aux [] l
     in
     let dtl = get_document_list tl in
     node "Typ_struct" ^^ print_pair (print_list dtl) (string x)
  | Typ_fun (tl, t) ->
     let dtl = List.map (print_typ ~only_desc) tl in
     let dt = print_typ ~only_desc t in
     node "Typ_fun" ^^ parens (print_list dtl ^^ comma ^/^ dt)

and print_typ_annot (a : typ_annot) : document =
  match a with
  | Unsigned -> string "Unsigned"
  | Long -> string "Long"
  | Short -> string "Short"

and print_typ ?(only_desc : bool = false) (t : typ) : document =
  let ddesc = print_typ_desc ~only_desc t.ty_desc in
  if only_desc then ddesc
  else
    let dannot =
      List.fold_left (fun d a -> print_typ_annot a ^^ blank 1 ^^ d) underscore
        t.ty_annot
    in
    let dattr =
      print_list (List.map (print_attribute ~only_desc) t.ty_attributes)
    in
    braces (separate (blank 1) [string "annot"; equals;
                                dannot ^^ semi ^//^ string "desc"; equals;
                                ddesc ^^ semi ^//^ string "attributes"; equals;
                                dattr])

and print_unop ?(only_desc : bool = false) (op : unary_op) : document =
  match op with
  | Unop_get -> string "Unop_get"
  | Unop_neg -> string "Unop_neg"
  | Unop_bitwise_neg -> string "Unop_bitwise_neg"
  | Unop_opp -> string "Unop_opp"
  | Unop_inc -> string "Unop_inc"
  | Unop_dec -> string "Unop_dec"
  | Unop_struct_access f -> node "Unop_struct_access" ^^ string f
  | Unop_struct_get f -> node "Unop_struct_get" ^^ string f
  (* | Unop_delete b -> node "Unop_delete" ^^ string (string_of_bool b) *)
  | Unop_cast t ->
     let dt = print_typ ~only_desc t in
     node "Unop_cast" ^^ dt

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

and print_prim ?(only_desc : bool = false) (p : prim) : document =
  match p with
  | Prim_unop op ->
     let dop = print_unop ~only_desc op in
     node "Prim_unop" ^^ dop
  | Prim_binop op ->
     let dop = print_binop op in
     node "Prim_binop" ^^ dop
  | Prim_new t ->
     let dt = print_typ ~only_desc t in
     node "Prim_new" ^^ dt
  | Prim_conditional_op -> node "Prim_conditional_op"

and print_lit (l : lit) : document =
  match l with
  | Lit_unit -> string "Lit_unit"
  | Lit_uninitialized -> string "Lit_uninitialized"
  | Lit_bool b -> node "Lit_bool" ^^ string (string_of_bool b)
  | Lit_int n -> node "Lit_int" ^^ string (string_of_int n)
  | Lit_double f -> node "Lit_double" ^^ string (string_of_float f)
  | Lit_string s ->
     node "Lit_string" ^^ dquotes (separate (backslash ^^ string "n") (lines s))

and print_val ?(only_desc : bool = false) (v : value) : document =
  match v with
  | Val_lit l ->
     let dl = print_lit l in
     node "Val_lit" ^^ parens dl
  | Val_ptr (l, _) ->
     if l = 0 then string "NULL"
     else fail None "print_val: pointers not implemented"
  | Val_array vl ->
     let dvl = List.map (print_val ~only_desc) vl in
     node "Val_array" ^^ print_list dvl
  | Val_struct vl ->
     let dvl = List.map (print_val ~only_desc) vl in
     node "Val_struct" ^^ print_list dvl
  | Val_prim p ->
     let dp = print_prim ~only_desc p in
     node "Val_prim" ^^ parens dp

and print_attribute ?(only_desc : bool = false) (a : attribute) : document =
  match a with
  | Identifier x ->
     string "Identifier" ^^ blank 1 ^^ string x
  | Aligned t ->
     string "Aligned" ^^ blank 1 ^^ print_trm ~only_desc t

and print_trm_desc ?(only_desc : bool = false) (t : trm_desc) : document =
  match t with
  | Trm_val v ->
     let dv = print_val ~only_desc v in
     node "Trm_val" ^^ parens dv
  | Trm_var x -> string "Trm_var" ^^ blank 1 ^^ string x
  | Trm_array tl ->
     let dtl = List.map (print_trm ~only_desc) tl in
     node "Trm_array" ^^ print_list dtl
  | Trm_struct tl ->
     let dtl = List.map (print_trm ~only_desc) tl in
     node "Trm_struct" ^^ print_list dtl
  | Trm_let (vk,(x,tx),t) -> 
    let dvk = match vk with 
    | Var_immutable -> string "Var_immutable"
    | Var_mutable ->  string "Var_mutable"
    
    in
    let dtx = print_typ ~only_desc tx in 
    let dt = print_trm ~only_desc t in 
    node "Trm_let" ^^
      parens (separate (comma ^^ break 1) [dvk;string x;dt;dtx])

  | Trm_let_fun (f, r, tvl, b) ->
    let dout = print_typ ~only_desc r in 
    let dtvl = List.map(function (x,tx) -> 
          let dtx = print_typ ~only_desc tx in 
          print_pair (string x) dtx) tvl in 
    let dt = print_trm ~only_desc b in 
    node "Trm_let_fun" ^^
      parens (separate (comma ^^ break 1)
        [string f; dout; print_list dtvl; dt])
  | Trm_typedef t -> print_typedef ~only_desc t
  | Trm_if (c, t, e) ->
     let dc = print_trm ~only_desc c in
     let dt = print_trm ~only_desc t in
     let de = print_trm ~only_desc e in
     node "Trm_if" ^^ parens (separate (comma ^^ break 1) [dc; dt; de])
  | Trm_seq tl ->
     let dtl = List.map (print_trm ~only_desc) tl in
     node "Trm_seq" ^^ print_list dtl
  | Trm_apps (f, tl) ->
     let df = print_trm ~only_desc f in
     let dtl = List.map (print_trm ~only_desc) tl in
     node "Trm_apps" ^^ parens (df ^^ comma ^/^ print_list dtl)
  | Trm_while (c, b) ->
     let dc = print_trm ~only_desc c in
     let db = print_trm ~only_desc b in
     node "Trm_while" ^^ parens (dc ^^ comma ^/^ db)
  | Trm_for (init, cond, step, body) ->
     let dinit = print_trm ~only_desc init in
     let dcond = print_trm ~only_desc cond in
     let dstep = print_trm ~only_desc step in
     let dbody = print_trm ~only_desc body in
     node "Trm_for" ^^ parens (separate (comma ^^ break 1)
       [dinit; dcond; dstep; dbody])
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
     node "Trm_switch" ^^ print_pair dcond (print_list dcases)
  | Trm_abort a ->
     let da =
       begin match a with
       | Ret t_o ->
          begin match t_o with
          | None -> node "Ret" ^^ underscore
          | Some t ->
             let dt = print_trm ~only_desc t in
             node "Ret" ^^ dt
          end
       | Break -> string "Break"
       | Continue -> string "Continue"
       end
     in
     node "Trm_abort" ^^ parens da
  | Trm_labelled (l, t) ->
     let dt = print_trm ~only_desc t in
     node "Trm_labelled" ^^ parens (string l ^^ comma ^/^ dt)
  | Trm_goto l ->
     node "Trm_goto" ^^ string l
  | Trm_decoration (l,t,r) ->
      let dt = print_trm ~only_desc t in
      node "Trm_decorated" ^^ parens (string l ^^ comma ^/^ dt ^^ comma ^/^ string r)
  | Trm_any t ->
    let dt = print_trm ~only_desc t in
      node "Trm_any"  ^^ parens (dt)

and print_typedef ?(only_desc : bool = false) (t : typedef) : document =
  match t with 
  | Typedef_abbrev (x,typ) ->
    let dt = print_typ ~only_desc typ in 
    node "Typedef" ^^ print_pair (string x) dt
  | Typedef_enum (x, enum_const_l) -> 
     let denum_const_l =
       print_list
         (List.map
            (fun (y, t_o) ->
              match t_o with
              | None -> print_pair (string y) underscore
              | Some t -> print_pair (string y) (print_trm ~only_desc t)
            )
            enum_const_l
         )
     in
     node "Typedef_enum" ^^ print_pair (string x) denum_const_l

and print_trm ?(only_desc : bool = false) (t : trm) : document =
  let ddesc = print_trm_desc ~only_desc t.desc in
  if only_desc then ddesc
  else
    let dannot =
      begin match t.annot with
      | None -> underscore
      | Some a ->
         begin match a with
         (* | Heap_allocated -> string "Heap_allocated" *)
         (* | Initialisation_instruction -> string "Initialisation_instruction" *)
         (* | Delete_instructions -> string "Delete_instructions" *)
         | No_braces -> string "No_braces"
         | Access -> string "Access"
         | Multi_decl -> string "Multi_decl"
         | Empty_cond -> string "Empty_cond"
         | App_and_set -> string "App_and_set"
         | Include h -> string "Include" ^^ blank 1 ^^ string h
         | Main_file -> string "Main_file"
         | Grouped_binding -> string "Grouped_binding"
         end
      end
    in
    let dloc =
      begin match t.loc with
      | None -> underscore
      | Some (filename, start_row, end_row, start_column, end_column) ->
         print_pair (string filename) (string (string_of_int start_row ^ "," ^ string_of_int start_column ^ ": " ^ string_of_int end_row ^ "," ^ string_of_int end_column) )

      end
    in
    let dinstr = string (string_of_bool t.is_statement) in
    let add_to_doc (add : print_addition) =
      match add with
      | Add_address_of_operator -> string "Add_address_of_operator"
      | Add_star_operator -> string "Add_star_operator"
    in
    let dadd =
      brackets (List.fold_left (fun d add -> d ^^ semi ^//^ add_to_doc add)
                  empty t.add)
    in
    let dtyp =
      match t.typ with
      | None -> underscore
      | Some ty -> print_typ ~only_desc ty
    in
    let dattr =
      print_list (List.map (print_attribute ~only_desc) t.attributes)
    in
    braces (separate (blank 1) [string "annot"; equals;
                                dannot ^^ semi ^//^ string "desc"; equals;
                                ddesc ^^ semi ^//^ string "loc"; equals;
                                dloc ^^ semi ^//^ string "is_statement"; equals;
                                dinstr ^^ semi ^//^ string "add"; equals;
                                dadd ^^ semi ^//^ string "typ"; equals;
                                dtyp ^^ semi ^//^ string "attributes"; equals;
                                dattr])

let print_ast ?(only_desc : bool = false) (out : out_channel) (t : trm) : unit =
  let d = print_trm ~only_desc t in
  PPrintEngine.ToChannel.pretty 0.9 80 out d

let ast_to_string ?(only_desc : bool = false) (t : trm) : string =
  let d = print_trm ~only_desc t in
  document_to_string d
