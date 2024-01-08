open PPrint
open Ast
open Trm
open Typ
open Mark
open Precedence
open Tools

(* TODO: implement support for printing trm annotations when style.ast.print_annot=true *)



(* TODO: generic move to Xdoc *)
type doc = document

let key_value_to_doc (kvs : (string * doc) list) : doc =
  list_to_doc ~bounds:[lbrace; rbrace] ~sep:semi (List.map (fun (k,v) -> string k ^^ colon ^^ v ^^ hardline) kvs)

(* TODO: generic move to Xdoc *)
let option_to_doc (string_none : string) (f : 'a -> doc) (opt : 'a option) : doc =
  match opt with
  | None -> string string_none
  | Some x -> f x

(*----------------------------------------------------------------------------------*)
(* Options for printing *)

(** Style object controlling printing options *)
type style = {
  ast: Ast.style;
  optitrust_syntax: bool; (* print "set(p,v)" instead of "p=v", and print "array_access(t,i)" etc *)
  pretty_matrix_notation: bool; (* print t[MINDEX(n,m,a,b)] as t[a][b] *)
  commented_pragma: bool; (* comment out pragram lines, for better tabulation by clang-format *)
}

(** Default style, depends on the global flags *)
let default_style () : style =
  let s = Ast.default_style() in
  { ast = s;
    optitrust_syntax = !Flags.print_optitrust_syntax;
    pretty_matrix_notation = !Flags.pretty_matrix_notation;
    commented_pragma = !Flags.use_clang_format; }

(** Style for reparsing *)
let style_for_reparse () : style =
  { ast = Ast.style_for_reparse();
    optitrust_syntax = false;
    pretty_matrix_notation = false;
    commented_pragma = false; }

(*----------------------------------------------------------------------------------*)
(* An optional memoization table that maps a [stringreprid] of a term
   to the [document] obtained by executing [trm_to_doc style] on it.
   If [stringreprs] is not [None], the stringreprs table is filled in
   during the calls to [trm_to_doc style], for terms that bear a [Annot_stringreprid].
   Make sure to invoke [label_subterms_with_fresh_stringreprids] on the
   AST being printed, and preferably to reinitialize the stringreprs
   table before starting. *)

(* [stringreprids]: Hashtable for storing the string representation of trm. *)
type stringreprs = (Ast.stringreprid, document) Hashtbl.t

(* [stringreprs]: string representations are stored only when the user uses string matching targets. *)
let stringreprs : stringreprs option ref = ref None

(* [clear_string_reprs ()]: clearn all the stored string representations. *)
let clear_stringreprs () =
  stringreprs := None

(* [get_stringreprs ()]: gets the current string representations. *)
let get_stringreprs () : stringreprs =
  match !stringreprs with
  | Some m -> m
  | None -> failwith "AstC_to_c.get_stringreprs: must call init_stringreprs or set_stringreprs first"

(* [get_and_clear_stringreprs ()]: gets the current string representations and delete them. *)
let get_and_clear_stringreprs () : stringreprs =
  let m = get_stringreprs() in
  stringreprs := None;
  m

(* [set_stringreprs t]: replaces the current string representations with [t]. *)
let set_stringreprs (t : stringreprs) : unit =
  stringreprs := Some t

(* [init_string_reprs]: creates the hashtable used for storing the string representations. *)
let init_stringreprs () =
  stringreprs := Some (Hashtbl.create 100)

(* [add_stringreprs_entry t d]: adds trm [t] into the table of representations. *)
let add_stringreprs_entry (t : trm) (d : document) : unit =
  match !stringreprs with
  | None -> ()
  | Some m ->
      match trm_get_stringreprid t with
      | None -> ()
      | Some id -> Hashtbl.add m id d

(* [print_stringreprs m]: for debugging purposes. *)
let print_stringreprs (m : stringreprs) : unit =
  let pr id d =
    Printf.printf "stringreprs[%d] = %s\n----\n" id (document_to_string d) in
  Printf.printf "====<stringreprs>====\n";
  Hashtbl.iter pr m;
  Printf.printf "====</stringreprs>====\n"

(*----------------------------------------------------------------------------------*)


(* To print back ast to C/C++ code, we convert all the ast components into  pprint documents.
   As a result, a well structured code will be generated. For further improving the printing
   one can use clang-format.  *)

(* **********************************************************************************************************
 * Note: to convert the OptiTrust ast to C/C++ code, we convert all the ast components into pprint documents.
 * As as result, a well structured code will be generated. One can apply clang-format to restructure the code
 * based on well known conventions.

**************************************************************************************************************)

(* [typ_desc_to_doc t]: converts ast type descriptions to pprint documents. *)
(* TODO: for types of the form   int[x]  the x is printed
        using the default style *)
let rec typ_desc_to_doc ?(is_injected : bool = false) (t : typ_desc) : document =
  match t with
  | Typ_const t when is_typ_ptr t -> typ_to_doc t ^^ string " const"
  | Typ_const t -> string " const "  ^^ typ_to_doc t
  | Typ_constr (tc, tid, args) ->

    let d_args = if args = [] || is_injected
      then empty
      else begin match args with
      | [{typ_desc = Typ_unit; _}] ->
        langle ^^ rangle
      | _ -> langle ^^ list_to_doc ~sep:comma ~bounds:[empty; empty] (List.map typ_to_doc args) ^^ rangle
      end in

    typconstr_to_doc tc ^^ d_args
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
    | Ptr_kind_mut -> typ_to_doc t ^^ star
    | Ptr_kind_ref -> typ_to_doc t ^^ ampersand
    end
  | Typ_array (t, s) ->
     let d = typ_to_doc t in
     begin match s with
     | Undefined -> d ^^ brackets empty
     | Const n -> d ^^ brackets (string (string_of_int n))
     | Trm t' -> d ^^ brackets (decorate_trm (default_style()) t')

     end
  | Typ_fun _ ->
     print_info None "AstC_to_c.typ_desc_to_doc: typ_fun not implemented\n";
     at
  | Typ_var (name, _) -> string name
  | Typ_record (rt, n) ->
    let d = typ_to_doc n in
    let drt = record_type_to_doc rt in
    drt ^^ blank 1 ^^ d
  | Typ_template_param n ->
    string n
  | Typ_arbitrary a_kind ->
        begin match a_kind with
        | Atyp ty -> string ty
        | _ -> failwith "AstC_to_c.typ_to_doc: arbitrary types entered as string should be entered by using Atyp"
        end
  | Typ_decl t ->
    string "decltype" ^^ parens (decorate_trm (default_style()) t)

and var_to_doc style (v : var) : document =
  if style.ast.print_var_id then
    Ast_to_text.print_ast_var style.ast v
  else
    (concat_map (fun q -> string q ^^ string "::") v.qualifier) ^^
    (string v.name)

and typconstr_to_doc ((qualifier, name) : typconstr) : document =
  (concat_map (fun q -> string q ^^ string "::") qualifier) ^^
  (string name)

(* [typ_annot_to_doc]: converts type annotations to pprint document. *)
and typ_annot_to_doc (a : typ_annot) : document =
  match a with
  | Unsigned -> string "unsigned"
  | Long -> string "long"
  | Short -> string "short"

(* [typ_to_doc]: converts ast types to pprint document. *)
and typ_to_doc (t : typ) : document =
  let is_injected = typ_has_attribute Injected t in
  let d = typ_desc_to_doc ~is_injected t.typ_desc in
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

(* [typed_var_to_doc style tx]: pairs like (x, int) are printed as int x. *)
and typed_var_to_doc: 'a. style -> ('a -> document) -> ('a * typ) -> document = fun style x_to_doc -> fun tx ->
  let (x, ty) = tx in
  let is_const = is_typ_const ty in
  let const_string = if is_const then blank 1 ^^ string " const " ^^ blank 1 else empty in
  let rec aux (t : typ) (s : size) : document * document list =
    let ds =
      match s with
      | Undefined -> brackets empty
      | Const n -> brackets (string (string_of_int n))
      | Trm t' -> brackets (decorate_trm style t')
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
     dattr ^^ const_string ^^ base ^^ blank 1 ^^ x_to_doc x ^^ concat bracketl
  | Typ_fun (tyl, ty) ->
    let ret_type = typ_to_doc ty in
    let arg_types = List.map typ_to_doc tyl in
    dattr ^^ ret_type ^^ blank 1 ^^ x_to_doc x ^^ (list_to_doc ~sep:comma ~bounds:[lparen; rparen] arg_types)
  | Typ_ptr _ ->
     dattr ^^ typ_to_doc ty ^^ blank 1 ^^ const_string ^^ x_to_doc x
  | _ -> const_string ^^ typ_to_doc ty ^^ blank 1 ^^ x_to_doc x

(* [lit_to_doc style l]: converts literals to pprint documents. *)
and lit_to_doc style (cstyles: cstyle_annot list) (typ : typ option) (l : lit) : document =
  match l with
  | Lit_unit -> semi
  | Lit_uninitialized -> empty
  | Lit_bool b -> string (string_of_bool b)
  | Lit_int i -> string (string_of_int i)
  | Lit_double f ->
    begin match typ with
    | Some { typ_desc = Typ_float; _ } ->
      string ((string_of_float f) ^ "f")
    | _ ->
      string (string_of_float f)
    end
  | Lit_string s -> dquotes (separate (backslash ^^ string "n") (lines s))
  | Lit_nullptr ->
      if List.mem Display_null_uppercase cstyles
        then string "NULL"
        else string "nullptr"

(* [unop_to_doc style op]: converts unary operators to pprint documents. *)
and unop_to_doc style (op : unary_op) : document =
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

(* [binop_to_doc style op]: converts binary operators to pprint documents. *)
and binop_to_doc style (op : binary_op) : document =
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
  (* FIXME: should not be used *)
  | Binop_exact_div -> string "exact_div"
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

(* [prim_to_doc style p]: converts primitives to pprint documents. *)
and prim_to_doc style (p : prim) : document =
  match p with
  | Prim_unop op -> unop_to_doc style op
  | Prim_binop op -> binop_to_doc style op
  | Prim_compound_assgn_op op -> (binop_to_doc style op) ^^ equals
  | Prim_overloaded_op p -> prim_to_doc style p
  | Prim_new t -> string "new" ^^ blank 1 ^^ typ_to_doc t
  | Prim_conditional_op -> separate (blank 1) [underscore; qmark; underscore; colon; underscore]

(* [val_to_doc style v]: converts values to pprint documents. *)
and val_to_doc style (cstyles : cstyle_annot list) (typ : typ option) (v : value) : document =
  match v with
  | Val_lit l -> lit_to_doc style cstyles typ l
  | Val_prim p -> prim_to_doc style p

(* [attr_to_doc a]: converts attributes to pprint documents. *)
and attr_to_doc (a : attribute) : document =
  match a with
  | Alignas t -> string "alignas" ^^ parens (decorate_trm (default_style()) t)
  | GeneratedTyp | Injected -> empty
  | Others -> empty

(* [decorate_trm style ~semicolon ~prec ~print_struct_init_type t]:
    - if [prec] is greater than the precedence of [t] then decorate [t] with parentheses
    - if [t] is marked,then decorate [t] with those marks.
    - if t is struct initialization list outside a variable declaration, then decorate [t] with a cast in front
    - if [semicolon] is true then decorate [t] with a semicolon at the end *)
and decorate_trm style ?(semicolon : bool = false) ?(prec : int = 0) ?(print_struct_init_type : bool = true) (t : trm) : document =
  let parentheses = parentheses_needed ~prec t in
  let dt = trm_to_doc style ~semicolon ~prec ~print_struct_init_type t in

  let t_pragmas = trm_get_pragmas t in
  let dpragmas = if t_pragmas = []
    then empty
    else
      let t_pragmas_str = List.map (fun d ->
        let intro = if style.commented_pragma then string "//" else empty in
          intro ^^ sharp ^^ string "pragma" ^^ blank 1 ^^ string "omp" ^^ blank 1 ^^ directive_to_doc d
        ) t_pragmas in



  list_to_doc ~sep:(string "\n") ~bounds:[empty; hardline] t_pragmas_str in

  let t_labels = trm_get_labels t in
  let dlabels = if t_labels = []
    then empty
    else
      let t_labels_str = List.map string t_labels in
      list_to_doc ~sep:(colon ^^ blank 1) ~bounds:[empty; colon] t_labels_str
    in

  let dt = if parentheses then parens (dt) else dpragmas ^^ dlabels ^^ dt in

  let t_marks = trm_get_marks t in

  if style.ast.print_annot then failwith "NOT YET IMPLEMENTED: ASTC_TO_C printing of annotations";

  let dt =
    if t_marks = [] || not style.ast.print_string_repr then
      dt
    else begin
      let sid =
        if not style.ast.print_string_repr then "" else begin
        match Trm.trm_get_stringreprid t with
        | None -> "[-]"
        | Some id -> Printf.sprintf "[%d]" id
        end in
      let smarks =
        if style.ast.print_mark
          then list_to_string ~sep:", " ~bounds:["";""] t_marks
          else "" in

      let sleft = string ("/*@" ^ sid ^ smarks ^ "*/") in
      let sright =  string ("/*" ^ sid ^ smarks ^ "@*/") in
      sleft ^^ dt ^^ sright
    end
    in
  (*printf "ERRORS L=%d\n" (List.length t.errors);*)
  if t.errors = [] || not style.ast.print_errors then
    dt
  else
    list_to_doc ~sep:hardline
      ~bounds:[(string "/*" ^^ hardline);
                (hardline ^^ string "*/" ^^ hardline )]
      (List.map (fun error -> string (sprintf "ERROR: %s" error)) t.errors)
    ^^ dt


(* [trm_var_to_doc style v t]: pretty prints trm_vars, including here type arguments and nested name specifiers. *)
and trm_var_to_doc style (x : var) (t : trm) : document =
  let typ_args = get_typ_arguments t in
  let typ_args_d = List.map typ_to_doc typ_args in
  let typ_args_d = begin match typ_args_d with | [] -> empty | _ -> list_to_doc ~sep:comma ~bounds:[langle; rangle] typ_args_d end in
  var_to_doc style x ^^ typ_args_d

(* [trm_to_doc style ~semicolon ~prec ~print_struct_init_type  t]: converts [t] to a pprint document *)
and trm_to_doc style ?(semicolon=false) ?(prec : int = 0) ?(print_struct_init_type : bool = false) (t : trm) : document =
  let loc = t.loc in
  let dsemi = if semicolon then semi else empty in
  let t_attributes = trm_get_attr t in
  let dattr =
    match t_attributes with
    | [] -> empty
    | al -> separate (blank 1) (List.map attr_to_doc al) ^^ blank 1
    in
  let d =
    begin match t.desc with
    | Trm_val v ->
      if trm_has_cstyle Empty_cond t
        then empty
        else dattr ^^ val_to_doc style (trm_get_cstyles t) (t.typ) v
    | Trm_var (_, x) ->
      (* if x.qvar_var = "this"
        then
          if trm_has_cstyle Implicit_this t
            then empty
            else string "this"
      else  *)
      let var_doc = trm_var_to_doc style x t in
      dattr ^^ var_doc
    | Trm_array tl -> let tl = Mlist.to_list tl in
      let dl = List.map (decorate_trm style ~semicolon ~print_struct_init_type:false) tl in
      dattr ^^ braces (separate (comma ^^ blank 1) dl)
    | Trm_record tl ->
      let tl = Mlist.to_list tl in
      let dec_trm (t : trm) = decorate_trm style ~print_struct_init_type:false ~semicolon t in
      let dl = List.map (fun (lb_opt, t1) ->
        match lb_opt with
        | Some lb -> dot ^^ string lb ^^ equals  ^^ dec_trm t1
        | None -> dec_trm t1
      ) tl in
      let init_type = if not print_struct_init_type
        then empty
        else begin match t.typ with
        | Some ty ->
          begin match ty.typ_desc with
          | Typ_constr (_, tid, _) when tid <> -1 -> parens(typ_to_doc ty)
          | _ -> empty
          end
        | None -> empty
        end
      in
      dattr ^^ init_type ^^ blank 1 ^^  braces (separate (comma ^^ blank 1) dl)
    | Trm_let (_,tx,t) -> dattr ^^ trm_let_to_doc style ~semicolon tx t
    | Trm_let_mult (_, tvl, tl) -> dattr ^^ trm_let_mult_to_doc style ~semicolon tvl tl
    | Trm_let_fun (f, r, tvl, b, _) ->
      let fun_annot = trm_get_cstyles t in
      let static = if trm_has_cstyle Static_fun t then string "static" else empty in
      dattr ^^ static ^^ blank 1 ^^ trm_let_fun_to_doc style ~semicolon fun_annot f r tvl b
    | Trm_typedef td ->
      let t_annot = trm_get_cstyles t in
      dattr ^^ typedef_to_doc style ~semicolon ~t_annot td
    | Trm_if (b, then_, else_) ->
      let db = decorate_trm style ~semicolon:false b in
      let dt = decorate_trm style ~semicolon:true then_ in
      begin match else_.desc with
      | Trm_val (Val_lit Lit_unit) ->
        dattr ^^ separate (blank 1) [string "if"; parens db; dt]
      | _ ->
        let de = decorate_trm style ~semicolon:true else_ in
        dattr ^^ separate (blank 1) [string "if"; parens db; dt] ^^
          hardline ^^ string "else" ^^ blank 1 ^^ de
      end
    | Trm_seq tl ->
      let tl_m = tl.marks in
      let tl = Mlist.to_list tl in
      let tl = List.filter (fun t -> not (trm_has_cstyle Redundant_decl t)) tl in
      if trm_has_cstyle Multi_decl t
        then dattr ^^ multi_decl_to_doc style loc tl
        else if trm_is_nobrace_seq t
          then
          let dl = List.map (decorate_trm style ~semicolon:true) tl in
          (* CHECK: #var-id, made nobrace visible *)
          (* DEBUG: let nb = string (sprintf "/*no-brace %d*/" (Option.get (Nobrace.get_id t))) in *)
          let nb = string "/*no-brace*/" in
          nb ^^ surround 2 1 lbrace (dattr ^^ separate hardline dl) rbrace
        else if (not !Flags.display_includes) && (trm_is_include t)
        then empty
        else
          let counter = ref (-1) in
          let dl = List.map (decorate_trm style ~semicolon:true) tl in
          let dl = Xlist.fold_lefti (fun i acc m ->
            if m <> [] then begin
              incr counter;
              let m = list_to_string ~sep:", " m in
              let s = string ("/*@" ^ m ^ "@*/") in
              Xlist.insert_at (i + !counter) s acc end
            else acc
          ) dl tl_m in
          counter := -1;
          let res = if trm_is_mainfile t then
            let header = if style.pretty_matrix_notation
              then (string "// NOTE: using pretty matrix notation") ^^ hardline
              else empty in
            header ^^ (separate (twice hardline) dl)
          else surround 2 1 lbrace (separate hardline dl) rbrace in
          dattr ^^ res
    | Trm_apps _ when trm_has_cstyle ResourceFormula t ->
      dattr ^^ formula_to_doc style t ^^ dsemi
    | Trm_apps (f, tl, _) ->
      dattr ^^ apps_to_doc style ~prec f tl ^^ dsemi
    | Trm_while (b, t) ->
      let db = decorate_trm style b in
      let dt = decorate_trm style ~semicolon:true t in
      dattr ^^ separate (blank 1) [string "while"; parens db; dt]
    | Trm_do_while (t, b) ->
      let dt = decorate_trm style t in
      let db = decorate_trm style b in
      dattr ^^ string "do " ^^ dt ^^ blank 1 ^^ string "while " ^^ parens db ^^ semi
    | Trm_for_c (init, cond, step, body, _) ->
      let dinit = decorate_trm style init in
      let dcond = decorate_trm style cond in
      let dstep = decorate_trm style step in
      let dbody = decorate_trm style ~semicolon:true body in
      dattr ^^ string "for" ^^ blank 1 ^^
        parens (separate (semi ^^ blank 1) [dinit; dcond; dstep]) ^^
          blank 1 ^^ dbody
    | Trm_for (l_range, body, loop_spec) ->
      let full_loop = (unpack_trm_for : ?loc:trm_loc -> loop_range -> trm -> trm) ?loc:t.loc l_range body in
      let dt = decorate_trm style full_loop in
      if style.ast.print_contract_internal_repr
        then string "/*" ^^ string "Contract: " ^^ loop_spec_to_doc style loop_spec ^^ string "*/" ^^ hardline ^^ dt
        else dt
    | Trm_switch (cond, cases) ->
      let dcond = decorate_trm style cond in
      let dcases =
        separate hardline
          (List.map
              (fun (tl, body) ->
                (match tl with
                | [] -> string "default" ^^ colon
                | _ ->
                    (separate hardline
                      (List.map (fun t ->
                        string "case" ^^ blank 1 ^^ decorate_trm style t ^^ colon) tl)
                    )
                ) ^^
                nest 2 (hardline ^^ decorate_trm style ~semicolon:true body ^^
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
          | Some t -> dattr ^^ string "return " ^^ decorate_trm style ~print_struct_init_type:true t ^^ dsemi
          end
      | Break _ -> dattr ^^ string "break" ^^ dsemi
      | Continue _ -> dattr ^^ string "continue" ^^ dsemi
      end
    | Trm_goto l -> dattr ^^ string "goto" ^^ blank 1 ^^ string l ^^ dsemi
    | Trm_arbitrary a_kind  ->
      let code_str =
      begin match a_kind with
      | Lit l -> string l
      | Expr e -> string e
      | Stmt s -> string s
      | Instr s -> string s ^^ semi
      | _ -> trm_fail t "AstC_to_c.trm_to_doc style: arbitrary code should be entered by using Lit, Expr and Stmt only"
      end  in
      dattr ^^ code_str
    | Trm_omp_routine  r -> dattr ^^ routine_to_doc r
    | Trm_extern (lang, tl) ->
        begin match tl with
        | [t1] ->
          let dt = decorate_trm style ~semicolon:true t1 in
          dattr ^^ string "extern " ^^ string lang ^^ blank 1 ^^ dt
        | _ ->
          let dl = List.map (decorate_trm style ~semicolon:true) tl in
          dattr ^^ string "extern " ^^ string lang ^^ blank 1^^surround 2 1 lbrace (separate hardline dl) rbrace
        end
    | Trm_namespace (name, t1, inline) ->
      let inline = if inline then string "inline" else empty in
      let dt = decorate_trm style ~semicolon:true t1 in
      dattr ^^ inline ^^ string "namespace" ^^ blank 1 ^^ string name ^^  blank 1 ^^ dt
    | Trm_using_directive nmspc -> string "using namespace " ^^ string nmspc ^^ semi
    | Trm_template (tpl, t1) ->
      let dl = decorate_trm style t1 in
      let dtpl = List.map (fun (n, tpk, _) ->
        match tpk with
        | Type_name typ_opt ->
          begin match typ_opt with
          | Some ty -> string "typename " ^^ string n ^^ equals ^^ typ_to_doc ty
          | None -> string "typename " ^^ string n
          end
        | NonType (ty, t_opt) ->
          begin match t_opt with
          | Some t1 -> typ_to_doc ty ^^ blank 1 ^^ string n ^^ equals ^^ decorate_trm style t1
          | None -> typ_to_doc ty ^^ blank 1 ^^ string n
          end
        | Template _ -> trm_fail t1 "AstC_to_c.template_param_kind_to_doc: nested templates are not supported"
        ) tpl in
      string "template" ^^ blank 1 ^^ (list_to_doc ~sep:comma ~bounds:[langle;rangle] dtpl) ^^ dl
    | Trm_fun (tvl, ty_opt, body, _) when trm_has_cstyle ResourceFormula t -> dattr ^^ formula_fun_to_doc style ~semicolon ty_opt tvl body
    | Trm_fun (tvl, ty_opt, body, _) -> dattr ^^ trm_fun_to_doc style ~semicolon ty_opt tvl body
     (* DEPRECATED | Trm_this ->
        if trm_has_cstyle Implicit_this t then empty else string "this" *)
    | Trm_delete (is_array, body) ->
      let is_arr = if is_array then string "[]" else empty in
      let dbody = decorate_trm style body in
      string "delete" ^^ is_arr ^^ blank 1 ^^ dbody ^^ semi
    end in
  (* Save the result in the optional stringreprs table, before returning the document *)
  add_stringreprs_entry t d;
  d

(* [record_type_to_doc rt]: converts a C++ record type to a pprint document *)
and record_type_to_doc (rt : record_type) : document =
  match rt with
  | Struct -> string "struct"
  | Union -> string "union"
  | Class -> string "class"


(* [trm_let_to_doc style ~semicolon tv init]: converts a variable declaration to print document *)
and trm_let_to_doc style ?(semicolon : bool = true) (tv : typed_var) (init : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dtx = typed_var_to_doc style ((var_to_doc style)) tv in
 (* DEPREACTED
  let dinit = begin match init.desc with
    | Trm_val (Val_lit Lit_uninitialized) -> dsemi
    | Trm_apps (_, args) when trm_has_cstyle Constructed_init init ->
      list_to_doc ~bounds:[lparen; rparen] ~sep:comma (List.map (trm_to_doc style) args) ^^ dsemi
    | _ ->
      equals ^^ blank 1 ^^ decorate_trm style ~print_struct_init_type:false init ^^ dsemi
  end in
  dtx ^^ blank 1 ^^ dinit
  *)
  match init.desc with
  | Trm_val (Val_lit Lit_uninitialized) -> dtx ^^ semi
  | Trm_apps (_, args, _) when trm_has_cstyle Constructed_init init ->
    dtx ^^ blank 1 ^^ list_to_doc ~bounds:[lparen; rparen] ~sep:comma (List.map (decorate_trm style) args) ^^ dsemi
  | Trm_array tl when trm_has_cstyle Brace_init init ->
      let tl = Mlist.to_list tl in
      dtx ^^ list_to_doc ~bounds:[lbrace; rbrace] ~sep:empty (List.map (decorate_trm style) tl) ^^ dsemi
  | _ ->
    dtx ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm style ~print_struct_init_type:false init ^^ dsemi

and loop_spec_to_doc style (loop_spec : loop_spec) : doc =
  option_to_doc "<no_loop_spec>" (loop_contract_to_doc style) loop_spec

and loop_contract_to_doc style (loop_contract : loop_contract) : doc =
  key_value_to_doc [
    "loop_ghosts", resource_item_list_to_doc style loop_contract.loop_ghosts;
    "invariant", resource_set_to_doc style loop_contract.invariant;
    "iter_contract", fun_contract_to_doc style loop_contract.iter_contract; ]

and fun_contract_to_doc style (fun_contract : fun_contract) : doc =
  key_value_to_doc [
    "pre", resource_set_to_doc style fun_contract.pre;
    "post", resource_set_to_doc style fun_contract.post; ]

and resource_item_list_to_doc style (resource_item_list : resource_item list) : doc =
  list_to_doc ~bounds:[lbracket; rbracket] ~sep:semi
    (List.map (resource_item_to_doc style) resource_item_list)

and resource_set_to_doc style (resource_set : resource_set) : doc =
  key_value_to_doc [
    "pure", resource_item_list_to_doc style resource_set.pure;
    "linear", resource_item_list_to_doc style resource_set.linear;
    "fun_specs", fun_spec_resource_varmap_to_doc style resource_set.fun_specs; ]

and resource_item_to_doc style (resource_item : resource_item) : doc =
  let hyp, formula = resource_item in
  let sid =
    if hyp.name.[0] = '#' && not style.ast.print_generated_ids
      then empty
      else string (hyp.name ^ ": ") in
  sid ^^ formula_to_doc style formula

and fun_spec_resource_varmap_to_doc style (fun_specs : fun_spec_resource varmap) : doc =
  string "<fun_spec for: " ^^
  (list_to_doc ~bounds:[lbracket; rbracket] ~sep:semi (List.map (fun (f, s) -> var_to_doc style f) (Var_map.bindings fun_specs))) ^^
  string ">"



(* [trm_let_mult_to_doc style ~semicolon tv vl tl]: converts multiple variable declarations to pprint document *)
and trm_let_mult_to_doc style ?(semicolon : bool = true) (tvl : typed_vars) (tl : trm list) : document =
  let rec get_inner_ptrs_and_consts (ty : typ) : typ =
    match ty.typ_desc with
    | Typ_const ({typ_desc = Typ_ptr { inner_typ = ty; _ }; _ }) -> get_inner_ptrs_and_consts ty
    | Typ_ptr { inner_typ = ty; _ } -> get_inner_ptrs_and_consts ty
    | Typ_array (ty, _) -> get_inner_ptrs_and_consts ty
    | _ -> ty
  in

  let get_ptrs_and_consts_doc (ty : typ) : document =
    let rec aux (l : document list) (ty : typ) : document list =
      match ty.typ_desc with
      | Typ_const (ty) when is_typ_ptr ty -> aux ((string "const ") :: l) ty
      | Typ_ptr {ptr_kind = pt_k; inner_typ = ty} ->
        let d = begin match pt_k with
        | Ptr_kind_mut -> star
        | Ptr_kind_ref -> ampersand
        end in
        aux (d :: l) ty
      | _ -> l
    in
    List.fold_left (fun d d1 -> d ^^ d1 ) empty (aux [] ty)
  in

  (* check if all the declarations are of the same type *)
  let ty = Xlist.fold_lefti (fun i acc (x, ty) ->
    let ty = get_inner_ptrs_and_consts ty in
    if i = 0 then ty
    else if ty <> acc then
      failwith "AstC_to_c.trm_let_mult_to_doc style: all variables in trm_let_mult must have the same type."
    else acc
    ) (typ_unit ()) tvl
  in

  let dsemi = if semicolon then semi else empty in
  let dtx = typ_to_doc ty in
  let dtl = List.map2 (fun (v,ty) t ->
  let ptr_ = get_ptrs_and_consts_doc ty in
    if is_trm_uninitialized t
      then ptr_ ^^ var_to_doc style v
      else ptr_ ^^ var_to_doc style v ^^ equals ^^ decorate_trm style t
  ) tvl tl in
  dtx  ^^ blank 1 ^^ list_to_doc ~sep:comma ~bounds:[empty; empty] dtl ^^ dsemi


(* [aux_class_constructor_to_doc style ]: converst class constructor declaration to pprint document. *)
(* DEPRECATED? _semicolon *)
and aux_class_constructor_to_doc style ?(_semicolon : bool = false)  (spec_annot  : cstyle_annot list) (name : var) (args : typed_vars) (init_l : trm list) (body : trm) : document =
  (* Deprecated? let dsemi = if semicolon then semi else empty in*)
  let argd = if List.length args = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) args) in
  let spec_annot = List.fold_left (fun acc c_annot -> match c_annot with | Class_constructor ck -> ck :: acc | _ -> acc) [] spec_annot in

  let spec_annot = if List.length spec_annot = 1 then List.nth spec_annot 0 else trm_fail body "astC_to_c.trm_class_constructor_to_doc: catastrophic error" in
  let explicit = ref empty in
  let bd, init_list = filter_out_from_seq (fun t -> trm_has_cstyle Member_initializer t ) body in
  let tr_inits =
    if List.length init_list = 0
      then []
      else
        List.map (fun t1 ->
          match set_struct_get_inv t1 with
          | Some (this, f, v) ->
            let d_v = decorate_trm style v in
            let init_arg = if trm_has_cstyle Brace_init v then d_v else parens(d_v) in
            string f ^^ init_arg
          | None -> string "bad member initializer"
              (* TODO: Debug  *)
            (* trm_fail t1 "AstC_to_c.aux_class_constructor_to_doc style: bad member initializer." *)

        ) init_list in
  let init_d = Tools.list_to_doc ~sep:comma tr_inits in
  let init_d = if init_d = empty then init_d else colon ^^ init_d in
  (* let init_d = empty in  *)
  let dt = match spec_annot with
    | Constructor_implicit -> equals ^^ blank 1 ^^ string  "implicit"
    | Constructor_default -> equals ^^ blank 1 ^^ string "default"
    | Constructor_explicit -> explicit := string "explicit"; decorate_trm style bd
    | Constructor_simpl ->  decorate_trm style bd
   in
  (separate (blank 1) [!explicit; var_to_doc style name; parens argd; init_d; dt])

(* [aux_class_destructor_to_doc style ]: converst class constructor declaration to pprint document. *)
and aux_class_destructor_to_doc style ?(semicolon : bool = false)  (spec_annot  : cstyle_annot list) (name : var) (body : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let spec_annot = List.fold_left (fun acc c_annot -> match c_annot with | Class_destructor dk -> dk :: acc | _ -> acc) [] spec_annot in
  let spec_annot = if List.length spec_annot = 1 then List.nth spec_annot 0 else failwith "astC_to_c.trm_class_constructor_to_doc: catastrophic error" in
  let dt = match spec_annot with
    | Destructor_default -> equals ^^ blank 1 ^^ string "default"
    | Destructor_delete -> equals ^^ blank 1 ^^ string "delete"
    | Destructor_simpl -> decorate_trm style body

   in
  (separate (blank 1) [tilde; var_to_doc style name; lparen ^^ rparen; dt]) ^^ dsemi


(* [aux_fun_to_doc style ~semicolon inline f r tvl b]: converts a function declaration to pprint document *)
and aux_fun_to_doc style ?(semicolon : bool = false) ?(const : bool = false) ?(inline : bool = false) (f : var) (r : typ) (tvl : typed_vars) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dinline = if inline then string "inline" else empty in
  let f = { f with name = string_subst "overloaded" "operator" f.name } in
  let argd = if List.length tvl = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) tvl) in
  let dr = typ_to_doc r in
  let const = if const then string "const" else empty in
  if is_trm_uninitialized b
    then (separate (blank 1) [dinline; dr; var_to_doc style f; parens argd; const]) ^^ dsemi
    else separate (blank 1) [dinline; dr; var_to_doc style f; parens argd; const; decorate_trm style b]

(* [trm_let_fun_to_doc style]: converts any OptiTrust function declaration(definition) to a pprint document. *)
and trm_let_fun_to_doc style ?(semicolon : bool = false) (fun_annot : cstyle_annot list) (f : var) (r : typ) (args : typed_vars) (b : trm) : document =
  if List.exists (function  | Class_constructor _ -> true | _ -> false ) fun_annot
    then aux_class_constructor_to_doc style fun_annot f args [] b
    else if List.exists (function  | Class_destructor _ -> true | _ -> false ) fun_annot
      then aux_class_destructor_to_doc style ~semicolon fun_annot f b
    else
      let inline = List.mem Fun_inline fun_annot in
      let const = List.mem Const_method fun_annot in
      aux_fun_to_doc style ~semicolon ~const ~inline f r args b

(* [trm_fun_to_doc style ~semicolon ty tvl b]: converts a lambda function from a resource formula to a pprint document. *)
and formula_fun_to_doc style ?(semicolon : bool = true) (ty : typ option) (tvl : typed_vars) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  string "fun" ^^ blank 1 ^^ separate (blank 1) (List.map (fun (v, _) -> var_to_doc style v) tvl) ^^ blank 1 ^^ string "->" ^^ blank 1 ^^ trm_to_doc style b ^^ dsemi

(* [trm_fun_to_doc style ~semicolon ty tvl b]: converts a lambda function to a pprint document. *)
and trm_fun_to_doc style ?(semicolon : bool = true) (ty : typ option) (tvl : typed_vars) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let argd = if List.length tvl = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) tvl) in
  let dr = match ty with | Some ty -> string "->" ^^ blank 1 ^^ typ_to_doc ty ^^ blank 1 | None -> blank 1 in
  let capt = brackets (ampersand) in
  separate (blank 1) ([capt; parens (argd); dr; decorate_trm style b]) ^^ dsemi

(* [access_ctrl_to_doc style acc_ctrl]: converts [acc_ctrl] to a pprint document. *)
and access_ctrl_to_doc style (acc_ctrl : access_control) : document =
  match acc_ctrl with
  | Access_public -> string "public:"
  | Access_private -> string "private:"
  | Access_protected -> string "protected:"
  | Access_unspecified -> empty

(* [typedef_to_doc style ~semicolon td]: converts a type definition to pprint document *)
and typedef_to_doc style ?(semicolon : bool = true) ?(t_annot : cstyle_annot list = []) (td : typedef) : document =
  let dsemi = if semicolon then semi else empty in
  let tc = [], td.typdef_tconstr in
  match td.typdef_body with
  | Typdef_alias t ->
      begin match t.typ_desc with
      | Typ_array _ ->
         string "typedef" ^^ blank 1 ^^ typed_var_to_doc style typconstr_to_doc (tc, t) ^^ dsemi
      | Typ_ptr {ptr_kind = Ptr_kind_mut; inner_typ = {typ_desc = Typ_fun (tyl, r); _}} ->
         let dl = List.map typ_to_doc tyl in
         let dr = typ_to_doc r in
         separate (blank 1)
         [string "typedef"; dr; parens (star ^^ typconstr_to_doc tc) ^^ parens (separate (comma ^^ blank 1) dl)] ^^ dsemi
      | _ ->
         separate (blank 1) [string "typedef"; typ_to_doc t; typconstr_to_doc tc] ^^ dsemi
      end
  | Typdef_record rfl ->
    let get_document_list ?(default_access : access_control = Access_private)(rtl : record_fields) : document list =
      let access_ctrl = ref default_access in
      List.fold_left (fun acc (rt, rt_annot) ->
        let fd =
        match rt with
        | Record_field_member (lb, ty) -> typed_var_to_doc style string (lb, ty) ^^ semi
        (* DEPRECATED
        | Record_field_method t1 -> trm_to_doc style t1
        *)
        | Record_field_method t1 ->
          let semi = if is_fun_with_empty_body t1 then semi else empty in
          decorate_trm style t1 ^^ semi
         in
        if rt_annot <> !access_ctrl
            then begin access_ctrl := rt_annot;acc @ [access_ctrl_to_doc style !access_ctrl; fd ] end
            else acc @ [fd ]

      ) [] rfl
       in
      let dl = get_document_list rfl in
      let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
      let record_type = typconstr_to_doc tc in
      if List.mem Is_struct t_annot
        then string "struct" ^^ blank 1 ^^ record_type ^^ sbody ^^ blank 1 ^^ semi
        else if List.mem Is_rec_struct t_annot
          then
            string "typedef " ^^ string "struct" ^^ blank 1 ^^ record_type ^^ blank 1 ^^ sbody ^^ record_type ^^ blank 1 ^^ semi
        else if List.mem Is_class t_annot then
          let dl = get_document_list ~default_access:Access_unspecified rfl in
          let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
          string "class" ^^ blank 1 ^^ record_type ^^ sbody ^^ blank 1 ^^ semi
        else
          string "typedef " ^^ string "struct" ^^ blank 1 ^^ sbody ^^ blank 1 ^^ record_type ^^ blank 1 ^^ semi
  | Typdef_sum _ ->
      failwith "AstC_to_c.typedef_to_doc style: sum types are not supported in C/C++"
  | Typdef_enum enum_const_l ->
      let const_doc_l =
        List.map
         (fun (y, t_o) ->
           match t_o with
           | None -> var_to_doc style y
           | Some t -> separate (blank 1) [var_to_doc style y; equals; decorate_trm style t]
         )
        enum_const_l in
      separate (blank 1) [string "enum"; typconstr_to_doc tc;
      braces (separate (comma ^^ blank 1) const_doc_l)] ^^ dsemi

(* [multi_decl_to_doc style loc tl]: converts a sequence with multiple variable declarations into a single multi variable declaration *)
and multi_decl_to_doc style (loc : location) (tl : trms) : document =
 let get_info (t : trm) : document =
  begin match t.desc with
  | Trm_let (vk, (x, _), init) ->
    begin match vk with
    | Var_immutable ->
      begin match init.desc with
      | Trm_val(Val_lit Lit_uninitialized) -> var_to_doc style x
      | _ -> var_to_doc style x ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm style init
      end
    | _ ->
      begin match init.desc with
      | Trm_apps (_, [base], _)-> var_to_doc style x ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm style base
      | _ -> var_to_doc style x
      end
    end
  | Trm_typedef _ -> string ""
  | _ -> loc_fail loc "AstC_to_c.multi_decl_to_doc style: only variables declarations allowed"
  end
 in
 let dnames = separate (comma ^^ blank 1) (List.map get_info tl) in
  begin match tl with
  | [] -> loc_fail loc "AstC_to_c.multi_deco_to_doc: empty multiple declaration"
  | [d] -> begin match d.desc with
           | Trm_typedef td -> typedef_to_doc style td
           | _ -> loc_fail loc "AstC_to_c.multi_decl_to_doc style: expected a typedef"
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
  | _ -> loc_fail loc "AstC_to_c.multi_decl_to_doc style: expected a trm_let"
  end

(* [apps_to_doc style ~prec f tl]: converts a function call to pprint document *)
and apps_to_doc style ?(prec : int = 0) (f : trm) (tl : trms) : document =
  let (prec, assoc) = precedence_trm f in
  let aux_arguments f_as_doc =
      f_as_doc ^^ list_to_doc ~empty ~sep:comma ~bounds:[lparen; rparen]  (List.map (decorate_trm style) tl)
      in
  let is_get_implicit_this t =
    match t.desc with
    | Trm_apps ({ desc = (Trm_val (Val_prim (Prim_unop Unop_get))); _}, [base], _) -> trm_has_cstyle Implicit_this base
    | _ -> false
  in

  match f.desc with
  (* Case of function pointers *)
  | Trm_apps ({ desc = (Trm_val (Val_prim (Prim_unop Unop_get))); _ }, [ { desc = Trm_var (_, x); _ } ], _) ->
      aux_arguments (var_to_doc style x)
  (* Case of MALLOC *)
  | Trm_var (_, x) when (style.pretty_matrix_notation && Tools.pattern_matches "MALLOC" x.name) ->
    let dims, size = Xlist.unlast tl in
    let error = "expected MALLOC(.., sizeof(..))" in
    let size_var = trm_inv ~error trm_var_inv size in
    assert (size_var.qualifier = []);
    let ty_str = String.(
      match sub size_var.name 0 (length "sizeof("),
            sub size_var.name (length "sizeof(") ((length size_var.name) - (length "sizeof()")),
            sub size_var.name ((length size_var.name) - (length ")")) (length ")") with
      | "sizeof(", ty_str, ")" -> ty_str
      | _ -> failwith error
    ) in
    let bracketed_trm t = brackets (decorate_trm style ~prec:0 t) in
    (string "malloc(sizeof(") ^^ (string ty_str) ^^
    (separate empty (List.map bracketed_trm dims)) ^^
    (string "))")
  (* Case of MFREE *)
  | Trm_var (_, x) when (style.pretty_matrix_notation && Tools.pattern_matches "MFREE" x.name) ->
    let dims, ptr = Xlist.unlast tl in
    (string "free") ^^ lparen ^^ (decorate_trm style ptr) ^^ rparen
  (* Case of function by name *)
  | Trm_var (_, x) ->
    let var_doc = trm_var_to_doc style x f in
    aux_arguments var_doc
  (* Case of inlined function *)
  | Trm_let_fun _ ->
        parens (decorate_trm style f) ^^ list_to_doc ~sep:comma ~bounds:[lparen; rparen] (List.map (decorate_trm style) tl)
  (* Case of primitive operations *)
  | Trm_val v ->
     begin match v with
     | Val_prim p ->
        begin match p with
        | Prim_unop op ->
           begin match tl with
           | [t] ->
              let d = decorate_trm style ~prec t in
              begin match op with
              | Unop_get when style.optitrust_syntax ->
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
              | Unop_struct_access f1 when style.optitrust_syntax ->
                  string "struct_access(" ^^ d ^^ comma ^^ string " " ^^ dquotes (string f1) ^^ string ")"
              | (Unop_struct_get f1 | Unop_struct_access f1) ->
                if is_get_implicit_this t then string f1
                else if is_get_operation t then
                    if trm_has_cstyle Display_no_arrow f
                      then
                        d ^^ dot ^^ string f1
                      else
                        let d = decorate_trm style ~prec (get_operation_arg t) in
                        d ^^ minus ^^ rangle ^^ string f1
                 else d ^^ dot ^^ string f1
              | Unop_cast ty ->
                 let dty = typ_to_doc ty in
                 parens dty ^^ blank 1 ^^ d
              end
           | _ ->
              trm_fail f "AstC_to_c.apps_to_doc style: unary operators must have one argument"
           end
        | Prim_binop op ->
          let (prec1, prec2) =
            if assoc = LtoR
              then (prec, prec + 1)
              else (prec + 1, prec)
            in
          let op_d = binop_to_doc style op in
          begin match tl with
          | [t1; t2] ->
            let d1 = decorate_trm style ~prec:prec1 t1 in
            let d2 = decorate_trm style ~prec:prec2 t2 in
            begin match op with
             | Binop_exact_div ->
              string "exact_div(" ^^ d1 ^^ comma ^^ space ^^ d2 ^^ string ")"
             | Binop_set when style.optitrust_syntax ->
                string "set(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
             | Binop_array_access when style.optitrust_syntax ->
                string "array_access(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
             | Binop_array_access | Binop_array_get ->
                let bracketed_trm t = brackets (decorate_trm style ~prec:0 t) in
                d1 ^^ if not style.pretty_matrix_notation then
                  bracketed_trm (t2)
                else begin match Matrix_trm.mindex_inv t2 with
                | None -> bracketed_trm (t2)
                | Some (_dims, indices) -> separate empty (List.map bracketed_trm indices)
                end
             | _ -> separate (blank 1) [d1; op_d; d2]
             end
          | _ -> trm_fail f "AstC_to_c.apps_to_doc style: binary_operators must have two arguments"
          end
        | Prim_compound_assgn_op _  ->
           begin match tl with
           | [t1; t2] ->
              let d1 = decorate_trm style ~prec t1 in
              let d2 = decorate_trm style ~prec t2 in
              let op_d = prim_to_doc style p in
              if style.optitrust_syntax
                then op_d ^^ parens (d1 ^^ comma ^^ d2)
                else separate (blank 1) [d1; op_d; d2]
          | _ -> trm_fail f "AstC_to_c.apps_to_doc style: expected at most two argumetns."
          end
        | Prim_overloaded_op p_b ->
           begin match tl with
           | [t1] ->
            let d1 = decorate_trm style ~prec t1 in
            let op_d = prim_to_doc style p_b in
            if style.optitrust_syntax
                then op_d ^^ parens (d1)
                else separate (blank 1) [op_d; d1]
           | [t1; t2] ->
              let d1 = decorate_trm style ~prec t1 in
              let d2 = decorate_trm style ~prec t2 in
              let op_d = prim_to_doc style p_b in
              begin match p_b with
              | Prim_binop op ->
                  begin match op with
                  | Binop_set when style.optitrust_syntax ->
                      string "set(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
                  | Binop_array_access when style.optitrust_syntax ->
                      string "array_access(" ^^ d1 ^^ comma ^^ string " " ^^ d2 ^^ string ")"
                  | Binop_array_access | Binop_array_get ->
                    let d2 = decorate_trm style ~prec:0 t2 in
                    d1 ^^ brackets (d2)
                  | _ -> separate (blank 1) [d1; op_d; d2]
                  end
              | Prim_unop Unop_post_inc ->
                if style.optitrust_syntax
                then parens (d1) ^^ op_d
                else separate (blank 1) [d1; op_d]
              | _ -> trm_fail f "AstC_to_c.apps_to_doc style: binary_operators must have two arguments"
              end
          | _ ->
            Printf.printf "Nb_args: %d" (List.length tl);
            trm_fail f "AstC_to_c.apps_to_doc style: expected at most two argumetns."
          end

        | Prim_conditional_op ->
           begin match tl with
           | [t1; t2; t3] ->
              let d1 = decorate_trm style ~prec:4 t1 in
              let d2 = decorate_trm style ~prec:4 t2 in
              let d3 = decorate_trm style ~prec:4 t3 in
              parens (separate (blank 1) [d1; qmark; d2; colon; d3])
           | _ ->
              trm_fail f
                "apps_to_doc style: conditional operator must have three arguments"
           end
        | Prim_new t ->
          (* Here we assume that trm_apps has only one trm as argument *)
          let value = List.hd tl in
          let tr_init = decorate_trm style value in
          let init_val = if is_trm_initialization_list value then tr_init else parens (tr_init) in
          string "new" ^^ blank 1 ^^ typ_to_doc t ^^ init_val
        end
     | _ -> trm_fail f (Printf.sprintf "AstC_to_c.apps_to_doc style: only primitive values may be applied %s\n" (Ast_to_text.ast_to_string f))
     end
   | _ ->
      let f_doc = decorate_trm style f in
      aux_arguments f_doc

(* [mode_to_doc m]: OpenMP mode to pprint document *)
and mode_to_doc (m : mode) : document =
  match m with
  | Shared_m -> string "shared"
  | None_ -> string "none"

(* [sched_type_to_doc st]: OpenMP scheduling type to pprint document *)
and sched_type_to_doc (st : sched_type) : document =
  match st with
  | Static -> string "static"
  | Dynamic -> string "dynamic"
  | Guided -> string "guided"
  | Runtime -> string "runtime"

(* [reduction_identifier_to_doc ri]: OpenMP reduction identifier to pprint document *)
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

(* [map_typ_to_doc mt]: OpenMP map_type to pprint document *)
and map_type_to_doc (mt : map_type) : document =
  match mt with
  | Alloc -> string "alloc" ^^ colon
  | To -> string "to" ^^ colon
  | From -> string "from" ^^ colon
  | ToFrom -> string "tofrom" ^^ colon
  | No_map -> empty

(* [proc_bind_to_doc pb]: OpenMP process bind to pprint document*)
and proc_bind_to_doc (pb : proc_bind) : document =
  match pb with
  | Master_pb -> string "master"
  | Close -> string "close"
  | Spread -> string "spread"

and dep_to_doc (d : dep) : document =
  match d with
  | Dep_var s -> var_to_doc (default_style ()) s  (* TODO #propagate-defaut: propagate style*)
  | Dep_ptr d -> star ^^ dep_to_doc d

(* [dependence_type_to_doc dp]: OpenMP variable dependence type to pprint document *)
and dependence_type_to_doc (dp : dependence_type) : document =
  match dp with
  | In vl -> let vl = List.map dep_to_doc vl in
    string "depend (in" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Out vl -> let vl = List.map dep_to_doc vl in
    string "depend (out" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Inout vl -> let vl = List.map dep_to_doc vl in
    string "depend (inout" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~empty ~sep:comma vl) ^^ rparen
  | Outin vl -> let vl = List.map dep_to_doc vl in
    string "depend (outin" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Sink vl -> let vl = List.map dep_to_doc vl in
    string "depend (sink" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Source -> string "source"

(* [clause_to_doc cl]: OpenMP clause to pprint document *)
and clause_to_doc (cl : clause) : document =
  let style = (default_style ()) in (* TODO #propagate-defaut: propagate style*)
  let vl_to_doc vs = separate_map (string ",") (var_to_doc style) vs in
  match cl with
  | Default m -> string "default" ^^ parens (mode_to_doc m)
  | Shared vl -> string "shared" ^^ parens (vl_to_doc vl)
  | Private vl -> string "private" ^^ parens (vl_to_doc vl)
  | FirstPrivate vl -> string "firstprivate" ^^ parens (vl_to_doc vl)
  | LastPrivate vl -> string "lastprivate" ^^ parens (vl_to_doc vl)
  | Linear (vl, step) -> string "linear" ^^ parens (vl_to_doc vl ^^ if step = 0 then empty else blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int step))
  | Reduction (ri, vl) -> string "reduction" ^^ parens (reduction_identifier_to_doc ri ^^ colon ^^ vl_to_doc vl)
  | Copyin vl -> string "copyin" ^^ parens (vl_to_doc vl)
  | CopyPrivate vl -> string "copyprivate" ^^ parens (vl_to_doc vl)
  | Map_c (mt, vl) -> string "map" ^^ parens (map_type_to_doc mt ^^  blank 1 ^^ vl_to_doc vl)
  | Defaultmap (mt, vl) -> string "defaultmap" ^^ parens (map_type_to_doc mt ^^  blank 1 ^^ vl_to_doc vl)
  | Safelen i -> string "safelen" ^^ parens (string (string_of_int i))
  | Collapse i -> string "collapse" ^^ parens (string (string_of_int i))
  | Simdlen i -> string "simdlen" ^^ parens (string (string_of_int i))
  | Aligned (vl, i) -> string "aligned" ^^ parens (vl_to_doc vl ^^ blank 1 ^^ colon ^^ blank 1 ^^ string (string_of_int i))
  | Uniform vl -> string "uniform" ^^ parens (vl_to_doc vl)
  | Inbranch -> string "inbranch"
  | NotInbranch -> string "notinbranch"
  | Nowait -> string "nowait"
  | Ordered_c i -> string "ordered" ^^ (if i = 0 then empty else parens (string (string_of_int i)))
  | If e-> string "if" ^^ parens (string e)
  | Device i -> string "device" ^^ parens (var_to_doc style i)
  | Num_threads i -> string "num_threads" ^^ parens (var_to_doc style i)
  | Schedule (st, i) -> string "schedule" ^^ parens (sched_type_to_doc st ^^ comma ^^ blank 1 ^^ var_to_doc style i)
  | Dist_schedule (st, i) -> string "dist_schedule" ^^ parens (sched_type_to_doc st ^^ comma ^^ blank 1 ^^ var_to_doc style i)
  | Parallel_c -> string "parallel"
  | Section_c -> string "section"
  | For_c -> string "for"
  | Taskgroup_c -> string "taskgroup"
  | Proc_bind pb -> string "proc_bind" ^^ parens (proc_bind_to_doc pb)
  | Priority i -> string "priority" ^^ parens (var_to_doc style i)
  | Depend dp ->
    let dpl = Tools.list_to_doc ~sep:(blank 1) ~empty (List.map dependence_type_to_doc dp) in
    dpl
  | Grainsize i -> string "grainsize" ^^ parens (string (string_of_int i))
  | Mergeable -> string "mergeable"
  | Nogroup -> string "nogroup"
  | Num_tasks i -> string "num_tasks" ^^ parens (string (string_of_int i))
  | Untied -> string "untied"
  | Final e -> string "final" ^^ parens (string e)
  | To_c vl -> string "to" ^^ parens (vl_to_doc vl)
  | From_c vl -> string "from" ^^ parens (vl_to_doc vl)
  | Link vl -> string "link" ^^ parens (vl_to_doc vl)
  | Num_teams n -> string "num_teams" ^^ parens (var_to_doc style n)
  | Thread_limit n -> string "thread_limit" ^^ parens (var_to_doc style n)

(* [atomic_operation_to_doc ao]: OpenMP atomic operation to pprint document *)
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

(* [directive_to_doc d]: OpenMP directive to pprint document *)
and directive_to_doc (d : directive) : document =
  let style = (default_style ()) in (* TODO #propagate-defaut: propagate style*)
  let vl_to_doc vs = separate_map (string ",") (var_to_doc style) vs in
  let tvl_to_doc tvs = separate_map (string ",") string tvs in
  match d with
  | Atomic ao -> string "atomic" ^^ blank 1 ^^ (atomic_operation_to_doc ao)
  | Atomic_capture -> string "atomic" ^^ blank 1 ^^ string "capture"
  | Barrier -> string "barrier"
  | Cancel (c, cl) -> string "cancel" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Cancellation_point (c, cl) -> string "cancellation" ^^ blank 1 ^^ string "point" ^^ parens (clause_to_doc c ^^ comma ^^ blank 1 ^^ list_to_doc ~sep:comma (List.map clause_to_doc cl))
  | Critical (name, hint) -> string "critical" ^^ parens (var_to_doc style name) ^^ string "hint" ^^ parens (string hint)
  | Declare_simd cl -> string "declare" ^^ blank 1 ^^ string "simd " ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Declare_reduction (ri, tvl, e, c) ->  string "declare" ^^ blank 1 ^^ string "simd" ^^ parens (
    reduction_identifier_to_doc ri ^^ blank 1 ^^ colon ^^ blank 1 ^^ tvl_to_doc tvl ^^
    string e ^^ clause_to_doc c)
  | Declare_target cl -> string "declare" ^^ blank 1 ^^ string "target " ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Distribute cl -> string "distribute" ^^ blank 1 ^^ (list_to_doc ~sep:comma ~empty (List.map clause_to_doc cl))
  | Distribute_parallel_for cl -> string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Distribute_parallel_for_simd cl -> string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (list_to_doc ~sep:comma ~empty (List.map clause_to_doc cl))
  | Distribute_simd -> string "distribute" ^^ blank 1 ^^ string "simd"
  | End_declare_target -> string "end" ^^ blank 1 ^^ string "declare " ^^ string "target"
  | Flush vl -> string "flush" ^^ vl_to_doc vl
  | For cl -> string "for" ^^ blank 1 ^^ (list_to_doc ~empty ~sep:(blank 1) (List.map clause_to_doc cl))
  | For_simd cl -> string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Master -> string "master"
  | Ordered cl -> string "ordered" ^^ blank 1 ^^ (list_to_doc ~empty ~sep:(blank 1) (List.map clause_to_doc cl))
  | Parallel  cl -> string "parallel" ^^ blank 1 ^^ (list_to_doc ~empty ~sep:(blank 1) (List.map clause_to_doc cl))
  | Parallel_for cl -> string "parallel" ^^ blank 1 ^^ string "for " ^^ (list_to_doc ~empty ~sep:(blank 1) (List.map clause_to_doc cl))
  | Parallel_for_simd  cl -> string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Parallel_sections  cl -> string "parallel" ^^ blank 1 ^^ string "sections" ^^ blank 1  ^^ (list_to_doc ~empty (List.map clause_to_doc cl))
  | Section -> string "section"
  | Sections cl -> string "sections" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Simd cl -> string "simd" ^^ blank 1 ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Single cl -> string "single" ^^ blank 1 ^^ (list_to_doc ~empty (List.map clause_to_doc cl))
  | Target cl -> string "target" ^^ blank 1 ^^ (list_to_doc ~sep:empty ~empty (List.map clause_to_doc cl))
  | Target_data cl -> string "target" ^^ blank 1 ^^ string "data"  ^^ blank 1 ^^ (list_to_doc ~sep:empty ~empty (List.map clause_to_doc cl))
  | Target_enter_data  cl -> string "target" ^^ blank 1 ^^ string "enter" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Target_exit_data  cl -> string "target" ^^ blank 1 ^^ string "exit" ^^ blank 1 ^^ string "data" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Target_teams cl -> string "target" ^^ blank 1 ^^ string "teams"  ^^ blank 1 ^^ (list_to_doc ~sep:empty ~empty (List.map clause_to_doc cl))
  | Target_teams_distribute cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (list_to_doc ~sep:empty ~empty (List.map clause_to_doc cl))
  | Target_teams_distribute_parallel_for_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parallel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (list_to_doc ~sep:empty(List.map clause_to_doc cl))
  | Target_teams_distribute_simd cl -> string "target" ^^ blank 1 ^^ string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (list_to_doc ~sep:empty (List.map clause_to_doc cl))
  | Target_update cl -> string "target" ^^ blank 1 ^^ string "update" ^^ blank 1 ^^ (list_to_doc ~sep:empty ~empty (List.map clause_to_doc cl))
  | Task cl -> string "task" ^^ blank 1 ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Taskgroup -> string "taskgroup"
  | Taskloop cl -> string "taskloop" ^^ blank 1 ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Taskloop_simd cl -> string "taskloop" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Taskwait cl -> string "taskwait" ^^ blank 1 ^^ (list_to_doc ~sep:(blank 1) ~empty (List.map clause_to_doc cl))
  | Taskyield -> string "taskyield"
  | Teams cl -> string "teams" ^^ blank 1 ^^ (list_to_doc ~sep:empty (List.map clause_to_doc cl))
  | Teams_distribute cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_end cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "end" ^^ (list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ (list_to_doc (List.map clause_to_doc cl))
  | Teams_distribute_parallel_for_simd cl -> string "teams" ^^ blank 1 ^^ string "distribute" ^^ blank 1 ^^ string "parllel" ^^ blank 1 ^^ string "for" ^^ blank 1 ^^ string "simd" ^^ blank 1 ^^(list_to_doc (List.map clause_to_doc cl))
  | Threadprivate vl -> string "threadprivate" ^^ parens (vl_to_doc vl)

(* [routine_to_doc r]: OpenMP routine to pprint document *)
and routine_to_doc (r : omp_routine) : document =
  let style = (default_style ()) in (* TODO #propagate-defaut: propagate style*)
  match r with
  | Set_num_threads i -> string "omp_set_num_threads" ^^ parens (string (string_of_int i)) ^^ semi
  | Get_num_threads -> string "omp_get_num_threads" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_max_threads -> string "omp_get_max_threads" ^^ lparen ^^ blank 1 ^^ rparen ^^ semi
  | Get_thread_num  -> string "omp_get_thread_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_procs  -> string "omp_get_num_procs" ^^ lparen ^^ blank 1 ^^ rparen ^^ semi
  | In_parallel  -> string "omp_in_parallel" ^^ lparen ^^ blank 1 ^^ rparen ^^ semi
  | Set_dynamic i -> string "omp_set_dynamic" ^^ parens (string (string_of_int i)) ^^ semi
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
  | Set_default_device i -> string "omp_set_default_device" ^^ parens (var_to_doc style i)
  | Get_default_device -> string "omp_get_default_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_devices  -> string "omp_get_num_devices" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_num_teams -> string "omp_get_num_teams" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_team_num  -> string "omp_team_num" ^^ lparen ^^ blank 1 ^^ rparen
  | Is_initial_device -> string "omp_is_initial_device" ^^ lparen ^^ blank 1 ^^ rparen
  | Init_lock lck -> string "omp_init_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Init_nest_lock lck -> string "omp_init_nest_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Destroy_lock lck -> string "omp_destroy_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Destroy_nest_lock lck -> string "omp_destroy_nest_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Set_lock lck-> string "omp_set_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Set_nest_lock lck -> string "omp_set_nest_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Unset_lock lck ->  string "omp_unset_lock" ^^ parens (ampersand ^^ var_to_doc style lck) ^^ semi
  | Unset_nest_lock lck -> string "omp_unset_nest_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Test_lock lck -> string "omp_test_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Test_nest_lock lck -> string "omp_test_nest_lock" ^^ parens (ampersand ^^ var_to_doc style lck)
  | Get_wtime -> string "get_wtime" ^^ lparen ^^ blank 1 ^^ rparen
  | Get_wtick -> string "get_wtich" ^^ lparen ^^ blank 1 ^^ rparen

(* [unpack_trm_for ~loc index start direction stop step body]: converts a simple for loop to a complex one before converting it to a pprint document *)
(* FIXME: #odoc why is annotation required on callees? *)
and unpack_trm_for ?(loc: location) (l_range : loop_range) (body : trm) : trm =
  let (index, start, direction, stop, step, _is_parallel ) = l_range in
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
      | _ -> trm_fail body "AstC_to_c.unpack_trm_for: can't use decrementing operators for upper bounded for loops"
      end
    | DirDown | DirDownEq ->
      begin match step with
      | Pre_dec ->
        trm_apps (trm_unop Unop_pre_dec) [trm_var index]
      | Post_dec ->
        trm_apps (trm_unop Unop_post_dec) [trm_var index]
      | Step st ->
        trm_apps (trm_prim (Prim_compound_assgn_op Binop_sub) ) [trm_var index; st]
      | _ -> trm_fail body "AstC_to_c.unpack_trm_for: can't use decrementing operators for upper bounded for loops"
      end

    end in
    trm_for_c ?loc init cond step body

and formula_to_doc style (f: formula): document =
  let open Resource_formula in
  match formula_model_inv f with
  | Some (t, formula) -> (trm_to_doc style t) ^^ blank 1 ^^ string "~>" ^^ blank 1 ^^ (trm_to_doc style formula)
  | None -> trm_to_doc style {f with annot = {f.annot with trm_annot_cstyle = []}}

(* [ast_to_doc ~comment_pragma ~optitrust_syntax t]: converts a full OptiTrust ast to a pprint document.
    If [comment_pragma] is true then OpenMP pragmas will be aligned to the left. If [optitrust_syntax] is true then encodings are made visible. *)
let ast_to_doc style (t : trm) : document =
  decorate_trm style t

(* [ast_to_outchannel ~comment_pragma ~optitrust_syntax out t]: print ast [t] to an out_channel [out] *)
let ast_to_outchannel style (out : out_channel) (t : trm) : unit =
  ToChannel.pretty 0.9 (!Flags.code_print_width) out (ast_to_doc style t)

(* [ast_to_file ~optitrust_syntax filename t]: print ast [t] to file [filename] *)
let ast_to_file style (filename : string) (t : trm) : unit =
  let out = open_out filename in
  ast_to_outchannel style out t;
  close_out out

(* [ast_to_string ~optitrust_syntax t]: converts ast [t] to string *)
let ast_to_string ?(style : style option) ?(optitrust_syntax : bool option) (t : trm) : string =
  let style = match style with None -> default_style() | Some s -> s in
  let style = match optitrust_syntax with None -> style | Some b -> { style with optitrust_syntax = b } in
  document_to_string (ast_to_doc style t)

(* [typ_to_string ty]: converts type [ty] to string *)
let typ_to_string (ty : typ) : string =
  let b = Buffer.create 80 in
  ToBuffer.pretty 0.9 (!Flags.code_print_width) b (typ_to_doc ty);
  Buffer.contents b
