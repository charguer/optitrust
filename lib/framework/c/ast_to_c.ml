open PPrint
open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Precedence
open Tools

(* TODO: implement support for printing trm annotations when style.print_annot=true *)



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
  print_contract_internal_repr: bool; (* print internal loop contract *)
  print_var_id: bool; (* print internal variable identifiers *)
  print_string_repr: bool; (* print string representation for expressions *)
  print_mark: bool; (* print marks *)
  print_annot: bool; (* print annotations *)
  print_errors: bool; (* print errors *)
  print_types: bool;
  optitrust_syntax: bool; (* print "set(p,v)" instead of "p=v", and print "array_access(t,i)" etc *)
  c_alloc: bool; (* print "malloc(sizeof(int))" instead of "new int" *)
  pretty_matrix_notation: bool; (* print t[MINDEX(n,m,a,b)] as t[a;b] *)
  pretty_fraction_notation: bool; (* print 1-1/2 instead of __frac_sub(__full,__frac_div(__full, 2)) *)
  commented_pragma: bool; (* comment out pragram lines, for better tabulation by clang-format *)
  hide_seq: bool; (* hide the content of sequences with {...},  used to avoid weird matches with stringrepr *)
}

(** Default style, depends on the global flags *)
let default_style () : style = {
  print_contract_internal_repr = false;
  print_var_id = !Flags.debug_var_id;
  print_string_repr = !Flags.debug_stringreprs;
  print_mark = true;
  print_annot = false; (* LATER: add support for this *)
  print_errors = true;
  print_types = false;
  optitrust_syntax = !Flags.print_optitrust_syntax;
  c_alloc = true;
  pretty_matrix_notation = !Flags.pretty_matrix_notation;
  pretty_fraction_notation = true;
  commented_pragma = !Flags.use_clang_format;
  hide_seq = false;
}

(** Style for target stringrepr (sExpr or sInstr) *)
let style_for_stringrepr : style = {
  print_contract_internal_repr = false;
  print_var_id = false;
  print_string_repr = false;
  print_mark = false;
  print_annot = false;
  print_errors = false;
  print_types = false;
  optitrust_syntax = false;
  c_alloc = true;
  pretty_matrix_notation = false;
  pretty_fraction_notation = true;
  commented_pragma = false;
  hide_seq = true;
}

(** Style for reparsing *)
let style_for_reparse : style = {
  print_contract_internal_repr = true;
  print_var_id = false;
  print_string_repr = false;
  print_mark = false;
  print_annot = false;
  print_errors = false;
  print_types = false;
  optitrust_syntax = false;
  c_alloc = false;
  pretty_matrix_notation = false;
  pretty_fraction_notation = false;
  commented_pragma = false;
  hide_seq = false;
}

(** Style for debugging var ids *)
let style_for_varids : style = {
  print_contract_internal_repr = false;
  print_var_id = true;
  print_string_repr = false;
  print_mark = true;
  print_annot = false;
  print_errors = true;
  print_types = false;
  optitrust_syntax = false;
  c_alloc = true;
  pretty_matrix_notation = false;
  pretty_fraction_notation = true;
  commented_pragma = false;
  hide_seq = false;
}

(** Style for debugging types *)
let style_for_types : style = {
  print_contract_internal_repr = false;
  print_var_id = false;
  print_string_repr = false;
  print_mark = true;
  print_annot = false;
  print_errors = true;
  optitrust_syntax = true;
  c_alloc = false;
  print_types = true;
  pretty_matrix_notation = false;
  pretty_fraction_notation = true;
  commented_pragma = false;
  hide_seq = false;
}

(*----------------------------------------------------------------------------------*)
(* An optional memoization table that maps a [stringreprid] of a term
   to the [document] obtained by executing [trm_to_doc style] on it.
   If [stringreprs] is not [None], the stringreprs table is filled in
   during the calls to [trm_to_doc style], for terms that bear a [Annot_stringreprid].
   Make sure to invoke [label_subterms_with_fresh_stringreprids] on the
   AST being printed, and preferably to reinitialize the stringreprs
   table before starting. *)

(** [stringreprids]: Hashtable for storing the string representation of trm. *)
type stringreprs = (Ast.stringreprid, document) Hashtbl.t

(** [stringreprs]: string representations are stored only when the user uses string matching targets. *)
let stringreprs : stringreprs option ref = ref None

(** [clear_string_reprs ()]: clearn all the stored string representations. *)
let clear_stringreprs () =
  stringreprs := None

(** [get_stringreprs ()]: gets the current string representations. *)
let get_stringreprs () : stringreprs =
  match !stringreprs with
  | Some m -> m
  | None -> failwith "Ast_to_c.get_stringreprs: must call init_stringreprs or set_stringreprs first"

(** [get_and_clear_stringreprs ()]: gets the current string representations and delete them. *)
let get_and_clear_stringreprs () : stringreprs =
  let m = get_stringreprs () in
  stringreprs := None;
  m

(** [set_stringreprs t]: replaces the current string representations with [t]. *)
let set_stringreprs (t : stringreprs) : unit =
  stringreprs := Some t

(** [init_string_reprs]: creates the hashtable used for storing the string representations. *)
let init_stringreprs () =
  stringreprs := Some (Hashtbl.create 100)

(** [add_stringreprs_entry t d]: adds trm [t] into the table of representations. *)
let add_stringreprs_entry (t : trm) (d : document) : unit =
  match !stringreprs with
  | None -> ()
  | Some m ->
      match trm_get_stringreprid t with
      | None -> ()
      | Some id -> Hashtbl.add m id d

(** [print_stringreprs m]: for debugging purposes. *)
let print_stringreprs (m : stringreprs) : unit =
  let pr id d =
    Tools.debug "stringreprs[%d] = %s\n----" id (document_to_string d) in
  Tools.debug "====<stringreprs>====";
  Hashtbl.iter pr m;
  Tools.debug "====</stringreprs>===="

let var_uninitialized = toplevel_var "__uninitialized"
let trm_uninitialized = trm_var var_uninitialized

(*----------------------------------------------------------------------------------*)

(* To print back ast to C/C++ code, we convert all the ast components into  pprint documents.
   As a result, a well structured code will be generated. For further improving the printing
   one can use clang-format.  *)

(* **********************************************************************************************************
 * Note: to convert the OptiTrust ast to C/C++ code, we convert all the ast components into pprint documents.
 * As as result, a well structured code will be generated. One can apply clang-format to restructure the code
 * based on well known conventions.

**************************************************************************************************************)

(** [typ_desc_to_doc style t]: converts ast type descriptions to pprint documents. *)
let rec typ_desc_to_doc style (t : typ) : document =
  Pattern.pattern_match t [
    Pattern.typ_auto (fun () -> string "auto");
    Pattern.typ_unit (fun () -> string "void");
    Pattern.typ_int (fun () -> string "int");
    Pattern.typ_uint (fun () -> string "unsigned int");
    Pattern.typ_f32 (fun () -> string "float");
    Pattern.typ_f64 (fun () -> string "double");
    Pattern.typ_bool (fun () -> string "bool");
    Pattern.typ_char (fun () -> string "char");
    Pattern.typ_u8 (fun () -> string "uint8_t");
    Pattern.typ_i8 (fun () -> string "int8_t");
    Pattern.typ_u16 (fun () -> string "uint16_t");
    Pattern.typ_i16 (fun () -> string "int16_t");
    Pattern.typ_u32 (fun () -> string "uint32_t");
    Pattern.typ_i32 (fun () -> string "int32_t");
    Pattern.typ_u64 (fun () -> string "uint64_t");
    Pattern.typ_i64 (fun () -> string "int64_t");
    Pattern.typ_usize (fun () -> string "size_t");
    Pattern.typ_isize (fun () -> string "ptrdiff_t");
    Pattern.(typ_const !(typ_var __)) (fun t () -> string "const " ^^ typ_to_doc style t);
    Pattern.(typ_const !__) (fun t () -> typ_to_doc style t ^^ string " const");
    Pattern.(typ_ptr !__) (fun t () -> typ_to_doc style t ^^ star);
    Pattern.(typ_ref !__) (fun t () -> typ_to_doc style t ^^ ampersand);
    Pattern.(typ_array __ __) (fun () ->
      let t, szs = typ_nested_array_inv t in
      let d = typ_to_doc style t in
      let dszs = List.map (fun s -> match s with
        | None -> brackets empty
        | Some t' -> brackets (decorate_trm style t')
      ) szs in
      d ^^ concat dszs
    );
    Pattern.(typ_fun !__ !__) (fun _ _ () ->
      Flags.verbose_warn None "Ast_to_c.typ_desc_to_doc: typ_fun not implemented\n";
      at
    );
    Pattern.(typ_pure_fun !__ !__) (fun args ret () ->
      let dargs = list_to_doc ~sep:(space ^^ star ^^ space) (List.map (typ_to_doc style) args) in
      let dret = typ_to_doc style ret in
      dargs ^^ space ^^ string "->" ^^ space ^^ dret
    );
    Pattern.(trm_apps1 (typ_var (var_eq typ_typeof_var)) !__) (fun t () ->
      string "decltype" ^^ parens (decorate_trm style t)
    );
    Pattern.(typ_apps !__ !__) (fun ty args () ->
      let d_args = if trm_has_cstyle InjectedClassName t
        then empty
        else begin match args with
        | [] -> langle ^^ rangle
        | _ -> langle ^^ list_to_doc ~sep:comma ~bounds:[empty; empty] (List.map (decorate_trm style) args) ^^ rangle
        end
      in
      var_to_doc style (remove_typ_namespace ty) ^^ d_args
    );
    Pattern.(typ_var !__) (fun tyx () ->
      var_to_doc style (remove_typ_namespace tyx)
    );
    Pattern.(trm_arbitrary !__) (fun a_kind () ->
      match a_kind with
      | Typ ty -> string "/* @arbitrary */" ^^ string ty ^^ string "/* arbitary@ */"
      | _ -> failwith "Ast_to_c.typ_to_doc: arbitrary types entered as string should be entered by using Typ"
    );
    Pattern.__ (fun () ->
      failwith "no printing implemented for %s\n" (Ast_to_text.typ_to_string t)
    )
  ]

and var_to_doc style (v : var) : document =
  if is_anon_var v then
    string (sprintf "#%d" v.id)
  else
    let qualified = (concat_map (fun q -> string q ^^ string "::") v.namespaces) ^^ string v.name in
    if style.print_var_id then
      qualified ^^ string ("/*#" ^ string_of_int v.id ^ "*/")
    else
      qualified

(** [typ_to_doc]: converts ast types to pprint document. *)
and typ_to_doc style (t : typ) : document =
  let d = typ_desc_to_doc style t in
  let dattr =
    match t.annot.trm_annot_attributes with
    | [] -> empty
    | al -> concat (List.map (attr_to_doc style) al)
  in
  dattr ^^ d

and may_annot_typ style (t : typ) (doc : document) : document =
  if style.print_types && not (is_typ_unit t) then
    parens (typ_to_doc style t) ^^ parens doc
  else
    doc

(** [typed_var_to_doc style tx]: pairs like (x, int) are printed as int x. *)
and typed_var_to_doc : 'a. ?predecl:bool -> style -> ('a -> document) -> ('a * typ) -> document =
  fun ?(predecl = false) style (x_to_doc: 'a -> document) ((x, ty) : 'a * typ) ->
  Pattern.pattern_match ty [
    Pattern.(typ_array !__ !__) (fun inner_typ size () ->
      let dsize = match size with
        | None -> empty
        | Some sz -> decorate_trm style sz
      in
      typed_var_to_doc ~predecl style (fun x -> x_to_doc x ^^ brackets dsize) (x, inner_typ)
    );
    Pattern.(typ_fun !__ !__) (fun arg_typs ret_typ () ->
      let dattr =
        match ty.annot.trm_annot_attributes with
        | [] -> empty
        | al -> concat (List.map (attr_to_doc style) al)
      in
      let dret_type = typ_to_doc style ret_typ in
      let darg_types = List.map (typ_to_doc style) arg_typs in
      let dname = if predecl then x_to_doc x else parens (star ^^ x_to_doc x) in
      dattr ^^ dret_type ^^ blank 1 ^^ dname ^^ (list_to_doc ~sep:comma ~bounds:[lparen; rparen] darg_types)
    );
    Pattern.__ (fun () ->
      let dextern = if predecl then string "extern" ^^ blank 1 else empty in
      dextern ^^ typ_to_doc style ty ^^ blank 1 ^^ x_to_doc x
    )
  ]

(** [lit_to_doc style l]: converts literals to pprint documents. *)
and lit_to_doc style (cstyles: cstyle_annot list) (l : lit) : document =
  match l with
  | Lit_unit -> empty
  | Lit_bool b -> string (string_of_bool b)
  | Lit_int (_, i) -> string (string_of_int i)
  | Lit_float (typ, f) ->
    begin match typ_var_inv typ with
    | Some v when var_eq v typ_f32_var ->
      string ((string_of_float f) ^ "f")
    | _ ->
      string (string_of_float f)
    end
  | Lit_string s -> dquotes (separate (backslash ^^ string "n") (lines s))
  | Lit_null ty ->
    if typ_ptr_inv ty = None then
      string "{}"
    else
      if List.mem Display_null_uppercase cstyles
        then string "NULL"
        else string "nullptr"

(** [unop_to_doc style op]: converts unary operators to pprint documents. *)
and unop_to_doc style (op : unary_op) : document =
  match op with
  | Unop_get -> star
  | Unop_address -> ampersand
  | Unop_neg -> bang
  | Unop_bitwise_neg -> tilde
  | Unop_minus -> minus
  | Unop_plus -> plus
  | Unop_post_incr | Unop_pre_incr -> twice plus
  | Unop_post_decr | Unop_pre_decr -> twice minus
  | Unop_struct_access s -> dot ^^ string s
  | Unop_struct_get s -> dot ^^ string s
  | Unop_cast t ->
     let dt = typ_to_doc style t in
     string "static_cast" ^^ langle ^^ dt ^^ rangle

(** [binop_to_doc style op]: converts binary operators to pprint documents. *)
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
  | Binop_exact_div -> slash
  | Binop_trunc_div -> slash
  | Binop_trunc_mod -> percent
  | Binop_le -> langle ^^ equals
  | Binop_lt -> langle
  | Binop_ge -> rangle ^^ equals
  | Binop_gt -> rangle
  | Binop_bitwise_and -> ampersand
  | Binop_bitwise_or -> bar
  | Binop_shiftl -> twice langle
  | Binop_shiftr -> twice rangle
  | Binop_xor -> caret

(** [prim_to_doc style p]: converts primitives to pprint documents. *)
and prim_to_doc style (ty: typ) (p : prim) : document =
  match p with
  | Prim_unop op -> may_annot_typ style ty (unop_to_doc style op)
  | Prim_binop op -> may_annot_typ style ty (binop_to_doc style op)
  | Prim_compound_assign_op op -> (binop_to_doc style op) ^^ equals
  | Prim_ref | Prim_ref_uninit ->
    string "ref" ^^ blank 1 ^^ typ_to_doc style ty
  | Prim_new | Prim_new_uninit ->
    string "new" ^^ blank 1 ^^ typ_to_doc style ty
  | Prim_delete ->
    if is_typ_array ty then string "delete[]" else string "delete"
  | Prim_array | Prim_record ->
    typ_to_doc style ty

(** [attr_to_doc a]: converts attributes to pprint documents. *)
and attr_to_doc style (a : attribute) : document =
  match a with
  | Alignas t -> string "alignas" ^^ parens (decorate_trm style t) ^^ blank 1
  | _ -> empty

(** [decorate_trm style ~semicolon ~prec ~print_struct_init_type t]:
    - if [prec] is greater than the precedence of [t] then decorate [t] with parentheses
    - if [t] is marked,then decorate [t] with those marks.
    - if t is struct initialization list outside a variable declaration, then decorate [t] with a cast in front
    - if [semicolon] is true then decorate [t] with a semicolon at the end *)
and decorate_trm style ?(semicolon : bool = false) ?(force_expr : bool = false) ?(prec : int = 0) ?(print_struct_init_type : bool = true) (t : trm) : document =
  let parentheses = parentheses_needed ~prec t in
  let dt = trm_to_doc style ~semicolon ~force_expr ~prec ~print_struct_init_type t in

  let t_pragmas = trm_get_pragmas t in
  let dpragmas = if t_pragmas = []
    then empty
    else
      let t_pragmas_str = List.map (fun d ->
        let intro = if style.commented_pragma then string "//" else empty in
          intro ^^ sharp ^^ string "pragma" ^^ blank 1 ^^ string "omp" ^^ blank 1 ^^ directive_to_doc d
        ) t_pragmas in
    list_to_doc ~sep:(string "\n") ~bounds:[empty; hardline] t_pragmas_str
  in

  let t_labels = trm_get_labels t in
  let dlabels = if t_labels = []
    then empty
    else
      let t_labels_str = List.map string t_labels in
      list_to_doc ~sep:(colon ^^ blank 1) ~bounds:[empty; colon ^^ blank 1] t_labels_str
  in

  let dt = if parentheses then parens (dt) else dpragmas ^^ dlabels ^^ dt in

  let t_marks = trm_get_marks t in

  if style.print_annot then failwith "NOT YET IMPLEMENTED: Ast_to_c printing of annotations";

  let dt =
    if t_marks = [] && not style.print_string_repr then
      dt
    else begin
      let sid =
        if not style.print_string_repr then "" else begin
        match Trm.trm_get_stringreprid t with
        | None -> "[-]"
        | Some id -> Printf.sprintf "[%d]" id
        end in
      let smarks =
        if style.print_mark
          then list_to_string ~sep:", " ~bounds:("","") t_marks
          else "" in

      let sleft = string ("/*@" ^ sid ^ smarks ^ "*/") in
      let sright =  string ("/*" ^ sid ^ smarks ^ "@*/") in
      sleft ^^ dt ^^ sright
    end
    in

  let dt =
    match t.typ with
    | Some typ ->
      may_annot_typ style typ dt
    | _ ->
      dt
  in

  (*printf "ERRORS L=%d\n" (List.length t.errors);*)
  if t.errors = [] || not style.print_errors then
    dt
  else begin
    let derror = list_to_doc ~sep:hardline
      ~bounds:[(string "/*" ^^ hardline);
                (hardline ^^ string "*/" ^^ hardline )]
      (List.map (fun error -> string (sprintf "ERROR: %s" error)) t.errors) in
    if !Flags.debug_errors_msg_embedded_in_ast && (List.length t.errors) > 0
      then Tools.debug "PRINTING ERROR IN COMMENT IN AST---:\n%s---" (Tools.document_to_string derror);
    derror ^^ dt
  end

(** [trm_var_to_doc style v t]: pretty prints trm_vars, including here type arguments and nested name specifiers. *)
and trm_var_to_doc style (x : var) (t : trm) : document =
  let typ_args = get_typ_arguments t in
  let typ_args_d = List.map (typ_to_doc style) typ_args in
  let typ_args_d = begin match typ_args_d with | [] -> empty | _ -> list_to_doc ~sep:comma ~bounds:[langle; rangle] typ_args_d end in
  var_to_doc style x ^^ typ_args_d

(** [trm_to_doc style ~semicolon ~force_expr ~prec ~print_struct_init_type  t]: converts [t] to a pprint document *)
and trm_to_doc style ?(semicolon=false) ?(force_expr=false) ?(prec : int = 0) ?(print_struct_init_type : bool = false) (t : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let t_attributes = trm_get_attr t in
  let dattr =
    match t_attributes with
    | [] -> empty
    | al -> concat (List.map (attr_to_doc style) al)
    in
  let d =
    begin match t.desc with
    | _ when trm_has_cstyle Type t -> typ_to_doc style t
    | _ when trm_has_cstyle ResourceFormula t ->
      dattr ^^ formula_to_doc style t ^^ dsemi
    | Trm_var x ->
      (* if x.qvar_var = "this"
        then
          if trm_has_cstyle Implicit_this t
            then empty
            else string "this"
      else  *)
      let var_doc = trm_var_to_doc style x t in
      dattr ^^ var_doc ^^ dsemi
    | Trm_lit l ->
      if trm_has_cstyle Empty_cond t && l = Lit_bool true
        then empty
        else dattr ^^ lit_to_doc style (trm_get_cstyles t) l ^^ dsemi
    | Trm_prim (ty, p) ->
      dattr ^^ prim_to_doc style ty p ^^ dsemi

    | Trm_let (tv, init) -> dattr ^^ trm_let_to_doc style ~semicolon tv init
    | Trm_let_mult bs -> dattr ^^ trm_let_mult_to_doc style ~semicolon bs
    | Trm_predecl tv -> dattr ^^ typed_var_to_doc ~predecl:true style (var_to_doc style) tv ^^ dsemi
    | Trm_typedef td ->
      let t_annot = trm_get_cstyles t in
      dattr ^^ typedef_to_doc style ~semicolon ~t_annot td
    | Trm_if (b, then_, else_) ->
      let shortcircuit = match (trm_bool_inv then_, trm_bool_inv else_) with
        | _, Some false when trm_has_cstyle Shortcircuit_and t -> Some (b, then_, string "&&", precedence_and)
        | Some true, _ when trm_has_cstyle Shortcircuit_or t -> Some (b, else_, string "||", precedence_or)
        | _ -> None
      in
      begin match shortcircuit with
      | Some (t1, t2, op, precedence) ->
        let (lprec, rprec) = sub_prec precedence in
        let d1 = decorate_trm style ~prec:lprec ~force_expr:true ~semicolon:false t1 in
        let d2 = decorate_trm style ~prec:rprec ~force_expr:true t2 in
        let d = dattr ^^ separate (blank 1) [ d1; op; d2 ] ^^ dsemi in
        if expr_prec precedence < prec then parens d else d
      | None when force_expr || trm_has_cstyle Ternary_cond t ->
        let (lprec, rprec) = sub_prec precedence_ternary_cond in
        let db = decorate_trm style ~prec:lprec ~force_expr:true ~semicolon:false b in
        let dt = decorate_trm style ~force_expr:true then_ in
        let de = decorate_trm style ~prec:rprec ~force_expr:true else_ in
        let d = dattr ^^ separate (blank 1) [db; string "?"; dt; string ":"; de] ^^ dsemi in
        if expr_prec precedence_ternary_cond < prec then parens d else d
      | None ->
        let db = decorate_trm style ~force_expr:true ~semicolon:false b in
        let dt = decorate_trm style ~semicolon:true then_ in
        begin match else_.desc with
        | Trm_lit Lit_unit ->
          dattr ^^ separate (blank 1) [string "if"; parens db; dt]
        | _ ->
          let de = decorate_trm style ~semicolon:true else_ in
          dattr ^^ separate (blank 1) [string "if"; parens db; dt] ^^
            hardline ^^ string "else" ^^ blank 1 ^^ de
        end
      end
    | Trm_seq (tl, result) ->
      if (not !Flags.display_includes) && (trm_is_include t) then
        empty
      else
        let dl = Mlist.flatten_marks (decorate_trm style ~semicolon:true)
          (fun m -> string ("/*@ " ^ list_to_string ~bounds:("", "") ~sep:", " m ^ " @*/")) tl
        in

        if trm_is_nobrace_seq t then
          (* CHECK: #var-id, made nobrace visible *)
          (* DEBUG: let nb = string (sprintf "/*no-brace %d*/" (Option.get (Nobrace.get_id t))) in *)
          let nb = string "/*no-brace*/" in
          nb ^^ surround 2 1 lbrace (dattr ^^ separate hardline dl) rbrace
        else
          let res =
            if style.hide_seq then
              string "{...}"
            else if trm_is_mainfile t then
              let header =
                if style.pretty_matrix_notation
                  then (string "// NOTE: using pretty matrix notation") ^^ hardline
                  else empty
                in
                header ^^ (separate (twice hardline) dl)
            else
              let dinstrs = match result with
                | None -> separate hardline dl
                | Some x -> separate hardline dl ^^ hardline ^^ var_to_doc style x ^^ semi
              in
              let lsep, rsep = if force_expr
                then (lparen ^^ lbrace, rbrace ^^ rparen)
                else (lbrace, rbrace)
              in
              surround 2 1 lsep dinstrs rsep
          in
          dattr ^^ res
    | Trm_apps (f, tl, _) ->
      dattr ^^ apps_to_doc style ~annot:t.annot ~print_struct_init_type ~prec f tl ^^ dsemi
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
      dt
      (* print_contract_internal_repr is handled in C_encoding, printing it here might be useful if encoding is heavily broken
      if style.print_contract_internal_repr
        then string "/*" ^^ string "Contract: " ^^ loop_spec_to_doc style loop_spec ^^ string "*/" ^^ hardline ^^ dt
        else dt*)
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
          | Some t -> dattr ^^ string "return " ^^ decorate_trm style ~force_expr:true ~print_struct_init_type:true t ^^ dsemi
          end
      | Break _ -> dattr ^^ string "break" ^^ dsemi
      | Continue _ -> dattr ^^ string "continue" ^^ dsemi
      end
    | Trm_goto l -> dattr ^^ string "goto" ^^ blank 1 ^^ string l ^^ dsemi
    | Trm_arbitrary a_kind ->
      let code_str =
      begin match a_kind with
      | Lit l -> string l
      | Expr e -> string e
      | Stmt s -> string s
      | Instr s -> string s ^^ semi
      | Comment s -> string s
      | _ -> trm_fail t "Ast_to_c.trm_to_doc style: arbitrary code should be entered by using Lit, Expr and Stmt only"
      end in
      string "/* @arbitrary */" ^^ dattr ^^ code_str ^^ string "/* arbitary@ */"
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
      let dtpl = List.map (fun (n, tpk) ->
        match tpk with
        | Typename typ_opt ->
          begin match typ_opt with
          | Some ty -> string "typename " ^^ var_to_doc style (remove_typ_namespace n) ^^ equals ^^ typ_to_doc style ty
          | None -> string "typename " ^^ var_to_doc style (remove_typ_namespace n)
          end
        | NonType (ty, t_opt) ->
          begin match t_opt with
          | Some t1 -> typ_to_doc style   ty ^^ blank 1 ^^ var_to_doc style n ^^ equals ^^ decorate_trm style t1
          | None -> typ_to_doc style ty ^^ blank 1 ^^ var_to_doc style n
          end
        ) tpl in
      string "template" ^^ blank 1 ^^ (list_to_doc ~sep:comma ~bounds:[langle;rangle] dtpl) ^^ dl
    | Trm_fun (tvl, ty_opt, body, _) -> dattr ^^ trm_fun_to_doc style ~semicolon ty_opt tvl body
    end in
  (* Save the result in the optional stringreprs table, before returning the document *)
  add_stringreprs_entry t d;
  d

(** [trm_let_to_doc style ~semicolon tv init]: converts a variable declaration to print document *)
and trm_let_to_doc style ?(semicolon : bool = true) (tv : typed_var) (init : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let dtx = typed_var_to_doc style (var_to_doc style) tv in
  match init.desc with
  | Trm_var v when var_eq v var_uninitialized -> dtx ^^ semi
  | Trm_fun (args, rettyp, body, _) when not (trm_has_cstyle Closure init) ->
    let fun_annot = trm_get_cstyles init in
    let static = if trm_has_cstyle Static_fun init then string "static" else empty in
    let hidden = if trm_has_cstyle BodyHiddenForLightDiff init then string " /* unchanged, collapsed for light diff */" else empty in
    let dexectime = empty in
      (* TODO: find why it does not work
      if not !Flags.report_exectime then empty else begin
        let exectime = init.ctx.ctx_resources_exectime in
        let microsec =  (1000000. *. exectime) in
        blank 1 ^^ string "/* typing time: " ^^ string (string_of_float microsec) ^^ string " */"
      end in *)
    static ^^ blank 1 ^^ dexectime ^^ trm_let_fun_to_doc ~semicolon style fun_annot (fst tv) rettyp args body ^^ hidden
  | Trm_apps (_, args, _) when trm_has_cstyle Constructed_init init ->
    dtx ^^ blank 1 ^^ list_to_doc ~bounds:[lparen; rparen] ~sep:comma (List.map (decorate_trm style) args) ^^ dsemi
  | Trm_apps (_, tl, _) when trm_has_cstyle Brace_init init && Option.is_some (trm_array_inv init) ->
    dtx ^^ list_to_doc ~bounds:[lbrace; rbrace] ~sep:empty (List.map (decorate_trm style) tl) ^^ dsemi
  | _ ->
    dtx ^^ blank 1 ^^ equals ^^ blank 1 ^^ decorate_trm style ~force_expr:true ~print_struct_init_type:false init ^^ dsemi

(** [trm_let_mult_to_doc style ~semicolon tv bs]: converts multiple variable declarations to pprint document *)
and trm_let_mult_to_doc style ?(semicolon : bool = true) (bs : (typed_var * trm) list) : document =
  let rec split_ptrs (ty: typ): (typ * document) =
    Pattern.pattern_match ty [
      Pattern.(typ_const (typ_ptr !__)) (fun ty () ->
        let ty, dptr = split_ptrs ty in
        ty, dptr ^^ blank 1 ^^ string "* const"
      );
      Pattern.(typ_ptr !__) (fun ty () ->
        let ty, dptr = split_ptrs ty in
        ty, dptr ^^ blank 1 ^^ star
      );
      Pattern.(typ_ref !__) (fun ty () ->
        let ty, dptr = split_ptrs ty in
        ty, dptr ^^ blank 1 ^^ ampersand
      );
      (* LATER: Also manage arrays *)
      Pattern.__ (fun () -> ty, empty)
    ]
  in

  let common_list = List.fold_lefti (fun i acc ((x, ty), init) ->
    let open Option.Monad in
    let ty, dptr = split_ptrs ty in
    if i = 0 then
      Some (ty, [(x, dptr, init)])
    else
      let* (common_ty, decl_list) = acc in
      if ty = common_ty then Some (common_ty, (x, dptr, init) :: decl_list) else None
    ) None bs in

  let dsemi = if semicolon then semi else empty in

  match common_list with
  | Some (common_ty, decl_list) ->
    (* check if all the declarations are of the same type *)
    let decl_list = List.rev_map (fun (x, dptr, init) ->
      let dinit = match trm_var_inv init with
        | Some v when var_eq v var_uninitialized -> empty
        | _ -> equals ^^ decorate_trm ~force_expr:true style init
      in
      dptr ^^ blank 1 ^^ var_to_doc style x ^^ dinit
      ) decl_list
    in
    let dtx = typ_to_doc style common_ty in
    dtx ^^ blank 1 ^^ list_to_doc ~sep:comma ~bounds:[empty; empty] decl_list ^^ dsemi
  | None ->
    let decl_list = List.map (fun (tv, init) -> trm_let_to_doc ~semicolon:false style tv init) bs in
    list_to_doc ~sep:comma ~bounds:[empty; empty] decl_list ^^ dsemi

(** [aux_class_constructor_to_doc style ]: converst class constructor declaration to pprint document. *)
(* DEPRECATED? _semicolon *)
and aux_class_constructor_to_doc style ?(_semicolon : bool = false)  (spec_annot  : cstyle_annot list) (name : var) (args : typed_vars) (init_l : trm list) (body : trm) : document =
  (* Deprecated? let dsemi = if semicolon then semi else empty in*)
  let argd = if List.length args = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) args) in
  let spec_annot = List.fold_left (fun acc c_annot -> match c_annot with | Class_constructor ck -> ck :: acc | _ -> acc) [] spec_annot in

  let spec_annot = if List.length spec_annot = 1 then List.nth spec_annot 0 else trm_fail body "Ast_to_c.trm_class_constructor_to_doc: catastrophic error" in
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
            (* trm_fail t1 "Ast_to_c.aux_class_constructor_to_doc style: bad member initializer." *)

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

(** [aux_class_destructor_to_doc style ]: converst class constructor declaration to pprint document. *)
and aux_class_destructor_to_doc style ?(semicolon : bool = false)  (spec_annot  : cstyle_annot list) (name : var) (body : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let spec_annot = List.fold_left (fun acc c_annot -> match c_annot with | Class_destructor dk -> dk :: acc | _ -> acc) [] spec_annot in
  let spec_annot = if List.length spec_annot = 1 then List.nth spec_annot 0 else failwith "Ast_to_c.trm_class_constructor_to_doc: catastrophic error" in
  let dt = match spec_annot with
    | Destructor_default -> equals ^^ blank 1 ^^ string "default"
    | Destructor_delete -> equals ^^ blank 1 ^^ string "delete"
    | Destructor_simpl -> decorate_trm style body

   in
  (separate (blank 1) [tilde; var_to_doc style name; lparen ^^ rparen; dt]) ^^ dsemi


(** [aux_fun_to_doc style ~semicolon inline f r tvl b]: converts a function declaration to pprint document *)
and aux_fun_to_doc style ?(semicolon : bool = false) ?(const : bool = false) ?(inline : bool = false) (f : var) (r : typ) (tvl : typed_vars) (b : trm) : document =
  let dinline = if inline then string "inline" else empty in
  let argd = if List.length tvl = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) tvl) in
  let dr = typ_to_doc style r in
  let const = if const then string "const" else empty in
  separate (blank 1) [dinline; dr; var_to_doc style f; parens argd; const; decorate_trm style b]

(** [trm_let_fun_to_doc style]: converts any OptiTrust function declaration(definition) to a pprint document. *)
and trm_let_fun_to_doc style ?(semicolon : bool = false) (fun_annot : cstyle_annot list) (f : var) (r : typ) (args : typed_vars) (b : trm) : document =
  if List.exists (function  | Class_constructor _ -> true | _ -> false ) fun_annot
    then aux_class_constructor_to_doc style fun_annot f args [] b
    else if List.exists (function  | Class_destructor _ -> true | _ -> false ) fun_annot
      then aux_class_destructor_to_doc style ~semicolon fun_annot f b
    else
      let inline = List.mem Fun_inline fun_annot in
      let const = List.mem Const_method fun_annot in
      aux_fun_to_doc style ~semicolon ~const ~inline f r args b

(** [trm_fun_to_doc style ~semicolon ty tvl b]: converts a lambda function from a resource formula to a pprint document. *)
and formula_fun_to_doc style (ty : typ) (tvl : typed_vars) (b : trm) : document =
  string "fun" ^^ blank 1 ^^ separate (comma ^^ blank 1) (List.map (fun (v, _) -> var_to_doc style v) tvl) ^^ blank 1 ^^ string "->" ^^ blank 1 ^^ trm_to_doc style b

(** [trm_fun_to_doc style ~semicolon ty tvl b]: converts a lambda function to a pprint document. *)
and trm_fun_to_doc style ?(semicolon : bool = false) (ty : typ) (tvl : typed_vars) (b : trm) : document =
  let dsemi = if semicolon then semi else empty in
  let argd = if List.length tvl = 0 then empty else separate (comma ^^ blank 1) (List.map (fun tv -> typed_var_to_doc style (var_to_doc style) tv) tvl) in
  let dr = if Typ.is_typ_auto ty then blank 1 else string "->" ^^ blank 1 ^^ typ_to_doc style ty ^^ blank 1 in
  let capt = brackets (ampersand) in
  separate (blank 1) ([capt; parens (argd); dr; decorate_trm style b]) ^^ dsemi

(** [access_ctrl_to_doc style acc_ctrl]: converts [acc_ctrl] to a pprint document. *)
and access_ctrl_to_doc style (acc_ctrl : access_control) : document =
  match acc_ctrl with
  | Access_public -> string "public:"
  | Access_private -> string "private:"
  | Access_protected -> string "protected:"
  | Access_unspecified -> empty

(** [typedef_to_doc style ~semicolon td]: converts a type definition to pprint document *)
and typedef_to_doc style ?(semicolon : bool = true) ?(t_annot : cstyle_annot list = []) (td : typedef) : document =
  let dsemi = if semicolon then semi else empty in
  let dname = var_to_doc style (remove_typ_namespace td.typedef_name) in
  match td.typedef_body with
  | Typedef_alias t ->
     string "typedef" ^^ blank 1 ^^ typed_var_to_doc style (var_to_doc style) (remove_typ_namespace td.typedef_name, t) ^^ dsemi
  | Typedef_record rfl ->
    let get_document_list ?(default_access : access_control = Access_private) (rtl : record_members) : document list =
      let access_ctrl = ref default_access in
      List.fold_left (fun acc (rt, rt_annot) ->
        let fd =
          match rt with
          | Record_field (lb, ty) -> typed_var_to_doc style string (lb, ty) ^^ semi
          | Record_method t1 -> decorate_trm ~semicolon:true style t1
        in
        if rt_annot <> !access_ctrl
            then begin access_ctrl := rt_annot;acc @ [access_ctrl_to_doc style !access_ctrl; fd ] end
            else acc @ [fd ]

      ) [] rfl
       in
      let dl = get_document_list rfl in
      let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
      if List.mem Struct t_annot then
        string "struct" ^^ blank 1 ^^ dname ^^ sbody ^^ blank 1 ^^ semi
      else if List.mem Rec_struct t_annot then
        string "typedef " ^^ string "struct" ^^ blank 1 ^^ dname ^^ blank 1 ^^ sbody ^^ dname ^^ blank 1 ^^ semi
      else if List.mem Class t_annot then
        let dl = get_document_list ~default_access:Access_unspecified rfl in
        let sbody = surround 2 1 lbrace (separate hardline dl) rbrace in
        string "class" ^^ blank 1 ^^ dname ^^ sbody ^^ blank 1 ^^ semi
      else
        string "typedef " ^^ string "struct" ^^ blank 1 ^^ sbody ^^ blank 1 ^^ dname ^^ blank 1 ^^ semi
  | Typedef_union union_const_l -> failwith "Typedef_union printing in C is not yet implemented"
  (*naive idea, but maybe I have to write by hand a function for each constructor? As well as the inversors? Because calling C unions is probably a stretch*)
  | Typedef_enum enum_const_l ->
      let const_doc_l =
        List.map
         (fun (y, t_o) ->
           match t_o with
           | None -> var_to_doc style y
           | Some t -> separate (blank 1) [var_to_doc style y; equals; decorate_trm style t]
         )
        enum_const_l in
      separate (blank 1) [string "enum"; dname;
      braces (separate (comma ^^ blank 1) const_doc_l)] ^^ dsemi

(** [apps_to_doc style ~prec f tl]: converts a function call to pprint document *)
and apps_to_doc style ?(prec : int = 0) ~(annot: trm_annot) ~(print_struct_init_type: bool) (f : trm) (tl : trms) : document =
  let (prec, assoc) = precedence_trm f in
  let aux_arguments f_as_doc =
      f_as_doc ^^ list_to_doc ~sep:comma ~bounds:[lparen; rparen]  (List.map (decorate_trm style ~force_expr:true) tl)
      in
  let is_get_implicit_this t =
    match trm_get_inv t with
    | Some base -> trm_has_cstyle Implicit_this base
    | _ -> false
  in

  match f.desc with
  (* Case of function pointers *)
  (*| Trm_apps ({ desc = (Trm_prim (Prim_unop Unop_get)); _ }, [ { desc = Trm_var x; _ } ], _) ->
      aux_arguments (var_to_doc style x)*)
  | Trm_var x ->
    let var_doc = trm_var_to_doc style x f in
    aux_arguments var_doc
  (* Case of primitive operations *)
  | Trm_prim (ty, p) ->
    begin match p with
    | Prim_unop op ->
        begin match tl with
        | [t] ->
          let d = decorate_trm style ~prec t in
          begin match op with
          (* | Unop_get when style.optitrust_syntax -> star ^^ d *)
          | Unop_get -> star ^^ d
          | Unop_address ->ampersand ^^ d
          | Unop_neg -> bang ^^ d
          | Unop_bitwise_neg -> tilde ^^ d
          | Unop_minus -> minus ^^ blank 1 ^^ d
          | Unop_plus -> plus ^^ blank 1 ^^ d
          | Unop_post_incr -> d ^^ twice plus
          | Unop_post_decr -> d ^^ twice minus
          | Unop_pre_incr -> twice plus ^^ d
          | Unop_pre_decr -> twice minus ^^ d
          | Unop_struct_access f1 when style.optitrust_syntax ->
              string "(" ^^ d ^^ (may_annot_typ style ty (string "[.]")) ^^ string f1 ^^ string ")"
          | Unop_struct_get f1 when style.optitrust_syntax ->
              string "(" ^^ d ^^ string "." ^^ string f1 ^^ string ")"
          | (Unop_struct_get f1 | Unop_struct_access f1) ->
            if is_get_implicit_this t then string f1
            else if is_get_operation t then
                if annot_has_cstyle No_struct_get_arrow annot
                  then
                    d ^^ dot ^^ string f1
                  else
                    let d = decorate_trm style ~force_expr:true ~prec (get_operation_arg t) in
                    d ^^ minus ^^ rangle ^^ string f1
              else d ^^ dot ^^ string f1
          | Unop_cast ty ->
              let dty = typ_to_doc style ty in
              parens dty ^^ blank 1 ^^ d
          end
        | _ ->
          trm_fail f "Ast_to_c.apps_to_doc: unary operators must have one argument"
        end
    | Prim_binop op ->
      let (prec1, prec2) = sub_prec (prec, assoc) in
      begin match tl with
      | [t1; t2] ->
        let d1 = decorate_trm style ~force_expr:true ~prec:prec1 t1 in
        let d2 = decorate_trm style ~force_expr:true ~prec:prec2 t2 in
        begin match op with
          | Binop_exact_div when not (is_typ_float ty) ->
            string "exact_div(" ^^ d1 ^^ comma ^^ space ^^ d2 ^^ string ")"
          (* | Binop_set when style.optitrust_syntax ->
            d1 ^^ string "=" ^^ d2 *)
          | Binop_array_access when style.optitrust_syntax ->
            string "(" ^^ d1 ^^ (may_annot_typ style ty (string "[+]")) ^^ d2 ^^ string ")"
          | Binop_array_access | Binop_array_get ->
            let bracketed_trm t = brackets (decorate_trm style ~prec:0 t) in
            d1 ^^ if not style.pretty_matrix_notation then
              bracketed_trm (t2)
            else begin match Matrix_trm.mindex_inv t2 with
            | None -> bracketed_trm (t2)
            | Some (_dims, indices) -> separate empty (List.map bracketed_trm indices)
            end
          | _ ->
            let op_d = binop_to_doc style op in
            separate (blank 1) [d1; op_d; d2]
          end
      | _ -> trm_fail f "Ast_to_c.apps_to_doc: binary_operators must have two arguments"
      end
    | Prim_compound_assign_op _  ->
      begin match tl with
      | [t1; t2] ->
        let d1 = decorate_trm style ~force_expr:true ~prec t1 in
        let d2 = decorate_trm style ~force_expr:true ~prec t2 in
        let op_d = prim_to_doc style ty p in
        if style.optitrust_syntax
          then op_d ^^ parens (d1 ^^ comma ^^ d2)
          else separate (blank 1) [d1; op_d; d2]
      | _ -> trm_fail f "Ast_to_c.apps_to_doc: expected two arguments"
      end
    | Prim_ref | Prim_new ->
      begin match tl with
      | [value] ->
        let tr_init = decorate_trm style ~force_expr:true ~print_struct_init_type:false value in
        let tr_prim = prim_to_doc style ty p in
        let is_trm_initialization_list (t : trm) : bool =
          match t.desc with
          | Trm_lit Lit_null t when Option.is_none (typ_ptr_inv t) -> true
          | _ -> Option.is_some (trm_array_inv t) || Option.is_some (trm_record_inv t)
        in
        let init_val = if is_trm_initialization_list value then tr_init else parens (tr_init) in
        tr_prim ^^ init_val
      | _ -> trm_fail f "Ast_to_c.apps_to_doc: expected one argument"
      end

    | Prim_ref_uninit | Prim_new_uninit ->
      begin match tl with
      | [] -> prim_to_doc style ty p
      | _ -> trm_fail f "Ast_to_c.apps_to_doc: expected zero arguments"
      end
    | Prim_delete ->
      begin match tl with
      | [ptr] ->
        let tr_ptr = decorate_trm style ~force_expr:true ptr in
        let tr_prim = prim_to_doc style ty p in
        tr_prim ^^ blank 1 ^^ tr_ptr
      | _ -> trm_fail f "Ast_to_c.apps_to_doc: expected one argument"
      end
    | Prim_array ->
      let dl = List.map (decorate_trm style ~print_struct_init_type:false) tl in
      braces (separate (comma ^^ blank 1) dl)
    | Prim_record ->
      let dec_trm (t : trm) = decorate_trm style ~print_struct_init_type:false t in
      let dl = List.map dec_trm tl in
      let init_type = if not print_struct_init_type
        then empty
        else parens (typ_to_doc style ty)
      in
      init_type ^^ blank 1 ^^ braces (separate (comma ^^ blank 1) dl)
    end
  | _ ->
    let f_doc = decorate_trm style f in
    aux_arguments f_doc

(** [mode_to_doc m]: OpenMP mode to pprint document *)
and mode_to_doc (m : mode) : document =
  match m with
  | Shared_m -> string "shared"
  | None_ -> string "none"

(** [sched_type_to_doc st]: OpenMP scheduling type to pprint document *)
and sched_type_to_doc (st : sched_type) : document =
  match st with
  | Static -> string "static"
  | Dynamic -> string "dynamic"
  | Guided -> string "guided"
  | Runtime -> string "runtime"

(** [reduction_identifier_to_doc ri]: OpenMP reduction identifier to pprint document *)
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

(** [map_typ_to_doc mt]: OpenMP map_type to pprint document *)
and map_type_to_doc (mt : map_type) : document =
  match mt with
  | Alloc -> string "alloc" ^^ colon
  | To -> string "to" ^^ colon
  | From -> string "from" ^^ colon
  | ToFrom -> string "tofrom" ^^ colon
  | No_map -> empty

(** [proc_bind_to_doc pb]: OpenMP process bind to pprint document*)
and proc_bind_to_doc (pb : proc_bind) : document =
  match pb with
  | Master_pb -> string "master"
  | Close -> string "close"
  | Spread -> string "spread"

and dep_to_doc (d : dep) : document =
  match d with
  | Dep_var s -> var_to_doc (default_style ()) s  (* TODO #propagate-defaut: propagate style*)
  | Dep_ptr d -> star ^^ dep_to_doc d

(** [dependence_type_to_doc dp]: OpenMP variable dependence type to pprint document *)
and dependence_type_to_doc (dp : dependence_type) : document =
  match dp with
  | In vl -> let vl = List.map dep_to_doc vl in
    string "depend (in" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Out vl -> let vl = List.map dep_to_doc vl in
    string "depend (out" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Inout vl -> let vl = List.map dep_to_doc vl in
    string "depend (inout" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Outin vl -> let vl = List.map dep_to_doc vl in
    string "depend (outin" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Sink vl -> let vl = List.map dep_to_doc vl in
    string "depend (sink" ^^ colon ^^ blank 1 ^^ ( list_to_doc ~sep:comma vl) ^^ rparen
  | Source -> string "source"

(** [clause_to_doc cl]: OpenMP clause to pprint document *)
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

(** [atomic_operation_to_doc ao]: OpenMP atomic operation to pprint document *)
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

(** [directive_to_doc d]: OpenMP directive to pprint document *)
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

(** [routine_to_doc r]: OpenMP routine to pprint document *)
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

(** [unpack_trm_for ~loc index start direction stop step body]: converts a simple for loop to a complex one before converting it to a pprint document *)
(* FIXME: #odoc why is annotation required on callees? *)
and unpack_trm_for ?(loc: location) (range : loop_range) (body : trm) : trm =
  let init = trm_let (range.index, typ_int) range.start in
  let cond = begin match range.direction with
    | DirUp -> trm_apps (trm_binop typ_int Binop_lt) [trm_var range.index; range.stop]
    | DirUpEq -> trm_apps (trm_binop typ_int Binop_le) [trm_var range.index; range.stop]
    | DirDown ->
      trm_apps (trm_binop typ_int Binop_gt) [trm_var range.index; range.stop]
    | DirDownEq ->
      trm_apps (trm_binop typ_int Binop_ge) [trm_var range.index; range.stop]
   end in
  let step =
    begin match range.direction with
    | DirUp | DirUpEq ->
      if trm_is_one range.step && trm_has_cstyle Prefix_step range.step then
        trm_apps (trm_unop typ_int Unop_pre_incr) [trm_var range.index]
      else if trm_is_one range.step && trm_has_cstyle Postfix_step range.step then
        trm_apps (trm_unop typ_int Unop_post_incr) [trm_var range.index]
      else
        trm_apps (trm_prim typ_int (Prim_compound_assign_op Binop_add)) [trm_var range.index; range.step]
    | DirDown | DirDownEq ->
      if trm_is_one range.step && trm_has_cstyle Prefix_step range.step then
        trm_apps (trm_unop typ_int Unop_pre_decr) [trm_var range.index]
      else if trm_is_one range.step && trm_has_cstyle Postfix_step range.step then
        trm_apps (trm_unop typ_int Unop_post_decr) [trm_var range.index]
      else
        trm_apps (trm_prim typ_int (Prim_compound_assign_op Binop_sub) ) [trm_var range.index; range.step]
    end in
    trm_for_c ?loc init cond step body

and formula_to_doc style (f: formula): document =
  let open Resource_formula in
  Pattern.pattern_match f [
    Pattern.(formula_model !__ !__) (fun t formula () ->
      Pattern.when_ (trm_has_cstyle ResourceModel f);
      decorate_trm style t ^^ blank 1 ^^ string "~>" ^^ blank 1 ^^ trm_to_doc style formula
    );
    Pattern.(formula_range !__ !__ (trm_int (eq 1))) (fun start stop () ->
      trm_to_doc ~prec:16 style start ^^ string ".." ^^ trm_to_doc ~prec:16 style stop
    );
    Pattern.(trm_fun !__ !__ !__ __) (fun tvl ty_opt body () -> formula_fun_to_doc style ty_opt tvl body
    );
    Pattern.(trm_apps2 (trm_specific_var var_forall_in) !__ (trm_fun (!__ ^:: nil) __ !__ __)) (fun range (index, _) body () ->
      string "forall" ^^ blank 1 ^^ var_to_doc style index ^^ blank 1 ^^ string "in" ^^ blank 1 ^^ trm_to_doc style range ^^ blank 1 ^^ string "->" ^^ blank 1 ^^ trm_to_doc style body
    );
    Pattern.(trm_apps2 (trm_specific_var var_group) !__ (trm_fun (!__ ^:: nil) __ !__ __)) (fun range (index, _) body () ->
      string "for" ^^ blank 1 ^^ var_to_doc style index ^^ blank 1 ^^ string "in" ^^ blank 1 ^^ trm_to_doc style range ^^ blank 1 ^^ string "->" ^^ blank 1 ^^ trm_to_doc style body
    );
    Pattern.(formula_frac_div !__ !__) (fun base divisor () ->
      Pattern.when_ (style.pretty_fraction_notation);
      trm_to_doc style base ^^ space ^^ string "/" ^^ space ^^ trm_to_doc style divisor
    );
    Pattern.(formula_frac_sub !__ !__) (fun base carved () ->
      Pattern.when_ (style.pretty_fraction_notation);
      trm_to_doc style base ^^ space ^^ string "-" ^^ space ^^ trm_to_doc style carved
    );
    Pattern.(trm_lit (eq (Lit_int (typ_frac, 1)))) (fun () ->
      Pattern.when_ (not style.pretty_fraction_notation);
      string "__full");
    Pattern.__ (fun () -> trm_to_doc style {f with annot = {f.annot with trm_annot_cstyle = []}})
  ]

(** [ast_to_doc t]: converts a full OptiTrust ast to a pprint document. *)
let ast_to_doc style (t : trm) : document =
  decorate_trm style t

(** [ast_to_outchannel out t]: print ast [t] to an out_channel [out] *)
let ast_to_outchannel style (out : out_channel) (t : trm) : unit =
  ToChannel.pretty 0.9 (!Flags.code_print_width) out (ast_to_doc style t)

(** [ast_to_file ~optitrust_syntax filename t]: print ast [t] to file [filename] *)
let ast_to_file style (filename : string) (t : trm) : unit =
  let out = open_out filename in
  ast_to_outchannel style out t;
  close_out out

(** [ast_to_string ~optitrust_syntax t]: converts ast [t] to string *)
let ast_to_string ?(width : PPrint.requirement = 80) ?(style : style = default_style ()) ?(optitrust_syntax : bool option) (t : trm) : string =
  let style = match optitrust_syntax with None -> style | Some b -> { style with optitrust_syntax = b } in
  document_to_string ~width (ast_to_doc style t)

(** [typ_to_string ty]: converts type [ty] to string *)
let typ_to_string ?(style = default_style ()) (ty : typ) : string =
  let b = Buffer.create 80 in
  ToBuffer.pretty 0.9 (!Flags.code_print_width) b (typ_to_doc style ty);
  Buffer.contents b
