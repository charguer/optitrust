open Clang.Ast
open Clang.Bindings
open Ast
open Tools



(* location of node *)
let loc_of_node (n : 'a node) : location =
  let start_location_of_node = Clang.get_range_start (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n))in
  let end_location_of_node = Clang.get_range_end (Clang.get_cursor_extent (Clang.Ast.cursor_of_node n )) in
  let (filename, start_row,start_column) = Clang.get_presumed_location start_location_of_node in
  let (_, end_row,end_column) = Clang.get_presumed_location end_location_of_node in
  Some(filename,start_row,end_row,start_column,end_column)

(* file which contains the node *)
let file_of_node (n : 'a node) : string =
  match loc_of_node n with
  | Some (filename,_,_,_,_) -> filename
  | _ -> fail None "file_of_node: bad location"

(* type to control the way nodes are translated *)
type val_type =
  | Lvalue
  | Rvalue

type scope_kind =
  | For_scope
  | While_scope
  (* todo later *)
  (* todo later *)
  (* | Do_scope *)
  | Switch_scope
  | Other_scope

(*
  map from variables to their type
  used for loops that do not declare their counter
  heap allocated variables are mapped to the type of the variables if they were
  not heap allocated
 *)

(* For the context, see the documentation of type [ctx] in [ast.ml]. *)

(* NOTE at the moment ctx_var is global, meaning that bindings are not
   removed when we leave a scope; LATER: would be cleaner to have ctx_var
   as argument to the recursive function. *)
let ctx_var : typ varmap ref = ref String_map.empty

let ctx_tconstr : typid varmap ref = ref String_map.empty

let ctx_typedef : typedef typmap ref = ref Typ_map.empty

let ctx_label : typid varmap ref = ref String_map.empty

let ctx_constr : typid varmap ref = ref String_map.empty


let ctx_var_add (tv : typvar) (t : typ) : unit =
  ctx_var := String_map.add tv t (!ctx_var)

let debug_typedefs = false

let ctx_tconstr_add (tn : typconstr) (tid : typid) : unit =
  if debug_typedefs then printf "Type %s has been added into map with typid %d\n" tn tid;
  ctx_tconstr := String_map.add tn tid (!ctx_tconstr)

let ctx_typedef_add (tn : typconstr) (tid : typid) (td : typedef) : unit =
  if debug_typedefs then printf "Typedef for %s has been registered\n" tn;
  ctx_typedef := Typ_map.add tid td (!ctx_typedef)

let ctx_label_add (lb : label) (tid : typid) : unit =
  ctx_label := String_map.add lb tid (!ctx_label)

let ctx_constr_add (c : constr) (tid : typid) : unit =
  ctx_constr := String_map.add c tid (!ctx_constr)


(* [get_ctx] returns the current context *)
let get_ctx () : ctx =
  { ctx_var = !ctx_var;
    ctx_tconstr = !ctx_tconstr;
    ctx_typedef = !ctx_typedef;
    ctx_label = !ctx_label;
    ctx_constr = !ctx_constr; }

let get_typid (tv : typvar) : int  =
   let tid = String_map.find_opt tv !ctx_tconstr in
   begin match tid with
   | Some id -> id
   | None -> -1
   end
(* mutable_vars contains the information on which variables are [Var_mutable]
  stack of lists of heap allocated variables
  each list corresponds to a new scope
  when a scope is closed, the corresponding variables must be deleted
*)
let mutable_vars : (scope_kind * (string list)) Stack.t = Stack.create ()

let is_mutable_var (x : string) : bool =
  Stack.fold (fun b (_, sl) -> b || List.mem x sl) false mutable_vars

(* scope opening instruction *)
let open_scope (kind : scope_kind) : unit = Stack.push (kind, []) mutable_vars

(* add a heap allocated variable to the current scope *)
let add_var (s : string) : unit =
  let (kind, sl) = Stack.pop mutable_vars in
  Stack.push (kind, (s :: sl)) mutable_vars

(*
  scope closing instruction
  t represents the part of the program in the current scope
 *)
let close_scope ?(_loc : location = None) (t : trm) : trm =
  match Stack.pop mutable_vars with
  | (_, []) -> t
  | _ -> t

(* manage a new scope while translating a statement *)
let compute_scope ?(loc : location = None) (kind : scope_kind) (f : unit -> trm) : trm =
  open_scope kind;
  close_scope ~_loc:loc (f ())

(*
  put the appropriate sequence of delete instructions before a return
  instruction given by t
  put back an empty list of variables to the mutable_vars stack since the scope is
  not closed
 *)
let return (t : trm) : trm =
  (* let tl = Stack.fold (fun tl (_, sl) -> tl ++ (delete_list sl)) [] mutable_vars in *)
  let tl = Stack.fold (fun tl (_,_) -> tl) [] mutable_vars in
  let (kind, _) = Stack.pop mutable_vars in
  open_scope kind;
  match tl with
  | [] -> t
  | _ -> trm_seq (* ~annot:(Some Delete_instructions) *) (tl ++ [t])

(*
  return the number of scopes to exit before a break/continue instruction
  look for closest for/while scope
  todo later: do scope too
 *)
let find_scope ?(break : bool = false) (kl : scope_kind list) : int =
  let rec aux (n : int) = function
    | []
    | For_scope :: _
    | While_scope :: _ ->
      n
    | Switch_scope :: kl ->
      if break then n else aux (n + 1) kl
    | _ :: kl -> aux (n + 1) kl
  in
  (* start with 1 since current scope must be exited *)
  aux 1 kl

(* compute the list of scope kinds (innermost to outermost) from current one *)
let scope_list () : scope_kind list =
  List.rev (Stack.fold (fun kl (kind, _) -> kind :: kl) [] mutable_vars)

(* return the list of the n top elements in the stack *)
let ntop (n : int) (s : 'a Stack.t) : 'a list =
  let rec aux (n : int) (acc : 'a list) : 'a list =
    if n = 0 then acc else aux (n - 1) (Stack.pop s :: acc)
  in
  try
    let al = aux n [] in
    List.iter (fun a -> Stack.push a s) al;
    List.rev al
  with
  | Stack.Empty -> fail None "ntop: bad expected number of elements"

(*
  put the appropriate sequence of delete instructions before a break/continue
  instruction given by t
  put back an empty list of variables to the mutable_vars stack since the scope is
  not closed
 *)
let abort ?(break : bool = false) (t : trm) : trm =
  let n = find_scope ~break (scope_list ()) in
  (* put the delete instruction for the n deepest scopes *)
  let tl =
    (* List.fold_left (fun tl (_, sl) -> tl ++ (delete_list sl)) []
      (ntop n mutable_vars) *)
    List.fold_left (fun tl (_,_) -> tl) []
      (ntop n mutable_vars)
  in
  let (kind, _) = Stack.pop mutable_vars in
  open_scope kind;
  match tl with
  | [] -> t
  | _ -> trm_seq (* ~annot:(Some Delete_instructions) *) (tl ++ [t])

(* names for overloaded operators (later matched for printing) *)
 let string_of_overloaded_op ?(loc : location = None)
    (op : clang_ext_overloadedoperatorkind) : string =
  match op with
  | Plus -> "overloaded+"
  | Minus -> "overloaded-"
  | Star -> "overloaded*"
  | Equal -> "overloaded="
  | PlusEqual -> "overloaded+="
  | MinusEqual -> "overloaded-="
  | StarEqual -> "overloaded*="
  | _ -> fail loc "string_of_overloaded_op: non supported operator"

let rec translate_type_desc ?(loc : location = None) ?(const : bool = false) (d : type_desc) : typ =
  match d with
  | Pointer q ->
    let t = translate_qual_type ~loc q in
    let {const;_} = q in
    if const then
      (typ_ptr (typ_const t))
    else
    typ_ptr t
  | ConstantArray {element = q; size = n; size_as_expr = eo} ->
    let t = translate_qual_type ~loc q in
    let {const;_} = q in
    begin match eo with
      | None -> typ_array t (Const n)
      | Some e ->
        let s = translate_expr e in
        if const then
           typ_array (typ_const t) (Trm s)
        else
          typ_array t (Trm s)
    end
  | VariableArray {element = q; size = eo} ->
    let t = translate_qual_type ~loc q in
    let s = translate_expr eo in
    typ_array t (Trm s)
  | IncompleteArray q ->
    let t = translate_qual_type ~loc q in
    typ_array t Undefined
  | BuiltinType b ->
    begin match b with
      | Void -> typ_unit ()
      | Bool -> if const then typ_const (typ_bool ()) else typ_bool ()
      | Int -> if const then typ_const (typ_int ()) else typ_int ()
      | UInt -> if const then typ_const (typ_int ~annot:[Unsigned] ()) else typ_int ~annot:[Unsigned] ()
      | Long -> if const then typ_const (typ_int ~annot:[Long] ()) else typ_int ~annot:[Long] ()
      | ULong -> if const then typ_const (typ_int ~annot:[Unsigned; Long] ()) else typ_int ~annot:[Unsigned; Long] ()
      | LongLong -> if const then typ_const (typ_int ~annot:[Long; Long] ()) else typ_int ~annot:[Unsigned; Long] ()
      | ULongLong -> if const then typ_const (typ_int ~annot:[Unsigned; Long; Long] ()) else typ_int ~annot:[Unsigned; Long; Long] ()
      | Float -> if const then typ_const (typ_float ()) else typ_float ()
      | Double -> if const then typ_const (typ_double ()) else typ_double ()
      | LongDouble -> if const then typ_const (typ_double ~annot:[Long] ()) else typ_double ~annot:[Long] ()
      | Char_S -> if const then typ_const (typ_char ()) else typ_char ()
      | UChar -> if const then typ_const (typ_char ~annot:[Unsigned] ()) else typ_char ~annot:[Unsigned] ()
      | Short -> if const then typ_const (typ_int ~annot:[Short] ()) else typ_int ~annot:[Short] ()
      | UShort -> if const then typ_const (typ_int ~annot:[Unsigned; Short] ()) else typ_int ~annot:[Unsigned; Short] ()
      | _ -> fail loc "translate_type_desc: builtin type not implemented"
    end
  | FunctionType {calling_conv = _; result = qr; parameters = po;
                  exception_spec = _; _} ->
    let tr = translate_qual_type ~loc qr in
    begin match po with
      | None -> typ_fun [] tr
      | Some {non_variadic = pl; variadic = _} ->
        let tl =
          List.map
            (fun (p : parameter) -> translate_qual_type ~loc p.desc.qual_type)
            pl
        in
        typ_fun tl tr
    end
  | Typedef {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
        typ_constr n (get_typid n) []
      | _ -> fail loc ("translate_type_desc: only identifiers are allowed in " ^
                       "type definitions")
    end
  | Elaborated {keyword = k; nested_name_specifier = _; named_type = q} ->
    begin match k with
      | Struct ->
        translate_qual_type ~loc q
      | _ ->
        fail loc "translate_type_desc: only struct allowed in elaborated type"
    end
  | Record {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
         typ_constr n (get_typid n) []
      | _ -> fail loc ("translate_type_desc: only identifiers are allowed in " ^
                       "records")
    end
  | Enum {nested_name_specifier = _; name = n; _} ->
    begin match n with
      | IdentifierName n ->
         typ_constr n (get_typid n) []
      | _ -> fail loc ("translate_type_desc: only identifiers are allowed in " ^
                       "enums")
    end
  | _ -> fail loc "translate_type_desc: not implemented"

and translate_qual_type ?(loc : location = None) (q : qual_type) : typ =
  let ({desc = d; const = c; _} : qual_type) = q in
  translate_type_desc ~loc ~const:c d

and translate_ident (id : ident_ref node) : string =
  let {decoration = _; desc = {nested_name_specifier = _; name = n; _}} = id in
  match n with
  | IdentifierName s -> s
  | _ ->
    let loc = loc_of_node id in
    fail loc "translate_ident_ref: not implemented"

and translate_stmt (s : stmt) : trm =
  let loc = loc_of_node s in
  let ctx = Some (get_ctx ()) in
  match s.desc with
  | Compound sl ->
    compute_scope ~loc Other_scope
      (fun () -> trm_seq ~loc ~ctx (List.map translate_stmt sl))
  | If {init = None; condition_variable = None; cond = c; then_branch = st;
        else_branch = seo} ->
    let tc = translate_expr c in
    let tt = compute_scope Other_scope (fun () -> translate_stmt st) in
    begin match seo with
      | None -> trm_if ~loc ~ctx tc tt (trm_lit Lit_unit)
      | Some se ->
        let te = compute_scope Other_scope (fun () -> translate_stmt se) in
        trm_if ~loc ~ctx tc tt te
    end
  | If _ ->
    fail loc "translate_stmt: variable declaration forbidden in if conditions"
  | While {condition_variable = _; cond = c; body = s} ->
    let tc = translate_expr c in
    let ts = compute_scope While_scope (fun () -> translate_stmt s) in
    trm_while ~loc ~ctx  tc ts
  (* todo: use while encoding in semantics *)
  | For {init = inito; condition_variable = None; cond = condo; inc = stepo;
         body} ->
    let translate_stmt_opt (so : stmt option) : trm =
      match so with
      | None -> trm_lit ~loc ~ctx  Lit_unit
      | Some s -> translate_stmt s
    in
    (* put a scope around the for loop for the counter declaration *)
    compute_scope Other_scope
      (fun () ->
         let init = translate_stmt_opt inito in
         let cond =
           match condo with
           (* no condition is equivalent to true *)
           | None -> trm_lit ~annot:(Some Empty_cond) ~loc ~ctx (Lit_bool true)
           | Some e -> translate_expr e
         in
         let step = translate_stmt_opt stepo in
         let body = compute_scope For_scope (fun () -> translate_stmt body) in
         trm_for ~loc ~ctx init cond step body
      )
  | For _ ->
    fail loc "translate_stmt: variable declaration forbidden in for conditions"
  | Return eo ->
    begin match eo with
      | None -> return (trm_abort ~loc ~ctx (Ret None))
      | Some e ->
        let t = translate_expr e in
        return (trm_abort ~loc ~ctx (Ret (Some t)))
    end
  | Break -> abort ~break:true (trm_abort ~loc ~ctx Break)
  | Continue -> abort (trm_abort ~loc ~ctx Continue)
  | Decl dl ->
    begin match dl with
      | [] -> fail loc "translate_stmt: empty declaration list"
      | [d] -> translate_decl d
      | _ -> trm_seq ~annot:(Some Multi_decl) ~loc ~ctx (translate_decl_list dl)
    end
  | Expr e -> translate_expr ~is_statement:true e
  | Label {label = l; body = s} ->
    let t = translate_stmt s in
    trm_labelled ~loc ~ctx l t
  | Null -> trm_lit ~loc ~ctx Lit_unit
  | Switch {init = None; condition_variable = None; cond = c;
            body = s} ->
    begin match s.desc with
      | Compound sl ->
        compute_scope Switch_scope (fun () -> translate_switch loc c sl)
      | _ ->
        fail loc "translate_stmt: switch cases must be in a compound statement"
    end
  | Switch _ ->
    fail loc
      "translate_stmt: variable declaration forbidden in switch conditions"
  | Goto l -> trm_goto ~loc ~ctx l
  | _ ->
    fail loc ("translate_stmt: the following statement is unsupported: " ^
              Clang.Stmt.show s)

(*
  translation of switch statements:
  - nested cases: only full sharing allowed
      case bla: case bli: … break allowed
      case bla: …; case bli: … break forbidden
  - warning about clangml ast:
      case bla: instr1; instr2; break represented as
      Case {lhs = bla; body = instr1}; instr2; break in case list
 *)
and translate_switch (loc : location) (cond : expr) (cases : stmt list) : trm =
  let t = translate_expr cond in
  let rec aux (loc : location) = function
    | [] -> []
    | s :: sl ->
      let loc' = loc_of_node s in
      begin match s.desc with
        | Case _ ->
          let (casel, s') = compute_cases [] s in
          let (body, sl') = compute_body loc [] (s' :: sl) in
          (casel, body) :: (aux loc' sl')
        | Default s' ->
          let (body, sl') = compute_body loc' [] (s' :: sl) in
          ([], body) :: (aux loc' sl')
        | _ -> fail loc "translate_switch: case or default expected"
      end
  in
  trm_switch ~loc ~ctx:(Some (get_ctx ())) t  (aux loc cases)

(*
  compute the list of nested cases described by s in reverse order and the first
  instruction of their body
 *)
and compute_cases (case_acc : trm list) (s : stmt) : trm list * stmt =
  let loc = loc_of_node s in
  match s.desc with
  | Case {lhs = e; rhs = None; body = s'} ->
    let t = translate_expr e in
    compute_cases (t :: case_acc) s'
  | Case _ -> fail loc "compute_cases: case ranges forbidden"
  | Default _ ->
    fail loc "compute_cases: please replace nested cases with default only"
  | _ -> (List.rev case_acc, s)

(*
  compute the body of a (nested) case starting from the beginning of the list
  stop at first break or fail if none
  return the rest of the case list too
  to find the first break, cases must be written without compound statements
    -> no variable declaration in cases
*)
and compute_body (loc : location) (body_acc : trm list)
    (sl : stmt list) : trm * (stmt list) =
  match sl with
  | [] -> fail loc "compute_body: cases must end with break"
  | s :: sl ->
    begin match s.desc with
      | Case _ | Default _ ->
        fail loc "compute_body: nested cases must share their whole content"
      | Break ->
        begin match List.rev body_acc with
          | [t] -> (t, sl)
          | tl -> (trm_seq ~annot:(Some No_braces) ~loc ~ctx:(Some (get_ctx ())) tl, sl)
        end
      | _ ->
        let t = translate_stmt s in
        compute_body loc (t :: body_acc) sl
    end

(* To translate the address of operator (&t), we translate just t
   as if it was a Lvalue, with [~val_t=Lvalue].
   When we translate a Lvalue of the form [*t], we translate just
   [t] as if it was a Rvalue. *)

and translate_expr ?(val_t = Rvalue) ?(is_statement : bool = false)
    (e : expr) : trm =
  let loc = loc_of_node e in
  let typ : typ option =
    let q = Clang.Type.of_node e in
    try Some (translate_qual_type ~loc q) with
    | _ ->
      print_info loc "translate_expr: unable to translate type %s\n"
        (Clang.Type.show q);
      None
  in
  let ctx = Some (get_ctx()) in
  match e.desc with
  | ConditionalOperator {cond; then_branch = Some e_then;
                         else_branch = e_else} ->
    let t_cond = translate_expr cond in
    let t_then = translate_expr e_then in
    let t_else = translate_expr e_else in
    trm_apps ~loc ~is_statement ~typ ~ctx (trm_prim ~loc ~ctx Prim_conditional_op)
      [t_cond; t_then; t_else]
  | ConditionalOperator _ ->
    fail loc
      "translate_expr: conditional operators without then branch unsupported"
  | IntegerLiteral i ->
    begin match i with
      | Int i -> trm_lit ~loc ~ctx (Lit_int i)
      | _ -> fail loc "translate_expr: only int literal allowed"
    end
  | BoolLiteral b -> trm_lit ~loc ~ctx (Lit_bool b)
  | FloatingLiteral f ->
    begin match f with
      | Float f -> trm_lit ~loc ~ctx (Lit_double f)
      | _ -> fail loc "translate_expr: only float literal allowed"
    end
  | StringLiteral {byte_width = _; bytes = s; string_kind = _} ->
    trm_lit ~loc ~ctx (Lit_string s)


  | InitList el ->
    (* maybe typ is already the value of tt ---let tt = translate_qual_type ~loc t in *)
    let tt = match typ with
      | None -> fail loc ("unable to obtain type of an initialization list")
      | Some ty -> ty
    in
    let tl = List.map translate_expr el in
    begin match tt.typ_desc with
      | Typ_array _ -> trm_array ~loc ~ctx ~typ:(Some tt) tl
      (* TODO: Fix this later *)
      | Typ_constr _ -> trm_struct ~loc ~ctx ~typ:(Some tt)  tl
      | Typ_var _ -> (* assumption: typedefs are only for struct *)
        trm_struct ~ctx ~loc ~typ:(Some tt)  tl
      | _ ->
        fail loc ("translate_decl: initialisation lists only " ^
                  "allowed for struct and array")
    end

  | UnaryExpr {kind = k; argument = a} ->
    begin match k with
      | SizeOf ->
        begin match a with
          | ArgumentExpr e ->
            let t = translate_expr e in
            trm_apps ~loc ~typ ~ctx (trm_var ~loc "sizeof") [t]
          | ArgumentType q ->
            let ty = translate_qual_type q in
            trm_var ~loc ~typ ~ctx ("sizeof(" ^ Ast_to_c.typ_to_string ty ^ ")")
        end
      | _ -> fail loc "translate_expr: unsupported unary expr"
    end
  | UnaryOperator {kind = k; operand = e} ->
    let loc = (* deduce location of infix symbol *)
      match loc, loc_of_node e with
      | Some (file,line1,col1,_,_), Some (_,line2,col2,_,_) ->
          Some (file,line1,col1,line2,col2)
      | _ -> None
      in
    begin match k with
      | AddrOf -> (* expectation: e is not a const variable *)
        (* We are translating a term of the form that involves [&p],
           for example, [int p = 3; f(&p)]. In our AST, [p] represents
           the address of the cell at which [3] is stored, thus the
           call is actually [f(p)]. In other words we drop the [&] operator. *)
        let {desc; annot; loc; add; attributes; _} =
          translate_expr ~val_t:Lvalue e
        in
        { desc;
          annot;
          loc; is_statement;
          add = Add_address_of_operator :: add;
          ctx;
          typ;
          attributes }
      | _ ->
        begin match k with
          | PostInc ->
            let t = translate_expr ~val_t:Lvalue e in
            trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc Unop_inc) [t]
          | PostDec ->
            let t = translate_expr ~val_t:Lvalue e in
            trm_apps ~loc ~is_statement ~typ ~ctx (trm_unop ~loc Unop_dec) [t]
          | Deref ->
            let t = translate_expr e in
            begin match val_t with
              | Lvalue ->
                (* We are translating a term t of the form [*p] that occurs
                   on the left-hand side of an assignment, such as [*p = v].
                   We want to encode the latter as [set(p, v)], this is why we
                   want to drop the [*] operator. *)
                { annot = t.annot;
                  desc = t.desc;
                  loc = t.loc;
                  is_statement = t.is_statement;
                  add = Add_star_operator :: t.add;
                  typ;
                  ctx;
                  attributes = t.attributes}
              | Rvalue -> trm_apps ~loc ~typ ~ctx (trm_unop ~loc Unop_get) [t]
            end
          | Minus ->
            let t = translate_expr e in
            trm_apps ~loc ~typ ~ctx (trm_unop ~loc ~ctx Unop_opp) [t]
          | Not ->
            let t = translate_expr e in
            trm_apps ~loc ~typ ~ctx (trm_unop ~loc ~ctx Unop_bitwise_neg) [t]
          | LNot ->
            let t = translate_expr e in
            trm_apps ~loc ~typ ~ctx (trm_unop ~loc ~ctx Unop_neg) [t]
          | _ -> fail loc "translate_expr: unary operator not implemented"
        end
    end
  | BinaryOperator {lhs = le; kind = k; rhs = re} ->
    let loc = (* deduce location of infix symbol *)
      match loc_of_node le, loc_of_node re with
      | Some (file,_,_,line1,col1), Some (_,line2,col2,_,_) ->
          Some (file,line1,col1,line2,col2)
      | _ -> None
      in
    let tr = translate_expr re in
    begin match k with
      | Assign ->
        let tl = translate_expr ~val_t:Lvalue le in
        trm_set ~loc ~ctx ~is_statement tl tr
      | AddAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~is_statement  tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_add) [tlr; tr])
      | SubAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx ~is_statement tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_sub) [tlr; tr])
      | MulAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~is_statement ~ctx tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_mul) [tlr; tr])
      | DivAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~is_statement ~ctx tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_div) [tlr; tr])
      | RemAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~is_statement ~ctx tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_mod) [tlr; tr])
      | ShlAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx ~is_statement  tll
          (trm_apps ~loc ~typ ~ctx (trm_binop ~loc ~ctx Binop_shiftl) [tlr; tr])
      | ShrAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx  ~is_statement tll
          (trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx  Binop_shiftr) [tlr; tr])
      | AndAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx  ~is_statement tll
          (trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_and) [tlr; tr])
      | OrAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx ~is_statement tll
          (trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_or) [tlr; tr])
      | XorAssign ->
        let tll = translate_expr ~val_t:Lvalue le in
        let tlr = translate_expr ~val_t:val_t le in
        trm_set ~annot:(Some App_and_set) ~loc ~ctx ~is_statement tll
          (trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_xor) [tlr; tr])
      | _ ->
        let tl = translate_expr ~val_t:val_t le in
        begin match k with
          | Mul -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_mul) [tl; tr]
          | Div -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_div) [tl; tr]
          | Add -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_add) [tl; tr]
          | Sub -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_sub) [tl; tr]
          | LT ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_lt) [tl; tr]
          | GT ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_gt) [tl; tr]
          | LE ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_le) [tl; tr]
          | GE ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_ge) [tl; tr]
          | EQ ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_eq) [tl; tr]
          | NE ->  trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_neq) [tl; tr]
          | And -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_bitwise_and) [tl; tr]
          | LAnd -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_and) [tl; tr]
          | Or -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_bitwise_or) [tl; tr]
          | LOr -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_or) [tl; tr]
          | Shl -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_shiftl) [tl; tr]
          | Shr -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_shiftr) [tl; tr]
          | Rem -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_mod) [tl; tr]
          | Xor -> trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_xor) [tl; tr]
          | _ -> fail loc "translate_expr: binary operator not implemented"
        end
    end
  | Call {callee = f; args = el} ->
    let tf = translate_expr f in
    begin match tf.desc with
    (* TODO: later think about other cases to handle here *)
    | Trm_var x when Str.string_match (Str.regexp "overloaded=") x 0 ->
        begin match el with
        | [tl;tr] -> trm_set ~loc ~ctx  ~is_statement (translate_expr ~val_t:Lvalue tl) (translate_expr tr)
        | _ -> fail loc "translate_expr: overloaded= expects two arguments"
        end
    | _-> trm_apps ~loc ~ctx  ~is_statement ~typ tf (List.map translate_expr el)
    end
  | DeclRef {nested_name_specifier = _; name = n; _} -> (* Occurrence of a variable *)
    begin match n with
      | IdentifierName s ->
        (*
          particular case for variables:
          we look at the type given in their declaration to take into account
          type aliases
         *)
        let typ : typ option =
          (*
            clangml version: does not work (q = InvalidType for several vars in
            picGoal.cpp)
           *)
          (* let q = Clang.Type.of_cursor (Clang.Expr.get_definition e) in
           * Some (translate_qual_type ~loc q) *)
          (* hack with ctx_var *)
          String_map.find_opt s !ctx_var
        in
        begin match val_t with
          | Rvalue when is_mutable_var s ->
            (* LATER: the Heap_allocated annotation on get should be replaced with
               a Var_mutable argument passed to trm_var *)
            trm_apps ~annot:(Some Mutable_var_get) ~loc ~ctx  ~typ
              (trm_unop ~loc ~ctx  Unop_get) [trm_var ~loc ~ctx  s]
          | _ -> trm_var ~loc ~typ s
        end
      | OperatorName op -> trm_var ~loc ~ctx ~typ (string_of_overloaded_op ~loc op)
      | _ -> fail loc "translate_expr: only identifiers allowed for variables"
    end
  | Member {base = eo; arrow = b; field = f} -> (* TODO: ARTHUR relire bien *)
    begin match eo with
      | None -> fail loc "translate_expr: field accesses should have a base"
      | Some e ->
        begin match f with
          | FieldName id ->
            let f = translate_ident id in
            let base = translate_expr ~val_t:Lvalue e in
           (*
             use struct_get when the base is a variable not heap allocated or
             the result of a struct_get/array_get
             question: other cases?
            *)
            (*
              case1: b was a function argument
              case2: b was a stack variable turned into heap variable
              case3: b was a const  --like in case 1.

              b->f
              b.f

              case1: b.f   is struct_get f b,   b->f   is just ( * b).f  meaning (struct_get f (get b))
              case2: b.f   translates to  b->f
                     b->f   translates to ( * b) -> f
                     the call to translate base already gives you "b" as "*b"

            *)
            begin match base.desc with
              | Trm_var x when not (is_mutable_var x) ->
                let base =
                  if b (* if arrow instead of dot *)
                    then trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx Unop_get) [base]   (* Code is [b->f], we encode it as [( *b ).f] *)
                    else base (* code is [b.f] *)
                  in
                    (* fail loc
                      "translate_expr: 1arrow field access should be on a pointer" *)
                trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx (Unop_struct_get f)) [base]
              | Trm_apps
                  ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get _))); _}, _)
              | Trm_apps
                  ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get)); _},
                   _) ->
                if b then
                  fail loc
                    "translate_expr: 2arrow field access should be on a pointer"
                else
                  trm_apps ~loc ~ctx ~typ (trm_unop ~loc (Unop_struct_get f)) [base]
              | _ ->
                let t =
                  if b then trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx Unop_get) [base]
                  else base
                in
                let res =
                  trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx (Unop_struct_access f)) [t]
                in
                begin match val_t with
                  | Lvalue -> res
                  | Rvalue ->
                    trm_apps ~annot:(Some Access) ~typ ~loc ~ctx
                      (trm_unop ~loc ~ctx Unop_get) [res]
                end
            end
          | _ -> fail loc "translate_expr: fields must be accessed by name"
        end
    end
  | ArraySubscript {base = e; index = i} ->
    let ti = translate_expr i in
    let te = translate_expr ~val_t:Lvalue e in
     (*
       override typ to account for typedefs:
       if e's type is x*, make sure typ is x (it is not always the case if x is
       declared through a typedef)
      *)
    let typ =
      match te.typ with
      | Some {typ_desc = Typ_array (ty, _); _} -> Some ty
      | Some {typ_desc = Typ_ptr ty; _} -> Some ty
      (* should not happen *)
      | _ -> None
    in
     (*
       as for struct_get, use array_get when te is a variable not heap allocated
       or the result of a struct_get/array_get
      *)
    begin match te.desc with
      | Trm_var x when not (is_mutable_var x) ->
        trm_apps ~loc ~ctx ~typ (trm_binop ~ctx ~loc Binop_array_get) [te; ti]
      | Trm_apps
          ({desc = Trm_val (Val_prim (Prim_unop (Unop_struct_get _))); _}, _)
      | Trm_apps
          ({desc = Trm_val (Val_prim (Prim_binop Binop_array_get)); _},
           _) ->
        trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_array_get) [te; ti]
      | _ ->
        let res =
          trm_apps ~loc ~ctx ~typ (trm_binop ~loc ~ctx Binop_array_access) [te; ti]
        in
        begin match val_t with
          | Lvalue -> res
          | Rvalue ->
            trm_apps ~annot:(Some Access) ~loc ~ctx ~typ (trm_unop ~loc ~ctx Unop_get)
              [res]
        end
    end
  | Construct {qual_type = _; args = el} ->
    (* only known use case: return of a struct variable *)
    begin match el with
      | [e] -> translate_expr ~is_statement ~val_t:val_t e
      | _ -> fail loc "translate_expr: unsupported construct"
    end
  | Cast {kind = k; qual_type = q; operand = e'} ->
    begin match k with
      | CStyle | Static ->
        let t = translate_qual_type ~loc q in
        let te' = translate_expr e' in
        trm_apps ~loc ~ctx ~typ (trm_unop ~loc ~ctx (Unop_cast t)) [te']
      | _ -> fail loc "translate_expr: only static casts are allowed"
    end
  | New {placement_args = _; qual_type = q; array_size = seo; init = ieo} ->
    let tq = translate_qual_type ~loc q in
    begin match ieo with
      | None -> ()
      | Some _ ->
        print_info loc
          "translate_expr: ignoring initialisation in new statement\n"
    end;
    begin match seo with
      | None -> trm_prim ~loc ~ctx (Prim_new tq)
      | Some se ->
        begin match translate_expr se with
          | {desc = Trm_val (Val_lit (Lit_int n)); loc; _} ->
            trm_prim ~loc ~ctx (Prim_new (typ_array tq (Const n)))
          | {desc = Trm_var x; loc; _} ->
            trm_prim ~loc ~ctx (Prim_new (typ_array tq (Trm (trm_var ~loc ~ctx x))))
          | _ ->
            fail loc ("translate_expr: new array size must be either " ^
                      "constant or variable")
        end
    end
  | UnexposedExpr ImplicitValueInitExpr ->
    print_info loc "translate_expr: implicit initial value\n";
    trm_lit ~loc ~ctx Lit_uninitialized
  (* sometimes Null is translated like this *)
  | UnknownExpr (GNUNullExpr, GNUNullExpr) -> trm_null ~loc ~ctx ()
  | ImplicitValueInit _ -> trm_lit ~loc ~ctx Lit_uninitialized
  | _ ->
    fail loc
      ("translate_expr: the following expression is unsupported: " ^
       Clang.Expr.show e)

and translate_attribute (loc : location) (a : Clang.Ast.attribute) : attribute =
  match a.desc with
  | Aligned {spelling = _; alignment_expr = e} -> Aligned (translate_expr e)
  | _ -> fail loc "translate_attribute: unsupported attribute"

and translate_decl_list (dl : decl list) : trm list =
  let loc =
    (* some recursive calls might be on the empty list *)
    match dl with
    | d :: _ -> loc_of_node d
    | _ -> None
  in
  match dl with
  | [] -> []
  | [{decoration = _; desc = RecordDecl _}] ->
    fail loc ("translate_decl_list: record declarations must be followed by " ^
              "type definitions")
  | [d] -> [translate_decl d]
  | {decoration = _; desc = RecordDecl {keyword = k; attributes = _;
                                        nested_name_specifier = _; name = rn;
                                        bases = _; fields = fl; final = _;
                                        complete_definition = _}} ::
    {decoration = _; desc = TypedefDecl {name = tn; underlying_type = _q}} ::
    dl' ->
    begin match k with
      | Struct ->
        (* typedef struct rn { int x,y; } tn;
           is only allowed if rn is empty or same as tn. *)
        if rn <> "" && rn <> tn
          then fail loc (sprintf "Typedef-struct: the struct name (%s) must match the typedef name (%s).\n" tn rn);

        (* First add the constructor name to the context, needed for recursive types *)
        let tid = next_typid () in
        ctx_tconstr_add tn tid;

        (* Second, parse the fields names and types *)
        let prod_list = List.map ( fun (d : decl) ->
          let loc = loc_of_node d in
          match d with
          | {decoration = _; desc = Field {name = fn; qual_type = q;
                                              bitwidth = _; init = _;
                                              attributes = al}} ->
            ctx_label_add fn tid;
            let ft = translate_qual_type ~loc q in
            let al = List.map (translate_attribute loc) al in
            let ty = {ft with typ_attributes = al} in
            (fn, ty)
          | _ -> fail loc ("translate_decl_list: only fields are allowed
                                in struct declaration")

        ) fl in
        (* Third, add the typedef to the context *)
        let two_names = if rn = "" then false else true in
        let td = {
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_prod (two_names, (List.rev prod_list))
          } in
        ctx_typedef_add tn tid td;
        let trm_td = trm_typedef ~loc ~ctx:(Some (get_ctx())) td in
        let tl' = translate_decl_list dl' in
        trm_td :: tl'

      | _ -> fail loc "translate_decl_list: only struct records are allowed"
    end
  | d :: d' :: dl ->
    let td = translate_decl d in
    let tl = translate_decl_list (d' :: dl) in
    td :: tl

and translate_decl (d : decl) : trm =
  let loc = loc_of_node d in
  let ctx = Some (get_ctx ()) in
  match d.desc with
  | EnumDecl {name = tn; constants; _} ->
    let enum_constant_l =
      List.map
        (fun ({desc = {constant_name; constant_init}; _} : enum_constant) ->
           match constant_init with
           | None -> ((constant_name, None) : var * (trm option))
           | Some e ->
             let t_init = translate_expr e in
             (constant_name, Some t_init)
        )
        constants
    in
    let tid = next_typid () in
    ctx_tconstr_add tn tid;
    let td = {
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_enum enum_constant_l
    } in
    ctx_typedef_add tn tid td;
    trm_typedef ~loc ~ctx td
  | Function {linkage = _; function_type = t; nested_name_specifier = _;
              name = n; body = bo; deleted = _; constexpr = _; _} ->
    let s =
      begin match n with
        | IdentifierName s -> s
        | OperatorName op -> string_of_overloaded_op ~loc op
        | _ ->
          fail loc ("translate_decl: only identifiers and overloaded " ^
                    "operators allowed for functions")
      end
    in
    let {calling_conv = _; result = _; parameters = po;
         exception_spec = _; _} = t in
    let tt = translate_type_desc ~loc (FunctionType t) in
    begin match tt.typ_desc with
      | Typ_fun (args_t, out_t) ->
        begin match po with
          | None ->
            if List.length args_t != 0 then
              fail loc "translate_decl: wrong size of argument list";
            let tb =
              match bo with
              | None -> trm_lit ~loc Lit_uninitialized
              | Some s -> translate_stmt s
            in
            trm_let_fun ~loc s out_t  [] tb
          | Some {non_variadic = pl; variadic = _} ->
            let args =
              List.combine
                (List.map
                   (fun {decoration = _;
                         desc = {qual_type = _; name = n; default = _}} -> n)
                   pl
                )
                args_t
            in
            List.iter (fun (y, ty) -> ctx_var_add y ty) args;
            let tb =
              match bo with
              | None -> trm_lit ~loc Lit_uninitialized
              | Some s -> translate_stmt s
            in
            trm_let_fun ~loc s out_t  args tb
        end
      |_ -> fail loc "translate_decl: should not happen"
    end
  | Var {linkage = _; var_name = n; var_type = t; var_init = eo; constexpr = _; _} ->
    let {const;_} = t in
    let tt = translate_qual_type ~loc t in
    let te =
      begin match eo with
      | None ->
        if const then trm_lit ~loc Lit_uninitialized
        else trm_prim ~loc (Prim_new tt)
      | Some e ->
        begin match e.desc with
        | InitList el -> (* {e1,e2,e3} *)(* Array(struct intstantiation) declaration  with initialization *)
          let tl = List.map translate_expr el in
          (* TODO: instead of matching on typ_desc, you need to match on
             [get_typ_kind ctx tt] where is defined in ast.ml

             let rec get_typ_kind ctx (ty : typ) : typ_kind =
               match ty.typ_desc with
               | Typ_const ty1 -> get_typ_kind ty1
               | (Typ_int | Type_float) -> Typ_kind_basic ty
               | (Typ_ptr | Type_array _) -> Typ_kind_array
               | Typ_fun -> Typ_kind_fun
               | Typ_var -> Typ_kind_var
               | Typ_constr (_,tyid,_) ->
                   match String_map.find tyid ctx.ctx_tconstr with
                   | Typedef_alias ty1 -> get_typ_kind ty1
                   | Typdef_prod -> Typ_kind_prod
                   | Typdef_sum -> Typ_kind_sum

             type typ_kind =
               | Typ_kind_array
               | Typ_kind_sum
               | Typ_kind_prod
               | Typ_kind_basic of typ_desc
               | Typ_kind_fun
               | Typ_kind_var

          TODO
            ..
            show_ctx tg =
              apply_to_target (fun t ->
                prints on stdout (ctx_to_text t.trm_ctx))
          *)
          begin match tt.typ_desc with
          | Typ_array _ -> trm_array ~loc ~typ:(Some tt) tl
          (* TODO: Check this one later *)
          | Typ_constr _ -> trm_struct ~loc ~typ:(Some tt) tl
          | Typ_var _ -> (* assumption: typedefs are only for struct*)
            trm_struct ~loc ~typ:(Some tt) tl
          | _ ->
            fail loc ("translate_decl: initialisation lists only " ^ "allowed for struct and array")
          end
        | _ -> translate_expr e
        end
      end
      in
    ctx_var_add n tt;
    if const then
      trm_let ~loc ~is_statement:true Var_immutable (n,tt) te
    else
      begin
        add_var n;
        begin match eo with
        | None ->
          trm_let ~loc  Var_mutable (n,typ_ptr ~typ_attributes:[GeneratedStar] tt) te
        | Some _ ->
          trm_let ~loc Var_mutable (n,typ_ptr ~typ_attributes:[GeneratedStar] tt) (trm_apps (trm_prim ~loc (Prim_new tt)) [te])
        end
      end
  | TypedefDecl {name = tn; underlying_type = q} ->
    let tid = next_typid () in
    ctx_tconstr_add tn tid;
    let tq = translate_qual_type ~loc q in
    let td = {
      typdef_typid = tid;
      typdef_tconstr = tn;
      typdef_vars = [];
      typdef_body = Typdef_alias tq
    }
    in
    ctx_typedef_add tn tid td;
    trm_typedef ~loc ~ctx td
  | TypeAlias {ident_ref = id; qual_type = q} ->
    begin match id.name with
      | IdentifierName tn ->
        let tid = next_typid () in
        ctx_tconstr_add tn tid;
        let tq = translate_qual_type ~loc q in
        let td = {
          typdef_typid = tid;
          typdef_tconstr = tn;
          typdef_vars = [];
          typdef_body = Typdef_alias tq;} in
        ctx_typedef_add tn tid td;
        trm_typedef ~loc ~ctx td
      | _ -> fail loc "translate_decl: only identifiers allowed for type aliases"
    end
  | RecordDecl _ ->
    fail loc "translate_decl: record declarations should not happen here"
  | _ -> fail loc "translate_decl: not implemented"

module Include_map = Map.Make(String)
type 'a imap = 'a Include_map.t

let filter_out_include (filename : string)
    (dl : decl list) : ((decl list) imap) * (decl list) =
  let rec aux (include_map : (decl list) imap) (dl : decl list) =
    match dl with
    | [] -> (include_map, [])
    | d :: dl ->
      let (include_map, file_decls) = aux include_map dl in
      let file = file_of_node d in
      (* keep only project included files *)
      if Filename.dirname file <> Filename.dirname filename
      then (include_map, file_decls)
      else
      if file <> filename
      then
        (Include_map.update (Filename.basename file)
           (fun dlo ->
              match dlo with
              | None -> Some [d]
              | Some dl -> Some (d :: dl)
           )
           include_map,
         file_decls

        )
      else (include_map, d :: file_decls)
  in
  aux (Include_map.empty) dl

(* For debugging: set this variable to true for obtaining a file where to
   see the clang AST that gets passed as input to the conversion to our AST. *)
let dump_clang_ast = false

let dump_clang_file = "clang_ast.ml"

let translate_ast (t : translation_unit) : trm =
  let {decoration = _; desc = {filename = filename; items = dl}} = t in
  print_info None "translate_ast: translating %s's AST...\n" filename;
  let (include_map, file_decls) = filter_out_include filename dl in
  if dump_clang_ast then begin
    let out_ast = open_out dump_clang_file in
    Include_map.iter
      (fun h dl ->
         Printf.fprintf out_ast "(* Include %s: *)\n" h;
         List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d))
           dl
      )
      include_map;
    Printf.fprintf out_ast "(* Main file: *)\n";
    List.iter (fun d -> Printf.fprintf out_ast "%s\n" (Clang.Decl.show d))
      file_decls;
    close_out out_ast;
  end;
  let loc = loc_of_node t in
  (* open a scope for global heap allocated variables *)
  compute_scope Other_scope
    (fun () ->
       let tinclude_map =
         Include_map.mapi
           (fun h dl ->
              trm_seq ~annot:(Some (Include h)) (translate_decl_list dl))
           include_map
       in
       let t =
         trm_seq ~loc ~annot:(Some Main_file) (translate_decl_list file_decls)
       in
       trm_seq ~annot:(Some No_braces)
         ((Include_map.fold (fun _ t tl -> t :: tl) tinclude_map []) ++ [t])
    )
