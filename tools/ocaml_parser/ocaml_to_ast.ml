open Optitrust_utils
open Optitrust_ast
open Ast
open Trm
open Typ
open Contextualized_error
open Mark
open Tools
open Flags
open Typedtree
open Asttypes

(** [tr_ast t]: transalate [t] into OptiTrust AST *)

type ocaml_ast = Typedtree.implementation

let rec add_end x l =
  match l with
  | [] -> [x]
  | a::l' -> a::(add_end x l')

let rec tr_constant (c : constant) : trm = match c with
  | Const_int n -> trm_int n
  | Const_char c -> trm_string (Char.escaped c)
  | Const_string (s, _, _) -> trm_string s
  | Const_float s -> trm_float (float_of_string s)
  | Const_int32 n -> trm_int (Int32.to_int n)
  | Const_int64 n -> trm_int (Int64.to_int n)
  | Const_nativeint n -> trm_int (Nativeint.to_int n)

and tr_pattern pat = match pat.pat_desc with
  | Tpat_var (id, _) -> (new_var (Ident.name id), typ_auto)
  | Tpat_any -> failwith "any"
  (*| Tpat_constraint (p, _) -> tr_pattern p*)
  | _ -> failwith "pattern not yet translatable"

and tr_value_binding (vb : value_binding) = let {vb_pat; vb_expr} = vb in trm_let (tr_pattern vb_pat) (tr_expression vb_expr)

and tr_let_exp (vb_l : value_binding list) (e : expression) : trm =
  let binding_list = List.map tr_value_binding vb_l in
  let full_list = add_end (tr_expression e) binding_list in
  trm_seq (Mlist.of_list full_list)


and tr_expression (e : expression) : trm =
  let aux = tr_expression in
  match e.exp_desc with
  | Texp_ident (_, l, _) -> (match l.txt with
                     | Lident s -> trm_var (new_var s)
                     | Ldot _ -> failwith "Ldot"
                     | Lapply _ -> failwith "Lapply")
  | Texp_constant c -> tr_constant c
  | Texp_let (_, vb_l, e) -> tr_let_exp vb_l e
  | Texp_apply (e, l) -> trm_apps (aux e)
             (List.map (fun x -> let (_,e) = x in (match e with | Some e -> aux e | _ -> failwith "None")) l)
  (*| Texp_function (lbl, exp0, pat, e) -> (match lbl with
                                | Nolabel -> (match exp0 with
                                              | None -> trm_fun [(tr_pattern pat)] typ_auto (aux e)
                                              | _ -> failwith "   ")
                                | _ -> failwith "  ") *)
  | Texp_match _ -> failwith "match not yet translatable"
  | Texp_construct _ -> failwith "construct not yet translatable"
  | _ -> failwith "expression not yet translatable"

and tr_core_type (ct : core_type) : typ =
  match ct.ctyp_desc with
  | Ttyp_constr (_, ident, _) ->
    let _name = (match ident.txt with | Lident l -> l |__ -> failwith "nyi") in
    typ_int
  | _ -> failwith "nyi"


(*faire fold ? Du produit de tout ca. *)

and tr_constructor_decl (cd : constructor_declaration) : record_member =
  let arguments = (match cd.cd_args with
                  | Cstr_tuple ctl -> ctl
                  | _ -> failwith "nyi") in
  Record_field (cd.cd_name.txt, typ_tuple (List.map tr_core_type arguments))

and tr_let (vb_l : value_binding list) : trm =
  let binding_list = List.map tr_value_binding vb_l in
  trm_seq (Mlist.of_list binding_list)

and tr_type (tl : type_declaration list) : typ =
  let td = (match tl with
  | [singleton] -> singleton
  | _ -> failwith "nyi") in
  let typedef_name = new_var (td.typ_name.txt) in

  let _type_params = td.typ_params in
  let type_kind = td.typ_kind in


  let constructors = (match type_kind with
                  | Ttype_variant cdl -> List.map (fun cd -> (tr_constructor_decl cd, Access_public)) cdl
                  | _ -> failwith "nyi") in

  let typedef_body = Typedef_record constructors in

  trm_typedef {typedef_name; typedef_body}


let tr_structure_desc (s : structure_item) : trm = match s.str_desc with
  | Tstr_eval (e, _) -> tr_expression e
  | Tstr_value (_, vb_l) -> tr_let vb_l
  | Tstr_type (_rec_flag, tl) -> tr_type tl
  | _ -> failwith "structure not yet translatable"

let tr_structure_list l = List.map (tr_structure_desc) l

let tr_ast (t : ocaml_ast) : trm =

  (*
  print_string "Printing the ast : \n";

  Pprintast.structure Format.std_formatter t;

  print_string "End _printing. \n";
  *)

 (trm_seq (Mlist.of_list (tr_structure_list t.structure.str_items)))






  (* List.map (some function) t*)

(*depuis foo.ml Run.script_cpp
parser foo.cpp // plus tard renommé en foo_in.cpp

depuis foo.ml Run.script_ml
parser foo_in.ml

*)

(*Show.let trm_internal t

*)

  (*apprendre comment lire l'ast t*)

  (*ctx_reset ();*)
  (* Initialize id_counter *)
  (*let {decoration = _; desc = {filename = filename; items = dl}} = t in
  verbose_info "tr_ast: translating %s's AST..." filename;
  let (include_map, file_decls) = filter_out_include filename dl in
  begin match !dump_clang_ast with
  | None -> ()
  | Some dump_clang_file ->
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
  end;*)
  (*give a default value _a trm*)

(*open Asttypes
open Parsetree
open Var
open Ast
open Ast_aux


(*
Documentation _parsetree in:
https://v2.ocaml.org/releases/4.11/htmlman/compilerlibref/Parsetree.html *)

type ocaml_ast = Parsetree.structure

(*#########################################################################*)
(* ** Errors *)

let unsupported ?(loc=loc_none) (m : string) : 'a =
  failwith (Ast_print.print_loc loc ^ ", unsupported: " ^ m)

(*#########################################################################*)
(* ** Translate expressions *)

let check_uniq ?loc fs : unit =
  let rec aux = function
  | [] | [_] -> ()
  | f1 :: f2 :: _ when f1 = f2 ->
    unsupported ?loc (Printf.sprintf "Field %s defined several times." f1)
  | _ :: fs -> aux fs in
  aux (List.sort compare fs)


let tr_longident (lid : Longident.t) : string =
  String.concat "." (Longident.flatten lid)

let tr_constant (c : constant) : cst =
  match c with
  | Pconst_integer (s, _) -> Cst_int (int_of_string s)
  | Pconst_float (s, _) -> Cst_float (float_of_string s)
  | Pconst_string (s, _, _) -> Cst_string s
  | Pconst_char _ -> unsupported "char"

let annotated_var_of_var (x : var) : annotated_var =
   (x, mk_styp_annot_none())

(* Translate a pattern in a place in which we only accept variables. *)
let tr_pat_to_var (p : pattern) : annotated_var =
  match p.ppat_desc with
  | Ppat_var var_loc ->
      annotated_var_of_var (var var_loc.txt)
  | Ppat_constraint ({ppat_desc = Ppat_var var_loc; _}, cty) ->
    (var var_loc.txt, mk_styp_annot cty)
  | Ppat_constraint ({ppat_desc = Ppat_any; _}, cty) ->
    (var "_", mk_styp_annot cty)
  | Ppat_any -> annotated_var_of_var (var "_")
  | Ppat_construct ({txt = Lident "()"; _}, _) ->
      let cty = mk_styp_annot_unit () in
      cty.styp_annot_typ <- Some the_typ_unit ;
      (var "()", cty)
  | _ ->
      unsupported ~loc:p.ppat_loc
        "We only accept variables here: use an explicit match-with construct for more complex patterns."

let tr_pat (p : pattern) : pat =
  let module VSet = Set.Make (struct type t = var let compare = compare end) in
  let add_strict x s =
    if VSet.mem x s then
      unsupported ~loc:p.ppat_loc
        (Printf.sprintf "The variable %s can't appear several times in this pattern." (print_var x))
    else VSet.add x s in
  let merge_strict varss =
    List.fold_left (fun s vars ->
      if VSet.disjoint s vars then VSet.union s vars
      else unsupported ~loc:p.ppat_loc "Variables can't appear several times in this pattern.") VSet.empty varss in
  (* The set _variables appearing in the pattern is also returned. *)
  let rec aux p : pat * VSet.t =
    let (pat_desc, vars) =
      match p.ppat_desc with
      | Ppat_any -> (Pat_any, VSet.empty)
      | Ppat_var var_loc ->
          let x = var var_loc.txt in
          (Pat_var x, VSet.singleton x)
      | Ppat_alias (p, var_loc) ->
          let x = var var_loc.txt in
          let (p, vars) = aux p in
          (Pat_alias (p, x), add_strict x vars)
      | Ppat_constant c -> (Pat_constant (tr_constant c), VSet.empty)
      | Ppat_tuple ps ->
          let (ps, vars) = List.split (List.map aux ps) in
          let vars = merge_strict vars in
          (Pat_tuple ps, vars)
      | Ppat_construct ({ txt = Lident "true" }, None) ->
          (Pat_constant (Cst_bool true), VSet.empty)
      | Ppat_construct ({ txt = Lident "false" }, None) ->
          (Pat_constant (Cst_bool false), VSet.empty)
      | Ppat_construct (c, None) ->
          (Pat_construct (constr (tr_longident c.txt), []), VSet.empty)
      | Ppat_construct (c, Some (tys, p)) ->
          assert (tys = []) ; (* I'm not sure when this is triggered. I guess only with GADTs. *)
          let (p, vars) = aux p in
          (Pat_construct (constr (tr_longident c.txt), [p]), vars)
      | Ppat_or (p1, p2) ->
          let (p1, vars1) = aux p1 in
          let (p2, vars2) = aux p2 in
          if VSet.equal vars1 vars2 then (Pat_or (p1, p2), vars1)
          else
            unsupported ~loc:p.ppat_loc "The same variables should appear on both sides _the pattern."
      | Ppat_constraint (p, ty) ->
          let (p, vars) = aux p in
          (Pat_constraint (p, mk_styp_annot ty), vars)
      | _ -> unsupported ~loc:p.ppat_loc "Unsupported pattern." in
    ({ pat_desc ; pat_loc = p.ppat_loc ; pat_typ = None }, vars) in
  fst (aux p)

(* unsupported
|	Ppat_interval _constant * constant
|	Ppat_variant _Asttypes.label * pattern option
|	Ppat_record _(Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
|	Ppat_array _pattern list
|	Ppat_type _Longident.t Asttypes.loc
|	Ppat_lazy _pattern
|	Ppat_unpack _string option Asttypes.loc
|	Ppat_exception _pattern
|	Ppat_extension _extension
|	Ppat_open _Longident.t Asttypes.loc * pattern
*)

let rec pexp_fun_get_annotated_args_body (e : expression) : (annotated_vars * expression) option =
  match e.pexp_desc with
  | Pexp_fun (Nolabel, None, p, e1) ->
      (* Here x and xs are annotated variables. *)
      let x = tr_pat_to_var p in
      begin match pexp_fun_get_annotated_args_body e1 with
      | None -> Some ([x], e1)
      | Some (xs, ei) -> Some (x::xs, ei)
      end
  | Pexp_fun (lbl, eo, p, e1) -> unsupported ~loc:e.pexp_loc "labeled arguments"
  | _ -> None

let ocaml_to_mode (e : expression) =
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident b; _ }, _) ->
      if b = "true" then Mode_in
      else if b = "false" then Mode_out
      else invalid_arg "modes must be booleans litterals."
  | _ -> invalid_arg "modes must be booleans."

let rec ocaml_list_to_modes (e : expression) : mode list =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "[]"; _ }, _) -> []
  | Pexp_construct ({ txt = Lident "::"; _ }, Some {pexp_desc = Pexp_tuple [e1; e2]; _ }) ->
      ocaml_to_mode e1 :: ocaml_list_to_modes e2
  | _ -> invalid_arg "the input must be a boolean list."

(* Parse the expression provided to an __instance to extract the list _overloading assumptions
  and the final expression.
  As this function can also be called on non-instance terms, it can't directly return exceptions.
  Instead we use a sum type with an exception: either we return the list _valid assumption or
  we return an exception (without throwing it) as first output. *)
let extract_assumptions =
  let rec aux acc e =
    let add a =
      match acc with
      | Either.Left acc -> Either.Left (a :: acc)
      | Either.Right ex -> Either.Right ex in
    let invalid ex =
      match acc with
      | Either.Left acc -> Either.Right (Invalid_argument ex)
      | Either.Right ex0 -> Either.Right ex0 in
    match e.pexp_desc with
    | Pexp_newtype (tn, e) -> aux (add (Asmpt_type (tconstr tn.txt))) e
    | Pexp_fun (Nolabel, None, p, e) ->
      begin
        match p.ppat_desc with
        | Ppat_constraint ({ ppat_desc = Ppat_var x ; _ }, t) ->
          aux (add (Asmpt_instance ((var x.txt, mk_styp_annot t), None))) e
        | Ppat_constraint ({ ppat_desc = Ppat_alias ({ ppat_desc = Ppat_var x ; _ }, y) ; _ }, t) ->
          aux (add (Asmpt_instance ((var x.txt, mk_styp_annot t), Some (var y.txt)))) e
        | _ -> aux (invalid "__instance assumption can't be an arbitrary pattern: it must be _the form (id : ty) or (id as id' : ty).") e
      end
    | Pexp_fun (_lbl, _e0, _p, e) -> aux (invalid "__instance assumption can't be labelled") e
    | _ -> (Either.map_left List.rev acc, e) in
  aux (Either.Left [])

let rec tr_exp_desc (e : expression) : trm_desc =
  match e.pexp_desc with
  | Pexp_ident lid_loc -> trm_desc_var (var (tr_longident lid_loc.txt))
  | Pexp_constant c -> trm_desc_cst (tr_constant c)
  | Pexp_let (rf, [vb], e2) ->
      let (x,t1) = tr_let vb in
      let t2 = tr_exp e2 in
      trm_desc_let rf x t1 t2

  | _ -> unsupported ~loc:e.pexp_loc "tr_exp_desc"

and tr_exp (e : expression) : trm =
  { trm_desc = tr_exp_desc e;
    trm_loc = e.pexp_loc;
    trm_typ = None;
    trm_env = None }

(* TODO
  let get_labeled_arg (name : string) (lblexps : (label * exp) list) : option exp
    List.assoc_opt

  let label_input = get_labeled_arg ...
  let inputs_opt = Option.map ocaml_list_to_modes label_input in
  trm_overload_new inputs_opt
*)

(* The let bindings can hide __overload or __instance declarations.
  In such cases, they might take assumptions as argument. *)
and tr_let (vb : value_binding) : annotated_var * trm =
  let t1 =
    (* The assumptions below are only valid if e_instance is indeed an __instance. *)
    let (assumptions, e_instance) = extract_assumptions vb.pvb_expr in
    (* In the cases in which e_instance is indeed an __instance, this function gets
      the assumptions. *)
    let get_assumptions () =
      match assumptions with
      | Either.Left assumptions -> assumptions
      | Either.Right ex -> raise ex in
    match e_instance.pexp_desc with
    | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "__instance"; _}; _}, aes) ->
        begin
          match List.rev aes with
          | [(Nolabel, e)] ->
            trm_overload_add ~loc:vb.pvb_loc (get_assumptions ()) (tr_exp e)
          | [(_lbl, _e)] -> invalid_arg "__instance argument can not be labelled."
          | _ -> invalid_arg "__instance needs one and only one argument."
        end
    | Pexp_constraint({pexp_desc = Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "__instance"; _}; _}, aes); _}, cty) ->
        begin
          match aes with
          | [(Nolabel, e)] ->
            trm_overload_add ~loc:vb.pvb_loc (get_assumptions ()) (trm_annot (tr_exp e) (mk_styp_annot cty))
          | [(_lbl, _e)] -> invalid_arg "__instance argument can not be labelled."
          | _ -> invalid_arg "__instance needs one and only one argument."
        end
    | _ ->
      (* We are not in an __instance: we can ignore the assumptions and revert to the
        original expression. *)
      match vb.pvb_expr.pexp_desc with
      | Pexp_ident { txt = Lident "__overload"; _ } -> trm_overload_new None
      | Pexp_apply ({pexp_desc = Pexp_ident {txt = Lident "__overload"; _}; _}, aes) ->
        let rec get_input_output input output = function
          | [] ->
            begin match input, output with
            | None, None -> trm_overload_new None
            | Some inputs, None -> trm_overload_new (Some (inputs, Mode_in))
            | Some inputs, Some output -> trm_overload_new (Some (inputs, output))
            | None, Some _ -> invalid_arg "An input mode list is expected if an output mode is provided."
            end
          | (Labelled "input", e) :: aes ->
            let input =
              match input with
              | None -> Some (ocaml_list_to_modes e)
              | Some _ -> invalid_arg "Multiple inputs provided for __overload." in
            get_input_output input output aes
          | (Labelled "output", e) :: aes ->
            let output =
              match output with
              | None -> Some (ocaml_to_mode e)
              | Some _ -> invalid_arg "Multiple inputs provided for __overload." in
            get_input_output input output aes
          | _ -> invalid_arg "overload argument must be ~input or ~output." in
        get_input_output None None aes
      | _ -> tr_exp vb.pvb_expr
    in
  (tr_value_binding vb, t1)

and tr_value_binding (vb : value_binding) : annotated_var =
  tr_pat_to_var vb.pvb_pat

and tr_structure (s : structure) : topdefs =
  List.map tr_structure_item s

and tr_structure_item (si : structure_item) : topdef =
  let attrs =
      match si.pstr_desc with
      | Pstr_value (rf, [vb]) ->
          let attrs = vb.pvb_attributes in
          let _docstrings =
            List.filter (function
              | { attr_name = {txt = ("ocaml.doc" | "ocaml.text"); _ };
                 attr_payload = PStr [ { pstr_desc =
                    Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string(body, _loc, None)); _  }, []) ; _ }] } ->
                    (* FIXME: Were these documentation strings meant to be reused? *)
                    true
              | _ -> false) attrs in
          attrs
      | _ -> []
    in
  { topdef_desc = tr_structure_item_desc si.pstr_desc;
    topdef_loc = si.pstr_loc;
    topdef_attrs = attrs }

and tr_structure_item_desc (sid : structure_item_desc) : topdef_desc =
  match sid with
  | Pstr_value (rf, [vb]) ->
      let (x, t1) = tr_let vb in
      topdef_desc_val rf x t1
  | Pstr_value (rf, []) -> assert false
  | Pstr_value (rf, vb :: vbs) -> unsupported ~loc:vb.pvb_loc "mutually recursive definitions"
  | Pstr_primitive vd ->
      topdef_desc_external (var vd.pval_name.txt) vd.pval_type vd.pval_prim
  | Pstr_type (rf, tds) -> topdef_desc_typ_def rf tds
  | Pstr_typext e -> unsupported ~loc:e.ptyext_loc "type extension"
  | Pstr_exception e -> unsupported ~loc:e.ptyexn_loc "exception"
  | Pstr_extension ((l, _), _) -> unsupported ~loc:l.loc "extension"
  | Pstr_eval (e, _) -> unsupported ~loc:e.pexp_loc "eval"
  | Pstr_attribute a -> unsupported ~loc:a.attr_loc "attribute"
  | _ -> unsupported "tr_structure_item_desc"



(*

Later:
  pour récupérer les listes de la forme [x1;x2...;xn],
  il faut récupérer du parsetree des objets construits avec :

  - Pexp_construct(... (Lident "::") ..., Some args)
  - (Lident "[]")

  et faire l'inversion jusqu'au nil.

*)

*)
