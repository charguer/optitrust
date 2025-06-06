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

(**

# Translation of variables

During translation from OCaml to OptiTrust, variables are mapped to
the type var using "name_to_var" which uses "unset_var_id" as var id.
After the translation, we use Scope_computation.infer_var_ids for
replacing all these variables with appropriate IDs: one fresh id is
generated for every binder, and this id is assigned to all occurrences
of the variable bound by this binder.

# Translation of let-bindings and blocks

To translate let-in from OCaml, we need to collapse sequences. Consider:
[[
   let x1 = u1 in
   let x2 = u2 in
   u3; (* same semantics as [let _ = u3] *)
   let x4 = u4 in
   u5 (* assumed not to be a let *)
]]
What we view at first is [let x1 = u1 in k'] (encoded in OCaml as a Texp_let)
First we translate [u1] in [t1]. Then we translate [k'] as [t']. Then two cases.
If [t'] is [Trm_seq instrs], then we return [Trm_seq (Trm_let(x,u1) :: instrs)].
Otherwise, [Trm_seq (Trm_let(x,u1) :: t' :: nil)].

Likewise, if we have a [u1; u2] (encoded in OCaml as Texp_seq),
First we translate [u1] in [t1]. Then we translate [u2] in [t2].
If [t2] is a [Trm_seq args], we return [Trm_seq (t1::args)].
Otherwise, we return [Trm_seq [t1;t2]].

# Translation of function calls

To translate functions, recall that OCaml uses curried syntax; but we want
to view this as n-ary function applications. We impose that the OCaml programmer
does not use partial or over-applications implicitly. Concretely, if we view
syntactically [f x y z] in Ocaml (encoded as
[Texp_app(Texp_app(Texp_app(Texp_var "f",Texp_var "x"),Texp_var "y"),Texp_var "z")])
then we encode it as [f(x,y,z)] in OptiTrust syntax,
that is, [Trm_apps(Trm_var "f", [Trm_var "x"; Trm_var "y"; Trm_var "z"])].
Therefore, to encode OCaml's, [Texp_app (u1, u2)], we first translate [u1] as
[t1] and [u2] as [t2]. If [t1] is a [Trm_app(t0,targs)], then we return
[Trm_app(t0,targs@[t2])]. Otherwise, we return [Trm_app(t1,t2)].

# Translation of function definitions

Likewise for function definitions, we treat [let f x y = t] in OCaml
(which is a Texp_function applied to Texp_function in OCaml typedtree) as
a single Trm_fun in OptiTrust.

*)

type ocaml_ast = Typedtree.implementation

let var_counter : int ref = ref 0
let fresh_res_variable () =
  let var_name = "__res" ^ (Int.to_string !var_counter) in
  incr var_counter;
  var_name

(**[binds_seq t]: adds a bound variable at the end of the sequence to satisfy the ast invariants. Should only be called after [trm_seq_enforce] *)
let binds_seq (t : trm) : trm =
  match trm_seq_inv t with
  | Some (tl, _) ->
    let tail =
      begin match Mlist.last tl with
        | Some tail -> tail
        | None -> failwith "Ocaml_to_ast.binds_seq : the given argument is an empty sequence of elements"
      end
    in
    let tl = Mlist.pop_back tl in
    let result = name_to_var (fresh_res_variable ()) in
    let tail' = trm_let (result, typ_auto) tail in
    let tl = Mlist.push_back tail' tl in(*
    let tl = Mlist.push_back (trm_var result) tl in *)
    trm_seq ~result tl (*TODO: check this together*)
  | None -> failwith "Ocaml_to_ast.binds_seq : the given arguments is not a sequence"


let rec tr_constant (c : constant) : trm = match c with
  | Const_int n -> trm_int n
  | Const_char c -> trm_string (Char.escaped c)
  | Const_string (s, _, _) -> trm_string s
  | Const_float s -> trm_float (float_of_string s)
  | Const_int32 n -> trm_int (Int32.to_int n)
  | Const_int64 n -> trm_int (Int64.to_int n)
  | Const_nativeint n -> trm_int (Nativeint.to_int n)

and tr_pattern (bind_toplevel : bool) pat = match pat.pat_desc with
  | Tpat_var (id, _) ->
    if bind_toplevel then
      (toplevel_var (Ident.name id), typ_auto)
    else
      (name_to_var (Ident.name id), typ_auto)
  | Tpat_any -> failwith "any"
  (*| Tpat_constraint (p, _) -> tr_pattern p*)
  | _ -> failwith "pattern not yet translatable"

and tr_value_binding (bind_toplevel : bool) (vb : value_binding) =
  let {vb_pat; vb_expr} = vb in
  trm_let (tr_pattern bind_toplevel vb_pat) (tr_expression vb_expr)

and tr_let_exp (vb_l : value_binding list) (e : expression) : trm =
  let body = tr_expression e in
  let binding_list = List.map (tr_value_binding false) vb_l in
  let full_list = (*TODO: change this code to match on the expression and not on the translated term. Just like what is done in the [tr_function] function's code*)
    (match body.desc with
    | Trm_seq (l, _) -> binding_list@(Mlist.to_list l)
    | _ -> binding_list@[body]) in

  trm_seq (Mlist.of_list full_list)

and tr_apply (e : expression) (l : ('a * (expression option)) list) : trm =
  let t = tr_expression e in
  let args = (List.map (fun x -> let (_,e) = x in (match e with | Some e -> tr_expression e | _ -> failwith "None")) l) in
  (* Flatten curried applications as explained at the top of the file *)
  let body = (match t.desc with (*TODO: change this code to match on the expression and not on the translated term. Just like what is done in the [tr_function] function's code*)
    | Trm_apps (t1, t2, _) -> trm_apps t1 (t2@args)
    | _ -> trm_apps t args ) in

  body

and tr_sequence (u1 : expression) (u2 : expression) : trm =
  let t1 = tr_expression u1 in
  let t2 = tr_expression u2 in

  let body1 = (*line could be removed if we wanted to keep blocks? Not so sure about this*)
    (match t1.desc with
    | Trm_seq (args, _) -> Mlist.to_list args
    | _ -> [t1]) in

  let body2 =
    (match t2.desc with
    | Trm_seq (args, _) -> Mlist.to_list args
    | _ -> [t2]) in

    trm_seq (Mlist.of_list (body1@body2))

(**[tr_pat_switch] translates an OCaml pattern into a [pat]. This is intended to be used while generating switch statements*)
and tr_pat_switch (p : pattern) : trm =
  match p.pat_desc with
  | Tpat_var (id, _) -> trm_pat_var (name_to_var (Ident.name id))
  | Tpat_any -> trm_pat_any ()
  | _ -> failwith "Pattern not handled when translating matches"

and tr_computation_pattern (p : computation general_pattern) : trm =
  match p.pat_desc with
  | Tpat_value v -> (match (v :> (value general_pattern)).pat_desc with (*small hack I found on the internet, not sure how good this is but at least it compiles*)
                    | Tpat_construct (_, cd, pats, _) ->
                    let constr = trm_var (inversor_var cd.cstr_name) in
                    (*can I use typed_vars to create terms? *)
                    let args = List.map tr_pat_switch pats in
                    trm_apps constr args
                    | _ -> failwith "Did not expect this pattern shape inside a match")
  | _ -> failwith "Did not expect this pattern shape in a match"

(**[tr_case] translates an OCaml [computation case] from a pattern matching into an Optitrust switch case. The [trm] argument represents the argument of the OCaml match*)
and tr_case (t : trm) (c_case : computation case) : bbtrm * trm =
  (*TODO: handle the [when] case, as [c_guard] in c_case*)
  let {c_lhs; c_guard; c_rhs} = c_case in
  let lhs =
    match c_guard with
    | Some e -> trm_bbe_and (trm_pat_is t (tr_computation_pattern c_lhs)) (tr_expression e)
    | None -> trm_pat_is t (tr_computation_pattern c_lhs)
  in
  let rhs = tr_expression c_rhs in

  (lhs, rhs)

(** [tr_function u] : expects a Texp_function *)
and tr_function (u : expression) : trm =
  let rec aux (acc_tvars : typed_vars) (u : expression) : trm =
    match u.exp_desc with
    | Texp_function {cases} -> begin
      match cases with (*add a sequence around the body for everything*)
        | [{c_lhs; c_rhs}] ->
          let tvar = tr_pattern false c_lhs in
          aux (tvar::acc_tvars) c_rhs
        | _ -> failwith "unsupported function binding pattern"
      end
    | _ ->
      assert (acc_tvars <> []);
      let body = trm_seq_enforce (tr_expression u) in
      let body' = binds_seq body in
      trm_fun (List.rev acc_tvars) typ_auto body'
  in
  aux [] u

and recognize_builtin_function (s : string) : trm option = (*TODO: handle all the cases*)
  match s with
  | "+" -> Some (trm_binop typ_int Binop_add)
  | _ -> None

and tr_expression (u : expression) : trm =
  let aux = tr_expression in
  match u.exp_desc with
  | Texp_ident (_, l, _) ->
    begin match l.txt with
    | Lident s ->
      begin match recognize_builtin_function s with
      | Some t -> t
      | None -> trm_var (name_to_var s)
    end
    | Ldot _ -> failwith "Ldot"
    | Lapply _ -> failwith "Lapply"
    end
  | Texp_constant c -> tr_constant c
  | Texp_let (_, vb_l, e) -> tr_let_exp vb_l e
  | Texp_apply (e, l) ->
      tr_apply e l

  | Texp_function {cases} ->
    tr_function u
  | Texp_match (u, cases, _) ->
    let t = aux u in
    (*List.map of the cases. Maybe add u as argument to tell it what to put on the left. It takes the computational cases. Look at what the ocaml ast looks like in case*)
    let translated_cases = List.map (tr_case t) cases in

    trm_my_switch translated_cases


  | Texp_construct (_, cd, args) ->
    let constr = trm_var (name_to_var cd.cstr_name) in
    let args = List.map tr_expression args in
    trm_apps constr args
  | Texp_ifthenelse (u1, u2, u3) ->
    (match u3 with
    | None -> failwith "else clause not present in if-then-else"
    | Some e ->
          let t1 = aux u1 in
          let t2 = trm_seq_enforce (aux u2) in
          let t2' = binds_seq t2 in
          let t3 = trm_seq_enforce (aux e) in
          let t3' = binds_seq t3 in
          trm_if t1 t2' t3')
  | Texp_sequence (u1, u2) ->
    tr_sequence u1 u2
  | _ -> failwith "expression not yet translatable"

and tr_core_type (ct : core_type) : typ =
  match ct.ctyp_desc with
  | Ttyp_constr (_, ident, _) ->
    let name = (match ident.txt with | Lident l -> l |__ -> failwith "tr_core_type: Identifier not handled") in
    let body = if name = "int" then typ_int else typ_var (name_to_typvar name) in
    body
    (* typ_int *)
    (* TODO: arrow *)
  | _ -> failwith "Core type not handled"

(*
 let rec init_aux (i:int) (f:int->int) : li = body
 *)

and inversor_var (name : string) : var =
  name_to_var ("Pattern__" ^ name)

and inversor_toplevel_var (name : string) : var =
  toplevel_var ("Pattern__" ^ name)

and tr_constructor_decl (cd : constructor_declaration) : union_constructor =
  let arguments = (match cd.cd_args with
                  | Cstr_tuple ctl -> ctl
                  | _ -> failwith "Argument type not handled") in
  (* Printf.printf "trying to add %s to the toplevels\n" cd.cd_name.txt;
  let constr_var = toplevel_var cd.cd_name.txt in
  Printf.printf "checking if %s : %d was added to the toplevels\n" constr_var.name constr_var.id;
  let _ = Hashtbl.find user_readable_id_for_toplevel_vars constr_var.id in *)

  { union_constructor_constructor = (* constr_var *) toplevel_var cd.cd_name.txt;
    union_constructor_inversor = inversor_toplevel_var cd.cd_name.txt;
    union_constructor_args_type = List.map tr_core_type arguments }

and tr_let (vb_l : value_binding list) : trm = (*also change this part to handle seq flattening*)
  let binding_list = List.map (tr_value_binding true) vb_l in
  trm_seq (Mlist.of_list binding_list)

and tr_type (tl : type_declaration list) : typ =
  let td = (match tl with
  | [singleton] -> singleton
  | _ -> failwith "Type declaration not handled: too many arguments") in
  let typedef_name = name_to_typvar (td.typ_name.txt) in (*should it be a typvar or a var ? This is not a "type variable" in the same sense as a polymorphic type, is this a problem? *)

  let _type_params = td.typ_params in (*ignore it for now. we will try to handle polymorphism without looking at this? Where will the variable be defined then? *)
  let type_kind = td.typ_kind in

  (*we want that constructors have type "union_constructor list"*)
  let constructors : union_constructor list =
    match type_kind with
    | Ttype_variant cdl -> List.map (tr_constructor_decl) cdl
    | _ -> failwith "Union constructor not handled"
    in

  let typedef_body = Typedef_union constructors in

  trm_typedef {typedef_name; typedef_body}


let tr_structure_desc (s : structure_item) : trm (* list of trm instead TODO *) = match s.str_desc with
  | Tstr_eval (e, _) -> tr_expression e   (* let _ = .. *)
  | Tstr_value (_, vb_l) -> tr_let vb_l   (* let f = .. *)
  | Tstr_type (_rec_flag, tl) -> tr_type tl  (* type t = .. *)
  (* TODO : handle cases where we can have a list of definiions, such as : ```let f = .. and g = ..    [.. ; ..]```
  *)
  | _ -> failwith "structure not yet translatable"

let tr_structure_list l =
  let str_list = List.map (tr_structure_desc) l in
  List.fold_left
    (fun acc t -> match trm_seq_inv t with
      | Some (tl, _) -> acc @ (Mlist.to_list tl)
      | None -> acc @ [t])
    [] str_list

(** [tr_ast t]: transalate [t] into OptiTrust AST *)
let tr_ast (t : ocaml_ast) : trm =
  (* TODO List.flatten ... *)
  (*
  print_string "Printing the ast : \n";

  Pprintast.structure Format.std_formatter t;

  print_string "End _printing. \n";
  *)
  (trm_seq (Mlist.of_list (tr_structure_list t.structure.str_items)))
