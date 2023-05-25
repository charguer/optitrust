open Ppxlib

let par_var_inv (pat : pattern) : string =
  match pat.ppat_desc with
  | Ppat_var loc -> loc.txt
  | _ -> failwith "JKSLDKJQSd"

let typ_to_string (typ : core_type) : string =
  match typ.ptyp_desc with
  | Ptyp_constr (loc, []) -> String.concat "_" (Ocaml_common.Longident.flatten loc.txt)
  | _ -> failwith "QLEMQLZKELM"

let deal_with_binding (loc : Location.t) (binding : value_binding) =
  let (module B) = Ast_builder.make loc in
  let name = par_var_inv binding.pvb_pat in
  (* binding.pvb_expr : fun arg1 -> .. -> fun argN -> body *)
  let rec dive_into_args expr args =
    match expr.pexp_desc with
    | Pexp_fun (lbl, _exp0, pat, expr) ->
      let label = match lbl with
      | Nolabel -> ""
      | Labelled l -> "~" ^ l
      | Optional l -> "?" ^ l
      in
      let (name, typ) = match pat.ppat_desc with
      | Ppat_constraint (pat, typ) -> (par_var_inv pat, typ)
      | _ -> failwith "JKSLDKJQSd"
      in
      (* arg = Debug_transfo.arg_typ label name; *)
      let arg = B.pexp_apply (B.evar ("Debug_transfo.arg_" ^ (typ_to_string typ))) [(Nolabel, (B.estring label)); (Nolabel, (B.evar name))] in
      dive_into_args expr (arg :: args)
    | _ ->
      let instrs = List.rev (expr :: args) in
      (*
      new_body = Debug_transfo.scope "name" (fun () ->
        args;
        body
      ) *)
      let continuation = B.pexp_fun Nolabel None B.punit (B.esequence instrs) in
      let new_body = B.pexp_apply (B.evar ("Debug_transfo.scope")) [(Nolabel, (B.estring name)); (Nolabel, continuation)] in
      { binding with pvb_expr = new_body }
  in
  dive_into_args binding.pvb_expr []

let expand ~ctxt expr _ =
  match expr.pexp_desc with
  | Pexp_let (recflag, bindings, next) ->
    let bindings = List.map (deal_with_binding expr.pexp_loc) bindings in
    {expr with pexp_desc = Pexp_let (recflag, bindings, next)}
  | _ ->
    failwith "SKLJQSD"

  (*
let ast_pattern =
  let open Ast_pattern in
  pstr
    (pstr_value nonrecursive
      (value_binding ~pat:__ ~expr:__)
    ^:: nil)
    *)

let my_extension =
  Extension.V3.declare "transfo"
    Extension.Context.expression
    Ast_pattern.(pstr ((pstr_eval __ __) ^:: nil))
    (* Ast_pattern.__ *)
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[rule] "transfo"