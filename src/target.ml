(* TODO: remove unnecessary open *)
open Ast
open Tools
open Path

include Constr

type path = Path.path
type paths = path list

type case_dir = Path.case_dir

(* NOW INCLUDED
type constr = Constr.constr
type target = constr list
*)

(* Shorthands for Trace functions *)
include Trace


(******************************************************************************)
(*                        Smart constructors for targets                      *)
(******************************************************************************)


(*
  a smart constructor builds a target
  thus, the user provides a target using them
  this list is then flattened to call resolve_target
  unit args are used because of optional arguments
  *)

(* Logic constraints *)

let cStrict : constr = 
  Constr_strict

let cTrue : constr =
  Constr_bool true

let cFalse : constr =
  Constr_bool false

let cChain (cstrs : constr list) : constr =
  Constr_chain cstrs

(* Relative targets *)

let cBefore : constr =
  Constr_relative TargetBefore

let cAfter : constr =
  Constr_relative TargetAfter

let cFirst : constr =
  Constr_relative TargetFirst

let cLast : constr =
  Constr_relative TargetLast

(* Used for checking the number of targets to match *)

let cMulti : constr =
  Constr_occurences ExpectedMulti

let cAnyNb : constr =
    Constr_occurences ExpectedAnyNb

let cNb (nb : int) : constr =
    Constr_occurences (ExpectedNb nb)

(* directions *)
let cRoot : constr =
    Constr_root

let cNth (n : int) : constr =
    Constr_dir (Dir_nth n)

let cCond : constr =
    Constr_dir Dir_cond

let cThen : constr =
    Constr_dir Dir_then

let cElse : constr =
    Constr_dir Dir_else

let cBody : constr =
    Constr_dir Dir_body

let cInit : constr =
    Constr_dir Dir_for_init

let cStep : constr =
    Constr_dir Dir_for_step

let cCallFun : constr = (* LATER: see if this is needed (cCallNotBuiltin) *)
    Constr_dir Dir_app_fun

let cArg (n : int) : constr =
    Constr_dir (Dir_arg n)

let cName : constr =
    Constr_dir Dir_name

let cDirCase (n : int) (cd : case_dir) : constr =
    Constr_dir (Dir_case (n, cd))

let cCaseName (n : int) : case_dir = Case_name n

let cCaseBody : case_dir = Case_body

let cEnumConst (n : int)
  (ecd : enum_const_dir) : constr =
    Constr_dir (Dir_enum_const (n, ecd))

let cEnumConstName : enum_const_dir = Enum_const_name

let cEnumConstVal : enum_const_dir = Enum_const_val

let cInclude (s : string) : constr =
    Constr_include s

let string_to_rexp (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp =
    { rexp_desc = s;
      rexp_exp = (if regexp then Str.regexp else Str.regexp_string) s;
      rexp_substr = substr;
      rexp_trm_kind = trmKind; }

  let string_to_rexp_opt (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp option =
    let res =
      if s = ""
        then None
        else Some (string_to_rexp regexp substr s trmKind)
      in
    res
  (* Matching by string *)
  let cInstrOrExpr ?(substr : bool = false) (tk : trm_kind) (s : string) : constr =
    Constr_regexp (string_to_rexp false substr s  tk)

  let cInstr ?(substr : bool = true) (s : string) : constr =
    cInstrOrExpr ~substr TrmKind_Instr s

  let cExpr ?(substr : bool = true) (s : string)  : constr =
    cInstrOrExpr ~substr TrmKind_Expr s

  let cInstrOrExprRegexp (tk : trm_kind) (substr : bool) (s : string) : constr =
    Constr_regexp (string_to_rexp true substr s tk)

  let cInstrRegexp ?(substr : bool = false) (s : string) : constr =
    cInstrOrExprRegexp TrmKind_Instr substr s

  let cExprRegexp ?(substr : bool = false) (s : string) : constr =
    cInstrOrExprRegexp TrmKind_Expr substr s

let cVarDef
  ?(regexp : bool = false) ?(substr : bool = false) ?(body : target = []) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  let p_body =  body in
    Constr_decl_var (ro, p_body)

let cFor ?(init : target = [])
  ?(cond : target = []) ?(step : target = []) ?(body : target = []) (name : string) : constr =
  let init =
      match name, init with
      | "", [] -> init
      | "", _ -> init
      | _, [] -> [cVarDef name]
      | _, _::_ -> init
      in
    Constr_for ( init,  cond,  step,  body)

let cWhile ?(cond : target = [])
  ?(body : target = []) (_ : unit) : constr =
  let p_cond = cond in
  let p_body = body in
    Constr_while (p_cond, p_body)

let cIf ?(cond : target = [])
  ?(then_ : target = []) ?(else_ : target = []) (_ : unit) : constr =
  let p_cond = cond in
  let p_then = then_ in
  let p_else = else_ in
    Constr_if (p_cond, p_then, p_else)

(* Converts a list of constraints into a [target_list_pred] *)
let target_list_simpl (cstrs : constr list) : target_list_pred =
  let n = List.length cstrs in
  make_target_list_pred
    (fun i -> if i < n then List.nth cstrs i else cFalse)
    (fun bs -> List.length bs = n && list_all_true bs)
    (fun () -> "target_list_simpl(" ^ (list_to_string (List.map constr_to_string cstrs) ^ ")"))

(* Converts a constraint into a [target_list_pred] that checks that at least one of the items in the list satisfies the given constraint *)
let target_list_one_st (cstr : constr) : target_list_pred =
  make_target_list_pred
    (fun _i -> cstr)
    (fun bs -> List.mem true bs)
    (fun () -> "target_list_one_st(" ^ (constr_to_string cstr) ^ ")")

(* Predicate that matches any list of arguments *)
let target_list_pred_always_true : target_list_pred =
  make_target_list_pred
    (fun _i -> cTrue)
    list_all_true
    (fun () -> "target_list_pred_always_true")

(* by default an empty name is no name *)
let cFunDef ?(args : target = []) ?(args_pred : target_list_pred = target_list_pred_always_true) ?(body : target = []) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp false name TrmKind_Expr in
  (* LATER: maybe an error if both args and args_pred are provided *)
  let p_args = match args with
    | [] -> args_pred
    | _ -> target_list_simpl args
    in
  Constr_decl_fun (ro, p_args, body)

(* toplevel fun declaration *)
let cTopFun
  ?(args : target = []) ?(args_pred : target_list_pred = target_list_pred_always_true)
  ?(body : target = []) (name : string) : constr =
  cChain [ cRoot; cFunDef ~args ~args_pred ~body name ]

let cTypDef
  ?(substr : bool = false) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  Constr_decl_type ro

let cEnum ?(name : string = "")
  ?(substr : bool = false) ?(constants : (string * (target)) list = [])
  ?(regexp : bool = false) (_ : unit) : constr =
  let c_n = string_to_rexp_opt regexp substr name TrmKind_Expr in
  let cec_o =
    match constants with
    | [] -> None
    | _ ->
        let cec =
          List.map
            (fun (n, pl) -> (string_to_rexp_opt regexp substr n TrmKind_Expr, pl))
            constants
        in
        Some cec
  in
  Constr_decl_enum (c_n, cec_o)

let cSeq ?(args : target = []) ?(args_pred:target_list_pred = target_list_pred_always_true) (_ : unit) : constr =
  let p_args =
  match args with
  | [] -> args_pred
  | _ -> (target_list_simpl args)
  in
  Constr_seq  p_args

(* LATER:probably don't need exact *)
let cVar ?(substr : bool = false) ?(regexp : bool = false) (name : string) : constr =
  (* LATER: ~only_instr:false might not work in other languages *)
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  Constr_var ro

let cBool (b : bool) : constr =
    Constr_lit (Lit_bool b)

let cInt (n : int) : constr =
    Constr_lit (Lit_int n)

let cDouble (f : float) : constr =
    Constr_lit (Lit_double f)

let cString (s : string) : constr =
    Constr_lit (Lit_string s)

(* let cPrim (p : prim) : constr =
    cStr (ast_to_string (trm_prim p)) *)

let cFun ?(fun_  : target = []) ?(args : target = []) ?(args_pred:target_list_pred = target_list_pred_always_true) (name:string) : constr =
  let exception Argument_Error of string in
  let p_fun =
  match name, fun_ with
  | "",_ -> fun_
  | _, [] -> [cVar name]
  | _,_ -> raise (Argument_Error "Can't provide both the path and the name of the function")

  in
  let args =
  match args with
  | [] -> args_pred
  | _ -> (target_list_simpl args)
  in
  Constr_app (p_fun,args,false)

let cDef (name : string) : constr =
  Constr_chain [cStrict;cFunDef name]

(* TODO: think about this *)
let cCall ?(fun_  : target = []) ?(args : target = []) ?(args_pred:target_list_pred = target_list_pred_always_true) ?(accept_encoded : bool = false) (name:string) : constr =
  let exception Argument_Error of string in
  let p_fun =
    match name, fun_ with
    | "",_ -> fun_
    | _, [] -> [cVar name]
    | _,_ -> raise (Argument_Error "Can't provide both the path and the name of the function")
    in
  let args =
    match args with
    | [] -> args_pred
    | _ -> (target_list_simpl args)
    in
  Constr_app (p_fun, args, accept_encoded)

let cLabel ?(substr : bool = false) ?(body : target = []) ?(regexp : bool = false) (label : string) : constr =
  let ro = string_to_rexp_opt regexp substr label TrmKind_Expr in
  let p_body = body in
  Constr_label (ro, p_body)

let cGoto ?(label : string = "")
  ?(substr : bool = false) ?(regexp : bool = false) (_ : unit) : constr =
  let ro = string_to_rexp_opt regexp substr label TrmKind_Expr in
  Constr_goto ro

let cReturn_target ?(res : target = [])
  (_ : unit) : constr =
  let p_res =  res in
  Constr_return p_res

let cAbrtAny : abort_kind = Any

let cAbrtRet : abort_kind = Return

let cAbrtBrk : abort_kind = Break

let cAbrtCtn : abort_kind = Continue

let cAbort ?(kind : abort_kind = Any)
  (_ : unit) : constr =
  Constr_abort kind

let cReturn : constr =
  Constr_abort (cAbrtRet)

let cBreak : constr =
  Constr_abort (cAbrtBrk)

let cContinue : constr =
  Constr_abort (cAbrtCtn)
(*
  the empty list is interpreted as no constraint on the accesses
  accesses are reversed so that users give constraints on what they see
  *)
let cAccesses ?(base : target = [])
  ?(accesses : constr_access list = []) (_ : unit) : constr =
  let p_base =  base in
  let accesses =
    match accesses with | [] -> None | cal -> Some (List.rev cal)
  in
    Constr_access (p_base, accesses)

let cIndex ?(index : target = []) (_ : unit) : constr_access =
  let p_index =  index in
  Array_access p_index

let cField ?(field : string = "") ?(substr : bool = false) ?(regexp : bool = false)
  (_ : unit) : constr_access =
  let ro = string_to_rexp_opt regexp substr field TrmKind_Expr in
  Struct_access ro

let cAccess : constr_access = Any_access

(* the empty list is interpreted as no constraint on the cases *)
let cSwitch ?(cond : target = [])
  ?(cases : (case_kind * (target)) list = []) (_ : unit) : constr =
  let p_cond =  cond in
  let c_cases =
    match cases with
    | [] -> None
    | _ -> Some (List.map (fun (k, pl) -> (k,  pl)) cases)
  in
    Constr_switch (p_cond, c_cases)

let cCase ?(value : target = []) (_ : unit) : case_kind =
  match value with
  | [] -> Case_any
  | _ -> Case_val ( value)

let cDefault : case_kind = Case_default

let cSet ?(lhs : target = []) ?(rhs : target = []) (_ : unit) : target =
  [
    cCall ~args:lhs "";
    cCall ~args:rhs "";
    cCall ~fun_:[cStrict;cInstr "="] ""
  ]





(******************************************************************************)
(*                          Target resolution                                 *)
(******************************************************************************)

(* NOW INCLUDED
let resolve_target = Constr.resolve_target
let resolve_target_between = Constr.resolve_target_between
*)

(*
  find the explicit path to the toplevel declaration of x if it exists
  assumption: x denotes a function or a type
  todo: generalise to other terms
 *)
let rec target_to_decl (x : var) (t : trm) : path option =
  match t.desc with
  | Trm_let_fun (f, _, _, _) when f = x -> Some []
  | Trm_typedef td when td.typdef_tconstr = x -> Some []
  | Trm_seq tl ->
     foldi
       (fun i dlo t' ->
         match dlo with
         | Some _ -> dlo
         | _ ->
            begin match target_to_decl x t' with
            | Some dl -> Some (Dir_nth i :: dl)
            | _ -> None
            end
       )
       None
       tl
  (* val, var, array, struct, if, apps, while, for, switch, abort, label *)
  | _ -> None



(******************************************************************************)
(*                          Apply on target operations                        *)
(******************************************************************************)

(* Type of transformations *)
module Transfo = struct
  type t = target -> unit
  type local = trm -> path -> trm
  type local_between = int -> local
end


let apply_on_path = Path.apply_on_path


(* [applyi_on_target ~replace tr tg]: Apply a specific Generic over a target or a list of targets, keep track over the index of the target
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
let applyi_on_target (tr : int -> trm -> path -> trm) (tg : target) : unit =
  Trace.apply (fun _ t ->
    let ps = resolve_target tg t in
    Tools.foldi (fun i t dl -> tr i t dl) t ps)

(* [apply_on_target ~replace tr tg]: Esentially the same as applyi_on_target, but without keeping track over the index of the target
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
let apply_on_target (tr : trm -> path -> trm) (tg : target) : unit =
  applyi_on_target (fun _i t dl -> tr t dl) tg

  
(* [apply_on_target_between ~replace_top tr tg]: Similar to apply_on_target, but the function considers the index too
      params:
        tr : transformation to be applied
        tg : target
      return:
        unit
*)
let applyi_on_target_between (tr : int -> trm -> (path*int)  -> trm) (tg : target) : unit =
  Trace.apply (fun _ t ->
    let ps = resolve_target_between tg t in
    Tools.foldi (fun i t (pk:path*int) -> tr i t pk) t ps)

let apply_on_target_between (tr : trm -> (path*int) -> trm) (tg : target) : unit =
  applyi_on_target_between (fun _i pk t -> tr pk t) tg

(* [apply_on_transformed_targets ~replace_top transformer tr tg]:
   Same as [apply_to_transformed_targets] except that there is some processing performed on each of the explicit path.
   This processing is done by the [transformer] function, which takes an explicit path, and returns some information
   that the transformation can take as input.
    params:
      transformer: ..
      tr: transformation to be applied
      tg: target
    return:
      unit
*)
let apply_on_transformed_targets (transformer : path -> 'a) (tr : 'a -> trm -> trm) (tg : target) : unit =
  Trace.apply (fun _ t ->
    let ps = resolve_target tg t in
    let descrs = List.map transformer ps in
    List.fold_left (fun t descr -> tr descr t) t descrs)


(******************************************************************************)
(*                                   Show                                     *)
(******************************************************************************)

(* [target_show_aux id t]: adds an annotation [trm_decoration]
   carrying the information [id] around the term t.
   If the flag [debug_ast] is set, the ast is printed on [stdout].
   --LATER: remove debug-ast in the future? *)
let target_show_aux (debug_ast : bool) (id : int) (t : trm) : trm =
    if debug_ast then
      Ast_to_text.print_ast ~only_desc:true stdout t;
    trm_decoration (Tools.left_decoration id) (Tools.right_decoration id) t

(* [target_show_transfo id t p]: adds an annotation [trm_decoration]
   carrying the information [id] around the term at path [p] in the term [t]. *)
let target_show_transfo (debug_ast : bool) (id : int): Transfo.local =
  apply_on_path (target_show_aux debug_ast id)

(* [target_between_show_aux id k t]: adds a decorated semi-column with identifier [id]
   at position [k] in the sequence described by the term [t]. *)
let target_between_show_aux (debug_ast : bool) (id : int) (k : int) (t : trm) : trm =
    if debug_ast then
      Ast_to_text.print_ast ~only_desc:true stdout t;
    match t.desc with
    | Trm_seq tl ->
      let lfront, lback = Tools.split_list_at k tl in
      let new_trm = trm_decoration (Tools.left_decoration id) (Tools.right_decoration id) (trm_var ";") in
      trm_seq ~annot:t.annot (lfront @ [new_trm] @ lback)
    | _ -> fail t.loc "target_between_show_aux: expected the surrounding sequence"

(* [target_between_show_transfo id k t p]: adds a decorated semi-column with identifier [id]
   at position [k] in the sequence at path [p] in the term [t]. *)
let target_between_show_transfo (debug_ast : bool) (id : int) : Transfo.local_between =
  fun (k:int) -> apply_on_path (target_between_show_aux debug_ast id k)

(* [show ~line:int tg] is a transformation for visualizing targets.
   The operation only executes if the command line argument [-exit-line]
   matches the [line] argument provided to the function. Otherwise, it is a noop. *)
let show ?(line : int = -1) ?(debug_ast : bool = false) (tg : target) : unit =
  only_interactive_step line (fun () ->
    if Constr.is_target_between tg then begin
      applyi_on_target_between (fun i  t (p,k) ->
        target_between_show_transfo debug_ast i k t p) tg
    end else begin
      applyi_on_target (fun i t p ->
        target_show_transfo debug_ast i t p) tg
    end)
