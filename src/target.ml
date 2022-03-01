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

include AstParser


(******************************************************************************)
(*                        Smart constructors for targets                      *)
(******************************************************************************)


(*
  a smart constructor builds a target
  thus, the user provides a target using them
  this list is then flattened to call resolve_target
  unit args are used because of optional arguments
  *)

(******************************************************************************)
(*                             Logic constraints                              *)
(******************************************************************************)

let cTrue : constr =
  Constr_bool true

let cFalse : constr =
  Constr_bool false

let cStrictNew : constr =
  Constr_depth (DepthAt 0)

let cStrict : constr =
  Constr_depth (DepthAt 1)

let cInDepth : constr =
  Constr_depth DepthAny

let cChain (cstrs : constr list) : constr =
  Constr_target cstrs

(******************************************************************************)
(*                             Relative targets                               *)
(******************************************************************************)

let tBefore : constr =
  Constr_relative TargetBefore

let tAfter : constr =
  Constr_relative TargetAfter

let tFirst : constr =
  Constr_relative TargetFirst

let tLast : constr =
  Constr_relative TargetLast

(******************************************************************************)
(*                            Number of targets                               *)
(******************************************************************************)

let nbMulti : constr =
  Constr_occurrences ExpectedMulti

let nbAny : constr =
    Constr_occurrences ExpectedAnyNb

let nbExact (nb : int) : constr =
    Constr_occurrences (ExpectedNb nb)

let tIndices ?(nb : int = -1) (indices : int list) : constr =
  let expected_nb = match nb with
    | -1 -> None
    | _ -> Some nb in
  Constr_occurrences (ExpectedSelected (expected_nb, indices)  )

let occIndex ?(nb : int = -1) (index : int) : constr =
  tIndices ~nb [index]

let occFirst : constr =
  Constr_occurrences FirstOcc

let occLast : constr =
  Constr_occurrences LastOcc

(******************************************************************************)
(*                                Directions                                  *)
(******************************************************************************)

let target_of_path (p : path) : target =
  List.map (fun d -> Constr_dir d) p

let dRoot : constr =
    Constr_root

let dArrayNth (n : int) : constr =
    Constr_dir (Dir_array_nth n)

let dSeqNth (n : int) : constr =
    Constr_dir (Dir_seq_nth n)

let dStructNth (n : int) : constr =
    Constr_dir (Dir_struct_nth n)


let dCond : constr =
    Constr_dir Dir_cond

let dThen : constr =
    Constr_dir Dir_then

let dElse : constr =
    Constr_dir Dir_else

let dBody : constr =
  Constr_dir Dir_body

let dForInit : constr =
    Constr_dir Dir_for_c_init

let dStep : constr =
    Constr_dir Dir_for_c_step

let dName : constr =
    Constr_dir Dir_name

let dDirCase (n : int) (cd : case_dir) : constr =
    Constr_dir (Dir_case (n, cd))

let dCaseName (n : int) : case_dir = Case_name n

let dCaseBody : case_dir = Case_body

let dEnumConst (n : int)
  (ecd : enum_const_dir) : constr =
    Constr_dir (Dir_enum_const (n, ecd))

let dEnumConstName : enum_const_dir = Enum_const_name

let dEnumConstVal : enum_const_dir = Enum_const_val

let dArg (n : int) : constr =
  Constr_dir (Dir_arg_nth n)


(* [string_to_rexp regexp substr s trmKind]  transforms a string into a regular expression
    used to match ast nodes based on their code representation.
    [string_to_rexp] - denotes a flag to tell if the string entered is a regular epxression or no
    [substr] - denotes a flag to decide if we should target strings that contain string [s] as a strict substring
    [trmKind] - denotes the kind of the ast note represented in code by string [s]
*)
let string_to_rexp (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp =
    { rexp_desc = (if regexp then "Regexp" else "String") ^ "(\"" ^ s ^ "\")";
      rexp_exp =
        (let pat1 = if regexp then s else Str.quote s in
        let pat2 = if substr then pat1 else ("^" ^ pat1 ^ "$") in
        Str.regexp pat2);
      rexp_substr = substr; (* LATER: is there a smart way to avoid rexp_substr in addition to the regexp? *)
      rexp_trm_kind = trmKind; }

  let string_to_rexp_opt (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp option =
    let res =
      if s = ""
        then None
        else Some (string_to_rexp regexp substr s trmKind)
      in
    res

(******************************************************************************)
(*                             String matching                                *)
(******************************************************************************)

let sInstrOrExpr ?(substr : bool = false) (tk : trm_kind) (s : string) : constr =
  Constr_regexp (string_to_rexp false substr s  tk)

let sInstr ?(substr : bool = true) (s : string) : constr =
  sInstrOrExpr ~substr TrmKind_Instr s

let sExpr ?(substr : bool = true) (s : string)  : constr =
  sInstrOrExpr ~substr TrmKind_Expr s

let sInstrOrExprRegexp (tk : trm_kind) (substr : bool) (s : string) : constr =
  Constr_regexp (string_to_rexp true substr s tk)

let sInstrRegexp ?(substr : bool = true) (s : string) : constr =
  sInstrOrExprRegexp TrmKind_Instr substr s

let sExprRegexp ?(substr : bool = true) (s : string) : constr =
  sInstrOrExprRegexp TrmKind_Expr substr s


(******************************************************************************)
(*                                Ast nodes                                   *)
(******************************************************************************)

let cInclude (s : string) : constr =
    Constr_include s

let cOr (tgl : target list) : constr =
  Constr_or tgl

let cAnd (tgl : target list) : constr =
  Constr_and tgl

let cDiff (tgl1 : target list) (tgl2 : target list) : constr =
  Constr_diff (tgl1, tgl2)

let typ_constraint_default : typ_constraint =
  (fun _ -> true)

let make_typ_constraint ?(typ:string="") ?(typ_pred : typ_constraint = typ_constraint_default) () : typ_constraint =
  if typ <> "" && typ_pred != typ_constraint_default
    then fail None "make_typ_constraint: cannot provide both ~typ and ~typ_pred.";
  if typ = ""
    then typ_pred
    else (fun (ty : typ) -> typ = (Ast_to_c.typ_to_string ty))

let cHasTypePred (pred : typ -> bool) : constr =
  Constr_hastype pred

let cHasTypeAst (ty : typ) : constr =
  let pred = (fun (ty2 : typ) -> same_types ty ty2) in
  cHasTypePred (make_typ_constraint ~typ_pred:pred ())

let cHasType (typ : string) : constr =
  cHasTypePred (make_typ_constraint ~typ ())

let with_type ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default)  (tg : target) : target =
  if typ = "" && typ_pred == typ_constraint_default
    then tg
    else [cAnd [tg; [cStrictNew; Constr_hastype (make_typ_constraint ~typ ~typ_pred ())]]]

let cArgPred ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (pred : string -> bool) : constr =
  Constr_arg (pred, make_typ_constraint ~typ ~typ_pred ())

let cArg ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let pred = if (name = "") then (fun _ -> true) else (fun x -> x = name) in
  cArgPred ~typ ~typ_pred pred

let cVarDef
  ?(regexp : bool = false) ?(substr : bool = false) ?(body : target = []) ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Instr in
  let ty_pred = make_typ_constraint ~typ ~typ_pred () in
  Constr_decl_var (ty_pred, ro, body)

let cFor ?(start : target = []) ?(direction : loop_dir option) ?(stop : target = []) ?(step : target = []) ?(body : target = []) (index : string) : constr =
  let ro = string_to_rexp_opt false false index TrmKind_Instr in
  Constr_for (ro, start, direction, stop, step, body)


let cForNestedAtDepth (i:int) : constr =
  Constr_target (List.flatten (List.init i (fun _ -> [cStrict; cFor ""])))

let cFor_c ?(init : target = [])
  ?(cond : target = []) ?(step : target = []) ?(body : target = []) (index : string) : constr =
  let init =
      match index, init with
      | "", [] -> init
      | "", _ -> init
      | _, [] -> [cVarDef index]
      | _, _::_ -> init
      in
    Constr_for_c ( init,  cond,  step,  body)

let cWhile ?(cond : target = [])
  ?(body : target = []) (_ : unit) : constr =
  let p_cond = cond in
  let p_body = body in
    Constr_while (p_cond, p_body)

let cDoWhile ?(body : target = [])
  ?(cond : target = [])  (_ : unit) : constr =
  let p_body = body in
  let p_cond = cond in
    Constr_do_while (p_cond, p_body)

let cIf ?(cond : target = [])
  ?(then_ : target = []) ?(else_ : target = []) (_ : unit) : constr =
  let p_cond = cond in
  let p_then = then_ in
  let p_else = else_ in
    Constr_if (p_cond, p_then, p_else)

let cThen : constr =
 Constr_target [cIf(); dThen]

(* Converts a list of targets into a [target_list_pred] *)
let target_list_simpl (args : targets) : target_list_pred =
  let n = List.length args in
  make_target_list_pred
    (fun i -> if i < n then List.nth args i else [cStrict;cFalse])
    (fun bs -> List.length bs = n && list_all_true bs)
    (fun () -> "target_list_simpl(" ^ (list_to_string (List.map target_to_string args) ^ ")"))

(* NOTE: the "_st" suffix means that the argument is a constraint and not a target
   --we might revisit this convention later if we find it not suitable *)

(* Converts a target into a [target_list_pred] that checks that at least one of the items in the list satisfies the given constraint *)
let target_list_one_st (tg : target) : target_list_pred =
  make_target_list_pred
    (fun _i -> tg)
    (fun bs -> List.mem true bs)
    (fun () -> "target_list_one_st(" ^ (target_to_string tg) ^ ")")


(* Converts a target into a [target_list_pred] that checks that at least all the items in the list satisfies the given constraint *)
let target_list_all_st (tg : target) : target_list_pred = (* LATER: KEEP ONLY THIS *)
  make_target_list_pred
    (fun _i -> tg)
    (fun bs -> List.for_all (fun b -> b = true) bs)
    (fun () -> "target_list_all_st(" ^ (target_to_string tg) ^ ")")

(* Predicate that matches any list of arguments *)
let target_list_pred_default : target_list_pred =
  make_target_list_pred
    (fun _i -> [cTrue])
    list_all_true
    (fun () -> "target_list_pred_default")

(* [combine_args args args_pred] takes [args] as a [target_list_simpl] if it is nonempty,
   and else returns [args_pred]; raise an error if the two arguments have non-default values *)
let combine_args (args:targets) (args_pred:target_list_pred) : target_list_pred =
  match args with
  | [] -> args_pred
  | _ ->
      if args_pred != target_list_pred_default
        then fail None "cFunDef: can't provide both args and args_pred";
      target_list_simpl args


(* by default an empty name is no name *)
let cFunDef ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = []) ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp false name TrmKind_Expr in
  let ty_pred = make_typ_constraint ~typ:ret_typ ~typ_pred:ret_typ_pred () in
  Constr_decl_fun (ty_pred, ro, combine_args args args_pred, body)

(* toplevel fun declaration *)
let cTopFunDef
  ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default)
  ?(body : target = []) ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  cChain [ dRoot; cStrict; cFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred name ]

let cTypDef
  ?(substr : bool = false) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  Constr_decl_type ro

let cDef (name : string) : constr =
  cOr [[cFunDef name];[cVarDef name];[cTypDef name]]

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

let cSeq ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) (_ : unit) : constr =
  Constr_seq (combine_args args args_pred)

let cVar ?(regexp : bool = false) ?(substr : bool = false) ?(trmkind : trm_kind = TrmKind_Expr) ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name trmkind in
  let c = Constr_var ro in
  if typ = "" && typ_pred == typ_constraint_default then c else (* this line is just an optimization *)
  Constr_target (with_type ~typ ~typ_pred [c])

(* [cLitPred pred_l] matches all the literals that statisfy the predicate [pred_l] *)
let cLitPred (pred_l : lit -> bool) : constr =
  Constr_lit pred_l

(* [cLit ] matches all the literals *)
let cLit : constr =
  cLitPred (function _ -> true)


(* [cIntPred pred] matches all the integer literals that statisfy the predicate [pred] *)
let cIntPred (pred : int -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_int n -> pred n
   | _ -> false
   end )

(* [cInt n] matches all string literals equal to [n] *)
let cInt (n : int) : constr =
  cIntPred (function m -> m = n)

(* [cDoublePred pred] matches all the double literals that statisfy the predicate [pred]*)
let cDoublePred (pred : float -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_double d -> pred d
   | _ -> false
   end )

(* [cDouble d] matches all the doubles equal to [d] *)
let cDouble (d : float) : constr =
  cDoublePred (function d1 -> d1 = d)

(* [cBoolPred pred] matches all the boolean literals that satisfy the predicate [pred] *)
let cBoolPred (pred : bool -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_bool b -> pred b
   | _ -> false
   end )

(* [cBool b] matches all the booleans equal to [b] *)
let cBool (b : bool) : constr =
  cBoolPred (function b1 -> b1 = b)

(* [cStringPred pred] matches all the string literals that satisfy the predicate [pred] *)
let cStringPred (pred : string -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_string s -> pred s
   | _ -> false
   end )

(* [let cString s] matches all the string literals equal to [s] *)
let cString (s : string) : constr =
  cStringPred (function s1 -> s1 = s)

(* [cCall] can match all kind of function calls *)
let cCall ?(fun_  : target = []) ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) ?(accept_encoded : bool = false) ?(regexp : bool = false) (name:string) : constr =
  let exception Argument_Error of string in
  let p_fun = match fun_ with
  | [] -> [cVar ~regexp  name]
  | _ ->
    begin match name with
    | "" -> fun_
    | _ -> raise (Argument_Error "Can't provide both the path and the name of the function")
    end in
  Constr_app (p_fun, combine_args args args_pred, accept_encoded)

(* [cFun] matches a function by its name; it cannot match primitive functions *)
let cFun ?(fun_  : target = []) ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) ?(regexp : bool = false) (name:string) : constr =
  cCall ~fun_ ~args ~args_pred ~accept_encoded:false ~regexp name

(* [cPrimPred f_pred] matches all primitives which satisfy the predicated [f_pred]*)
let cPrimPred (f_pred : prim -> bool) : constr =
  Constr_prim f_pred

(* [cPrim] matches all [p] primitives *)
let cPrim (p : prim) : constr =
  cPrimPred (fun p2 -> p2 = p)

(* [cPrimPredFun ~args ~args_pred  p] matches only primitive function calls which satisfy the predicate [prim_pred]
    and the other constraints in [args] or [args_pred]
*)
let cPrimPredFun ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) (prim_pred:prim -> bool) : constr =
   cCall ~fun_:[cPrimPred prim_pred] ~args ~args_pred ~accept_encoded:true ""

(* [cPrimFun ~args ~args_pred  p] matches only primitive function calls with priimitive [p]*)
let cPrimFun ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) (p:prim) : constr =
   cPrimPredFun ~args ~args_pred (fun p2 -> p2 = p)

(* [cPrimFunArith] matches all the primitive arithmetic operations *)
let cPrimFunArith ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) () : constr =
  cPrimPredFun ~args ~args_pred (fun p2 -> (is_arith_fun p2))

(* [let cPrimNew ~arg ()] matches all the encode new primitive operations*)
let cPrimNew ?(arg : target = []) () : constr =
  cPrimPredFun ~args:[arg] (function Prim_new _ -> true | _ -> false)

(* [cInit ~arg ()] matches all the initialization values of variable declarations *)
let cInit ?(arg:target = []) () : constr =
  cChain ([ cPrimNew ~arg (); dArg 0 ])

(* [dInit] similar to cInit  but this one doesn't match on depth *)
let dInit : constr =
  cChain [cStrict; cInit ()]

(* [cWrite ~lhs ~rhs ()] matches write operations with left hand side [lhs] and right hand side [rhs], if right(left) hand side are
    left empty, then no contraint on the side of the set operation will be applied.
*)
let cWrite ?(lhs : target = [cTrue]) ?(rhs : target = []) ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (_ : unit) : constr =
  let lhs_typed = with_type ~typ ~typ_pred lhs in
  let rhs_typed = with_type ~typ ~typ_pred rhs in
  cPrimPredFun ~args:[lhs_typed; rhs_typed] (fun p -> match p with | Prim_binop Binop_set | Prim_compound_assgn_op _ -> true | _ -> false)

(* [cRead] matches all the get operations on mutable variables *)
let cRead ?(addr : target = [cTrue]) () : constr =
  cPrimFun ~args:[addr] (Prim_unop Unop_get)

(* [cReadOrWrite] *)
let cReadOrWrite ?(addr : target = [cTrue]) () : constr =
  cOr [[cWrite ~lhs:addr ()];[cRead ~addr ()]]

(* [cWriteVar x] matches a set operation for variable [x] *)
let cWriteVar (x : var) : constr =
  cWrite ~lhs:[cVar x] ()

(* [cReadVar x] matches a read operation for variable [x] *)
let cReadVar (x : var) : constr =
  cRead ~addr:[cVar x] ()

(* [cMark m] matches all the ast nodes with annotation Mark m*)
let cMark (m : mark) : constr =
  Constr_mark ((fun m1 -> m1 = m), "exactly:" ^ m)

(* [cMarks ms] matches all the ast nodes marked with Mark m when m is an element of ms *)
let cMarks (ms : mark list) : constr =
  Constr_mark ((fun m1 -> List.mem m1 ms), "one of:" ^ (Tools.list_to_string ms))

let cMarkSt (pred : mark -> bool) : constr =
  Constr_mark (pred, "such_that:" ^ "todo")

(* [cMarkAny] matches all the ast nodes marked with Mark m, where m can be any positive integer *)
let cMarkAny : constr =
  Constr_mark ((fun _ -> true), "any_mark")

(* [cLabel ~substr ~body ~regexp label] matches C labels*)
let cLabel ?(substr : bool = false) ?(body : target = []) ?(regexp : bool = false) (label : string) : constr =
  let ro = string_to_rexp_opt regexp substr label TrmKind_Expr in
  let p_body = body in
  Constr_label (ro, p_body)

(* [cLabelBdoy ~substr ~body ~regexp label] matches C label bodys*)
let cLabelBody ?(substr : bool = false) ?(body : target = []) ?(regexp : bool = false) (label : string) : constr =
  cChain [cLabel ~substr ~body ~regexp label; dBody]

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

(* [cAny] matches all the calls to function ANY *)
let cAny : constr =
  cFun "ANY"

(* [cChoose] matches all the calls to function CHOOSE *)
let cChoose : constr =
  cFun "CHOOSE"


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

let dLHS : constr =
  cChain [cWrite(); dArg 0]

let dRHS : constr =
  cChain [cWrite (); dArg 1]

let cTargetInDepth (tg : target) : constr =
  Constr_target (Constr_depth DepthAny :: tg)


(* [cAccesses ~base ~accesses ()] matches array_accesses or struct accesses
    depending on [accesses] parameter. [base] is a target on the base of an access
    and [accesses] is a list of constraints on accesses.
    Note:
      the empty list is interpreted as no constraint on the accesses
      accesses are reversed so that users give constraints on what they see
*)

let cAccesses ?(base : target = [])
  ?(accesses : constr_access list = []) ?(inner_accesses : bool = true)(_ : unit) : constr =
  let p_base =  base in
  let accesses =
    match accesses with | [] -> None | cal -> Some (List.rev cal)
  in
    Constr_access (p_base, accesses, inner_accesses)

(* [cIndex ~index ()] is an access constrint in index [index], because the
    index can be a variable or an integer it should be given as a target.
*)
let cIndex ?(index : target = []) (_ : unit) : constr_access =
  let p_index =  index in
  Array_access p_index

(* [cField ~field ~substr ~regexp ()] is an access constraint on field [field]
      since the field is a string this constructor allows to use more advance
      string matching like matching all substrings which contain [field] as a
      substring. Also one can constaint accesses on multiple fields by enabling
      regular expressions when setting [regexp] to true.
 *)
let cField ?(field : string = "") ?(substr : bool = false) ?(regexp : bool = false)
  (_ : unit) : constr_access =
  let ro = string_to_rexp_opt regexp substr field TrmKind_Expr in
  Struct_access ro

(* [cAccess] matches any access no matter if it is a struct access or an array access *)
let cAccess : constr_access =
  Any_access

(* [cFieldAccess field] field matches all struct accesses in field [field]*)
let cFieldAccess ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(field : field = "" )  () : constr =
 cAccesses ~base ~accesses:[cField ~field ~substr ~regexp ()] ()

(* [cFieldRead ~base ~substr ~regexp ~field ] matches all struct accesses at field [field] with base [base]
    which are at the base of a get operation
*)
let cFieldRead ?(field : field = "") ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false)  () : constr =
  cRead ~addr:[cFieldAccess ~base ~substr ~regexp ~field ()] ()

(* [cFieldWrite ~base field] matches all struct field write operations*)
let cFieldWrite ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(rhs : target = []) ?(field : field = "")  () : constr =
  let lhs = [cFieldAccess ~base ~substr ~regexp ~field ()] in
  cWrite ~lhs ~rhs ()

(* [cFieldReadOrWrite ~base ~substr ~regexp ~field] matches all read or write operations
*)
let cFieldReadOrWrite ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(field : field = "")  () : constr =
 cOr [[cFieldWrite ~base ~substr ~regexp ~field ()];[cFieldRead ~base ~substr ~regexp ~field ()] ]

(* [cCellAccess ~base ~index ] matches all array accesses at index [index] with base [base] *)
let cCellAccess ?(base : target = []) ?(index : target = [])  () : constr =
  cAccesses ~base ~accesses:[cIndex ~index ()] ()

(* [cCellRead ~base index] matches all array accesses at index [index] with base [base]
    which are under a get operation
*)
let cCellRead ?(base : target = []) ?(index : target = []) (): constr =
  cRead ~addr:[cCellAccess ~base ~index ()] ()

(* [cCellWrite ~base ~index ~arg] matches all array index write operations*)
let cCellWrite ?(base : target = []) ?(rhs:target = []) ?(index : target = []) (): constr =
  let lhs = [cCellAccess ~base ~index ()]  in
  cWrite ~lhs ~rhs ()

(* [cCellReadOrWrite ~base ~index ] matches all read or write operations on array cells with
  base [base] and index [index]
*)
let cCellReadOrWrite ?(base : target = []) ?(index : target = []) () : constr =
  cOr [[cCellRead ~base ~index ()];[cCellWrite ~base ~index ()]]


(* [cArrayInit] matches all array initialization lists *)
let cArrayInit : constr =
  Constr_array_init

(* [cStructInit] matches all struct initialization lists *)
let cStructInit : constr =
  Constr_struct_init

(* [cCell arary_size] matches all arrray cells in an array initialization *)
let cCell ?(cell_index : int option = None) (): constr =
  match cell_index with
  | None -> cChain [cArrayInit; cStrict; cTrue]
  | Some i -> cChain [cArrayInit; dArrayNth i]



(******************************************************************************)
(*                          Target resolution                                 *)
(******************************************************************************)

(* NOW INCLUDED
let resolve_target = Constr.resolve_target
let resolve_target_between = Constr.resolve_target_between
*)

(* [filter_constr_occurrence tg] *)
let filter_constr_occurrence (tg : target) : target =
  List.filter (function Constr_occurrences _ -> false | _ -> true ) tg

(* [enable_multi_targets tg]: matching multiple targets is not possible then enable it otherwise
    do nothing
 *)
let enable_multi_targets (tg : target) : target =
    if List.exists (function Constr_occurrences _ -> true | _ -> false) tg
      then tg
      else nbMulti::tg

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
let applyp_on_path = Path.applyp_on_path

let convert_stringreprs_from_documentation_to_string (m : AstC_to_c.stringreprs) : (stringreprid, string) Hashtbl.t =
  Tools.hashtbl_map_values (fun _id d -> Tools.document_to_string ~width:PPrint.infinity d) m

let compute_stringreprs ?(optitrust_syntax:bool=false) (f : trm->bool) (t : trm) : trm * AstC_to_c.stringreprs =
  let t2 = Ast.label_subterms_with_fresh_stringreprids f t in
  AstC_to_c.init_stringreprs();
  let t2_c_syntax = Ast_fromto_AstC.cfeatures_intro t2 in
  let _doc = AstC_to_c.ast_to_doc ~optitrust_syntax t2_c_syntax in (* fill in the [AstC_to_c.stringreprs] table, ignore the result *)
  let m = AstC_to_c.get_and_clear_stringreprs() in
  t2, m

(* Label subterms with fresh stringreprids, then build a table that maps stringreprids to the corresponding documents
   NOTE: this function will not work in the presence of multiple traces
   LATER: this function should proabably be moved/merged into the view_subterms function. *)
let compute_stringreprs_and_update_ast ?(optitrust_syntax:bool=false) (f : trm->bool) : AstC_to_c.stringreprs =
  let stringreprs = ref None in
  (* Note: through [Trace.apply], we modify the current AST by adding ids in the term annotation *)
  Trace.apply (fun t ->
    let t2, m = compute_stringreprs ~optitrust_syntax f t in
    stringreprs := Some m;
    t2
    );
  match !stringreprs with
  | Some m -> m
  | _ -> assert false (* table was set in Trace.apply *)


let debug_disappearing_mark = true
exception Interrupted_applyi_on_transformed_targets of trm

let fix_target (tg : target) : target =
  (* Occurrence constraints should be unique *)
  let check_occurrences = List.exists (function Constr_occurrences _ -> true | _ -> false) tg in
  (* If there are logic constraints then multiple occurrences are allowed *)
  let check_logic = List.exists (function Constr_or _ | Constr_and _ -> true | _ -> false) tg in
  if (not check_occurrences) && check_logic then nbMulti :: tg else tg

(* [with_stringreprs_available_for tgs t (fun t2 -> action)]  executes the [action]
   in a context where the AST [t] is viewed as [t2], which is a copy of [t] where
   certain nodes have their string representation available. Which nodes are concerned
   depend on the regexp constraints expressed in the targets [tgs]. *)
let with_stringreprs_available_for (tgs : target list) (t : trm) (f : trm -> 'a) : 'a =
  let kinds = Constr.get_target_regexp_kinds tgs in
  (* for debug  List.iter (fun k -> Printf.printf "(kind:%s)" (Constr.trm_kind_to_string k)) kinds;
      Printf.printf "==end of kinds==\n"; *)
  let t2, m = compute_stringreprs (Constr.match_regexp_trm_kinds kinds) t in
  (* FOR DEBUG: AstC_to_c.trm_print_debug t2;*)
  (* AstC_to_c.print_stringreprs m; for debug *)
  let stringreprs = convert_stringreprs_from_documentation_to_string m in
  Constr.stringreprs := Some stringreprs;
  (* for debug Constr.print_stringreprs();*)
  let r = f t2 in
  Constr.stringreprs := None;
  r

(* [resolve_target_with_stringreprs_available tg t] similar to resolve_target but this one computes
    first the string representation of all the ast nodes *)
let resolve_target_with_stringreprs_available (tg : target) (t : trm) : paths =
  with_stringreprs_available_for [tg] t (fun t2 -> resolve_target tg t2)

(* [resolve_target_exactly_one_with_stringreprs_available tg t] similar to resolve_target_exactly_one but this one computes
    first the string representation of all the ast nodes *)
let resolve_target_exactly_one_with_stringreprs_available (tg : target) (t : trm) : path =
  with_stringreprs_available_for [tg] t (fun t2 -> resolve_target_exactly_one tg t2)

(* [resolve_path_with_stringreprs_available p t] similar to resolve_path but this one computes first the string
    representation of all the ast nodes first *)
let resolve_path_with_stringreprs_available (p : path) (t : trm) :  trm =
  with_stringreprs_available_for [target_of_path p] t (fun t2 -> resolve_path p t2)

(* [applyi_on_transformed_targets transformer tr tg]: Apply a transformation [tr] on target [tg]
      params:
        transformer: change the resolved path so that more information about the context of the node is given
        tr: transformation to be applied at the nodes corresponedt to target [tg]
        tg: target
      return:
        unit
*)
let applyi_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : int -> trm -> 'a -> trm) (tg : target) : unit =
  let tg = fix_target tg in
  Trace.apply (fun t -> with_stringreprs_available_for [tg] t (fun t ->
      (* LATER: use apply_with_stringreprs
                           and take an optional list of auxiliary targets as argument *)
    let ps =
      Trace.timing ~cond:!Flags.analyse_time_details ~name:"resolve_targets" (fun () ->
          resolve_target tg t) in
    let ps = if rev then List.rev ps else ps in
    match ps with
    | [] -> t
    | [p] -> tr 0 t (transformer p)
    | _ ->
        let marks = List.map (fun _ -> Mark.next()) ps in
        (* add marks for occurences -- could be implemented in a single path, if optimization were needed *)
        (* Tools.printf "Before applyin_marks: %s\n" (AstC_to_c.ast_to_string t); *)
        let t =
            Trace.timing ~cond:!Flags.analyse_time_details ~name:"resolve_add_mark" (fun () ->
              List.fold_left2 (fun t p m -> apply_on_path (trm_add_mark m) t p) t ps marks) in
        (* Tools.printf "After applying_marks: %s\n" (AstC_to_c.ast_to_string t); *)
        (* iterate over these marks *)
        begin try
          Tools.fold_lefti (fun imark t m ->
            Trace.timing ~cond:!Flags.analyse_time_details ~name:(sprintf "process target %d" imark) (fun () ->
              match resolve_target [nbAny;cMark m] t with
              | [p] ->
                  let t = apply_on_path (trm_remove_mark m) t p in
                  tr imark t (transformer p)
              | ps ->
                  let msg =
                    if ps <> []
                      then "applyi_on_transformed_targets: a mark was duplicated"
                      else (*failwith*) (Tools.sprintf "applyi_on_transformed_targets: mark %s disappeared" m)
                    in
                  if debug_disappearing_mark
                    then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
                    else fail None msg
            )
          ) t marks
        with Interrupted_applyi_on_transformed_targets t -> t
        end
    ))

(* [apply_on_transformed_targets ~replace_top transformer tr tg]:
    Same as [applyi_to_transformed_targets] except that here the index of the resolved_path is not considered
    params:
      transformer: ..
      tr: transformation to be applied
      tg: target
    return:
      unit
*)
let apply_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : trm -> 'a -> trm) (tg : target) : unit =
  applyi_on_transformed_targets  ~rev transformer (fun _i t descr -> tr t descr) tg


(* [applyi_on_targets ~replace tr tg]:  A specialization of [applyi_on_transformed_targets] but here the transformer
      is the identity function.
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
(*  (tr : int * trm -> path -> trm) *)
let applyi_on_targets (tr : int -> trm -> path -> trm) (tg : target) : unit =
  applyi_on_transformed_targets (fun p -> p) tr tg



(* [apply_on_targets ~replace tr tg]: A specialization of [applyi_on_targets] but here the index of the resolved path is not
      taken into account.
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
let apply_on_targets (tr : trm -> path -> trm) (tg : target) : unit =
  applyi_on_targets (fun _i t dl -> tr t dl) tg



(* [iteri_on_transformed_targets] is similar to [applyi] except that it is meant to for
   transformations that are implemented in terms of other transformations with unit return type.
   LATER: try to better factorize the code.
   LATER: add timing measurements *)

let iteri_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : int -> trm -> 'a -> unit) (tg : target) : unit =
  let tg = fix_target tg in
  Trace.call (fun t -> with_stringreprs_available_for [tg] t (fun t ->
    let ps = resolve_target tg t
      (* ALTERNATIVE with_stringreprs_available_for tg t (fun t2 -> resolve_target tg t2) *) in
    let ps = if rev then List.rev ps else ps in
    let marks = List.map (fun _ -> Mark.next()) ps in
    let _t_before = t in
    (* add marks for occurences -- could be implemented in a single path, if optimization were needed *)
    let t = List.fold_left2 (fun t p m -> apply_on_path (trm_add_mark m) t p) t ps marks in
    Trace.set_ast t; (* Never use the function [set_ast] in another file! *)
    (* iterate over these marks *)
    try
      List.iteri (fun imark m ->
        let t = Trace.ast() in (* valid because inside the scope of [Trace.call] *)
        match resolve_target [nbAny;cMark m] t with
        | [p] ->
            (* Here we don't call [Marks.remove] to avoid a circular dependency issue *)
            let t = apply_on_path (trm_remove_mark m) t p in
            Trace.set_ast t; (* Never use the function [set_ast] in another file! *)
            tr imark t (transformer p)
        | ps ->
            let msg =
              if ps <> []
                then "iteri_on_transformed_targets: a mark was duplicated"
                else (Tools.sprintf "iteri_on_transformed_targets: mark %s disappeared" m)
              in
            if debug_disappearing_mark
              then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
              else fail None msg
      ) marks
    with Interrupted_applyi_on_transformed_targets t -> Trace.set_ast t (* view the ast when the bug appears *)
    ))

(* Variants *)

let iter_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : trm -> 'a -> unit) (tg : target) : unit =
  iteri_on_transformed_targets ~rev transformer (fun _i t descr -> tr t descr) tg

let iteri_on_targets ?(rev : bool = false) (tr : int -> trm -> path -> unit) (tg : target) : unit =
  iteri_on_transformed_targets~rev (fun p -> p) tr tg

let iter_on_targets ?(rev : bool = false) (tr : trm -> path -> unit) (tg : target) : unit =
  iteri_on_targets ~rev (fun _i t dl -> tr t dl) tg


(* [applyi_on_transformed_targets_between transformer tr tg]: Apply a transformation [tr] on a target relative to [tg]
      params:
        transformer: change the resolved path so that more information about the context of the node is given
        tr: transformation to be applied at the nodes corresponedt to target [tg]
        tg: target
      return:
        unit
*)
let applyi_on_transformed_targets_between (transformer : path * int -> 'a) (tr : int -> trm -> 'a -> trm) (tg : target) : unit =
  Trace.apply (fun t -> with_stringreprs_available_for [tg] t (fun t ->
  let ps =
    Trace.timing ~cond:!Flags.analyse_time_details ~name:"resolve_targets" (fun () -> resolve_target_between tg t
      (* ALTERNATIVE
      with_stringreprs_available_for tg t (fun t2 ->
        resolve_target_between tg t2) *) ) in
  let marks = List.map (fun _ -> Mark.next ()) ps in
  let t = Trace.timing ~cond:!Flags.analyse_time_details ~name:"resolve_add_mark" (fun () -> List.fold_left2 (fun t (p_to_seq, i) m -> apply_on_path (trm_add_mark_between i m) t p_to_seq ) t ps marks) in
  try
    Tools.fold_lefti (fun imark t m ->
      Trace.timing ~cond:!Flags.analyse_time_details ~name:(sprintf "process target %d" imark) (fun () ->
        match resolve_target [nbAny;cMark m] t with
        | [p_to_seq] ->
          let t_seq, _ = resolve_path_and_ctx p_to_seq t in
          let i = begin match get_mark_index m t_seq with | Some i -> i | None -> fail t_seq.loc "applyi_on_transformed_targets_between: could not get the between index" end in
          let t = apply_on_path (trm_remove_mark_between m) t p_to_seq in
          tr imark t (transformer (p_to_seq,i))
        | ps ->
          let msg =
            if ps <> []
              then "applyi_on_transformed_targets_between: a mark was duplicated"
              else (Tools.sprintf "applyi_on_transformed_targets_between: mark %s disappeared" m) in
          if debug_disappearing_mark
            then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
            else fail None msg
      )) t marks
    with Interrupted_applyi_on_transformed_targets t -> t
  ))

(* [apply_on_transformed_targets_between ~replace_top transformer tr tg]:
    Same as [applyi_to_transformed_targets_between] except that here the index of the resolved_path is not considered.
    params:
      transformer: ..
      tr: transformation to be applied
      tg: target
    return:
      unit
*)
let apply_on_transformed_targets_between (transformer: path * int -> 'a) (tr : trm -> 'a -> trm) (tg : target) : unit =
  applyi_on_transformed_targets_between transformer (fun _i t descr -> tr t descr) tg


(* [applyi_on_targets_between ~replace tr tg]:  A specialization of [applyi_on_transformed_targets_between] but here the transformer
      is the identity function.
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
let applyi_on_targets_between (tr : int -> trm -> path * int -> trm) (tg : target) : unit =
  applyi_on_transformed_targets_between (fun (p,i) -> (p,i)) tr tg

(* [apply_on_targets_between ~replace tr tg]: A specialization of [applyi_on_targets_between] but here the index of the resolved path is not
      taken into account.
      params:
        tg : target
        tr : transformation to be applied
      return:
        unit
*)
let apply_on_targets_between (tr : trm -> 'a -> trm) (tg : target) : unit =
  applyi_on_targets_between (fun _i t pk -> tr t pk) tg



(******************************************************************************)
(*                                   Show                                     *)
(******************************************************************************)

(* [target_show_aux m t]: adds a mark [m] around the term t.
*)
let target_show_aux (m : mark) (t : trm) : trm =
  trm_add_mark m t

(* [target_show_transfo m t p]: adds a mark [m]
  around the term at path [p] in the term [t]. *)
let target_show_transfo (m : mark) : Transfo.local =
  apply_on_path (target_show_aux m)

(* [target_between_show_aux m k t]: adds a a mark [m]
   at position [k] in the marks list of the sequence described by the term [t]. *)
let target_between_show_aux (m : mark) (k : int) (t : trm) : trm =
    trm_add_mark_between k m t

(* [target_between_show_transfo id k t p]: adds a mark with identifier [id]
   at position [k] in the sequence at path [p] in the term [t]. *)
let target_between_show_transfo (m : mark) : Transfo.local_between =
  fun (k:int) -> apply_on_path (target_between_show_aux m k)

(* [bigstep s] is a shorthand for [Trace.bigstep s] *)
let bigstep (s : string) : unit =
  Trace.bigstep s

(* [show_next_id] is used for batch mode execution of unit tests,
   to generate names of for marks.
   Only used when [Flags.execute_show_even_in_batch_mode] is set.  *)
let (show_next_id, show_next_id_reset) : (unit -> int) * (unit -> unit) =
  Tools.resetable_fresh_generator()

(* [show ~line:int tg] is a transformation for visualizing targets.
   The operation add marks if the command line argument [-exit-line]
   matches the [line] argument provided to the function. Otherwise, the
   [show] function only checks that the path resolve properly.
   There is no need for a prefix such as [!!] in front of the [show]
   function, because it is recognized as a special function by the preprocessor
   that generates the [foo_with_lines.ml] instrumented source. *)
let show ?(line : int = -1) ?(reparse : bool = false) (tg : target) : unit =
  (* Automatically add [nbMulti] if there is no occurence constraint *)
  let tg = enable_multi_targets tg in
  if reparse then reparse_alias();
  let should_exit = (Flags.get_exit_line() = Some line) in
  let batch_mode = (Flags.get_exit_line() = None) in
  let marks_base = show_next_id() in
  let mark_of_occurence (i:int) : string =
    if batch_mode && !Flags.execute_show_even_in_batch_mode
      then sprintf "%d_%d" marks_base i
      else sprintf "%d" i
    in
  if should_exit || (!Flags.execute_show_even_in_batch_mode && batch_mode) then begin
    if Constr.is_target_between tg then begin
      applyi_on_targets_between (fun i t (p,k) ->
        let m = mark_of_occurence i in
        target_between_show_transfo m k t p) tg
    end else begin
      applyi_on_targets (fun i t p ->
        let m = mark_of_occurence i in
        target_show_transfo m t p) tg
    end;
    if should_exit
      then dump_diff_and_exit()
  end else begin
    (* only check targets are valid *)
    if Constr.is_target_between tg
      then applyi_on_targets_between (fun _i t (_p,_k) -> t) tg
      else applyi_on_targets (fun _i t _p -> t) tg
  end

(* [get_trm_at] returns that trm that corresponds to the target [tg]
    Note:
      Call this function only on targets which resolve to a unique ast node
*)
let get_trm_at (tg : target) : trm =
  let t_ast = ref (trm_unit ()) in
  Trace.call (fun t ->
    let tg_path = resolve_target_exactly_one_with_stringreprs_available tg t in
    t_ast := Path.resolve_path tg_path t
  );
  !t_ast

(* [get_ast ()] returns the full ast*)
let get_ast () : trm =
  get_trm_at []




(******************************************************************************)
(*                          Reparse                                           *)
(******************************************************************************)
(* LATER: We can use the following type for reparsing *)
(* type reparse = | Reparse_none | Reparse_only_paths | Reparse_all *)



(* [get_function_name_at dl] get the name of the function that corresponds to [dl]*)
let get_function_name_at (dl : path) : string option =
  let fun_decl = get_trm_at (target_of_path dl) in
  match fun_decl.desc with
  | Trm_let_fun (f, _, _, _) -> Some f
  | _ -> None


(* [get_top_level_function_name_containing dl] get the name of the toplevel function which contains the path [dl]  *)
let get_toplevel_function_name_containing (dl : path) : string option =
  match dl with
  | Dir_seq_nth i :: Dir_body :: _ -> get_function_name_at [Dir_seq_nth i]
  | _ -> None


(* [reparse_only fun_nmaes] reparse only those functions whose identifier is contained in [fun_names]*)
let reparse_only (fun_names : string list) : unit =
  Trace.call (fun t ->
    let chopped_ast, chopped_ast_map  =  hide_function_bodies (function f -> not (List.mem f fun_names)) t in
    let parsed_chopped_ast = Trace.reparse_trm  (Trace.get_context ()) chopped_ast in
    let new_ast = update_chopped_ast parsed_chopped_ast chopped_ast_map in
    Trace.set_ast new_ast
  )

(* [get_relative_type tg] get the type of target relative , Before, After, First Last *)
let get_relative_type (tg : target) : target_relative option =
  List.fold_left (fun acc x ->
    match acc with
    | Some _ -> acc
    | None ->
      begin match x with
      | Constr_relative occ -> Some occ
      | _ -> None
      end
  ) None tg


(* [reparse_after tr] is a wrapper to invoke for forcing the reparsing
    after a transformation. For example because it modifies type definitions.
    See example in [Struct.inline]. The argument [~reparse:false] can be
    specified to deactivate the reparsing.
*)
let reparse_after ?(reparse : bool = true) (tr : Transfo.t) : Transfo.t =
  fun (tg : target) ->
    let tg = enable_multi_targets tg in
    let ast = (get_ast()) in
    (* LATER: it would be nice to avoid computing the
       with_stringreprs_available_for which we already compute later on
       during [tr tg]. *)
    let tg_paths = with_stringreprs_available_for [tg] ast (fun ast ->
      if Constr.is_target_between tg
      then let tg_ps = resolve_target_between tg ast in
           fst (List.split tg_ps)
      else resolve_target tg ast
      ) in
    tr tg;
    if reparse then begin
      if !Flags.use_light_diff then
      let fun_names = List.map get_toplevel_function_name_containing tg_paths in
      let fun_names = Tools.list_remove_duplicates (List.filter_map (fun d -> d) fun_names) in
      reparse_only fun_names
      else Trace.reparse();
    end


(* LATER: use this more efficient version that avoids computing path resolution twice

type apply_on_target_arg = trm -> path -> trm

let list_of_option (o : 'a option) : 'a list =
  match o with
  | None -> []
  | Some x -> [x]

let reparse_after ?(reparse : bool = true) (tr_of : (apply_on_target_arg -> apply_on_target_arg) -> Transfo.t) : Transfo.t =
  fun (tg : target) ->
    let function_names_to_reparse : string list ref = ref [] in
    let reparse_where (tr : apply_on_target_arg) : apply_on_target_arg =
      fun (t:trm) (p:path) ->
        function_names_to_reparse := (list_of_option (get_toplevel_function_name_containing p)) @ !function_names_to_reparse;
        tr t p
      in
    tr_of reparse_where tg;
    let func_names_to_keep = Tools.remove_duplicates !function_names_to_reparse in
    if reparse then reparse_only func_names_to_keep


example usage: in Access_basic.
let transform ?(reparse : bool = false) (f_get : trm -> trm) (f_set : trm -> trm) : Target.Transfo.t =
  Target.reparse_after ~reparse (fun reparse_where ->
    Target.apply_on_targets (reparse_where (Accesses_core.transform f_get f_set)))

*)
