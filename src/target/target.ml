open Ast
open Trm
open Typ
open Mark
open Tools
open Path

include Constr

(* see Path.ml *)
type path = Path.path
type paths = path list
type case_dir = Path.case_dir

(* Shorthands for Trace functions *)
include Trace

(******************************************************************************)
(*                        Smart constructors for targets                      *)
(******************************************************************************)


(*
  a smart constructor builds a target
  thus, the user provides a target using them
  this list is then flattened to call resolve_target
  unit args are used because of optional arguments *)

(******************************************************************************)
(*                             Logic constraints                              *)
(******************************************************************************)

(* [cTrue]: matches anything.*)
let cTrue : constr =
  Constr_bool true

(* [cFalse]: matches nothing. *)
let cFalse : constr =
  Constr_bool false

(* [cStrictNew]: matches at depth zero. *)
let cStrictNew : constr =
  Constr_depth (DepthAt 0)

(* [cStrict]: matches at depth 1. *)
let cStrict : constr =
  Constr_depth (DepthAt 1)

(* [cInDepth]: matches at any depth. *)
let cInDepth : constr =
  Constr_depth DepthAny

(* [cTarget cstrs]: a constraint build on top of a list of constraints. *)
let cTarget (cstrs : constr list) : constr =
  Constr_target cstrs

(******************************************************************************)
(*                             Relative targets                               *)
(******************************************************************************)
(* [tBefore]: matches the location before an instruction. *)
let tBefore : constr =
  Constr_relative TargetBefore

(* [tAfter]: matches the location after an instruction. *)
let tAfter : constr =
  Constr_relative TargetAfter

(* [tFirst]: matches the first instruction on a sequence. *)
let tFirst : constr =
  Constr_relative TargetFirst

(* [tLast]: matches the last instruction on a sequence. *)
let tLast : constr =
  Constr_relative TargetLast

(******************************************************************************)
(*                            Number of targets                               *)
(******************************************************************************)

(* [nbMulti]: matches one or more trms
   Note: all the targets that resolve to more than one trm require this constraint. *)
let nbMulti : constr =
  Constr_occurrences ExpectedMulti

(* [nbAny]: matches zero or more trms. *)
let nbAny : constr =
    Constr_occurrences ExpectedAnyNb

(* [nbExact nb]: matches [nb] trms. *)
let nbExact (nb : int) : constr =
    Constr_occurrences (ExpectedNb nb)

(* [occIndices ~nb indices]: matches target occurrences based on [indices]. *)
let occIndices ?(nb : int = -1) (indices : int list) : constr =
  let expected_nb = match nb with
    | -1 -> None
    | _ -> Some nb in
  Constr_occurrences (ExpectedSelected (expected_nb, indices)  )

(* [occIndex ~nb index]: matches the trm with the index occurrence [index]. *)
let occIndex ?(nb : int = -1) (index : int) : constr =
  occIndices ~nb [index]

(* [occFirst]: matches the first occurrence of the target. *)
let occFirst : constr =
  Constr_occurrences FirstOcc

(* [occLast]: matches the last occurrence of a target. *)
let occLast : constr =
  Constr_occurrences LastOcc

(******************************************************************************)
(*                                Directions                                  *)
(******************************************************************************)

(* [target_of_path p]: converts path [p] to a target. *)
let target_of_path (p : path) : target =
  [Constr_paths [p]]

(* [target_of_paths ps]: converts paths [ps] to a target. *)
let target_of_paths (ps : paths) : target =
  [Constr_paths ps]

(* [dRoot]: matches the root of the ast. *)
let dRoot : constr =
    Constr_root

(* [dBefore]: matches the interstice before the instruction at index [n] in a sequence. *)
let dBefore (i : int) : constr =
  Constr_dir (Dir_before i)

(* [dAfter]: matches the interstice after the instruction at index [n] in a sequence. *)
let dAfter (i : int) : constr =
  Constr_dir (Dir_before (i + 1))

(* [dArrayNth]: matches the trm with index [n] on an array initialization list. *)
let dArrayNth (n : int) : constr =
    Constr_dir (Dir_array_nth n)

(* [dSeqNth]: matches the instruction with index [n] on a sequence. *)
let dSeqNth (n : int) : constr =
    Constr_dir (Dir_seq_nth n)

(* [dStructNth]: matches the trm with index [n] on a struct initialization list. *)
let dStructNth (n : int) : constr =
    Constr_dir (Dir_struct_nth n)

(* [dCond]: matches a condition. *)
let dCond : constr =
    Constr_dir Dir_cond

(* [dThen]: matches a then branch. *)
let dThen : constr =
    Constr_dir Dir_then

(* [dElse]: matches an else branch. *)
let dElse : constr =
    Constr_dir Dir_else

(* [dBody]: matches the body of a definition, if or else branches etc. *)
let dBody : constr =
  Constr_dir Dir_body

(* [dVarBody]: matches the body of a variable definition, bypassing the new operation. *)
let dVarBody : constr =
  Constr_dir Dir_var_body

(* [dForStart]: matches the initialization value of a simple loop. *)
let dForStart : constr =
  Constr_dir Dir_for_start

(* [dForStop]: matches the bound trm of a simple for loop. *)
let dForStop : constr =
  Constr_dir Dir_for_stop

(* [dForStep]: matches the step trm of a simple for loop. *)
let dForStep : constr =
  Constr_dir Dir_for_step

(* [dForCInit]: matches the initialization trm of a for_c loop. *)
let dForCInit : constr =
    Constr_dir Dir_for_c_init

(* [dForCStep]: matches the step trm of a for_c loop. *)
let dForCStep : constr =
    Constr_dir Dir_for_c_step

(* [dName]: matches a variable occurrence(a function or variable name). *)
let dName : constr =
    Constr_dir Dir_name

(* [dDirCase n cd]: matches a case group on switch statement. *)
let dDirCase (n : int) (cd : case_dir) : constr =
    Constr_dir (Dir_case (n, cd))

(* [dCaseName n]: matches case [n] on a switch statement. *)
let dCaseName (n : int) : case_dir = Case_name n

(* [dCaseBody]: matches a case body on a switch statement. *)
let dCaseBody : case_dir = Case_body

(* [dEnumConst n]: matches a constant enum declaration. *)
let dEnumConst (n : int)
  (ecd : enum_const_dir) : constr =
    Constr_dir (Dir_enum_const (n, ecd))

(* [dEnumConstName]: matches a constant enum name. *)
let dEnumConstName : enum_const_dir = Enum_const_name

(* [dEnumConstVal]: matches a constant enum value . *)
let dEnumConstVal : enum_const_dir = Enum_const_val

(* [dArg]: matches nth argument of a function call or function declaration. *)
let dArg (n : int) : constr =
  Constr_dir (Dir_arg_nth n)


(* [string_to_rexp regexp substr s trmKind]:  transforms a string into a regular expression
    used to match ast nodes based on their code representation.
    [string_to_rexp] - denotes a flag to tell if the string entered is a regular epxression or no
    [substr] - denotes a flag to decide if we should target strings that contain string [s] as a strict substring
    [trmKind] - denotes the kind of the ast note represented in code by string [s]. *)
let string_to_rexp (regexp : bool) (substr : bool) (s : string) (trmKind : trm_kind) : rexp =
    { rexp_desc = (if regexp then "Regexp" else "String") ^ "(\"" ^ s ^ "\")";
      rexp_exp =
        (let pat1 = if regexp then s else Str.quote s in
        let pat2 = if substr then pat1 else ("^" ^ pat1 ^ "$") in
        Str.regexp pat2);
      rexp_substr = substr; (* LATER: is there a smart way to avoid rexp_substr in addition to the regexp? *)
      rexp_trm_kind = trmKind; }
(* [string_to_rexp_opt regexp substr s trmKind]: if s = "" then returns nothing else it returns the same result as
     [string_to_rexp]. *)
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

(* [sInstrOrExpr ~substr tk s]: matches an instruction or an expression
    [substr] - if true matches also substrings
    [tk] - matches only trms of [tk] kind
    [s] - string representation of a trm. *)
let sInstrOrExpr ?(substr : bool = false) (tk : trm_kind) (s : string) : constr =
  Constr_regexp (string_to_rexp false substr s  tk)

(* [sInstr ~substr s]: matches an instruction. *)
let sInstr ?(substr : bool = true) (s : string) : constr =
  sInstrOrExpr ~substr TrmKind_Instr s

(* [sExpr ~substr s]: matches an expression. *)
let sExpr ?(substr : bool = true) (s : string)  : constr =
  sInstrOrExpr ~substr TrmKind_Expr s

(* [sInstrOrExprRegexp tk substr s]: matches an instruction or expression using regular expressions. *)
let sInstrOrExprRegexp (tk : trm_kind) (substr : bool) (s : string) : constr =
  Constr_regexp (string_to_rexp true substr s tk)

(* [sInstrRegexp ~substr s]: matches an instruction using regular expressions. *)
let sInstrRegexp ?(substr : bool = true) (s : string) : constr =
  sInstrOrExprRegexp TrmKind_Instr substr s

(* [sExprRegexp ~substr s]: matches an expression using regular expressions. *)
let sExprRegexp ?(substr : bool = true) (s : string) : constr =
  sInstrOrExprRegexp TrmKind_Expr substr s


(******************************************************************************)
(*                                Ast nodes                                   *)
(******************************************************************************)

let cPred (p : trm -> bool) : constr =
  Constr_pred p

(* [cInclude s]: matches include directives. *)
let cInclude (s : string) : constr =
    Constr_include s

(* [cOr tgl]: matches the union of the target list [tgl]. *)
let cOr (tgl : target list) : constr =
  Constr_or tgl

(* TODO: should this add 'nbAny'? *)
(* [any f l]: targets any [f l0], .., [f lN] *)
let any (f : 'a -> constr) (l : 'a list) : constr =
  cOr (List.map (fun x -> [f x]) l)

(* LATER: symbole infixe/prefixe? *)
(* [multi f l]: targets multiple [f l0], .., [f lN] *)
let multi (f : 'a -> constr) (l : 'a list) : constr =
  cTarget [nbMulti; cOr (List.map (fun x -> [f x]) l)]

(* [cAnd tgl]: matches the intersection of the target list [tgl]. *)
let cAnd (tgl : target list) : constr =
  Constr_and tgl

(* [cDiff tgl1 tgl2]: matches the difference between [tgl1] and [tgl2]. *)
let cDiff (tgl1 : target list) (tgl2 : target list) : constr =
  Constr_diff (tgl1, tgl2)

(* [typ_constraint_default]: matches all types. *)
let typ_constraint_default : typ_constraint =
  (fun _ -> true)

(* [make_typ_constraint ~typ ~typ_pred ()]: make a type constraint based on [typ] or [typ_pred]. *)
let make_typ_constraint ?(typ:string="") ?(typ_pred : typ_constraint = typ_constraint_default) () : typ_constraint =
  if typ <> "" && typ_pred != typ_constraint_default
    then fail None "Target.make_typ_constraint: cannot provide both ~typ and ~typ_pred.";
  if typ = ""
    then typ_pred
    else (fun (ty : typ) -> typ = (AstC_to_c.typ_to_string ty))

(* [cHasTypePred pred]: matches all types that satisfy [pred]. *)
let cHasTypePred (pred : typ -> bool) : constr =
  Constr_hastype pred

(* [cHasTypeAst ty]: matches [ty] types. *)
let cHasTypeAst (ty : typ) : constr =
  let pred = (fun (ty2 : typ) -> same_types ty ty2) in
  cHasTypePred (make_typ_constraint ~typ_pred:pred ())

(* [cHasType typ]: matches all types that have the same string representation as [typ]. *)
let cHasType (typ : string) : constr =
  cHasTypePred (make_typ_constraint ~typ ())

(* [with_type ~typ ~typ_pred tg]: matches all targets of type [typ] or their type can be matched by [typ_pred]. *)
let with_type ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (tg : target) : target =
  if typ = "" && typ_pred == typ_constraint_default
    then tg
    else begin
      if typ <> "" && typ_pred == typ_constraint_default
        then [cAnd [tg; [cHasType typ] ]]
      else if typ = "" && not (typ_pred == typ_constraint_default)

        then [cAnd [tg; [cHasTypePred typ_pred]]]
      else
        fail None "Target.with_type: type targets should be used with the type or a predicated on the type but not
                   both at the same time"
    end

(* [cArgPred ~typ ~typ_pred pred]: matches all arguments that match the predicate [pred], and whose type  is [typ] or
  satisfies [typ_pred]. *)
let cArgPred ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (pred : string -> bool) : constr =
  Constr_arg (pred, make_typ_constraint ~typ ~typ_pred ())

(* [cArg ~typ ~typ_pred name]: matches all arguments of name [name] and whose type is [typ] or satisfies [typ_pred]. *)
let cArg ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let pred = if (name = "") then (fun _ -> true) else (fun x -> x = name) in
  cArgPred ~typ ~typ_pred pred

(* [cVarDef ~regexp ~substr ~body ~typ ~typ_pred name]: matches all variable definitions
    [regexp] - match using regular expressions
    [substr] - match names partially
    [body] - match based on their body
    [typ] - match based on type
    [typ_pred] - match based on type. *)
let cVarDef ?(regexp : bool = false) ?(substr : bool = false) ?(body : target = []) ?(typ : string = "")
  ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Instr in
  let ty_pred = make_typ_constraint ~typ ~typ_pred () in
  Constr_decl_var (ty_pred, ro, body)


(* [cVarDefs vars]: matches a list of variable definitions based on their names. *)
let cVarDefs (vars : string list) : constr =
  let vardefs = List.map (fun v -> [cVarDef v]) vars in
  cOr vardefs

(* [cVarDefReg reg]: matches variable definitions using regexp. *)
let cVarDefReg (reg : string) : constr =
  cVarDef ~regexp:true reg

(* [cVarInit var]: matches the initialization value of a variable defintioon with name [var]. *)
let cVarInit (var : string) : constr =
  cTarget [cVarDef var; dVarBody]

(* [cVarDef ~regexp ~substr ~body ~typ ~typ_pred name]: matches all multiple variable definitions
    [regexp] - match using regular expressions
    [substr] - match names partially
    [body] - match based on their body
    [typ] - match based on type
    [typ_pred] - match based on type. *)
let cVarsDef ?(regexp : bool = false) ?(substr : bool = false) ?(body : target = []) ?(typ : string = "")
  ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Instr in
  let ty_pred = make_typ_constraint ~typ ~typ_pred () in
  Constr_decl_vars (ty_pred, ro, body)

(* [cFor ~start ~direction ~stop ~step ~body index] matches simple for loops
     [start] - match based on the initial value trm
     [direction] - match based on the direction of the loop
     [stop] - match based on the bound value trm
     [body] - match based on the step value trm
     [index] - match based on the index. *)
let cFor ?(start : target = []) ?(direction : loop_dir option) ?(stop : target = []) ?(step : target = [])
  ?(body : target = []) (index : string) : constr =
  let ro = string_to_rexp_opt false false index TrmKind_Instr in
  Constr_for (ro, start, direction, stop, step, body)


(* [cForNestedAtDepth i]: matches the loop at depth [i] on a nested loops trm. *)
let cForNestedAtDepth (i:int) : constr =
  Constr_target (List.flatten (List.init i (fun _ -> [cStrict; cFor ""])))

(* [cFor_c ~init ~cond ~step ~body index]: matches for_c loops
    [init] - match based on the initialization trm
    [cond] - match based on the condition trm
    [step] - match based on the step trm
    [body] - match based on the body trm
    [index] - match based on the index. *)
let cFor_c ?(init : target = []) ?(cond : target = []) ?(step : target = []) ?(body : target = [])
  (index : string) : constr =
  let init =
      match index, init with
      | "", [] -> init
      | "", _ -> init
      | _, [] -> [cVarDef index]
      | _, _::_ -> init
      in
    Constr_for_c ( init,  cond,  step,  body)

(* [cWhile ~cond ~body ()]: matches while loop
     [cond] - match based on the condition
     [body] - match based on the body. *)
let cWhile ?(cond : target = []) ?(body : target = []) () : constr =
  let p_cond = cond in
  let p_body = body in
    Constr_while (p_cond, p_body)

(* [cDoWhile ~body ~cond ()]: matches a do while loop
    [ody] - match based on the body
    [cond] - match based on the condition. *)
let cDoWhile ?(body : target = []) ?(cond : target = [])  () : constr =
  let p_body = body in
  let p_cond = cond in
    Constr_do_while (p_cond, p_body)

(* [cIf ~cond ~then ~else ()]: matches an if statement
    [cond] - match based on the condition
    [then_] - match based on the then branch
    [else_] - match based on the else branch. *)
let cIf ?(cond : target = []) ?(then_ : target = []) ?(else_ : target = []) (_ : unit) : constr =
  let p_cond = cond in
  let p_then = then_ in
  let p_else = else_ in
    Constr_if (p_cond, p_then, p_else)

(* [cThen]: matches a then branch. *)
let cThen : constr =
 Constr_target [cIf(); dThen]

(* [target_list_simpl args]: convert a list of targets into a [target_list_pred]. *)
let target_list_simpl (args : targets) : target_list_pred =
  let n = List.length args in
  make_target_list_pred
    (fun i -> if i < n then List.nth args i else [cStrict;cFalse])
    (fun bs -> List.length bs = n && Xlist.all_true bs)
    (fun () -> "target_list_simpl(" ^ (list_to_string (List.map target_to_string args) ^ ")"))

(* NOTE: the "_st" suffix means that the argument is a constraint and not a target
   --we might revisit this convention later if we find it not suitable. *)

(* [target_list_one_st tg]: convert a target into a [target_list_pred] that checks that at least
   one of the items in the list satisfies the given constraint. *)
let target_list_one_st (tg : target) : target_list_pred =
  make_target_list_pred
    (fun _i -> tg)
    (fun bs -> List.mem true bs)
    (fun () -> "target_list_one_st(" ^ (target_to_string tg) ^ ")")


(* [target_list_all_st tg]: convert a target into a [target_list_pred] that checks that at least
   all the items in the list satisfies the given constraint. *)
let target_list_all_st (tg : target) : target_list_pred = (* LATER: KEEP ONLY THIS. *)
  make_target_list_pred
    (fun _i -> tg)
    (fun bs -> List.for_all (fun b -> b = true) bs)
    (fun () -> "target_list_all_st(" ^ (target_to_string tg) ^ ")")

(* [target_list_pred_default]: predicate that matches any list of arguments. *)
let target_list_pred_default : target_list_pred =
  make_target_list_pred
    (fun _i -> [cTrue])
    Xlist.all_true
    (fun () -> "target_list_pred_default")

(* [combine_args args args_pred]: takes [args] as a [target_list_simpl] if it is nonempty,
   and else returns [args_pred]; raise an error if the two arguments have non-default values. *)
let combine_args (args:targets) (args_pred:target_list_pred) : target_list_pred =
  match args with
  | [] -> args_pred
  | _ ->
      if args_pred != target_list_pred_default
        then fail None "cFunDef: can't provide both args and args_pred";
      target_list_simpl args

(* [cFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def name]: matches function definitions
     [args] - match based on arguments
     [args_pred] - match based on arguments
     [body] - match based on the body of the function
     [ret_typ] - match based on the return type
     [ret_typ_pred] - match based on the return type
     [regexp] - match based on regexp
     [is_def] - if false matches also declarations
     [name] - match based on the name of the function. *)
let cFunDef ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = [])
  ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false)
  ?(is_def : bool = true) ?(clang_id : Clang.cxcursor option)(name : string) : constr =
  let ro = string_to_rexp_opt regexp false name TrmKind_Expr in
  let ty_pred = make_typ_constraint ~typ:ret_typ ~typ_pred:ret_typ_pred () in
  Constr_decl_fun (ty_pred, ro, combine_args args args_pred, body, is_def, clang_id)

(* [cFunBody] same as [cFunDef] followed by [dBody]. *)
let cFunBody ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = [])
  ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false)
  ?(is_def : bool = true) ?(clang_id : Clang.cxcursor option)(name : string) : constr =
  cTarget [cFunDef ~args ~args_pred ~ret_typ ~ret_typ_pred ~regexp ~is_def ?clang_id name; dBody]

(* [cFunDefAndDecl ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def name]: matches function definitions and declarations
     [args] - match based on arguments
     [args_pred] - match based on arguments
     [body] - match based on the body of the function
     [ret_typ] - match based on the return type
     [ret_typ_pred] - match based on the return type
     [regexp] - match based on regexp
     [name] - match based on the name of the function. *)
let cFunDefAndDecl ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = [])
  ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false)
  ?(clang_id : Clang.cxcursor option)(name : string) : constr =
  let fund (is_def : bool) = cFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def ?clang_id name in
  cOr [[fund true]; [fund false]]

(* [cTopFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def name]: matches top level function definitions
     [args] - match based on arguments
     [args_pred] - match based on arguments
     [body] - match based on the body of the function
     [ret_typ] - match based on the return type
     [ret_typ_pred] - match based on the return type
     [regexp] - match based on regexp
     [is_def] - if false matches also declarations
     [name] - match based on the name of the function. *)
let cTopFunDef ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = [])
  ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false)
  ?(is_def : bool = true) ?(clang_id : Clang.cxcursor option) (name : string) : constr =
  cTarget [ dRoot; cStrict; cFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def ?clang_id name ]

(* [cTopFunDefAndDecl ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp name]: matches top level function definitions
     and declarations
     [args] - match based on arguments
     [args_pred] - match based on arguments
     [body] - match based on the body of the function
     [ret_typ] - match based on the return type
     [ret_typ_pred] - match based on the return type
     [regexp] - match based on regexp
     [name] - match based on the name of the function. *)
let cTopFunDefAndDecl ?(args : targets = []) ?(args_pred : target_list_pred = target_list_pred_default) ?(body : target = [])
  ?(ret_typ : string = "") ?(ret_typ_pred : typ_constraint = typ_constraint_default) ?(regexp : bool = false)
  ?(clang_id : Clang.cxcursor option) (name : string) : constr =
  let topfund (is_def : bool) = cTopFunDef ~args ~args_pred ~body ~ret_typ ~ret_typ_pred ~regexp ~is_def ?clang_id name in
  cOr [[topfund true ]; [topfund false]]

(* [cTopFunDefAndDeclReg reg]: matches top level function definitions and declarations based on regexp [reg]. *)
let cTopFunDefAndDeclReg (reg : string) : constr =
  cTopFunDefAndDecl ~regexp:true reg

(* [cTopFunDefs names]: matches multiple top level function definitions based on [names]. *)
let cTopFunDefs (names : string list) : constr =
  cOr (List.map (fun name -> [cTopFunDef name]) names)

(* [cTopFunDefReg]: matches top level function definitions using the regular expression [reg]. *)
let cTopFunDefReg (reg : string) : constr =
  cTopFunDef ~regexp:true reg

(* [cTop ~regexp name]: matches toplevel declarations(for the moment only functions are matched). *)
let cTop ?(regexp : bool = false) (name : string) : constr =
  cTopFunDef ~regexp name

(* [cTypDef ~substr ~regexp name]: matches a type definition
    [substr] - match based on name partially
    [regexp] - match based on regexp(on name)
    [name] - match based on name. *)
let cTypDef ?(substr : bool = false) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  Constr_decl_type ro

(* [cDef name]: matches a definition based on [name]. *)
let cDef (name : string) : constr =
  cOr [[cFunDef name];[cVarDef name];[cTypDef name]]

(* [cEnum ~name ~substr ~constants ~regexp ()] match constant enum declarations
    [name] - match based on name
    [substr] - match partially based on name
    [constants] - match base on each enum component
    [regexp] - match based on regexp. *)
let cEnum ?(name : string = "") ?(substr : bool = false) ?(constants : (string * (target)) list = [])
  ?(regexp : bool = false) () : constr =
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

(* [cSeq ~args ~args_pred ()]: matches a sequence
    [args] - match based on instructions
    [args_pred] - match based on instructions that satisfy [args_pred]. *)
let cSeq ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) () : constr =
  Constr_seq (combine_args args args_pred)

(* [cVar ~regexp ~substr ~trmkind ~typ ~typ_pred name]: matches variable occurrences
    [regepx] - match based on regexp
    [substr] - match partially
    [typ] - match based on type
    [typ_pred] - match based on type predicate. *)
let cVar ?(regexp : bool = false) ?(substr : bool = false) ?(typ : string = "")
  ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  let c = Constr_var ro in
  if typ = "" && typ_pred == typ_constraint_default then c else (* this line is just an optimization. *)
  Constr_target (with_type ~typ ~typ_pred [c])

(* [cVarReg reg]: matches variables occurrences based on regexp. *)
let cVarReg (reg : string) : constr =
  cVar ~regexp:true reg

(* [cLitPred pred_l]: matches all the literals that statisfy the predicate [pred_l]. *)
let cLitPred (pred_l : lit -> bool) : constr =
  Constr_lit pred_l

(* [cLit]: matches all the literals. *)
let cLit : constr =
  cLitPred (function _ -> true)

(* [cIntPred pred]: matches all the integer literals that statisfy the predicate [pred]. *)
let cIntPred (pred : int -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_int n -> pred n
   | _ -> false
   end )

(* [cInt n]: matches all int literals equal to [n]. *)
let cInt (n : int) : constr =
  cIntPred (function m -> m = n)

(* [cDoublePred pred]: matches all the double literals that statisfy the predicate [pred]*)
let cDoublePred (pred : float -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_double d -> pred d
   | _ -> false
   end )

(* [cDouble d] match all the doubles equal to [d]. *)
let cDouble (d : float) : constr =
  cDoublePred (function d1 -> d1 = d)

(* [cBoolPred pred] match all the boolean literals that satisfy the predicate [pred]. *)
let cBoolPred (pred : bool -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_bool b -> pred b
   | _ -> false
   end )

(* [cBool b]: matches all the booleans equal to [b]. *)
let cBool (b : bool) : constr =
  cBoolPred (function b1 -> b1 = b)

(* [cStringPred pred]: matches all the string literals that satisfy the predicate [pred]. *)
let cStringPred (pred : string -> bool) : constr =
  cLitPred (function l ->
   begin match l with
   | Lit_string s -> pred s
   | _ -> false
   end )

(* [let cString s]: matches all the string literals equal to [s]. *)
let cString (s : string) : constr =
  cStringPred (function s1 -> s1 = s)

(* [cCall ~fun_ ~args ~args_pred ~accept_encoded ~regexp name]: function applications
     [fun_] - match based on function
     [args] - match based on the arguments
     [args_pred] - match based the predicate on the arguments
     [accept_encoded] - match encoded functions
     [regexp] - match based on regexp
     [name] - match based on name of the function. *)
let cCall ?(fun_  : target = []) ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default)
  ?(accept_encoded : bool = false) ?(regexp : bool = false) (name:string) : constr =
  let exception Argument_Error of string in
  let p_fun = match fun_ with
  | [] -> [cVar ~regexp  name]
  | _ ->
    begin match name with
    | "" -> fun_
    | _ -> raise (Argument_Error "Can't provide both the path and the name of the function")
    end in
  Constr_app (p_fun, combine_args args args_pred, accept_encoded)

(* [cFun ~fun_ ~args ~args_pred ~regexp name]: matches function call
    [fun_] - match based on the called function
    [args] - match based on the arguments of the call
    [args_pred] - match based on the arguments that satisfy a given predicate
    [regexp] - match based on regexp
    [name] - match based on the name of the called function . *)
let cFun ?(fun_  : target = []) ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default)
  ?(regexp : bool = false) (name:string) : constr =
  cCall ~fun_ ~args ~args_pred ~accept_encoded:false ~regexp name

(* [cFuns funs]: matches a list of function calls based on their names. *)
let cFuns (funs : string list) : constr =
  let funcalls = List.map (fun f -> [cFun f]) funs in
  cOr funcalls

(* [cPrimPred f_pred]: matches all primitive functions that satisfy the predicate [f_pred]*)
let cPrimPred (f_pred : prim -> bool) : constr =
  Constr_prim f_pred

(* [cPrim p]: matches all [p] primitives. *)
let cPrim (p : prim) : constr =
  cPrimPred (fun p2 -> p2 = p)

(* [cPrimPredFun ~args ~args_pred  prim_pred]: matches only primitive function calls that satisfy the predicate [prim_pred]
    and the other constraints in [args] or [args_pred]
    [args] - match based on the arguments
    [args_pred] - match based on a predicate on arguments
    [prim_pred] - match based on a primitive predicate. *)
let cPrimPredFun ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) (prim_pred:prim -> bool) : constr =
   cCall ~fun_:[cPrimPred prim_pred] ~args ~args_pred ~accept_encoded:true ""

(* [cPrimFun ~args ~args_pred p]: matches only primitive function calls with primitive [p]
    [args] - match based on the arguments
    [args_pred] - match based on a predicated over arguments
    [p] - match based on the primitive [p]. *)
let cPrimFun ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) (p:prim) : constr =
   cPrimPredFun ~args ~args_pred (fun p2 -> p2 = p)

(* [cPrimFunArith ~args ~args_pred ()]: matches primitive arithmetic operations
     [args] - match based on the arguments
     [args_pred] - match based on a predicate on the arguments. *)
let cPrimFunArith ?(args : targets = []) ?(args_pred:target_list_pred = target_list_pred_default) () : constr =
  cPrimPredFun ~args ~args_pred (fun p2 -> (is_arith_fun p2))

let cBinop ?(lhs : target = [cTrue]) ?(rhs : target = []) (op : binary_op) : constr =
  cPrimFun ~args:[lhs; rhs] (Prim_binop op)

(* [let cPrimNew ~arg ()]: matches "new" primitive operation
    [arg] - match based on the arguments of the "new" primitive. *)
let cPrimNew ?(arg : target = []) () : constr =
  cPrimPredFun ~args:[arg] (function Prim_new _ -> true | _ -> false)

(* [dVarInit: alias to dVarBody. *)
let dVarInit : constr =
   dVarBody

(* [dInit]: alias to dBody used for variable initializations. *)
let dInit : constr =
  dBody

(* [cWrite ~lhs ~rhs ~typ ~typ_pred ()]: matches a write(set) operation
     [lhs] - match based on the left operand
     [rhs] - match based on the right operand
     [typ] - match based on the type of the left operand
     [typ_pred] - match based on a predicate on the type of the left operand. *)
let cWrite ?(lhs : target = [cTrue]) ?(rhs : target = []) ?(typ : string = "")
  ?(typ_pred : typ_constraint = typ_constraint_default) (_ : unit) : constr =
  let lhs_typed = with_type ~typ ~typ_pred lhs in
  let rhs_typed = with_type ~typ ~typ_pred rhs in
  cPrimPredFun ~args:[lhs_typed; rhs_typed] (fun p -> match p with | Prim_binop Binop_set | Prim_compound_assgn_op _ -> true | _ -> false)

(* [cRead ~addr ()]: matches a get operation
    [addr] - match based on the read argument. *)
let cRead ?(addr : target = [cTrue]) () : constr =
  cPrimFun ~args:[addr] (Prim_unop Unop_get)

(* [cReadOrWrite ~addr ()]: matches a read or a write operation
     [addr] - match based on the read argument. *)
let cReadOrWrite ?(addr : target = [cTrue]) () : constr =
  cOr [[cWrite ~lhs:addr ()];[cRead ~addr ()]]

(* [cWriteVar ~regexp ~substr ~typ ~typ_pred name]: matches a write operation on a variable
    [regexp] - match based on regexp
    [substr] - match partially
    [typ] - match based on type
    [typ_pred] - match based on a predicate on the type
    [name] - match based on the name of the variable. *)
let cWriteVar ?(regexp : bool = false) ?(substr : bool = false) ?(typ : string = "") ?(typ_pred : typ_constraint = typ_constraint_default) (name : string) : constr =
  cWrite ~lhs:[cStrictNew;cVar ~regexp ~substr ~typ ~typ_pred name] ()

(* [cReadVar x]: matches a read operation on variable [x]. *)
let cReadVar (x : string) : constr =
  cRead ~addr:[cStrictNew; cVar x] ()

(* [cMark m]: matches a trm that is marked with mark [m]. *)
let cMark (m : mark) : constr =
  Constr_mark ((fun m1 -> m1 = m), "exactly:" ^ m)

(* [cMarks ms]: matches a trm that is marked with one of the marks [ms]. *)
let cMarks (ms : mark list) : constr =
  Constr_mark ((fun m1 -> List.mem m1 ms), "one of:" ^ (Tools.list_to_string ms))

(* [cMarkSt pred]: matches a trm that is marked with a mark that satisfies the predicate [pred]. *)
let cMarkSt (pred : mark -> bool) : constr =
  Constr_mark (pred, "such_that:" ^ "todo")

(* [cMarkAny]: matches a trm that is marked with any mark. *)
let cMarkAny : constr =
  Constr_mark ((fun _ -> true), "any_mark")

(* [cLabel ~substr ~body ~regexp label]:  match a  C labels
    [substr] - match label name partially
    [body] - match based on the trm the label is labelling
    [regep] - match based on regexp
    [label] - match based on label name. *)
let cLabel ?(substr : bool = false) ?(body : target = []) ?(regexp : bool = false) (label : string) : constr =
  let ro = string_to_rexp_opt regexp substr label TrmKind_Expr in
  let p_body = body in
  Constr_label (ro, p_body)

(* [cGoto ~label ~substr ~regexp ()]: matches a goto statement
    [label] - match based on the label it points to
    [substr] - match label name partially
    [regexp] - match based on regexp. *)
let cGoto ?(label : string = "") ?(substr : bool = false) ?(regexp : bool = false) () : constr =
  let ro = string_to_rexp_opt regexp substr label TrmKind_Expr in
  Constr_goto ro

(* [cReturn_tg ~res ()]: matches a return statement. *)
let cReturn_tg ?(res : target = []) () : constr =
  let p_res =  res in
  Constr_return p_res

(* [cAbrtAny]: any abort kind. *)
let cAbrtAny : abort_kind = Any

(* [cAbrtRet]: return abort kind. *)
let cAbrtRet : abort_kind = Return

(* [cAbrtBrk]: brake abort kind. *)
let cAbrtBrk : abort_kind = Break

(* [cAbrtCtn]: continue abort kind. *)
let cAbrtCtn : abort_kind = Continue

(* [cAbort ~kind ()]: matches an abort statement based on its [kind]. *)
let cAbort ?(kind : abort_kind = Any) () : constr =
  Constr_abort kind

(* [cReturn]: matches a return statement. *)
let cReturn : constr =
  Constr_abort (cAbrtRet)

(* [cBreadk]: matches a break statement. *)
let cBreak : constr =
  Constr_abort (cAbrtBrk)

(* [cContinue]: matches a continue statement. *)
let cContinue : constr =
  Constr_abort (cAbrtCtn)

(* [cAny]: matches a call to function "ANY". *)
let cAny : constr =
  cFun "ANY"

(* [cChoose]: matches a call to function "CHOOSE". *)
let cChoose : constr =
  cFun "CHOOSE"

(* [cAlloc ~d]: matches a call to Optitrust MALLOCI or CALLOCI where I = d. *)
let cAlloc (d : int option) : constr =
  let d = begin match d with | Some d -> string_of_int d | _ -> "." end in
  cFun ~regexp:true ("M.\\(NDEX\\|ALLOC\\)" ^ d)

(* [cMalloc ~d]: matches a call to Optitrust MALLOCI where I = d. *)
let cMalloc ?(d : int option) () : constr =
  let d = begin match d with | Some d -> string_of_int d | _ -> "." end in
  cFun ~regexp:true ("MALLOC" ^ d)

(* [cMindex ~d]:  match a call to Optitrust MINDEXI where I = d. *)
let cMindex ?(d : int option) ?(args : targets = []) () : constr =
  match d with
  | Some d -> cFun ~args ("MINDEX" ^ (string_of_int d))
  | None -> cFun ~args ~regexp:true "MINDEX."

(* [cCalloc ~d ()]: matches a call to Optitrust CALLOCI where I = d. *)
let cCalloc ?(d : int option) () : constr =
  let d = begin match d with | Some d -> string_of_int d | _ -> "." end in
  cFun ~regexp:true ("MALLOC" ^ d)

(* [cSwitch ~cond ~cases ()]: matches a switch statement
    [cond] - match based on the condition
    [cases] - match based on the cases. *)
let cSwitch ?(cond : target = []) ?(cases : (case_kind * (target)) list = []) () : constr =
  let p_cond =  cond in
  let c_cases =
    match cases with
    | [] -> None
    | _ -> Some (List.map (fun (k, pl) -> (k,  pl)) cases)
  in
    Constr_switch (p_cond, c_cases)

(* [cCase ~value ()]: matches case based on its value. *)
let cCase ?(value : target = []) () : case_kind =
  match value with
  | [] -> Case_any
  | _ -> Case_val ( value)

(* [cDefault]: default case. *)
let cDefault : case_kind = Case_default

(* [dLHS]: matches the left hand side of a write operation. *)
let dLHS : constr =
  cTarget [cWrite(); dArg 0]

(* [dRHS]: matches the right hand side of a write operation. *)
let dRHS : constr =
  cTarget [cWrite (); dArg 1]

(* [cTargetInDepth tg]: matches target [tg] in any depth. *)
let cTargetInDepth (tg : target) : constr =
  Constr_target (Constr_depth DepthAny :: tg)


(* [cAccesses ~base ~accesses ()]: matches an array_access or a struct access
      [base]  - match based on the base of an access
      [accesses] - a list of constraints on accesses.
      [inner_accesses] - match based on inner accesses
    Note:
      the empty list is interpreted as no constraint on the accesses,
      accesses are reversed so that users give constraints on what they see. *)
let cAccesses ?(base : target = []) ?(accesses : constr_access list = []) ?(inner_accesses : bool = true)() : constr =
  let p_base =  base in
  let accesses =
    match accesses with | [] -> None | cal -> Some (List.rev cal)
  in
    Constr_access (p_base, accesses, inner_accesses)

(* [cIndex ~index ()]: matches based on the [index] of the array access. *)
let cIndex ?(index : target = []) () : constr_access =
  let p_index =  index in
  Array_access p_index

(* [cField ~field ~substr ~regexp ()]: matches based on the field of a struct access
     Since a field is a string, this constructor allows us to use more advanced string matching.
     [field] - match based on the field as a string
     [substr] - match a field partially
     [regexp] - match based on regexp . *)
let cField ?(field : string = "") ?(substr : bool = false) ?(regexp : bool = false)
  (_ : unit) : constr_access =
  let ro = string_to_rexp_opt regexp substr field TrmKind_Expr in
  Struct_access ro

(* [cAccess]: matches any access. *)
let cAccess : constr_access =
  Any_access

(* [cFieldAccess field]: matches a struct access
     [base] - match based on the base of the access
     [substr] - match partially on the field
     [regexp] - match based on regexp on the accessed field
     [field] - match based on the accessed field. *)
let cFieldAccess ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(field : field = "" )  () : constr =
 cAccesses ~base ~accesses:[cField ~field ~substr ~regexp ()] ()

(* [cFieldRead field]: matches a read operation on a struct access
     [field] - match based on the accessed field
     [base] - match based on the base of the access
     [substr] - match partially on the field
     [regexp] - match based on regexp on the accessed field. *)
let cFieldRead ?(field : field = "") ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false)  () : constr =
  cRead ~addr:[cFieldAccess ~base ~substr ~regexp ~field ()] ()

(* [cFieldRead field]: matches a write operation on a struct access
     [field] - match based on the accessed field
     [base] - match based on the base of the access
     [substr] - match partially on the field
     [regexp] - match based on regexp on the accessed field. *)
let cFieldWrite ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(rhs : target = []) ?(field : field = "")  () : constr =
  let lhs = [cFieldAccess ~base ~substr ~regexp ~field ()] in
  cWrite ~lhs ~rhs ()


(* [cFieldRead field]: matches a read or awrite operation on a struct access
     [field] - match based on the accessed field
     [base] - match based on the base of the access
     [substr] - match partially on the field
     [regexp] - match based on regexp on the accessed field. *)
let cFieldReadOrWrite ?(base : target = []) ?(substr : bool = false) ?(regexp : bool = false) ?(field : field = "")  () : constr =
 cOr [[cFieldWrite ~base ~substr ~regexp ~field ()];[cFieldRead ~base ~substr ~regexp ~field ()] ]

(* [cCellAccess ~base ~index ]:  match an array accesses
     [base] - match based on the base of the access
     [index] - match based on the index of the access. *)
let cCellAccess ?(base : target = []) ?(index : target = [])  () : constr =
  cAccesses ~base ~accesses:[cIndex ~index ()] () (* TODO: are we missing a Cstrict before cIndex here? *)

(* [cCellRead ~base ~index ]:  match a read operation on an array accesses
     [base] - match based on the base of the access
     [index] - match based on the index of the access. *)
let cCellRead ?(base : target = []) ?(index : target = []) (): constr =
  cRead ~addr:[cCellAccess ~base ~index ()] ()

(* [cCellRead ~base ~index ]:  match a write operation on an array accesses
     [base] - match based on the base of the access
     [index] - match based on the index of the access. *)
let cCellWrite ?(base : target = []) ?(rhs:target = []) ?(index : target = []) (): constr =
  let lhs = [cCellAccess ~base ~index ()]  in
  cWrite ~lhs ~rhs ()

(* [cCellRead ~base ~index ]:  match a read operation or a write operation on an array accesses
     [base] - match based on the base of the access
     [index] - match based on the index of the access. *)
let cCellReadOrWrite ?(base : target = []) ?(index : target = []) () : constr =
  cOr [[cCellRead ~base ~index ()];[cCellWrite ~base ~index ()]]

(* [cArrayInit]: matches an array initialization list. *)
let cArrayInit : constr =
  Constr_array_init

(* [cStructInit]: matches a struct initialization list. *)
let cStructInit : constr =
  Constr_struct_init

(* [cCell ~cell_index ()]: matches an array cell on an array initialization list
    [cell_index] - match based on the cell index. *)
let cCell ?(cell_index : int option) () : constr =
  match cell_index with
  | None -> cTarget [cArrayInit; cStrict; cTrue]
  | Some i -> cTarget [cArrayInit; dArrayNth i]

let cArrayWrite (x : string) : constr =
  cWrite ~lhs:[cCellAccess ~base:[cVar x] ()] ()

let cArrayWriteAccess (x : string) : constr =
  cTarget [cWrite (); dLHS; cCellAccess ~base:[cVar x] ()]

(* FIXME: seems weird *)
let cArrayRead ?(index = []) (x : string) : constr =
  cRead ~addr:[cDiff
    [[cCellAccess ~base:[cVar x] ~index ()]]
    [[cArrayWriteAccess x]]] ()

let cPlusEq (lhs_tg : target) : constr =
  cPrimFun ~args:[lhs_tg; [cTrue]] (Prim_compound_assgn_op Binop_add)

(* [cOmp_match_all]: matches an OpenMP directive. *)
let cOmp_match_all : directive->bool =
  fun _ -> true

(* [cOmp ~pred ()]: matches an OpenMP directive that satisfies the predicate [pred]. *)
let cOmp ?(pred : (directive->bool) = cOmp_match_all) () : constr =
  let str =
    if pred == cOmp_match_all then "cOmp_match_all" else "cOmp_custom_pred" in
  Constr_omp (pred, str)

(* [cNamespace ~substr ~regexp name]: matches a namespace
    [substr] - match namespace name partially
    [regep] - match based on regexp
    [name] - match based on namespace name. *)
let cNamespace ?(substr : bool = false) ?(regexp : bool = false) (name : string) : constr =
  let ro = string_to_rexp_opt regexp substr name TrmKind_Expr in
  Constr_namespace ro

(******************************************************************************)
(*                          Target resolution                                 *)
(******************************************************************************)

(* NOW INCLUDED
let resolve_target = Constr.resolve_target
let resolve_target_between = Constr.resolve_target_between
*)

(* [check tg]: can be used as a debugging step. *)
let check (tg : target) : unit =
  Trace.call (fun t -> ignore (resolve_target tg t))

(* [filter_constr_occurrence tg]: filter occurrence constraints. *)
let filter_constr_occurrence (tg : target) : target =
  List.filter (function Constr_occurrences _ -> false | _ -> true ) tg

(* [enable_multi_targets tg]: convert target [tg] to a target that can match  multiple occurrences. *)
let enable_multi_targets (tg : target) : target =
    if List.exists (function Constr_occurrences _ -> true | _ -> false) tg
      then tg
      else nbMulti::tg

(* [relative_target tg]: convert target [tg] to a relative target. *)
let relative_target (tg : target) : target =
  if List.exists (function Constr_relative _ -> true | _ -> false) tg
    then tg
    else tBefore :: tg

(******************************************************************************)
(*                          Apply on target operations                        *)
(******************************************************************************)

(* [Transfo]: type of transformations. *)
module Transfo = struct
  type t = target -> unit
  type local = trm -> path -> trm
  type local_between = int -> local
end

let apply_on_path = Path.apply_on_path

(* [convert_stringreprs_from_documentation_to_string m]: convert string representations [m] to a string. *)
let convert_stringreprs_from_documentation_to_string (m : AstC_to_c.stringreprs) : (stringreprid, string) Hashtbl.t =
  Tools.hashtbl_map_values (fun _id d -> Tools.document_to_string ~width:PPrint.infinity d) m

(* [compute_stringreprs ~optitrust_syntax ~topfuns f t]: compute string representation recursively for trm [t]

    LATER: we should only compute stringreprs in top level functions that are
    targeted by the targets; to that end, we need to hide the body of the
    top level definitions that are not targeted). *)
let compute_stringreprs ?(optitrust_syntax:bool=false) ?(topfuns:Constr.constr_name list option) (f : trm->bool) (t : trm) : trm * AstC_to_c.stringreprs =
  (* DEBUG Printf.printf "compute_stringreprs %d\n" (match topfuns with
    | None -> -1
    | Some ts -> List.length ts); *)
  let t2 = Trm.label_subterms_with_fresh_stringreprids f t in

  let t3 =
    match topfuns with
    | None -> t2 (* need string reprs for all top level functions *)
    | Some topfuns_regexps -> (* need only for certain functions *)
        (* Note: if topfuns_regexps = [], we need stringrepr for no functions at all *)
        (* DEBUG Printf.printf "compute_stringreprs functions:\n";
        List.iter (function
          | None -> assert false
          | Some rexp -> Printf.printf "-> %s\n" (rexp_to_string rexp)) topfuns_regexps; *)
        let hidetopfun topfunvar =
          not (List.exists (fun rexp -> Constr.check_name rexp topfunvar.name) topfuns_regexps) in
        let t3, _ = hide_function_bodies hidetopfun t2 in
        t3
    in
  AstC_to_c.init_stringreprs();
  let t3_c_syntax = Ast_fromto_AstC.cfeatures_intro (trm_erase_var_ids t3) in
  let _doc = AstC_to_c.(ast_to_doc { style with optitrust_syntax }) t3_c_syntax in (* fill in the [AstC_to_c.stringreprs] table, ignore the result *)
  let m = AstC_to_c.get_and_clear_stringreprs() in
  t2, m (* we return t2, not t3, because t3 has hidden bodies *)

(* [compute_stringreprs_and_update_ast ~optitrust_syntax f]: label subterms with fresh stringreprsid, then build
     a table that maps stringreprids to the corresponding documents
     LATER: this function should proabably be moved/merged into the view_subterms function. . *)
let compute_stringreprs_and_update_ast ?(optitrust_syntax:bool=false) (f : trm->bool) : AstC_to_c.stringreprs =
  let stringreprs = ref None in
  (* Note: through [Trace.apply], we modify the current AST by adding ids in the term annotation. *)
  Trace.apply (fun t ->
    let t2, m = compute_stringreprs ~optitrust_syntax f t in
    stringreprs := Some m;
    t2
    );
  match !stringreprs with
  | Some m -> m
  | _ -> assert false (* table was set in Trace.apply *)


(* [debug_dissapearing_mark]: only for debugging purposes. *)
let debug_disappearing_mark = true

(* [Interrupted_applyi_on_transformed_targets]: exception raise when targets are not resolved successfully. *)
exception Interrupted_applyi_on_transformed_targets of trm

(* [fix_target tg]: fix target [tg]. *)
let fix_target (tg : target) : target =
  (* Occurrence constraints should be unique. *)
  let check_occurrences = List.exists (function Constr_occurrences _ -> true | _ -> false) tg in
  (* If there are logic constraints then multiple occurrences are allowed. *)
  let check_logic = List.exists (function Constr_or _ | Constr_and _ -> true | _ -> false) tg in
  if (not check_occurrences) && check_logic then nbMulti :: tg else tg

(* [with_stringreprs_available_for tgs t (fun t2 -> action)]:  execute the [action]
   in a context where the AST [t] is viewed as [t2], which is a copy of [t] where
   certain nodes have their string representation available. Which nodes are concerned
   depend on the regexp constraints expressed in the targets [tgs]. *)
let with_stringreprs_available_for (tgs : target list) (t : trm) (f : trm -> 'a) : 'a =
  let kinds = Constr.get_target_regexp_kinds tgs in
  (* for debug  List.iter (fun k -> Printf.printf "(kind:%s)" (Constr.trm_kind_to_string k)) kinds;
      Printf.printf "==end of kinds==\n";. *)
  let topfuns = Constr.get_target_regexp_topfuns_opt tgs in
  let t2, m = compute_stringreprs ?topfuns:topfuns (Constr.match_regexp_trm_kinds kinds) t in
  if !Flags.debug_stringreprs then
    AstC_to_c.print_stringreprs m;
  let stringreprs = convert_stringreprs_from_documentation_to_string m in
  Constr.stringreprs := Some stringreprs;
  if !Flags.debug_stringreprs then
    Constr.print_stringreprs();
  let r = f t2 in
  Constr.stringreprs := None;
  r

(* [resolve_target_with_stringreprs_available tg t]: similar to [resolve_target] but this one computes
    first the string representation of all the ast nodes. *)
let resolve_target_with_stringreprs_available (tg : target) (t : trm) : paths =
  with_stringreprs_available_for [tg] t (fun t2 -> resolve_target tg t2)

(* [resolve_target_exactly_one_with_stringreprs_available tg t]: similar to [resolve_target_exactly_one] but this one computes
    first the string representation of all the ast nodes. *)
let resolve_target_exactly_one_with_stringreprs_available (tg : target) (t : trm) : path =
  with_stringreprs_available_for [tg] t (fun t2 -> resolve_target_exactly_one tg t2)

(* [resolve_path_with_stringreprs_available p t]: similar to [resolve_path] but this one computes first the string
    representation of all the ast nodes first. *)
let resolve_path_with_stringreprs_available (p : path) (t : trm) :  trm =
  with_stringreprs_available_for [target_of_path p] t (fun t2 -> resolve_path p t2)

(* TODO: should be this name for the 'current_ast' version. *)
(* [path_of_target_mark_one m t]: a wrapper for calling [resolve_target] with a mark for which we
    expect a single occurence. *)
let path_of_target_mark_one (m : mark) (t : trm) : path =
  match resolve_target [nbExact 1; cMark m] t with
  | [p] -> p
  | _ -> fail t.loc "Target.resolve_target_mark_one: unreachable because nbExact 1 should return one path"

(* [resolve_target_mark_one_else_any m t]: a wrapper for calling [resolve_target] with a mark for which we
    expect a single occurence. *)
let resolve_target_mark_one_else_any (m : mark) (t : trm) : paths =
    try resolve_target [nbExact 1; cMark m] t
    with Ast.Resolve_target_failure _ ->
      resolve_target [nbAny; cMark m] t

(* [resolve_target_between_mark_one_else_any]: a wrapper for calling [resolve_target] with a mark for
    which we expect a single occurence. *)
let resolve_target_between_mark_one_else_any (m : mark) (t : trm) : (path * int) list =
    try resolve_target_between [nbExact 1; cMark m] t
    with Ast.Resolve_target_failure _ ->
        resolve_target_between [nbAny; cMark m] t

(* [applyi_on_transformed_targets transformer tr tg]: apply a transformation [tr] on target [tg],
     [transformer] - change the resolved path so that more information about the context of the node is given,
     [tr] - transformation to be applied at the nodes corresponding at target [tg],
     [tg] - target. *)
let applyi_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : int -> trm -> 'a -> trm) (tg : target) : unit =
  Stats.comp_stats "applyi_on_transformed_targets" (fun () ->
  let tg = fix_target tg in
  Trace.apply (fun t -> with_stringreprs_available_for [tg] t (fun t ->
      (* LATER: use apply_with_stringreprs
                           and take an optional list of auxiliary targets as argument. *)
    let ps =
      Stats.stats ~cond:!Flags.analyse_stats_details ~name:"resolve_targets" (fun () ->
          resolve_target tg t) in
    let ps = if rev then List.rev ps else ps in
    match ps with
    | [] -> t
    | [p] -> tr 0 t (transformer p)
    | _ ->
        let marks = List.map (fun _ -> Mark.next()) ps in
        (* add marks for occurences -- could be implemented in a single path, if optimization were needed. *)
        (* Printf.printf "Before applyin_marks: %s\n" (AstC_to_c.ast_to_string t);. *)
        let t =
             Stats.comp_stats "applyi_on_transformed_targets add marks" (fun () ->
              Stats.stats ~cond:!Flags.analyse_stats_details ~name:"resolve_add_mark" (fun () ->
              List.fold_left2 (fun t p m -> apply_on_path (trm_add_mark m) t p) t ps marks)) in
        (* Printf.printf "After applying_marks: %s\n" (AstC_to_c.ast_to_string t);. *)
        (* iterate over these marks. *)
        Stats.comp_stats "applyi_on_transformed_targets apply transfo" (fun () ->
        begin try
          Xlist.fold_lefti (fun imark t m ->
            Stats.stats ~cond:!Flags.analyse_stats_details ~name:(Printf.sprintf "process target %d" imark) (fun () ->
              let ps = resolve_target_mark_one_else_any m t in
              match ps with
              | [p] ->
                  let t = apply_on_path (trm_rem_mark m) t p in
                  tr imark t (transformer p)
              | ps ->
                  let msg =
                    if ps <> []
                      then "applyi_on_transformed_targets: a mark was duplicated"
                      else (*failwith*) (Printf.sprintf "applyi_on_transformed_targets: mark %s disappeared" m)
                    in
                  if debug_disappearing_mark
                    then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
                    else fail None msg
            )
          ) t marks
        with Interrupted_applyi_on_transformed_targets t -> t
        end)
    )))

(* [apply_on_transformed_targets ~replace_top transformer tr tg]: similar to [applyi_to_transformed_targets] except,
     that here the index of the resolved_path is not considered.
      [transformer] - change the resolved path so that more information about the context of the node is given,
      [tr] - transformation to be applied at the nodes corresponding at target [tg],
      [tg] - target. *)
let apply_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : trm -> 'a -> trm) (tg : target) : unit =
  applyi_on_transformed_targets  ~rev transformer (fun _i t descr -> tr t descr) tg


(* [applyi_on_targets ~replace tr tg]: similar [applyi_on_transformed_targets] but here the transformer is the
     identity function
      [tr] - transformation to be applied at the nodes corresponding at target [tg]
      [tg] - target. *)
let applyi_on_targets (tr : int -> trm -> path -> trm) (tg : target) : unit =
  applyi_on_transformed_targets (fun p -> p) tr tg


(* [apply_on_targets ~replace tr tg]: similar to [applyi_on_targets] but here the index of the resolved path is not
    taken into account,
      [tr] - transformation to be applied,
      [tg] - target. *)
let apply_on_targets (tr : trm -> path -> trm) (tg : target) : unit =
  applyi_on_targets (fun _i t dl -> tr t dl) tg

(* [transfo_on_targets tr tg]: similar to [apply_on_targets] but this one is applies [tr] on the fly. *)
let transfo_on_targets (tr : trm -> trm) (tg : target) : unit =
  apply_on_targets (fun t dl -> apply_on_path tr t dl) tg

(* [iteri_on_transformed_targets transformer tr tg]: similar to [applyi_on_transformed_targets] except this one is meant
     to be used for combi transformations,
     [rev] - process the resolved paths in reverse order,
     [transformer] - change the resolved path so that more information about the context of the node is given,
     [tr] - transformation to be applied at the nodes corresponding at target [tg],
     [tg] - target,
     LATER: try to better factorize the code.
     LATER: add timing measurements. *)
let iteri_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : int -> trm -> 'a -> unit) (tg : target) : unit =
  Stats.comp_stats "iteri_on_transformed_targets" (fun () ->
  let tg = fix_target tg in
  Trace.call (fun t -> with_stringreprs_available_for [tg] t (fun t ->
    Stats.comp_stats "iteri_on_transformed_targets with_stringreprs_available" (fun () ->
    let ps =
      Stats.comp_stats "iteri_on_transformed_targets resolve_target" (fun () ->
        resolve_target tg t)
      (* ALTERNATIVE with_stringreprs_available_for tg t (fun t2 -> resolve_target tg t2). *) in
    let ps = if rev then List.rev ps else ps in
    match ps with
    | [] -> ()
    | [p] -> Stats.comp_stats "iteri_on_transformed_targets transform one" (fun () ->
          tr 0 t (transformer p))
    | _ ->
      let marks = List.map (fun _ -> Mark.next()) ps in
      let _t_before = t in
      (* add marks for occurences -- could be implemented in a single pass, if optimization were needed. *)
      let t =
        Stats.comp_stats "iteri_on_transformed_targets add marks" (fun () ->
          List.fold_left2 (fun t p m -> apply_on_path (trm_add_mark m) t p) t ps marks) in
      Trace.set_ast t; (* Never use the function [set_ast] in another file!. *)
      (* iterate over these marks *)
      try
        List.iteri (fun imark m ->
          let t = Trace.ast() in (* valid because inside the scope of [Trace.call]. *)
          let ps = Stats.comp_stats "iteri_on_transformed_targets find mark" (fun () ->
            resolve_target_mark_one_else_any m t) in
          match ps with
          | [p] ->
              (* Here we don't call [Marks.remove] to avoid a circular dependency issue. *)
              let t =
                Stats.comp_stats "iteri_on_transformed_targets remove mark" (fun () ->
                  apply_on_path (trm_rem_mark m) t p) in
              Trace.set_ast t; (* Never use the function [set_ast] in another file!. *)
              Stats.comp_stats (Printf.sprintf "iteri_on_transformed_targets perform transfo %d" imark) (fun () ->
                  tr imark t (transformer p)
              )
          | ps ->
              let msg =
                if ps <> []
                  then "iteri_on_transformed_targets: a mark was duplicated"
                  else (Printf.sprintf "iteri_on_transformed_targets: mark %s disappeared" m)
                in
              if debug_disappearing_mark
                then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
                else fail None msg
        ) marks
      with Interrupted_applyi_on_transformed_targets t -> Trace.set_ast t (* view the ast when the bug appears. *)
      ))))

(* [iter_on_transformed_targets ~rev transformer tr tg]: similar to [apply_on_transformed_targets] except,
     that here the index of the resolved_path is not considered.
      [rev] - process the resolved paths in reverse order,
      [transformer] - change the resolved path so that more information about the context of the node is given,
      [tr] - transformation to be applied at the nodes corresponding at target [tg],
      [tg] - target. *)
let iter_on_transformed_targets ?(rev : bool = false) (transformer : path -> 'a) (tr : trm -> 'a -> unit) (tg : target) : unit =
  iteri_on_transformed_targets ~rev transformer (fun _i t descr -> tr t descr) tg

(* [iteri_on_targets ~replace tr tg]: similar to [iteri_on_transformed_targets] except that here the transformer is
    the identity function
        [tg] - target
        [tr] - transformation to be applied at the nodes corresponding at target [tg]. *)
let iteri_on_targets ?(rev : bool = false) (tr : int -> trm -> path -> unit) (tg : target) : unit =
  iteri_on_transformed_targets~rev (fun p -> p) tr tg

(* [iter_on_targets ~rev tr tg]: similar to [iteri_on_targets] but here the index of the resolved path is not
     taken into account
      [rev] - process the resolved paths in reverse order
      [tr] - transformation to be applied at the nodes corresponding at target [tg]
      [tg] - target. *)
let iter_on_targets ?(rev : bool = false) (tr : trm -> path -> unit) (tg : target) : unit =
  iteri_on_targets ~rev (fun _i t dl -> tr t dl) tg


(* [applyi_on_transformed_targets_between transformer tr tg]: apply transformation [tr] on a target relative to [tg]
      [transformer] - change the resolved path so that more information about the context of the node is given
      [tr] - transformation to be applied at the nodes corresponding to target [tg]
      [tg] - target. *)
let applyi_on_transformed_targets_between (transformer : path * int -> 'a) (tr : int -> trm -> 'a -> trm) (tg : target) : unit =
  Trace.apply (fun t -> with_stringreprs_available_for [tg] t (fun t ->
  let ps =
    Stats.stats ~cond:!Flags.analyse_stats_details ~name:"resolve_targets" (fun () -> resolve_target_between tg t
      (* ALTERNATIVE
      with_stringreprs_available_for tg t (fun t2 ->
        resolve_target_between tg t2). *) ) in
  match ps with
  | [] -> t
  | [p] -> tr 0 t (transformer p)
  | _ ->
    let marks = List.map (fun _ -> Mark.next ()) ps in
    let t = Stats.stats ~cond:!Flags.analyse_stats_details ~name:"resolve_add_mark" (fun () ->
      List.fold_left2 (fun t (p_to_seq, i) m -> apply_on_path (trm_add_mark_between i m) t p_to_seq ) t ps marks) in
    try
      Xlist.fold_lefti (fun imark t m ->
        Stats.stats ~cond:!Flags.analyse_stats_details ~name:(Printf.sprintf "process target %d" imark) (fun () ->
          let ps = resolve_target_mark_one_else_any m t in
          match ps with
          | [p_to_seq] ->
            let t_seq, _ = resolve_path_and_ctx p_to_seq t in
            let i = begin match get_mark_index m t_seq with
             | Some i -> i |
              None -> fail t_seq.loc "applyi_on_transformed_targets_between: could not get the between index" end in
            let t = apply_on_path (trm_rem_mark_between m) t p_to_seq in
            tr imark t (transformer (p_to_seq,i))
          | ps ->
            let msg =
              if ps <> []
                then "applyi_on_transformed_targets_between: a mark was duplicated"
                else (Printf.sprintf "applyi_on_transformed_targets_between: mark %s disappeared" m) in
            if debug_disappearing_mark
              then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
              else fail None msg
        )) t marks
      with Interrupted_applyi_on_transformed_targets t -> t
    ))

(* [apply_on_transformed_targets_between transformer tr tg]: similar to [applyi_to_transformed_targets_between] except that
     here the index of the resolved_path is not considered.
      [transformer] - change the resolved path so that more information about the context of the node is given,
      [tr] - transformation to be applied at the nodes corresponding to target [tg],
      [tg] - target. *)
let apply_on_transformed_targets_between (transformer: path * int -> 'a) (tr : trm -> 'a -> trm) (tg : target) : unit =
  applyi_on_transformed_targets_between transformer (fun _i t descr -> tr t descr) tg


(* [applyi_on_targets_between ~replace tr tg]: similar to [applyi_on_transformed_targets_between] except that here
    the transformer is the identity function.
      [tr] - transformation to be applied at the nodes corresponding to target [tg]
      [tg] - target. *)
let applyi_on_targets_between (tr : int -> trm -> path * int -> trm) (tg : target) : unit =
  applyi_on_transformed_targets_between (fun (p,i) -> (p,i)) tr tg

(* [apply_on_targets_between ~replace tr tg]: similar to [applyi_on_targets_between] except that here the index
     of the resolved path is not taken into account.
      [tg] - target,
      [tr] - transformation to be applied. *)
let apply_on_targets_between (tr : trm -> 'a -> trm) (tg : target) : unit =
  (* TRICK: If tg is not a relative target then the following line makes it relative, this is used only for OpenMP pragmas. *)
  let tg = relative_target tg in
  applyi_on_targets_between (fun _i t pk -> tr t pk) tg



(******************************************************************************)
(*                                   New target system TODO: deprecate old one *)
(******************************************************************************)

(* [trm_add_marks_at_paths marks ps t] adds at the paths [ps] the marks named
   [marks] in the term [t], and returns the resulting term. *)
(* LATER: could use a system to set all the marks in a single pass over the ast,
    able to hand the Dir_before *)
let trm_add_marks_at_paths (marks:mark list) (ps:paths) (t:trm) : trm =
  if List.length ps <> List.length marks
    then failwith "trm_add_marks_at_paths: expects as many marks as paths";
  List.fold_left2 (fun t p m ->
      match last_dir_before_inv p with
      | None -> apply_on_path (trm_add_mark m) t p
      | Some (p_to_seq,i) -> apply_on_path (trm_add_mark_between i m) t p_to_seq)
    t ps marks

(* [trm_add_mark_at_paths markof ps t] adds a mark computed as
   [markof pi ti] at the path [pi] reaching a subterm [ti]
   among the list of paths [ps] *)
let trm_add_mark_at_paths (markof:path->trm->mark) (ps:paths) (t:trm) : trm =
  let marks = List.map (fun pi ->
    let ti = Path.get_trm_at_path pi t in
    markof pi ti) ps in
  trm_add_marks_at_paths marks ps t

(* LATER: add an optimization flag for transformations who know that they don't
   break the paths in the case of multiple targets, this avoids placing marks
   in the tree when -dump-trace is not requested *)
(* [iteri ?rev tr tg]: execute operation [tr] to each of the paths targeted by [tg].
     [rev] - process the resolved paths in reverse order,
     [tg] - target
     [tr] - processing to be applied at the nodes corresponding at target [tg];
            [tr i t p] where
            [i] is the index of the occurrence,
            [t] is the current full ast
            [p] is the path towards the target occurrence. *)
let iteri ?(rev : bool = false) (tr : int -> trm -> path -> unit) (tg : target) : unit =
  (* TODO TEMPORARY *)
  let c_o_r_bak = !Constr.old_resolution in
  Constr.old_resolution := false;
  let tr_wrapped i t p =
    Constr.old_resolution := c_o_r_bak;
    tr i t p;
    Constr.old_resolution := false
    in

  let tg = fix_target tg in
  let t = Trace.ast() in
  with_stringreprs_available_for [tg] t (fun t ->
      let ps = resolve_target tg t in
    let ps = if rev then List.rev ps else ps in
    match ps with
    | [] -> ()
    | [p] -> (* Call the transformation at that path *)
             tr_wrapped 0 t p
    | _ ->
      (* LATER: optimization to avoid mark for first occurrence *)
      let marks = List.map (fun _ -> Mark.next()) ps in
      let t = trm_add_marks_at_paths marks ps t in
      Trace.set_ast t;
      (* Iterate over these marks *)
      try
        List.iteri (fun occ m ->
          let t = Trace.ast() in
          (* Recover the path to the i-th mark (the one number [occ]) *)
          let ps = resolve_target_mark_one_else_any m t in
          match ps with
          | [p] ->
              (* Start by removing the mark *)
              (* Here we don't call [Marks.remove] to avoid a circular dependency issue. *)
              let t =
                match last_dir_before_inv p with
                | None -> apply_on_path (trm_rem_mark m) t p
                | Some (p_to_seq,i) -> apply_on_path (trm_rem_mark_between m) t p_to_seq
                in
              Trace.set_ast t;
              (* Call the transformation at that path *)
              tr_wrapped occ t p
          | ps ->
              (* There were not exactly one occurrence of the mark: either zero or multiple *)
              let msg =
                if ps <> []
                  then "iteri_on_transformed_targets: a mark was duplicated"
                  else (Printf.sprintf "iteri_on_transformed_targets: mark %s disappeared" m)
                in
              if debug_disappearing_mark
                then (Printf.eprintf "%s\n" msg; raise (Interrupted_applyi_on_transformed_targets t))
                else fail None msg
        ) marks
      with Interrupted_applyi_on_transformed_targets t ->
         (* Record the ast carried by the exception to allow visualizing the ast *)
         Trace.set_ast t
      );
    Constr.old_resolution := c_o_r_bak (* TEMPORARY *)

(* [iter] same as [iteri] but without occurence index *)
let iter ?(rev : bool = false) (tr : trm -> path -> unit) : target -> unit =
  iteri ~rev (fun occ t p -> tr t p)

let iter_at_target_paths ?(rev : bool = false) (transfo : trm -> unit) (tg : target) : unit =
  iter ~rev (fun t p -> transfo (Path.get_trm_at_path p t)) tg

(** [apply_at_path transfo p]: follow a path from the AST root to apply a function on the corresponding subterm *)
let apply_at_path (transfo : trm -> trm) (p : path) : unit =
  Trace.apply (fun t -> Path.apply_on_path transfo t p)

(** [resolve_path p]: follow a path from the AST root and return the subterm found *)
let resolve_path (p: path): trm =
  Path.resolve_path p (Trace.ast ())

(* [applyi tr tg]: apply transformation [tr] on the current ast
   to each of the paths targeted by [tg].
     [tg] - target
     [tr] - a call to [tr i t p] should compute the updated term where
            [i] is the index of the occurrence,
            [t] is the current full ast
            [p] is the path towards the target occurrence. *)
let applyi ?(rev : bool = false) (tr : int -> trm -> path -> trm) (tg : target): unit =
  iteri ~rev (fun occ t p -> Trace.set_ast (tr occ t p)) tg

let apply ?(rev : bool = false) (tr : trm -> path -> trm) (tg : target) : unit =
  applyi ~rev (fun _occ t p -> tr t p) tg

let apply_at_target_paths ?(rev : bool = false) (transfo : trm -> trm) (tg : target) : unit =
  iter ~rev (fun _ p -> apply_at_path transfo p) tg

let applyi_at_target_paths ?(rev : bool = false) (transfo : int -> trm -> trm) (tg : target) : unit =
  iteri ~rev (fun i _ p -> apply_at_path (transfo i) p) tg

(* ... [transfo t i] where [t] denotes the sequence and [i] denotes the index
   of the item in the sequence before which the target is aiming at. *)
let apply_at_target_paths_before (transfo : trm -> int -> trm) (tg : target) : unit =
  iter (fun _ pb ->
    let (p,i) = Path.last_dir_before_inv_success pb in
    apply_at_path (fun tseq -> transfo tseq i) p) tg


(******************************************************************************)
(*                                   Show                                     *)
(******************************************************************************)

(* [target_show_aux m t]: add mark [m] around the term [t]. *)
let target_show_aux ?(types : bool = false) (m : mark) (t : trm) : trm =
   let ty_as_string = begin match t.typ with | Some ty -> AstC_to_c.typ_to_string ty | _ ->  "" end in
   let m = if types then Printf.sprintf "m %s" ty_as_string else m in
   trm_add_mark m t

(* [target_show_transfo m t p]: apply [target_show_aux] at the trm [t] with path [p]. *)
let target_show_transfo ?(types : bool = false)(m : mark) : Transfo.local =
  apply_on_path (target_show_aux ~types m)

(* [target_between_show_aux m k t]: add a a mark [m] at the trm [t] at index [k] at position
    [k] in the marks list of the sequence described by the term [t]. *)
let target_between_show_aux (m : mark) (k : int) (t : trm) : trm =
    trm_add_mark_between k m t

(* [target_between_show_transfo id k t p]: apply [target_between_show_transfo_aux] at trm [t] with path [p]. *)
let target_between_show_transfo (m : mark) : Transfo.local_between =
  fun (k:int) -> apply_on_path (target_between_show_aux m k)


(* [show_next_id] used for batch mode execution of unit tests, to generate names of for marks.
    Only used when [Flags.execute_show_even_in_batch_mode] is set.  *)
let (show_next_id, show_next_id_reset) : (unit -> int) * (unit -> unit) =
  Tools.resetable_fresh_generator()

(* [show ~line:int tg]: transformation for visualizing targets.
   The operation add marks if the command line argument [-exit-line]
   matches the [line] argument provided to the function. Otherwise, the
   [show] function only checks that the path resolve properly.
   There is no need for a prefix such as [!!] in front of the [show]
   function, because it is recognized as a special function by the preprocessor
   that generates the [foo_with_lines.ml] instrumented source. *)
let show ?(line : int = -1) ?(types : bool = false) (tg : target) : unit =
  (* DEPRECATED ?(reparse : bool = false)  if reparse then reparse_alias(); *)
  (* Calling [enable_multi_targets] to automatically add [nbMulti] if there is no occurence constraint. *)
  let tg = enable_multi_targets tg in
  let interactive_action () =
    let marks_base = show_next_id() in
    let mark_of_occurence (i:int) : string =
      if (*DEPRECATED batch_mode &&*) !Flags.execute_show_even_in_batch_mode
        then Printf.sprintf "%d_%d" marks_base i
        else Printf.sprintf "%d" i
      in
    if Constr.is_target_between tg then begin
      applyi_on_targets_between (fun i t (p,k) ->
        let m = mark_of_occurence i in
        target_between_show_transfo m k t p) tg
    end else begin
      applyi_on_targets (fun i t p ->
        let m = mark_of_occurence i in
        target_show_transfo ~types m t p) tg
    end
    in
  let action_otherwise () =
    (* If in regular batch mode, then we only check that the targets are valid *)
    if Constr.is_target_between tg
      then applyi_on_targets_between (fun _i t (_p,_k) -> t) tg
      else applyi_on_targets (fun _i t _p -> t) tg
    in
  Trace.show_step ~line ~interactive_action ~action_otherwise ()

(* [show_ast] enables to view the current ast. *)
let show_ast ?(line:int = -1) () : unit =
  let t = Trace.ast() in
  Trace.interactive_step ~line ~ast_before:(fun () -> empty_ast) ~ast_after:(fun () -> t)

let show_computed_res ?(line:int = -1) ?(ast: trm = Trace.ast ()) () : unit =
  Flags.(with_flag display_resources false (fun () ->
    with_flag always_name_resource_hyp true (fun () ->
      Trace.interactive_step ~line ~ast_before:(fun () -> ast)
        ~ast_after:(fun () -> Ast_fromto_AstC.computed_resources_intro ast))))

(* LATER: Fix me *)
(* [show_type ~line ~reparse tg]: an alias for show with the argument [types] set to true. *)
let show_type ?(line : int = -1) (*DEPRECATED?(reparse : bool = false)*) (tg : target) : unit =
  show ~line (* DEPRECATED ~reparse*) ~types:true tg


  (** LATER rename to get_trm_at_option et gt_trm_at_exn *)
(* [get_trm_at_unsome tg]: similar to [get_trm_at] but this one fails incase there is not trm that corresponds to the target [tg]. *)
let get_trm_at_exn (tg : target) : trm =
  let t = Trace.ast() in
  let tg_path = resolve_target_exactly_one_with_stringreprs_available tg t in
  Path.resolve_path tg_path t

(* [get_trm_at]: get the trm that corresponds to the target [tg]
    NOTE: call this function only on targets that resolve to a unique trm. *)
let get_trm_at (tg : target) : trm option =
  try Some (get_trm_at_exn tg)
  with _ -> None

(* [get_ast ()]: return the full ast. *)
let get_ast () : trm =
  Tools.unsome (get_trm_at [])


(******************************************************************************)
(*                          Reparse                                           *)
(******************************************************************************)
(* LATER: We can use the following type for reparsing. *)
(* type reparse = | Reparse_none | Reparse_only_paths | Reparse_all. *)


(* [get_function_var_at dl]: get the name of the function that corresponds to [dl]. *)
let get_function_var_at (dl : path) : var option =
  let fun_decl = match get_trm_at (target_of_path dl) with
    | Some fd -> fd
    | None -> fail None "get_function_name_at: couldn't retrive the function name at the targeted path"
   in
  match fun_decl.desc with
  | Trm_let_fun (f, _, _, _, _) -> Some f
  | _ -> None


(* [get_top_level_function_var_containing dl]: get the name of the top level function that contains the path [dl]. *)
let get_toplevel_function_var_containing (dl : path) : var option =
  match dl with
  | Dir_seq_nth i :: Dir_body :: _ -> get_function_var_at [Dir_seq_nth i]
  | _ -> None


(* [reparse_only fun_nmaes]: reparse only those functions whose identifier is contained in [fun_names]. *)
let reparse_only ?(update_cur_ast : bool = true) (fun_names : var list) : unit =
  Trace.parsing_step (fun () -> Trace.call (fun t ->
    let chopped_ast, chopped_ast_map  =  hide_function_bodies (function f -> not (List.mem f fun_names)) t in
    let parsed_chopped_ast = Trace.reparse_trm (Trace.get_context ()) chopped_ast in
    if update_cur_ast then begin
      let new_ast = update_chopped_ast parsed_chopped_ast chopped_ast_map in
      Trace.set_ast new_ast
    end
  ))

(* [get_relative_type tg]: get the type of target relative , Before, After, First Last. *)
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


(* [reparse_after tr]: wrapper to force the reparsing after applying a transformation.
    For example type definitions are modified.
    See example in [Record.reveal_field]. The argument [~reparse:false] can be
    specified to deactivate the reparsing.
    There is an optimization for reparsing only top-level functions that are
    involved in the path targeted by the target [tg]; LATER: should deactivate
    this for transformations that make global changes beyond the targetd functions *)
(* TODO: change strategy for reparse, probably based on missing types?
   else on annotations added by clangml but cleared by smart-constructors
   TODO URGENT: the resolve_target does not work with the new Dir_before system *)
let reparse_after ?(update_cur_ast : bool = true) ?(reparse : bool = true) (tr : Transfo.t) (tg : target) : unit =
    if not reparse then tr tg else begin
      let tg = enable_multi_targets tg in
      let ast = (get_ast()) in
      (* LATER: it would be nice to avoid computing the
        with_stringreprs_available_for which we already compute later on
        during [tr tg]. *)
      (* FIXME: will not appear in trace *)
      let tg_paths = with_stringreprs_available_for [tg] ast (fun ast ->
        if Constr.is_target_between tg
        then let tg_ps = resolve_target_between tg ast in
            fst (List.split tg_ps)
        else resolve_target tg ast
        ) in
      tr tg;
      if !Flags.use_light_diff then begin
        let fun_vars = List.map get_toplevel_function_var_containing tg_paths in
        let fun_vars = Xlist.remove_duplicates (List.filter_map (fun d -> d) fun_vars) in
        reparse_only ~update_cur_ast fun_vars
      end else
        Trace.reparse ~update_cur_ast ();
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


(******************************************************************************)
(*                               Target aliases                               *)
(******************************************************************************)

(* TODO: should be default *)

let resolve_target_current_ast (tg : target) : paths =
  resolve_target tg (Trace.ast ())

let resolve_path_current_ast (p : path) : trm  =
  Path.resolve_path p (Trace.ast ())

let path_of_target_mark_one_current_ast (m : mark) : path =
  path_of_target_mark_one m (Trace.ast ())

let (~~) f a b = f b a
