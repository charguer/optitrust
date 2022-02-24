(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 2.1 of   *)
(*  the License, or  (at your option) any later version.               *)
(*  This file is also distributed under the terms of the               *)
(*  INRIA Non-Commercial License Agreement.                            *)
(*                                                                     *)
(* *********************************************************************)

(* C abstract syntax after elaboration *)

(* Locations *)

type location = string * int            (* filename, line number *)

(* Identifiers *)

type ident =
  { name: string;                       (* name as in the source *)
    stamp: int }                        (* unique ID *)

(* Kinds of integers *)

type ikind =
  | IBool       (** [_Bool] *)
  | IChar       (** [char] *)
  | ISChar      (** [signed char] *)
  | IUChar      (** [unsigned char] *)
  | IInt        (** [int] *)
  | IUInt       (** [unsigned int] *)
  | IShort      (** [short] *)
  | IUShort     (** [unsigned short] *)
  | ILong       (** [long] *)
  | IULong      (** [unsigned long] *)
  | ILongLong   (** [long long] (or [_int64] on Microsoft Visual C) *)
  | IULongLong  (** [unsigned long long] (or [unsigned _int64] on Microsoft
                    Visual C) *)

(** Kinds of floating-point numbers*)

type fkind =
    FFloat      (** [float] *)
  | FDouble     (** [double] *)
  | FLongDouble (** [long double] *)


(** Constants *)

type float_cst = {
  hex : bool;
  intPart : string;
  fracPart : string;
  exp : string;
}

type constant =
  | CInt of int64 * ikind * string      (* as it appeared in the source *)
  | CFloat of float_cst * fkind
  | CStr of string
  | CWStr of int64 list
  | CEnum of ident * int64              (* enum tag, integer value *)

(** Attributes *)

type attr_arg =
  | AIdent of string
  | AInt of int64
  | AString of string

type attribute =
  | AConst
  | AVolatile
  | ARestrict
  | AAlignas of int                     (* always a power of 2 *)
  | Attr of string * attr_arg list

type attributes = attribute list

(** Storage classes *)

type storage =
  | Storage_default (* used for toplevel names without explicit storage *)
  | Storage_extern
  | Storage_static
  | Storage_auto    (* used for block-scoped names without explicit storage *)
  | Storage_register

(** Unary operators *)

type unary_operator =
  | Ominus	(* unary "-" *)
  | Oplus	(* unary "+" *)
  | Olognot	(* "!" *)
  | Onot        (* "~" *)
  | Oderef      (* unary "*" *)
  | Oaddrof     (* "&" *)
  | Opreincr    (* "++" prefix *)
  | Opredecr    (* "--" prefix *)
  | Opostincr   (* "++" postfix *)
  | Opostdecr   (* "--" postfix *)
  | Odot of string (* ".field" *)
  | Oarrow of string (* "->field" *)

type binary_operator =
  | Oadd        (* binary "+" *)
  | Osub        (* binary "-" *)
  | Omul        (* binary "*" *)
  | Odiv        (* "/" *)
  | Omod        (* "%" *)
  | Oand        (* "&" *)
  | Oor         (* "|" *)
  | Oxor        (* "^" *)
  | Oshl        (* "<<" *)
  | Oshr        (* ">>" *)
  | Oeq         (* "==" *)
  | One         (* "!=" *)
  | Olt         (* "<" *)
  | Ogt         (* ">" *)
  | Ole         (* "<=" *)
  | Oge         (* ">=" *)
  | Oindex      (* "a[i]" *)
  | Oassign     (* "=" *)
  | Oadd_assign (* "+=" *)
  | Osub_assign (* "-=" *)
  | Omul_assign (* "*=" *)
  | Odiv_assign (* "/=" *)
  | Omod_assign (* "%=" *)
  | Oand_assign (* "&=" *)
  | Oor_assign  (* "|=" *)
  | Oxor_assign (* "^=" *)
  | Oshl_assign (* "<<=" *)
  | Oshr_assign (* ">>=" *)
  | Ocomma      (* "," *)
  | Ologand     (* "&&" *)
  | Ologor      (* "||" *)

(** Types *)

type typ =
  | TVoid of attributes
  | TInt of ikind * attributes
  | TFloat of fkind * attributes
  | TPtr of typ * attributes
  | TRef of typ (* OptiTrust adds support for reference types *)
  | TArray of typ * (int64 * exp) option * attributes
  | TFun of typ * (ident * typ) list option * bool * attributes
  | TNamed of ident * attributes
  | TStruct of ident * attributes
  | TUnion of ident * attributes
  | TEnum of ident * attributes

(** Struct or union field *)

and field = {
    fld_name: string;
    fld_typ: typ;
    fld_bitfield: int option;
    fld_anonymous: bool;
}

and struct_or_union =
  | Struct
  | Union

(** Expressions *)

and exp = { edesc: exp_desc; etyp: typ; eloc: location }

and exp_desc =
  | EConst of constant
  | ESizeof of typ
  | EAlignof of typ
  | EVar of ident
  | EUnop of unary_operator * exp
  | EBinop of binary_operator * exp * exp * typ
                           (* the type at which the operation is performed *)
  | EConditional of exp * exp * exp
  | ECast of typ * exp
  | ECompound of typ * init
  | ECall of exp * exp list

(** Initializers *)

and identtyp = ident * typ

and init =
  | Init_single of exp
  | Init_array of init list
  | Init_struct of identtyp * (field * init) list
  | Init_union of ident * field * init

(** GCC extended asm *)

type asm_operand = string option * string * exp

(** Statements *)

type stmt = { sdesc: stmt_desc; sloc: location }

and stmt_desc =
  | Sskip
  | Sdo of exp
  | Sseq of stmt * stmt
  | Sif of exp * stmt * stmt
  | Swhile of exp * stmt
  | Sdowhile of stmt * exp
  | Sfor of stmt * exp * stmt * stmt
  | Sbreak
  | Scontinue
  | Sswitch of exp * stmt
  | Slabeled of slabel * stmt
  | Sgoto of string
  | Sreturn of init option (* OptiTrust generalized from exp option *)
  | Sblock of stmt list
  | Sdecl of decl
  | Sasm of attributes * string * asm_operand list * asm_operand list * string list

and slabel =
  | Slabel of string
  | Scase of exp * int64
  | Sdefault

(** Declarations *)

and decl =
  storage * ident * typ * init option

(** Function definitions *)

type fundef = {
    fd_storage: storage;
    fd_inline: bool;
    fd_name: ident;
    fd_attrib: attributes;
    fd_ret: typ;                   (* return type *)
    fd_params: (ident * typ) list; (* formal parameters *)
    fd_vararg: bool;               (* variable arguments? *)
    fd_locals: decl list;          (* local variables *)
    fd_body: stmt
}

(** Element of an enumeration *)

type enumerator = ident * int64 * exp option

(** Global declarations *)

type globdecl =
  { gdesc: globdecl_desc; gloc: location }

and globdecl_desc =
  | Gdecl of decl           (* variable declaration, function prototype *)
  | Gfundef of fundef                   (* function definition *)
  | Gcompositedecl of struct_or_union * ident * attributes
                                        (* struct/union declaration *)
  | Gcompositedef of struct_or_union * ident * attributes * field list
                                        (* struct/union definition *)
  | Gtypedef of ident * typ             (* typedef *)
  | Genumdef of ident * attributes * enumerator list
                                        (* enum definition *)
  | Gpragma of string                   (* #pragma directive *)

type program = globdecl list

(** Builtin types and functions *)

type builtins = {
  builtin_typedefs: (string * typ) list;
  builtin_functions: (string * (typ * typ list * bool)) list
}


(* ------------------- *)

(* Empty location *)

let no_loc = ("", -1)

(* Construct a dummy expression
   (used in particular to annotate TArray when the index does not come from the source) *)

let no_exp =
  { edesc = EConst (CStr ""); etyp = TVoid []; eloc = no_loc }

let is_no_exp e =
  e == no_exp

(* Construct an integer constant *)

let intconst v ik =
  { edesc = EConst(CInt(v, ik, "")); etyp = TInt(ik, []); eloc = no_loc }

(* Construct a non-zero constants that can be used for missing conditionals in loops *)

let exp_missing_for_loop_conditional =
  intconst 1L IInt

let is_exp_missing_for_loop_conditional e =
  e == exp_missing_for_loop_conditional

(* Construct the 0 float constant of double type *)

let floatconst0 =
  { edesc = EConst(CFloat({hex=false; intPart="0"; fracPart="0"; exp="0"}, FDouble));
    etyp = TFloat(FDouble, []);
    eloc = no_loc }

(* Construct a cast expression *)

let ecast ty e = { edesc = ECast(ty, e); etyp = ty; eloc = no_loc }

(* Construct the literal "0" with void * type *)

let nullconst = ecast (TPtr(TVoid [], [])) (intconst 0L IInt)

(* Construct an assignment expression *)

let eassign e1 e2 = { edesc = EBinop(Oassign, e1, e2, e1.etyp); etyp = e1.etyp; eloc = no_loc }

(* Construct a "," expression.  Reassociate to the left so that
   it prints more nicely. *)

let rec ecomma e1 e2 =
  match e2.edesc with
  | EBinop(Ocomma, e2', e2'', _) ->
     ecomma (ecomma e1 e2') e2''
  | _ ->
     { edesc = EBinop(Ocomma, e1, e2, e2.etyp); etyp = e2.etyp; eloc = no_loc }

(* Construct a cascade of "," expressions.
   Associate to the left so that it prints more nicely. *)

let ecommalist el e =
  match el with
  | [] -> e
  | e1 :: el -> ecomma (List.fold_left ecomma e1 el) e

(* Construct an address-of expression.  Can be applied not just to
   an l-value but also to a sequence or a conditional of l-values. *)

let rec eaddrof e =
  match e.edesc with
  | EUnop(Oderef, e1) -> e1
  | EBinop(Ocomma, e1, e2, _) -> ecomma e1 (eaddrof e2)
  | EConditional(e1, e2, e3) ->
      { edesc = EConditional(e1, eaddrof e2, eaddrof e3); etyp = TPtr(e.etyp, []); eloc = no_loc }
  | _ -> { edesc = EUnop(Oaddrof, e); etyp = TPtr(e.etyp, []); eloc = no_loc }

(* Construct a sequence *)

let sseq loc s1 s2 =
  match s1.sdesc, s2.sdesc with
  | Sskip, _ -> s2
  | _, Sskip -> s1
  | _, Sblock sl -> { sdesc = Sblock(s1 :: sl); sloc = loc }
  | _, _ -> { sdesc = Sseq(s1, s2); sloc = loc }

(* Construct an assignment statement *)

let sassign loc lv rv =
  { sdesc = Sdo (eassign lv rv); sloc = loc }

(* Dummy skip statement *)

let sskip = { sdesc = Sskip; sloc = no_loc }
