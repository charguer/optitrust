open AST
open BinInt
open BinNums
open Cminor
open Coqlib
open Csharpminor
open Datatypes
open Errors
open Integers
open List0
open Maps
open Mergesort

type compilenv = coq_Z PTree.t

(** val var_addr : compilenv -> ident -> Cminor.expr **)

let var_addr cenv id =
  match PTree.get id cenv with
  | Some ofs -> Cminor.Econst (Oaddrstack (Ptrofs.repr ofs))
  | None -> Cminor.Econst (Oaddrsymbol (id, Ptrofs.zero))

(** val transl_constant : constant -> Cminor.constant **)

let transl_constant = function
| Ointconst n -> Cminor.Ointconst n
| Ofloatconst n -> Cminor.Ofloatconst n
| Osingleconst n -> Cminor.Osingleconst n
| Olongconst n -> Cminor.Olongconst n

(** val transl_expr : compilenv -> expr -> Cminor.expr res **)

let rec transl_expr cenv = function
| Evar id -> OK (Cminor.Evar id)
| Eaddrof id -> OK (var_addr cenv id)
| Econst cst -> OK (Cminor.Econst (transl_constant cst))
| Eunop (op, e1) ->
  (match transl_expr cenv e1 with
   | OK x -> OK (Cminor.Eunop (op, x))
   | Error msg0 -> Error msg0)
| Ebinop (op, e1, e2) ->
  (match transl_expr cenv e1 with
   | OK x ->
     (match transl_expr cenv e2 with
      | OK x0 -> OK (Cminor.Ebinop (op, x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Eload (chunk, e0) ->
  (match transl_expr cenv e0 with
   | OK x -> OK (Cminor.Eload (chunk, x))
   | Error msg0 -> Error msg0)

(** val transl_exprlist : compilenv -> expr list -> Cminor.expr list res **)

let rec transl_exprlist cenv = function
| [] -> OK []
| e1 :: e2 ->
  (match transl_expr cenv e1 with
   | OK x ->
     (match transl_exprlist cenv e2 with
      | OK x0 -> OK (x :: x0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)

type exit_env = bool list

(** val shift_exit : exit_env -> nat -> nat **)

let rec shift_exit e n =
  match e with
  | [] -> n
  | b :: e' ->
    if b
    then (match n with
          | O -> O
          | S m -> S (shift_exit e' m))
    else S (shift_exit e' n)

(** val switch_table : lbl_stmt -> nat -> (coq_Z * nat) list * nat **)

let rec switch_table ls k =
  match ls with
  | LSnil -> ([], k)
  | LScons (o, _, rem) ->
    (match o with
     | Some ni ->
       let (tbl, dfl) = switch_table rem (S k) in (((ni, k) :: tbl), dfl)
     | None -> let (tbl, _) = switch_table rem (S k) in (tbl, k))

(** val switch_env : lbl_stmt -> exit_env -> exit_env **)

let rec switch_env ls e =
  match ls with
  | LSnil -> e
  | LScons (_, _, ls') -> false :: (switch_env ls' e)

(** val transl_stmt : compilenv -> exit_env -> stmt -> Cminor.stmt res **)

let rec transl_stmt cenv xenv = function
| Sskip -> OK Cminor.Sskip
| Sset (id, e) ->
  (match transl_expr cenv e with
   | OK x -> OK (Sassign (id, x))
   | Error msg0 -> Error msg0)
| Sstore (chunk, e1, e2) ->
  (match transl_expr cenv e1 with
   | OK x ->
     (match transl_expr cenv e2 with
      | OK x0 -> OK (Cminor.Sstore (chunk, x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Scall (optid, sig0, e, el) ->
  (match transl_expr cenv e with
   | OK x ->
     (match transl_exprlist cenv el with
      | OK x0 -> OK (Cminor.Scall (optid, sig0, x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sbuiltin (optid, ef, el) ->
  (match transl_exprlist cenv el with
   | OK x -> OK (Cminor.Sbuiltin (optid, ef, x))
   | Error msg0 -> Error msg0)
| Sseq (s1, s2) ->
  (match transl_stmt cenv xenv s1 with
   | OK x ->
     (match transl_stmt cenv xenv s2 with
      | OK x0 -> OK (Cminor.Sseq (x, x0))
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sifthenelse (e, s1, s2) ->
  (match transl_expr cenv e with
   | OK x ->
     (match transl_stmt cenv xenv s1 with
      | OK x0 ->
        (match transl_stmt cenv xenv s2 with
         | OK x1 -> OK (Cminor.Sifthenelse (x, x0, x1))
         | Error msg0 -> Error msg0)
      | Error msg0 -> Error msg0)
   | Error msg0 -> Error msg0)
| Sloop s0 ->
  (match transl_stmt cenv xenv s0 with
   | OK x -> OK (Cminor.Sloop x)
   | Error msg0 -> Error msg0)
| Sblock s0 ->
  (match transl_stmt cenv (true :: xenv) s0 with
   | OK x -> OK (Cminor.Sblock x)
   | Error msg0 -> Error msg0)
| Sexit n -> OK (Cminor.Sexit (shift_exit xenv n))
| Sswitch (long, e, ls) ->
  let (tbl, dfl) = switch_table ls O in
  (match transl_expr cenv e with
   | OK x ->
     transl_lblstmt cenv (switch_env ls xenv) ls (Cminor.Sswitch (long, x,
       tbl, dfl))
   | Error msg0 -> Error msg0)
| Sreturn o ->
  (match o with
   | Some e ->
     (match transl_expr cenv e with
      | OK x -> OK (Cminor.Sreturn (Some x))
      | Error msg0 -> Error msg0)
   | None -> OK (Cminor.Sreturn None))
| Slabel (lbl, s0) ->
  (match transl_stmt cenv xenv s0 with
   | OK x -> OK (Cminor.Slabel (lbl, x))
   | Error msg0 -> Error msg0)
| Sgoto lbl -> OK (Cminor.Sgoto lbl)

(** val transl_lblstmt :
    compilenv -> exit_env -> lbl_stmt -> Cminor.stmt -> Cminor.stmt res **)

and transl_lblstmt cenv xenv ls body =
  match ls with
  | LSnil -> OK (Cminor.Sseq ((Cminor.Sblock body), Cminor.Sskip))
  | LScons (_, s, ls') ->
    (match transl_stmt cenv xenv s with
     | OK x ->
       transl_lblstmt cenv (tl xenv) ls' (Cminor.Sseq ((Cminor.Sblock body),
         x))
     | Error msg0 -> Error msg0)

(** val block_alignment : coq_Z -> coq_Z **)

let block_alignment sz =
  if zlt sz (Zpos (Coq_xO Coq_xH))
  then Zpos Coq_xH
  else if zlt sz (Zpos (Coq_xO (Coq_xO Coq_xH)))
       then Zpos (Coq_xO Coq_xH)
       else if zlt sz (Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH))))
            then Zpos (Coq_xO (Coq_xO Coq_xH))
            else Zpos (Coq_xO (Coq_xO (Coq_xO Coq_xH)))

(** val assign_variable :
    (compilenv * coq_Z) -> (ident * coq_Z) -> compilenv * coq_Z **)

let assign_variable cenv_stacksize = function
| (id, sz) ->
  let (cenv, stacksize) = cenv_stacksize in
  let ofs = align stacksize (block_alignment sz) in
  ((PTree.set id ofs cenv), (Z.add ofs (Z.max Z0 sz)))

(** val assign_variables :
    (compilenv * coq_Z) -> (ident * coq_Z) list -> compilenv * coq_Z **)

let assign_variables cenv_stacksize vars =
  fold_left assign_variable vars cenv_stacksize

module VarOrder =
 struct
  type t = ident * coq_Z

  (** val leb : t -> t -> bool **)

  let leb v1 v2 =
    (fun x -> x) (zle (snd v1) (snd v2))
 end

module VarSort = Sort(VarOrder)

(** val build_compilenv : coq_function -> compilenv * coq_Z **)

let build_compilenv f =
  assign_variables (PTree.empty, Z0) (VarSort.sort f.fn_vars)

(** val transl_funbody :
    compilenv -> coq_Z -> coq_function -> Cminor.coq_function res **)

let transl_funbody cenv stacksize f =
  match transl_stmt cenv [] f.fn_body with
  | OK x ->
    OK { Cminor.fn_sig = f.fn_sig; Cminor.fn_params = f.fn_params;
      Cminor.fn_vars = f.fn_temps; fn_stackspace = stacksize;
      Cminor.fn_body = x }
  | Error msg0 -> Error msg0

(** val transl_function : coq_function -> Cminor.coq_function res **)

let transl_function f =
  let (cenv, stacksize) = build_compilenv f in
  if zle stacksize Ptrofs.max_unsigned
  then transl_funbody cenv stacksize f
  else Error
         (msg
           ('C'::('m'::('i'::('n'::('o'::('r'::('g'::('e'::('n'::(':'::(' '::('t'::('o'::('o'::(' '::('m'::('a'::('n'::('y'::(' '::('l'::('o'::('c'::('a'::('l'::(' '::('v'::('a'::('r'::('i'::('a'::('b'::('l'::('e'::('s'::(','::(' '::('s'::('t'::('a'::('c'::('k'::(' '::('s'::('i'::('z'::('e'::(' '::('e'::('x'::('c'::('e'::('e'::('d'::('e'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val transl_fundef : fundef -> Cminor.fundef res **)

let transl_fundef f =
  transf_partial_fundef transl_function f

(** val transl_program : program -> Cminor.program res **)

let transl_program p =
  transform_partial_program transl_fundef p
