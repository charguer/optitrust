open AST
open Coqlib
open Datatypes
open LTL
open List0
open Maps
open UnionFind

module U = UF(PTree)

(** val record_branch : U.t -> node -> bblock -> U.t **)

let record_branch uf pc = function
| [] -> uf
| i :: _ -> (match i with
             | Lbranch s -> U.union uf pc s
             | _ -> uf)

(** val record_branches : coq_function -> U.t **)

let record_branches f =
  PTree.fold record_branch f.fn_code U.empty

(** val record_cond :
    ((code * U.t) * bool) -> node -> bblock -> (code * U.t) * bool **)

let record_cond st pc = function
| [] -> st
| i :: _ ->
  (match i with
   | Lcond (_, _, s1, s2) ->
     let (p, _) = st in
     let (c, u) = p in
     if peq (U.repr u s1) (U.repr u s2)
     then (((PTree.remove pc c), (U.union u pc s1)), true)
     else st
   | _ -> st)

(** val record_conds_1 : (code * U.t) -> (code * U.t) * bool **)

let record_conds_1 = function
| (c, u) -> PTree.fold record_cond c ((c, u), false)

(** val record_conds : (code * U.t) -> U.t **)

let rec record_conds cu =
  let (cu', changed) = record_conds_1 cu in
  if changed then record_conds cu' else snd cu

(** val record_gotos : coq_function -> U.t **)

let record_gotos f =
  record_conds (f.fn_code, (record_branches f))

(** val tunnel_instr : U.t -> instruction -> instruction **)

let tunnel_instr u i = match i with
| Lbranch s -> Lbranch (U.repr u s)
| Lcond (cond, args, s1, s2) ->
  let s1' = U.repr u s1 in
  let s2' = U.repr u s2 in
  if peq s1' s2' then Lbranch s1' else Lcond (cond, args, s1', s2')
| Ljumptable (arg, tbl) -> Ljumptable (arg, (map (U.repr u) tbl))
| _ -> i

(** val tunnel_block : U.t -> bblock -> bblock **)

let tunnel_block u b =
  map (tunnel_instr u) b

(** val tunnel_function : coq_function -> coq_function **)

let tunnel_function f =
  let u = record_gotos f in
  { fn_sig = f.fn_sig; fn_stacksize = f.fn_stacksize; fn_code =
  (PTree.map1 (tunnel_block u) f.fn_code); fn_entrypoint =
  (U.repr u f.fn_entrypoint) }

(** val tunnel_fundef : fundef -> fundef **)

let tunnel_fundef f =
  transf_fundef tunnel_function f

(** val tunnel_program : program -> program **)

let tunnel_program p =
  transform_program tunnel_fundef p
