open AST
open BinNums
open List0
open Maps
open Postorder
open RTL

(** val renum_pc : positive PTree.t -> node -> node **)

let renum_pc pnum pc =
  match PTree.get pc pnum with
  | Some pc' -> pc'
  | None -> Coq_xH

(** val renum_instr : positive PTree.t -> instruction -> instruction **)

let renum_instr pnum i = match i with
| Inop s -> Inop (renum_pc pnum s)
| Iop (op, args, res, s) -> Iop (op, args, res, (renum_pc pnum s))
| Iload (chunk, addr, args, res, s) ->
  Iload (chunk, addr, args, res, (renum_pc pnum s))
| Istore (chunk, addr, args, src, s) ->
  Istore (chunk, addr, args, src, (renum_pc pnum s))
| Icall (sg, ros, args, res, s) ->
  Icall (sg, ros, args, res, (renum_pc pnum s))
| Ibuiltin (ef, args, res, s) -> Ibuiltin (ef, args, res, (renum_pc pnum s))
| Icond (cond, args, s1, s2) ->
  Icond (cond, args, (renum_pc pnum s1), (renum_pc pnum s2))
| Ijumptable (arg, tbl) -> Ijumptable (arg, (List0.map (renum_pc pnum) tbl))
| _ -> i

(** val renum_node :
    positive PTree.t -> code -> node -> instruction -> code **)

let renum_node pnum c' pc i =
  match PTree.get pc pnum with
  | Some pc' -> PTree.set pc' (renum_instr pnum i) c'
  | None -> c'

(** val renum_cfg : positive PTree.t -> code -> code **)

let renum_cfg pnum c =
  PTree.fold (renum_node pnum) c PTree.empty

(** val transf_function : coq_function -> coq_function **)

let transf_function f =
  let pnum = postorder (successors_map f) f.fn_entrypoint in
  { fn_sig = f.fn_sig; fn_params = f.fn_params; fn_stacksize =
  f.fn_stacksize; fn_code = (renum_cfg pnum f.fn_code); fn_entrypoint =
  (renum_pc pnum f.fn_entrypoint) }

(** val transf_fundef : fundef -> fundef **)

let transf_fundef fd =
  transf_fundef transf_function fd

(** val transf_program : program -> program **)

let transf_program p =
  transform_program transf_fundef p
