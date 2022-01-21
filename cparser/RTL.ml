open AST
open BinNums
open BinPos
open Datatypes
open List0
open Maps
open Op
open Registers

type node = positive

type instruction =
| Inop of node
| Iop of operation * reg list * reg * node
| Iload of memory_chunk * addressing * reg list * reg * node
| Istore of memory_chunk * addressing * reg list * reg * node
| Icall of signature * (reg, ident) sum * reg list * reg * node
| Itailcall of signature * (reg, ident) sum * reg list
| Ibuiltin of external_function * reg builtin_arg list * reg builtin_res
   * node
| Icond of condition * reg list * node * node
| Ijumptable of reg * node list
| Ireturn of reg option

type code = instruction PTree.t

type coq_function = { fn_sig : signature; fn_params : reg list;
                      fn_stacksize : coq_Z; fn_code : code;
                      fn_entrypoint : node }

type fundef = coq_function AST.fundef

type program = (fundef, unit) AST.program

(** val transf_function :
    (node -> instruction -> instruction) -> coq_function -> coq_function **)

let transf_function transf f =
  { fn_sig = f.fn_sig; fn_params = f.fn_params; fn_stacksize =
    f.fn_stacksize; fn_code = (PTree.map transf f.fn_code); fn_entrypoint =
    f.fn_entrypoint }

(** val successors_instr : instruction -> node list **)

let successors_instr = function
| Inop s -> s :: []
| Iop (_, _, _, s) -> s :: []
| Iload (_, _, _, _, s) -> s :: []
| Istore (_, _, _, _, s) -> s :: []
| Icall (_, _, _, _, s) -> s :: []
| Ibuiltin (_, _, _, s) -> s :: []
| Icond (_, _, ifso, ifnot) -> ifso :: (ifnot :: [])
| Ijumptable (_, tbl) -> tbl
| _ -> []

(** val successors_map : coq_function -> node list PTree.t **)

let successors_map f =
  PTree.map1 successors_instr f.fn_code

(** val instr_uses : instruction -> reg list **)

let instr_uses = function
| Inop _ -> []
| Iop (_, args, _, _) -> args
| Iload (_, _, args, _, _) -> args
| Istore (_, _, args, src, _) -> src :: args
| Icall (_, s0, args, _, _) ->
  (match s0 with
   | Coq_inl r -> r :: args
   | Coq_inr _ -> args)
| Itailcall (_, s, args) ->
  (match s with
   | Coq_inl r -> r :: args
   | Coq_inr _ -> args)
| Ibuiltin (_, args, _, _) -> params_of_builtin_args args
| Icond (_, args, _, _) -> args
| Ijumptable (arg, _) -> arg :: []
| Ireturn o -> (match o with
                | Some arg -> arg :: []
                | None -> [])

(** val instr_defs : instruction -> reg option **)

let instr_defs = function
| Iop (_, _, res, _) -> Some res
| Iload (_, _, _, dst, _) -> Some dst
| Icall (_, _, _, res, _) -> Some res
| Ibuiltin (_, _, res, _) -> (match res with
                              | BR r -> Some r
                              | _ -> None)
| _ -> None

(** val max_pc_function : coq_function -> positive **)

let max_pc_function f =
  PTree.fold (fun m pc _ -> Pos.max m pc) f.fn_code Coq_xH

(** val max_reg_instr : positive -> node -> instruction -> positive **)

let max_reg_instr m _ = function
| Inop _ -> m
| Iop (_, args, res, _) -> fold_left Pos.max args (Pos.max res m)
| Iload (_, _, args, dst, _) -> fold_left Pos.max args (Pos.max dst m)
| Istore (_, _, args, src, _) -> fold_left Pos.max args (Pos.max src m)
| Icall (_, s0, args, res, _) ->
  (match s0 with
   | Coq_inl r -> fold_left Pos.max args (Pos.max r (Pos.max res m))
   | Coq_inr _ -> fold_left Pos.max args (Pos.max res m))
| Itailcall (_, s, args) ->
  (match s with
   | Coq_inl r -> fold_left Pos.max args (Pos.max r m)
   | Coq_inr _ -> fold_left Pos.max args m)
| Ibuiltin (_, args, res, _) ->
  fold_left Pos.max (params_of_builtin_args args)
    (fold_left Pos.max (params_of_builtin_res res) m)
| Icond (_, args, _, _) -> fold_left Pos.max args m
| Ijumptable (arg, _) -> Pos.max arg m
| Ireturn o -> (match o with
                | Some arg -> Pos.max arg m
                | None -> m)

(** val max_reg_function : coq_function -> positive **)

let max_reg_function f =
  Pos.max (PTree.fold max_reg_instr f.fn_code Coq_xH)
    (fold_left Pos.max f.fn_params Coq_xH)
